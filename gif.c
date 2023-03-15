#include <grub/bitmap.h>
#include <grub/types.h>
#include <grub/normal.h>
#include <grub/dl.h>
#include <grub/mm.h>
#include <grub/misc.h>
#include <grub/bufio.h>
#include <grub/safemath.h>
#include <grub/time.h>
#include <grub/video.h>

GRUB_MOD_LICENSE ("GPLv3+");

#define MIN(A, B) ((A) < (B) ? (A) : (B))
#define MAX(A, B) ((A) > (B) ? (A) : (B))

#define GIF_DEBUG
//#define GIF_RELEASE
#define LOOP 0 //0:not loop 1:loop

typedef struct Entry {
    grub_uint16_t length;
    grub_uint16_t prefix;
    grub_uint8_t  suffix;
} Entry;

typedef struct Table {
    int bulk;
    int nentries;
    Entry *entries;
} Table;

#ifdef GIF_DEBUG
static grub_command_t cmd;
#endif

static const grub_uint8_t gif_magic[6] = 
{0x47, 0x49, 0x46, 0x38, 0x39, 0x61};

typedef struct grub_gif_palette {
    int size;
    grub_uint8_t colors[0x100 * 3];
} grub_gif_palette;

typedef struct grub_gif_GCE {
    grub_uint16_t delay;
    grub_uint8_t tindex;
    grub_uint8_t disposal;
    int input;
    int transparency;
} grub_gif_GCE;

struct grub_gif_data
{
  grub_file_t file;
  struct grub_video_bitmap **bitmap;
  //grub_uint8_t *image_data;

  grub_off_t anim_start;
  grub_uint16_t image_width, image_height;
  grub_uint16_t depth;
  grub_uint16_t loop_count;

  grub_gif_GCE gce;
  grub_gif_palette *palette;
  grub_gif_palette lct, gct;

  grub_uint8_t bgindex;
  grub_uint8_t *canvas, *frame;
  grub_uint16_t fx, fy, fw, fh;

  void (*plain_text)(
      struct grub_gif_data *gif, grub_uint16_t tx, grub_uint16_t ty,
      grub_uint16_t tw, grub_uint16_t th, grub_uint8_t cw, grub_uint8_t ch,
      grub_uint8_t fg, grub_uint8_t bg
  );
  void (*comment)(struct grub_gif_data *gif);
  void (*application)(struct grub_gif_data *gif, char id[8], char auth[3]);
};

static grub_uint16_t
read_num(grub_file_t file)
{
    grub_uint8_t bytes[2];

    grub_file_read(file, bytes, 2);

    return bytes[0] + (((grub_uint16_t) bytes[1]) << 8);
}

static void
discard_sub_blocks(struct grub_gif_data *gif)
{
    grub_uint8_t size;

    do {
        grub_file_read(gif->file, &size, 1);
        //lseek(gif->fd, size, SEEK_CUR);
        grub_file_seek(gif->file, gif->file->offset + size);
    } while (size);
}

static Table *
new_table(int key_size)
{
    int key;
    int init_bulk = MAX(1 << (key_size + 1), 0x100);
    Table *table = grub_malloc(sizeof(*table) + sizeof(Entry) * init_bulk);
    if (table) {
        table->bulk = init_bulk;
        table->nentries = (1 << key_size) + 2;
        table->entries = (Entry *) &table[1];
        for (key = 0; key < (1 << key_size); key++)
            table->entries[key] = (Entry) {1, 0xFFF, key};
    }
    return table;
}

static void
read_plain_text_ext(struct grub_gif_data *gif)
{
    if (gif->plain_text) {
        grub_uint16_t tx, ty, tw, th;
        grub_uint8_t cw, ch, fg, bg;
        grub_off_t sub_block;
        //lseek(gif->fd, 1, SEEK_CUR); /* block size = 12 */
        grub_file_seek(gif->file, gif->file->offset + 1);
        tx = read_num(gif->file);
        ty = read_num(gif->file);
        tw = read_num(gif->file);
        th = read_num(gif->file);
        grub_file_read(gif->file, &cw, 1);
        grub_file_read(gif->file, &ch, 1);
        grub_file_read(gif->file, &fg, 1);
        grub_file_read(gif->file, &bg, 1);
        grub_file_seek(gif->file, gif->file->offset);
        //sub_block = lseek(gif->file, 0, SEEK_CUR);
        sub_block = gif->file->offset;
        gif->plain_text(gif, tx, ty, tw, th, cw, ch, fg, bg);
        grub_file_seek(gif->file, sub_block);
        //lseek(gif->file, sub_block, SEEK_SET);
    } else {
        /* Discard plain text metadata. */
        //lseek(gif->file, 13, SEEK_CUR);
        grub_file_seek(gif->file, gif->file->offset + 13);
    }
    /* Discard plain text sub-blocks. */
    discard_sub_blocks(gif);
}

static void
read_graphic_control_ext(struct grub_gif_data *gif)
{
    grub_uint8_t rdit;

    /* Discard block size (always 0x04). */
    //lseek(gif->fd, 1, SEEK_CUR);
    grub_file_seek(gif->file, gif->file->offset + 1);
    //gif->file->offset = gif->file->offset + 1;

    //read(gif->fd, &rdit, 1);
    grub_file_read(gif->file, &rdit, 1);
    gif->gce.disposal = (rdit >> 2) & 3;
    gif->gce.input = rdit & 2;
    gif->gce.transparency = rdit & 1;
    gif->gce.delay = read_num(gif->file);
    grub_file_read(gif->file, &gif->gce.tindex, 1);
    /* Skip block terminator. */
    //lseek(gif->file, 1, SEEK_CUR);
    grub_file_seek(gif->file, gif->file->offset + 1);
}

static void
read_comment_ext(struct grub_gif_data *gif)
{
    if (gif->comment) {
        //grub_off_t sub_block = lseek(gif->file, 0, SEEK_CUR);
        grub_off_t sub_block = gif->file->offset;
        //grub_file_seek(sub_block, gif->file->offset);
        gif->comment(gif);
        //lseek(gif->file, sub_block, SEEK_SET);
        grub_file_seek(gif->file, sub_block);
    }
    /* Discard comment sub-blocks. */
    discard_sub_blocks(gif);
}

static void
read_application_ext(struct grub_gif_data *gif)
{
    char app_id[8];
    char app_auth_code[3];

    /* Discard block size (always 0x0B). */
    //lseek(gif->fd, 1, SEEK_CUR);
    gif->file->offset = gif->file->offset + 1;
    /* Application Identifier. */
    grub_file_read(gif->file, app_id, 8);
    /* Application Authentication Code. */
    grub_file_read(gif->file, app_auth_code, 3);
    if (!grub_strncmp(app_id, "NETSCAPE", sizeof(app_id))) {
        /* Discard block size (0x03) and constant byte (0x01). */
        //lseek(gif->file, 2, SEEK_CUR);
        gif->file->offset = gif->file->offset + 2;
        gif->loop_count = read_num(gif->file);
        /* Skip block terminator. */
        //lseek(gif->file, 1, SEEK_CUR);
        gif->file->offset = gif->file->offset + 1;
    } else if (gif->application) {
        //grub_off_t sub_block = lseek(gif->fd, 0, SEEK_CUR);
        grub_off_t sub_block = gif->file->offset;
        gif->application(gif, app_id, app_auth_code);
        //lseek(gif->file, sub_block, SEEK_SET);
        gif->file->offset = gif->file->offset + sub_block;
        discard_sub_blocks(gif);
    } else {
        discard_sub_blocks(gif);
    }
}

static void
read_ext(struct grub_gif_data *gif)
{
    grub_uint8_t label;
    grub_file_read(gif->file, &label, 1);
    switch (label) {
    case 0x01:
        read_plain_text_ext(gif);
        break;
    case 0xF9:
        read_graphic_control_ext(gif);
        break;
    case 0xFE:
        read_comment_ext(gif);
        break;
    case 0xFF:
        read_application_ext(gif);
        break;
    default:
        grub_error(GRUB_ERR_BAD_FILE_TYPE, "unknown extension: %02X\n", label);
    }
}


static void
render_frame_rect(struct grub_gif_data *gif, grub_uint8_t *buffer)
{
    int i, j, k;
    grub_uint8_t index, *color;
    i = gif->fy * gif->image_width + gif->fx;
    for (j = 0; j < gif->fh; j++) {
        for (k = 0; k < gif->fw; k++) {
            index = gif->frame[(gif->fy + j) * gif->image_width + gif->fx + k];
            color = &gif->palette->colors[index*3];
            if (!gif->gce.transparency || index != gif->gce.tindex)
                grub_memcpy(&buffer[(i+k)*3], color, 3);
        }
        i += gif->image_width;
    }
}

static void
dispose(struct grub_gif_data *gif)
{
    int i, j, k;
    grub_uint8_t *bgcolor;
    switch (gif->gce.disposal) {
    case 2: /* Restore to background color. */
        bgcolor = &gif->palette->colors[gif->bgindex*3];
        i = gif->fy * gif->image_width + gif->fx;
        for (j = 0; j < gif->fh; j++) {
            for (k = 0; k < gif->fw; k++)
                grub_memcpy(&gif->canvas[(i+k)*3], bgcolor, 3);
            i += gif->image_width;
        }
        break;
    case 3: /* Restore to previous, i.e., don't update canvas.*/
        break;
    default:
        /* Add frame non-transparent pixels to canvas. */
        render_frame_rect(gif, gif->canvas);
    }
}

static grub_uint16_t
get_key(struct grub_gif_data *gif, int key_size, grub_uint8_t *sub_len, grub_uint8_t *shift, grub_uint8_t *byte)
{
    int bits_read;
    int rpad;
    int frag_size;
    grub_uint16_t key;

    key = 0;
    for (bits_read = 0; bits_read < key_size; bits_read += frag_size) {
        rpad = (*shift + bits_read) % 8;
        if (rpad == 0) {
            /* Update byte. */
            if (*sub_len == 0) {
                grub_file_read(gif->file, sub_len, 1); /* Must be nonzero! */
                if (*sub_len == 0)
                    return 0x1000;
            }
            grub_file_read(gif->file, byte, 1);
            (*sub_len)--;
        }
        frag_size = MIN(key_size - bits_read, 8 - rpad);
        key |= ((grub_uint16_t) ((*byte) >> rpad)) << bits_read;
    }
    /* Clear extra bits to the left. */
    key &= (1 << key_size) - 1;
    *shift = (*shift + key_size) % 8;
    return key;
}

/* Add table entry. Return value:
 *  0 on success
 *  +1 if key size must be incremented after this addition
 *  -1 if could not realloc table */
static int
add_entry(Table **tablep, grub_uint16_t length, grub_uint16_t prefix, grub_uint8_t suffix)
{
    Table *table = *tablep;
    if (table->nentries == table->bulk) {
        table->bulk *= 2;
        table = grub_realloc(table, sizeof(*table) + sizeof(Entry) * table->bulk);
        if (!table) return -1;
        table->entries = (Entry *) &table[1];
        *tablep = table;
    }
    table->entries[table->nentries] = (Entry) {length, prefix, suffix};
    table->nentries++;
    if ((table->nentries & (table->nentries - 1)) == 0)
        return 1;
    return 0;
}

/* Compute output index of y-th input line, in frame of height h. */
static int
interlaced_line_index(int h, int y)
{
    int p; /* number of lines in current pass */

    p = (h - 1) / 8 + 1;
    if (y < p) /* pass 1 */
        return y * 8;
    y -= p;
    p = (h - 5) / 8 + 1;
    if (y < p) /* pass 2 */
        return y * 8 + 4;
    y -= p;
    p = (h - 3) / 4 + 1;
    if (y < p) /* pass 3 */
        return y * 4 + 2;
    y -= p;
    /* pass 4 */
    return y * 2 + 1;
}

/* Decompress image pixels.
 * Return 0 on success or -1 on out-of-memory (w.r.t. LZW code table). */
static int
read_image_data(struct grub_gif_data *gif, int interlace)
{
    grub_uint8_t sub_len, shift, byte;
    int init_key_size, key_size, table_is_full = 0;
    int frm_off, frm_size, str_len = 0, i, p, x, y;
    grub_uint16_t key, clear, stop;
    int ret;
    Table *table;
    Entry entry = {.length = 0, .prefix = 0, .suffix = 0};
    grub_off_t start, end;

    grub_file_read(gif->file, &byte, 1);
    key_size = (int) byte;
    if (key_size < 2 || key_size > 8)
        return -1;
    
    //start = lseek(gif->fd, 0, SEEK_CUR);
    start = gif->file->offset;
    discard_sub_blocks(gif);
    //end = lseek(gif->fd, 0, SEEK_CUR);
    end = gif->file->offset;
    //lseek(gif->fd, start, SEEK_SET);
    //start = grub_file_seek(gif->file, gif->anim_start);
    gif->file->offset = start;
    clear = 1 << key_size;
    stop = clear + 1;
    table = new_table(key_size);
    key_size++;
    init_key_size = key_size;
    sub_len = shift = 0;
    key = get_key(gif, key_size, &sub_len, &shift, &byte); /* clear code */
    frm_off = 0;
    ret = 0;
    frm_size = gif->fw*gif->fh;
    //error?
    while (frm_off < frm_size) {
        if (key == clear) {
            key_size = init_key_size;
            table->nentries = (1 << (key_size - 1)) + 2;
            table_is_full = 0;
        } else if (!table_is_full) {
            ret = add_entry(&table, str_len + 1, key, entry.suffix);
            if (ret == -1) {
                grub_free(table);
                return -1;
            }
            if (table->nentries == 0x1000) {
                ret = 0;
                table_is_full = 1;
            }
        }
        
        key = get_key(gif, key_size, &sub_len, &shift, &byte);
        if (key == clear) continue;
        if (key == stop || key == 0x1000) break;
        if (ret == 1) key_size++;
        entry = table->entries[key];
        str_len = entry.length;
        for (i = 0; i < str_len; i++) {
            p = frm_off + entry.length - 1;
            x = p % gif->fw;
            y = p / gif->fw;
            if (interlace)
                y = interlaced_line_index((int) gif->fh, y);
            gif->frame[(gif->fy + y) * gif->image_width + gif->fx + x] = entry.suffix;
            if (entry.prefix == 0xFFF)
                break;
            else
                entry = table->entries[entry.prefix];
        }
        frm_off += str_len;
        if (key < table->nentries - 1 && !table_is_full)
            table->entries[table->nentries - 1].suffix = entry.suffix;
    }
    grub_free(table);
    if (key == stop)
        grub_file_read(gif->file, &sub_len, 1); /* Must be zero! */
    gif->file->offset = end;
    return 0;
}

/* Read image.
 * Return 0 on success or -1 on out-of-memory (w.r.t. LZW code table). */
static int
read_image(struct grub_gif_data *gif)
{
    grub_uint8_t fisrz;
    int interlace;

    /* Image Descriptor. */
    gif->fx = read_num(gif->file);
    gif->fy = read_num(gif->file);

    if (gif->fx >= gif->image_width || gif->fy >= gif->image_height)
        return -1;
    
    gif->fw = read_num(gif->file);
    gif->fh = read_num(gif->file);
    
    gif->fw = MIN(gif->fw, gif->image_width - gif->fx);
    gif->fh = MIN(gif->fh, gif->image_height - gif->fy);
    
    grub_file_read(gif->file, &fisrz, 1);
    interlace = fisrz & 0x40;
    /* Ignore Sort Flag. */
    /* Local Color Table? */
    if (fisrz & 0x80) {
        /* Read LCT */
        gif->lct.size = 1 << ((fisrz & 0x07) + 1);
        grub_file_read(gif->file, gif->lct.colors, 3 * gif->lct.size);
        gif->palette = &gif->lct;
    } else
        gif->palette = &gif->gct;
    /* Image Data. */
    
    return read_image_data(gif, interlace);
}

/* Return 1 if got a frame; 0 if got GIF trailer; -1 if error. */
static int grub_get_frame(struct grub_gif_data *gif)
{
    char sep;
    dispose(gif);
    grub_file_read(gif->file, &sep, 1);
    while (sep != ',') {
        if (sep == ';')
            return 0;
        if (sep == '!')
            read_ext(gif);
        else return -1;
        grub_file_read(gif->file, &sep, 1);
    }
    if (read_image(gif) == -1){
        return -1;
    }
    return 1;
}

static struct grub_gif_data *
grub_gif_decode_gif (struct grub_gif_data *data)
{
    grub_uint8_t magic[6];
    grub_uint16_t width, height, depth;
    grub_uint8_t fdsz, bgidx, aspect;
    int i;
    grub_uint8_t *bgcolor;
    int gct_sz;
    struct grub_gif_data * grub_gif;
    //int ret;

    if (grub_file_read (data->file, &magic[0], 6) != 6){
        goto fail;
        //return grub_errno;
    }

    if (grub_memcmp (magic, gif_magic, sizeof (gif_magic)))
        goto fail;
        //return grub_error (GRUB_ERR_BAD_FILE_TYPE, "gif: not a gif file");

    width = read_num(data->file);
    height = read_num(data->file);
    /* FDSZ */
    grub_file_read (data->file, &fdsz, 1);
    /* Presence of GCT */
    if(!(fdsz & 0x80)){
        goto fail;
    }
    /* Color Space's Depth */
    depth = ((fdsz >> 4) & 7) + 1;
    /* Ignore Sort Flag. */
    /* GCT Size */
    gct_sz = 1 << ((fdsz & 0x07) + 1);
    /* Background Color Index */
    grub_file_read (data->file, &bgidx, 1);
    /* Aspect Ratio */
    grub_file_read (data->file, &aspect, 1);
    /* Create grub_gif Structure. */

    grub_gif = grub_calloc(1, sizeof(* grub_gif));

    if(!grub_gif){
        goto fail;
    }
    grub_gif->file = data->file;
    grub_gif->image_width = width;
    grub_gif->image_height = height;
    grub_gif->depth = depth;
    grub_gif->gct.size = gct_sz;
    grub_file_read(data->file, &grub_gif->gct.size, 3 * grub_gif->gct.size);
    grub_gif->palette = &grub_gif->gct;
    grub_gif->bgindex = bgidx;
    grub_gif->frame = grub_calloc(4, width * height);

    if (!grub_gif->frame) {
        grub_free(grub_gif);
        goto fail;
    }

    grub_gif->canvas = &grub_gif->frame[width * height];

    if (grub_gif->bgindex)
        grub_memset(grub_gif->frame, grub_gif->bgindex, grub_gif->image_width * grub_gif->image_height);
    bgcolor = &grub_gif->palette->colors[grub_gif->bgindex*3];

    if (bgcolor[0] || bgcolor[1] || bgcolor [2])
        for (i = 0; i < grub_gif->image_width * grub_gif->image_height; i++)
        grub_memcpy(&grub_gif->canvas[i*3], bgcolor, 3);
    grub_gif->anim_start = grub_gif->file->offset;//grub_file_seek(grub_gif->file, data->file->offset);

    goto ok;

fail:
    return 0;
ok:
	return grub_gif;
}

static void
grub_render_frame(struct grub_gif_data *gif, grub_uint8_t *buffer)
{
    grub_memcpy(buffer, gif->canvas, gif->image_width * gif->image_height * 3);
    render_frame_rect(gif, buffer);
}

static int
gd_is_bgcolor(struct grub_gif_data *gif, grub_uint8_t color[3])
{
    return !grub_memcmp(&gif->palette->colors[gif->bgindex*3], color, 3);
}

#if defined(LOOP)
static void
gd_rewind(struct grub_gif_data *gif)
{
    gif->file->offset = gif->anim_start;
}
#endif

static grub_err_t
grub_video_reader_gif (struct grub_video_bitmap **bitmap,
		       const char *filename)
{
    int ret;
    int i, j;
    grub_file_t file;
    struct grub_gif_data * data;
    struct grub_gif_data * gif;
    grub_uint8_t *color, *frame;
    grub_video_color_t video_color;

    file = grub_buffile_open (filename, GRUB_FILE_TYPE_PIXMAP, 0);
    if (!file)
        return grub_errno;

    data = grub_zalloc (sizeof (*data));
    if (data == NULL){
        return grub_errno;
    }

    data->file = file;
    data->bitmap = bitmap;

    gif = grub_gif_decode_gif(data);

    frame = grub_malloc(gif->image_width * gif->image_height * 3);
    if (!frame) {
        return grub_errno;
    }
    
    if(!gif){
        return grub_errno;
    }

    color = &gif->gct.colors[gif->bgindex * 3];
    video_color = grub_video_map_rgb (color[0], color[1], color[2]);
    grub_video_fill_rect (video_color, 0, 0, gif->image_width, gif->image_height);
    grub_video_swap_buffers ();

    while(1){
        grub_sleep(0.001);
        ret = grub_get_frame(gif);

        if (ret == -1)
        {
            grub_free(data);
            grub_free(gif);

            if (grub_errno != GRUB_ERR_NONE){
                grub_video_bitmap_destroy (*bitmap);
                *bitmap = 0;
            }

            grub_file_close (file);
            return grub_errno;
        }

        grub_render_frame(gif, frame);

        color = frame;

        for(i = 0; i < gif->image_height; i++) {
            for (j = 0; j < gif->image_width; j++) {
                if (!gd_is_bgcolor(gif, color))
                {
                    video_color = grub_video_map_rgb (color[0], color[1], color[2]);
                    grub_video_fill_rect (video_color, j, i, 1, 1);
                } 
                else if(((i >> 2) + (j >> 2)) & 1)
                {
                    video_color = grub_video_map_rgb (0x7F, 0x7F, 0x7F);
                    grub_video_fill_rect (video_color, j, i, 1, 1);
                } 
                else
                {
                    video_color = grub_video_map_rgb (0x00, 0x00, 0x00);
                    grub_video_fill_rect (video_color, j, i, 1, 1);
                }
                color += 3;
            }
        }
        grub_video_swap_buffers ();

        if (ret == 0){
            if(LOOP){
                gd_rewind(gif);
            }else{
                break;
            }
        }
    }
    
    grub_free(data);
    grub_free(gif);
    
    if (grub_errno != GRUB_ERR_NONE){
        grub_video_bitmap_destroy (*bitmap);
        *bitmap = 0;
    }

    grub_file_close (file);
    return grub_errno;
}


#if defined(GIF_DEBUG)
static grub_err_t
grub_cmd_giftest (grub_command_t cmd_d __attribute__ ((unused)),
		  int argc, char **args)
{
    struct grub_video_bitmap *bitmap = 0;

    if (argc != 1)
        return grub_error (GRUB_ERR_BAD_ARGUMENT, N_("filename expected"));

    grub_video_reader_gif (&bitmap, args[0]);
    
    if (grub_errno != GRUB_ERR_NONE)
        return grub_errno;

    grub_video_bitmap_destroy (bitmap);

    return GRUB_ERR_NONE;
}
#endif

#if defined(GIF_RELEASE)
static struct grub_video_bitmap_reader gif_reader = {
  .extension = ".gif",
  .reader = grub_video_reader_gif,
  .next = 0
};
#endif

GRUB_MOD_INIT(gif)
{
#if defined(GIF_DEBUG)
    cmd = grub_register_command ("giftest", grub_cmd_giftest,
                                "FILE",
                                "Tests loading of GIF bitmap.");
#endif

#if defined(GIF_RELEASE)
    grub_video_bitmap_reader_register (&gif_reader);
#endif
}

GRUB_MOD_FINI(gif)
{
#if defined(GIF_DEBUG)
    grub_unregister_command (cmd);
#endif

#if defined(GIF_RELEASE)
    grub_video_bitmap_reader_unregister (&gif_reader);
#endif
}
