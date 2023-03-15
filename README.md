# grub_gifdec

## 运行和编译环境
grub 2.6.0

## 使用方法
将gif.c文件复制到grub/grub-core/video/readers/目录下。之后修改grub/grub-core/Makefile.core.def文件，添加以下代码

``` text
module = {
  name = gif;
  common = video/readers/gif.c;
};
```
之后编译grub项目就可生成gif.mod文件，之后复制到/boot/grub/{目标型号}/目录下，我的{目标型号}是i386-pc。

## 各种叠甲
这是一个很垃圾的项目，花了两个星期弄的，只是将gifdec的代码放到grub项目里。让他成功运行了而已，而且效果很差，只是突发奇想，想试试让grub能解析并显示gif。此项目不知道下次更新需要多久，大三了还这么多课很无语。别再真机上实验，开一个虚拟机。