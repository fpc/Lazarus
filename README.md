Welcome to Lazarus
==================

Lazarus is a Rapid Application Development Tool for Free Pascal.
It comes with the LCL - Lazarus component library, which contains platform
independent visual components like buttons, windows, checkbox, treeview and
many, many more. The LCL is platform independent, so you can write an
application once and then compile for various platforms without changing code.

[Free Pascal](https://www.freepascal.org) is a fast Object Pascal compiler,
that runs on more than 20 platforms (Linux, Windows, BSD, OS/2, DOS, PowerPC,
and many more).

The LCL currently supports:
* Linux/FreeBSD (Gtk2, Gtk3, Qt4, Qt5, Qt6)
* all flavors of Windows (even WinCE)
* macOS (Cocoa, Carbon, Gtk2, Qt4, Qt5, Qt6)

The LCL still contains code for Gtk1 (although this target is obsolete).

### Compilation

You don't need ```./configure```, just do  
```make clean bigide``` (```gmake clean bigide``` in BSD).

This will create the Lazarus executable with a lot of packages.
Start it and enjoy.

If the above gives an error, you can try to build a minimal IDE with  
```make clean all``` (```gmake clean all``` in BSD).

### Installation and Requirements

See [Lazarus Wiki](https://wiki.freepascal.org/Category:Install) for details.

### Usage

Start the IDE with:
```shell
cd your/lazarus/directory
./lazarus
```

### Documentation

The official site is www.lazarus-ide.org.
Documents about specific topics can be found at 
https://wiki.freepascal.org/Lazarus_Documentation.
Examples on how to use the LCL can be found in the [examples](examples) directory.
Help, documents and files about Free Pascal are at www.freepascal.org.

### Mailing list and Forum

There are very active and helpful forum and mailing list for Lazarus, where the
developers interact, share ideas, discuss problems, and of course answer
questions.

You can subscribe to mailing list at
http://lists.lazarus.freepascal.org/mailman/listinfo/lazarus.

Forum is available at
https://forum.lazarus.freepascal.org/.

### How to help Lazarus

If you find bugs, don't hesitate to use issue tracker,
or send an email to the list.
Lazarus source code and issue tracker are located at [GitLab](https://gitlab.com/freepascal.org/lazarus/lazarus).
