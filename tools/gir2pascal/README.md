
gir2pascal
==========

This is my personal fork of the *gir2pascal* utility, a program to convert the
GIR metadata format (= XML files) used by [GObject introspection][] into usable
Pascal source code, suitable for generating corresponding language bindings.

The original is part of the Lazarus Code and Component Repository ([lazarus-ccr][],
[wiki article][wiki-ccr]), see `applications/gobject-introspection/`.  

I started working on this after I found a number of problems and shortcomings of
the original *gir2pascal*, when trying to process the API description of [Poppler]
version 0.82 (`Poppler-0.18.gir`), which also includes a number of dependencies
(namely `GObject-2.0.gir`, `GLib-2.0.gir`, `Gio-2.0.gir` and `cairo-1.0.gir`).
It's faily obvious that *gir2pascal* wasn't updated for quite some time, and it
struggled with the current GIR format (e.g. not knowing about and thus showing
numerous warnings for the `<source-position>` element). It also failed to handle
the 'opaque' data types that an up-to-date `GLib-2.0.gir` seems to be using for
its mutex locking.

Note: 
My changes were developed and tested using [fpc][] 3.0.4 and the default `*.gir`
files that [Gentoo][] Linux installs for `poppler-0.82` and `glib-2.60.7`. The
modified *gir2pascal* handles `Poppler-0.18.gir` and all its dependencies just
fine, and the resulting `*.pas` files compile without errors. However, because
I later decided to take a different approach (not using Poppler), I haven't
checked if the generated API bindings are actually usable - so consider that
part **untested**...


---
You can create a (set of) patch(es) from my branch, which should apply cleanly
to the current (r6630) 'upstream' repository folder.

	git format-patch master..nitehawk


License
-------

This project builds upon the original Lazarus CCR version and is thus intended
to follow the same licensing principles. For the `gobject-introspection` folder
this seems to be [GPL v2](LICENSE.md), as referenced in a number of file headers.


[GObject introspection]: https://gi.readthedocs.io/
[lazarus-ccr]: https://sourceforge.net/projects/lazarus-ccr/
[wiki-ccr]: https://wiki.lazarus.freepascal.org/Lazarus-ccr_SourceForge_repository
[poppler]: https://poppler.freedesktop.org/
[fpc]: https://freepascal.org/
[Gentoo]: https://gentoo.org/