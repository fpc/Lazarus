# gir2pas

The [gir2pas][] utility is a program to convert the
GIR metadata format (= XML files) used by [GObject introspection][] into usable
Pascal source code, suitable for generating corresponding language bindings.

It was originaly named [gir2pascal] maintained as part of the Lazarus Code and Component Repository ([lazarus-ccr][],
[wiki article][wiki-ccr]), see `applications/gobject-introspection/`. Later it has
received some maintenance by [n1tehawk][] and finally was imported from his
repository into Lazarus source tree.

# License

This project builds upon the original Lazarus CCR version and is thus intended
to follow the same licensing principles. For the `gobject-introspection` folder
this seems to be [GPL v2](LICENSE.md), as referenced in a number of file headers.


[gir2pascal]: https://wiki.freepascal.org/gir2pascal
[GObject introspection]: https://gi.readthedocs.io/
[lazarus-ccr]: https://sourceforge.net/projects/lazarus-ccr/
[wiki-ccr]: https://wiki.lazarus.freepascal.org/Lazarus-ccr_SourceForge_repository
[n1tehawk]: https://github.com/n1tehawk/gir2pascal
