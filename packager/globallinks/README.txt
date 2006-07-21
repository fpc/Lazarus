This directory is used for global "package links".

"Package links" are used by the IDE whenever it did not found the .lpk file of a
required package. The files should have filenames with the format
<pkgname>-<version>.lpl.
Where <pkgname> is a valid pascal identifier and <version> is for example
1.2.3.4 (one to four integers divided by point).
Each file should contain a single absolute filename. The filename can contain
macros.


