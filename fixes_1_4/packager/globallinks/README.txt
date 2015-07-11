This directory is used for global "package links".

"Package links" are used by the IDE whenever it did not found the .lpk file of a
required package. The files should have filenames with the format
<pkgname>-<version>.lpl.
Where <pkgname> is a valid pascal identifier and <version> is for example
1.2.3.4 (trailing .0 can be omitted, but at least one number is required).
For example if you have a package abc with version 0.1.0.0 you can create a link
abc-0.1.lpl or abc-0.1.0.lpl or abc-0.1.0.0.lpl.
Each file should contain a single absolute filename. The filename can contain
macros.

To update the lpl files use the tool tools/lplupdate
