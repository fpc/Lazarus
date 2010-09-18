
This little application allows to view JSON data in a graphical tree.
Each array and object is represented in the tree with the members (or
elements) below them.

It should be compilable with FPC version 2.4.0 and up.

The data can be loaded from a file, or can be pasted from the clipboard.
(Edit|Paste as new document)

It is possible to edit the JSON (change member names, values), add items,
and save the data. It is also possible to copy&paste parts of the structure.

Options menu:

When compiled with FPC 2.5.1 or higher, the 'Strict JSON' option appears
under the 'Options' menu. This option will be off by default (matching
TJSONParser's behaviour). See fpc/packages/fcl-json/src/README.txt 
for an explanation of the Strict JSON property.

The 'Start new document with Object' option will cause the application to
create an object as root object when File-New is chosen.

The 'Sort member names' will sort the member names in an object
alphabetically.

