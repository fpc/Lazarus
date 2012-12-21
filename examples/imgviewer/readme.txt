Image viewer
============

This example application shows how to load and show image/graphical files.

It also demonstrates
- scaling images
- using the Lazarus functions FindFirstUTF8 and FindNextUTF8 to recursively seek files and directories
- dealing with key presses using the KeyDown event
- using BeginUpdate and EndUpdate to improve processing speed of certain controls (a ListBox in this case)
- setting the cursor to hourglass and reset it to indicate a long-running operation is going on (e.g. when recursively loading directories with a large amount of images)

Possible improvements:
- add support for other file formats
- add a setting that allows automatic scaling down if a picture is larger than the control
- use a cache in a different thread to preload images the user is likely to look at next