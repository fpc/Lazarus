This is a demonstration of the anchor docking manager of the LCL.
The docking is not yet complete in the LCL and so is the docking manager, so
don't expect too much. It can already dock forms side by side, separated by
splitters, dock in pages (like a TPageControl) and change the docking via popup
menu.

ToDos:
At the time of writing this README the following does not work:

1. For Windows users:
Docking does not work yet under the win32/64 widgetset. The forms are docked,
but the focus and mouse messages are not delegated correctly.

2. Docking forms with menus.

3. Restore layouts with pages.

4. Restore layouts with spiral splitters

5. Drag and Drop docking.


More information can be found at
http://wiki.lazarus.freepascal.org/Anchor_Docking

