This project demonstrates how to write and use your own docking manager.
It's a Lazarus adaptation of a Delphi 7 project, which also can be used to
demonstrate remaining LCL drag-drop flaws.

The docking manager itself resides in EasyDockSite.pas and EasyDockHelpers.pas,
ready for use. Add EasyDockSite.pas to the uses list of your forms,
and set the DockManagerClass property to EasyTree for every dock site (form or control).

fMain is the project main form,
fTree can show an dump of the internal tree structure (in debug mode),
fDockable is a sample floating dock site.

More information can be found at:
http://wiki.lazarus.freepascal.org/LCL_Drag_Drop
See also:
http://wiki.lazarus.freepascal.org/Anchor_Docking

Configuration Options
---------------------
Various test scenarios can be selected by $defines.
In fMain:
- docker: use panel or entire form as docksite
- easy: use default or EasyTree dockmanager
- dragform: dragging controls or forms (forms not on all platforms)

In EasyDockSite:
- NoDrop: required patch applied to the dragmanager?
- splitter_color: use special color for splitter (debug splitters)

In EasyDockHelpers:
- restore: show restore button (purpose?)

ToDos:
At the time of writing this README the following does not yet work:

1. Layouts can not be saved and restored (to be copied from LDock)

2. Hidden (invisible) controls are not handled.

3. Notebooks do not initialize properly (resize the form to fix that)

4. Captions can be empty?

5. Dropped controls do not preserve their original extent?

