The examples/dockmanager directory contains several projects,
provided by DoDi <DrDiettrich1@aol.com>

patches/
========
contains some patches, which may not yet have found their way into the LCL.

package/easydocking
===================
contains an tree docking manager, with notebook docking capabilities.
This package is used by the other projects.

easytree/easydocking
====================
demonstrates the EasyDockSite manager features.

easyedit/easyeditor
===================
demonstrates an multi-window editor with dockable pages (files).

elasticsite/project1
====================
demonstrates elastic dock sites, which become visible only after a control
has been docked into them.

elasticsite/SiteTest
====================
demonstrates elastic panels and dockable forms.

elasticsite/MakeSite
====================
demonstrates persistent layouts, using default features.
For special forms see ide_demo/MiniIDE

ide_demo/MiniIDE
================
A Lazarus-like IDE with:
- dockable windows
- multiple editor forms
- persistent layouts with special form handling

Loading a layout should not affect files in the editors, because these are project specific. This can result in empty editor windows, for now.
More features to come...


toolbar/test1
=============
mainly demonstrates TToolBar/TToolButton related problems.
(see provided patches)
