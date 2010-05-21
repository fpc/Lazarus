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
- elastic docksites
- multiple editor forms
- persistent layouts with special form handling

ide_demo2/MiniIDE2
==================
A Lazarus-like IDE with:
- dockable windows, including editor forms
- persistent layouts with special form handling

In contrast to MiniIDE this project only uses dockable forms,
floating in a hostsite that allows to dock multiple forms together.
An editor form only is one of such forms, special only in the save/reload of its content.
An elastic site is created only for the MainBar, to allow for monolithic (single-form) layouts.


toolbar/test1
=============
mainly demonstrates TToolBar/TToolButton related problems.
(see provided patches)
