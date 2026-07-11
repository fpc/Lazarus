# IDE/Packages

This folder contains packages that are part of the core Lazarus IDE project.

The content of those packages will be treated the same as files in the actual project. Sources, including interface-sections can freely change (without announcement), as long as the projects in the IDE folder keep working. (Those are lazarus.lpi, lazbuild.lpi, startlazarus.lpi).

While you are free to use the files according to their license, your code may fail to run/compile at anytime due to changes in those files. And this is considered as "By Design".


If you are writing extensions to the IDE, you are encouraged to use the IdeIntf (and other *Intf) packages.