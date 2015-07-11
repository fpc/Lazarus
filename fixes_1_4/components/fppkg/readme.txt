This is a graphical front end for fppkg, the freepascal package manager. This application is meant to 
be integrated tightly with Lazarus. Providing an easy system to search and add packages to the FPC and the IDE.

In the ./src folder there is a .lpk file that will register the tool in the IDE. From there you can start
the package manager. Usage is pretty self explanatory, you can search and filter out packages. Then you 
can mark them for installation and once you're done you can apply installation.

The package manager will display icons in front of packages indicating the support level for the package.
Packages with an FPC icon are officially supported by FPC and bugs can be submitted to FPC. The same
for official Lazarus packages.

It's also possible to compile the package manager as a stand alone tool.