This package implements project templates.

Installing this package in the IDE adds a category to the 'File-New' dialog:
'Template projects'

It also adds a 'New project from Template' menu item to the 'File' menu, as
well as a 'project template options' menu under the 'Tools' menu.
This menu item is visible only if the templates directory (see next paragraph)
exists and contains at least one template subdirectory.

In the 'Project template options', a directory can be selected. This directory
should contain a subdirectory per template. Each template directory contains
a template for a project: A collection of files which will be copied and
used as a new project.

During the copy, the package will scan the file/directory names and the 
contents of the files for variable substitution: __VARNAME__ will be 
replaced by the value of a variable VARNAME

By default, the engine knows 2 variables:
ProjDir  : The directory where the new project will be created.
ProjName : The name of the project.

Additional variables must be defined in a file called 'project.ini' in the
directory of the project. This is a file in Windows INI format.

The section [Variables] will be scanned for variable names:
[Variables]
VarName1=Description 1
VarName2=Description 2

The variables found will be presented with their description in a dialog, 
and the user must present a value for the variables.

The 'project.ini' file can contain a second section, called 'Project', which
can contain some info about the project. The following keywords can be found
there:

ProjectFile
  Any file that has a name equal to this (no extension) is treated specially, 
  it is renamed to the project name. That is 
  ProjectFile=example
  will replace example.lpi example.lpr and example.cfg with the name given
  by the user. By default, the value of ProjectFile is assumed to be 'project'.
  (note that the .ini file is not copied)
 
Name
  Name of the template 
Author
  Author of the template
Description
  Short (one-line) description of the project
Recurse
  A boolean value (1/0) which tells the engine to recurse in subdirectories
  or not.
Exclude
  Comma separated list of filename extensions which should not be searched
  for keyword subsitution.

The Name and description will be presented in the 'File-New' dialog.
