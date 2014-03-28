{ Copyright (C) 2005 Mattias Gaertner

 ***************************************************************************
 *                                                                         *
 *   This source is free software; you can redistribute it and/or modify   *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 *   This code is distributed in the hope that it will be useful, but      *
 *   WITHOUT ANY WARRANTY; without even the implied warranty of            *
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU     *
 *   General Public License for more details.                              *
 *                                                                         *
 *   A copy of the GNU General Public License is available on the World    *
 *   Wide Web at <http://www.gnu.org/copyleft/gpl.html>. You can also      *
 *   obtain it by writing to the Free Software Foundation,                 *
 *   Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.        *
 *                                                                         *
 ***************************************************************************

  Abstract:
    Example, how to setup the codetools, load a pascal unit and
    extend the include path of a directory.
}
program SetIncludePath;

{$mode objfpc}{$H+}

uses
  Classes, SysUtils, CodeToolManager, DefineTemplates, FileProcs;
  
var
  Directory: String;
  DirectoryTemplate: TDefineTemplate;
  IncPathTemplate: TDefineTemplate;
  SubDirectory: String;
begin
  // create a template for the current directory
  // all child nodes of this template are only valid for this directory.
  Directory:=ExpandFileNameUTF8(GetCurrentDirUTF8);
  DirectoryTemplate:=TDefineTemplate.Create('Current working directory',
    'Example template for current working directory','',Directory,da_Directory);

  // Example 1:
  //  Add a sub template to extend the include search path #IncPath
  //  only for the current directory, not its sub directories.
  // Note: 'myincludes' is a relative path, so it won't work for sub directories.
  IncPathTemplate:=TDefineTemplate.Create('Add myincludes to the IncPath',
    'Add myincludes to the include search path',
    IncludePathMacroName,  // variable name: #IncPath
    IncludePathMacro+';myincludes' // new value: $(#IncPath);myincludes
    ,da_Define // da_Define extends the IncPath only for this directory
    );
  DirectoryTemplate.AddChild(IncPathTemplate);

  // Example 2:
  //  Add a sub template to extend the include search path #IncPath
  //  with an absolute path for the current directory and all its sub directories.
  // Note: '/tmp/myincludes' is an absolute path, so works for sub directories.
  IncPathTemplate:=TDefineTemplate.Create('Add /tmp/myincludes to the IncPath',
    'Add /tmp/myincludes to the include search path',
    IncludePathMacroName,  // variable name: #IncPath
    IncludePathMacro+';'+SetDirSeparators('/tmp/myincludes') // new value: $(#IncPath);/tmp/myincludes
    ,da_DefineRecurse // da_DefineRecuse extends the IncPath for this directory and all sub directories
    );
  DirectoryTemplate.AddChild(IncPathTemplate);

  // Example 3:
  //  Using the #DefinePath macro you can use the current directory to create absolute paths.
  IncPathTemplate:=TDefineTemplate.Create('Add ./myincludes to the IncPath',
    'Add ./myincludes to the include search path',
    IncludePathMacroName,  // variable name: #IncPath
    IncludePathMacro+';'+DefinePathMacro+'/myincludes' // new value: $(#IncPath);$(#DefinePath)/myincludes
    ,da_DefineRecurse // da_DefineRecuse extends the IncPath for this directory and all sub directories
    );
  DirectoryTemplate.AddChild(IncPathTemplate);

  // add the directory template to the tree
  CodeToolBoss.DefineTree.Add(DirectoryTemplate);

  writeln('Directory="',Directory,'"',
    ' IncPath="',CodeToolBoss.GetIncludePathForDirectory(Directory),'"');
  SubDirectory:=AppendPathDelim(Directory)+'sub';
  writeln('SubDirectory="',SubDirectory,'"',
    ' IncPath="',CodeToolBoss.GetIncludePathForDirectory(SubDirectory),'"');
end.

