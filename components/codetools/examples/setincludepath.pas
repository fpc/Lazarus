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
begin
  // create a template for the current directory
  // all child nodes of this templates are only valid for this directory.
  Directory:=ExpandFileNameUTF8(GetCurrentDirUTF8);
  DirectoryTemplate:=TDefineTemplate.Create('Current working directory',
    'Example template for current working directory','',Directory,da_Directory);

  // add a sub template to extend the include search path #IncPath.
  IncPathTemplate:=TDefineTemplate.Create('Add myincludes to the IncPath',
    'Add myincludes to the include search path',
    IncludePathMacroName,  // variable name: #IncPath
    '$('+IncludePathMacroName+');myincludes' // new value: $(#IncPath);myincludes
    ,da_Define
    );
  DirectoryTemplate.AddChild(IncPathTemplate);
  
  // add the directory template to the tree
  CodeToolBoss.DefineTree.Add(DirectoryTemplate);

  writeln('Directory="',Directory,'"',
    ' IncPath="',CodeToolBoss.GetIncludePathForDirectory(Directory,true),'"');
end.

