{
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

  Author: Mattias Gaertner

  Abstract:
    Demo for automatic fixing the filenames of include directives and uses
    section.
}
program FixFilenames;

{$mode objfpc}{$H+}

uses
  Classes, SysUtils, DefineTemplates, CodeToolsConfig,
  CodeToolManager, CodeCache;

const
  ConfigFilename = 'codetools.config';
var
  Options: TCodeToolsOptions;
  Code: TCodeBuffer;
  Filename: String;
begin
  // setup the Options
  Options:=TCodeToolsOptions.Create;

  // To not parse the FPC sources every time, the options are saved to a file.
  if FileExists(ConfigFilename) then
    Options.LoadFromFile(ConfigFilename);

  // setup your paths
  Options.FPCPath:='/usr/bin/ppc386';
  Options.FPCSrcDir:=ExpandFileName('~/freepascal/fpc');
  Options.LazarusSrcDir:=ExpandFileName('~/pascal/lazarus');

  // optional: ProjectDir and TestPascalFile exists only to easily test some
  // things.
  Options.ProjectDir:=GetCurrentDir+'/scanexamples/';
  Options.TestPascalFile:=Options.ProjectDir+'simpleunit1.pas';

  // init the codetools
  if not Options.UnitLinkListValid then
    writeln('Scanning FPC sources may take a while ...');
  CodeToolBoss.Init(Options);

  // save the options and the FPC unit links results.
  Options.SaveToFile(ConfigFilename);

  // load the example unit
  Filename:=ExpandFileName('scanexamples/brokenfilenames.pas');
  Code:=CodeToolBoss.LoadFile(Filename,false,false);
  if Code=nil then
    raise Exception.Create('unable to read '+Filename);
    
  // fix the filenames in the include directives
  if not CodeToolBoss.FixIncludeFilenames(Code,true) then
    raise Exception.Create('unable to fix include filesnames in '+Filename+' '+CodeToolBoss.ErrorMessage);

  // fix the unitnames in the uses section
  //if not CodeToolBoss.FixUsesSectionsCase(Code) then
  //  raise Exception.Create('unable to fix unit names in '+Filename+' '+CodeToolBoss.ErrorMessage);
  writeln(Code.Source);
end.

