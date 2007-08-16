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
    Demonstrating, how to add invoke identifier completion.
}
program IdentifierCompletion;

{$mode objfpc}{$H+}

uses
  Classes, SysUtils, CodeCache, CodeToolManager, FileProcs,
  CodeToolsConfig, IdentCompletionTool;

const
  ConfigFilename = 'codetools.config';
var
  Options: TCodeToolsOptions;
  Filename: string;
  Code: TCodeBuffer;
  X: Integer;
  Y: Integer;
  //Tool: TCodeTool;
begin
  if (ParamCount>=1) and (Paramcount<3) then begin
    writeln('Usage:');
    writeln('  ',ParamStr(0));
    writeln('  ',ParamStr(0),' <filename> <X> <Y>');
  end;

  // setup the Options
  Options:=TCodeToolsOptions.Create;

  // To not parse the FPC sources every time, the options are saved to a file.
  writeln('Config=',ConfigFilename);
  if FileExists(ConfigFilename) then begin
    Options.LoadFromFile(ConfigFilename);
  end else begin
    Options.InitWithEnvironmentVariables;
    if Options.FPCPath='' then
      Options.FPCPath:='/usr/bin/ppc386';
    if Options.FPCSrcDir='' then
      Options.FPCSrcDir:=ExpandFileName('~/freepascal/fpc');
    if Options.LazarusSrcDir='' then
      Options.LazarusSrcDir:=ExpandFileName('~/pascal/lazarus');
  end;

  // optional: ProjectDir and TestPascalFile exists only to easily test some
  // things.
  Options.ProjectDir:=GetCurrentDir+'/scanexamples/';
  Options.TestPascalFile:=Options.ProjectDir+'identcomplexample.pas';
  X:=20;
  Y:=10;

  // init the codetools
  if not Options.UnitLinkListValid then
    writeln('Scanning FPC sources may take a while ...');
  CodeToolBoss.Init(Options);

  // save the options and the FPC unit links results.
  Options.SaveToFile(ConfigFilename);

  writeln('FPCSrcDir=',Options.FPCSrcDir);
  writeln('FPC=',Options.FPCPath);
  if (ParamCount>=3) then begin
    Options.TestPascalFile:=ExpandFileName(ParamStr(1));
    X:=StrToInt(ParamStr(2));
    Y:=StrToInt(ParamStr(3));
  end;

  // load the file
  Filename:=Options.TestPascalFile;
  Code:=CodeToolBoss.LoadFile(Filename,false,false);
  if Code=nil then
    raise Exception.Create('loading failed '+Filename);

  //CodeToolBoss.Explore(Code,Tool,false,true);
  //Tool.WriteDebugTreeReport;

  // gather identifiers:
  writeln('GatherIdentifiers ',Code.Filename,'(X=',X,',Y=',Y,')');
  if CodeToolBoss.GatherIdentifiers(Code,X,Y) then
  begin
    writeln('Identifiers found: Count=',CodeToolBoss.IdentifierList.Count);
  end else begin
    raise Exception.Create('GatherIdentifiers failed');
  end;
end.

