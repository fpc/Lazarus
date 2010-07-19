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
    Simple demonstrating, how to setup the codetools, FPC and Lazarus Source
    directory to find a declaration.
}
program FindDeclaration;

{$mode objfpc}{$H+}

uses
  Classes, SysUtils, CodeCache, CodeToolManager, DefineTemplates,
  CodeToolsConfig, SimpleUnit1;

const
  ConfigFilename = 'codetools.config';
var
  Options: TCodeToolsOptions;
  NewCode: TCodeBuffer;
  NewX, NewY, NewTopLine: integer;
  Code: TCodeBuffer;
  X: Integer;
  Y: Integer;
  Filename: String;
begin
  if (ParamCount>=1) and (Paramcount<3) then begin
    writeln('Usage:');
    writeln('  ',ParamStr(0));
    writeln('  ',ParamStr(0),' <filename> <X> <Y>');
  end;
  
  // setup the Options
  Options:=TCodeToolsOptions.Create;
  try
    // setup your paths
    writeln('Config=',ConfigFilename);
    if FileExists(ConfigFilename) then begin
      // To not parse the FPC sources every time, the options are saved to a file.
      Options.LoadFromFile(ConfigFilename);
    end;
    Options.InitWithEnvironmentVariables;
    if Options.FPCSrcDir='' then
      Options.FPCSrcDir:=ExpandFileName('~/freepascal/fpc');
    if Options.LazarusSrcDir='' then
      Options.LazarusSrcDir:=ExpandFileName('~/pascal/lazarus');

    // optional: ProjectDir and TestPascalFile exists only to easily test some
    // things.
    Options.ProjectDir:=SetDirSeparators(GetCurrentDir+'/scanexamples/');
    Filename:=Options.ProjectDir+'simpleunit1.pas';

    // init the codetools
    if not Options.UnitLinkListValid then
      writeln('Scanning FPC sources may take a while ...');
    CodeToolBoss.Init(Options);

    // save the options and the FPC unit links results.
    Options.SaveToFile(ConfigFilename);

    // Example: find declaration of 'TObject'
    X:=5;
    Y:=43;

    writeln('FPCDIR=',Options.FPCSrcDir);
    writeln('PP=',Options.FPCPath);
    writeln('LAZARUSDIR=',Options.LazarusSrcDir);
    writeln('TARGET=',Options.TargetOS);
    writeln('TARGETCPU=',Options.TargetProcessor);
    if (ParamCount>=3) then begin
      Options.TestPascalFile:=ExpandFileName(ParamStr(1));
      X:=StrToInt(ParamStr(2));
      Y:=StrToInt(ParamStr(3));
    end;

    // Step 1: load the file
    Code:=CodeToolBoss.LoadFile(Filename,false,false);
    if Code=nil then
      raise Exception.Create('loading failed '+Filename);

    // Step 2: find declaration
    if CodeToolBoss.FindDeclaration(Code,X,Y,NewCode,NewX,NewY,NewTopLine) then
    begin
      writeln('Declaration found: ',NewCode.Filename,' Line=',NewY,' Column=',NewX);
    end else begin
      writeln('Declaration not found: ',CodeToolBoss.ErrorMessage);
    end;
  except
    on E: Exception do begin
      writeln('Error: ',E.Message);
    end;
  end;
  Options.Free;
end.

