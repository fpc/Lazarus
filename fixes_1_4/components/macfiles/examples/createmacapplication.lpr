{
 *****************************************************************************
 *                                                                           *
 *  This file is part of the Lazarus Component Library (LCL)                 *
 *                                                                           *
 *  See the file COPYING.LCL, included in this distribution,                 *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************

  Author: Mattias Gaertner
}
program CreateMacApplication;

{$mode objfpc}{$H+}

uses
  Classes, SysUtils, CustApp, MacOSFiles, MacApplicationRes, FileUtil;
  
type

  { TCreateMacAppFolderApplication }

  TCreateMacAppFolderApplication = class(TCustomApplication)
  public
    procedure Run;
    procedure WriteUsage;
  end;

{ TCreateMacAppFolderApplication }

procedure TCreateMacAppFolderApplication.Run;
var
  Filename: String;
begin
  if ParamCount<1 then
    WriteUsage
  else begin
    Filename:=ExpandFileName(Params[1]);
    CreateMacOSXApplicationResources(Filename,ExtractFileNameOnly(Filename),'');
  end;
end;

procedure TCreateMacAppFolderApplication.WriteUsage;
begin
  writeln('Creates a MacOSX application folder <filename.app>');
  writeln('Author: Mattias Gaertner');
  writeln('');
  writeln('Usage:');
  writeln('');
  writeln('  '+ExeName+' <filename>');
end;

var
  App: TCreateMacAppFolderApplication;
begin
  App:=TCreateMacAppFolderApplication.Create(nil);
  App.Run;
  App.Free;
end.

