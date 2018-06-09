{ /***************************************************************************
                     helpoptions.pas  -  Lazarus IDE unit
                     ------------------------------------

 ***************************************************************************/

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
 *   Inc., 51 Franklin Street - Fifth Floor, Boston, MA 02110-1335, USA.   *
 *                                                                         *
 ***************************************************************************

  Author: Mattias Gaertner

  Abstract:
    - THelpOptions
}
unit HelpOptions;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  // LCL
  LCLProc,
  // LazUtils
  LazFileUtils, Laz2_XMLCfg, LazFileCache,
  // IdeIntf
  LazHelpIntf, IDEOptionsIntf, IDEOptEditorIntf, MacroIntf,
  // IDE
  LazarusIDEStrConsts, IDEOptionDefs, LazConf;

type
  { THelpOptions }

  THelpOptions = class(TAbstractIDEHelpOptions)
  private
    FFilename: string;
    FFPCDocsHTMLDirectory: string;
    procedure SetFPCDocsHTMLDirectory(const AValue: string);
    procedure SetFilename(const AValue: string);
  public
    class function GetGroupCaption:string; override;
    class function GetInstance: TAbstractIDEOptions; override;
    procedure DoAfterWrite(Restore: boolean); override;
  public
    constructor Create;
    procedure Clear;
    procedure Load;
    procedure Save;
    procedure SetDefaultFilename;
    procedure Assign(Source: TPersistent); override;
    function IsEqual(HelpOpts: THelpOptions): boolean;
    function CreateCopy: THelpOptions;
  public
    property Filename: string read FFilename write SetFilename;
    function GetEffectiveFPCDocsHTMLDirectory: string;
  published
    property FPCDocsHTMLDirectory: string read FFPCDocsHTMLDirectory
                                          write SetFPCDocsHTMLDirectory;
  end;
  
var
  HelpOpts: THelpOptions; // set by the IDE

const
  HelpOptionsVersion = 1;
  DefaultHelpOptsFile = 'helpoptions.xml';

implementation

{ THelpOptions }

procedure THelpOptions.SetFilename(const AValue: string);
begin
  if FFilename = AValue then Exit;
  FFilename := AValue;
end;

procedure THelpOptions.SetFPCDocsHTMLDirectory(const AValue: string);
begin
  if FFPCDocsHTMLDirectory = AValue then Exit;
  FFPCDocsHTMLDirectory := AValue;
end;

constructor THelpOptions.Create;
begin
  Clear;
end;

class function THelpOptions.GetGroupCaption: string;
begin
  Result := lisHelp;
end;

class function THelpOptions.GetInstance: TAbstractIDEOptions;
begin
  Result := HelpOpts;
end;

procedure THelpOptions.DoAfterWrite(Restore: boolean);
begin
  if not Restore then
    Save;
end;

procedure THelpOptions.Clear;
begin
  FFPCDocsHTMLDirectory := '';
end;

procedure THelpOptions.Load;
var
  XMLConfig: TXMLConfig;
  FileVersion: integer;
  Storage: TXMLOptionsStorage;
begin
  try
    XMLConfig := TXMLConfig.Create(FFileName);
    try
      FileVersion := XMLConfig.GetValue('HelpOptions/Version/Value',0);
      if (FileVersion <> 0) and (FileVersion < HelpOptionsVersion) then
        DebugLn('Note: Loading old Help options file', FFileName);
      FPCDocsHTMLDirectory:=
                    XMLConfig.GetValue('HelpOptions/FPCDocs/HTML/Directory','');
        
      if HelpViewers <> nil then
      begin
        Storage := TXMLOptionsStorage.Create(XMLConfig, 'Viewers');
        try
          HelpViewers.Load(Storage);
        finally
          FreeAndNil(Storage);
        end;
      end;

      if HelpDatabases <> nil then
      begin
        Storage := TXMLOptionsStorage.Create(XMLConfig,'Databases');
        try
          HelpDatabases.Load(Storage);
        finally
          FreeAndNil(Storage);
        end;
      end;

    finally
      XMLConfig.Free;
    end;
  except
    on E: Exception do
      DebugLn('[THelpOptions.Load]  error reading "',FFilename,'": ',E.Message);
  end;
end;

procedure THelpOptions.Save;
var
  XMLConfig: TXMLConfig;
  Storage: TXMLOptionsStorage;
begin
  try
    InvalidateFileStateCache;
    XMLConfig:=TXMLConfig.CreateClean(FFileName);
    try
      XMLConfig.SetValue('HelpOptions/Version/Value',HelpOptionsVersion);
      XMLConfig.SetDeleteValue('HelpOptions/FPCDocs/HTML/Directory',
                               FPCDocsHTMLDirectory,'');

      if HelpViewers <> nil then
      begin
        Storage := TXMLOptionsStorage.Create(XMLConfig,'Viewers');
        try
          HelpViewers.Save(Storage);
        finally
          FreeAndNil(Storage);
        end;
      end;

      if HelpDatabases <> nil then
      begin
        Storage := TXMLOptionsStorage.Create(XMLConfig,'Databases');
        try
          HelpDatabases.Save(Storage);
        finally
          FreeAndNil(Storage);
        end;
      end;

      XMLConfig.Flush;
    finally
      XMLConfig.Free;
    end;
  except
    on E: Exception do
      DebugLn('[THelpOptions.Save]  error writing "',FFilename,'": ',E.Message);
  end;
end;

procedure THelpOptions.SetDefaultFilename;
var
  ConfFileName: string;
begin
  ConfFileName := AppendPathDelim(GetPrimaryConfigPath)+DefaultHelpOptsFile;
  CopySecondaryConfigFile(DefaultHelpOptsFile);
  if (not FileExistsUTF8(ConfFileName)) then
    DebugLn('NOTE: help options config file not found - using defaults');
  FFilename := ConfFilename;
end;

procedure THelpOptions.Assign(Source: TPersistent);
begin
  if Source is THelpOptions then
    FPCDocsHTMLDirectory := THelpOptions(Source).FPCDocsHTMLDirectory
  else
    inherited Assign(Source);
end;

function THelpOptions.IsEqual(HelpOpts: THelpOptions): boolean;
begin
  Result := FPCDocsHTMLDirectory = HelpOpts.FPCDocsHTMLDirectory;
end;

function THelpOptions.CreateCopy: THelpOptions;
begin
  Result := THelpOptions.Create;
  Result.Assign(Self);
end;

function THelpOptions.GetEffectiveFPCDocsHTMLDirectory: string;
begin
  Result:=FPCDocsHTMLDirectory;
  IDEMacros.SubstituteMacros(Result);
  Result:=AppendURLPathDelim(Result);
end;

initialization
  RegisterIDEOptionsGroup(GroupHelp, THelpOptions);
end.

