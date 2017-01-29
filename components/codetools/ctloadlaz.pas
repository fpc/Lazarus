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
 *   Inc., 51 Franklin Street - Fifth Floor, Boston, MA 02110-1335, USA.   *
 *                                                                         *
 ***************************************************************************

  Author: Mattias Gaertner

  Abstract:
    Function to load compiler options from Lazarus projects and packages.
}
unit ctloadlaz;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileProcs, LazFileUtils, Laz2_XMLCfg;

type
  TCTLParseString = record
    UnparsedValue: string;
    ParsedValue: string;
    ParseStamp: int64;
    Parsing: boolean;
  end;

  TCTLModuleValue = (
    clmBaseDir,
    clmFilename,
    clmIncPath,
    clmUnitPath,
    clmSrcPath,
    clmOutputDir
    );

  { TCTLazarusModule }

  TCTLazarusModule = class
  private
    FFilename: string;
    FFiles: TStrings;
    FName: string;
    function GetBaseDir: string;
    function GetIncludePath: string;
    function GetOutputDir: string;
    function GetSrcPath: string;
    function GetUnitPath: string;
  protected
    FParseValues: array[TCTLModuleValue] of TCTLParseString;
    procedure SetUnparsedValue(Prop: TCTLModuleValue; NewValue: string);
    procedure SetBaseDir(AValue: string); virtual;
    procedure SetFilename(AValue: string); virtual;
    procedure SetFiles(AValue: TStrings); virtual;
    procedure SetIncludePath(AValue: string); virtual;
    procedure SetName(AValue: string); virtual;
    procedure SetOutputDir(AValue: string); virtual;
    procedure SetSrcPath(AValue: string); virtual;
    procedure SetUnitPath(AValue: string); virtual;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear; virtual;
    procedure LoadFromFile(aFilename: string); virtual;
    procedure LoadFromConfig(Cfg: TXMLConfig); virtual;
    property Filename: string read FFilename write SetFilename;
    property Name: string read FName write SetName;
    property BaseDir: string read GetBaseDir write SetBaseDir;
    property UnitPath: string read GetUnitPath write SetUnitPath;
    property IncludePath: string read GetIncludePath write SetIncludePath;
    property SrcPath: string read GetSrcPath write SetSrcPath;
    property OutputDir: string read GetOutputDir write SetOutputDir;
    property Files: TStrings read FFiles write SetFiles;
  end;

  TCTLazarusPackage = class(TCTLazarusModule)
  public
  end;

  TCTLazarusProject = class(TCTLazarusModule)
  public
  end;

  { TCTLazarusManager }

  TCTLazarusManager = class
  private
    FActiveProject: TCTLazarusProject;
    FPrimaryConfigPath: string;
    procedure SetActiveProject(AValue: TCTLazarusProject);
    procedure SetPrimaryConfigPath(AValue: string);
  public
    constructor Create;
    destructor Destroy; override;
    procedure ParseParams;
    property PrimaryConfigPath: string read FPrimaryConfigPath write SetPrimaryConfigPath;
    property ActiveProject: TCTLazarusProject read FActiveProject write SetActiveProject;
    procedure OpenProject(var aProject: TCTLazarusProject; aFilename: string);
  end;

var
  CTLazarusBoss: TCTLazarusManager = nil;
  CTLazParseStamp: int64 = CTInvalidChangeStamp64;

procedure CreateCTLazarusBoss;
procedure IncreaseCTLazParseStamp;

implementation

procedure CreateCTLazarusBoss;
begin
  CTLazarusBoss:=TCTLazarusManager.Create;
end;

procedure IncreaseCTLazParseStamp;
begin
  CTIncreaseChangeStamp64(CTLazParseStamp);
end;

{ TCTLazarusModule }

function TCTLazarusModule.GetBaseDir: string;
begin
  Result:=FParseValues[clmBaseDir].UnparsedValue;
end;

function TCTLazarusModule.GetIncludePath: string;
begin
  Result:=FParseValues[clmIncPath].UnparsedValue;
end;

function TCTLazarusModule.GetOutputDir: string;
begin
  Result:=FParseValues[clmOutputDir].UnparsedValue;
end;

function TCTLazarusModule.GetSrcPath: string;
begin
  Result:=FParseValues[clmSrcPath].UnparsedValue;
end;

function TCTLazarusModule.GetUnitPath: string;
begin
  Result:=FParseValues[clmUnitPath].UnparsedValue;
end;

procedure TCTLazarusModule.SetUnparsedValue(Prop: TCTLModuleValue;
  NewValue: string);
begin
  if FParseValues[Prop].UnparsedValue=NewValue then exit;
  if FParseValues[Prop].Parsing then
    raise Exception.Create('TCTLazarusModule.SetParsedValue can not set while parsing');
  FParseValues[Prop].UnparsedValue:=NewValue;
  FParseValues[Prop].ParseStamp:=CTLazParseStamp;
  IncreaseCTLazParseStamp;
end;

procedure TCTLazarusModule.SetBaseDir(AValue: string);
var
  NewValue: String;
begin
  NewValue:=TrimFilename(AValue);
  if BaseDir=NewValue then Exit;
  SetUnparsedValue(clmBaseDir,NewValue);
end;

procedure TCTLazarusModule.SetFilename(AValue: string);
begin
  if FFilename=AValue then Exit;
  FFilename:=AValue;
  BaseDir:=ExtractFilePath(FFilename);
  IncreaseCTLazParseStamp;
end;

procedure TCTLazarusModule.SetFiles(AValue: TStrings);
begin
  if FFiles.Equals(AValue) then Exit;
  FFiles.Assign(AValue);
end;

procedure TCTLazarusModule.SetIncludePath(AValue: string);
var
  NewValue: String;
begin
  NewValue:=Trim(AValue);
  if IncludePath=NewValue then Exit;
  SetUnparsedValue(clmIncPath,NewValue);
end;

procedure TCTLazarusModule.SetName(AValue: string);
begin
  if FName=AValue then Exit;
  FName:=AValue;
  IncreaseCTLazParseStamp;
end;

procedure TCTLazarusModule.SetOutputDir(AValue: string);
var
  NewValue: String;
begin
  NewValue:=TrimFilename(AValue);
  if OutputDir=NewValue then Exit;
  SetUnparsedValue(clmOutputDir,NewValue);
end;

procedure TCTLazarusModule.SetSrcPath(AValue: string);
var
  NewValue: String;
begin
  NewValue:=Trim(AValue);
  if SrcPath=NewValue then Exit;
  SetUnparsedValue(clmSrcPath,NewValue);
end;

procedure TCTLazarusModule.SetUnitPath(AValue: string);
var
  NewValue: String;
begin
  NewValue:=Trim(AValue);
  if UnitPath=NewValue then Exit;
  SetUnparsedValue(clmUnitPath,NewValue);
end;

constructor TCTLazarusModule.Create;
begin
  FFiles:=TStringList.Create;
end;

destructor TCTLazarusModule.Destroy;
begin
  Clear;
  FreeAndNil(FFiles);
  inherited Destroy;
end;

procedure TCTLazarusModule.Clear;
begin
  FFiles.Clear;
  UnitPath:='';
  IncludePath:='';
  SrcPath:='';
  OutputDir:='';
end;

procedure TCTLazarusModule.LoadFromFile(aFilename: string);
var
  Cfg: TXMLConfig;
begin
  Clear;
  Filename:=TrimAndExpandFilename(aFilename);
  Cfg:=TXMLConfig.Create(Filename);
  try
    LoadFromConfig(Cfg);
  finally
    Cfg.Free;
  end;
end;

procedure TCTLazarusModule.LoadFromConfig(Cfg: TXMLConfig);
begin
  if Cfg=nil then ;
  // ToDo
  RaiseCatchableException('not implemented yet');
end;

{ TCTLazarusManager }

procedure TCTLazarusManager.SetPrimaryConfigPath(AValue: string);
begin
  if FPrimaryConfigPath=AValue then Exit;
  FPrimaryConfigPath:=AValue;
end;

procedure TCTLazarusManager.SetActiveProject(AValue: TCTLazarusProject);
begin
  if FActiveProject=AValue then Exit;
  FActiveProject:=AValue;
end;

constructor TCTLazarusManager.Create;
begin

end;

destructor TCTLazarusManager.Destroy;
begin
  inherited Destroy;
end;

procedure TCTLazarusManager.ParseParams;

  function ParamIs(Param, Option: string; out Value: string): boolean;
  begin
    Value:='';
    if LeftStr(Param,length(Option))<>Option then exit(false);
    Value:=copy(Param,length(Option)+1,length(Param));
    Result:=true;
  end;

var
  i: Integer;
  p: String;
  s: string;
begin
  for i:=1 to Paramcount do begin
    p:=ParamStr(i);;
    if ParamIs(p,'--pcp=',s) or ParamIs(p,'--primary-config-path=',s) then
      PrimaryConfigPath:=s;
  end;
end;

procedure TCTLazarusManager.OpenProject(var aProject: TCTLazarusProject;
  aFilename: string);
begin
  aFilename:=TrimAndExpandFilename(aFilename);
  if aProject=nil then
    aProject:=TCTLazarusProject.Create;
  aProject.LoadFromFile(aFilename);
end;

initialization
  CTLazarusBoss:=TCTLazarusManager.Create;
finalization;
  FreeAndNil(CTLazarusBoss);
end.

