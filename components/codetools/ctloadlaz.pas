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
    Function to load compiler options from Lazarus projects and packages.
}
unit ctloadlaz;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type

  { TCTLazarusModule }

  TCTLazarusModule = class
  private
    FBaseDir: string;
    FFilename: string;
    FFiles: TStrings;
    FIncludePath: string;
    FName: string;
    FOutputDir: string;
    FUnitPath: string;
    procedure SetBaseDir(AValue: string);
    procedure SetFilename(AValue: string);
    procedure SetFiles(AValue: TStrings);
    procedure SetIncludePath(AValue: string);
    procedure SetName(AValue: string);
    procedure SetOutputDir(AValue: string);
    procedure SetUnitPath(AValue: string);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    property Filename: string read FFilename write SetFilename;
    property Name: string read FName write SetName;
    property BaseDir: string read FBaseDir write SetBaseDir;
    property UnitPath: string read FUnitPath write SetUnitPath;
    property IncludePath: string read FIncludePath write SetIncludePath;
    property OutputDir: string read FOutputDir write SetOutputDir;
    property Files: TStrings read FFiles write SetFiles;
  end;

  TCTLazarusPackage = class(TCTLazarusModule)

  end;

  TCTLazarusProject = class(TCTLazarusModule)

  end;

  { TCTLazarusManager }

  TCTLazarusManager = class
  private
    FPrimaryConfigPath: string;
    procedure SetPrimaryConfigPath(AValue: string);
  public
    constructor Create;
    destructor Destroy; override;
    procedure ParseParams;
    property PrimaryConfigPath: string read FPrimaryConfigPath write SetPrimaryConfigPath;
    property ActiveModule: TCTLazarusModule;
  end;

var
  LazarusOptions: TCTLazarusManager = nil;

implementation

{ TCTLazarusModule }

procedure TCTLazarusModule.SetBaseDir(AValue: string);
begin
  if FBaseDir=AValue then Exit;
  FBaseDir:=AValue;
end;

procedure TCTLazarusModule.SetFilename(AValue: string);
begin
  if FFilename=AValue then Exit;
  FFilename:=AValue;
end;

procedure TCTLazarusModule.SetFiles(AValue: TStrings);
begin
  if FFiles=AValue then Exit;
  FFiles:=AValue;
end;

procedure TCTLazarusModule.SetIncludePath(AValue: string);
begin
  if FIncludePath=AValue then Exit;
  FIncludePath:=AValue;
end;

procedure TCTLazarusModule.SetName(AValue: string);
begin
  if FName=AValue then Exit;
  FName:=AValue;
end;

procedure TCTLazarusModule.SetOutputDir(AValue: string);
begin
  if FOutputDir=AValue then Exit;
  FOutputDir:=AValue;
end;

procedure TCTLazarusModule.SetUnitPath(AValue: string);
begin
  if FUnitPath=AValue then Exit;
  FUnitPath:=AValue;
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

end;

{ TCTLazarusManager }

procedure TCTLazarusManager.SetPrimaryConfigPath(AValue: string);
begin
  if FPrimaryConfigPath=AValue then Exit;
  FPrimaryConfigPath:=AValue;
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

initialization
  LazarusOptions:=TCTLazarusManager.Create;
finalization;
  FreeAndNil(LazarusOptions);
end.

