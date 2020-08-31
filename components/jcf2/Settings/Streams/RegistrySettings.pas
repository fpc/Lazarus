unit RegistrySettings;

{(*}
(*------------------------------------------------------------------------------
 Delphi Code formatter source code 

The Original Code is RegistrySettings.pas, released October 2001.
The Initial Developer of the Original Code is Anthony Steele. 
Portions created by Anthony Steele are Copyright (C) 1999-2008 Anthony Steele.
All Rights Reserved.
Contributor(s): Anthony Steele. 

The contents of this file are subject to the Mozilla Public License Version 1.1
(the "License"). you may not use this file except in compliance with the License.
You may obtain a copy of the License at http://www.mozilla.org/NPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied.
See the License for the specific language governing rights and limitations 
under the License.

Alternatively, the contents of this file may be used under the terms of
the GNU General Public License Version 2 or later (the "GPL") 
See http://www.gnu.org/licenses/gpl.html
------------------------------------------------------------------------------*)
{*)}

{$I JcfGlobal.inc}

interface

{ this unit is conrete subclasses of the input & output streams
 that wrap the registry
 it is not portable as it uses the regirst via a TRegistry object }

  { object to read settings from the registry
    Due to design above (ie the ExtractSection function),
    this object serves as both the root and the section
  }
uses
  Registry, Classes, SysUtils,
  { local }
  SettingsStream;

type

  TSettingsRegistryOutput = class(TSettingsOutput)
  private
    fReg: TRegIniFile;
    fsSection: string;

  public

    constructor Create(const psRootKey: string);

    destructor Destroy; override;

    procedure OpenSection(const psName: string); override;
    procedure CloseSection(const psName: string); override;

    procedure Write(const psTagName, psValue: string); override;
    procedure Write(const psTagName: string; const piValue: integer); override;
    procedure Write(const psTagName: string; const pbValue: boolean); override;
    procedure Write(const psTagName: string; const pdValue: double); override;
    procedure Write(const psTagName: string; const pcValue: TStrings); override;
  end;

  { TSettingsInputRegistry }

  TSettingsInputRegistry = class(TSettingsInput)
  private
    fReg: TRegIniFile;
    fsSection: string;
    fbOwnReg: boolean;

  public
    constructor Create(const psRootKey: string); overload;
    constructor Create(const pcReg: TRegIniFile; const psSection: string); overload;

    destructor Destroy; override;

    function ExtractSection(const psSection: string): TSettingsInput; override;

    function HasTag(const psTag: string): boolean; override;

    function Read(const psTag: string): string; override;
    function Read(const psTag, psDefault: string): string; override;
    function Read(const psTag: string; const piDefault: integer): integer; override;
    function Read(const psTag: string; const pfDefault: double): double; override;
    function Read(const psTag: string; const pbDefault: boolean): boolean; override;
    function Read(const psTag: string; const pcStrings: TStrings): boolean; override;
  end;

implementation

uses
  { local }
  JcfMiscFunctions,
  JcfStringUtils;

{-------------------------------------------------------------------------------
  Settings Output Registry }

constructor TSettingsRegistryOutput.Create(const psRootKey: string);
begin
  Assert(psRootKey <> '');
  inherited Create;

  fReg := TRegIniFile.Create;
  fReg.OpenKey(psRootKey, True);
  fsSection := '';
end;

destructor TSettingsRegistryOutput.Destroy;
begin
  FreeAndNil(fReg);
  inherited;
end;

procedure TSettingsRegistryOutput.OpenSection(const psName: string);
begin
  fsSection := psName;
end;

procedure TSettingsRegistryOutput.CloseSection(const psName: string);
begin
  fsSection := '';
end;

procedure TSettingsRegistryOutput.Write(const psTagName, psValue: string);
begin
  Assert(fReg <> nil);
  fReg.WriteString(fsSection, psTagName, psValue);
end;

procedure TSettingsRegistryOutput.Write(const psTagName: string; const piValue: integer);
begin
  Assert(fReg <> nil);
  fReg.WriteInteger(fsSection, psTagName, piValue);
end;

procedure TSettingsRegistryOutput.Write(const psTagName: string; const pbValue: boolean);
begin
  Assert(fReg <> nil);
  fReg.WriteBool(fsSection, psTagName, pbValue);
end;

procedure TSettingsRegistryOutput.Write(const psTagName: string; const pdValue: double);
begin
  Assert(fReg <> nil);
  // WTF? WriteFloat is broken in D6?
  fReg.WriteString(fsSection, psTagName, Float2Str(pdValue));
end;

// method from JcfSetBase
procedure TSettingsRegistryOutput.Write(const psTagName: string;
  const pcValue: TStrings);
var
  liLoop: integer;
  lsItem: string;
begin
  Assert(pcValue <> nil);

  Write(psTagName, 'String list');
  Write(psTagName + '_Count', pcValue.Count);

  for liLoop := 1 to pcValue.Count do
  begin
    lsItem := psTagName + '_' + string(PadNumber(liLoop));
    Write(lsItem, pcValue.Strings[liLoop - 1]);
  end;
end;

{-------------------------------------------------------------------------------
  SettingsInputRegistry }

constructor TSettingsInputRegistry.Create(const psRootKey: string);
begin
  Assert(psRootKey <> '');
  inherited Create;

  fReg := TRegIniFile.Create;
  fReg.OpenKey(psRootKey, True);

  fsSection := '';
  fbOwnReg  := True;
end;

constructor TSettingsInputRegistry.Create(const pcReg: TRegIniFile;
  const psSection: string);
begin
  Assert(psSection <> '');
  Assert(pcReg <> nil);

  inherited Create;

  fReg      := pcReg;
  fsSection := psSection;
  fbOwnReg  := False;
end;



destructor TSettingsInputRegistry.Destroy;
begin
  if fbOwnReg then
    FreeAndNil(fReg)
  else
    fReg := nil;

  inherited Destroy;
end;

function TSettingsInputRegistry.ExtractSection(const psSection: string): TSettingsInput;
begin
  Assert(fReg <> nil);
  Result := TSettingsInputRegistry.Create(fReg, psSection);
end;

function TSettingsInputRegistry.HasTag(const psTag: string): boolean;
const
  NON_EXISISTENCE_MARKER = '~';
var
  lsValue: string;
begin
  Assert(fReg <> nil);
  lsValue := fReg.ReadString(fsSection, psTag, NON_EXISISTENCE_MARKER);
  Result  := (lsValue <> NON_EXISISTENCE_MARKER);
end;

function TSettingsInputRegistry.Read(const psTag: string): string;
begin
  Assert(fReg <> nil);
  Result := fReg.ReadString(fsSection, psTag, '')
end;

function TSettingsInputRegistry.Read(const psTag, psDefault: string): string;
begin
  Assert(fReg <> nil);
  Result := fReg.ReadString(fsSection, psTag, psDefault)
end;

function TSettingsInputRegistry.Read(const psTag: string;
  const piDefault: integer): integer;
begin
  Assert(fReg <> nil);
  Result := fReg.ReadInteger(fsSection, psTag, piDefault)
end;

function TSettingsInputRegistry.Read(const psTag: string;
  const pfDefault: double): double;
begin
  Assert(fReg <> nil);
  Result := Str2Float(fReg.ReadString(fsSection, psTag, Float2Str(pfDefault)));
end;

function TSettingsInputRegistry.Read(const psTag: string;
  const pbDefault: boolean): boolean;
begin
  Assert(fReg <> nil);
  Result := fReg.ReadBool(fsSection, psTag, pbDefault)
end;


// method from JcfSetBase
function TSettingsInputRegistry.Read(const psTag: string;
  const pcStrings: TStrings): boolean;
var
  lsItemName, lsItem: string;
  liCount, liLoop:    integer;
begin
  Assert(pcStrings <> nil);

  lsItem := Read(psTag, '');
  if lsItem = '' then
  begin
    Result := False;
  end
  else
  begin
    pcStrings.Clear;

    liCount := Read(psTag + '_Count', 0);

    for liLoop := 1 to liCount do
    begin
      lsItemName := psTag + '_' + string(PadNumber(liLoop));
      lsItem     := Read(lsItemName, '');
      if lsItem <> '' then
        pcStrings.Add(lsItem);
    end;

    Result := True;
  end;
end;


end.
