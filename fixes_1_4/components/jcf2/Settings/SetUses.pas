{(*}
(*------------------------------------------------------------------------------
 Delphi Code formatter source code 

The Original Code is SetUses.pas, released October 2000.
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

unit SetUses;

{ AFS 14 October 2K
 Functonality needed at work, so might as well do it generically
 uses clause find replace can
 - remove a unit name wherever found in a uses clause
 - insert unit names into interface and implementation uses clauses
 - relace unit(s) with a substitute unit wherever they occur
 }

{$I JcfGlobal.inc}

interface

uses
  { delphi }Classes,
  { local }JcfSetBase, SettingsStream;

type

  TSetUses = class(TSetBase)
  private
    fbRemoveEnabled, fbInsertInterfaceEnabled, fbInsertImplementationEnabled,
    fbFindReplaceEnabled: boolean;

    fsRemove, fsInsertInterface, fsInsertImplementation, fsFind, fsReplace: TStringList;

  protected

  public
    constructor Create;
    destructor Destroy; override;

    procedure WriteToStream(const pcOut: TSettingsOutput); override;
    procedure ReadFromStream(const pcStream: TSettingsInput); override;

    function GetReplace: string;

    property RemoveEnabled: boolean Read fbRemoveEnabled Write fbRemoveEnabled;
    property InsertInterfaceEnabled: boolean
      Read fbInsertInterfaceEnabled Write fbInsertInterfaceEnabled;
    property InsertImplementationEnabled: boolean
      Read fbInsertImplementationEnabled Write fbInsertImplementationEnabled;
    property FindReplaceEnabled: boolean Read fbFindReplaceEnabled
      Write fbFindReplaceEnabled;

    property Remove: TStringList Read fsRemove;
    property InsertInterface: TStringList Read fsInsertInterface;
    property InsertImplementation: TStringList Read fsInsertImplementation;
    property Find: TStringList Read fsFind;
    property Replace: TStringList Read fsReplace;
  end;

implementation

uses SysUtils;

const
  REG_REMOVE_ENABLED = 'RemoveEnabled';
  REG_INSERT_INTERFACE_ENABLED = 'InsertInterfaceEnabled';
  REG_INSERT_IMPLEMENTATION_ENABLED = 'InsertImplementationEnabled';
  REG_FIND_REPLACE_ENABLED = 'FindReplaceEnabled';

  REG_REMOVE  = 'Remove';
  REG_INSERT_INTERFACE = 'InsertInterface';
  REG_INSERT_IMPLEMENTATION = 'InsertImplementation';
  REG_FIND    = 'Find';
  REG_REPLACE = 'Replace';

constructor TSetUses.Create;
begin
  inherited;
  SetSection('Uses');

  fsRemove  := TStringList.Create;
  fsInsertInterface := TStringList.Create;
  fsInsertImplementation := TStringList.Create;
  fsFind    := TStringList.Create;
  fsReplace := TStringList.Create;
end;

destructor TSetUses.Destroy;
begin
  FreeAndNil(fsRemove);
  FreeAndNil(fsInsertInterface);
  FreeAndNil(fsInsertImplementation);
  FreeAndNil(fsFind);
  FreeAndNil(fsReplace);

  inherited;
end;

function TSetUses.GetReplace: string;
var
  liLoop: integer;
begin
  Result := '';

  for liLoop := 0 to fsReplace.Count - 1 do
  begin
    Result := Result + fsReplace[liLoop];
    if liLoop <> (fsReplace.Count - 1) then
      Result := Result + ', ';
  end;
end;

procedure TSetUses.ReadFromStream(const pcStream: TSettingsInput);
begin
  Assert(pcStream <> nil);

  fbRemoveEnabled      := pcStream.Read(REG_REMOVE_ENABLED, False);
  fbInsertInterfaceEnabled := pcStream.Read(REG_INSERT_INTERFACE_ENABLED, False);
  fbInsertImplementationEnabled :=
    pcStream.Read(REG_INSERT_IMPLEMENTATION_ENABLED, False);
  fbFindReplaceEnabled := pcStream.Read(REG_FIND_REPLACE_ENABLED, False);

  pcStream.Read(REG_REMOVE, fsRemove);
  pcStream.Read(REG_INSERT_INTERFACE, fsInsertInterface);
  pcStream.Read(REG_INSERT_IMPLEMENTATION, fsInsertImplementation);
  pcStream.Read(REG_FIND, fsFind);
  pcStream.Read(REG_REPLACE, fsReplace);
end;

procedure TSetUses.WriteToStream(const pcOut: TSettingsOutput);
begin
  Assert(pcOut <> nil);

  pcOut.Write(REG_REMOVE_ENABLED, fbRemoveEnabled);
  pcOut.Write(REG_INSERT_INTERFACE_ENABLED, fbInsertInterfaceEnabled);
  pcOut.Write(REG_INSERT_IMPLEMENTATION_ENABLED, fbInsertImplementationEnabled);
  pcOut.Write(REG_FIND_REPLACE_ENABLED, fbFindReplaceEnabled);

  pcOut.Write(REG_REMOVE, fsRemove);
  pcOut.Write(REG_INSERT_INTERFACE, fsInsertInterface);
  pcOut.Write(REG_INSERT_IMPLEMENTATION, fsInsertImplementation);
  pcOut.Write(REG_FIND, fsFind);
  pcOut.Write(REG_REPLACE, fsReplace);
end;

end.
