{(*}
(*------------------------------------------------------------------------------
 Delphi Code formatter source code 

The Original Code is SetCaps.pas, released April 2000.
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

unit SetCaps;

{ settings to do with capitalisation
  AFS 29 Dec 1999
}

{$I JcfGlobal.inc}

interface

uses JcfSetBase, SettingsTypes, SettingsStream;

type

  TSetCaps = class(TSetBase)
  private

    fbEnabled: boolean;
    feReservedWords, feOperators, feDirectives, feConstants,
    feTypes: TCapitalisationType;

  protected
  public
    constructor Create;

    procedure WriteToStream(const pcOut: TSettingsOutput); override;
    procedure ReadFromStream(const pcStream: TSettingsInput); override;

    property Enabled: boolean Read fbEnabled Write fbEnabled;

    property ReservedWords: TCapitalisationType
      Read feReservedWords Write feReservedWords;

    property Operators: TCapitalisationType Read feOperators Write feOperators;
    property Directives: TCapitalisationType Read feDirectives Write feDirectives;
    property Constants: TCapitalisationType Read feConstants Write feConstants;
    property Types: TCapitalisationType Read feTypes Write feTypes;

  end;


implementation

const
  REG_ENABLED    = 'Enabled';
  REG_RESERVED_WORDS = 'ReservedWords';
  REG_OPERATORS  = 'Operators';
  REG_DIRECTIVES = 'Directives';
  REG_CONSTANTS  = 'Constants';
  REG_TYPES      = 'Types';

  { TSetCaps }

constructor TSetCaps.Create;
begin
  inherited;
  SetSection('Capitalisation');
end;

procedure TSetCaps.ReadFromStream(const pcStream: TSettingsInput);
begin
  Assert(pcStream <> nil);

  fbEnabled := pcStream.Read(REG_ENABLED, True);

  feReservedWords := TCapitalisationType(pcStream.Read(REG_RESERVED_WORDS,
    Ord(ctLower)));
  feOperators := TCapitalisationType(pcStream.Read(REG_OPERATORS, Ord(ctLower)));
  feDirectives := TCapitalisationType(pcStream.Read(REG_DIRECTIVES, Ord(ctLower)));
  feConstants := TCapitalisationType(pcStream.Read(REG_CONSTANTS, Ord(ctLower)));
  feTypes := TCapitalisationType(pcStream.Read(Reg_TYPES, Ord(ctLower)));
end;

procedure TSetCaps.WriteToStream(const pcOut: TSettingsOutput);
begin
  Assert(pcOut <> nil);

  pcOut.Write(REG_ENABLED, fbEnabled);

  pcOut.Write(REG_RESERVED_WORDS, Ord(feReservedWords));
  pcOut.Write(REG_OPERATORS, Ord(feOperators));
  pcOut.Write(REG_DIRECTIVES, Ord(feDirectives));
  pcOut.Write(REG_CONSTANTS, Ord(feConstants));
  pcOut.Write(REG_TYPES, Ord(feTypes));
end;

end.
