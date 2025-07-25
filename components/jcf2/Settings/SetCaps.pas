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

{$mode delphi}

interface

uses JcfSetBase, SettingsTypes, SettingsStream;

type

  TSetCaps = class(TSetBase)
  private

    fbEnabled: boolean;
    feReservedWords, feOperators, feDirectives, feConstants,
    feTypes: TCapitalisationType;
    fbIdentifiersNormalizeCapitalisation: boolean; // change the case to be equal of the word first use.
    fbNotIdentifiersNormalizeCapitalisation: boolean; // change the case to be equal of the word first use.
    fbNormalizeCapitalisationOneNamespace: boolean;
    feHexadecimalNumbers: TCapitalisationType;
    feFloatingPointNumbers: TCapitalisationType;
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

    property IdentifiersNormalizeCapitalisation: boolean Read fbIdentifiersNormalizeCapitalisation Write fbIdentifiersNormalizeCapitalisation; // for words not in the list, change the case to be equal of the word first use.
    property NotIdentifiersNormalizeCapitalisation: boolean Read fbNotIdentifiersNormalizeCapitalisation Write fbNotIdentifiersNormalizeCapitalisation; // for words not in the list, change the case to be equal of the word first use.
    property NormalizeCapitalisationOneNamespace: boolean Read fbNormalizeCapitalisationOneNamespace Write fbNormalizeCapitalisationOneNamespace; // share word list for identifiers and not identifiers.
    property HexadecimalNumbers: TCapitalisationType read feHexadecimalNumbers write feHexadecimalNumbers;
    property FloatingPointNumbers: TCapitalisationType read feFloatingPointNumbers write feFloatingPointNumbers;
  end;


implementation

const
  REG_ENABLED    = 'Enabled';
  REG_RESERVED_WORDS = 'ReservedWords';
  REG_OPERATORS  = 'Operators';
  REG_DIRECTIVES = 'Directives';
  REG_CONSTANTS  = 'Constants';
  REG_TYPES      = 'Types';
  REG_HEX_NUM    = 'HexadecimalNumbers';
  REG_FLOAT_NUM  = 'FloatingPointNumbers';

  REG_IDENTIFIERSNORMALIZECAPITALISATION    = 'IdentifiersNormalizeCapitalisation';
  REG_NOTIDENTIFIERSNORMALIZECAPITALISATION = 'NotIdentifiersNormalizeCapitalisation';
  REG_NORMALIZECAPITALISATIONONENAMESPACE = 'NormalizeCapitalisationOneNamespace';
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
  fbIdentifiersNormalizeCapitalisation := pcStream.Read(REG_IDENTIFIERSNORMALIZECAPITALISATION, False);
  fbNotIdentifiersNormalizeCapitalisation := pcStream.Read(REG_NOTIDENTIFIERSNORMALIZECAPITALISATION, False);
  fbNormalizeCapitalisationOneNamespace := pcStream.Read(REG_NORMALIZECAPITALISATIONONENAMESPACE, False);

  feReservedWords := TCapitalisationType(pcStream.Read(REG_RESERVED_WORDS,
    Ord(ctLower)));
  feOperators := TCapitalisationType(pcStream.Read(REG_OPERATORS, Ord(ctLower)));
  feDirectives := TCapitalisationType(pcStream.Read(REG_DIRECTIVES, Ord(ctLower)));
  feConstants := TCapitalisationType(pcStream.Read(REG_CONSTANTS, Ord(ctLower)));
  feTypes := TCapitalisationType(pcStream.Read(Reg_TYPES, Ord(ctLower)));
  feHexadecimalNumbers := TCapitalisationType(pcStream.Read(Reg_HEX_NUM, Ord(ctLeaveAlone)));
  feFloatingPointNumbers := TCapitalisationType(pcStream.Read(Reg_FLOAT_NUM, Ord(ctLeaveAlone)));
end;

procedure TSetCaps.WriteToStream(const pcOut: TSettingsOutput);
begin
  Assert(pcOut <> nil);

  pcOut.Write(REG_ENABLED, fbEnabled);
  pcOut.Write(REG_IDENTIFIERSNORMALIZECAPITALISATION, fbIdentifiersNormalizeCapitalisation);
  pcOut.Write(REG_NOTIDENTIFIERSNORMALIZECAPITALISATION, fbNotIdentifiersNormalizeCapitalisation);
  pcOut.Write(REG_NORMALIZECAPITALISATIONONENAMESPACE, fbNormalizeCapitalisationOneNamespace);

  pcOut.Write(REG_RESERVED_WORDS, Ord(feReservedWords));
  pcOut.Write(REG_OPERATORS, Ord(feOperators));
  pcOut.Write(REG_DIRECTIVES, Ord(feDirectives));
  pcOut.Write(REG_CONSTANTS, Ord(feConstants));
  pcOut.Write(REG_TYPES, Ord(feTypes));
  pcOut.Write(REG_HEX_NUM, Ord(feHexadecimalNumbers));
  pcOut.Write(REG_FLOAT_NUM, Ord(feFloatingPointNumbers));
end;

end.
