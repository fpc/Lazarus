unit Nesting;
{(*}
(*------------------------------------------------------------------------------
 Delphi Code formatter source code

The Original Code is Nesting, released May 2003.
The Initial Developer of the Original Code is Anthony Steele.
Portions created by Anthony Steele are Copyright (C) 1999-2008 Anthony Steele.
All Rights Reserved.
Contributor(s):
Anthony Steele.
Adem Baba

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

{ AFS 10 Jan 2002
  This is fairly generic code so it has it's own class
  to store on each token nesting level info for a variety of indicators
  such as
  - begin end block nesting level
  - record case nesting level
  - case statement, try statment etc.
  - procedure nesting level

  Easier and faster to set this up once
  with a visitor and store it on a leaf node
  than the generate it on the fly
}

type

  TNestingLevelType = (
    nlBlock, // generic code indent
    nlCaseSelector,
    nlRecordType,
    nlRecordVariantSection,
    nlProcedure,
    nlRoundBracket, nlSquareBracket,
    nlStatementLabel);

  TNestingLevelList = class(TObject)
  private
    { store a nesting level for one of the above enums
      Adem Baba suggested that an array indexed by enum
      would be simpler and faster than a TObjectList }
    fiValues: array[TNestingLevelType] of integer;

  public
    procedure Clear;

    procedure Assign(const pcSource: TNestingLevelList);

    { clients do not have unrestricted write access to these values
      should only increment and dec them,
      e.g. nlRoundBracket is incremented on each '(' and decemented on ')' }
    procedure IncLevel(const peItemType: TNestingLevelType);
    procedure DecLevel(const peItemType: TNestingLevelType);

    function GetLevel(const peItemType: TNestingLevelType): integer;

    { by the end of the unit, everything opened should have been closed }
    function FinalTest: string;
    function Total: integer;
  end;

implementation

uses SysUtils;

procedure TNestingLevelList.DecLevel(const peItemType: TNestingLevelType);
begin
  dec(fiValues[peItemType]);
end;


procedure TNestingLevelList.IncLevel(const peItemType: TNestingLevelType);
begin
  inc(fiValues[peItemType]);
end;

function TNestingLevelList.GetLevel(const peItemType: TNestingLevelType): integer;
begin
  Result := fiValues[peItemType];
end;


{ at the end of it all, all should be back to zero }
function TNestingLevelList.FinalTest: string;
var
  leLoop: TNestingLevelType;
begin
  Result := '';

  for leLoop := low(TNestingLevelType) to High(TNestingLevelType) do
  begin
    if fiValues[leLoop] > 0 then
    begin
      Result := 'Final nesting level = ' + IntToStr(fiValues[leLoop]);
      break;
    end;
  end;
end;

procedure TNestingLevelList.Assign(const pcSource: TNestingLevelList);
var
  leLoop: TNestingLevelType;
begin

  if pcSource = nil then
  begin
    Clear;
  end
  else
  begin
    for leLoop := low(TNestingLevelType) to High(TNestingLevelType) do
    begin
      fiValues[leLoop] := pcSource.GetLevel(leLoop);
    end;
  end;

end;

procedure TNestingLevelList.Clear;
var
  leLoop: TNestingLevelType;
begin
  for leLoop := low(TNestingLevelType) to High(TNestingLevelType) do
    fiValues[leLoop] := 0;
end;

function TNestingLevelList.Total: integer;
var
  leLoop: TNestingLevelType;
begin

  Result := 0;
  for leLoop := low(TNestingLevelType) to High(TNestingLevelType) do
  begin
    Result := Result + fiValues[leLoop];
  end;
end;

end.
