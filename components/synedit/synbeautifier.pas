{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: SynHighlighterGeneral.pas, released 2000-04-07.
The Original Code is based on the mwGeneralSyn.pas file from the
mwEdit component suite by Martin Waldenburg and other developers, the Initial
Author of this file is Martin Waldenburg.
Portions written by Martin Waldenburg are copyright 1999 Martin Waldenburg.
All Rights Reserved.

Contributors to the SynEdit and mwEdit projects are listed in the
Contributors.txt file.

Alternatively, the contents of this file may be used under the terms of the
GNU General Public License Version 2 or later (the "GPL"), in which case
the provisions of the GPL are applicable instead of those above.
If you wish to allow use of your version of this file only under the terms
of the GPL and not to allow others to use your version of this file
under the MPL, indicate your decision by deleting the provisions above and
replace them with the notice and other provisions required by the GPL.
If you do not delete the provisions above, a recipient may use your version
of this file under either the MPL or the GPL.

$Id: SynHighlighterGeneral.pas,v 1.3 2000/11/08 22:09:59 mghie Exp $

You may retrieve the latest version of this file at the SynEdit home page,
located at http://SynEdit.SourceForge.net
}
unit SynBeautifier;

{$I synedit.inc}

interface

uses
  Classes, SysUtils, SynEditMiscClasses, SynEditTextBase;

type
  { TSynCustomBeautifier }

  TSynCustomBeautifier = class(TComponent)
  public
    function CanUnindent(const Editor: TSynEditBase;  const Line: string;
                         const PhysCaretX: Integer): Boolean; virtual; abstract;
    function UnIndentLine(const Editor: TSynEditBase;  const Line: string;
                          const Lines: TSynEditStrings; const PhysCaret: TPoint;
                          out DelChars, InsChars: String;
                          out CaretNewX: Integer): String; virtual; abstract;   // Todo InsChar are not supprted for undo
    function IndentLine(const Editor: TSynEditBase; Line: string;
                        const Lines: TSynEditStrings; const PhysCaret: TPoint;
                        out DelChars, InsChars: String; out CaretNewX: Integer;
                        RemoveCurrentIndent: Boolean = False): String;
                        virtual; abstract;                                      // Todo DelChar are not supprted for undo
    function GetIndentForLine(Editor: TSynEditBase; const Line: string;
                              const Lines: TSynEditStrings;
                              const PhysCaret: TPoint): Integer; virtual; abstract;
  end;

  TSynBeautifierIndentType = (sbitSpace, sbitCopySpaceTab, sbitPositionCaret);

  { TSynBeautifier }

  TSynBeautifier = class(TSynCustomBeautifier)
  private
    FIndentType: TSynBeautifierIndentType;
  public
    function LeftSpaces(Editor: TSynEditBase; const Line: string;
                        Physical: boolean): Integer;
    function CanUnindent(const Editor: TSynEditBase;  const Line: string;
                         const PhysCaretX: Integer): Boolean; override;
    function UnIndentLine(const Editor: TSynEditBase;  const Line: string;
                          const Lines: TSynEditStrings; const PhysCaret: TPoint;
                          out DelChars, InsChars: String;
                          out CaretNewX: Integer): String; override;            // Todo InsChar are not supprted for undo
    function IndentLine(const Editor: TSynEditBase; Line: string;
                        const Lines: TSynEditStrings; const PhysCaret: TPoint;
                        out DelChars, InsChars: String; out CaretNewX: Integer;
                        RemoveCurrentIndent: Boolean = False): String; override; // Todo DelChar are not supprted for undo
    function GetIndentForLine(Editor: TSynEditBase; const Line: string;
                              const Lines: TSynEditStrings;
                              const PhysCaret: TPoint): Integer; override;
  published
    property IndentType: TSynBeautifierIndentType read FIndentType write FIndentType;
  end;

implementation
uses SynEdit;

{ TSynBeautifier }

function TSynBeautifier.LeftSpaces(Editor: TSynEditBase; const Line: string; Physical: boolean): Integer;
var
  p: PChar;
begin
  p := pointer(Line);
  if Assigned(p) then begin
    Result := 0;
    while p^ in [#1..#32] do begin
      Inc(p);
      Inc(Result);
    end;
    if Physical and (Result>0) then
      Result := TSynEdit(Editor).LogicalToPhysicalCol(Line, -1, Result+1)-1; // TODO, Need the real index of the line
  end else
    Result := 0;
end;

function TSynBeautifier.CanUnindent(const Editor: TSynEditBase;
  const Line: string; const PhysCaretX: Integer): Boolean;
begin
  Result := (LeftSpaces(Editor, Line, True) = PhysCaretX - 1);
end;

function TSynBeautifier.UnIndentLine(const Editor: TSynEditBase;
  const Line: string; const Lines: TSynEditStrings; const PhysCaret: TPoint;
  out DelChars, InsChars: String; out CaretNewX: Integer): String;
var
  SpaceCount1, SpaceCount2: Integer;
  BackCounter, LogSpacePos: Integer;
  LogCaret: TPoint;
begin
  SpaceCount1 := LeftSpaces(Editor, Line, true);
  SpaceCount2 := 0;
  if (SpaceCount1 > 0) then begin
    BackCounter := PhysCaret.Y - 2;
    while BackCounter >= 0 do begin
      SpaceCount2 := LeftSpaces(Editor, Lines[BackCounter], true);
      if SpaceCount2 < SpaceCount1 then
        break;
      Dec(BackCounter);
    end;
  end;
  if SpaceCount2 = SpaceCount1 then
    SpaceCount2 := 0;
  // remove visible spaces
  LogSpacePos := TSynEdit(Editor).PhysicalToLogicalCol(Line, PhysCaret.y-1, SpaceCount2 + 1);
  LogCaret := TSynEdit(Editor).PhysicalToLogicalPos(PhysCaret);
  CaretNewX :=  SpaceCount2 + 1;
  Lines.EditDelete(LogSpacePos, PhysCaret.Y, LogCaret.X - LogSpacePos);
  DelChars := copy(Line, LogSpacePos, LogCaret.X - LogSpacePos);
  InsChars := ''; // TODO: if tabs were removed, maybe fill-up with spaces
  Result :=copy(Line, 1, LogSpacePos-1) + copy(Line, LogCaret.X, MaxInt);
end;

function TSynBeautifier.IndentLine(const Editor: TSynEditBase; Line: string;
  const Lines: TSynEditStrings; const PhysCaret: TPoint; out DelChars,
  InsChars: String; out CaretNewX: Integer; RemoveCurrentIndent: Boolean): String;
var
  SpaceCount1, SpaceCount2: Integer;
  BackCounter: Integer;
  Temp: string;
begin
  DelChars := '';
  If RemoveCurrentIndent then begin
    SpaceCount1 := LeftSpaces(Editor, Line, False);
    DelChars := copy(Line, 1, SpaceCount1);
    Line := copy(Line, SpaceCount1 + 1, MaxInt);
  end;

  SpaceCount2 := 0;
  BackCounter := PhysCaret.Y - 1;
  if BackCounter > 0 then
    repeat
      Dec(BackCounter);
      Temp := Lines[BackCounter];
      SpaceCount2 := LeftSpaces(Editor, Temp, True);
    until (BackCounter = 0) or (Temp <> '');

  case FIndentType of
    sbitSpace:
      InsChars := StringOfChar(' ', SpaceCount2);
    sbitCopySpaceTab:
      InsChars := copy(Temp, 1, TSynEdit(Editor).PhysicalToLogicalCol(Temp,
                                                 BackCounter, SpaceCount2+1)-1);
    sbitPositionCaret:
      if Line <> '' then
        InsChars := StringOfChar(' ', SpaceCount2)
      else
        InsChars := '';
  end;
  Result := InsChars;
  CaretNewX := TSynEdit(Editor).LogicalToPhysicalCol(Result + Line, PhysCaret.y - 1, SpaceCount2+1);
end;

function TSynBeautifier.GetIndentForLine(Editor: TSynEditBase;
  const Line: string; const Lines: TSynEditStrings; const PhysCaret: TPoint): Integer;
var
  s1, s2: string;
begin
  IndentLine(Editor, Line, Lines, PhysCaret, s1, s2, Result, False);
end;

end.

