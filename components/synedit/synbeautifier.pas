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
  Classes, SysUtils, SynEditMiscClasses, SynEditTextBase, SynEditPointClasses;

type

  TSynBeautifierGetIndentEvent =
    procedure(Sender: TObject; LogCaret: TPoint; Line: Integer;
              var Indent, BasedLine: Integer; var ReplaceIndent: Boolean) of object;

  { TSynCustomBeautifier }

  TSynCustomBeautifier = class(TComponent)
  private
    FOnGetDesiredIndent: TSynBeautifierGetIndentEvent;
  public
    function CanUnindent(const Editor: TSynEditBase; const Lines: TSynEditStrings;
                         const ACaret: TSynEditCaret): Boolean; virtual; abstract;
    function UnIndentLine(const Editor: TSynEditBase;
                          const Lines: TSynEditStrings; const ACaret: TSynEditCaret;
                          out CaretNewX: Integer): Boolean; virtual; abstract;   // Todo InsChar are not supprted for undo
    function IndentLine(const Editor: TSynEditBase;
                        const Lines: TSynEditStrings; const ACaret: TSynEditCaret;
                        out CaretNewX: Integer): Boolean; virtual; abstract;                                      // Todo DelChar are not supprted for undo
    function GetDesiredIndentForLine
             (Editor: TSynEditBase; const Lines: TSynEditStrings;
              const ACaret: TSynEditCaret): Integer; virtual; abstract;
    function GetDesiredIndentForLine
             (Editor: TSynEditBase; const Lines: TSynEditStrings;
              const ACaret: TSynEditCaret; out ReplaceIndent: Boolean;
              out DesiredIndent: String): Integer; virtual; abstract;
    property OnGetDesiredIndent: TSynBeautifierGetIndentEvent
      read FOnGetDesiredIndent write FOnGetDesiredIndent;
  end;

  TSynBeautifierIndentType = (sbitSpace, sbitCopySpaceTab, sbitPositionCaret);

  { TSynBeautifier }

  TSynBeautifier = class(TSynCustomBeautifier)
  private
    FIndentType: TSynBeautifierIndentType;
  public
    function GetCurrentIndent(Editor: TSynEditBase; const Line: string;
                        Physical: boolean): Integer;
    function CanUnindent(const Editor: TSynEditBase; const Lines: TSynEditStrings;
                         const ACaret: TSynEditCaret): Boolean; override;
    function UnIndentLine
             (const Editor: TSynEditBase; const Lines: TSynEditStrings;
              const ACaret: TSynEditCaret; out CaretNewX: Integer): Boolean; override;
    function IndentLine
             (const Editor: TSynEditBase; const Lines: TSynEditStrings;
              const ACaret: TSynEditCaret; out CaretNewX: Integer): Boolean; override;
    function GetDesiredIndentForLine
             (Editor: TSynEditBase; const Lines: TSynEditStrings;
              const ACaret: TSynEditCaret): Integer; override;
    function GetDesiredIndentForLine
             (Editor: TSynEditBase; const Lines: TSynEditStrings;
              const ACaret: TSynEditCaret; out ReplaceIndent: Boolean;
              out DesiredIndent: String): Integer; override;
  published
    property IndentType: TSynBeautifierIndentType read FIndentType write FIndentType;
  end;

implementation
uses SynEdit;

{ TSynBeautifier }

function TSynBeautifier.GetCurrentIndent(Editor: TSynEditBase; const Line: string; Physical: boolean): Integer;
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
  const Lines: TSynEditStrings; const ACaret: TSynEditCaret): Boolean;
begin
  Result := (GetCurrentIndent(Editor, ACaret.LineText, True) = ACaret.CharPos - 1);
end;

function TSynBeautifier.UnIndentLine(const Editor: TSynEditBase;
  const Lines: TSynEditStrings; const ACaret: TSynEditCaret;
  out CaretNewX: Integer): Boolean;
var
  SpaceCount1, SpaceCount2: Integer;
  BackCounter, LogSpacePos: Integer;
  LogCaret: TPoint;
  Line: String;
begin
  Line := ACaret.LineText;
  SpaceCount1 := GetCurrentIndent(Editor, Line, true);
  SpaceCount2 := 0;
  if (SpaceCount1 > 0) then begin
    BackCounter := ACaret.LinePos - 2;
    while BackCounter >= 0 do begin
      SpaceCount2 := GetCurrentIndent(Editor, Lines[BackCounter], true);
      if SpaceCount2 < SpaceCount1 then
        break;
      Dec(BackCounter);
    end;
  end;
  if SpaceCount2 = SpaceCount1 then
    SpaceCount2 := 0;
  // remove visible spaces
  LogSpacePos := TSynEdit(Editor).PhysicalToLogicalCol(Line, ACaret.LinePos-1, SpaceCount2 + 1);
  LogCaret := ACaret.LineBytePos;
  CaretNewX :=  SpaceCount2 + 1;
  Lines.EditDelete(LogSpacePos, ACaret.LinePos, LogCaret.X - LogSpacePos);
  Result := True;
end;

function TSynBeautifier.IndentLine(const Editor: TSynEditBase;
  const Lines: TSynEditStrings; const ACaret: TSynEditCaret;
  out CaretNewX: Integer): Boolean;
var
  IndentPos: Integer;
  InsChars: String;
  ReplaceIndent: Boolean;
begin
  IndentPos := GetDesiredIndentForLine(Editor, Lines, ACaret, ReplaceIndent, InsChars);
  if IndentPos < 0 then
    exit(False);

  if ReplaceIndent then
    Lines.EditDelete(1, ACaret.LinePos,
                     GetCurrentIndent(Editor, ACaret.LineText, False));

  if (FIndentType = sbitPositionCaret) and (ACaret.LineText = '') then
    InsChars := '';

  Lines.EditInsert(1, ACaret.LinePos, InsChars);
  CaretNewX := IndentPos;
  Result := True;
end;

function TSynBeautifier.GetDesiredIndentForLine(Editor: TSynEditBase;
  const Lines: TSynEditStrings; const ACaret: TSynEditCaret;
  out ReplaceIndent: Boolean; out DesiredIndent: String): Integer;
var
  BackCounter, PhysLen: Integer;
  Temp: string;
  FoundLine: LongInt;
begin
  Result := 0;
  BackCounter := ACaret.LinePos - 1;
  if BackCounter > 0 then
    repeat
      Dec(BackCounter);
      Temp := Lines[BackCounter];
      Result := GetCurrentIndent(Editor, Temp, True) + 1;
    until (BackCounter = 0) or (Temp <> '');

  FoundLine := BackCounter + 1;
  ReplaceIndent := False;
  if assigned(FOnGetDesiredIndent) then
    FOnGetDesiredIndent(Editor, ACaret.LineBytePos, ACaret.LinePos, Result,
                        FoundLine, ReplaceIndent);

  if Result < 0 then exit;

  if FoundLine > 0 then
    Temp := Lines[FoundLine-1]
  else
    FoundLine := BackCounter + 1;
  Temp := copy(Temp, 1, GetCurrentIndent(Editor, Temp, False));

  case FIndentType of
    sbitCopySpaceTab:
      begin
        DesiredIndent := copy(Temp, 1,
          TSynEdit(Editor).PhysicalToLogicalCol(Temp, FoundLine - 1, Result) - 1);
        PhysLen := TSynEdit(Editor).LogicalToPhysicalCol(Temp, ACaret.LinePos - 1, Length(Temp) + 1);
        if PhysLen < Result then
          DesiredIndent := DesiredIndent + StringOfChar(' ', Result - PhysLen);
      end;
    else
      DesiredIndent := StringOfChar(' ', Result - 1);
  end;
end;

function TSynBeautifier.GetDesiredIndentForLine(Editor: TSynEditBase;
  const Lines: TSynEditStrings; const ACaret: TSynEditCaret): Integer;
var
  Dummy: String;
  Replace: Boolean;
begin
  Result := GetDesiredIndentForLine(Editor, Lines, ACaret, Replace, Dummy);
end;

end.

