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
  Classes, SysUtils, LCLProc, SynEditMiscClasses, SynEditTextBase, SynEditPointClasses,
  SynEditKeyCmds;

type

  TSynCustomBeautifier = class;

  // Callback for indent
  TSynBeautifierSetIndentProc =
    procedure(
      (* LinePos:
           1-based, the line that should be changed *)
      LinePos: Integer;
      (* Indent:
           New indent in spaces (Logical = Physical *)
      Indent: Integer;
      (* RelativeToLinePos:
           Indent specifies +/- offset from indent on RTLine (0: for absolute indent) *)
      RelativeToLinePos: Integer = 0;
      (* IndentChars:
           String used to build indent; maybe empty, single char, or string (usually 1 tab or 1 space)
           The String will be repeated and cut as needed, then filled with spaces at the end
         * NOTE: If this is specified the TSynBeautifierIndentType is ignored
      *)
      IndentChars: String = '';
      (* IndentCharsFromLinePos:
           Use tab/space mix from this Line for indent (if specified > 0)
           "IndentChars" will only be used, if the found tab/space mix is to short
         * NOTE: If this is specified the TSynBeautifierIndentType is ignored
      *)
      IndentCharsFromLinePos: Integer = 0
    ) of object;

  // Event triggered if Lines may needs Indend
  TSynBeautifierGetIndentEvent =
    function(
      Sender: TObject;                       // the beautifier
      Editor: TObject;                       // the synedit
      LogCaret, OldLogCaret: TPoint;         // Caret after and before the edit action
      FirstLinePos, LastLinePos: Integer;    // Changed lines. this can include lines outside the range of OldLogCaret to LogCaret
      Reason: TSynEditorCommand;             // what caused the event
      SetIndentProc: TSynBeautifierSetIndentProc
     ): boolean of object;


  { TSynCustomBeautifier }

  TSynCustomBeautifier = class(TComponent)
  private
    FAutoIndent: Boolean;
    FOnGetDesiredIndent: TSynBeautifierGetIndentEvent;
    FCurrentEditor: TSynEditBase; // For callback / applyIndent
    FCurrentLines: TSynEditStrings;
  protected
    procedure DoBeforeCommand(const ACaret: TSynEditCaret;
                              var Command: TSynEditorCommand); virtual; abstract;
    procedure DoAfterCommand(const ACaret: TSynEditCaret;
                             var Command: TSynEditorCommand;
                             StartLinePos, EndLinePos: Integer); virtual; abstract;
  public
    procedure Assign(Src: TPersistent); override;
    procedure BeforeCommand(const Editor: TSynEditBase; const Lines: TSynEditStrings;
                            const ACaret: TSynEditCaret; var Command: TSynEditorCommand;
                            InitialCmd: TSynEditorCommand);
    procedure AfterCommand(const Editor: TSynEditBase; const Lines: TSynEditStrings;
                           const ACaret: TSynEditCaret; var Command: TSynEditorCommand;
                           InitialCmd: TSynEditorCommand; StartLinePos, EndLinePos: Integer);
    // GetDesiredIndentForLine: Returns the 1-based Physical x pos
    function GetDesiredIndentForLine
             (Editor: TSynEditBase; const Lines: TSynEditStrings;
              const ACaret: TSynEditCaret): Integer; virtual; abstract;
    function GetDesiredIndentForLine
             (Editor: TSynEditBase; const Lines: TSynEditStrings;
              const ACaret: TSynEditCaret; out ReplaceIndent: Boolean;
              out DesiredIndent: String): Integer; virtual; abstract;
    property OnGetDesiredIndent: TSynBeautifierGetIndentEvent
      read FOnGetDesiredIndent write FOnGetDesiredIndent;
    property AutoIndent: Boolean read FAutoIndent write FAutoIndent;
  end;

  TSynCustomBeautifierClass = class of TSynCustomBeautifier;
  TSynBeautifierIndentType = (sbitSpace, sbitCopySpaceTab, sbitPositionCaret);

  { TSynBeautifier }

  TSynBeautifier = class(TSynCustomBeautifier)
  private
    FIndentType: TSynBeautifierIndentType;
  protected
    // requiring FCurrentEditor, FCurrentLines
    procedure DoBeforeCommand(const ACaret: TSynEditCaret;
                              var Command: TSynEditorCommand); override;
    procedure DoAfterCommand(const ACaret: TSynEditCaret;
                             var Command: TSynEditorCommand;
                             StartLinePos, EndLinePos: Integer); override;
    function GetIndent(const LinePos: Integer; out BasedOnLine: Integer): Integer;
    function AdjustCharMix(DesiredIndent: Integer; CharMix, AppendMix: String): String;
    function GetCharMix(const LinePos, Indent: Integer;
                        var IndentCharsFromLinePos: Integer = 0): String;
    procedure ApplyIndent(LinePos: Integer; Indent: Integer;
                          RelativeToLinePos: Integer = 0; IndentChars: String = '';
                          IndentCharsFromLinePos: Integer = 0);
    function UnIndentLine(const ACaret: TSynEditCaret; out CaretNewX: Integer): Boolean;
  public
    procedure Assign(Src: TPersistent); override;
    // Retruns a 0-based position (even 0-based physical)
    function GetIndentForLine(Editor: TSynEditBase; const Line: string;
                        Physical: boolean): Integer;
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

{ TSynCustomBeautifier }

procedure TSynCustomBeautifier.Assign(Src: TPersistent);
begin
  if assigned(Src) and (src is TSynCustomBeautifier) then begin
    FCurrentEditor := TSynCustomBeautifier(Src).FCurrentEditor;
    FCurrentLines := TSynCustomBeautifier(Src).FCurrentLines;
    FOnGetDesiredIndent := TSynCustomBeautifier(Src).FOnGetDesiredIndent;
    FAutoIndent := TSynCustomBeautifier(Src).FAutoIndent;
  end
  else
    inherited;
end;

procedure TSynCustomBeautifier.BeforeCommand(const Editor: TSynEditBase;
  const Lines: TSynEditStrings; const ACaret: TSynEditCaret;
  var Command: TSynEditorCommand; InitialCmd: TSynEditorCommand);
var
  Worker: TSynCustomBeautifier;
begin
  // Since all synedits share one beautifier, create a temp instance.
  // Todo: have individual beautifiers
  Worker := TSynCustomBeautifierClass(self.ClassType).Create(nil);
  Worker.assign(self);
  Worker.FCurrentEditor := Editor;
  Worker.FCurrentLines := Lines;
  try
    Worker.DoBeforeCommand(ACaret, Command);
  finally
    Worker.Free;
  end;
end;

procedure TSynCustomBeautifier.AfterCommand(const Editor: TSynEditBase;
  const Lines: TSynEditStrings; const ACaret: TSynEditCaret;
  var Command: TSynEditorCommand; InitialCmd: TSynEditorCommand;
  StartLinePos, EndLinePos: Integer);
var
  Worker: TSynCustomBeautifier;
begin
  // Since all synedits share one beautifier, create a temp instance.
  // Todo: have individual beautifiers
  Worker := TSynCustomBeautifierClass(self.ClassType).Create(nil);
  Worker.assign(self);
  Worker.FCurrentEditor := Editor;
  Worker.FCurrentLines := Lines;
  try
    Worker.DoAfterCommand(ACaret, Command, StartLinePos, EndLinePos);
  finally
    Worker.Free;
  end;
end;

{ TSynBeautifier }

procedure TSynBeautifier.Assign(Src: TPersistent);
begin
  inherited Assign(Src);
  if assigned(Src) and (src is TSynBeautifier) then begin
    FIndentType := TSynBeautifier(Src).FIndentType;
    FCurrentEditor := TSynBeautifier(Src).FCurrentEditor;
    FCurrentLines := TSynBeautifier(Src).FCurrentLines;
  end;
end;

procedure TSynBeautifier.DoBeforeCommand(const ACaret: TSynEditCaret;
  var Command: TSynEditorCommand);
var
  x: Integer;
begin
  if (Command = ecDeleteLastChar) and
     (FAutoIndent) and
     (ACaret.CharPos > 1) and
     (not TSynEdit(FCurrentEditor).ReadOnly) and
     ( (not TSynEdit(FCurrentEditor).SelAvail) or
       (eoPersistentBlock in TSynEdit(FCurrentEditor).Options2) ) and
     (GetIndentForLine(FCurrentEditor, ACaret.LineText, True) = ACaret.CharPos - 1)
  then begin
    FCurrentLines.UndoList.CurrentReason := ecSmartUnindent;

    UnIndentLine(ACaret, x);
    ACaret.CharPos := x;
    Command := ecNone;
  end;
end;

procedure TSynBeautifier.DoAfterCommand(const ACaret: TSynEditCaret;
  var Command: TSynEditorCommand; StartLinePos, EndLinePos: Integer);
var
  y, b, Indent: Integer;
  s: String;
begin
  if EndLinePos < 1 then
    exit;
  if assigned(FOnGetDesiredIndent) and
    FOnGetDesiredIndent(self, FCurrentEditor, ACaret.LineBytePos,
                        ACaret.OldLineBytePos, StartLinePos, EndLinePos, Command,
                        @ApplyIndent)
  then
    exit;

  if ((Command = ecLineBreak) or (Command = ecInsertLine)) and FAutoIndent then begin
    if (Command = ecLineBreak) then
      y := ACaret.LinePos
    else
      y := ACaret.LinePos + 1;

    if (FCurrentLines[y-2] = '') and (FCurrentLines[y-1] <> '') then
      Indent := 0
    else
      Indent := GetIndent(y, b);

    if Indent > 0 then begin
      s := GetCharMix(y, Indent, b);
      if (FIndentType = sbitPositionCaret) and (FCurrentLines[y-1] = '') then
        s := '';
      FCurrentLines.EditInsert(1, y, s);
    end;

    if (Command = ecLineBreak) then begin
      ACaret.IncForcePastEOL;
      ACaret.CharPos := Indent + 1;
      ACaret.DecForcePastEOL;
    end;
  end;
end;

function TSynBeautifier.UnIndentLine(const ACaret: TSynEditCaret;
  out CaretNewX: Integer): Boolean;
var
  SpaceCount1, SpaceCount2: Integer;
  BackCounter, LogSpacePos, FillSpace: Integer;
  LogCaret: TPoint;
  Line, Temp: String;
begin
  Line := ACaret.LineText;
  SpaceCount1 := GetIndentForLine(FCurrentEditor, Line, true); // physical, desired pos
  SpaceCount2 := 0;
  if (SpaceCount1 > 0) then begin
    BackCounter := ACaret.LinePos - 2;
    while BackCounter >= 0 do begin
      Temp := FCurrentLines[BackCounter];
      SpaceCount2 := GetIndentForLine(FCurrentEditor, Temp, true);
      if (SpaceCount2 < SpaceCount1) and (temp <> '') then
        break;
      Dec(BackCounter);
    end;
  end;
  if SpaceCount2 = SpaceCount1 then
    SpaceCount2 := 0;
  // remove visible spaces
  LogSpacePos := FCurrentLines.PhysicalToLogicalCol(Line, ACaret.LinePos-1, SpaceCount2 + 1);
  FillSpace := SpaceCount2 + 1 - FCurrentLines.LogicalToPhysicalCol(Line, ACaret.LinePos-1, LogSpacePos);
  LogCaret := ACaret.LineBytePos;
  CaretNewX :=  SpaceCount2 + 1;
  FCurrentLines.EditDelete(LogSpacePos, ACaret.LinePos, LogCaret.X - LogSpacePos);
  if FillSpace > 0 then
    FCurrentLines.EditInsert(LogSpacePos, ACaret.LinePos, StringOfChar(' ', FillSpace));
  Result := True;
end;

function TSynBeautifier.GetIndent(const LinePos: Integer; out BasedOnLine: Integer): Integer;
var
  Temp: string;
begin
  BasedOnLine := LinePos - 1;
  while (BasedOnLine > 0) do begin
    dec(BasedOnLine);
    Temp := FCurrentLines[BasedOnLine];
    if Temp <> '' then begin
      Result := GetIndentForLine(FCurrentEditor, Temp, True);
      exit;
    end;
  end;
  BasedOnLine := LinePos;
  Result := GetIndentForLine(FCurrentEditor, FCurrentLines[BasedOnLine], True);
end;

function TSynBeautifier.AdjustCharMix(DesiredIndent: Integer; CharMix, AppendMix: String): String;
var
  i: Integer;
  CurLen: Integer;
begin
  CurLen := FCurrentLines.LogicalToPhysicalCol(CharMix, -1, length(CharMix)+1) - 1; // TODO: Need the real index of the line
  if AppendMix <> '' then begin
    while CurLen < DesiredIndent do begin
      CharMix := CharMix + AppendMix;
      CurLen := FCurrentLines.LogicalToPhysicalCol(CharMix, -1, length(CharMix)+1) - 1; // TODO: Need the real index of the line
    end
  end;

  i := length(CharMix);
  while CurLen > DesiredIndent do begin
    Dec(i);
    CurLen := FCurrentLines.LogicalToPhysicalCol(CharMix, -1, i+1) - 1; // TODO: Need the real index of the line
  end;

  CharMix := copy(CharMix, 1, i) + StringOfChar(' ', DesiredIndent - CurLen);
  Result := CharMix;
end;

function TSynBeautifier.GetCharMix(const LinePos, Indent: Integer;
  var IndentCharsFromLinePos: Integer = 0): String;
var
  Temp, KnownMix, BasedMix: string;
  KnownPhysLen, PhysLen: Integer;
  BackCounter: LongInt;
begin
  if FIndentType <> sbitCopySpaceTab then begin
    IndentCharsFromLinePos := 0;
    Result := StringOfChar(' ', Indent);
    exit;
  end;

  if (IndentCharsFromLinePos > 0) and (IndentCharsFromLinePos <= FCurrentLines.Count) then
  begin
    Temp := FCurrentLines[IndentCharsFromLinePos];
    KnownMix := copy(Temp, 1, GetIndentForLine(FCurrentEditor, Temp, False));
  end
  else
    KnownMix := '';
  BasedMix := KnownMix;
  KnownPhysLen := GetIndentForLine(FCurrentEditor, KnownMix, True);

  BackCounter := LinePos;
  while (BackCounter > 0) and (KnownPhysLen < Indent) do begin
    dec(BackCounter);
    Temp := FCurrentLines[BackCounter];
    if Temp <> '' then begin
      Temp := copy(Temp, 1, GetIndentForLine(FCurrentEditor, Temp, False));
      PhysLen := GetIndentForLine(FCurrentEditor, Temp, True);
      if (PhysLen > KnownPhysLen) and (copy(temp, 1, length(BasedMix)) = BasedMix) then
      begin
        KnownMix := Temp;
        KnownPhysLen := PhysLen;
        IndentCharsFromLinePos := BackCounter + 1;
      end;
    end;
  end;

  Result := AdjustCharMix(Indent, KnownMix, '');
end;

procedure TSynBeautifier.ApplyIndent(LinePos: Integer;
  Indent: Integer; RelativeToLinePos: Integer; IndentChars: String = '';
  IndentCharsFromLinePos: Integer = 0);
var
  CharMix: String;
  i: Integer;
begin
  if (LinePos < 1) or (LinePos > FCurrentEditor.Lines.Count) then
    exit;

  // calculate the final indent needed
  if (RelativeToLinePos > 0) and (RelativeToLinePos <= FCurrentEditor.Lines.Count) then
    Indent := Indent + GetIndentForLine(FCurrentEditor, FCurrentLines[RelativeToLinePos-1], True);
  if Indent< 0 then
    Indent := 0;

  // Calculate the charmix
  CharMix := '';
  if Indent > 0 then begin
    if (IndentCharsFromLinePos > 0) and (IndentCharsFromLinePos <= FCurrentEditor.Lines.Count) then begin
      CharMix := FCurrentLines[IndentCharsFromLinePos-1];
      i :=  GetIndentForLine(FCurrentEditor, CharMix, False);
      CharMix := AdjustCharMix(Indent, copy(CharMix, 1, i), IndentChars);
    end
    else if IndentChars <> '' then begin
      CharMix := AdjustCharMix(Indent, '', IndentChars);
    end
    else begin
      i := LinePos;
      CharMix := GetCharMix(LinePos, Indent, i);
    end;
  end;

  {$IFDEF VerboseIndenter}
  DebugLn(['TSynBeautifier.ApplyIndent IndentChars="',dbgstr(IndentChars),' Indent=',Indent]);
  {$ENDIF}

  i :=  GetIndentForLine(FCurrentEditor, FCurrentLines[LinePos-1], False);
  FCurrentLines.EditDelete(1, LinePos, i);
  if (CharMix <> '') and not((FIndentType = sbitPositionCaret) and (FCurrentLines[LinePos-1] = '')) then
    FCurrentLines.EditInsert(1, LinePos, CharMix);

  {$IFDEF VerboseIndenter}
  DebugLn(['TSynBeautifier.ApplyIndent Line="',dbgstr(FCurrentLines.ExpandedStrings[LinePos-1]),'"']);
  {$ENDIF}
end;

function TSynBeautifier.GetIndentForLine(Editor: TSynEditBase; const Line: string; Physical: boolean): Integer;
var
  p: PChar;
begin
  p := PChar(Line);
  if Assigned(p) then begin
    Result := 0;
    while p^ in [#1..#32] do begin
      Inc(p);
      Inc(Result);
    end;
    if Physical and (Result>0) then
      Result := FCurrentLines.LogicalToPhysicalCol(Line, -1, Result+1)-1; // TODO, Need the real index of the line
  end else
    Result := 0;
end;

function TSynBeautifier.GetDesiredIndentForLine(Editor: TSynEditBase;
  const Lines: TSynEditStrings; const ACaret: TSynEditCaret;
  out ReplaceIndent: Boolean; out DesiredIndent: String): Integer;
var
  BackCounter, PhysLen: Integer;
  Temp: string;
  FoundLine: LongInt;
begin
  Result := 1;
  FCurrentLines := Lines; // for GetCurrentIndent
  BackCounter := ACaret.LinePos - 1;
  if BackCounter > 0 then
    repeat
      Dec(BackCounter);
      Temp := Lines[BackCounter];
      Result := GetIndentForLine(Editor, Temp, True) + 1; // Physical
    until (BackCounter = 0) or (Temp <> '');

  FoundLine := BackCounter + 1;
  ReplaceIndent := False;
  //if assigned(FOnGetDesiredIndent) then
  //  FOnGetDesiredIndent(Editor, ACaret.LineBytePos, ACaret.LinePos, Result,
  //                      FoundLine, ReplaceIndent);

  //if Result < 0 then exit;

  //if FoundLine > 0 then
  //  Temp := Lines[FoundLine-1]
  //else
  //  FoundLine := BackCounter + 1;
  Temp := copy(Temp, 1, GetIndentForLine(Editor, Temp, False));

  case FIndentType of
    sbitCopySpaceTab:
      begin
        DesiredIndent := copy(Temp, 1,
                   Lines.PhysicalToLogicalCol(Temp, FoundLine - 1, Result) - 1);
        PhysLen := Lines.LogicalToPhysicalCol(Temp, ACaret.LinePos - 1, Length(Temp) + 1);
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

