{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

Alternatively, the contents of this file may be used under the terms of the
GNU General Public License Version 2 or later (the "GPL"), in which case
the provisions of the GPL are applicable instead of those above.
If you wish to allow use of your version of this file only under the terms
of the GPL and not to allow others to use your version of this file
under the MPL, indicate your decision by deleting the provisions above and
replace them with the notice and other provisions required by the GPL.
If you do not delete the provisions above, a recipient may use your version
of this file under either the MPL or the GPL.

-------------------------------------------------------------------------------}

(* Naming Conventions:
  Byte = Logical: Refers to the location any TextToken has in the String.
         In Utf8String some TextToken can have more than one byte
  Char = Physical: Refers to the (x-)location on the screen matrix.
         Some TextToken (like tab) can spawn multiply char locations
*)

unit SynEditPointClasses;

{$I synedit.inc}

interface

uses
  Classes, SysUtils, LCLProc,
  {$IFDEF SYN_MBCSSUPPORT}
  Imm,
  {$ENDIF}
  SynEditTextBase, SynEditTypes, SynEditMiscProcs, SynEditTextBuffer;

type

  TInvalidateLines = procedure(FirstLine, LastLine: integer) of Object;
  TLinesCountChanged = procedure(FirstLine, Count: integer) of Object;

  { TSynEditPointBase }

  TSynEditPointBase = class
  protected
    FLines: TSynEditStrings;
    FOnChangeList: TMethodList;
    FLockCount: Integer;
    FMaxLeftChar: PInteger;
    procedure DoLock; virtual;
    Procedure DoUnlock; virtual;
  public
    constructor Create;
    constructor Create(Lines: TSynEditStrings);
    destructor Destroy; override;
    procedure AddChangeHandler(AHandler: TNotifyEvent);
    procedure RemoveChangeHandler(AHandler: TNotifyEvent);
    procedure Lock;
    Procedure Unlock;
    property  Lines: TSynEditStrings read FLines write FLines;
    property  MaxLeftChar: PInteger write FMaxLeftChar;
  end;

  TSynEditCaret = class;

  { TSynEditSelection }

  TSynEditSelection = class(TSynEditPointBase)
    FCaret: TSynEditCaret;
    fUndoList: TSynEditUndoList;
    FInvalidateLinesMethod: TInvalidateLines;
    FLinesDeletedMethod: TLinesCountChanged;
    FLinesInsertedMethod: TLinesCountChanged;
    FEnabled: Boolean;
    FSpacesToTabs: Boolean;
    FTabWidth: Integer;
    FActiveSelectionMode: TSynSelectionMode;
    FSelectionMode:       TSynSelectionMode;
    FStartLinePos: Integer; // 1 based
    FStartBytePos: Integer; // 1 based
    FEndLinePos: Integer; // 1 based
    FEndBytePos: Integer; // 1 based
  private
    function  AdjustBytePosToCharacterStart(Line: integer; BytePos: integer): integer;
    function  GetFirstLineBytePos: TPoint;
    function  GetLastLineBytePos: TPoint;
    procedure SetEnabled(const Value : Boolean);
    procedure SetActiveSelectionMode(const Value: TSynSelectionMode);
    procedure SetSelectionMode      (const AValue: TSynSelectionMode);
    function  GetStartLineBytePos: TPoint;
    procedure SetStartLineBytePos(Value: TPoint);
    function  GetEndLineBytePos: TPoint;
    procedure SetEndLineBytePos(Value: TPoint);
    function  GetSelText: string;
    procedure SetSelText(const Value: string);
  public
    constructor Create(ALines: TSynEditStrings);
    //destructor Destroy; override;
    procedure AdjustAfterTrimming; // TODO: Move into TrimView
    procedure SetSelTextPrimitive(PasteMode: TSynSelectionMode; Value: PChar;
      AddToUndoList: Boolean = false; ChangeReason: TSynChangeReason = crInsert);
    function  SelAvail: Boolean;
    function  SelCanContinue(ACaret: TSynEditCaret): Boolean;
    function  IsBackwardSel: Boolean; // SelStart < SelEnd ?
    property  Enabled: Boolean read FEnabled write SetEnabled;
    property  SpacesToTabs: Boolean read FSpacesToTabs write FSpacesToTabs;
    property  ActiveSelectionMode: TSynSelectionMode
                read FActiveSelectionMode write SetActiveSelectionMode;
    property  SelectionMode: TSynSelectionMode
                read FSelectionMode write SetSelectionMode;
    property  SelText: String read GetSelText write SetSelText;
    // Start and End positions are in the order they where defined
    // This may mean Startpos is behind EndPos in the text
    property  StartLineBytePos: TPoint
                read GetStartLineBytePos write SetStartLineBytePos;
    property  EndLineBytePos: TPoint
                read GetEndLineBytePos write SetEndLineBytePos;
    property  StartLinePos: Integer read FStartLinePos;
    property  EndLinePos: Integer read FEndLinePos;
    property  StartBytePos: Integer read FStartBytePos;
    property  EndBytePos: Integer read FEndBytePos;
    // First and Last Pos are ordered according to the text flow (LTR)
    property  FirstLineBytePos: TPoint read GetFirstLineBytePos;
    property  LastLineBytePos: TPoint read GetLastLineBytePos;
    property  InvalidateLinesMethod : TInvalidateLines write FInvalidateLinesMethod;
    property  LinesDeletedMethod: TLinesCountChanged write FLinesDeletedMethod;
    property  LinesInsertedMethod: TLinesCountChanged write FLinesInsertedMethod;
    property  Caret: TSynEditCaret read FCaret write FCaret;
    property  UndoList: TSynEditUndoList read fUndoList write fUndoList;
    // TODO: Move dependend functions to Lines
    property TabWidth: integer read FTabWidth write FTabWidth;
  end;

  { TSynEditCaret }

  TSynEditCaret = class(TSynEditPointBase)
  private
    FAllowPastEOL: Boolean;
    FForcePastEOL: Boolean;
    FLinePos: Integer; // 1 based
    FCharPos: Integer; // 1 based
    FOldLinePos: Integer; // 1 based
    FOldCharPos: Integer; // 1 based
    FAdjustToNextChar: Boolean;
    procedure AdjustToChar;
    procedure InternalSetLineCharPos(NewLine, NewCharPos: Integer);
    procedure setCharPos(const AValue: Integer);
    procedure SetAllowPastEOL(const AValue: Boolean);
    procedure setLinePos(const AValue: Integer);
    function  GetLineCharPos: TPoint;
    procedure SetLineCharPos(AValue: TPoint);
    function  GetBytePos: Integer;
    procedure SetBytePos(const AValue: Integer);
    function  GetLineBytePos: TPoint;
    procedure SetLineBytePos(const AValue: TPoint);
    function  GetLineText: string;
    procedure SetLineText(const AValue : string);
  protected
    procedure DoLock; override;
    Procedure DoUnlock; override;
  public
    constructor Create;
    property OldLinePos: Integer read FOldLinePos;
    property OldCharPos: Integer read FOldCharPos;
    property LinePos: Integer read fLinePos write setLinePos;
    property CharPos: Integer read fCharPos write setCharPos;
    property LineCharPos: TPoint read GetLineCharPos write SetLineCharPos;
    property BytePos: Integer read GetBytePos write SetBytePos;
    property LineBytePos: TPoint read GetLineBytePos write SetLineBytePos;
    property LineText: string read GetLineText write SetLineText;
    property AdjustToNextChar: Boolean read FAdjustToNextChar write FAdjustToNextChar;
    property AllowPastEOL: Boolean read FAllowPastEOL write SetAllowPastEOL;
    property ForcePastEOL: Boolean read FForcePastEOL write FForcePastEOL;
  end;

implementation

{ TSynEditPointBase }

procedure TSynEditPointBase.DoLock;
begin
end;

procedure TSynEditPointBase.DoUnlock;
begin
end;

constructor TSynEditPointBase.Create;
begin
  FOnChangeList := TMethodList.Create;
end;

constructor TSynEditPointBase.Create(Lines : TSynEditStrings);
begin
  Create;
  FLines := Lines;
end;

destructor TSynEditPointBase.Destroy;
begin
  FreeAndNil(FOnChangeList);
  inherited Destroy;
end;

procedure TSynEditPointBase.AddChangeHandler(AHandler : TNotifyEvent);
begin
  FOnChangeList.Add(TMethod(AHandler));
end;

procedure TSynEditPointBase.RemoveChangeHandler(AHandler : TNotifyEvent);
begin
  FOnChangeList.Remove(TMethod(AHandler));
end;

procedure TSynEditPointBase.Lock;
begin
  if FLockCount = 0 then
    DoLock;
  inc(FLockCount);
end;

procedure TSynEditPointBase.Unlock;
begin
  dec(FLockCount);
  if FLockCount = 0 then
    DoUnLock;
end;

{ TSynEditCaret }

constructor TSynEditCaret.Create;
begin
  inherited Create;
  fLinePos:= 1;
  fCharPos:= 1;
  FAllowPastEOL := True;
  FForcePastEOL := False;
end;

procedure TSynEditCaret.setLinePos(const AValue : Integer);
begin
  if fLinePos = AValue then exit;
  InternalSetLineCharPos(AValue, FCharPos);
end;

procedure TSynEditCaret.AdjustToChar;
var
  CharWidths: TPhysicalCharWidths;
  LogLen: Integer;
  ScreenPos: Integer;
  LogPos: Integer;
  L: String;
begin
  L := LineText;
  CharWidths := FLines.GetPhysicalCharWidths(L, FLinePos-1);
  LogLen := Length(CharWidths);
  ScreenPos := 1;
  LogPos := 0;

  while LogPos < LogLen do begin
    if ScreenPos = FCharPos then exit;
    if ScreenPos + CharWidths[LogPos] > FCharPos then begin
      if L[LogPos+1] = #9 then exit;
      if FAdjustToNextChar then
        FCharPos := ScreenPos + CharWidths[LogPos]
      else
        FCharPos := ScreenPos;
      exit;
    end;
    ScreenPos := ScreenPos + CharWidths[LogPos];
    inc(LogPos);
  end;
end;

procedure TSynEditCaret.setCharPos(const AValue : Integer);
begin
  if fCharPos = AValue then exit;
  InternalSetLineCharPos(FLinePos, AValue);
end;

procedure TSynEditCaret.SetAllowPastEOL(const AValue: Boolean);
begin
  if FAllowPastEOL = AValue then exit;
  FAllowPastEOL := AValue;
  if not FAllowPastEOL then
    InternalSetLineCharPos(FLinePos, FCharPos);
end;

function TSynEditCaret.GetLineCharPos : TPoint;
begin
  Result := Point(fCharPos, fLinePos);
end;

procedure TSynEditCaret.SetLineCharPos(AValue : TPoint);
begin
  if (fCharPos = AValue.X) and (fLinePos = AValue.Y) then exit;
  InternalSetLineCharPos(AValue.y, AValue.X);
end;

procedure TSynEditCaret.InternalSetLineCharPos(NewLine, NewCharPos: Integer);
var
  nMaxX: Integer;
  Line: string;
begin
  Lock;
  try
    nMaxX := FMaxLeftChar^;
    if NewLine > FLines.Count then
      NewLine := FLines.Count;
    if NewLine < 1 then begin
      // this is just to make sure if Lines stringlist should be empty
      NewLine := 1;
      if not (FAllowPastEOL or FForcePastEOL) then
        nMaxX := 1;
    end else begin
      if not (FAllowPastEOL or FForcePastEOL) then begin
        Line := Lines[NewLine - 1];
        nMaxX := Lines.LogicalToPhysicalCol(Line, NewLine - 1, length(Line)+1);
      end;
    end;
    if NewCharPos > nMaxX then
      NewCharPos := nMaxX;
    if NewCharPos < 1 then
      NewCharPos := 1;

    fCharPos:= NewCharPos;
    fLinePos:= NewLine;
    AdjustToChar;
  finally
    Unlock;
  end;
end;

function TSynEditCaret.GetBytePos: Integer;
begin
  Result := LineBytePos.X;
end;

procedure TSynEditCaret.SetBytePos(const AValue: Integer);
begin
  CharPos :=  FLines.LogicalToPhysicalPos(Point(AValue, LinePos)).X;
end;

function TSynEditCaret.GetLineBytePos: TPoint;
begin
  Result := FLines.PhysicalToLogicalPos(LineCharPos);
end;

procedure TSynEditCaret.SetLineBytePos(const AValue: TPoint);
begin
  LineCharPos := FLines.LogicalToPhysicalPos(AValue);
end;

function TSynEditCaret.GetLineText : string;
begin
  if (LinePos >= 1) and (LinePos <= FLines.Count) then
    Result := FLines[LinePos - 1]
  else
    Result := '';
end;

procedure TSynEditCaret.SetLineText(const AValue : string);
begin
  if (LinePos >= 1) and (LinePos <= Max(1, FLines.Count)) then
    FLines[LinePos - 1] := AValue;
end;

procedure TSynEditCaret.DoLock;
begin
  FOldCharPos := FCharPos;
  FOldLinePos := FLinePos;
end;

procedure TSynEditCaret.DoUnlock;
begin
  if (FOldCharPos <> FCharPos) or (FOldLinePos <> FLinePos) then
    fOnChangeList.CallNotifyEvents(self);
end;

{ TSynEditSelection }

constructor TSynEditSelection.Create(ALines : TSynEditStrings);
begin
  Inherited Create(ALines);
  FActiveSelectionMode := smNormal;
  FStartLinePos := 1;
  FStartBytePos := 1;
  FEndLinePos := 1;
  FEndBytePos := 1;
  FEnabled := True;
end;

procedure TSynEditSelection.AdjustAfterTrimming;
begin
  if FStartBytePos > Length(FLines[FStartLinePos-1]) + 1 then
    FStartBytePos := Length(FLines[FStartLinePos-1]) + 1;
  if FEndBytePos > Length(FLines[FEndLinePos-1]) + 1 then
    FEndBytePos := Length(FLines[FEndLinePos-1]) + 1;
end;

function TSynEditSelection.GetSelText : string;

  function CopyPadded(const S: string; Index, Count: integer): string;
  var
    SrcLen: Integer;
    DstLen: integer;
    P: PChar;
  begin
    SrcLen := Length(S);
    DstLen := Index + Count;
    if SrcLen >= DstLen then
      Result := Copy(S, Index, Count)
    else begin
      SetLength(Result, DstLen);
      P := PChar(Pointer(Result));
      StrPCopy(P, Copy(S, Index, Count));
      Inc(P, SrcLen);
      FillChar(P^, DstLen - Srclen, $20);
    end;
  end;

  procedure CopyAndForward(const S: string; Index, Count: Integer; var P:
    PChar);
  var
    pSrc: PChar;
    SrcLen: Integer;
    DstLen: Integer;
  begin
    SrcLen := Length(S);
    if (Index <= SrcLen) and (Count > 0) then begin
      Dec(Index);
      pSrc := PChar(Pointer(S)) + Index;
      DstLen := Min(SrcLen - Index, Count);
      Move(pSrc^, P^, DstLen);
      Inc(P, DstLen);
      P^ := #0;
    end;
  end;

  procedure CopyPaddedAndForward(const S: string; Index, Count: Integer;
    var P: PChar);
  var
    OldP: PChar;
    Len: Integer;
  begin
    OldP := P;
    CopyAndForward(S, Index, Count, P);
    Len := Count - (P - OldP);
    FillChar(P^, Len, #$20);
    Inc(P, Len);
  end;


const
  sLineBreak = {$IFDEF SYN_LAZARUS}LineEnding{$ELSE}#$0D#$0A{$ENDIF};
var
  First, Last, TotalLen: Integer;
  ColFrom, ColTo: Integer;
  I: Integer;
{$IFDEF SYN_MBCSSUPPORT}
  l, r: Integer;
  s: string;
{$ELSE}
  ColLen: integer;
{$ENDIF}
  P: PChar;
begin
  if not SelAvail then
    Result := ''
  else begin
    if IsBackwardSel then begin
      ColFrom := FEndBytePos;
      First := FEndLinePos - 1;
      ColTo := FStartBytePos;
      Last := FStartLinePos - 1;
    end else begin
      ColFrom := FStartBytePos;
      First := FStartLinePos - 1;
      ColTo := FEndBytePos;
      Last := FEndLinePos - 1;
    end;
    TotalLen := 0;
    case ActiveSelectionMode of
      smNormal:
        if (First = Last) then
          Result := Copy(FLines[First], ColFrom, ColTo - ColFrom)
        else begin
          // step1: calculate total length of result string
          TotalLen := Max(0, Length(FLines[First]) - ColFrom + 1);
          for i := First + 1 to Last - 1 do
            Inc(TotalLen, Length(FLines[i]));
          Inc(TotalLen, ColTo - 1);
          Inc(TotalLen, Length(sLineBreak) * (Last - First));
          // step2: build up result string
          SetLength(Result, TotalLen);
          P := PChar(Pointer(Result));
          CopyAndForward(FLines[First], ColFrom, MaxInt, P);
          CopyAndForward(sLineBreak, 1, MaxInt, P);
          for i := First + 1 to Last - 1 do begin
            CopyAndForward(FLines[i], 1, MaxInt, P);
            CopyAndForward(sLineBreak, 1, MaxInt, P);
          end;
          {$IFDEF SYN_LAZARUS}
          CopyPaddedAndForward(FLines[Last], 1, ColTo - 1, P);
          {$ELSE}
          CopyAndForward(FLines[Last], 1, ColTo - 1, P);
          {$ENDIF}
        end;
      smColumn:
        begin
          if ColFrom > ColTo then
            SwapInt(ColFrom, ColTo);
          // step1: calclate total length of result string
{$IFNDEF SYN_MBCSSUPPORT}
          ColLen := ColTo - ColFrom;
          TotalLen := ColLen + (ColLen + Length(sLineBreak)) * (Last - First);
          // step2: build up result string
          SetLength(Result, TotalLen);
          P := PChar(Pointer(Result));
          for i := First to Last - 1 do begin
            CopyPaddedAndForward(FLines[i], ColFrom, ColLen, P);
            CopyAndForward(sLineBreak, 1, MaxInt, P);
          end;
          CopyPaddedAndForward(FLines[Last], ColFrom, ColLen, P);
{$ELSE} //SYN_MBCSSUPPORT
          for i := First to Last do begin
            s := FLines[i];
            l := ColFrom;
            r := ColTo;
            MBCSGetSelRangeInLineWhenColumnActiveSelectionMode(s, l, r);
            Inc(TotalLen, r - l);
          end;
          Inc(TotalLen, Length(sLineBreak) * (Last - First));
          // step2: build up result string
          SetLength(Result, TotalLen);
          P := PChar(Result);
          for i := First to Last - 1 do begin
            s := FLines[i];
            l := ColFrom;
            r := ColTo;
            MBCSGetSelRangeInLineWhenColumnActiveSelectionMode(s, l, r);
            CopyPaddedAndForward(s, l, r - l, P);
            CopyAndForward(sLineBreak, 1, MaxInt, P);
          end;
          s := FLines[Last];
          l := ColFrom;
          r := ColTo;
          MBCSGetSelRangeInLineWhenColumnActiveSelectionMode(s, l, r);
          CopyPaddedAndForward(FLines[Last], l, r - l, P);
{$ENDIF}
        end;
      smLine:
        begin
          // If block selection includes LastLine,
          // line break code(s) of the last line will not be added.
          // step1: calclate total length of result string
          for i := First to Last do
            Inc(TotalLen, Length(FLines[i]) + Length(sLineBreak));
          if Last = FLines.Count - 1 then
            Dec(TotalLen, Length(sLineBreak));
          // step2: build up result string
          SetLength(Result, TotalLen);
          P := PChar(Pointer(Result));
          for i := First to Last - 1 do begin
            CopyAndForward(FLines[i], 1, MaxInt, P);
            CopyAndForward(sLineBreak, 1, MaxInt, P);
          end;
          CopyAndForward(FLines[Last], 1, MaxInt, P);
          if Last < FLines.Count - 1 then
            CopyAndForward(sLineBreak, 1, MaxInt, P);
        end;
    end;
  end;
end;

procedure TSynEditSelection.SetSelText(const Value : string);
begin
  SetSelTextPrimitive(smNormal, PChar(Value), true);
end;

procedure TSynEditSelection.SetSelTextPrimitive(PasteMode : TSynSelectionMode;
  Value : PChar; AddToUndoList: Boolean = false;
  ChangeReason: TSynChangeReason = crInsert);
var
  BB, BE: TPoint;
  TempString: string;

  procedure DeleteSelection;
  var
    x, MarkOffset: Integer;
    UpdateMarks: boolean;
    {$IFDEF SYN_MBCSSUPPORT}
    l, r: Integer;
    {$ENDIF}
  begin
    UpdateMarks := FALSE;
    MarkOffset := 0;
    case ActiveSelectionMode of
      smNormal:
        begin
          if FLines.Count > 0 then begin
              // Create a string that contains everything on the first line up
              // to the selection mark, and everything on the last line after
              // the selection mark.
            TempString := Copy(FLines[BB.Y - 1], 1, BB.X - 1) +
              Copy(FLines[BE.Y - 1], BE.X, MaxInt);
            // Delete all FLines in the selection range.
            FLines.DeleteLines(BB.Y-1, BE.Y - BB.Y);
            FLines[BB.Y - 1] := TempString;
          end;
          UpdateMarks := TRUE;
          FCaret.LineBytePos := BB;
        end;
      smColumn:
        begin
            // swap X if needed
          if BB.X > BE.X then
          {$IFDEF SYN_COMPILER_3_UP}
            SwapInt(BB.X, BE.X);
          {$ELSE}
          begin
            x := BB.X;
            BB.X := BE.X;
            BE.X := x;
          end;
          {$ENDIF}
          for x := BB.Y - 1 to BE.Y - 1 do begin
            TempString := FLines[x];
            {$IFNDEF SYN_MBCSSUPPORT}
            Delete(TempString, BB.X, BE.X - BB.X);
            {$ELSE}
            l := BB.X;
            r := BE.X;
            MBCSGetSelRangeInLineWhenColumnActiveSelectionMode(TempString, l, r);
            {$IFDEF USE_UTF8BIDI_LCL}
            VDelete(TempString, l, r - 1);
            {$ELSE USE_UTF8BIDI_LCL}
            Delete(TempString, l, r - l);
            {$ENDIF USE_UTF8BIDI_LCL}
            {$ENDIF}
            FLines[x] := TempString;
          end;
          // FLines never get deleted completely, so keep caret at end.
          FCaret.LineBytePos := Point(BB.X, FEndLinePos);
          // Column deletion never removes a line entirely, so no mark
          // updating is needed here.
        end;
      smLine:
        begin
          if BE.Y = FLines.Count then begin
            FLines[BE.Y - 1] := '';
            for x := BE.Y - 2 downto BB.Y - 1 do
              FLines.Delete(x);
          end else
            for x := BE.Y - 1 downto BB.Y - 1 do
              FLines.Delete(x);
            // smLine deletion always resets to first column.
          FCaret.LineCharPos := Point(1, BB.Y);
          UpdateMarks := TRUE;
          MarkOffset := 1;
        end;
    end;
    // Update marks
    if UpdateMarks then
      FLinesDeletedMethod(BB.Y, BE.Y - BB.Y + MarkOffset);
  end;

  procedure InsertText;

    function CountLines(p: PChar): integer;
    begin
      Result := 0;
      while p^ <> #0 do begin
        if p^ = #13 then
          Inc(p);
        if p^ = #10 then
          Inc(p);
        Inc(Result);
        p := GetEOL(p);
      end;
    end;

    function InsertNormal: Integer;
    var
      sLeftSide: string;
      sRightSide: string;
      Str: string;
      Start: PChar;
      P: PChar;
      LogCaretXY: TPoint;
      PhysicalLineEndPos: LongInt;
    begin
      Result := 0;
      LogCaretXY := FCaret.LineBytePos;
      sLeftSide := Copy(FCaret.LineText, 1, LogCaretXY.X - 1);
      if LogCaretXY.X - 1 > Length(sLeftSide) then begin
        PhysicalLineEndPos:= FLines.LogicalToPhysicalPos
          (Point(Length(sLeftSide)+1, FCaret.LinePos)).X-1;
        sLeftSide := sLeftSide
                     + CreateTabsAndSpaces(FCaret.CharPos,
                                           FCaret.CharPos-1-PhysicalLineEndPos,
                                           FTabWidth, FSpacesToTabs);
      end;
      sRightSide := Copy(FCaret.LineText, LogCaretXY.X,
                         Length(FCaret.LineText) - (LogCaretXY.X - 1));
      // step1: insert the first line of Value into current line
      Start := PChar(Value);
      P := GetEOL(Start);
      if P^ <> #0 then begin
        SetString(Str, Value, P - Start);
        FLines.InsertLines(FCaret.LinePos - 1, CountLines(P));
        FLines[FCaret.LinePos - 1] := sLeftSide + Str;
      end else begin
        FLines[FCaret.LinePos - 1] := sLeftSide + Value + sRightSide;
        FCaret.BytePos := 1 + Length(sLeftSide + Value);
      end;
      // step2: insert left lines of Value
      while P^ <> #0 do begin
        if P^ = #13 then
          Inc(P);
        if P^ = #10 then
          Inc(P);
        FCaret.LinePos := FCaret.LinePos + 1;
        Start := P;
        P := GetEOL(Start);
        if P = Start then begin
          if p^ <> #0 then
            FLines[FCaret.LinePos - 1] := ''
          else
            FLines[FCaret.LinePos - 1] := sRightSide;
        end else begin
          SetString(Str, Start, P - Start);
          if p^ <> #0 then
            FLines[FCaret.LinePos - 1] := Str
          else
            FLines[FCaret.LinePos - 1] := Str + sRightSide
        end;
        if p^=#0 then
          FCaret.BytePos := 1 + Length(FLines[FCaret.LinePos - 1]) - Length(sRightSide);
        Inc(Result);
      end;
    end;

    function InsertColumn: Integer;
    var
      Str: string;
      Start: PChar;
      P: PChar;
      Len: Integer;
      InsertPos: Integer;
      LogicalInsertPos: Integer;
    begin
      // Insert string at current position
      InsertPos := FCaret.CharPos;
      Start := PChar(Value);
      repeat
        P := GetEOL(Start);
        if P <> Start then begin
          SetLength(Str, P - Start);
          Move(Start^, Str[1], P - Start);
          if FCaret.LinePos > FLines.Count then {useless check. FCaret.LinePos cannot exceed FLines.Count}
            FLines.Add(StringOfChar(' ', InsertPos - 1) + Str)
          else begin
            TempString := FLines[FCaret.LinePos - 1];
            Len := Length(TempString);
            LogicalInsertPos := FLines.PhysicalToLogicalCol(TempString,
                                                 FCaret.LinePos - 1, InsertPos);
            if Len < LogicalInsertPos
            then begin
              TempString :=
                TempString + StringOfChar(' ', LogicalInsertPos - Len - 1)
                + Str
            end else begin
              {$IFDEF SYN_MBCSSUPPORT}
              if mbTrailByte = ByteType(TempString, InsertPos) then
                Insert(Str, TempString, InsertPos + 1)
              else
              {$ENDIF}
                System.Insert(Str, TempString, LogicalInsertPos);
            end;
            FLines[FCaret.LinePos - 1] := TempString;
          end;
        end;
        if p^ in [#10,#13] then begin
          if (p[1] in [#10,#13]) and (p[1]<>p^) then
            inc(p,2)
          else
            Inc(P);
          if FCaret.LinePos = FLines.Count then
            FLines.Add(StringOfChar(' ', InsertPos - 1));
          FCaret.LinePos := FCaret.LinePos + 1;
        end;
        Start := P;
      until P^ = #0;
      FCaret.BytePos:= FCaret.BytePos + Length(Str);
      Result := 0;
    end;

    function InsertLine: Integer;
    var
      Start: PChar;
      P: PChar;
      Str: string;
      n: Integer;
    begin
      Result := 0;
      FCaret.CharPos := 1;
      // Insert string before current line
      Start := PChar(Value);
      repeat
        P := GetEOL(Start);
        if P <> Start then begin
          SetLength(Str, P - Start);
          Move(Start^, Str[1], P - Start);
        end else
          Str := '';
        if (P^ = #0) then begin  // Not a full line?
          n := FLines.Count;
          if (n >= FCaret.LinePos) then
            FLines[FCaret.LinePos - 1] := Str + FLines[FCaret.LinePos - 1]
          else
            FLines.Add(Str);
          FCaret.CharPos := 1 + Length(Str);
        end else begin
          FLines.Insert(FCaret.LinePos - 1, Str);
          FCaret.LinePos := FCaret.LinePos + 1;
          Inc(Result);
          if P^ = #13 then
            Inc(P);
          if P^ = #10 then
            Inc(P);
          Start := P;
        end;
      until P^ = #0;
    end;

  var
    StartLine: Integer;
    InsertedLines: Integer;
  begin
    if Value = '' then
      Exit;
    if FLines.Count = 0 then
      FLines.Add('');

    // Using a TStringList to do this would be easier, but if we're dealing
    // with a large block of text, it would be very inefficient.  Consider:
    // Assign Value parameter to TStringList.Text: that parses through it and
    // creates a copy of the string for each line it finds.  That copy is passed
    // to the Add method, which in turn creates a copy.  Then, when you actually
    // use an item in the list, that creates a copy to return to you.  That's
    // 3 copies of every string vs. our one copy below.  I'd prefer no copies,
    // but we aren't set up to work with PChars that well.

    StartLine := FCaret.LinePos;
    case PasteMode of
      smNormal:
        InsertedLines := InsertNormal;
      smColumn:
        InsertedLines := InsertColumn;
      smLine:
        InsertedLines := InsertLine;
    else
      InsertedLines := 0;
    end;
    // We delete selected based on the current selection mode, but paste
    // what's on the clipboard according to what it was when copied.
    // Update marks
    if InsertedLines > 0 then
      FLinesInsertedMethod(StartLine, InsertedLines);
  end;

var
  StartInsert, EndInsert: TPoint;
begin
  FLines.BeginUpdate;
  try
    // BB is lower than BE
    BB := FirstLineBytePos;
    BE := LastLineBytePos;
    if SelAvail then begin
      // todo: better move add-undo past actual delete
      FLines[BB.Y - 1] := FLines[BB.Y - 1]; // TrimRealSpaces (in case of smNormal or smLine
      if AddToUndoList then begin
        if ChangeReason in [crSilentDelete, crSilentDeleteAfterCursor] then begin
          if IsBackwardSel then
            fUndoList.AddChange(crSilentDeleteAfterCursor, StartLineBytePos, EndLineBytePos,
                                GetSelText, ActiveSelectionMode)
          else
            fUndoList.AddChange(crSilentDelete, StartLineBytePos, EndLineBytePos,
                                GetSelText,  ActiveSelectionMode);
        end else begin
          if IsBackwardSel then
            fUndoList.AddChange(crDeleteAfterCursor, StartLineBytePos, EndLineBytePos,
                                GetSelText, ActiveSelectionMode)
          else
            fUndoList.AddChange(crDelete, StartLineBytePos, EndLineBytePos,
                                GetSelText,  ActiveSelectionMode);
        end;
      end;
      DeleteSelection;
      EndLineBytePos := BB; // deletes selection // calls selection changed
    end;
    if (Value <> nil) and (Value[0] <> #0) then begin
      StartInsert := FCaret.LineBytePos;
      InsertText;
      if AddToUndoList then begin
        EndInsert := FCaret.LineBytePos;
        if ActiveSelectionMode = smLine then begin // The ActiveSelectionMode of the deleted block
          StartInsert.x := 1;
          if EndInsert.x = 1 then begin
            dec(EndInsert.y);
            EndInsert.x := Length(FLines[EndInsert.y - 1]);
          end;
        end;
        if ChangeReason in [crSilentDelete, crSilentDeleteAfterCursor, crDelete,
                            crDeleteAfterCursor] then
          ChangeReason := crInsert;
        fUndoList.AddChange(ChangeReason, StartInsert, EndInsert, '', PasteMode);
      end;
      StartLineBytePos := FCaret.LineBytePos; // reset selection
    end;
  finally
    FLines.EndUpdate; // May reset Block Begin
  end;
end;

function TSynEditSelection.GetStartLineBytePos : TPoint;
begin
  Result.y := FStartLinePos;
  Result.x := FStartBytePos;
end;

procedure TSynEditSelection.SetEnabled(const Value : Boolean);
begin
  if FEnabled = Value then exit;
  FEnabled := Value;
  if not Enabled then SetStartLineBytePos(EndLineBytePos);
end;

procedure TSynEditSelection.SetStartLineBytePos(Value : TPoint);
// logical position (byte)
var
  nInval1, nInval2: integer;
  SelChanged: boolean;
begin
  Value.x := MinMax(Value.x, 1, FMaxLeftChar^);
  Value.y := MinMax(Value.y, 1, fLines.Count);
  if (ActiveSelectionMode = smNormal) then
    if (Value.y >= 1) and (Value.y <= FLines.Count) then
      Value.x := AdjustBytePosToCharacterStart(Value.y,Value.x)
    else
      Value.x := 1;
  if SelAvail then begin
    if FStartLinePos < FEndLinePos then begin
      nInval1 := Min(Value.Y, FStartLinePos);
      nInval2 := Max(Value.Y, FEndLinePos);
    end else begin
      nInval1 := Min(Value.Y, FEndLinePos);
      nInval2 := Max(Value.Y, FStartLinePos);
    end;
    FInvalidateLinesMethod(nInval1, nInval2);
    SelChanged := TRUE;
  end else begin
    SelChanged := (FStartBytePos <> Value.X) or (FStartLinePos <> Value.Y) or
                  (FEndBytePos <> Value.X) or (FEndLinePos <> Value.Y);
  end;
  FActiveSelectionMode := FSelectionMode;
  FStartLinePos := Value.Y;
  FStartBytePos := Value.X;
  FEndLinePos := Value.Y;
  FEndBytePos := Value.X;
  if SelChanged then
    fOnChangeList.CallNotifyEvents(self);
end;

function TSynEditSelection.GetEndLineBytePos : TPoint;
begin
  Result.y := FEndLinePos;
  Result.x := FEndBytePos;
end;

procedure TSynEditSelection.SetEndLineBytePos(Value : TPoint);
var
  nLine: integer;
  {$IFDEF SYN_MBCSSUPPORT}
  s: string;
  {$ENDIF}
begin
  if FEnabled then begin
    Value.x := MinMax(Value.x, 1, FMaxLeftChar^);
    Value.y := MinMax(Value.y, 1, fLines.Count);
    if (ActiveSelectionMode = smNormal) then
      if (Value.y >= 1) and (Value.y <= fLines.Count) then
        Value.x := AdjustBytePosToCharacterStart(Value.y,Value.x)
      else
        Value.x := 1;
    if (Value.X <> FEndBytePos) or (Value.Y <> FEndLinePos) then begin
      {$IFDEF SYN_MBCSSUPPORT}
      if Value.Y <= fLines.Count then begin
        s := fLines[Value.Y - 1];
        if (Length(s) >= Value.X) and (mbTrailByte = ByteType(s, Value.X)) then
          Dec(Value.X);
      end;
      {$ENDIF}
      if (Value.X <> FEndBytePos) or (Value.Y <> FEndLinePos) then begin
        if (ActiveSelectionMode = smColumn) and (Value.X <> FEndBytePos) then begin
          FInvalidateLinesMethod(
            Min(FStartLinePos, Min(FEndLinePos, Value.Y)),
            Max(FStartLinePos, Max(FEndLinePos, Value.Y)));
          FEndLinePos := Value.Y;
          FEndBytePos := Value.X;
        end else begin
          nLine := FEndLinePos;
          FEndLinePos := Value.Y;
          FEndBytePos := Value.X;
          if (ActiveSelectionMode <> smColumn) or (FStartBytePos <> FEndBytePos) then
            FInvalidateLinesMethod(nLine, FEndLinePos);
        end;
        FOnChangeList.CallNotifyEvents(self);
      end;
    end;
  end;
end;

procedure TSynEditSelection.SetSelectionMode(const AValue: TSynSelectionMode);
begin
  FSelectionMode := AValue;
  SetActiveSelectionMode(AValue);
  fOnChangeList.CallNotifyEvents(self);
end;

procedure TSynEditSelection.SetActiveSelectionMode(const Value: TSynSelectionMode);
begin
  if FActiveSelectionMode <> Value then begin
    FActiveSelectionMode := Value;
    if SelAvail then
      FInvalidateLinesMethod(-1, -1);
    FOnChangeList.CallNotifyEvents(self);
  end;
end;

// Only needed if the Selection is set from External
function TSynEditSelection.AdjustBytePosToCharacterStart(Line : integer; BytePos : integer) : integer;
var
  s: string;
begin
  Result := BytePos;
  if Result < 1 then
    Result := 1
  else if (Line >= 1) and (Line <= FLines.Count) then begin
    s := FLines[Line-1];
    if (Result <= length(s)) and FLines.IsUtf8 then
      Result:=UTF8FindNearestCharStart(PChar(Pointer(s)),length(s),Result - 1) + 1;
  end;
  if Result <> BytePos then debugln(['Selection needed byte adjustment  Line=', Line, ' BytePos=', BytePos, ' Result=', Result]);
end;

function TSynEditSelection.GetFirstLineBytePos: TPoint;
begin
  if IsBackwardSel then
    Result := EndLineBytePos
  else
    Result := StartLineBytePos;
end;

function TSynEditSelection.GetLastLineBytePos: TPoint;
begin
  if IsBackwardSel then
    Result := StartLineBytePos
  else
    Result := EndLineBytePos;
end;

function TSynEditSelection.SelAvail : Boolean;
begin
  Result := (FStartBytePos <> FEndBytePos) or
    ((FStartLinePos <> FEndLinePos) and (FActiveSelectionMode <> smColumn));
end;

function TSynEditSelection.SelCanContinue(ACaret: TSynEditCaret): Boolean;
begin
  if SelAvail then exit(True);
  Result := (FActiveSelectionMode = smColumn) and (FEndLinePos = ACaret.LinePos)
    and (FEndBytePos = ACaret.BytePos);
end;

function TSynEditSelection.IsBackwardSel: Boolean;
begin
  Result := (FStartLinePos > FEndLinePos)
    or ((FStartLinePos = FEndLinePos) and (FStartBytePos > FEndBytePos));
end;

end.

