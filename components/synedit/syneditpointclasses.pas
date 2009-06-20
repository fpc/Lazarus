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
  SynEditTextBase, SynEditTypes, SynEditMiscProcs;//, SynEditTextBuffer;

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
    FEnabled: Boolean;
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
    procedure SetSelTextPrimitive(PasteMode: TSynSelectionMode; Value: PChar);
    function  SelAvail: Boolean;
    function  SelCanContinue(ACaret: TSynEditCaret): Boolean;
    function  IsBackwardSel: Boolean; // SelStart < SelEnd ?
    property  Enabled: Boolean read FEnabled write SetEnabled;
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
    property  Caret: TSynEditCaret read FCaret write FCaret;
    property  UndoList: TSynEditUndoList read fUndoList write fUndoList;
    // TODO: Move dependend functions to Lines
    property TabWidth: integer read FTabWidth write FTabWidth;
  end;

  { TSynEditCaret }

  TSynEditCaret = class(TSynEditPointBase)
  private
    FAllowPastEOL: Boolean;
    FForcePastEOL: Integer;
    FKeepCaretX: Boolean;
    FLinePos: Integer;     // 1 based
    FCharPos: Integer;     // 1 based
    FLastCharPos: Integer; // used by KeepCaretX
    FOldLinePos: Integer; // 1 based
    FOldCharPos: Integer; // 1 based
    FAdjustToNextChar: Boolean;
    procedure AdjustToChar;
    procedure InternalSetLineCharPos(NewLine, NewCharPos: Integer;
                                     KeepLastCharPos: Boolean = False);
    procedure setCharPos(const AValue: Integer);
    procedure SetAllowPastEOL(const AValue: Boolean);
    procedure SetKeepCaretX(const AValue: Boolean);
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
    procedure AssignFrom(Src: TSynEditCaret);
    procedure IncForcePastEOL;
    procedure DecForcePastEOL;
    function IsAtLineChar(aPoint: TPoint): Boolean;
    function IsAtPos(aCaret: TSynEditCaret): Boolean;
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
    property KeepCaretX: Boolean read FKeepCaretX write SetKeepCaretX;
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
  FForcePastEOL := 0;
end;

procedure TSynEditCaret.AssignFrom(Src: TSynEditCaret);
begin
  FOldCharPos := FCharPos;
  FOldLinePos := FLinePos;

  FLines := Src.FLines;
  FMaxLeftChar := Src.FMaxLeftChar;
  FAllowPastEOL := Src.FAllowPastEOL;
  FKeepCaretX := Src.FKeepCaretX;
  FLinePos := Src.FLinePos;
  FCharPos := Src.FCharPos;
  FLastCharPos := Src.FLastCharPos;
end;

procedure TSynEditCaret.IncForcePastEOL;
begin
  inc(FForcePastEOL);
end;

procedure TSynEditCaret.DecForcePastEOL;
begin
  dec(FForcePastEOL);
end;

function TSynEditCaret.IsAtLineChar(aPoint: TPoint): Boolean;
begin
  Result := (FLinePos = aPoint.y) and (FCharPos = aPoint.x);
end;

function TSynEditCaret.IsAtPos(aCaret: TSynEditCaret): Boolean;
begin
  Result := IsAtLineChar(aCaret.LineCharPos);
end;

procedure TSynEditCaret.setLinePos(const AValue : Integer);
begin
  if fLinePos = AValue then exit;
  InternalSetLineCharPos(AValue, FLastCharPos, True);
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

procedure TSynEditCaret.SetKeepCaretX(const AValue: Boolean);
begin
  if FKeepCaretX = AValue then exit;
  FKeepCaretX := AValue;
  if FKeepCaretX then
    FLastCharPos := FCharPos;
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

procedure TSynEditCaret.InternalSetLineCharPos(NewLine, NewCharPos: Integer;
  KeepLastCharPos: Boolean = False);
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
      if (not FAllowPastEOL) and (FForcePastEOL = 0) then
        nMaxX := 1;
    end else begin
      if (not FAllowPastEOL) and (FForcePastEOL = 0) then begin
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
    if (not KeepLastCharPos) or (not FKeepCaretX) then
      FLastCharPos := FCharPos;
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
  SetSelTextPrimitive(smNormal, PChar(Value));
end;

procedure TSynEditSelection.SetSelTextPrimitive(PasteMode : TSynSelectionMode;
  Value : PChar);
var
  BB, BE: TPoint;

  procedure DeleteSelection;
  var
    y, l, r, xb, xe: Integer;
  begin
    case ActiveSelectionMode of
      smNormal:
        begin
          if FLines.Count > 0 then begin
            if BE.Y > BB.Y + 1 then begin
              FLines.EditLinesDelete(BB.Y + 1, BE.Y - BB.Y - 1);
              BE.Y := BB.Y + 1;
            end;
            if BE.Y > BB.Y then begin
              l := length(FLines[BB.Y - 1]);
              if BB.X > l + 1 then BB.X := l + 1;
              BE.X := BE.X + l;
              FLines.EditLineJoin(BB.Y);
              BE.Y := BB.Y;
            end;
            FLines.EditDelete(BB.X, BB.Y, BE.X - BB.X);
          end;
          FCaret.LineBytePos := BB;
        end;
      smColumn:
        begin
          FCaret.LineBytePos := BB;
          l := FCaret.CharPos;
          FCaret.LineBytePos := BE;
          r := FCaret.CharPos;
          // swap l, r if needed
          if l > r then
          {$IFDEF SYN_COMPILER_3_UP}
            SwapInt(l, r);
          {$ELSE}
          begin
            y := l;
            l := r;
            r := y;
          end;
          {$ENDIF}
          for y := BB.Y to BE.Y do begin
            FCaret.LineCharPos := Point(l, y);
            xb := FCaret.BytePos;
            FCaret.LineCharPos := Point(r, y);
            xe := Min(FCaret.BytePos, 1 + length(FCaret.LineText));
            if xe > xb then
              FLines.EditDelete(xb, y, xe - xb);
          end;
          // FLines never get deleted completely, so keep caret at end.
          FCaret.LineCharPos := Point(l, FEndLinePos);
          // Column deletion never removes a line entirely, so no mark
          // updating is needed here.
        end;
      smLine:
        begin
          if BE.Y = FLines.Count then begin
            // Keep the (CrLf of) last line, since no Line exists to replace it
            FLines.EditDelete(1, BE.Y, length(FLines[BE.Y - 1]));
            dec(BE.Y)
          end;
          if BE.Y >= BB.Y then
            FLines.EditLinesDelete(BB.Y, BE.Y - BB.Y + 1);
          FCaret.LineCharPos := Point(1, BB.Y);
        end;
    end;
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
      Str: string;
      Start: PChar;
      P: PChar;
      LogCaretXY: TPoint;
    begin
      Result := 0;
      LogCaretXY := FCaret.LineBytePos;

      Start := PChar(Value);
      P := GetEOL(Start);
      if P^ = #0 then begin
        FLines.EditInsert(LogCaretXY.X, LogCaretXY.Y, Value);
        FCaret.BytePos := FCaret.BytePos + Length(Value);
      end else begin
        SetString(Str, Value, P - Start);
        FLines.EditInsert(LogCaretXY.X, LogCaretXY.Y, Str);
        FLines.EditLineBreak(LogCaretXY.X + Length(Str), LogCaretXY.Y);
        Result :=  CountLines(P);
        if Result > 1 then
          FLines.EditLinesInsert(LogCaretXY.Y + 1, Result - 1);
        while P^ <> #0 do begin
          if P^ = #13 then
            Inc(P);
          if P^ = #10 then
            Inc(P);
          LogCaretXY.Y := LogCaretXY.Y + 1;
          Start := P;
          P := GetEOL(Start);
          if P <> Start then begin
            SetString(Str, Start, P - Start);
            FLines.EditInsert(1, LogCaretXY.Y, Str);
          end
          else
            Str := '';
        end;
        FCaret.LinePos := LogCaretXY.Y;
        FCaret.BytePos := 1 + Length(Str);
      end;
    end;

    function InsertColumn: Integer;
    var
      Str: string;
      Start: PChar;
      P: PChar;
    begin
      // Insert string at current position
      Result := 0;
      FCaret.IncForcePastEOL;
      Start := PChar(Value);
      repeat
        P := GetEOL(Start);
        if P <> Start then begin
          SetLength(Str, P - Start);
          Move(Start^, Str[1], P - Start);
          FLines.EditInsert(FCaret.BytePos, FCaret.LinePos, Str);
        end;
        if p^ in [#10,#13] then begin
          if (p[1] in [#10,#13]) and (p[1]<>p^) then
            inc(p,2)
          else
            Inc(P);
          if FCaret.LinePos = FLines.Count then
            FLines.EditLinesInsert(FCaret.LinePos + 1, 1);
            // No need to inc result => adding at EOF
          FCaret.LinePos := FCaret.LinePos + 1;
        end;
        Start := P;
      until P^ = #0;
      FCaret.BytePos:= FCaret.BytePos + Length(Str);
      FCaret.DecForcePastEOL;
    end;

    function InsertLine: Integer;
    var
      Start: PChar;
      P: PChar;
      Str: string;
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
          FLines.EditInsert(1, FCaret.LinePos, Str);
          FCaret.BytePos := 1 + Length(Str);
        end else begin
          FLines.EditLinesInsert(FCaret.LinePos, 1, Str);
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

    case PasteMode of
      smNormal:
        InsertNormal;
      smColumn:
        InsertColumn;
      smLine:
        InsertLine;
    end;
  end;

begin
  FLines.BeginUpdate;
  FCaret.Lock;
  try
    // BB is lower than BE
    BB := FirstLineBytePos;
    BE := LastLineBytePos;
    if SelAvail then begin
      DeleteSelection;
      StartLineBytePos := BB; // deletes selection // calls selection changed
    end;
    if (Value <> nil) and (Value[0] <> #0) then begin
      FCaret.LineBytePos := StartLineBytePos;
      InsertText;
      StartLineBytePos := FCaret.LineBytePos; // reset selection
    end;
  finally
    FCaret.Unlock;
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

