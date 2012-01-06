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

{off $DEFINE SynCaretDebug}

interface

uses
  Classes, SysUtils, Controls, LCLProc, LCLType, LCLIntf,
  {$IFDEF SYN_MBCSSUPPORT}
  Imm,
  {$ENDIF}
  LazSynEditText, SynEditTypes, SynEditMiscProcs;//, SynEditTextBuffer;

type

  TInvalidateLines = procedure(FirstLine, LastLine: integer) of Object;
  TLinesCountChanged = procedure(FirstLine, Count: integer) of Object;
  TMaxLeftCharFunc = function: Integer of object;

  { TSynEditPointBase }

  TSynEditPointBase = class
  private
    function GetLocked: Boolean;
  protected
    FLines: TSynEditStrings;
    FOnChangeList: TMethodList;
    FLockCount: Integer;
    procedure SetLines(const AValue: TSynEditStrings); virtual;
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
    property  Lines: TSynEditStrings read FLines write SetLines;
    property Locked: Boolean read GetLocked;
  end;

  TSynEditCaret = class;

  { TSynEditSelection }

  TSynEditSelection = class(TSynEditPointBase)
  private
    FAutoExtend: Boolean;
    FCaret: TSynEditCaret;
    FHide: Boolean;
    FInternalCaret: TSynEditCaret;
    FInvalidateLinesMethod: TInvalidateLines;
    FEnabled: Boolean;
    FHookedLines: Boolean;
    FIsSettingText: Boolean;
    FActiveSelectionMode: TSynSelectionMode;
    FSelectionMode:       TSynSelectionMode;
    FStartLinePos: Integer; // 1 based
    FStartBytePos: Integer; // 1 based
    FEndLinePos: Integer; // 1 based
    FEndBytePos: Integer; // 1 based
    FPersistent: Boolean;
    FPersistentLock: Integer;
    FIgnoreNextCaretMove: Boolean;
    (* On any modification, remember the position of the caret.
       If it gets moved from there to either end of the block, this should be ignored
       This happens, if Block and caret are adjusted directly
    *)
    FLastCarePos: TPoint;
    function  AdjustBytePosToCharacterStart(Line: integer; BytePos: integer): integer;
    function  GetFirstLineBytePos: TPoint;
    function  GetLastLineBytePos: TPoint;
    procedure SetCaret(const AValue: TSynEditCaret);
    procedure SetEnabled(const Value : Boolean);
    procedure SetActiveSelectionMode(const Value: TSynSelectionMode);
    procedure SetHide(const AValue: Boolean);
    procedure SetPersistent(const AValue: Boolean);
    procedure SetSelectionMode      (const AValue: TSynSelectionMode);
    function  GetStartLineBytePos: TPoint;
    procedure SetStartLineBytePos(Value: TPoint);
    procedure AdjustStartLineBytePos(Value: TPoint);
    function  GetEndLineBytePos: TPoint;
    procedure SetEndLineBytePos(Value: TPoint);
    function  GetSelText: string;
    procedure SetSelText(const Value: string);
    procedure DoCaretChanged(Sender: TObject);
    procedure AdjustAfterTrimming; // TODO: Move into TrimView?
  protected
    procedure DoLock; override;
    procedure DoUnlock; override;
    Procedure LineChanged(Sender: TSynEditStrings; AIndex, ACount : Integer);
    procedure DoLinesEdited(Sender: TSynEditStrings; aLinePos, aBytePos, aCount,
                            aLineBrkCnt: Integer; aText: String);
  public
    constructor Create(ALines: TSynEditStrings; aActOnLineChanges: Boolean);
    destructor Destroy; override;
    procedure AssignFrom(Src: TSynEditSelection);
    procedure SetSelTextPrimitive(PasteMode: TSynSelectionMode; Value: PChar; AReplace: Boolean = False);
    function  SelAvail: Boolean;
    function  SelCanContinue(ACaret: TSynEditCaret): Boolean;
    function  IsBackwardSel: Boolean; // SelStart < SelEnd ?
    procedure SortSelectionPoints;
    procedure IgnoreNextCaretMove;
    procedure IncPersistentLock;
    procedure DecPersistentLock;
    procedure Clear;
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
    property  StartLineBytePosAdjusted: TPoint
                 write AdjustStartLineBytePos;
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
    property  Caret: TSynEditCaret read FCaret write SetCaret;
    property  Persistent: Boolean read FPersistent write SetPersistent;
    // automatically Start/Exctend selection if caret moves
    // (depends if caret was at block border or not)
    property  AutoExtend: Boolean read FAutoExtend write FAutoExtend;
    property  Hide: Boolean read FHide write SetHide;
  end;

  { TSynEditCaret }

  TSynEditCaret = class(TSynEditPointBase)
  private
    FAllowPastEOL: Boolean;
    FAutoMoveOnEdit: Integer;
    FForcePastEOL: Integer;
    FForceAdjustToNextChar: Integer;
    FKeepCaretX: Boolean;
    FLinePos: Integer;     // 1 based
    FCharPos: Integer;     // 1 based
    FLastCharPos: Integer; // used by KeepCaretX
    FBytePos, FBytePosOffset: Integer;     // 1 based
    FOldLinePos: Integer; // 1 based
    FOldCharPos: Integer; // 1 based
    FAdjustToNextChar: Boolean;
    FMaxLeftChar: TMaxLeftCharFunc;
    FChangeOnTouch: Boolean;
    FSkipTabs: Boolean;
    FTouched: Boolean;

    procedure AdjustToChar;
    procedure UpdateBytePos;
    function GetOldLineBytePos: TPoint;
    function GetOldLineCharPos: TPoint;
    procedure InternalSetLineCharPos(NewLine, NewCharPos: Integer;
                                     KeepLastCharPos: Boolean = False;
                                     ForceSet: Boolean = False);
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
    procedure SetSkipTabs(const AValue: Boolean);
  protected
    procedure SetLines(const AValue: TSynEditStrings); override;
    procedure DoLock; override;
    Procedure DoUnlock; override;
    procedure DoLinesEdited(Sender: TSynEditStrings; aLinePos, aBytePos, aCount,
                            aLineBrkCnt: Integer; aText: String);
  public
    constructor Create;
    destructor Destroy; override;
    procedure AssignFrom(Src: TSynEditCaret);
    procedure IncForcePastEOL;
    procedure DecForcePastEOL;
    procedure IncForceAdjustToNextChar;
    procedure DecForceAdjustToNextChar;
    procedure IncAutoMoveOnEdit;
    procedure DecAutoMoveOnEdit;
    procedure ChangeOnTouch;
    function IsAtLineChar(aPoint: TPoint): Boolean;
    function IsAtLineByte(aPoint: TPoint): Boolean;
    function WasAtLineChar(aPoint: TPoint): Boolean;
    function WasAtLineByte(aPoint: TPoint): Boolean;
    function IsAtPos(aCaret: TSynEditCaret): Boolean;

    property OldLinePos: Integer read FOldLinePos;
    property OldCharPos: Integer read FOldCharPos;
    property OldLineCharPos: TPoint read GetOldLineCharPos;
    property OldLineBytePos: TPoint read GetOldLineBytePos;

    property LinePos: Integer read fLinePos write setLinePos;
    property CharPos: Integer read fCharPos write setCharPos;
    property LineCharPos: TPoint read GetLineCharPos write SetLineCharPos;
    property BytePos: Integer read GetBytePos write SetBytePos;
    property LineBytePos: TPoint read GetLineBytePos write SetLineBytePos;
    property LineText: string read GetLineText write SetLineText;

    property AdjustToNextChar: Boolean read FAdjustToNextChar write FAdjustToNextChar;
    property SkipTabs: Boolean read FSkipTabs write SetSkipTabs;
    property AllowPastEOL: Boolean read FAllowPastEOL write SetAllowPastEOL;
    property KeepCaretX: Boolean read FKeepCaretX write SetKeepCaretX;
    property MaxLeftChar: TMaxLeftCharFunc write FMaxLeftChar;
  end;

  TSynCaretType = (ctVerticalLine, ctHorizontalLine, ctHalfBlock, ctBlock);

  { TSynEditScreenCaret }

  TSynEditScreenCaret = class
  private
    FCharHeight: Integer;
    FCharWidth: Integer;
    FClipRight: Integer;
    FClipBottom: Integer;
    FClipLeft: Integer;
    FClipTop: Integer;
    FDisplayPos: TPoint;
    FDisplayType: TSynCaretType;
    FExtraLinePixel, FExtraLineChars: Integer;
    FOnExtraLineCharsChanged: TNotifyEvent;
    FVisible: Boolean;
    FHandleOwner: TWinControl;
    function GetHandle: HWND;
    function GetHandleAllocated: Boolean;
    procedure SetCharHeight(const AValue: Integer);
    procedure SetCharWidth(const AValue: Integer);
    procedure SetClipRight(const AValue: Integer);
    procedure SetDisplayPos(const AValue: TPoint);
    procedure SetDisplayType(const AType: TSynCaretType);
    procedure SetVisible(const AValue: Boolean);
  private
    FClipExtraPixel: Integer;
    {$IFDeF SynCaretDebug}
    FDebugShowCount: Integer;
    {$ENDIF}
    FPixelWidth, FPixelHeight: Integer;
    FOffsetX, FOffsetY: Integer;
    FCurrentPosX, FCurrentPosY: Integer;
    FCurrentVisible, FCurrentCreated: Boolean;
    FCurrentClippedWidth: Integer;
    FLockCount: Integer;
    FLockedUpdateNeeded: Boolean;
    procedure SetClipBottom(const AValue: Integer);
    procedure SetClipExtraPixel(AValue: Integer);
    procedure SetClipLeft(const AValue: Integer);
    procedure SetClipRect(const AValue: TRect);
    procedure SetClipTop(const AValue: Integer);
    procedure CalcExtraLineChars;
    procedure UpdateDisplayType;
    procedure UpdateDisplay;
    procedure ShowCaret;
    procedure HideCaret;
    property Handle: HWND read GetHandle;
    property HandleAllocated: Boolean read GetHandleAllocated;
  public
    constructor Create(AHandleOwner: TWinControl);
    destructor Destroy; override;
    procedure  Hide; // Keep visible = true
    procedure  DestroyCaret(SkipHide: boolean = False);
    procedure  Lock;
    procedure  UnLock;
    property HandleOwner: TWinControl read FHandleOwner;
    property CharWidth:   Integer read FCharWidth write SetCharWidth;
    property CharHeight:  Integer read FCharHeight write SetCharHeight;
    property ClipLeft:    Integer read FClipLeft write SetClipLeft;
    property ClipRight:   Integer read FClipRight write SetClipRight;           // First pixel outside the allowed area
    property ClipTop:     Integer read FClipTop write SetClipTop;
    property ClipRect:    TRect write SetClipRect;
    property ClipBottom:  Integer read FClipBottom write SetClipBottom;
    property ClipExtraPixel: Integer read FClipExtraPixel write SetClipExtraPixel; // Amount of pixels, after  the last full char (half visible char width)
    property Visible:     Boolean read FVisible write SetVisible;
    property DisplayType: TSynCaretType read FDisplayType write SetDisplayType;
    property DisplayPos:  TPoint  read FDisplayPos write SetDisplayPos;
    property ExtraLineChars: Integer read FExtraLineChars; // Extend the longest line by x chars
    property OnExtraLineCharsChanged: TNotifyEvent
             read FOnExtraLineCharsChanged write FOnExtraLineCharsChanged;
  end;

implementation

{ TSynEditPointBase }

function TSynEditPointBase.GetLocked: Boolean;
begin
  Result := FLockCount > 0;
end;

procedure TSynEditPointBase.SetLines(const AValue: TSynEditStrings);
begin
  FLines := AValue;
end;

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
  FMaxLeftChar := nil;
  fLinePos:= 1;
  fCharPos:= 1;
  FAllowPastEOL := True;
  FForcePastEOL := 0;
  FAutoMoveOnEdit := 0;
  if FLines <> nil then
    FLines.AddEditHandler(@DoLinesEdited);
end;

destructor TSynEditCaret.Destroy;
begin
  if FLines <> nil then
    FLines.RemoveEditHandler(@DoLinesEdited);
  inherited Destroy;
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

procedure TSynEditCaret.IncForceAdjustToNextChar;
begin
  Inc(FForceAdjustToNextChar);
end;

procedure TSynEditCaret.DecForceAdjustToNextChar;
begin
  Dec(FForceAdjustToNextChar);
end;

procedure TSynEditCaret.IncAutoMoveOnEdit;
begin
  if FAutoMoveOnEdit =0 then
    UpdateBytePos;;
  inc(FAutoMoveOnEdit);
end;

procedure TSynEditCaret.DecAutoMoveOnEdit;
begin
  dec(FAutoMoveOnEdit);
end;

procedure TSynEditCaret.ChangeOnTouch;
begin
  FChangeOnTouch := True;
  if not Locked then
    FTouched := False;
end;

function TSynEditCaret.IsAtLineChar(aPoint: TPoint): Boolean;
begin
  Result := (FLinePos = aPoint.y) and (FCharPos = aPoint.x);
end;

function TSynEditCaret.IsAtLineByte(aPoint: TPoint): Boolean;
begin
  Result := (FLinePos = aPoint.y) and (BytePos = aPoint.x);
end;

function TSynEditCaret.WasAtLineChar(aPoint: TPoint): Boolean;
begin
  Result := (FOldLinePos = aPoint.y) and (FOldCharPos = aPoint.x);
end;

function TSynEditCaret.WasAtLineByte(aPoint: TPoint): Boolean;
begin
  Result := (FOldLinePos = aPoint.y) and
            (FLines.PhysicalToLogicalPos(Point(FOldCharPos, FOldLinePos)).X = aPoint.x);
end;

function TSynEditCaret.IsAtPos(aCaret: TSynEditCaret): Boolean;
begin
  Result := IsAtLineChar(aCaret.LineCharPos);
end;

procedure TSynEditCaret.setLinePos(const AValue : Integer);
begin
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
  CharWidths := FLines.GetPhysicalCharWidths(Pchar(L), length(L), FLinePos-1);
  LogLen := Length(CharWidths);
  ScreenPos := 1;
  LogPos := 0;

  while LogPos < LogLen do begin
    if ScreenPos = FCharPos then exit;
    if ScreenPos + CharWidths[LogPos] > FCharPos then begin
      if (L[LogPos+1] = #9) and (not FSkipTabs) then exit;
      if FAdjustToNextChar or (FForceAdjustToNextChar > 0) then
        FCharPos := ScreenPos + CharWidths[LogPos]
      else
        FCharPos := ScreenPos;
      exit;
    end;
    ScreenPos := ScreenPos + CharWidths[LogPos];
    inc(LogPos);
  end;
end;

procedure TSynEditCaret.UpdateBytePos;
begin
  FBytePos := FLines.LogPhysConvertor.PhysicalToLogical(FLinePos-1, FCharPos, FBytePosOffset);
end;

function TSynEditCaret.GetOldLineBytePos: TPoint;
begin
  Result := FLines.PhysicalToLogicalPos(OldLineCharPos);
end;

function TSynEditCaret.GetOldLineCharPos: TPoint;
begin
  Result := Point(FOldCharPos, FOldLinePos);
end;

procedure TSynEditCaret.setCharPos(const AValue : Integer);
begin
  InternalSetLineCharPos(FLinePos, AValue);
end;

procedure TSynEditCaret.SetAllowPastEOL(const AValue: Boolean);
begin
  if FAllowPastEOL = AValue then exit;
  FAllowPastEOL := AValue;
  if not FAllowPastEOL then
    InternalSetLineCharPos(FLinePos, FCharPos, True, True);
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
  InternalSetLineCharPos(AValue.y, AValue.X);
end;

procedure TSynEditCaret.InternalSetLineCharPos(NewLine, NewCharPos: Integer;
  KeepLastCharPos: Boolean = False; ForceSet: Boolean = False);
var
  nMaxX, i: Integer;
  Line: string;
begin
  Lock;
  FTouched := True;
  try
    if (fCharPos <> NewCharPos) or (fLinePos <> NewLine) or ForceSet then begin
      if FMaxLeftChar <> nil then
        nMaxX := FMaxLeftChar()
      else
        nMaxX := MaxInt;
      if NewLine > FLines.Count then
        NewLine := FLines.Count;
      if NewLine < 1 then begin
        // this is just to make sure if Lines stringlist should be empty
        NewLine := 1;
        if (not FAllowPastEOL) and (FForcePastEOL = 0) then
          nMaxX := 1;
      end else begin
        Line := Lines[NewLine - 1];
        i := Lines.LogicalToPhysicalCol(Line, NewLine - 1, length(Line)+1);
        if ((not FAllowPastEOL) and (FForcePastEOL = 0)) or (nMaxX < i) then
          nMaxX := i;
      end;
      if NewCharPos > nMaxX then
        NewCharPos := nMaxX;
      if NewCharPos < 1 then
        NewCharPos := 1;

      fCharPos:= NewCharPos;
      fLinePos:= NewLine;
      AdjustToChar;
      if (not KeepLastCharPos) or (not FKeepCaretX) then
        FLastCharPos := FCharPos;
      if FAutoMoveOnEdit <> 0 then
        UpdateBytePos;
    end;
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

procedure TSynEditCaret.SetSkipTabs(const AValue: Boolean);
begin
  if FSkipTabs = AValue then exit;
  FSkipTabs := AValue;
  if FSkipTabs then begin
    Lock;
    AdjustToChar;
    Unlock;
  end;
end;

procedure TSynEditCaret.SetLines(const AValue: TSynEditStrings);
begin
  if FLines = AValue then exit;
  if FLines <> nil then
    FLines.RemoveEditHandler(@DoLinesEdited);
  inherited SetLines(AValue);
  if FLines <> nil then
    FLines.AddEditHandler(@DoLinesEdited);
end;

procedure TSynEditCaret.DoLock;
begin
  FTouched := False;
  FOldCharPos := FCharPos;
  FOldLinePos := FLinePos;
end;

procedure TSynEditCaret.DoUnlock;
begin
  if not FChangeOnTouch then
    FTouched := False;
  FChangeOnTouch := False;
  if (FOldCharPos <> FCharPos) or (FOldLinePos <> FLinePos) or FTouched then
    fOnChangeList.CallNotifyEvents(self);
  // All notifications called, reset oldpos
  FTouched := False;
  FOldCharPos := FCharPos;
  FOldLinePos := FLinePos;
end;

procedure TSynEditCaret.DoLinesEdited(Sender: TSynEditStrings; aLinePos, aBytePos, aCount,
  aLineBrkCnt: Integer; aText: String);
  // Todo: refactor / this is a copy from selection
  function AdjustPoint(aPoint: Tpoint): TPoint; inline;
  begin
    Result := aPoint;
    if aLineBrkCnt < 0 then begin
      (* Lines Deleted *)
      if aPoint.y > aLinePos then begin
        Result.y := Max(aLinePos, Result.y + aLineBrkCnt);
        if Result.y = aLinePos then
          Result.x := Result.x + aBytePos - 1;
      end;
    end
    else
    if aLineBrkCnt > 0 then begin
      (* Lines Inserted *)
      if (aPoint.y = aLinePos) and (aPoint.x >= aBytePos) then begin
        Result.x := Result.x - aBytePos + 1;
        Result.y := Result.y + aLineBrkCnt;
      end;
      if aPoint.y > aLinePos then begin
        Result.y := Result.y + aLineBrkCnt;
      end;
    end
    else
    if aCount <> 0 then begin
      (* Chars Insert/Deleted *)
      if (aPoint.y = aLinePos) and (aPoint.x >= aBytePos) then
        Result.x := Max(aBytePos, Result.x + aCount);
    end;
  end;

var
  p: TPoint;
begin
  if FAutoMoveOnEdit > 0 then begin
    IncForcePastEOL;
    p :=  AdjustPoint(Point(FBytePos, FLinePos));
    p.x := FLines.LogPhysConvertor.LogicalToPhysical(p.y-1, p.x, FBytePosOffset);
    FBytePos := -1;
    LineCharPos := p;
    if FBytePos < 0 then
      UpdateBytePos;
    DecForcePastEOL;
  end;
end;

{ TSynEditSelection }

constructor TSynEditSelection.Create(ALines : TSynEditStrings; aActOnLineChanges: Boolean);
begin
  Inherited Create(ALines);
  FInternalCaret := TSynEditCaret.Create; // TODO: does not need FLines.AddEditHandler
  FInternalCaret.Lines := FLines;

  FActiveSelectionMode := smNormal;
  FStartLinePos := 1;
  FStartBytePos := 1;
  FEndLinePos := 1;
  FEndBytePos := 1;
  FEnabled := True;
  FHookedLines := aActOnLineChanges;
  FIsSettingText := False;
  if FHookedLines then begin
    FLines.AddEditHandler(@DoLinesEdited);
    FLines.AddChangeHandler(senrLineChange, {$IFDEF FPC}@{$ENDIF}LineChanged);
  end;
end;

destructor TSynEditSelection.Destroy;
begin
  FreeAndNil(FInternalCaret);
  if FHookedLines then begin
    FLines.RemoveEditHandler(@DoLinesEdited);
    FLines.RemoveChangeHandler(senrLineChange, {$IFDEF FPC}@{$ENDIF}LineChanged);
  end;
  inherited Destroy;
end;

procedure TSynEditSelection.AssignFrom(Src: TSynEditSelection);
begin
  //FEnabled             := src.FEnabled;
  FHide                := src.FHide;
  FActiveSelectionMode := src.FActiveSelectionMode;
  FSelectionMode       := src.FSelectionMode;
  FStartLinePos        := src.FStartLinePos; // 1 based
  FStartBytePos        := src.FStartBytePos; // 1 based
  FEndLinePos          := src.FEndLinePos; // 1 based
  FEndBytePos          := src.FEndBytePos; // 1 based
  FPersistent          := src.FPersistent;
end;

procedure TSynEditSelection.AdjustAfterTrimming;
begin
  if FStartBytePos > Length(FLines[FStartLinePos-1]) + 1 then
    FStartBytePos := Length(FLines[FStartLinePos-1]) + 1;
  if FEndBytePos > Length(FLines[FEndLinePos-1]) + 1 then
    FEndBytePos := Length(FLines[FEndLinePos-1]) + 1;
  // Todo: Call ChangeNotification
end;

procedure TSynEditSelection.DoLock;
begin
  inherited DoLock;
  FLastCarePos := Point(-1, -1);
end;

procedure TSynEditSelection.DoUnlock;
begin
  inherited DoUnlock;
  FLastCarePos := Point(-1, -1);
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


var
  First, Last, TotalLen: Integer;
  ColFrom, ColTo: Integer;
  I: Integer;
  P: PChar;
  C1, C2: Integer;
  Col, Len: array of Integer;

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
        if (First = Last) then begin
          Result := Copy(FLines[First], ColFrom, ColTo - ColFrom);
          I := (ColTo - ColFrom) - length(Result);
          if I > 0 then
            Result := Result + StringOfChar(' ', I);
        end else begin
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
          CopyPaddedAndForward(FLines[Last], 1, ColTo - 1, P);
        end;
      smColumn:
        begin
          // Calculate the byte positions for each line
          SetLength(Col, Last - First + 1);
          SetLength(Len, Last - First + 1);
          FInternalCaret.AllowPastEOL := True;
          FInternalCaret.LineBytePos := FirstLineBytePos;
          C1 := FInternalCaret.CharPos;
          FInternalCaret.LineBytePos := LastLineBytePos;
          C2 := FInternalCaret.CharPos;
          if C1 > C2 then
            SwapInt(C1, C2);

          TotalLen := 0;
          for i := First to Last do begin
            FInternalCaret.LineCharPos := Point(C1, i + 1);
            Col[i - First] := FInternalCaret.BytePos;
            FInternalCaret.LineCharPos := Point(C2, i + 1);
            Len[i - First] := Max(0, FInternalCaret.BytePos - Col[i - First]);
            Inc(TotalLen, Len[i - First]);
          end;
          Inc(TotalLen, Length(LineEnding) * (Last - First));
          // build up result string
          SetLength(Result, TotalLen);
          P := PChar(Pointer(Result));
          for i := First to Last do begin
            CopyPaddedAndForward(FLines[i], Col[i-First], Len[i-First], P);
            if i < Last then
              CopyAndForward(LineEnding, 1, MaxInt, P);
          end;
        end;
      smLine:
        begin
          // If block selection includes LastLine,
          // line break code(s) of the last line will not be added.
          // step1: calclate total length of result string
          for i := First to Last do
            Inc(TotalLen, Length(FLines[i]) + Length(LineEnding));
          if Last = FLines.Count - 1 then
            Dec(TotalLen, Length(LineEnding));
          // step2: build up result string
          SetLength(Result, TotalLen);
          P := PChar(Pointer(Result));
          for i := First to Last - 1 do begin
            CopyAndForward(FLines[i], 1, MaxInt, P);
            CopyAndForward(LineEnding, 1, MaxInt, P);
          end;
          CopyAndForward(FLines[Last], 1, MaxInt, P);
          if Last < FLines.Count - 1 then
            CopyAndForward(LineEnding, 1, MaxInt, P);
        end;
    end;
  end;
end;

procedure TSynEditSelection.SetSelText(const Value : string);
begin
  SetSelTextPrimitive(FActiveSelectionMode, PChar(Value));
end;

procedure TSynEditSelection.DoCaretChanged(Sender: TObject);
begin
  // FIgnoreNextCaretMove => caret skip selection
  if FIgnoreNextCaretMove then begin
    FIgnoreNextCaretMove := False;
    FLastCarePos := Point(-1, -1);
    exit;
  end;

  if (FCaret.IsAtLineByte(StartLineBytePos) or
      FCaret.IsAtLineByte(EndLineBytePos)) and
     FCaret.WasAtLineChar(FLastCarePos)
  then
    exit;
  FLastCarePos := Point(-1, -1);

  if FAutoExtend then begin
    if (not FHide) and (FCaret.WasAtLineByte(EndLineBytePos)) then
      SetEndLineBytePos(FCaret.LineBytePos)
    else
    if (not FHide) and (FCaret.WasAtLineByte(StartLineBytePos)) then
      AdjustStartLineBytePos(FCaret.LineBytePos)
    else begin
      StartLineBytePos := Point(FCaret.OldCharPos, FCaret.OldLinePos);
      EndLineBytePos := FCaret.LineBytePos;
      if Persistent and IsBackwardSel then
        SortSelectionPoints;
    end;
    exit;
  end;

  if FPersistent or (FPersistentLock > 0) then
    exit;

  StartLineBytePos := FCaret.LineBytePos;
end;

procedure TSynEditSelection.LineChanged(Sender: TSynEditStrings; AIndex, ACount: Integer);
begin
  if (FCaret <> nil) and (not FCaret.AllowPastEOL) and (not FIsSettingText) then
    AdjustAfterTrimming;
end;

procedure TSynEditSelection.DoLinesEdited(Sender: TSynEditStrings; aLinePos, aBytePos, aCount,
  aLineBrkCnt: Integer; aText: String);

  function AdjustPoint(aPoint: Tpoint): TPoint; inline;
  begin
    Result := aPoint;
    if aLineBrkCnt < 0 then begin
      (* Lines Deleted *)
      if aPoint.y > aLinePos then begin
        Result.y := Max(aLinePos, Result.y + aLineBrkCnt);
        if Result.y = aLinePos then
          Result.x := Result.x + aBytePos - 1;
      end;
    end
    else
    if aLineBrkCnt > 0 then begin
      (* Lines Inserted *)
      if (aPoint.y = aLinePos) and (aPoint.x >= aBytePos) then begin
        Result.x := Result.x - aBytePos + 1;
        Result.y := Result.y + aLineBrkCnt;
      end;
      if aPoint.y > aLinePos then begin
        Result.y := Result.y + aLineBrkCnt;
      end;
    end
    else
    if aCount <> 0 then begin
      (* Chars Insert/Deleted *)
      if (aPoint.y = aLinePos) and (aPoint.x >= aBytePos) then
        Result.x := Max(aBytePos, Result.x + aCount);
    end;
  end;

begin
  if FIsSettingText then exit;
  if FPersistent or (FPersistentLock > 0) or
     ((FCaret <> nil) and (not FCaret.Locked))
  then begin
    if FActiveSelectionMode <> smColumn then begin // TODO: adjust ypos, height in smColumn mode
      AdjustStartLineBytePos(AdjustPoint(StartLineBytePos));
      EndLineBytePos := AdjustPoint(EndLineBytePos);
    end;
    // Todo: Change Lines in smColumn
  end
  else begin
    // Change the Selection, if change was made by owning SynEdit (Caret.Locked)
    // (InternalSelection has no Caret)
    if (FCaret <> nil) and (FCaret.Locked) then
      StartLineBytePos := FCaret.LineBytePos;
  end;
end;

procedure TSynEditSelection.SetSelTextPrimitive(PasteMode : TSynSelectionMode;
  Value : PChar; AReplace: Boolean = False);
var
  BB, BE: TPoint;

  procedure DeleteSelection;
  var
    y, l, r, xb, xe: Integer;
    Str: string;
    Start, P: PChar;
    //LogCaretXY: TPoint;
  begin
    case ActiveSelectionMode of
      smNormal, smLine:
        begin
          if FLines.Count > 0 then begin

            if AReplace and (Value <> nil) then begin
              // AReplace = True
              while Value^ <> #0 do begin
                Start := PChar(Value);
                P := GetEOL(Start);
                Value := P;

                if Value^ = #13 then Inc(Value);
                if Value^ = #10 then Inc(Value);

                SetString(Str, Start, P - Start);

                if BE.y > BB.y then begin
                  FLines.EditDelete(BB.x, BB.Y, 1+Length(FLines[BB.y-1]) - BB.x);
                  FLines.EditInsert(BB.x, BB.Y, Str);
                  if (PasteMode = smLine) or (Value > P) then begin
                    inc(BB.y);
                    BB.x := 1;
                  end
                  else
                    BB.X := BB.X + length(Str);
                end
                else begin
                  FLines.EditDelete(BB.x, BB.Y, BE.x - BB.x);
                  // BE will be block-.nd, also used by SynEdit to set caret
                  if (ActiveSelectionMode = smLine) or (Value > P) then begin
                    FLines.EditLineBreak(BB.x, BB.Y);
                    inc(BE.y);
                    BE.x := 1;
                  end
                  else
                    BE.X := BB.X + length(Str);
                  FLines.EditInsert(BB.x, BB.Y, Str);
                  BB := BE; // end of selection
                end;

                if (BB.Y = BE.Y) and (BB.X = BE.X) then begin
                  FInternalCaret.LineBytePos := BB;
                  exit;
                end;

              end;
            end;

            // AReplace = False
            if BE.Y > BB.Y + 1 then begin
              FLines.EditLinesDelete(BB.Y + 1, BE.Y - BB.Y - 1);
              BE.Y := BB.Y + 1;
            end;
            if BE.Y > BB.Y then begin
              l := length(FLines[BB.Y - 1]);
              BE.X := BE.X + Max(l, BB.X - 1);
              FLines.EditLineJoin(BB.Y, StringOfChar(' ', Max(0, BB.X - (l+1))));
              BE.Y := BB.Y;
            end;
            if BE.X <> BB.X then
              FLines.EditDelete(BB.X, BB.Y, BE.X - BB.X);
          end;
          FInternalCaret.LineBytePos := BB;
        end;
      smColumn:
        begin
          // AReplace has no effect
          FInternalCaret.LineBytePos := BB;
          l := FInternalCaret.CharPos;
          FInternalCaret.LineBytePos := BE;
          r := FInternalCaret.CharPos;
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
            FInternalCaret.LineCharPos := Point(l, y);
            xb := FInternalCaret.BytePos;
            FInternalCaret.LineCharPos := Point(r, y);
            xe := Min(FInternalCaret.BytePos, 1 + length(FInternalCaret.LineText));
            if xe > xb then
              FLines.EditDelete(xb, y, xe - xb);
          end;
          FInternalCaret.LineCharPos := Point(l, BB.Y);
          BB := FInternalCaret.LineBytePos;
          // Column deletion never removes a line entirely,
          // so no (vertical) mark updating is needed here.
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
      LogCaretXY := FInternalCaret.LineBytePos;

      Start := PChar(Value);
      P := GetEOL(Start);
      if P^ = #0 then begin
        FLines.EditInsert(LogCaretXY.X, LogCaretXY.Y, Value);
        FInternalCaret.BytePos := FInternalCaret.BytePos + Length(Value);
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
        FInternalCaret.LinePos := LogCaretXY.Y;
        FInternalCaret.BytePos := 1 + Length(Str);
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
      FInternalCaret.IncForcePastEOL;
      Start := PChar(Value);
      repeat
        P := GetEOL(Start);
        if P <> Start then begin
          SetLength(Str, P - Start);
          Move(Start^, Str[1], P - Start);
          FLines.EditInsert(FInternalCaret.BytePos, FInternalCaret.LinePos, Str);
        end;
        if p^ in [#10,#13] then begin
          if (p[1] in [#10,#13]) and (p[1]<>p^) then
            inc(p,2)
          else
            Inc(P);
          if FInternalCaret.LinePos = FLines.Count then
            FLines.EditLinesInsert(FInternalCaret.LinePos + 1, 1);
            // No need to inc result => adding at EOF
          FInternalCaret.LinePos := FInternalCaret.LinePos + 1;
        end;
        Start := P;
      until P^ = #0;
      FInternalCaret.BytePos:= FInternalCaret.BytePos + Length(Str);
      FInternalCaret.DecForcePastEOL;
    end;

    function InsertLine: Integer;
    var
      Start: PChar;
      P: PChar;
      Str: string;
    begin
      Result := 0;
      FInternalCaret.CharPos := 1;
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
          FLines.EditInsert(1, FInternalCaret.LinePos, Str);
          FInternalCaret.BytePos := 1 + Length(Str);
        end else begin
          FLines.EditLinesInsert(FInternalCaret.LinePos, 1, Str);
          FInternalCaret.LinePos := FInternalCaret.LinePos + 1;
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
  FIsSettingText := True;
  FLines.BeginUpdate; // Todo: can we get here, without paintlock?
  try
    // BB is lower than BE
    BB := FirstLineBytePos;
    BE := LastLineBytePos;
    if SelAvail then begin
      if FActiveSelectionMode = smLine then begin
        BB.X := 1;
        if BE.Y = FLines.Count then begin
          // Keep the (CrLf of) last line, since no Line exists to replace it
          BE.x := 1 + length(FLines[BE.Y - 1]);
        end else begin
          inc(BE.Y);
          BE.x := 1;
        end;
      end;
      DeleteSelection;
      StartLineBytePos := BB; // deletes selection // calls selection changed
      // Need to update caret (syncro edit follows on every edit)
      if FCaret <> nil then
        FCaret.LineCharPos := FInternalCaret.LineCharPos; // must equal BB
    end
    else
    if FCaret <> nil then
      StartLineBytePos := FCaret.LineBytePos;

    FInternalCaret.LineBytePos := StartLineBytePos;
    if (Value <> nil) and (Value[0] <> #0) then begin
      InsertText;
      StartLineBytePos := FInternalCaret.LineBytePos; // reset selection
    end;
    if FCaret <> nil then
      FCaret.LineCharPos := FInternalCaret.LineCharPos;
  finally
    FLines.EndUpdate;
    FIsSettingText := False;
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
  if not Enabled then SetStartLineBytePos(StartLineBytePos);
end;

procedure TSynEditSelection.SetStartLineBytePos(Value : TPoint);
// logical position (byte)
var
  nInval1, nInval2: integer;
  WasAvail: boolean;
begin
  WasAvail := SelAvail;
  Value.y := MinMax(Value.y, 1, fLines.Count);
  if (FCaret = nil) or FCaret.AllowPastEOL then
    Value.x := Max(Value.x, 1)
  else
    Value.x := MinMax(Value.x, 1, length(Lines[Value.y - 1])+1);
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
  end;
  FActiveSelectionMode := FSelectionMode;
  FHide := False;
  FStartLinePos := Value.Y;
  FStartBytePos := Value.X;
  FEndLinePos := Value.Y;
  FEndBytePos := Value.X;
  if FCaret <> nil then
    FLastCarePos := Point(FCaret.OldCharPos, FCaret.OldLinePos);
  if WasAvail then
    fOnChangeList.CallNotifyEvents(self);
end;

procedure TSynEditSelection.AdjustStartLineBytePos(Value: TPoint);
begin
  if FEnabled then begin
    Value.y := MinMax(Value.y, 1, fLines.Count);
    if (FCaret = nil) or FCaret.AllowPastEOL then
      Value.x := Max(Value.x, 1)
    else
      Value.x := MinMax(Value.x, 1, length(Lines[Value.y - 1])+1);
    if (ActiveSelectionMode = smNormal) then
      if (Value.y >= 1) and (Value.y <= FLines.Count) then
        Value.x := AdjustBytePosToCharacterStart(Value.y,Value.x)
      else
        Value.x := 1;

    if (Value.X <> FStartBytePos) or (Value.Y <> FStartLinePos) then begin
      if (ActiveSelectionMode = smColumn) and (Value.X <> FStartBytePos) then
        FInvalidateLinesMethod(Min(FStartLinePos, Min(FEndLinePos, Value.Y)),
                               Max(FStartLinePos, Max(FEndLinePos, Value.Y)))
      else
      if (ActiveSelectionMode <> smColumn) or (FStartBytePos <> FEndBytePos) then
        FInvalidateLinesMethod(FStartLinePos, Value.Y);
      FStartLinePos := Value.Y;
      FStartBytePos := Value.X;
      if FCaret <> nil then
        FLastCarePos := Point(FCaret.OldCharPos, FCaret.OldLinePos);
      FOnChangeList.CallNotifyEvents(self);
    end;
  end;
end;

function TSynEditSelection.GetEndLineBytePos : TPoint;
begin
  Result.y := FEndLinePos;
  Result.x := FEndBytePos;
end;

procedure TSynEditSelection.SetEndLineBytePos(Value : TPoint);
{$IFDEF SYN_MBCSSUPPORT}
var
  s: string;
{$ENDIF}
var
  WasAvail: Boolean;
begin
  if FEnabled then begin
    Value.y := MinMax(Value.y, 1, fLines.Count);
    if (FCaret = nil) or FCaret.AllowPastEOL then
      Value.x := Max(Value.x, 1)
    else
      Value.x := MinMax(Value.x, 1, length(Lines[Value.y - 1])+1);
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
        WasAvail := SelAvail;
        if (ActiveSelectionMode = smColumn) and (Value.X <> FEndBytePos) then
          FInvalidateLinesMethod(Min(FStartLinePos, Min(FEndLinePos, Value.Y)),
                                 Max(FStartLinePos, Max(FEndLinePos, Value.Y)))
        else
        if (ActiveSelectionMode <> smColumn) or (FStartBytePos <> FEndBytePos) then
          FInvalidateLinesMethod(FEndLinePos, Value.Y);
        FEndLinePos := Value.Y;
        FEndBytePos := Value.X;
        if FCaret <> nil then
          FLastCarePos := Point(FCaret.OldCharPos, FCaret.OldLinePos);
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

procedure TSynEditSelection.SetHide(const AValue: Boolean);
begin
  if FHide = AValue then exit;
  FHide := AValue;
  FInvalidateLinesMethod(Min(FStartLinePos, FEndLinePos),
                         Max(FStartLinePos, FEndLinePos) );
  FOnChangeList.CallNotifyEvents(self);
end;

procedure TSynEditSelection.SetPersistent(const AValue: Boolean);
begin
  if FPersistent = AValue then exit;
  FPersistent := AValue;
  if (not FPersistent) and (FCaret <> nil) and
     not ( FCaret.IsAtLineByte(StartLineBytePos) or
           FCaret.IsAtLineByte(EndLineBytePos) )
  then
    Clear;
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

procedure TSynEditSelection.SetCaret(const AValue: TSynEditCaret);
begin
  if FCaret = AValue then exit;
  if FCaret <> nil then
    Caret.RemoveChangeHandler(@DoCaretChanged);
  FCaret := AValue;
  if FCaret <> nil then
    Caret.AddChangeHandler(@DoCaretChanged);
end;

function TSynEditSelection.SelAvail : Boolean;
begin
  if FHide then exit(False);
  if (FActiveSelectionMode = smColumn) then begin
    Result := (FStartBytePos <> FEndBytePos) and (FStartLinePos = FEndLinePos);
    if (not Result) and (FStartLinePos <> FEndLinePos) then begin
      // Todo: Cache values, but we need notification, if ines are modified (even only by change of tabwidth...)
      Result := Lines.LogicalToPhysicalPos(StartLineBytePos).X <>
                Lines.LogicalToPhysicalPos(EndLineBytePos).X;
    end;
  end
  else
    Result := (FStartBytePos <> FEndBytePos) or (FStartLinePos <> FEndLinePos);
end;

function TSynEditSelection.SelCanContinue(ACaret: TSynEditCaret): Boolean;
begin
  if SelAvail then exit(True);
  Result := (not FHide) and
            (FActiveSelectionMode = smColumn) and (FEndLinePos = ACaret.LinePos) and
            (FEndBytePos = ACaret.BytePos);
end;

function TSynEditSelection.IsBackwardSel: Boolean;
begin
  Result := (FStartLinePos > FEndLinePos)
    or ((FStartLinePos = FEndLinePos) and (FStartBytePos > FEndBytePos));
end;

procedure TSynEditSelection.SortSelectionPoints;
begin
  if IsBackwardSel then begin
    SwapInt(FStartLinePos, FEndLinePos);
    SwapInt(FStartBytePos, FEndBytePos);
  end;
end;

procedure TSynEditSelection.IgnoreNextCaretMove;
begin
  FIgnoreNextCaretMove := True;
end;

procedure TSynEditSelection.IncPersistentLock;
begin
  inc(FPersistentLock);
end;

procedure TSynEditSelection.DecPersistentLock;
begin
  dec(FPersistentLock);
  if (FPersistentLock = 0) and (FCaret <> nil) and FCaret.Locked then
    FLastCarePos := Point(FCaret.OldCharPos, FCaret.OldLinePos);
end;

procedure TSynEditSelection.Clear;
begin
  if Caret <> nil then
    StartLineBytePos := Caret.LineBytePos
  else
    StartLineBytePos := StartLineBytePos;
end;

{ TSynEditScreenCaret }

constructor TSynEditScreenCaret.Create(AHandleOwner: TWinControl);
begin
  inherited Create;
  FHandleOwner := AHandleOwner;
  FVisible := False;
  FCurrentVisible := False;
  FCurrentCreated := False;
  FCurrentPosX := -1;
  FCurrentPosY := -1;
  FCurrentClippedWidth := -1;
  FClipExtraPixel := 0;
  FLockCount := 0;
end;

destructor TSynEditScreenCaret.Destroy;
begin
  DestroyCaret;
  inherited Destroy;
end;

procedure TSynEditScreenCaret.Hide;
begin
  HideCaret;
end;

procedure TSynEditScreenCaret.DestroyCaret(SkipHide: boolean = False);
begin
  if FCurrentCreated and HandleAllocated then begin
    {$IFDeF SynCaretDebug}
    debugln(['SynEditCaret DestroyCaret for HandleOwner=',FHandleOwner, ' DebugShowCount=', FDebugShowCount]);
    {$ENDIF}
    LCLIntf.DestroyCaret(Handle);
  end;
  FCurrentCreated := False;
  FCurrentVisible := False;
  if not SkipHide then
    FVisible := False;
end;

procedure TSynEditScreenCaret.Lock;
begin
  inc(FLockCount);
end;

procedure TSynEditScreenCaret.UnLock;
begin
  dec(FLockCount);
  if (FLockCount=0) and FLockedUpdateNeeded then
    UpdateDisplay;
end;

procedure TSynEditScreenCaret.SetClipRight(const AValue: Integer);
begin
  if FClipRight = AValue then exit;
  FClipRight := AValue;
  UpdateDisplay;
end;

procedure TSynEditScreenCaret.SetCharHeight(const AValue: Integer);
begin
  if FCharHeight = AValue then exit;
  FCharHeight := AValue;
  UpdateDisplayType;
end;

function TSynEditScreenCaret.GetHandle: HWND;
begin
  Result :=FHandleOwner.Handle;
end;

function TSynEditScreenCaret.GetHandleAllocated: Boolean;
begin
  Result :=FHandleOwner.HandleAllocated;
end;

procedure TSynEditScreenCaret.SetCharWidth(const AValue: Integer);
begin
  if FCharWidth = AValue then exit;
  FCharWidth := AValue;
  UpdateDisplayType;
end;

procedure TSynEditScreenCaret.SetDisplayPos(const AValue: TPoint);
begin
  if (FDisplayPos.x = AValue.x) and (FDisplayPos.y = AValue.y) and
     (FVisible = FCurrentVisible)
  then
    exit;
  FDisplayPos := AValue;
  UpdateDisplay;
end;

procedure TSynEditScreenCaret.SetDisplayType(const AType: TSynCaretType);
begin
  if FDisplayType = AType then exit;
  FDisplayType := AType;
  UpdateDisplayType;
end;

procedure TSynEditScreenCaret.SetVisible(const AValue: Boolean);
begin
  if FVisible = AValue then exit;
  FVisible := AValue;
  UpdateDisplay;
end;

procedure TSynEditScreenCaret.UpdateDisplayType;
begin
  case FDisplayType of
    ctVerticalLine:
      begin
        FPixelWidth     := 2;
        FPixelHeight    := FCharHeight - 2;
        FOffsetX        := -1;
        FOffsetY        :=  1;
        FExtraLinePixel :=  1;
      end;
    ctBlock:
      begin
        FPixelWidth     := FCharWidth;
        FPixelHeight    := FCharHeight - 2;
        FOffsetX        := 0;
        FOffsetY        := 1;
        FExtraLinePixel := FCharWidth;
      end;
    ctHalfBlock:
      begin
        FPixelWidth     := FCharWidth;
        FPixelHeight    := (FCharHeight - 2) div 2;
        FOffsetX        := 0;
        FOffsetY        := FPixelHeight + 1;
        FExtraLinePixel := FCharWidth;
      end;
    ctHorizontalLine:
      begin
        FPixelWidth     := FCharWidth;
        FPixelHeight    := 2;
        FOffsetX        := 0;
        FOffsetY        := FCharHeight - 1;
        FExtraLinePixel := FCharWidth;
      end;
  end;
  CalcExtraLineChars;
  DestroyCaret(True);
  UpdateDisplay;
end;

procedure TSynEditScreenCaret.SetClipBottom(const AValue: Integer);
begin
  if FClipBottom = AValue then exit;
  FClipBottom := AValue;
  UpdateDisplay;
end;

procedure TSynEditScreenCaret.SetClipExtraPixel(AValue: Integer);
begin
  if FClipExtraPixel = AValue then Exit;
  {$IFDeF SynCaretDebug}
  debugln(['SynEditCaret ClipRect for HandleOwner=',FHandleOwner, ' ExtraPixel=', dbgs(AValue)]);
  {$ENDIF}
  FClipExtraPixel := AValue;
  CalcExtraLineChars;
  UpdateDisplay;
end;

procedure TSynEditScreenCaret.SetClipLeft(const AValue: Integer);
begin
  if FClipLeft = AValue then exit;
  FClipLeft := AValue;
  UpdateDisplay;
end;

procedure TSynEditScreenCaret.SetClipRect(const AValue: TRect);
begin
  if (FClipLeft = AValue.Left) and (FClipRight = AValue.Right) and
     (FClipTop = AValue.Top) and (FClipBottom = AValue.Bottom)
  then
    exit;
  {$IFDeF SynCaretDebug}
  debugln(['SynEditCaret ClipRect for HandleOwner=',FHandleOwner, ' Rect=', dbgs(AValue)]);
  {$ENDIF}
  FClipLeft   := AValue.Left;
  FClipRight  := AValue.Right;
  FClipTop    := AValue.Top;
  FClipBottom := AValue.Bottom;
  UpdateDisplay;
end;

procedure TSynEditScreenCaret.SetClipTop(const AValue: Integer);
begin
  if FClipTop = AValue then exit;
  FClipTop := AValue;
  UpdateDisplay;
end;

procedure TSynEditScreenCaret.CalcExtraLineChars;
var
  OldExtraChars: Integer;
begin
  OldExtraChars := FExtraLineChars;
  FExtraLineChars := Max(0, FExtraLinePixel - FClipExtraPixel + FCharWidth - 1)
                     div FCharWidth;
  if (FExtraLineChars <> OldExtraChars) and assigned(FOnExtraLineCharsChanged) then
    FOnExtraLineCharsChanged(Self);
end;

procedure TSynEditScreenCaret.UpdateDisplay;
begin
  if FLockCount > 0 then begin
    FLockedUpdateNeeded := True;
    exit;
  end;
  FLockedUpdateNeeded := False;
  if FVisible then
    ShowCaret
  else
    HideCaret;
end;

procedure TSynEditScreenCaret.ShowCaret;
var
  x, y, w: Integer;
begin
  if not HandleAllocated then
    exit;
  x := FDisplayPos.x + FOffsetX;
  y := FDisplayPos.y + FOffsetY;
  w := FPixelWidth;
  if x + w >= FClipRight then
    w := FClipRight - x - 1;
  if (w <= 0) or
     (x < FClipLeft) or (x >= FClipRight) or
     (y < FClipTop) or (y >= FClipBottom)
  then begin
    HideCaret;
    exit;
  end;

  if (not FCurrentCreated) or (FCurrentClippedWidth <> w) then begin
    {$IFDeF SynCaretDebug}
    debugln(['SynEditCaret CreateCaret for HandleOwner=',FHandleOwner, ' DebugShowCount=', FDebugShowCount, ' Width=', w, ' pref-width=', FPixelWidth, ' Height=', FPixelHeight, '  FCurrentCreated=',FCurrentCreated,  ' FCurrentVisible=',FCurrentVisible]);
    FDebugShowCount := 0;
    {$ENDIF}
    //if FCurrentCreated  then
    //  LCLIntf.DestroyCaret(Handle);
    // // Create caret includes destroy
    CreateCaret(Handle, 0, w, FPixelHeight);
    FCurrentCreated := True;
    FCurrentVisible := False;
    FCurrentClippedWidth := w;
    FCurrentPosX := x - 1;
    SetCaretRespondToFocus(Handle, False); // Only for GTK
  end;
  if (x <> FCurrentPosX) or (y <> FCurrentPosY) then begin
    {$IFDeF SynCaretDebug}
    debugln(['SynEditCaret SetPos for HandleOwner=',FHandleOwner, ' x=', x, ' y=',y]);
    {$ENDIF}
    SetCaretPosEx(Handle, x, y);
    FCurrentPosX := x;
    FCurrentPosY := y;
  end;
  if (not FCurrentVisible) then begin
    {$IFDeF SynCaretDebug}
    debugln(['SynEditCaret ShowCaret for HandleOwner=',FHandleOwner, ' FDebugShowCount=',FDebugShowCount]);
    inc(FDebugShowCount);
    {$ENDIF}
    if LCLIntf.ShowCaret(Handle) then
      FCurrentVisible := True;
  end;
end;

procedure TSynEditScreenCaret.HideCaret;
begin
  if not HandleAllocated then
    exit;
  if not FCurrentCreated then exit;
  if FCurrentVisible then begin
    {$IFDeF SynCaretDebug}
    debugln(['SynEditCaret HideCaret for HandleOwner=',FHandleOwner, ' FDebugShowCount=',FDebugShowCount]);
    dec(FDebugShowCount);
    {$ENDIF}
    if LCLIntf.HideCaret(Handle) then
      FCurrentVisible := False;
  end;
end;

end.

