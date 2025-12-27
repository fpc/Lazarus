{
 *****************************************************************************
  This file is part of the LazEdit package from the Lazarus IDE.

  This content of this file is licensed: Modified LGPL-2
  Or at the users choice: Modified LGPL-3
  See the file COPYING.modifiedLGPL.txt, included in the Lazarus distribution,
  for details about the license.

  Alternatively, the contents of this file may be used under the terms of the
  Mozilla Public License Version 1.1 or 2.0 http://www.mozilla.org/MPL/

  A copy used under either License can have the other Licenses removed from this
  header. A note should be added that the original file is available with the
  above choice of License.
  Used units may have to be amended according to the choice.
 *****************************************************************************
}
unit LazEditHighlighter;

{$mode objfpc}{$H+}

interface

uses
  Classes, fgl,
  // LazEdit
  LazEditTextAttributes, LazEditLineItemLists, LazEditHighlighterUtils, LazClasses, LazEditTypes,
  LazEditMiscProcs;

type

  TLazEditHighlighterAttributes = class(TLazEditTextAttribute)
  published
    property Foreground;
    property Background;
    property FrameColor;

    property ForePriority;
    property BackPriority;
    property FramePriority;

    property FrameStyle;
    property FrameEdges;

    property Style;
    property BoldPriority;
    property ItalicPriority;
    property UnderlinePriority;
    property StrikeOutPriority;

    property OnChange;
  end;
  TLazEditHighlighterAttributesClass = class of TLazEditHighlighterAttributes;

  TLazEditHighlighterAttributesModifier = class(TLazEditTextAttributeModifier)
  published
    property Foreground;
    property Background;
    property FrameColor;

    property ForePriority;
    property BackPriority;
    property FramePriority;

    property FrameStyle;
    property FrameEdges;

    property Style;
    property BoldPriority;
    property ItalicPriority;
    property UnderlinePriority;
    property StrikeOutPriority;

    property OnChange;
  published
    property BackAlpha;
    property ForeAlpha;
    property FrameAlpha;

    property StyleMask;
  end;
  TLazEditHighlighterAttributesModifierClass = class of TLazEditHighlighterAttributesModifier;

  TLazEditHighlighterAttributes_Eol = class(TLazEditHighlighterAttributes)
  published
    property ExtendPastEol;
  end;

  TLazEditHighlighterAttributesModifier_Eol = class(TLazEditHighlighterAttributesModifier)
  published
    property ExtendPastEol;
  end;

  { TLazEditStringsBase }

  TLazEditStringsBase = class(specialize TFreeNotifyingGeneric<TStrings>)
  protected
    function GetRange(Index: Pointer): TLazEditLineItems; virtual; abstract;
    procedure PutRange(Index: Pointer; const ARange: TLazEditLineItems); virtual; abstract;
  public
    destructor Destroy; override;
    procedure SendHighlightChanged(aIndex, aCount: Integer); virtual; abstract;
    procedure SendHighlightAttributeChanged; virtual; abstract;
    procedure SendHighlightRescanNeeded; virtual; abstract;
    function  GetPChar(ALineIndex: Integer): PChar;                                       // experimental
    function  GetPChar(ALineIndex: Integer; out ALen: Integer): PChar; virtual; abstract; // experimental
    property Ranges[Index: Pointer]: TLazEditLineItems read GetRange write PutRange;
  end;

  TLazEditHighlighterAttachedLines = specialize TFPGList<TLazEditStringsBase>;

  { TLazEditCustomHighlighter }

  TLazEditCustomHighlighter = class(TLazEditAttributeOwner)
  private type
    TLazEditHlUpdateFlag = (ufAttribChanged, ufRescanNeeded);
    TLazEditHlUpdateFlags = set of TLazEditHlUpdateFlag;
  strict private
    FUpdateLock: integer;
    FUpdateFlags: TLazEditHlUpdateFlags;

    FAttachedLines: TLazEditHighlighterAttachedLines;
    FCurrentLines: TLazEditStringsBase;

    FLineIndex: TLineIdx;
    FLineText: String;
    FLinePtr: Pchar;
    F_IsInNextToEOL: Boolean;

    FTokenAttributeMergeResult: TLazEditTextAttributeMergeResult;
    FTokenAttributeList: TLazCustomEditTextAttributeArray;

    function GetIsUpdating: boolean; inline;
    procedure InternalEndUpdate;

  private
    procedure SetCurrentLines(AValue: TLazEditStringsBase); virtual;
    procedure DoAttachedLinesFreed(Sender: TObject);
    procedure SendRescanNeededNotification; virtual;

    procedure DoStartAtLine; virtual;
  protected
    procedure DoBeginUpdate; virtual;
    procedure DoEndUpdate; virtual;

  protected
    (* ------------------ *
     * Lines / RangesList *
     * ------------------ *)
    procedure DoAttachedToLines(Lines: TLazEditStringsBase); virtual;
    procedure DoDetachingFromLines(Lines: TLazEditStringsBase); virtual;
    procedure DoCurrentLinesChanged; virtual;
    procedure RequestFullRescan;

    property AttachedLines: TLazEditHighlighterAttachedLines read FAttachedLines;

    (* ------------------ *
     * Current Line       *
     * ------------------ *)
    procedure InitForScaningLine; virtual;
    property  CurrentLineText: string read FLineText;
    property  LinePtr: Pchar read FLinePtr;

    property FIsInNextToEOL: Boolean read F_IsInNextToEOL;  deprecated 'use IsInNextToEOL / to be removed in 5.99';
    property IsInNextToEOL: Boolean read F_IsInNextToEOL write F_IsInNextToEOL;
  protected
    (* ------------------ *
     * Token / Attributes *
     * ------------------ *)
    procedure SendAttributeChangeNotification;
    procedure MergeModifierToTokenAttribute(
      var ACurrentResultAttrib: TLazCustomEditTextAttribute;
      AModifierAttrib: TLazCustomEditTextAttribute;
      AModifierLeftCol: integer = -2;
      AModifierRightCol: integer = -2;
      ASkipResultBounds: boolean = False
    );
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure BeginUpdate; inline;
    procedure EndUpdate; inline;
    property IsUpdating: boolean read GetIsUpdating;

    (* ------------------ *
     * Lines / RangesList *
     * ------------------ *)

    procedure AttachToLines(ALines: TLazEditStringsBase); virtual;
    procedure DetachFromLines(ALines: TLazEditStringsBase); virtual;
    property CurrentLines: TLazEditStringsBase read FCurrentLines write SetCurrentLines;

  public
    (* ------------------ *
     * Current Line       *
     * ------------------ *)
    procedure StartAtLineIndex(ALineIdx: TLineIdx);  // 0 based
    procedure ContinueNextLine;  // To be called at EOL; does not read the range
    procedure SetAlternativeLineTextForGetTokens(const AnInjectedText: String; ALineIdx: TLineIdx); experimental;
    property  LineIndex: TLineIdx read FLineIndex;

    procedure Next; virtual; abstract;
    procedure NextToEol;
    function NextToLogX(ALogX: IntPos; ARestartLineIfNeeded: Boolean = False): boolean;
    function GetEol: Boolean; virtual; abstract;

    (* ------------------ *
     * Token / Attributes *
     * ------------------ *)

    function GetTokenKind: integer; virtual; abstract;
    function GetToken: String; virtual; abstract;
    function GetTokenPos: Integer; virtual; abstract; // 0-based
    function GetTokenLen: Integer; virtual; abstract;
    procedure GetTokenEx(out TokenStart: PChar; out TokenLength: integer); virtual; abstract;
    (* GetTokenAttribute / GetEndOfLineAttribute
       The base attribute
     * GetTokenAttributeEx / GetEndOfLineAttributeEx
       The final attribute with merged modifiers (if HL has modifiers)
    *)
    function GetTokenAttribute: TLazEditTextAttribute; virtual; abstract;
    function GetTokenAttributeEx: TLazCustomEditTextAttribute; virtual;
    function GetTokenAttributeList: TLazCustomEditTextAttributeArray;
    function GetEndOfLineAttribute: TLazEditTextAttribute; virtual; // valid after line was scanned to EOL
    function GetEndOfLineAttributeEx: TLazCustomEditTextAttribute; virtual; // valid after line was scanned to EOL
  end;

  { TLazEditCustomRangesHighlighter }

  TLazEditCustomRangesHighlighter = class(TLazEditCustomHighlighter)
  strict private
    FCurrentRanges: TLazHighlighterLineRangeList;
    FRangesChangeStamp: QWord;

  private
    procedure SetCurrentLines(AValue: TLazEditStringsBase); override;
    procedure SendRescanNeededNotification; override;

    (* ------------------ *
     * Current Line       *
     * ------------------ *)
    procedure DoStartAtLine; override;

  protected
    (* ------------------ *
     * RangesList         *
     * ------------------ *)
    function GetRangeIdentifier: Pointer; virtual;
    function CreateRangeList(ALines: TLazEditStringsBase): TLazHighlighterLineRangeList; virtual;
    procedure DoAttachedToLines(Lines: TLazEditStringsBase; ARangeList: TLazHighlighterLineRangeList); reintroduce; virtual;
    procedure DoDetachingFromLines(Lines: TLazEditStringsBase; ARangeList: TLazHighlighterLineRangeList); reintroduce; virtual;

    property CurrentRanges: TLazHighlighterLineRangeList read FCurrentRanges;

  public
    (* ------------------ *
     * Scan               *
     * ------------------ *)
    // NOTE: should be protected, but historically...
    function  GetRange: Pointer; virtual; abstract;
    procedure SetRange(Value: Pointer); virtual; abstract;
    procedure ResetRange; virtual; abstract;

    (* ------------------ *
     * Lines / RangesList *
     * ------------------ *)
    procedure AttachToLines(ALines: TLazEditStringsBase); override; final;
    procedure DetachFromLines(ALines: TLazEditStringsBase); override; final;
  end;



implementation

{ TLazEditStringsBase }

destructor TLazEditStringsBase.Destroy;
begin
  inherited Destroy;
  DoDestroy;
end;

function TLazEditStringsBase.GetPChar(ALineIndex: Integer): PChar;
var
  l: Integer;
begin
  Result := GetPChar(ALineIndex, l);
end;

{ TLazEditCustomHighlighter }

procedure TLazEditCustomHighlighter.DoAttachedLinesFreed(Sender: TObject);
begin
  if Sender = FCurrentLines then
    CurrentLines := nil;
  FAttachedLines.Remove(TLazEditStringsBase(Sender));
end;

procedure TLazEditCustomHighlighter.SendRescanNeededNotification;
var
  i: Integer;
begin
  for i := 0 to AttachedLines.Count - 1 do
    AttachedLines[i].SendHighlightRescanNeeded;
end;

procedure TLazEditCustomHighlighter.DoStartAtLine;
begin
  InitForScaningLine;
end;

procedure TLazEditCustomHighlighter.DoBeginUpdate;
begin
  //
end;

procedure TLazEditCustomHighlighter.DoEndUpdate;
begin
  //
end;

procedure TLazEditCustomHighlighter.SetCurrentLines(AValue: TLazEditStringsBase);
begin
  if AValue = FCurrentLines then
    exit;

  FCurrentLines := AValue;
  FLineIndex := -1;
  FLineText  := '';
  FLinePtr   := nil;
  DoCurrentLinesChanged;
end;

function TLazEditCustomHighlighter.GetIsUpdating: boolean;
begin
  Result := FUpdateLock > 0;
end;

procedure TLazEditCustomHighlighter.InternalEndUpdate;
begin
  DoEndUpdate;
  if ufAttribChanged in FUpdateFlags then SendAttributeChangeNotification;
  if ufRescanNeeded in FUpdateFlags then RequestFullRescan;
  FUpdateFlags := [];
end;

procedure TLazEditCustomHighlighter.DoAttachedToLines(Lines: TLazEditStringsBase);
begin
  //
end;

procedure TLazEditCustomHighlighter.DoDetachingFromLines(Lines: TLazEditStringsBase);
begin
  //
end;

procedure TLazEditCustomHighlighter.DoCurrentLinesChanged;
begin
  //
end;

procedure TLazEditCustomHighlighter.RequestFullRescan;
begin
  if IsUpdating then begin
    Include(FUpdateFlags, ufRescanNeeded);
    exit;
  end;
  Exclude(FUpdateFlags, ufRescanNeeded);

  SendRescanNeededNotification;
end;

procedure TLazEditCustomHighlighter.InitForScaningLine;
begin
  //
end;

procedure TLazEditCustomHighlighter.SendAttributeChangeNotification;
var
  i: Integer;
begin
  if IsUpdating then begin
    Include(FUpdateFlags, ufAttribChanged);
    exit;
  end;
  Exclude(FUpdateFlags, ufAttribChanged);

  for i := 0 to AttachedLines.Count - 1 do
    AttachedLines[i].SendHighlightAttributeChanged;
end;

procedure TLazEditCustomHighlighter.MergeModifierToTokenAttribute(
  var ACurrentResultAttrib: TLazCustomEditTextAttribute;
  AModifierAttrib: TLazCustomEditTextAttribute; AModifierLeftCol: integer;
  AModifierRightCol: integer; ASkipResultBounds: boolean);
var
  tp: Integer;
  b: TLazEditDisplayTokenBound;
begin
  if FTokenAttributeMergeResult = nil then
    FTokenAttributeMergeResult := TLazEditTextAttributeMergeResult.Create;

  if ACurrentResultAttrib <> FTokenAttributeMergeResult then begin
    if FTokenAttributeList <> nil then
      FTokenAttributeList[0] := ACurrentResultAttrib;
    // first call, replace the callers result
    FTokenAttributeMergeResult.Assign(ACurrentResultAttrib);
    if not ASkipResultBounds then begin
      tp := GetTokenPos+1;
      FTokenAttributeMergeResult.SetFrameBoundsLog(tp, tp+GetTokenLen);
    end;
    ACurrentResultAttrib := FTokenAttributeMergeResult;
  end;

  if FTokenAttributeList <> nil then begin
    tp := Length(FTokenAttributeList);
    SetLength(FTokenAttributeList, tp+1);
    FTokenAttributeList[tp] := AModifierAttrib;
  end;

  if AModifierLeftCol <> -2 then begin
    b.Init(-1, AModifierLeftCol);
    AModifierAttrib.StartX := b;
  end;
  if AModifierRightCol <> -2 then begin
    b.Init(-1, AModifierRightCol);
    AModifierAttrib.EndX := b;
  end;

  FTokenAttributeMergeResult.Merge(AModifierAttrib);
end;

constructor TLazEditCustomHighlighter.Create(AOwner: TComponent);
begin
  FAttachedLines := TLazEditHighlighterAttachedLines.Create;
  inherited Create(AOwner);
end;

destructor TLazEditCustomHighlighter.Destroy;
begin
  inherited Destroy;
  FTokenAttributeMergeResult.Free;
  FAttachedLines.Free;
end;

procedure TLazEditCustomHighlighter.BeginUpdate;
begin
  if FUpdateLock = 0 then
    DoBeginUpdate;
  inc(FUpdateLock);
end;

procedure TLazEditCustomHighlighter.EndUpdate;
begin
  if FUpdateLock = 0 then begin
    assert(False, 'TLazEditCustomHighlighter.EndUpdate: UpdateLock > 0');
    exit;
  end;

  dec(FUpdateLock);
  if FUpdateLock = 0 then
    InternalEndUpdate;
end;

procedure TLazEditCustomHighlighter.AttachToLines(ALines: TLazEditStringsBase);
begin
  FAttachedLines.Add(ALines);
  ALines.AddFreeNotification(@DoAttachedLinesFreed);
  DoAttachedToLines(ALines);
  FCurrentLines := nil;
end;

procedure TLazEditCustomHighlighter.DetachFromLines(ALines: TLazEditStringsBase);
begin
  DoDetachingFromLines(ALines);
  FAttachedLines.Remove(ALines);
  if FAttachedLines.IndexOf(ALines) < 0 then
    ALines.RemoveFreeNotification(@DoAttachedLinesFreed);
end;

procedure TLazEditCustomHighlighter.StartAtLineIndex(ALineIdx: TLineIdx);
begin
  FLineIndex := ALineIdx;
  FLineText := CurrentLines[ALineIdx];
  FLinePtr := PChar(FLineText);
  F_IsInNextToEOL := False;

  DoStartAtLine;
  InitForScaningLine;
end;

procedure TLazEditCustomHighlighter.ContinueNextLine;
begin
  inc(FLineIndex);
  FLineText := CurrentLines[FLineIndex];
  FLinePtr := PChar(FLineText);
  F_IsInNextToEOL := False;

  InitForScaningLine;
end;

procedure TLazEditCustomHighlighter.SetAlternativeLineTextForGetTokens(const AnInjectedText: String;
  ALineIdx: TLineIdx);
begin
  FLineIndex := ALineIdx;
  FLineText := AnInjectedText;
  FLinePtr := PChar(FLineText);
  F_IsInNextToEOL := False;

  DoStartAtLine;
  InitForScaningLine;
end;

procedure TLazEditCustomHighlighter.NextToEol;
begin
  if not GetEol then begin
    F_IsInNextToEOL := True;
    repeat
      Next;
    until GetEol;
    F_IsInNextToEOL := False;
  end;
end;

function TLazEditCustomHighlighter.NextToLogX(ALogX: IntPos; ARestartLineIfNeeded: Boolean
  ): boolean;
var
  Start: Integer;
begin
  Result := False;
  ALogX := ToIdx(ALogX);
  if ARestartLineIfNeeded then begin
    if GetEol or (GetTokenPos > ALogX) then
      StartAtLineIndex(FLineIndex); // TODO: avaid re-fetching CurrentLineText
  end;

  while not GetEol do begin
    Start := GetTokenPos;
    if Start > ALogX then
      exit;
    if ALogX < Start + GetTokenLen then
      exit(True);
    Next;
  end;
end;

function TLazEditCustomHighlighter.GetTokenAttributeEx: TLazCustomEditTextAttribute;
var
  tp: Integer;
begin
  Result := GetTokenAttribute;
  if Result <> nil then begin
    // Highlighter that need different bounds must implement their own GetTokenAttributeEx;
    tp := GetTokenPos+1;
    Result.SetFrameBoundsLog(tp, tp+GetTokenLen);
  end;
end;

function TLazEditCustomHighlighter.GetTokenAttributeList: TLazCustomEditTextAttributeArray;
var
  r: TLazCustomEditTextAttribute;
begin
  SetLength(FTokenAttributeList,1);
  FTokenAttributeList[0] := nil;
  r := GetTokenAttributeEx;
  if FTokenAttributeList[0] = nil then begin
    if r = nil then
      SetLength(FTokenAttributeList, 0)
    else
      FTokenAttributeList[0] := r;
  end;
  Result := FTokenAttributeList;
  SetLength(FTokenAttributeList, 0)
end;

function TLazEditCustomHighlighter.GetEndOfLineAttribute: TLazEditTextAttribute;
begin
  Result := nil;
end;

function TLazEditCustomHighlighter.GetEndOfLineAttributeEx: TLazCustomEditTextAttribute;
begin
  Result := GetEndOfLineAttribute;
end;

{ TLazEditCustomRangesHighlighter }

procedure TLazEditCustomRangesHighlighter.SetCurrentLines(AValue: TLazEditStringsBase);
begin
  if AValue = CurrentLines then
    exit;

  if AValue <> nil then begin
    FCurrentRanges := TLazHighlighterLineRangeList(AValue.Ranges[GetRangeIdentifier]);
    CurrentRanges.ValidatedChangeStamp := FRangesChangeStamp;
  end
  else
    FCurrentRanges := nil;

  inherited SetCurrentLines(AValue);
end;

procedure TLazEditCustomRangesHighlighter.SendRescanNeededNotification;
begin
  {$PUSH}{$R-}{$Q-}
  inc(FRangesChangeStamp);
  {$POP}
  if CurrentRanges <> nil then
    CurrentRanges.ValidatedChangeStamp := FRangesChangeStamp;

  inherited SendRescanNeededNotification;
end;

procedure TLazEditCustomRangesHighlighter.DoStartAtLine;
begin
  if LineIndex = 0 then
    ResetRange
  else
    SetRange(CurrentRanges[LineIndex - 1]);

  //inherited DoStartAtLine;
end;

function TLazEditCustomRangesHighlighter.GetRangeIdentifier: Pointer;
begin
  Result := Self;
end;

function TLazEditCustomRangesHighlighter.CreateRangeList(ALines: TLazEditStringsBase
  ): TLazHighlighterLineRangeList;
begin
  Result := TLazHighlighterLineRangeShiftList.Create;
end;

procedure TLazEditCustomRangesHighlighter.DoAttachedToLines(Lines: TLazEditStringsBase;
  ARangeList: TLazHighlighterLineRangeList);
begin
  //
end;

procedure TLazEditCustomRangesHighlighter.DoDetachingFromLines(Lines: TLazEditStringsBase;
  ARangeList: TLazHighlighterLineRangeList);
begin
  //
end;

procedure TLazEditCustomRangesHighlighter.AttachToLines(ALines: TLazEditStringsBase);
var
  r: TLazHighlighterLineRangeList;
begin
  // Don't call inherited, since DoAttachedToLines needs to be called differently / and adding to list is only happening once per lines
  r := TLazHighlighterLineRangeList(ALines.Ranges[GetRangeIdentifier]);
  if assigned(r) then begin
    r.IncRefCount;
  end
  else begin
    r := CreateRangeList(ALines);
    ALines.Ranges[GetRangeIdentifier] := r;
    r.InvalidateAll;
    r.ValidatedChangeStamp := FRangesChangeStamp;
    AttachedLines.Add(ALines);
    ALines.AddFreeNotification(@DoAttachedLinesFreed);
  end;
  DoAttachedToLines(ALines, r);
  CurrentLines := nil;
end;

procedure TLazEditCustomRangesHighlighter.DetachFromLines(ALines: TLazEditStringsBase);
var
  r: TLazHighlighterLineRangeList;
begin
  r := TLazHighlighterLineRangeList(ALines.Ranges[GetRangeIdentifier]);
  if not assigned(r) then
    exit;

  r.DecRefCount;
  DoDetachingFromLines(ALines, r);
  if r.RefCount = 0 then begin
    ALines.Ranges[GetRangeIdentifier] := nil;
    if FCurrentRanges = r then begin
      FCurrentRanges := nil;
      CurrentLines := nil;
    end;
    r.Free;
    AttachedLines.Remove(ALines);
    ALines.RemoveFreeNotification(@DoAttachedLinesFreed);
  end;
end;

end.

