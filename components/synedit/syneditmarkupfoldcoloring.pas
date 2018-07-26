{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: SynEditMarkupFoldColoring.pas, released 2015-12-07.
Copyleft (c) 2015-2016 x2nie - Fathony Luth.

The Original SynEdit Project is based on mwCustomEdit.pas by Martin Waldenburg,
part of the mwEdit component suite.
Portions created by Martin Waldenburg are Copyright (C) 1998 Martin Waldenburg.
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



Features:
  - paint keywords in multiple colors, depends on fold block level or by config
  - paint vertical line between paired open~close fold
  - vertical line and/or keyword can be disabled
  - independent, can be used for any SynHighlighter
  - many features are well tested for PasSynPas.pas
  - only active when SynEdit.Highlighter is TSynCustomFoldHighlighter

-------------------------------------------------------------------------------}
unit SynEditMarkupFoldColoring;

{$mode objfpc}{$H+}
{ $define SynEditMarkupFoldColoringDebug}
{ $define WithSynMarkupFoldColorDebugGutter}

interface

uses
  Classes, SysUtils, Graphics, SynEditMarkup, SynEditMiscClasses, Controls,
  LCLProc, LCLType, SynEditHighlighter,
  SynEditHighlighterFoldBase, LazSynEditText, SynEditTextBase, SynEditTypes
  {$IFDEF WithSynMarkupFoldColorDebugGutter}, SynGutterBase, SynTextDrawer{$ENDIF}
;

type

  {$IFDEF WithSynMarkupFoldColorDebugGutter}
  TSynEditMarkupFoldColors = class;

  { TIDESynMarkupFoldColorDebugGutter }

  TIDESynMarkupFoldColorDebugGutter = class(TSynGutterPartBase)
  protected
    FOwner: TSynEditMarkupFoldColors;
    function  PreferedWidth: Integer; override;
  public
    procedure Paint(Canvas: TCanvas; AClip: TRect; FirstLine, LastLine: integer);
      override;
  end;
  {$ENDIF}

  PMarkupFoldColorInfo = ^TMarkupFoldColorInfo;
  TMarkupFoldColorInfo = record
    PhysX, PhysX2, PhysCol: Integer;
    ColorIdx: Integer;
    Border  : Boolean;
    Ignore  : Boolean; //no color no line
    SrcNode : TSynFoldNodeInfo;
    Level, LevelAfter : integer; //needed by non nest nodes
  end;

  TMarkupFoldColorInfos = array of TMarkupFoldColorInfo;
  TSynFoldNodeInfos     = array of TSynFoldNodeInfo; //for quick compare detection

  TColumnCacheEntry = Integer;
  PColumnCacheEntry = ^TColumnCacheEntry;

  { TMarkupFoldColorsLineColor }

  TMarkupFoldColorsLineColor = class
  private
    FAlpha: Byte;
    FColor: TColor;
    FOnChange: TNotifyEvent;
    FOnChanged: TNotifyEvent;
    FPriority: Integer;
    FStyle: TSynLineStyle;
    procedure SetAlpha(AValue: Byte);
    procedure SetColor(AValue: TColor);
    procedure SetPriority(AValue: Integer);
    procedure SetStyle(AValue: TSynLineStyle);
    procedure Changed;
  public
    constructor Create;
    property Color: TColor read FColor write SetColor; // clDefault will take Color[].Frame or Color[].Foreground
    property Style: TSynLineStyle read FStyle write SetStyle;
    property Alpha: Byte read FAlpha write SetAlpha;
    property Priority: Integer read FPriority write SetPriority;
    property OnChange: TNotifyEvent read FOnChanged write FOnChanged;
  end;

  { TSynEditMarkupFoldColorsColumnCache }

  TSynEditMarkupFoldColorsColumnCache = class(TSynManagedStorageMem)
  private
    function GetColumnData(Index: Integer): TColumnCacheEntry;
    function GetIsValidForLine(Index: Integer): Boolean;
    procedure SetColumnData(Index: Integer; AValue: TColumnCacheEntry);
  protected
    procedure LineTextChanged(AIndex: Integer; ACount: Integer = 1); override;
    procedure InsertedLines(AIndex, ACount: Integer); override;
    //procedure DeletedLines(AIndex, ACount: Integer); override;
    procedure Invalidate;
  public
    constructor Create;
    property ColumnData[Index: Integer]: TColumnCacheEntry read GetColumnData write SetColumnData; default;
    property IsValidForLine[Index: Integer]: Boolean read GetIsValidForLine;
  end;

  { TSynEditMarkupFoldColors }

  TSynEditMarkupFoldColors = class(TSynEditMarkup)
  private
    {$IFDEF WithSynMarkupFoldColorDebugGutter}
    FDebugGutter: TIDESynMarkupFoldColorDebugGutter;
    {$ENDIF}
    function GetFirstCharacterColumn(pIndex: Integer): TColumnCacheEntry;
    procedure TextBufferChanged(pSender: TObject);
  private
    FColorCount: Integer;
    fHighlighter: TSynCustomFoldHighlighter;
    fMarkupColors: array of TSynSelectedColor;
    fLineColors : array of TMarkupFoldColorsLineColor;
    fNestList, fNestList2: TLazSynEditNestedFoldsList;

    // cache
    FColumnCache: TSynEditMarkupFoldColorsColumnCache;
    fFoldColorInfosCount,
    fFoldColorInfosCapacity: Integer;

    fDefaultGroup: integer;
    fFoldColorInfos: TMarkupFoldColorInfos;

    fPreparedRow: integer;
    fLastOpenNode: TSynFoldNodeInfo;
    fLastIndex,
    fLastOpenIndex: Integer;
    fLastEnabled: Boolean;

    procedure DoMarkupParentFoldAtRow(pRow: Integer);
    procedure DoMarkupParentCloseFoldAtRow(pRow: Integer);
    function  GetColor(pIndex: Integer): TSynSelectedColor;
    function  GetLineColor(pIndex: Integer): TMarkupFoldColorsLineColor;
    procedure SetColorCount(AValue: Integer);
    procedure SetDefaultGroup(pValue: integer);
    procedure SetFoldColorInfosCount(pNewCount: Integer);
    procedure InitNestList;
    property FirstCharacterColumn[pIindex: Integer]: TColumnCacheEntry read GetFirstCharacterColumn;
  protected
    // Notifications about Changes to the text
    procedure DoTextChanged({%H-}pStartLine, pEndLine, {%H-}pCountDiff: Integer); override; // 1 based
    procedure SetLines(const pValue: TSynEditStrings); override;
    procedure HighlightChanged(pSender: TSynEditStrings; pIndex, pCount: Integer);
    procedure DoEnabledChanged(pSender: TObject); override;
    procedure ColorChanged(pMarkup: TObject);
  public
    constructor Create(pSynEdit : TSynEditBase);
    destructor Destroy; override;
    function RealEnabled: Boolean; override;
    procedure BeginMarkup; override;
    function GetMarkupAttributeAtRowCol(const pRow: Integer;
                                        const pStartCol: TLazSynDisplayTokenBound;
                                        const {%H-}pRtlInfo: TLazSynDisplayRtlInfo): TSynSelectedColor; override;
    procedure GetNextMarkupColAfterRowCol(const pRow: Integer;
                                         const pStartCol: TLazSynDisplayTokenBound;
                                         const {%H-}pRtlInfo: TLazSynDisplayRtlInfo;
                                         out   pNextPhys, pNextLog: Integer); override;

    procedure PrepareMarkupForRow(pRow : Integer); override;
    property DefaultGroup : integer read fDefaultGroup write SetDefaultGroup;
    property ColorCount: Integer read FColorCount write SetColorCount;
    property Color[pIndex: Integer]: TSynSelectedColor read GetColor;
    property LineColor[pIndex: Integer]: TMarkupFoldColorsLineColor read GetLineColor;
  end;

implementation
uses
  SynEdit,
  SynEditMiscProcs,
  {$IFDEF SynEditMarkupFoldColoringDebug}
  SynHighlighterPas,
  strutils,
  {$endif}
  Dialogs;

{ TMarkupFoldColorsLineColor }

procedure TMarkupFoldColorsLineColor.SetColor(AValue: TColor);
begin
  if FColor = AValue then Exit;
  FColor := AValue;
  Changed;
end;

procedure TMarkupFoldColorsLineColor.SetAlpha(AValue: Byte);
begin
  if FAlpha = AValue then Exit;
  FAlpha := AValue;
  Changed;
end;

procedure TMarkupFoldColorsLineColor.SetPriority(AValue: Integer);
begin
  if FPriority = AValue then Exit;
  FPriority := AValue;
  Changed;
end;

procedure TMarkupFoldColorsLineColor.SetStyle(AValue: TSynLineStyle);
begin
  if FStyle = AValue then Exit;
  FStyle := AValue;
  Changed;
end;

procedure TMarkupFoldColorsLineColor.Changed;
begin
  if FOnChange <> nil then
    FOnChange(Self);
end;

constructor TMarkupFoldColorsLineColor.Create;
begin
  FStyle := slsSolid;
  FColor := clDefault;
  FPriority := 0;
  inherited;
end;

{ TSynEditMarkupFoldColorsColumnCache }

function TSynEditMarkupFoldColorsColumnCache.GetColumnData(Index: Integer
  ): TColumnCacheEntry;
begin
  Result := PColumnCacheEntry(ItemPointer[Index])^;
end;

function TSynEditMarkupFoldColorsColumnCache.GetIsValidForLine(Index: Integer
  ): Boolean;
begin
  Result := PColumnCacheEntry(ItemPointer[Index])^ > 0;
end;

procedure TSynEditMarkupFoldColorsColumnCache.SetColumnData(Index: Integer;
  AValue: TColumnCacheEntry);
begin
  PColumnCacheEntry(ItemPointer[Index])^ := AValue;
end;

procedure TSynEditMarkupFoldColorsColumnCache.LineTextChanged(AIndex: Integer;
  ACount: Integer);
var
  i: Integer;
begin
  for i := AIndex to AIndex + ACount - 1 do
    ColumnData[i] := 0;
end;

procedure TSynEditMarkupFoldColorsColumnCache.InsertedLines(AIndex,
  ACount: Integer);
var
  i: Integer;
begin
  for i := AIndex to AIndex + ACount - 1 do
    ColumnData[i] := 0;
end;

procedure TSynEditMarkupFoldColorsColumnCache.Invalidate;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    ColumnData[i] := 0;
end;

constructor TSynEditMarkupFoldColorsColumnCache.Create;
begin
  inherited;
  ItemSize := SizeOf(TColumnCacheEntry);
end;

{$IFDEF WithSynMarkupFoldColorDebugGutter}
{ TIDESynMarkupFoldColorDebugGutter }

function TIDESynMarkupFoldColorDebugGutter.PreferedWidth: Integer;
begin
  Result := 600;
end;

procedure TIDESynMarkupFoldColorDebugGutter.Paint(Canvas: TCanvas;
  AClip: TRect; FirstLine, LastLine: integer);
var
  TextDrawer: TheTextDrawer;
  dc: HDC;
  rcLine: TRect;
  LineHeight, c, i, j: Integer;
  iLine: LongInt;
  s, fc: string;
begin
  TextDrawer := Gutter.TextDrawer;
  dc := Canvas.Handle;
  TextDrawer.BeginDrawing(dc);
  try
    TextDrawer.SetBackColor(Gutter.Color);
    TextDrawer.SetForeColor(TCustomSynEdit(SynEdit).Font.Color);
    TextDrawer.SetFrameColor(clNone);
    with AClip do
      TextDrawer.ExtTextOut(Left, Top, ETO_OPAQUE, AClip, nil, 0);

    rcLine := AClip;
    rcLine.Bottom := AClip.Top;
    LineHeight := TCustomSynEdit(SynEdit).LineHeight;
    c := TCustomSynEdit(SynEdit).Lines.Count;
    for i := FirstLine to LastLine do
    begin
      iLine := FoldView.DisplayNumber[i];
      if (iLine <= 0) or (iLine > c) then break;
      // next line rect
      rcLine.Top := rcLine.Bottom;
      rcLine.Bottom := rcLine.Bottom + LineHeight;

      FOwner.PrepareMarkupForRow(iLine);
      s := '';
      for j := 0 to FOwner.fFoldColorInfosCount - 1 do begin
        with FOwner.fFoldColorInfos[j] do
          s := s + '('
           + IntToStr(PhysX) + ',' + IntToStr(PhysX2) + ',' + IntToStr(PhysCol) + '/'
           + IntToStr(ColorIdx) + '/'
           + BoolToStr(Border, True)[1] + BoolToStr(Ignore, True)[1] + '/'
           + IntToStr(Level) + ',' + IntToStr(LevelAfter)
           + ') ';
      while length(s) < 21 * (j+1) do s := s + ' ';
      end;
      s := IntToStr(FOwner.fFoldColorInfosCount) + s;
      if iLine < FOwner.FColumnCache.Count then
        s := s + ', '+IntToStr(FOwner.FColumnCache[ToIdx(iLine)]);

      TextDrawer.ExtTextOut(rcLine.Left, rcLine.Top, ETO_OPAQUE or ETO_CLIPPED, rcLine,
        PChar(Pointer(S)),Length(S));
    end;

  finally
    TextDrawer.EndDrawing;
  end;
end;
{$ENDIF}


{$IFDEF SynEditMarkupFoldColoringDebug}
function FoldTypeToStr(p_FoldType: Pointer): String;
begin
  WriteStr(Result, TPascalCodeFoldBlockType(PtrUInt(p_FoldType)));
  while length(Result) < 17 do Result := Result + ' ';
end;
{$ENDIF}


{ TSynEditMarkupFoldColors }

constructor TSynEditMarkupFoldColors.Create(pSynEdit: TSynEditBase);
begin
  inherited Create(pSynEdit);

  {$IFDEF WithSynMarkupFoldColorDebugGutter}
  FDebugGutter := TIDESynMarkupFoldColorDebugGutter.Create(TSynEdit(pSynEdit).RightGutter.Parts);
  FDebugGutter.FOwner := Self;
  {$ENDIF}

  FColumnCache := TSynEditMarkupFoldColorsColumnCache.Create;

  fHighlighter := TSynCustomFoldHighlighter(TCustomSynEdit(SynEdit).Highlighter);
  if Assigned(fHighlighter)
  and not (fHighlighter  is TSynCustomFoldHighlighter) then
    fHighlighter := nil;

  fDefaultGroup := 0;
  fFoldColorInfosCount := 0;
  SetLength(fFoldColorInfos, 50);
  fFoldColorInfosCapacity := 50;

  fNestList := TLazSynEditNestedFoldsList.Create(Lines, fHighlighter);
  fNestList.ResetFilter;
  fNestList.FoldGroup := fDefaultGroup;
  fNestList.FoldFlags := [sfbIncludeDisabled];
  fNestList.IncludeOpeningOnLine := True;

  // for scanning the "if" of a "then" to find the indent
  fNestList2 := TLazSynEditNestedFoldsList.Create(Lines, fHighlighter);
  fNestList2.ResetFilter;
  fNestList2.FoldGroup := fDefaultGroup;
  fNestList2.FoldFlags := [sfbIncludeDisabled];
  fNestList2.IncludeOpeningOnLine := False;

  MarkupInfo.Foreground := clGreen;
  MarkupInfo.Background := clNone;
  MarkupInfo.Style := [];
  MarkupInfo.StyleMask := [];
  MarkupInfo.FrameEdges:= sfeLeft;

  SetColorCount(6);
  fMarkupColors[0].Foreground  := clRed;
  fMarkupColors[1].Foreground  := $000098F7; //orange
  fMarkupColors[2].Foreground  := $0022CC40; //green
  fMarkupColors[3].Foreground  := $00CCCC00; //cyan
  fMarkupColors[4].Foreground  := $00FF682A; //blue
  fMarkupColors[5].Foreground  := $00CF00C4; //purple
end;

destructor TSynEditMarkupFoldColors.Destroy;
begin
  if Lines <> nil then
    Lines.Ranges[Self] := nil;
  FColumnCache.Free;

  ColorCount := 0;
  if Assigned(Lines) then begin
    Lines.RemoveChangeHandler(senrHighlightChanged, @HighlightChanged);
    Lines.RemoveNotifyHandler(senrTextBufferChanged, @TextBufferChanged);
  end;
  FreeAndNil(fNestList);
  FreeAndNil(fNestList2);
  inherited Destroy;
end;

function TSynEditMarkupFoldColors.RealEnabled: Boolean;
begin
  Result := (not IsTempDisabled) and Enabled and (FColorCount > 0);
end;

procedure TSynEditMarkupFoldColors.BeginMarkup;
begin
  {$IFDEF SynEditMarkupFoldColoringDebug}
  //DebugLn('BeginMarkup');
  {$ENDIF}
  inherited BeginMarkup;
  if not Assigned(fHighlighter) then
    exit;
  fNestList.Clear; // for next markup start
  fNestList2.Clear;
end;

function TSynEditMarkupFoldColors.GetMarkupAttributeAtRowCol(
  const pRow: Integer; const pStartCol: TLazSynDisplayTokenBound;
  const pRtlInfo: TLazSynDisplayRtlInfo): TSynSelectedColor;
var
  i, x2both: integer;
begin
  Result := nil;
  if not Assigned(fHighlighter) then exit;
  if (fPreparedRow = pRow) then begin
    {$IFDEF SynEditMarkupFoldColoringDebug}
    //DebugLn('   GetMarkupAttributeAtRowCol %d/%d', [aRow, aStartCol.Logical]);
    {$ENDIF}

    x2both := 0;
    for i := 0 to fFoldColorInfosCount - 1 do
      with fFoldColorInfos[i] do
        if not Ignore
        and (PhysX < PhysX2)
        and (ColorIdx >= 0)
        and (pStartCol.Physical >= PhysX)
        and (pStartCol.Physical < PhysX2) then begin
          {$IFDEF SynEditMarkupFoldColoringDebug}
          //DebugLn('      X=%d X2=%d Y=%d, C=%d B=%s I=%s', [X, X2, Y, ColorIdx, IfThen(Border, 'X', '-'), IfThen(Ignore, 'X', '-')]);
          {$ENDIF}

          Result := MarkupInfo;
          x2both := max(x2both, PhysX2);
          if Border then begin
            MarkupInfo.Clear;
            MarkupInfo.SetFrameBoundsPhys(PhysX, x2both);
            MarkupInfo.FrameAlpha := fLineColors[ColorIdx].Alpha;
            MarkupInfo.FramePriority := fLineColors[ColorIdx].Priority;
            MarkupInfo.FrameColor := fLineColors[ColorIdx].Color;
            if MarkupInfo.FrameColor = clDefault then begin
              if (fMarkupColors[ColorIdx].FrameColor <> clNone) and
                 (fMarkupColors[ColorIdx].FrameColor <> clDefault)
              then
                MarkupInfo.FrameColor := fMarkupColors[ColorIdx].FrameColor
              else
                MarkupInfo.FrameColor := fMarkupColors[ColorIdx].Foreground;
            end;
            MarkupInfo.FrameStyle := fLineColors[ColorIdx].Style;
            MarkupInfo.FrameEdges := sfeLeft;
          end else begin
            MarkupInfo.Assign(fMarkupColors[ColorIdx]);
            MarkupInfo.SetFrameBoundsPhys(PhysX, PhysX2);
          end;
        end;
  end;
end;

procedure TSynEditMarkupFoldColors.GetNextMarkupColAfterRowCol(
  const pRow: Integer; const pStartCol: TLazSynDisplayTokenBound;
  const pRtlInfo: TLazSynDisplayRtlInfo; out pNextPhys, pNextLog: Integer);
var i : integer;
begin
  {$IFDEF SynEditMarkupFoldColoringDebug}
  //DebugLn('GetNextMarkupColAfterRowCol %d/%d', [aRow, aStartCol.Logical]);
  {$ENDIF}
  if not Assigned(fHighlighter)
  or (fPreparedRow <> pRow) then
    exit;

  pNextLog := -1;
  pNextPhys := -1;
  for i := 0 to fFoldColorInfosCount - 1  do
    with fFoldColorInfos[i] do begin
      if not Ignore and (ColorIdx >= 0) and
         (  ((not Border) and  fMarkupColors[ColorIdx].IsEnabled) or
            (Border and (fLineColors[ColorIdx].Color <> clNone))
         ) and
         (PhysX < PhysX2) and (pStartCol.Physical < PhysX) and (pStartCol.Physical <= PhysX2)
      then begin
        pNextPhys := fFoldColorInfos[i].PhysX;
        break;
      end;
    end;
end;

function TSynEditMarkupFoldColors.GetFirstCharacterColumn(pIndex: Integer
  ): TColumnCacheEntry;
var
  l: String;
  s, p: Integer;
begin
  l := SynEdit.Lines[pIndex];
  s := length(l);
  p := 1;
  while (p <= s)
  //and (l[p] in [#9, #32, '/']) do inc(p);
  and (l[p] in [#9, #32]) do inc(p);
  if p > s then
    exit(high(Result));
  Result := TCustomSynEdit(SynEdit).LogicalToPhysicalPos(Point(p, toPos(pIndex))).x;
end;

procedure TSynEditMarkupFoldColors.TextBufferChanged(pSender: TObject);
begin
  if not Enabled then
    exit;
  InitNestList;

  if pSender <> nil then
    TSynEditStrings(pSender).Ranges[Self] := nil;

  FColumnCache.Capacity := SynEdit.Lines.Capacity;
  FColumnCache.Count := SynEdit.Lines.Count;
  Lines.Ranges[Self] := FColumnCache;

  FColumnCache.Invalidate;
end;

procedure TSynEditMarkupFoldColors.DoMarkupParentFoldAtRow(pRow: Integer);
var
  lNodeCol: TColumnCacheEntry;
  i, lLvl, lLineIdx, lCurIndex: Integer;
  lCurNode: TSynFoldNodeInfo;

  procedure AddVerticalLine;
  begin
    with fFoldColorInfos[lCurIndex] do begin
      SrcNode:= lCurNode; //needed by close node
      PhysCol := lNodeCol;
      PhysX := lNodeCol;
      PhysX2 := PhysX + 1;
      Border := PhysX < GetFirstCharacterColumn(lLineIdx); // use real one here not cache
      Ignore :=
        (Border and (sfaOutlineNoLine in lCurNode.FoldAction))
        or (not Border);
      Level := lLvl;
      ColorIdx := Max(0, lLvl) mod FColorCount;
      {$IFDEF SynEditMarkupFoldColoringDebug}
      //DebugLn('  %.5d %.2d %.2d-%.2d: %d - %s %.5d:%s', [Row, PhysCol, PhysX, PhysX2, Level, IfThen(sfaClose in SrcNode.FoldAction, 'C ', IfThen(sfaOpen in SrcNode.FoldAction, 'O ', '??')),ToPos(SrcNode.LineIndex),FoldTypeToStr(SrcNode.FoldType)]);
      {$ENDIF}
    end;
  end;

  procedure InitColumnForKeepLvl(LineIdx, FoldGroup: Integer);
  var
    LevelOpenNode: TSynFoldNodeInfo;
    i, ONodeFirstCol: integer;
  begin
    ONodeFirstCol := FirstCharacterColumn[LineIdx];
    FColumnCache[LineIdx] := ONodeFirstCol;

    fNestList2.Line := LineIdx;
    fNestList2.FoldGroup := FoldGroup;
    if fNestList2.Count = 0 then
      exit;
    LevelOpenNode := fNestList2.HLNode[fNestList2.Count-1];
    if sfaInvalid in LevelOpenNode.FoldAction then
      exit; // try node before ??

    if not FColumnCache.IsValidForLine[LevelOpenNode.LineIndex] then begin
      if (LevelOpenNode.NestLvlStart <  fHighlighter.FoldBlockEndLevel(LevelOpenNode.LineIndex-1, lCurNode.FoldGroup, [sfbIncludeDisabled])) and
         (sfaCloseAndOpen in LevelOpenNode.FoldAction)
      then
        InitColumnForKeepLvl(LevelOpenNode.LineIndex, FoldGroup)
      else
        FColumnCache[LevelOpenNode.LineIndex] := FirstCharacterColumn[LevelOpenNode.LineIndex];
      if not FColumnCache.IsValidForLine[LevelOpenNode.LineIndex] then
        exit;
    end;
    i := FColumnCache[LevelOpenNode.LineIndex];
    FColumnCache[LineIdx] := min(i, ONodeFirstCol);
  end;

var
  lKeepLevel: Boolean;
  LastNode: TSynFoldNodeInfo;
begin
  lLineIdx := ToIdx(pRow);
  fNestList.Line := lLineIdx;
  fHighlighter.CurrentLines := Lines;
  LastNode.LineIndex := -1;

  // get first character for current line
  if not FColumnCache.IsValidForLine[lLineIdx] then
    FColumnCache[lLineIdx] := FirstCharacterColumn[lLineIdx];

  lLvl := 0;
  i := 0; // starting at the node with the lowest line number
  while i < fNestList.Count do begin
    lCurNode := fNestList.HLNode[i];
    // sanity check
    Assert(sfaOpen in lCurNode.FoldAction, 'no sfaOpen in lCurNode.FoldAction');
    {$IFDEF SynEditMarkupFoldColoringDebug}
    //DebugLn('  O: %s %s %s', [IfThen(sfaOutline in lCurNode.FoldAction, 'X', '-'), IfThen(sfaClose in lCurNode.FoldAction, 'C ', IfThen(sfaOpen in lCurNode.FoldAction, 'O ', '??')),FoldTypeToStr(lCurNode.FoldType)]);
    {$ENDIF}
    if (sfaOutline in lCurNode.FoldAction)
    and not (sfaInvalid in lCurNode.FoldAction)
    and (lCurNode.LineIndex <> lLineIdx) then begin

      if ( sfaOutlineForceIndent in lCurNode.FoldAction) then
        inc(lLvl)
      else if ( sfaOutlineMergeParent in lCurNode.FoldAction) then
        dec(lLvl);

      // new FoldColorInfo
      SetFoldColorInfosCount(fFoldColorInfosCount + 1);
      lCurIndex := fFoldColorInfosCount - 1;

      {$IFDEF SynEditMarkupFoldColoringDebug}
      //if (fLastOpenNode.LineIndex >= 0) then
      //  DebugLn('   %s %s - %s %s', [FoldTypeToStr(fLastOpenNode.FoldType), IfThen(sfaOutlineKeepLevel in fLastOpenNode.FoldAction, '(Keep)', ''), FoldTypeToStr(lCurNode.FoldType), IfThen(sfaOutlineKeepLevel in lCurNode.FoldAction, '(Keep)', '')]);
      {$ENDIF}


      // find lastnode // first opening node on this line, that is = hl.line[-1].endnestlevel (-1)
      if (lCurNode.NestLvlStart <  fHighlighter.FoldBlockEndLevel(lCurNode.LineIndex-1, lCurNode.FoldGroup, [sfbIncludeDisabled])) and
         (sfaCloseAndOpen in lCurNode.FoldAction)
// // TODO: check that this is the FIRST sfaCloseAndOpen on this line
      then
        InitColumnForKeepLvl(lCurNode.LineIndex, lCurNode.FoldGroup);

      if not FColumnCache.IsValidForLine[lCurNode.LineIndex] then
        FColumnCache[lCurNode.LineIndex] := FirstCharacterColumn[lCurNode.LineIndex];
      lNodeCol := FColumnCache[lCurNode.LineIndex];

      { do not keep level if two consecutive sfaOutlineKeepLevel nodes are
        on different lines and start on different columns                  }
      if (LastNode.LineIndex >= 0)
      and (sfaOutlineKeepLevel in LastNode.FoldAction) then begin
        {$IFDEF SynEditMarkupFoldColoringDebug}
        //DebugLn('   %.5d/%.5d %.2d/%.2d', [LastNode.LineIndex+1,lCurNode.LineIndex+1, FFoldColorInfos[fLastIndex].PhysCol, lNodeCol]);
        //DbgOut('    keep');
        {$ENDIF}
        lKeepLevel := True;
        if (sfaOutlineKeepLevel in lCurNode.FoldAction)
        and not (LastNode.LineIndex = lCurNode.LineIndex)
        and not (fFoldColorInfos[fLastIndex].PhysCol = lNodeCol) then begin
          {$IFDEF SynEditMarkupFoldColoringDebug}
          //DbgOut(' not');
          {$ENDIF}
          inc(lLvl);
          lKeepLevel := False;
        end;
        {$IFDEF SynEditMarkupFoldColoringDebug}
        //DebugLn('');
        {$ENDIF}
      end else
        lKeepLevel := False;

      AddVerticalLine;

      if lKeepLevel then begin
        // keep level for none sfaOutlineKeepLevel after sfaOutlineKeepLevel
        lLvl := fFoldColorInfos[fLastIndex].Level;
        if fFoldColorInfos[fLastIndex].PhysX < fFoldColorInfos[lCurIndex].PhysX then
          fFoldColorInfos[lCurIndex].PhysX := fFoldColorInfos[fLastIndex].PhysX;
        fFoldColorInfos[lCurIndex].Level := lLvl;
        fFoldColorInfos[lCurIndex].ColorIdx := Max(0, lLvl) mod FColorCount;
        // overwrite first character column with new value
        if fFoldColorInfos[fLastIndex].PhysX < FColumnCache[fFoldColorInfos[lCurIndex].SrcNode.LineIndex] then
          FColumnCache[fFoldColorInfos[lCurIndex].SrcNode.LineIndex] := fFoldColorInfos[fLastIndex].PhysX;
        {$IFDEF SynEditMarkupFoldColoringDebug}
        //with FFoldColorInfos[lCurIndex] do
        //  DebugLn('  > > > %.2d %.2d-%.2d: %d - %s %.5d:%s', [PhysCol, PhysX, PhysX2, Level, IfThen(sfaClose in SrcNode.FoldAction, 'C ', IfThen(sfaOpen in SrcNode.FoldAction, 'O ', '??')),ToPos(SrcNode.LineIndex),FoldTypeToStr(SrcNode.FoldType)]);
        {$ENDIF}
      end;

      if not (sfaOutlineKeepLevel in lCurNode.FoldAction) then
        inc(lLvl);

      LastNode := lCurNode;
      fLastIndex := lCurIndex;
      fLastOpenNode := lCurNode;
      fLastOpenIndex := lCurIndex;

      with fFoldColorInfos[fFoldColorInfosCount - 1] do begin
        LevelAfter  := lLvl;  // used in DoMarkupParentCloseFoldAtRow
      end;
    end;
    inc(i);
  end;
end;

procedure TSynEditMarkupFoldColors.DoMarkupParentCloseFoldAtRow(pRow: Integer);
var
  lMaxLevel, lvl, lCurIndex: Integer;
  lCurNode: TSynFoldNodeInfo;
  lKeepLevel: Boolean;
  lNodeCol: TColumnCacheEntry;

  function AddHighlight: Boolean;
  var
    lPhysX, lPhysX2, j: Integer;
  begin
    Result := False;
    // ignore implicit close nodes at end of line, especially if line is empty
    // or at least has less characters as vertical line is on
    if not(sfaCloseForNextLine in lCurNode.FoldAction) then begin
      Result := True;
      lPhysX := TCustomSynEdit(SynEdit).LogicalToPhysicalPos(Point(ToPos(lCurNode.LogXStart), ToPos(lCurNode.LineIndex))).x;
      lPhysX2 := TCustomSynEdit(SynEdit).LogicalToPhysicalPos(Point(ToPos(lCurNode.LogXEnd), ToPos(lCurNode.LineIndex))).x;
      if lCurNode.LogXStart < lCurNode.LogXEnd then begin
        {$IFDEF SynEditMarkupFoldColoringDebug}
        //DebugLn('    %d < %d', [lCurNode.LogXStart, lCurNode.LogXEnd]);
        {$ENDIF}
        for j := 0 to fFoldColorInfosCount - 1 do
          if (fFoldColorInfos[j].PhysX = lPhysX)
          and (fFoldColorInfos[j].Border)
          and (fFoldColorInfos[j].SrcNode.FoldType = lCurNode.FoldType )
          and (fFoldColorInfos[j].SrcNode.FoldLvlEnd = lCurNode.FoldLvlStart ) then begin
            {$IFDEF SynEditMarkupFoldColoringDebug}
            //DebugLn('      X2: %d->%d', [FFoldColorInfos[j].X2, lCurNode.LogXEnd + 1]);
            {$ENDIF}
            fFoldColorInfos[j].PhysX2 := lPhysX2;
            fFoldColorInfos[j].Border := False;
          end;
      end;

      SetFoldColorInfosCount(fFoldColorInfosCount + 1);
      lCurIndex := fFoldColorInfosCount - 1;
      with fFoldColorInfos[lCurIndex] do begin
        Ignore := False;
        Border := False;
        SrcNode:= lCurNode; //needed by close node
        PhysX := lPhysX;
        //if not FColumnCache.IsValidForLine[lCurNode.LineIndex] then
        //  FColumnCache[lCurNode.LineIndex] := FirstCharacterColumn[lCurNode.LineIndex];
        PhysCol := lNodeCol; //FColumnCache[lCurNode.LineIndex];
        PhysX2 := lPhysX2;
        Level := lvl;
        lMaxLevel := Max(lMaxLevel, lvl);
        if not (sfaOutlineNoColor in lCurNode.FoldAction) then
           ColorIdx := Max(0, lvl) mod FColorCount
        else
           ColorIdx := -1;

        {$IFDEF SynEditMarkupFoldColoringDebug}
        //DebugLn('  %.5d %.2d %.2d-%.2d: %d - %s %.5d:%s - %s', [Row, PhysCol, PhysX, PhysX2, Level, IfThen(sfaClose in SrcNode.FoldAction, 'C ', IfThen(sfaOpen in SrcNode.FoldAction, 'O ', '??')),ToPos(SrcNode.LineIndex),FoldTypeToStr(SrcNode.FoldType), IfThen(lKeepLevel, 'Keep', '')]);
        {$ENDIF}
      end;
    end;
  end;

var
  lLineIdx,i,j,lvlA , k: integer;
  lNodeList: TLazSynFoldNodeInfoList;

begin
  lLineIdx := ToIdx(pRow);

  // as all nodes will be on pRow we can set lNodeCol here already
  if not FColumnCache.IsValidForLine[lLineIdx] then
    FColumnCache[lLineIdx] := FirstCharacterColumn[lLineIdx];
  lNodeCol := FColumnCache[lLineIdx];

  fHighlighter.CurrentLines := Lines;

  lNodeList := fHighlighter.FoldNodeInfo[lLineIdx];
  lNodeList.ClearFilter; // only needed once, in case the line was already used
  lNodeList.AddReference;
  try
    lNodeList.ActionFilter := [sfaOutline];
    lvl := 0;
    J := fFoldColorInfosCount - 1;
    if J >=0 then
      lvl := max(0,fFoldColorInfos[J].LevelAfter);
    lMaxLevel := lvl;
    i := 0;
    repeat
      lCurNode := lNodeList[i];
      lCurIndex := fFoldColorInfosCount - 1;
      // sanity check
      Assert(lCurNode.LineIndex = lLineIdx, 'Node not on aRow');

      {$IFDEF SynEditMarkupFoldColoringDebug}
      //if not (sfaInvalid in lCurNode.FoldAction) then
      //  DebugLn('  C: %s %s %s', [IfThen(sfaOutline in lCurNode.FoldAction, 'X', '-'), IfThen(sfaClose in lCurNode.FoldAction, 'C ', IfThen(sfaOpen in lCurNode.FoldAction, 'O ', '??')),FoldTypeToStr(lCurNode.FoldType)]);
      {$ENDIF}

      if not (sfaInvalid in lCurNode.FoldAction)
      and (sfaOutline in lCurNode.FoldAction) then begin
        if sfaOpen in lCurNode.FoldAction then begin

          if ( sfaOutlineForceIndent in lCurNode.FoldAction) then
            inc(lvl)
          else if ( sfaOutlineMergeParent in lCurNode.FoldAction) then
            dec(lvl);

          {$IFDEF SynEditMarkupFoldColoringDebug}
          //if (fLastOpenNode.LineIndex >= 0) then
          //  DebugLn('   %s %s - %s %s', [FoldTypeToStr(fLastOpenNode.FoldType), IfThen(sfaOutlineKeepLevel in fLastOpenNode.FoldAction, '(Keep)', ''), FoldTypeToStr(lCurNode.FoldType), IfThen(sfaOutlineKeepLevel in lCurNode.FoldAction, '(Keep)', '')]);
          {$ENDIF}

          { do not keep level if two consecutive sfaOutlineKeepLevel nodes are
            on different lines and start on different columns                  }
          if (fLastOpenNode.LineIndex >= 0)
          and (sfaOutlineKeepLevel in fLastOpenNode.FoldAction) then begin
            {$IFDEF SynEditMarkupFoldColoringDebug}
            //DebugLn('    keep');
            {$ENDIF}
            lKeepLevel := True;
            if (sfaOutlineKeepLevel in lCurNode.FoldAction)
            and not (fLastOpenNode.LineIndex = lLineIdx)
            and not (fFoldColorInfos[fLastIndex].PhysCol = lNodeCol) then begin
              inc(lvl);
              lKeepLevel := False;
            end;

          end else
            lKeepLevel := False;

          if AddHighlight then begin
            if lKeepLevel then begin
              // overwrite first character column with new value
              if fFoldColorInfos[fLastOpenIndex].PhysX < FColumnCache[fFoldColorInfos[lCurIndex].SrcNode.LineIndex] then begin
                FColumnCache[fFoldColorInfos[lCurIndex].SrcNode.LineIndex] := fFoldColorInfos[fLastOpenIndex].PhysX;
                Assert(fFoldColorInfos[lCurIndex].SrcNode.LineIndex = lLineIdx, 'fFoldColorInfos[lCurIndex].SrcNode.LineIndex <> lLineIdx');
                lNodeCol := FColumnCache[lLineIdx]
              end;
              {$IFDEF SynEditMarkupFoldColoringDebug}
              //with FFoldColorInfos[lCurIndex] do
              //  DebugLn('  > > > %.2d %.2d-%.2d: %d - %s %.5d:%s', [PhysCol, PhysX, PhysX2, Level, IfThen(sfaClose in SrcNode.FoldAction, 'C ', IfThen(sfaOpen in SrcNode.FoldAction, 'O ', '??')),ToPos(SrcNode.LineIndex),FoldTypeToStr(SrcNode.FoldType)]);
              {$ENDIF}
            end;

            if not (sfaOutlineKeepLevel in lCurNode.FoldAction) then
              inc(lvl);

            lvlA := lvl;
            fLastIndex := lCurIndex;
            fLastOpenNode := lCurNode;
            fLastOpenIndex := lCurIndex;

            with fFoldColorInfos[fFoldColorInfosCount - 1] do begin
              LevelAfter  := lvlA;
            end;
          end;
        end else if sfaClose in lCurNode.FoldAction then begin
          lKeepLevel := False;
          for j := fFoldColorInfosCount - 1 downto 0 do begin
            with fFoldColorInfos[j].SrcNode do begin
              if (FoldType = lCurNode.FoldType)
              and (FoldGroup = lCurNode.FoldGroup)
              and (sfaOpen in FoldAction)
              and (NestLvlEnd = lCurNode.NestLvlStart) then begin
                lvl := fFoldColorInfos[j].Level;
                lvlA := fFoldColorInfos[j].LevelAfter;
                if AddHighlight then begin
                  fLastIndex := lCurIndex;

                  with fFoldColorInfos[fFoldColorInfosCount - 1] do begin
                    LevelAfter  := lvlA;
                  end;
                  // if found opening position is behind closing position:
                  // delete this as it does not have to be drawn
                  if fFoldColorInfos[j].PhysX > fFoldColorInfos[fFoldColorInfosCount - 1].PhysX then begin
                    for k := j to fFoldColorInfosCount - 1 - 1 do begin
                      fFoldColorInfos[k] := fFoldColorInfos[k+1];
                    end;
                    dec(fFoldColorInfosCount);
                  end;
                end;
                break;
              end;
            end;
          end;
        end;
      end;
      inc(i);
    until i >= lNodeList.Count;
  finally
    lNodeList.ReleaseReference;
  end;
end;

function TSynEditMarkupFoldColors.GetColor(pIndex: Integer): TSynSelectedColor;
begin
  Assert((pIndex >= 0) and (pIndex < FColorCount), 'Index out of range');
  Result := fMarkupColors[pIndex];
end;

function TSynEditMarkupFoldColors.GetLineColor(pIndex: Integer
  ): TMarkupFoldColorsLineColor;
begin
  Assert((pIndex >= 0) and (pIndex < FColorCount), 'Index out of range');
  Result := fLineColors[pIndex];
end;

procedure TSynEditMarkupFoldColors.SetColorCount(AValue: Integer);
var
  i: Integer;
begin
  if FColorCount = AValue then Exit;

  for i := AValue to FColorCount - 1 do begin
    fMarkupColors[i].Free;
    fLineColors[i].Free;
  end;

  SetLength(fMarkupColors, AValue);
  SetLength(fLineColors, AValue);

  for i := FColorCount to AValue - 1 do begin
    fMarkupColors[i] := TSynSelectedColor.Create;
    fMarkupColors[i].Clear;
    fMarkupColors[i].OnChange := @ColorChanged;
    fLineColors[i] := TMarkupFoldColorsLineColor.Create;
    fLineColors[i].OnChange := @ColorChanged;
  end;

  FColorCount := AValue;
end;

procedure TSynEditMarkupFoldColors.PrepareMarkupForRow(pRow: Integer);
var
  i, lLastX, j: Integer;

begin
  if not Assigned(fHighlighter)
  and not (TCustomSynEdit(Self.SynEdit).Highlighter is TSynCustomFoldHighlighter) then
    exit;

  {$IFDEF SynEditMarkupFoldColoringDebug}
  //DebugLn(#10'PrepareMarkupForRow %d', [aRow]);
  {$ENDIF}

  fPreparedRow := pRow;
  fFoldColorInfosCount := 0; //reset needed to prevent using of invalid area

  // invalidate LastNode
  fLastIndex := -1;
  fLastOpenNode.LineIndex := -1;
  fLastOpenIndex := -1;

  {$IFDEF SynEditMarkupFoldColoringDebug}
  //DebugLn('  ----- DoMarkupParentFoldAtRow ------');
  {$ENDIF}
  DoMarkupParentFoldAtRow(pRow);

  {$IFDEF SynEditMarkupFoldColoringDebug}
  //DebugLn('  --- DoMarkupParentCloseFoldAtRow ---');
  {$ENDIF}
  DoMarkupParentCloseFoldAtRow(pRow);

  // delete parents with bigger x
  // to keep out mis indented blocks
  lLastX := MaxInt;
  for i := fFoldColorInfosCount - 1 downto 0 do begin
    if fFoldColorInfos[i].PhysX > lLastX then begin
      for j := i to length(fFoldColorInfos) - 2 do begin
        fFoldColorInfos[j] := fFoldColorInfos[j + 1];
      end;
      dec(fFoldColorInfosCount);
    end;
    lLastX := fFoldColorInfos[i].PhysX;
  end;
  {$IFDEF SynEditMarkupFoldColoringDebug}
  //DebugLn('  -------------- Final ---------------');
  //for i := 0 to FFoldColorInfosCount - 1 do with FFoldColorInfos[i] do begin
  //  DebugLn('  %.5d %.2d %.2d-%.2d: %d - %s %.5d:%s - %d %s', [Row, PhysCol, PhysX, PhysX2, Level, IfThen(sfaClose in SrcNode.FoldAction, 'C ', IfThen(sfaOpen in SrcNode.FoldAction, 'O ', '??')),ToPos(SrcNode.LineIndex),FoldTypeToStr(SrcNode.FoldType), ColorIdx, IfThen(Ignore, 'Ignore', '')]);
  //end;
  {$ENDIF}
end;

procedure TSynEditMarkupFoldColors.SetDefaultGroup(pValue: integer);
begin
  if fDefaultGroup = pValue then Exit;
  fDefaultGroup := pValue;
  fNestList.FoldGroup := fDefaultGroup;
end;

procedure TSynEditMarkupFoldColors.SetFoldColorInfosCount(pNewCount: Integer);
begin
  if pNewCount > fFoldColorInfosCapacity then begin
    // expand array
    fFoldColorInfosCapacity := pNewCount + 49;
    SetLength(fFoldColorInfos, fFoldColorInfosCapacity);
  end;
  fFoldColorInfosCount := pNewCount;
end;

procedure TSynEditMarkupFoldColors.InitNestList;
begin
  if Assigned(fNestList) then
    fNestList.Lines := Lines;
  if Assigned(fNestList2) then
    fNestList2.Lines := Lines;
end;

procedure TSynEditMarkupFoldColors.DoTextChanged(pStartLine, pEndLine, pCountDiff: Integer);
var
  lNode: TSynFoldNodeInfo;
  lNodeIdx, lEndLine, lLineIdx, lBottomLine, lDecreaseCount, lOuterNodeIdx: Integer;
  nl: LongInt;
  x: TColumnCacheEntry;
  {$IFDEF SynEditMarkupFoldColoringDebug}
  t: QWord;
  {$ENDIF}
begin
  if not Enabled then
    exit;

  {$IFDEF SynEditMarkupFoldColoringDebug}
  //DebugLn('   DoTextChanged %d-%d: %d', [StartLine, EndLine, ACountDiff]);
  {$ENDIF}

  // lines available?
  if Lines.Count = 0 then
    exit;

  // called by accident
  if pStartLine = 0 then
    exit;

  // no TSynCustomFoldHighlighter
  if not Assigned(fHighlighter) then
    exit;

  fHighlighter.CurrentLines := Lines;
  // highlighter still scanning
  if fHighlighter.NeedScan then
    exit;

  {$IFDEF SynEditMarkupFoldColoringDebug}
  t := GetTickCount64;
  {$ENDIF}

  if pEndLine < 0 then
    pEndLine := pStartLine
  else
    // pEndLine seems to be the first line after the change
    pEndLine := pEndLine - 1;
  lEndLine := pEndLine;
  FColumnCache[ToIdx(lEndLine)] := FirstCharacterColumn[ToIdx(lEndLine)];
  x := FColumnCache[ToIdx(lEndLine)];
  lBottomLine := TCustomSynEdit(SynEdit).TopLine + TCustomSynEdit(SynEdit).LinesInWindow;

  fNestList.Clear;
  fNestList2.Clear;
  lLineIdx := ToIdx(pStartLine);
  fNestList.Line := lLineIdx;
  lNodeIdx := fNestList.Count - 1;
  lOuterNodeIdx := -1;
  if lNodeIdx >= 0 then begin
    lDecreaseCount := 2;
    while (lNodeIdx >= 0)
    and (lDecreaseCount > 0) do begin
      dec(lDecreaseCount);
      while lNodeIdx >= 0 do begin
        dec(lNodeIdx);
        lNode := fNestList.HLNode[lNodeIdx];
        if not (sfaInvalid in lNode.FoldAction)
        and (sfaOutline in lNode.FoldAction)
        and not (sfaOutlineKeepLevel in lNode.FoldAction)
        and not (
          (sfaOpen in lNode.FoldAction)
          and (lNode.LineIndex = lLineIdx)
        ) then begin
          if lNodeIdx >= 0 then
            lOuterNodeIdx := lNodeIdx;
          break;
        end;
      end;
    end;
  end;
  if (lOuterNodeIdx >= 0) then begin
    lEndLine := ToPos(fNestList.NodeEndLine[lOuterNodeIdx]);
  end else begin
    // if there is no outer Outline:
    // expand lEndline if x is left to FirstCharacterColumn;
    lLineIdx := ToIdx(lEndLine);
    while x <= FirstCharacterColumn[lLineIdx] do begin
      // if x (FirstCharacterColoum of pStartLine) is left or equal to
      // the FirstCharacterColumn of line lLineIdx
      // then the real pEndLine is at the pEndLine of the lines last node
      fNestList.Line := lLineIdx;
      if fNestList.Count = 0 then
        break;
      nl := fNestList.NodeEndLine[fNestList.Count - 1];
      if nl = lLineIdx then
        break;
      {$IFDEF SynEditMarkupFoldColoringDebug}
      DebugLn('   %d -> %d [%d/%d]', [lLineIdx, nl, x, FirstCharacterColumn[nl]]);
      {$ENDIF}
      lLineIdx := nl;
    end;
    lLineIdx := ToPos(lLineIdx);
    lEndLine := Max(lEndLine, lLineIdx);
  end;

  // invalidate cache
  FColumnCache.LineTextChanged(ToIdx(pStartLine), Max(pEndLine, lEndLine) - pStartLine);

  if lEndLine > pEndLine then begin
    {$IFDEF SynEditMarkupFoldColoringDebug}
    //DebugLn('   InvalidateSynLines(%d, %d)', [EndLine + 1, lEndLine]);
    {$ENDIF}
    InvalidateSynLines(pEndLine + 1 , Min(lEndLine, lBottomLine));
  end;

  {$IFDEF SynEditMarkupFoldColoringDebug}
  DebugLn('*** DoTextChanged %d-%d-%d-%d duration=%d', [pStartLine, pEndLine, lEndLine, lBottomLine, GetTickCount64 - t]);
  {$ENDIF}

end;

procedure TSynEditMarkupFoldColors.SetLines(const pValue: TSynEditStrings);
var
  old: TSynEditStrings;
begin
  if Lines <> nil then
    Lines.Ranges[Self] := nil;
  if Enabled then begin
    old := Lines;
    if Assigned(old)
    and (pValue <> old) then begin
      // change:
      // remove Changehandler
      old.RemoveChangeHandler(senrHighlightChanged, @HighlightChanged);
      old.RemoveNotifyHandler(senrTextBufferChanged, @TextBufferChanged);
      FColumnCache.Invalidate;
    end;
  end;
  inherited SetLines(pValue);
  if Enabled then begin
    if (pValue <> old) then begin
      // change:
      if Assigned(pValue) then begin
        // add Changehandler
        pValue.AddChangeHandler(senrHighlightChanged, @HighlightChanged);
        pValue.AddNotifyHandler(senrTextBufferChanged, @TextBufferChanged);
        InitNestList;
      end else begin
        if Assigned(fNestList) then
          fNestList.Lines := nil;
        if Assigned(fNestList2) then
          fNestList2.Lines := nil;
        //DebugLn('*** SetLines');
      end;
    end;
  end;
  if Lines <> nil then begin
    FColumnCache.Capacity := Lines.Capacity;
    FColumnCache.Count := Lines.Count;
    Lines.Ranges[Self] := FColumnCache;
  end;
  FColumnCache.Invalidate;
end;

procedure TSynEditMarkupFoldColors.HighlightChanged(pSender: TSynEditStrings;
  pIndex, pCount: Integer);
var
  newHighlighter: TSynCustomHighlighter;
begin
  {$IFDEF SynEditMarkupFoldColoringDebug}
  //DebugLn('   HighlightChanged: aIndex=%d aCount=%d', [aIndex, aCount]);
  {$ENDIF}

  if (pIndex <> -1)
  or (pCount <> -1) then
    exit;

  newHighlighter := TCustomSynEdit(self.SynEdit).Highlighter;
  if Assigned(newHighlighter)
  and not (newHighlighter is TSynCustomFoldHighlighter) then
    newHighlighter := nil;

  if (newHighlighter = fHighlighter) then
    exit;

  fHighlighter := TSynCustomFoldHighlighter(newHighlighter);

  fNestList.HighLighter := fHighlighter;
  fNestList2.HighLighter := fHighlighter;

  if not Enabled then
    exit;

  FColumnCache.Invalidate;
end;

procedure TSynEditMarkupFoldColors.DoEnabledChanged(pSender: TObject);
begin
  if Enabled = fLastEnabled then
    exit;
  fLastEnabled := Enabled;
  if fLastEnabled then begin
    {$IFDEF SynEditMarkupFoldColoringDebug}
    //DebugLn('   *** TSynEditMarkupFoldColors Enabled');
    {$ENDIF}
    if Assigned(Lines) then begin
      // add Changehandler
      Lines.AddChangeHandler(senrHighlightChanged, @HighlightChanged);
      Lines.AddNotifyHandler(senrTextBufferChanged, @TextBufferChanged);
      InitNestList;
    end;
  end else begin
    {$IFDEF SynEditMarkupFoldColoringDebug}
    //DebugLn('   *** TSynEditMarkupFoldColors Disabled');
    {$ENDIF}
    if Assigned(Lines) then begin
      // remove Changehandler
      Lines.RemoveChangeHandler(senrHighlightChanged, @HighlightChanged);
      Lines.RemoveNotifyHandler(senrTextBufferChanged, @TextBufferChanged);
      FColumnCache.Invalidate;
    end;
  end;
  if Assigned(Lines) then
    InvalidateSynLines(1, Lines.Count);
end;

procedure TSynEditMarkupFoldColors.ColorChanged(pMarkup: TObject);
begin
  if Assigned(Lines) then
    InvalidateSynLines(1, Lines.Count);
end;

end.


