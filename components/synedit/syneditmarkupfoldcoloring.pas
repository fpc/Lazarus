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
  LCLProc, LCLType, SynEditFoldedView, SynEditHighlighter,
  SynEditHighlighterFoldBase, LazSynEditText
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
    Row, PhysX, PhysX2, PhysCol: Integer;
    ColorIdx: Integer;
    Border  : Boolean;
    Ignore  : Boolean; //no color no line
    SrcNode : TSynFoldNodeInfo;
    LevelBefore, Level, LevelAfter : integer; //needed by non nest nodes
  end;

  TMarkupFoldColorInfos = array of TMarkupFoldColorInfo;
  TSynFoldNodeInfos     = array of TSynFoldNodeInfo; //for quick compare detection

  { TSynEditMarkupFoldColors }

  TSynEditMarkupFoldColors = class(TSynEditMarkup)
  private
    {$IFDEF WithSynMarkupFoldColorDebugGutter}
    FDebugGutter: TIDESynMarkupFoldColorDebugGutter;
    {$ENDIF}
    fUpdateColors: Boolean;
    function GetFirstCharacterColumn(pIndex: Integer): Byte;
    procedure TextBufferChanged(pSender: TObject);
  private
    fHighlighter: TSynCustomFoldHighlighter;
    fMarkupColors: array of TSynSelectedColor;
    fNestList: TLazSynEditNestedFoldsList;

    // cache
    fFirstCharacterPhysColCache: Array of Byte;
    fCacheCount,
    fCacheCapacity,
    fFoldColorInfosCount,
    fFoldColorInfosCapacity: Integer;

    fDefaultGroup: integer;
    fFoldColorInfos: TMarkupFoldColorInfos;

    fColors : array of TColor;
    fPreparedRow: integer;
    fLastNode,
    fLastOpenNode: TSynFoldNodeInfo;
    fLastIndex,
    fLastOpenIndex: Integer;
    fLastEnabled: Boolean;

    fFirstInvalidCacheLine, fLastInvalidCacheLine: integer;
    fUpdateCache: Boolean;

    procedure DoMarkupParentFoldAtRow(pRow: Integer);
    procedure DoMarkupParentCloseFoldAtRow(pRow: Integer);
    function GetColor(pIndex: Integer): TSynSelectedColor;
    procedure SetDefaultGroup(pValue: integer);
    procedure SetCacheCount(pNewCount: Integer);
    procedure SetFoldColorInfosCount(pNewCount: Integer);
    procedure InitCache;
    procedure ClearCache;
    procedure UpdateCache;
    procedure UpdateCacheRange(pFrom, pTo: Integer);
    procedure UpdateColors;
    property FirstCharacterColumn[pIindex: Integer]: Byte read GetFirstCharacterColumn;
  protected
    // Notifications about Changes to the text
    procedure DoTextChanged({%H-}pStartLine, pEndLine, {%H-}pCountDiff: Integer); override; // 1 based
    procedure SetLines(const pValue: TSynEditStrings); override;
    procedure LinesChanged(pSender: TSynEditStrings; pIndex, pCount: Integer);
    procedure HighlightChanged(pSender: TSynEditStrings; pIndex, pCount: Integer);
    procedure DoEnabledChanged(pSender: TObject); override;
    procedure ColorChanged(pMarkup: TObject);
  public
    constructor Create(pSynEdit : TSynEditBase);
    destructor Destroy; override;
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
    property Color[pIndex: Integer]: TSynSelectedColor read GetColor;
  end;

implementation
uses
  SynEdit,
  SynEditTypes,
  SynEditMiscProcs,
  {$IFDEF SynEditMarkupFoldColoringDebug}
  SynHighlighterPas,
  strutils,
  {$endif}
  Dialogs;

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
           + IntToStr(Level) + ',' + IntToStr(LevelBefore) + ',' + IntToStr(LevelAfter)
           + ') ';
      while length(s) < 21 * (j+1) do s := s + ' ';
      end;
      s := IntToStr(FOwner.fFoldColorInfosCount) + s;
      if iLine < length(FOwner.fFirstCharacterPhysColCache) then
        s := s + ', '+IntToStr(FOwner.fFirstCharacterPhysColCache[ToIdx(iLine)]);

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
var
  i: Integer;
begin
  inherited Create(pSynEdit);

  {$IFDEF WithSynMarkupFoldColorDebugGutter}
  FDebugGutter := TIDESynMarkupFoldColorDebugGutter.Create(TSynEdit(pSynEdit).RightGutter.Parts);
  FDebugGutter.FOwner := Self;
  {$ENDIF}

  fCacheCapacity := 0;
  SetCacheCount(100);
  fFirstInvalidCacheLine := -1;
  fLastInvalidCacheLine := -1;

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

  SetLength(fMarkupColors, 10);
  for i := 0 to length(fMarkupColors) - 1 do begin
    fMarkupColors[i] := TSynSelectedColor.Create;
    fMarkupColors[i].OnChange := @ColorChanged;
  end;

  MarkupInfo.Foreground := clGreen;
  MarkupInfo.Background := clNone;
  MarkupInfo.Style := [];
  MarkupInfo.StyleMask := [];
  MarkupInfo.FrameEdges:= sfeLeft;

  SetLength(fColors, 6);
  fMarkupColors[0].Foreground  := clRed;
  fMarkupColors[1].Foreground  := $000098F7; //orange
  fMarkupColors[2].Foreground  := $0022CC40; //green
  fMarkupColors[3].Foreground  := $00CCCC00; //cyan
  fMarkupColors[4].Foreground  := $00FF682A; //blue
  fMarkupColors[5].Foreground  := $00CF00C4; //purple
  fMarkupColors[6].Foreground  := clNone;
  fMarkupColors[7].Foreground  := clNone;
  fMarkupColors[8].Foreground  := clNone;
  fMarkupColors[9].Foreground := clNone;

end;

destructor TSynEditMarkupFoldColors.Destroy;
var
  i: Integer;
begin
  for i := 0 to Length(fMarkupColors) - 1 do
    fMarkupColors[i].Free;
  if Assigned(Lines) then begin
    Lines.RemoveChangeHandler(senrLineCount, @LinesChanged);
    Lines.RemoveChangeHandler(senrHighlightChanged, @HighlightChanged);
    Lines.RemoveNotifyHandler(senrTextBufferChanged, @TextBufferChanged);
  end;
  FreeAndNil(fNestList);
  inherited Destroy;
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
  if fUpdateColors then
    UpdateColors;
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

    x2both := -3;
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
          if x2both = -3 then begin //first call flag
            MarkupInfo.FrameColor:= clNone;
            MarkupInfo.Foreground:= clNone;
            MarkupInfo.Background:= clNone;
            MarkupInfo.FrameEdges:= sfeNone;
            x2both := 0;
          end;

          Result := MarkupInfo;
          x2both := max(x2both, PhysX2);
          MarkupInfo.SetFrameBoundsPhys(PhysX, x2both);
          if Border then begin
            MarkupInfo.FrameColor := fColors[ColorIdx];
            MarkupInfo.FrameEdges := sfeLeft;
          end else begin
            MarkupInfo.FrameColor := clNone;
            MarkupInfo.FrameEdges := sfeNone;
            MarkupInfo.Foreground := fColors[ColorIdx];
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
      if not Ignore and (ColorIdx >= 0) and (PhysX < PhysX2) and (pStartCol.Physical < PhysX) and (pStartCol.Physical <= PhysX2) then begin
        pNextPhys := fFoldColorInfos[i].PhysX;
        break;
      end;
    end;
end;

function TSynEditMarkupFoldColors.GetFirstCharacterColumn(pIndex: Integer): Byte;
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
  if (p > 255) or (p > s) then p := 255;
  Result := Min(TCustomSynEdit(SynEdit).LogicalToPhysicalPos(Point(p, toPos(pIndex))).x, 255);
end;

procedure TSynEditMarkupFoldColors.TextBufferChanged(pSender: TObject);
begin
  if not Enabled then
    exit;
  InitCache;
end;

procedure TSynEditMarkupFoldColors.DoMarkupParentFoldAtRow(pRow: Integer);
var
  lNodeCol: Byte;
  i, lLvl, lLineIdx, lCurIndex: Integer;
  lCurNode: TSynFoldNodeInfo;

  procedure AddVerticalLine;
  begin
    if fFirstCharacterPhysColCache[lCurNode.LineIndex] = 0 then
      fFirstCharacterPhysColCache[lCurNode.LineIndex] := FirstCharacterColumn[lCurNode.LineIndex];
    lNodeCol := fFirstCharacterPhysColCache[lCurNode.LineIndex];

    with fFoldColorInfos[lCurIndex] do begin
      SrcNode:= lCurNode; //needed by close node
      PhysCol := lNodeCol;
      Row  := pRow;
      PhysX := lNodeCol;
      PhysX2 := PhysX + 1;
      Border := PhysX < GetFirstCharacterColumn(lLineIdx); // use real one here not cache
      Ignore :=
        (Border and (sfaOutlineNoLine in lCurNode.FoldAction))
        or (not Border);
      Level := lLvl;
      ColorIdx := Max(0, lLvl) mod (length(fColors));
      {$IFDEF SynEditMarkupFoldColoringDebug}
      //DebugLn('  %.5d %.2d %.2d-%.2d: %d - %s %.5d:%s', [Row, PhysCol, PhysX, PhysX2, Level, IfThen(sfaClose in SrcNode.FoldAction, 'C ', IfThen(sfaOpen in SrcNode.FoldAction, 'O ', '??')),ToPos(SrcNode.LineIndex),FoldTypeToStr(SrcNode.FoldType)]);
      {$ENDIF}
    end;
  end;

var
  lvlB, lvlA: Integer;
  lKeepLevel: Boolean;

begin
  lLineIdx := ToIdx(pRow);
  fNestList.Line := lLineIdx;
  fHighlighter.CurrentLines := Lines;

  // get first character for current line
  if fFirstCharacterPhysColCache[lLineIdx] = 0 then
    fFirstCharacterPhysColCache[lLineIdx] := FirstCharacterColumn[lLineIdx];

  lLvl := 0;
  i := 0;
  while i < fNestList.Count do begin
    lCurNode := fNestList.HLNode[i];
    // sanity check
    Assert(fCacheCapacity > lCurNode.LineIndex, 'TSynEditMarkupFoldColors.DoMarkupParentFoldAtRow: cache arrays too small');
    Assert(sfaOpen in lCurNode.FoldAction, 'no sfaOpen in lCurNode.FoldAction');
    {$IFDEF SynEditMarkupFoldColoringDebug}
    //DebugLn('  O: %s %s %s', [IfThen(sfaOutline in lCurNode.FoldAction, 'X', '-'), IfThen(sfaClose in lCurNode.FoldAction, 'C ', IfThen(sfaOpen in lCurNode.FoldAction, 'O ', '??')),FoldTypeToStr(lCurNode.FoldType)]);
    {$ENDIF}
    if (sfaOutline in lCurNode.FoldAction)
    and not (sfaInvalid in lCurNode.FoldAction)
    and (lCurNode.LineIndex <> lLineIdx) then begin
      lvlB := lLvl;

      if ( sfaOutlineForceIndent in lCurNode.FoldAction) then
        inc(lLvl)
      else if ( sfaOutlineMergeParent in lCurNode.FoldAction) then
        dec(lLvl);

      if fFirstCharacterPhysColCache[lCurNode.LineIndex] = 0 then
        fFirstCharacterPhysColCache[lCurNode.LineIndex] := FirstCharacterColumn[lCurNode.LineIndex];
      lNodeCol := fFirstCharacterPhysColCache[lCurNode.LineIndex];

      // new FoldColorInfo
      SetFoldColorInfosCount(fFoldColorInfosCount + 1);
      lCurIndex := fFoldColorInfosCount - 1;

      {$IFDEF SynEditMarkupFoldColoringDebug}
      //if (fLastOpenNode.LineIndex >= 0) then
      //  DebugLn('   %s %s - %s %s', [FoldTypeToStr(fLastOpenNode.FoldType), IfThen(sfaOutlineKeepLevel in fLastOpenNode.FoldAction, '(Keep)', ''), FoldTypeToStr(lCurNode.FoldType), IfThen(sfaOutlineKeepLevel in lCurNode.FoldAction, '(Keep)', '')]);
      {$ENDIF}

      { do not keep level if two consecutive sfaOutlineKeepLevel nodes are
        on different lines and start on different columns                  }
      if (fLastNode.LineIndex >= 0)
      and (sfaOutlineKeepLevel in fLastNode.FoldAction) then begin
        {$IFDEF SynEditMarkupFoldColoringDebug}
        //DebugLn('   %.5d/%.5d %.2d/%.2d', [fLastNode.LineIndex+1,lCurNode.LineIndex+1, FFoldColorInfos[fLastIndex].PhysCol, lNodeCol]);
        //DbgOut('    keep');
        {$ENDIF}
        lKeepLevel := True;
        if (sfaOutlineKeepLevel in lCurNode.FoldAction)
        and not (fLastNode.LineIndex = lCurNode.LineIndex)
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
        fFoldColorInfos[lCurIndex].ColorIdx := Max(0, lLvl) mod (length(fColors));
        // overwrite first character column with new value
        if fFoldColorInfos[fLastIndex].PhysX < fFirstCharacterPhysColCache[fFoldColorInfos[lCurIndex].SrcNode.LineIndex] then
          fFirstCharacterPhysColCache[fFoldColorInfos[lCurIndex].SrcNode.LineIndex] := fFoldColorInfos[fLastIndex].PhysX;
        {$IFDEF SynEditMarkupFoldColoringDebug}
        //with FFoldColorInfos[lCurIndex] do
        //  DebugLn('  > > > %.2d %.2d-%.2d: %d - %s %.5d:%s', [PhysCol, PhysX, PhysX2, Level, IfThen(sfaClose in SrcNode.FoldAction, 'C ', IfThen(sfaOpen in SrcNode.FoldAction, 'O ', '??')),ToPos(SrcNode.LineIndex),FoldTypeToStr(SrcNode.FoldType)]);
        {$ENDIF}
      end;

      if not (sfaOutlineKeepLevel in lCurNode.FoldAction) then
        inc(lLvl);

      fLastNode := lCurNode;
      fLastIndex := lCurIndex;
      fLastOpenNode := lCurNode;
      fLastOpenIndex := lCurIndex;

      lvlA := lLvl;

      with fFoldColorInfos[fFoldColorInfosCount - 1] do begin
        LevelBefore := lvlB;
        LevelAfter  := lvlA;
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
  lNodeCol: Byte;

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
        Row := pRow; //ToPos(lCurNode.LineIndex);
        PhysX := lPhysX;
        //if fFirstCharacterPhysColCache[lCurNode.LineIndex] = 0 then
        //  fFirstCharacterPhysColCache[lCurNode.LineIndex] := FirstCharacterColumn[lCurNode.LineIndex];
        PhysCol := lNodeCol; //fFirstCharacterPhysColCache[lCurNode.LineIndex];
        PhysX2 := lPhysX2;
        Level := lvl;
        lMaxLevel := Max(lMaxLevel, lvl);
        if not (sfaOutlineNoColor in lCurNode.FoldAction) then
           ColorIdx := Max(0, lvl) mod (length(fColors))
        else
           ColorIdx := -1;

        {$IFDEF SynEditMarkupFoldColoringDebug}
        //DebugLn('  %.5d %.2d %.2d-%.2d: %d - %s %.5d:%s - %s', [Row, PhysCol, PhysX, PhysX2, Level, IfThen(sfaClose in SrcNode.FoldAction, 'C ', IfThen(sfaOpen in SrcNode.FoldAction, 'O ', '??')),ToPos(SrcNode.LineIndex),FoldTypeToStr(SrcNode.FoldType), IfThen(lKeepLevel, 'Keep', '')]);
        {$ENDIF}
      end;
    end;
  end;

var
  lLineIdx,i,j,lvlB,lvlA , k: integer;
  lNodeList: TLazSynFoldNodeInfoList;

begin
  lLineIdx := ToIdx(pRow);

  // as all nodes will be on pRow we can set lNodeCol here already
  if fFirstCharacterPhysColCache[lLineIdx] = 0 then
    fFirstCharacterPhysColCache[lLineIdx] := FirstCharacterColumn[lLineIdx];
  lNodeCol := fFirstCharacterPhysColCache[lLineIdx];

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
      Assert(fCacheCapacity > lCurNode.LineIndex, 'TSynEditMarkupFoldColors.DoMarkupParentFoldAtRow: cache arrays too small');

      {$IFDEF SynEditMarkupFoldColoringDebug}
      //if not (sfaInvalid in lCurNode.FoldAction) then
      //  DebugLn('  C: %s %s %s', [IfThen(sfaOutline in lCurNode.FoldAction, 'X', '-'), IfThen(sfaClose in lCurNode.FoldAction, 'C ', IfThen(sfaOpen in lCurNode.FoldAction, 'O ', '??')),FoldTypeToStr(lCurNode.FoldType)]);
      {$ENDIF}

      if not (sfaInvalid in lCurNode.FoldAction)
      and (sfaOutline in lCurNode.FoldAction) then begin
        if sfaOpen in lCurNode.FoldAction then begin
          lvlB := lvl;

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
              if fFoldColorInfos[fLastOpenIndex].PhysX < fFirstCharacterPhysColCache[fFoldColorInfos[lCurIndex].SrcNode.LineIndex] then begin
                fFirstCharacterPhysColCache[fFoldColorInfos[lCurIndex].SrcNode.LineIndex] := fFoldColorInfos[fLastOpenIndex].PhysX;
                Assert(fFoldColorInfos[lCurIndex].SrcNode.LineIndex = lLineIdx, 'fFoldColorInfos[lCurIndex].SrcNode.LineIndex <> lLineIdx');
                lNodeCol := fFirstCharacterPhysColCache[lLineIdx]
              end;
              {$IFDEF SynEditMarkupFoldColoringDebug}
              //with FFoldColorInfos[lCurIndex] do
              //  DebugLn('  > > > %.2d %.2d-%.2d: %d - %s %.5d:%s', [PhysCol, PhysX, PhysX2, Level, IfThen(sfaClose in SrcNode.FoldAction, 'C ', IfThen(sfaOpen in SrcNode.FoldAction, 'O ', '??')),ToPos(SrcNode.LineIndex),FoldTypeToStr(SrcNode.FoldType)]);
              {$ENDIF}
            end;

            if not (sfaOutlineKeepLevel in lCurNode.FoldAction) then
              inc(lvl);

            lvlA := lvl;
            fLastNode := lCurNode;
            fLastIndex := lCurIndex;
            fLastOpenNode := lCurNode;
            fLastOpenIndex := lCurIndex;

            with fFoldColorInfos[fFoldColorInfosCount - 1] do begin
              LevelBefore := lvlB;
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
                lvlB := lvl;
                lvl := fFoldColorInfos[j].Level;
                lvlA := fFoldColorInfos[j].LevelAfter;
                if AddHighlight then begin
                  fLastNode := lCurNode;
                  fLastIndex := lCurIndex;

                  with fFoldColorInfos[fFoldColorInfosCount - 1] do begin
                    LevelBefore := lvlB;
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
  Assert((pIndex >= 0) and (pIndex < Length(fMarkupColors)), 'Index out of range');
  Result := fMarkupColors[pIndex];
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

  if fUpdateCache then
    UpdateCache;

  fPreparedRow := pRow;
  fFoldColorInfosCount := 0; //reset needed to prevent using of invalid area

  // invalidate fLastNode
  fLastNode.LineIndex := -1;
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

  // update Col if last parent open node is closed and reopened on current line
  for i := fFoldColorInfosCount - 1 downto 2 do begin
    if (sfaOpen in fFoldColorInfos[i-2].SrcNode.FoldAction)
    and (sfaClose in fFoldColorInfos[i-1].SrcNode.FoldAction)
    and (sfaOpen in fFoldColorInfos[i].SrcNode.FoldAction)
    and (fFoldColorInfos[i-2].SrcNode.FoldType = fFoldColorInfos[i-1].SrcNode.FoldType)
    and (fFoldColorInfos[i-1].SrcNode.FoldType = fFoldColorInfos[i].SrcNode.FoldType)
    and (fFoldColorInfos[i-1].PhysX = fFoldColorInfos[i].PhysX)
    and (fFoldColorInfos[i-1].PhysX2 = fFoldColorInfos[i].PhysX2)
    and (fFoldColorInfos[i-1].Row = pRow)
    and (fFoldColorInfos[i].Row = pRow) then begin
      fFirstCharacterPhysColCache[ToIdx(pRow)] := fFoldColorInfos[i-2].PhysCol;
    end;
  end;

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

procedure TSynEditMarkupFoldColors.SetCacheCount(pNewCount: Integer);
var
  i: Integer;
begin
  if pNewCount > fCacheCapacity then begin
    // expand array
    fCacheCapacity := pNewCount + 900;
    SetLength(fFirstCharacterPhysColCache, fCacheCapacity);
  end;
  if pNewCount > fCacheCount then begin
    // clear new section
    for i := fCacheCount to pNewCount - 1 do
      fFirstCharacterPhysColCache[i] := 0;
  end;
  fCacheCount := pNewCount;
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

procedure TSynEditMarkupFoldColors.InitCache;
begin
  if Assigned(fNestList) then
    fNestList.Lines := Lines;
  // set cache size
  SetCacheCount(Lines.Count);
end;

procedure TSynEditMarkupFoldColors.ClearCache;
var
  i: Integer;
begin
  for i := 0 to fCacheCount - 1 do
    fFirstCharacterPhysColCache[i] := 0;
  //DebugLn('*** ClearCache');
  UpdateCacheRange(0, fCacheCount - 1);
end;

procedure TSynEditMarkupFoldColors.UpdateCache;
var
  i, max, lTopLineIdx: Integer;
  lFrom, lTo, j: Integer;
begin
  // force calculating of fFirstCharacterPhysColCache only for lines
  // in front of TopLine
  fUpdateCache := False;
  lTopLineIdx := ToIdx(TCustomSynEdit(SynEdit).TopLine);
  max := Min(SynEdit.Lines.Count, length(fFirstCharacterPhysColCache)) - 1;

  if (fFirstInvalidCacheLine >= 0) and (fFirstInvalidCacheLine < lTopLineIdx) then
    for j := fFirstInvalidCacheLine to min(fLastInvalidCacheLine, lTopLineIdx) do begin
      if fFirstCharacterPhysColCache[j] = 0 then
        PrepareMarkupForRow(ToPos(j));
    end;

  fNestList.Clear;
  fFirstInvalidCacheLine := -1;
  fLastInvalidCacheLine := -1;
  //DebugLn('    Cache updated');
end;

procedure TSynEditMarkupFoldColors.UpdateCacheRange(pFrom, pTo: Integer);
var
  i: Integer;
begin
  if (fFirstInvalidCacheLine >= 0) then begin
    for i := pFrom to fFirstInvalidCacheLine - 1 do
      fFirstCharacterPhysColCache[i] := 0;
    for i := fLastInvalidCacheLine + 1 to pTo do
      fFirstCharacterPhysColCache[i] := 0;
  end
  else begin
    for i := pFrom to pTo do
      fFirstCharacterPhysColCache[i] := 0;
  end;

  if (fFirstInvalidCacheLine < 0) or (pFrom < fFirstInvalidCacheLine) then
    fFirstInvalidCacheLine := pFrom;
  if pTo > fLastInvalidCacheLine then
    fLastInvalidCacheLine := pTo;
  fUpdateCache := True;
end;

procedure TSynEditMarkupFoldColors.UpdateColors;
var
  c, i: Integer;

  procedure AddColor(pColor: TSynSelectedColor);
  begin
    if pColor.Foreground = clNone then exit;
    fColors[c] := pColor.Foreground;
    inc(c);
  end;

begin
  SetLength(fColors, Length(fMarkupColors));
  c := 0;
  for i := 0 to length(fMarkupColors) - 1 do
    AddColor(fMarkupColors[i]);
  if c = 0 then begin
    fColors[c] := $0000FF; // default red
    inc(c);
  end;
  SetLength(fColors, c);
  fUpdateColors := False;
end;

procedure TSynEditMarkupFoldColors.DoTextChanged(pStartLine, pEndLine, pCountDiff: Integer);
var
  lNode: TSynFoldNodeInfo;
  lNodeIdx, lEndLine, lLineIdx, lBottomLine, lDecreaseCount, lOuterNodeIdx: Integer;
  nl: LongInt;
  x: Byte;
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
  if fFirstCharacterPhysColCache[ToIdx(lEndLine)] = 0 then
    fFirstCharacterPhysColCache[ToIdx(lEndLine)] := FirstCharacterColumn[ToIdx(lEndLine)];
  x := fFirstCharacterPhysColCache[ToIdx(lEndLine)];
  lBottomLine := TCustomSynEdit(SynEdit).TopLine + TCustomSynEdit(SynEdit).LinesInWindow;

  fNestList.Clear;
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
  UpdateCacheRange(ToIdx(pStartLine), ToIdx(Max(pEndLine, lEndLine)));

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
  if Enabled then begin
    old := Lines;
    if Assigned(old)
    and (pValue <> old) then begin
      // change:
      // remove Changehandler
      old.RemoveChangeHandler(senrLineCount, @LinesChanged);
      old.RemoveChangeHandler(senrHighlightChanged, @HighlightChanged);
      old.RemoveNotifyHandler(senrTextBufferChanged, @TextBufferChanged);
      ClearCache;
    end;
  end;
  inherited SetLines(pValue);
  if Enabled then begin
    if (pValue <> old) then begin
      // change:
      if Assigned(pValue) then begin
        // add Changehandler
        pValue.AddChangeHandler(senrLineCount, @LinesChanged);
        pValue.AddChangeHandler(senrHighlightChanged, @HighlightChanged);
        pValue.AddNotifyHandler(senrTextBufferChanged, @TextBufferChanged);
        InitCache;
      end else begin
        // clear cache
        SetCacheCount(0);
        if Assigned(fNestList) then
          fNestList.Lines := nil;
        //DebugLn('*** SetLines');
      end;
    end;
  end;
end;

procedure TSynEditMarkupFoldColors.LinesChanged(pSender: TSynEditStrings;
                                        pIndex, pCount: Integer);
var
  absCount,
  idx, i, lCount: Integer;
begin
  if not Enabled then
    exit;

  {$IFDEF SynEditMarkupFoldColoringDebug}
  //DebugLn('   LinesChanged: aIndex=%d aCount=%d', [aIndex, aCount]);
  {$ENDIF}

  lCount := Min(Length(fFirstCharacterPhysColCache) - 1, pSender.Count);
  idx := ToIdx(pIndex);
  if (pCount < 0)
  and (idx >= 0) then begin
    // lines deleted
    absCount := Abs(pCount);
    for i := idx to lCount - absCount do
      fFirstCharacterPhysColCache[i] := fFirstCharacterPhysColCache[i + absCount];
  end;
  SetCacheCount(pSender.Count);
  if (pCount > 0) then begin
    if idx >= 0 then begin
      // lines added
      for i := lCount - pCount downto idx do
        fFirstCharacterPhysColCache[i + pCount] := fFirstCharacterPhysColCache[i];
      UpdateCacheRange(idx, Min(idx + pCount, Length(fFirstCharacterPhysColCache) - 1));
    end else begin
      // first lines will be inserted
      UpdateCacheRange(0, Length(fFirstCharacterPhysColCache) - 1);
    end;
    //DebugLn('*** LinesChanged');
  end;
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

  if not Enabled then
    exit;

  ClearCache;
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
      Lines.AddChangeHandler(senrLineCount, @LinesChanged);
      Lines.AddChangeHandler(senrHighlightChanged, @HighlightChanged);
      Lines.AddNotifyHandler(senrTextBufferChanged, @TextBufferChanged);
      InitCache;
    end;
  end else begin
    {$IFDEF SynEditMarkupFoldColoringDebug}
    //DebugLn('   *** TSynEditMarkupFoldColors Disabled');
    {$ENDIF}
    if Assigned(Lines) then begin
      // remove Changehandler
      Lines.RemoveChangeHandler(senrLineCount, @LinesChanged);
      Lines.RemoveChangeHandler(senrHighlightChanged, @HighlightChanged);
      Lines.RemoveNotifyHandler(senrTextBufferChanged, @TextBufferChanged);
      ClearCache;
    end;
  end;
  if Assigned(Lines) then
    InvalidateSynLines(1, Lines.Count);
end;

procedure TSynEditMarkupFoldColors.ColorChanged(pMarkup: TObject);
begin
  fUpdateColors := True;
  if Assigned(Lines) then
    InvalidateSynLines(1, Lines.Count);
end;

end.


