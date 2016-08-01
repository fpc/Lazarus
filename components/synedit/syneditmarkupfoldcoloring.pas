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


Known Issues:
  - wrong drawing vertical lines position when a line is mixed with tab char
  - poor configuration
  - no design time


-------------------------------------------------------------------------------}
unit SynEditMarkupFoldColoring;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,Graphics, SynEditMarkup, SynEditMiscClasses, Controls,
  LCLProc, SynEditFoldedView, SynEditHighlighter, SynEditHighlighterFoldBase;

type

  PMarkupFoldColorInfo = ^TMarkupFoldColorInfo;
  TMarkupFoldColorInfo = record
    Y, X, X2: Integer;
    ColorIdx: Integer;
    Border  : Boolean;
    Ignore  : Boolean; //no color no line
    SrcNode : TSynFoldNodeInfo;
    LevelBefore, LevelAfter : integer;//needed by non nest nodes
  end;

  TMarkupFoldColorInfos = array of TMarkupFoldColorInfo;
  TSynFoldNodeInfos     = array of TSynFoldNodeInfo; //for quick compare detection

  { TSynEditMarkupFoldColors }

  TSynEditMarkupFoldColors = class(TSynEditMarkup)
  private
    FNestList: TLazSynEditNestedFoldsList;
    FDefaultGroup: integer;
     // Physical Position
    FHighlights : TMarkupFoldColorInfos; //array of TMarkupFoldColorInfo;
    Colors : array of TColor;

    {%region invalidating}
    CurrentY : integer;  //??
    FCaretY : integer;    // flag identify for refresh begin______
    FPrevCaretText : string;  // flag identify for refresh begin______
    {%endregion}

    procedure DoMarkupParentFoldAtRow(aRow: Integer);
    procedure DoMarkupParentCloseFoldAtRow(aRow: Integer);

    function GetFoldHighLighter: TSynCustomFoldHighlighter;
    procedure SetDefaultGroup(AValue: integer);
  protected
    // Notifications about Changes to the text
    procedure DoTextChanged({%H-}StartLine, EndLine, {%H-}ACountDiff: Integer); override; // 1 based
    procedure DoCaretChanged(Sender: TObject); override;
  public
    constructor Create(ASynEdit : TSynEditBase);
    destructor Destroy; override;
    function GetMarkupAttributeAtRowCol(const aRow: Integer;
                                        const aStartCol: TLazSynDisplayTokenBound;
                                        const {%H-}AnRtlInfo: TLazSynDisplayRtlInfo): TSynSelectedColor; override;
    procedure GetNextMarkupColAfterRowCol(const aRow: Integer;
                                         const aStartCol: TLazSynDisplayTokenBound;
                                         const {%H-}AnRtlInfo: TLazSynDisplayRtlInfo;
                                         out   ANextPhys, ANextLog: Integer); override;

    procedure PrepareMarkupForRow(aRow : Integer); override;
    procedure EndMarkup; override;
    property DefaultGroup : integer read FDefaultGroup write SetDefaultGroup;
  end;

implementation
uses
  SynEdit,SynEditTypes, SynEditMiscProcs;

  {%region Sorting FoldInfo -fold}
  function CompareFI(Item1, Item2: Pointer): Integer;
  begin
    result := PMarkupFoldColorInfo(Item1)^.X - PMarkupFoldColorInfo(Item2)^.X;
    if result = 0 then
        result := PMarkupFoldColorInfo(Item1)^.X2 - PMarkupFoldColorInfo(Item2)^.X2;
    if result = 0 then
        result := (PMarkupFoldColorInfo(Item1)^.X2 - PMarkupFoldColorInfo(Item1)^.X)
          - (PMarkupFoldColorInfo(Item2)^.X2 - PMarkupFoldColorInfo(Item2)^.X);
  end;

  function SortLeftMostFI(a: TMarkupFoldColorInfos): TMarkupFoldColorInfos;
  var
    l : TFpList;
    i : integer;
  begin
    l := TFpList.Create;
    for i := 0 to Pred(Length(a)) do
      l.Add( PMarkupFoldColorInfo(@a[i]) );
    l.Sort(@CompareFI);

    SetLength(result, Length(a));
    for i := 0 to Pred(l.Count) do
      result[i] := PMarkupFoldColorInfo(l[i])^;
     l.Free;
  end;
  {%endregion}

{ TSynEditMarkupFoldColors }

constructor TSynEditMarkupFoldColors.Create(ASynEdit: TSynEditBase);
begin
  inherited Create(ASynEdit);

  FNestList := TLazSynEditNestedFoldsList.Create(Lines, GetFoldHighLighter);
  FNestList.ResetFilter;
  FNestList.FoldGroup := FDefaultGroup;//1;//FOLDGROUP_PASCAL;
  FNestList.FoldFlags :=  [sfbIncludeDisabled]; //[];//
  FNestList.IncludeOpeningOnLine := True; //False; //

  MarkupInfo.Foreground := clGreen;
  MarkupInfo.Background := clNone; //clFuchsia;
  MarkupInfo.Style := [];
  MarkupInfo.StyleMask := [];
  MarkupInfo.FrameEdges:= sfeLeft;//sfeAround;//sfeBottom;//

  SetLength(Colors, 5);
  Colors[0] := clRed;
  Colors[1] := $000098F7; //orange
  Colors[2] := $0022CC40; //green
  //Colors[3] := $00D5D500; // $0098CC42; // $00D1D54A; // teal
  Colors[3] := $00FF682A; //blue
  Colors[4] := $00CF00C4; //purple
end;

destructor TSynEditMarkupFoldColors.Destroy;
begin
  FreeAndNil(FNestList);
  inherited Destroy;
end;

function TSynEditMarkupFoldColors.GetMarkupAttributeAtRowCol(
  const aRow: Integer; const aStartCol: TLazSynDisplayTokenBound;
  const AnRtlInfo: TLazSynDisplayRtlInfo): TSynSelectedColor;
var
  i,x2both : integer;
begin
  Result := nil;
  if (CurrentY = aRow) then begin

    x2both := -3; //flag
    for i := 0 to length(FHighlights)-1 do
      with FHighlights[i] do
        if not Ignore
        and (X < X2)
        and (ColorIdx >= 0)
        and (aStartCol.Logical >= x)
        and (aStartCol.Logical < X2) then
        begin
          //MarkupInfo.FrameColor:= clGreen; //debug
          if x2both = -3 then //first call flag
          begin
            MarkupInfo.FrameColor:= clNone;
            MarkupInfo.Foreground:= clNone;
            MarkupInfo.Background:= clNone;
            MarkupInfo.FrameEdges:= sfeNone;
            x2both := 0;
          end;

          Result := MarkupInfo;
          x2both := max(x2both, x2);
          MarkupInfo.SetFrameBoundsLog(x, x2both);
          if Border then
          begin
            MarkupInfo.FrameColor:= Colors[ColorIdx];
            MarkupInfo.FrameEdges:= sfeLeft;//sfeAround;//
          end
          else
            MarkupInfo.Foreground := Colors[ColorIdx];

          //MarkupInfo.FrameEdges:= sfeAround; //debug

          {//2nd debug
          if x > x2 then
          begin
            MarkupInfo.Background:= clYellow;
            MarkupInfo.SetFrameBoundsLog(x-1, x2+20);
            MarkupInfo.FrameColor:= clBlue; //debug
          end;}

          //break;
        end;
  end;
end;

procedure TSynEditMarkupFoldColors.GetNextMarkupColAfterRowCol(
  const aRow: Integer; const aStartCol: TLazSynDisplayTokenBound;
  const AnRtlInfo: TLazSynDisplayRtlInfo; out ANextPhys, ANextLog: Integer);
var i : integer;
begin
  ANextLog := -1;
  ANextPhys := -1;
  if (CurrentY = aRow)  then
  for i := 0 to length(FHighlights)-1  do
    with FHighlights[i] do
    begin
      //if Ignore or (ColorIdx < 0) or (X >= X2) or (aStartCol.Logical >= x) or (aStartCol.Logical > X2) then
        //continue;
      if not Ignore and (ColorIdx >= 0) and (X < X2) and (aStartCol.Logical < x) and (aStartCol.Logical <= X2) then
      begin
        ANextLog := FHighlights[i].X;
        break;
      end;
    end;
end;

procedure TSynEditMarkupFoldColors.DoMarkupParentFoldAtRow(aRow: Integer);
var
  i,lvl,z : integer; //iterate parents fold

  procedure AddVerticalLine( ANode: TSynFoldNodeInfo );
  begin
    z := Length(FHighlights);
    SetLength(FHighlights, z+1);
    with FHighlights[z] do begin
      SrcNode:= ANode; //needed by close node
      Border := ANode.LineIndex + 1 <> aRow;
      X  := ANode.LogXStart + 1;
      Y  := aRow;//ANode.LineIndex + 1;
      X2 := X+1; //ANode.LogXEnd + 1;
      Ignore := False;

      if Border and (sfaOutlineNoLine in ANode.FoldAction) then
        Ignore := True;
      if not Border and (sfaOutlineNoColor in ANode.FoldAction) then
        Ignore := True;
        ColorIdx := lvl mod (length(Colors))
    end;
  end;

var
  y, lvlB,lvlA: Integer;
  TmpNode: TSynFoldNodeInfo;
  NestCount : integer;
  procedure Later(var J:integer);
  begin
    inc(J);
  end;
  function Allowed(J: integer):boolean;
  begin
    result := J < NestCount;
  end;

begin
  y := aRow-1;
  FNestList.Line := y;
  NestCount := FNestList.Count;

  lvl := 0;
  i := 0;
  while Allowed(i) do
  begin

    TmpNode := FNestList.HLNode[i];
    //find till valid
    while (sfaInvalid in TmpNode.FoldAction ) and Allowed(i+1) do //(i < FNestList.Count) do
    begin
      Later(i);
      TmpNode := FNestList.HLNode[i];
    end;

    if (sfaOutline in TmpNode.FoldAction ) then
    //avoid bug of IncludeOpeningOnLine := False;
    if (sfaOpen in TmpNode.FoldAction)  and (TmpNode.LineIndex + 1 = aRow) then
    begin {do nothing here} end
    else
    begin
      lvlB := lvl;

      if ( sfaOutlineForceIndent in TmpNode.FoldAction) then
        inc(lvl);
      if ( sfaOutlineMergeParent in TmpNode.FoldAction) then
        dec(lvl);

      AddVerticalLine(TmpNode);
      if not( sfaOutlineKeepLevel in TmpNode.FoldAction) then
        inc(lvl);

      lvlA := lvl;

      with FHighlights[z] do begin
        LevelBefore := lvlB;
        LevelAfter  := lvlA;
      end;
    end;

    Later(i);
    //break; //debug
  end;
end;

procedure TSynEditMarkupFoldColors.DoMarkupParentCloseFoldAtRow(aRow: Integer);
var
  lvl,z : integer;

  procedure AddHighlight( ANode: TSynFoldNodeInfo );
  var x,j : integer;
  begin
        //don't replace; don't add when already found
    x  := ANode.LogXStart + 1;

    if ANode.LogXStart < ANode.LogXEnd then
    for j := 0 to Pred(length(FHighlights)) do
      if (FHighlights[j].X = x)
      and (FHighlights[j].Border)
      and (FHighlights[j].SrcNode.FoldType = ANode.FoldType )
      and (FHighlights[j].SrcNode.FoldLvlEnd = ANode.FoldLvlStart )
      then begin
       FHighlights[j].X2 := ANode.LogXEnd+1 ;//exit; //
       FHighlights[j].Border := False

      end;

    //exit; //debug
    z := Length(FHighlights);
    SetLength(FHighlights, z+1);
    with FHighlights[z] do begin
      Border := False;
      SrcNode:= ANode; //needed by close node
      Y  := ANode.LineIndex + 1;
      X  := ANode.LogXStart + 1;
      X2 := ANode.LogXEnd + 1;
      //ColorIdx := lvl;
      if not (sfaOutlineNocolor in ANode.FoldAction) then
         ColorIdx := lvl mod (length(Colors))
      else
         ColorIdx := -1;
    end;
  end;

var
  y,i,j,lvlB,lvlA : integer;
  HL: TSynCustomFoldHighlighter;
  NodeList: TLazSynFoldNodeInfoList;
  TmpNode: TSynFoldNodeInfo;
  Found : boolean;
begin
  y := aRow -1;

  HL := TCustomSynEdit(self.SynEdit).Highlighter as TSynCustomFoldHighlighter;
  HL.CurrentLines := Lines;
  HL.FoldNodeInfo[y].ClearFilter; // only needed once, in case the line was already used

  NodeList := HL.FoldNodeInfo[y];
  NodeList.AddReference;
  try
    NodeList.ActionFilter := [sfaOutline];
    //NodeList.FoldFlags:= [sfbIncludeDisabled];
    lvl := 0;
    J := Length(FHighlights)-1;
    if J >=0 then
      lvl := max(0,FHighlights[J].LevelAfter);
    i := 0;
    repeat
      TmpNode := NodeList[i];

      //find till valid
      while (sfaInvalid in TmpNode.FoldAction) and (i + 1 < NodeList.Count) do
      begin
        inc(i);
        TmpNode := NodeList[i];
      end;
      if not (sfaInvalid in TmpNode.FoldAction) and (sfaOutline in TmpNode.FoldAction) then begin
        if sfaOpen in TmpNode.FoldAction then
        begin
          lvlB := lvl;

          if ( sfaOutlineForceIndent in TmpNode.FoldAction) then
            inc(lvl);
          if ( sfaOutlineMergeParent in TmpNode.FoldAction) then
            dec(lvl);

          AddHighlight(TmpNode);
          if not( sfaOutlineKeepLevel in TmpNode.FoldAction) then
            inc(lvl);
          lvlA := lvl;

          with FHighlights[z] do begin
            LevelBefore := lvlB;
            LevelAfter  := lvlA;
          end;

        end
        else
        if sfaClose in TmpNode.FoldAction then
        begin
          Found := False;
          for j := Length(FHighlights)-1 downto 0 do begin
            with FHighlights[j].SrcNode do begin
              if  (FoldType = TmpNode.FoldType) and
                (FoldGroup = TmpNode.FoldGroup) and
                (sfaOpen in FoldAction) and
                // (FoldLvlEnd = TmpNode.FoldLvlStart)
                (NestLvlEnd = TmpNode.NestLvlStart)

                then begin
                  lvl := FHighlights[j].ColorIdx;
                  lvlB := FHighlights[j].LevelBefore;
                  Found := True;
                  break;
                end;
            end;
          end;
          if Found then begin
            AddHighlight(TmpNode);
            lvl := lvlB;
          end;

          //if not( sfaOutlineKeepLevel in TmpNode.FoldAction) then
            //inc(lvl);
        end;
      end;

      inc(i);
    until i >= NodeList.Count;

  finally
    NodeList.ReleaseReference;
  end;
end;

procedure TSynEditMarkupFoldColors.PrepareMarkupForRow(aRow: Integer);
begin
  CurrentY := aRow;
  SetLength(FHighlights,0); //reset needed to prevent using of invalid area

  if not (TCustomSynEdit(self.SynEdit).Highlighter is TSynCustomFoldHighlighter) then
    exit;

  //DoMarkupFoldAtRow(aRow);
  DoMarkupParentFoldAtRow(aRow);
  DoMarkupParentCloseFoldAtRow(aRow);
  //DoMarkupRangeFoldAtRow(aRow);

  FHighlights := SortLeftMostFI(FHighlights);
end;

procedure TSynEditMarkupFoldColors.EndMarkup;
begin
  inherited EndMarkup;
  FNestList.Clear; // for next markup start
end;

function TSynEditMarkupFoldColors.GetFoldHighLighter: TSynCustomFoldHighlighter;
begin
  result := TCustomSynEdit(self.SynEdit).Highlighter as TSynCustomFoldHighlighter;
end;

procedure TSynEditMarkupFoldColors.SetDefaultGroup(AValue: integer);
begin
  if FDefaultGroup = AValue then Exit;
  FDefaultGroup := AValue;
  FNestList.FoldGroup := FDefaultGroup;//1;//FOLDGROUP_PASCAL;
end;

{.$define debug_FC_line_changed}
procedure TSynEditMarkupFoldColors.DoTextChanged(StartLine, EndLine,
  ACountDiff: Integer);
{$ifdef debug_FC_line_changed}
var F : TCustomForm;
begin
  F := GetParentForm(self.SynEdit);
  if F <> nil then
    //F.Caption := Format('Start:%d Endline:%d  Diff:%d',[StartLine, EndLIne, ACountDiff]);
  F.Caption := F.Caption +  Caret.LineText
{$else}



  function GetPairCloseFold(aRow, X : integer  ): Integer;
  var
    y,i,LCnt : integer;
    HL: TSynCustomFoldHighlighter;
    NodeList: TLazSynFoldNodeInfoList;
    TmpNode, CloseNode: TSynFoldNodeInfo;

    function FindEndNode(StartNode: TSynFoldNodeInfo;
                       {var} YIndex, NIndex: Integer): TSynFoldNodeInfo;
      function SearchLine(ALineIdx: Integer; var ANodeIdx: Integer): TSynFoldNodeInfo;
      begin
        NodeList.Line := ALineIdx;
        repeat
          inc(ANodeIdx);
          Result := NodeList[ANodeIdx];
        until (sfaInvalid in Result.FoldAction)
           or (Result.NestLvlEnd <= StartNode.NestLvlStart);
      end;

    begin
      Result := SearchLine(YIndex, NIndex);
      if not (sfaInvalid in Result.FoldAction) then
        exit;

      inc(YIndex);
      while (YIndex < LCnt) and
            (HL.FoldBlockMinLevel(YIndex, StartNode.FoldGroup, [sfbIncludeDisabled])
             > StartNode.NestLvlStart)
      do
        inc(YIndex);
      if YIndex = LCnt then
        exit;

      NIndex := -1;
      Result := SearchLine(YIndex, NIndex);

      if (Result.LogXEnd = 0) or (sfaLastLineClose in Result.FoldAction) then
        Result.FoldAction := [sfaInvalid]; // LastLine closed Node(maybe force-closed?)
    end;

  begin
    Result := -1;
    y := aRow -1;

    HL := TCustomSynEdit(self.SynEdit).Highlighter as TSynCustomFoldHighlighter;
    HL.CurrentLines := Lines;
    LCnt := Lines.Count;
    HL.FoldNodeInfo[y].ClearFilter; // only needed once, in case the line was already used

    NodeList := HL.FoldNodeInfo[y];
    NodeList.AddReference;
    try
      NodeList.ActionFilter := [sfaOpen];
      i := 0;
      repeat
        TmpNode := NodeList[i];

        if TmpNode.LogXStart < X-1 then
        begin
          inc(i);
          continue;
        end;

        //find till valid
        while (sfaInvalid in TmpNode.FoldAction) and (i < NodeList.Count) do
        begin
          inc(i);
          TmpNode := NodeList[i];
        end;
        if not (sfaInvalid in TmpNode.FoldAction) then
        begin
          CloseNode := FindEndNode(TmpNode, y, i);
          //AddHighlight(TmpNode);
          Result := CloseNode.LineIndex;
          exit;
        end;

        inc(i);
      until i >= NodeList.Count;

    finally
      NodeList.ReleaseReference;
    end;
  end;


  function IsFoldMoved( aRow: Integer ): integer;
  var S : string;
    i,n : integer;
  begin
    Result := -1;
    n := -1;

    S := Caret.LineText;
    for i := 1 to Min(Length(S), Length(FPrevCaretText)) do
    begin
      if S[i] <> FPrevCaretText[i] then
      begin
        n := i;
        break;
      end;
    end;

    if n < 0 then exit;

    Result := GetPairCloseFold(aRow, n);
    //limit to screen bottom
    if Result > 0 then
    begin
      inc(Result);//because sometime 'end' has trailing vertical line
      with TCustomSynEdit(SynEdit) do
        Result := min(Result, TopLine +LinesInWindow);// . .RowToScreenRow(i);
    end;

  end;
var
  EndFoldLine,y : integer;
begin
  if EndLine < 0 then exit; //already refreshed by syn

  y := Caret.LineBytePos.y;
  EndFoldLine := IsFoldMoved(y);
  if EndFoldLine > 0 then
  begin
    InvalidateSynLines(y+1, EndFoldLine);
  end;

  FPrevCaretText := Caret.LineText;
  // I found that almost anything has been repaint by the SynEdit,
  // except the trailing space editing: we should repaint them here.
{$endif}
end;

procedure TSynEditMarkupFoldColors.DoCaretChanged(Sender: TObject);
var Y : integer;
begin
  Y := Caret.LineBytePos.y;
  if Y = FCaretY then exit;

  FCaretY := Y;
  FPrevCaretText := Caret.LineText;
end;



end.

