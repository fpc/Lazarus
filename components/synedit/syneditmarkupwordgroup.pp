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
unit SynEditMarkupWordGroup;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, SynEditMarkup, SynEditMiscClasses, Controls,
  LCLProc, SynEditHighlighter, SynEditHighlighterFoldBase;

type

  TWordPoint = record
    Y, X, X2: Integer;
  end;


  { TSynEditMarkupWordGroup }

  TSynEditMarkupWordGroup = class(TSynEditMarkup)
  private
    // Physical Position
    FHighlightPos1: TWordPoint;
    FHighlightPos2: TWordPoint;
    FHighlightPos3: TWordPoint;
    FHighlighter: TSynCustomHighlighter;
    FNeedInvalidate: Boolean;
    FForceInvalidate: Boolean;
    procedure SetHighlighter(const AValue: TSynCustomHighlighter);
  protected
    procedure FindMatchingWords(LogCaret: TPoint;
      out Word1, Word2, Word3: TWordPoint);
    procedure DoCaretChanged(Sender: TObject); override;
    procedure DoTopLineChanged(OldTopLine : Integer); override;
    procedure DoLinesInWindoChanged(OldLinesInWindow : Integer); override;
    procedure DoTextChanged(StartLine, EndLine, ACountDiff: Integer); override;
    procedure DoMarkupChanged(AMarkup: TSynSelectedColor); override;
    procedure DoEnabledChanged(Sender: TObject); override;
    procedure DoVisibleChanged(AVisible: Boolean); override;
    procedure InvalidateCurrentHighlight;
  public
    constructor Create(ASynEdit: TSynEditBase);
    procedure DecPaintLock; override;

    function GetMarkupAttributeAtRowCol(const aRow: Integer;
                                        const aStartCol: TLazSynDisplayTokenBound;
                                        const AnRtlInfo: TLazSynDisplayRtlInfo): TSynSelectedColor; override;
    procedure GetNextMarkupColAfterRowCol(const aRow: Integer;
                                         const aStartCol: TLazSynDisplayTokenBound;
                                         const AnRtlInfo: TLazSynDisplayRtlInfo;
                                         out   ANextPhys, ANextLog: Integer); override;

    property  Highlighter: TSynCustomHighlighter
      read FHighlighter write SetHighlighter;
  end;

implementation

Function CompareWordPoints(P1, P2: TWordPoint): Integer;
begin
  if P1.Y < P2.Y then Exit(-1);
  if P1.Y > P2.Y then Exit(+1);
  If P1.X < P2.X then Exit(-1);
  If P1.X > P2.X then Exit(+1);
  If P1.X2 < P2.X2 then Exit(-1);
  If P1.X2 > P2.X2 then Exit(+1);
  Result := 0;
end;

Function PointToWordPoint(P1: TPoint): TWordPoint;
begin
  Result.Y := P1.Y;
  Result.X := P1.X;
  Result.X2 := P1.X+1;
end;

  { TSynEditMarkupWordGroup }

constructor TSynEditMarkupWordGroup.Create(ASynEdit : TSynEditBase);
begin
  inherited Create(ASynEdit);
  FHighlightPos1.Y := -1;
  FHighlightPos2.Y := -1;
  MarkupInfo.Foreground := clNone;// clNone;
  MarkupInfo.FrameColor := clred;// clNone;
  MarkupInfo.Background := clNone;
  MarkupInfo.Style := [];
  MarkupInfo.StyleMask := [];
end;

procedure TSynEditMarkupWordGroup.DecPaintLock;
begin
  inherited DecPaintLock;
  if (FPaintLock = 0) and FNeedInvalidate then
    InvalidateCurrentHighlight;
end;

procedure TSynEditMarkupWordGroup.SetHighlighter(const AValue: TSynCustomHighlighter);
begin
  FHighlighter := AValue;
end;

procedure TSynEditMarkupWordGroup.FindMatchingWords(LogCaret: TPoint;
  out Word1, Word2, Word3: TWordPoint);
var
  LCnt: Integer;
  HL: TSynCustomFoldHighlighter;
  NodeList: TLazSynFoldNodeInfoList;

  function FindEndNode(StartNode: TSynFoldNodeInfo;
                       var YIndex, NIndex: Integer): TSynFoldNodeInfo;
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

  function FindStartNode(EndNode: TSynFoldNodeInfo;
                       var YIndex, NIndex: Integer): TSynFoldNodeInfo;
    function SearchLine(ALineIdx: Integer; var ANodeIdx: Integer): TSynFoldNodeInfo;
    begin
      NodeList.Line := ALineIdx;
      if ANodeIdx < 0 then
        ANodeIdx := NodeList.Count;
      repeat
        dec(ANodeIdx);
        Result := NodeList[ANodeIdx];
      until (sfaInvalid in Result.FoldAction)
         or (Result.NestLvlStart <= EndNode.NestLvlEnd);
    end;
  begin
    Result := SearchLine(YIndex, NIndex);
    if not(sfaInvalid in Result.FoldAction) then
      exit;

    dec(YIndex);
    while (YIndex >= 0) and
          (HL.FoldBlockMinLevel(YIndex, EndNode.FoldGroup, [sfbIncludeDisabled]) > EndNode.NestLvlEnd)
    do
      dec(YIndex);
    if YIndex < 0 then
      exit;

    NIndex := -1;
    Result := SearchLine(YIndex, NIndex);

    if (Result.LogXEnd = 0) or (sfaLastLineClose in Result.FoldAction) then
      Result.FoldAction := [sfaInvalid]; // LastLine closed Node(maybe force-closed?)
  end;

  function CompareBounds(const Node1, Node2: TSynFoldNodeInfo): Boolean;
  begin
    Result := (not (sfaInvalid in Node1.FoldAction)) and
              (not (sfaInvalid in Node2.FoldAction)) and
              (Node1.LogXStart = Node2.LogXStart) and
              (Node1.LogXEnd = Node2.LogXEnd);
  end;

  function CheckNeighbourNode(const CurNode: TSynFoldNodeInfo; Offset: Integer;
    var FoundNode: TSynFoldNodeInfo): Boolean;
  var
    TmpNode: TSynFoldNodeInfo;
  begin
    TmpNode := NodeList[CurNode.NodeIndex + Offset];
    Result := CompareBounds(CurNode, TmpNode);
    if Result then
      FoundNode := TmpNode;
  end;

var
  i, y: integer;
  StartNode, CloseNode, Node3, TmpNode: TSynFoldNodeInfo;
begin
  Word1.Y := -1;
  Word2.Y := -1;
  Word3.Y := -1;
  if (not assigned(Lines)) or (not MarkupInfo.IsEnabled) or
     (LogCaret.Y < 1) or (LogCaret.Y > Lines.Count)  or (LogCaret.X < 1)
  then
    Exit;
  if not (FHighlighter is TSynCustomFoldHighlighter) then
    exit;

  hl := TSynCustomFoldHighlighter(FHighlighter);
  y := LogCaret.Y - 1;
  LCnt := Lines.Count;
  HL.CurrentLines := Lines;
  HL.FoldNodeInfo[y].ClearFilter; // only needed once, in case the line was already used

  (* Find the node under caret.
     - for "end" (Procedure and begin) this is the inner (end of begin) node
     - For "{$Else}" this is the closing node
  *)
  i := 0;
  NodeList := HL.FoldNodeInfo[y];
  NodeList.AddReference;
  try
    NodeList.ActionFilter := [sfaMarkup];
    TmpNode := NodeList[i];
    while not(sfaInvalid in TmpNode.FoldAction) and (TmpNode.LogXEnd < LogCaret.X-1) do
    begin
      inc(i);
      TmpNode := NodeList[i];
    end;
    if (TmpNode.LogXStart > LogCaret.X - 1) or (sfaInvalid in TmpNode.FoldAction) then
      exit;

    (* Find other end *)
    Node3.FoldAction := [sfaInvalid];
    if TmpNode.FoldAction * [sfaOpenFold, sfaOneLineOpen] <> [] then begin
      StartNode := TmpNode;
      CloseNode := FindEndNode(StartNode, y, i);
      if (sfaInvalid in CloseNode.FoldAction) then
        exit;
      // NodeList now holds the closing line
      if not CheckNeighbourNode(CloseNode, 1, Node3)
      then CheckNeighbourNode(CloseNode, -1, Node3);
    end else begin
      CloseNode := TmpNode;
      CheckNeighbourNode(CloseNode, 1, Node3); // still having the correct NodeList;
      StartNode := FindStartNode(CloseNode, y, i);
      if (sfaInvalid in StartNode.FoldAction) then
        exit;
      // NodeList now holds the opening line
      if sfaInvalid in Node3.FoldAction then
        CheckNeighbourNode(StartNode, -1, Node3); // StartNode could be $ELSE
    end;

    (* Find optional 3rd Node *)
    if not(sfaInvalid in Node3.FoldAction) then begin
      i := Node3.NodeIndex;
      y := Node3.LineIndex;
      if Node3.FoldAction * [sfaOpenFold, sfaOneLineOpen] <> [] then
        Node3 := FindEndNode(Node3, y, i)
      else
        Node3 := FindStartNode(Node3, y, i);
    end;

  finally
    NodeList.ReleaseReference;
  end;

  Word1.Y  := StartNode.LineIndex + 1;
  Word1.X  := StartNode.LogXStart + 1;
  Word1.X2 := StartNode.LogXEnd + 1;
  Word2.Y  := CloseNode.LineIndex + 1;
  Word2.X  := CloseNode.LogXStart + 1;
  Word2.X2 := CloseNode.LogXEnd + 1;
  if not(sfaInvalid in Node3.FoldAction) then
  begin
    Word3.Y  := Node3.LineIndex + 1;
    Word3.X  := Node3.LogXStart + 1;
    Word3.X2 := Node3.LogXEnd + 1;
  end;
end;

procedure TSynEditMarkupWordGroup.DoCaretChanged(Sender: TObject);
var
  C: TPoint;
begin
  if Caret = nil then exit;
  C := Caret.LineBytePos;
  if ( (C.Y = FHighlightPos1.Y) and (C.X >= FHighlightPos1.X) and (C.X <= FHighlightPos1.X2) ) or
     ( (C.Y = FHighlightPos2.Y) and (C.X >= FHighlightPos2.X) and (C.X <= FHighlightPos2.X2) ) or
     ( (C.Y = FHighlightPos3.Y) and (C.X >= FHighlightPos3.X) and (C.X <= FHighlightPos3.X2) ) then
    exit;
  InvalidateCurrentHighlight;
end;

procedure TSynEditMarkupWordGroup.DoTopLineChanged(OldTopLine: Integer);
begin
end;

procedure TSynEditMarkupWordGroup.DoLinesInWindoChanged(OldLinesInWindow: Integer);
begin
end;

procedure TSynEditMarkupWordGroup.DoTextChanged(StartLine, EndLine,
  ACountDiff: Integer);
begin
  InvalidateCurrentHighlight;
end;

procedure TSynEditMarkupWordGroup.DoMarkupChanged(AMarkup: TSynSelectedColor);
begin
  FForceInvalidate := True;
  InvalidateCurrentHighlight;
end;

procedure TSynEditMarkupWordGroup.DoEnabledChanged(Sender: TObject);
begin
  FForceInvalidate := True;
  InvalidateCurrentHighlight;
end;

procedure TSynEditMarkupWordGroup.DoVisibleChanged(AVisible: Boolean);
begin
  inherited DoVisibleChanged(AVisible);
  if SynEdit.IsVisible then
    InvalidateCurrentHighlight;
end;

procedure TSynEditMarkupWordGroup.InvalidateCurrentHighlight;
var
  NewPos, NewAntiPos, NewMiddlePos : TWordPoint;
begin
  FNeedInvalidate := True;
  if (Caret = nil) or (not SynEdit.HandleAllocated) or (FPaintLock > 0) or
     (not SynEdit.IsVisible)
  then
    exit;

  FNeedInvalidate := False;
  FindMatchingWords(Caret.LineBytePos, NewPos, NewAntiPos, NewMiddlePos);

  // invalidate old highlighting, if changed
  if (FHighlightPos1.Y > 0)
  and ((CompareWordPoints(FHighlightPos1, NewPos) <> 0) or FForceInvalidate)
  then
    InvalidateSynLines(FHighlightPos1.Y,FHighlightPos1.Y);

  if (FHighlightPos2.Y > 0)
  and ((CompareWordPoints(FHighlightPos2, NewAntiPos) <> 0) or FForceInvalidate)
  then
    InvalidateSynLines(FHighlightPos2.Y,FHighlightPos2.Y);

  if (FHighlightPos3.Y > 0)
  and ((CompareWordPoints(FHighlightPos3, NewMiddlePos) <> 0) or FForceInvalidate)
  then
    InvalidateSynLines(FHighlightPos3.Y,FHighlightPos3.Y);

  FForceInvalidate := False;

  // invalidate new highlighting, if changed
  if (NewPos.Y>0)
  and (CompareWordPoints(FHighlightPos1, NewPos) <> 0) then
    InvalidateSynLines(NewPos.Y, NewPos.Y);

  if (NewAntiPos.Y>0)
  and (CompareWordPoints(FHighlightPos2, NewAntiPos) <> 0) then
    InvalidateSynLines(NewAntiPos.Y, NewAntiPos.Y);

  if (NewMiddlePos.Y>0)
  and (CompareWordPoints(FHighlightPos3, NewMiddlePos) <> 0) then
    InvalidateSynLines(NewMiddlePos.Y, NewMiddlePos.Y);

  FHighlightPos1     := NewPos;
  FHighlightPos2 := NewAntiPos;
  FHighlightPos3 := NewMiddlePos;
//  DebugLn('TCustomSynEdit.InvalidateCurrentHighlight C P=',dbgs(NewPos),' A=',dbgs(NewAntiPos), ' LP=',dbgs(fLogicalPos),' LA',dbgs(fLogicalAntiPos));
end;

function TSynEditMarkupWordGroup.GetMarkupAttributeAtRowCol(const aRow: Integer;
  const aStartCol: TLazSynDisplayTokenBound; const AnRtlInfo: TLazSynDisplayRtlInfo): TSynSelectedColor;
begin
  Result := nil;
  if (FHighlightPos1.y = aRow) and
   (aStartCol.Logical >= FHighlightPos1.x) and (aStartCol.Logical < FHighlightPos1.X2) then
  begin
    Result := MarkupInfo;
    MarkupInfo.SetFrameBoundsLog(FHighlightPos1.x, FHighlightPos1.x2);
  end
  else
  if (FHighlightPos3.y = aRow) and
   (aStartCol.Logical >= FHighlightPos3.x) and (aStartCol.Logical < FHighlightPos3.X2) then
  begin
    Result := MarkupInfo;
    MarkupInfo.SetFrameBoundsLog(FHighlightPos3.x, FHighlightPos3.x2);
  end
  else
  if (FHighlightPos2.y = aRow) and
   (aStartCol.Logical >= FHighlightPos2.x) and (aStartCol.Logical < FHighlightPos2.X2) then
  begin
    Result := MarkupInfo;
    MarkupInfo.SetFrameBoundsLog(FHighlightPos2.x, FHighlightPos2.x2);
  end;
end;

procedure TSynEditMarkupWordGroup.GetNextMarkupColAfterRowCol(const aRow: Integer;
  const aStartCol: TLazSynDisplayTokenBound; const AnRtlInfo: TLazSynDisplayRtlInfo; out ANextPhys,
  ANextLog: Integer);
  Procedure CheckCol(Column: Integer; var Result: Integer);
  begin
    if (Column <= aStartCol.Logical) or ((Result >= 0) and (Result < Column)) then exit;
    Result := Column;
  end;
begin
  ANextLog := -1;
  ANextPhys := -1;
  if (FHighlightPos1.y = aRow) then begin
    CheckCol(FHighlightPos1.X, ANextLog);
    CheckCol(FHighlightPos1.X2, ANextLog);
  end;
  if (FHighlightPos3.y = aRow) then begin
    CheckCol(FHighlightPos3.X, ANextLog);
    CheckCol(FHighlightPos3.X2, ANextLog);
  end;
  if (FHighlightPos2.y = aRow) then begin
    CheckCol(FHighlightPos2.X, ANextLog);
    CheckCol(FHighlightPos2.X2, ANextLog);
  end;
end;

end.

