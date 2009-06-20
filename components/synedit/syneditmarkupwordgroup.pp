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
    procedure SetHighlighter(const AValue: TSynCustomHighlighter);
  protected
    procedure FindMatchingWords(PhysCaret: TPoint;
      out Word1, Word2, Word3: TWordPoint);
    procedure DoCaretChanged(Sender: TObject); override;
    procedure DoTopLineChanged(OldTopLine : Integer); override;
    procedure DoLinesInWindoChanged(OldLinesInWindow : Integer); override;
    procedure DoTextChanged(StartLine, EndLine : Integer); override;
    procedure DoMarkupChanged(AMarkup: TSynSelectedColor); override;
    procedure InvalidateCurrentHighlight;
  public
    constructor Create(ASynEdit: TSynEditBase);

    function GetMarkupAttributeAtRowCol(const aRow, aCol: Integer): TSynSelectedColor; override;
    function GetNextMarkupColAfterRowCol(const aRow, aCol: Integer): Integer; override;

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

procedure TSynEditMarkupWordGroup.SetHighlighter(const AValue: TSynCustomHighlighter);
begin
  FHighlighter := AValue;
end;

procedure TSynEditMarkupWordGroup.FindMatchingWords(PhysCaret: TPoint;
  out Word1, Word2, Word3: TWordPoint);
var
  LCnt: Integer;
  HL: TSynCustomFoldHighlighter;

  function FindEndNode(StartNode: TSynFoldNodeInfo;
                       var YIndex, NIndex: Integer): TSynFoldNodeInfo;
  begin
    repeat
      inc(NIndex);
      Result := HL.FoldNodeInfo[YIndex, NIndex, []];
    until (sfaInvalid in Result.FoldAction) or
          (Result.FoldLvlEnd <= StartNode.FoldLvlStart);
    if not (sfaInvalid in Result.FoldAction) then
      exit;

    inc(YIndex);
    while (YIndex < LCnt) and
          (HL.MinimumFoldLevel(YIndex) > StartNode.FoldLvlStart) do
      inc(YIndex);
    if YIndex = LCnt then
      exit;

    NIndex := -1;
    repeat
      inc(NIndex);
      Result:= HL.FoldNodeInfo[YIndex, NIndex, []];
    until (sfaInvalid in Result.FoldAction) or
          (Result.FoldLvlEnd <= StartNode.FoldLvlStart);
    if (Result.LogXEnd = 0) then
      Result.FoldAction := [sfaInvalid]; // LastLine closed Node(maybe force-closed?)
  end;

  function FindStartNode(EndNode: TSynFoldNodeInfo;
                       var YIndex, NIndex: Integer): TSynFoldNodeInfo;
  begin
    repeat
      dec(NIndex);
      Result := HL.FoldNodeInfo[YIndex, NIndex, []];
    until (sfaInvalid in Result.FoldAction) or
          (Result.FoldLvlStart <= EndNode.FoldLvlEnd);
    if not(sfaInvalid in Result.FoldAction) then
      exit;

    dec(YIndex);
    while (YIndex >= 0) and (HL.MinimumFoldLevel(YIndex) > EndNode.FoldLvlEnd) do
      dec(YIndex);
    if YIndex < 0 then
      exit;
    NIndex := HL.FoldNodeInfoCount[YIndex, []];
    repeat
      dec(NIndex);
      Result:= HL.FoldNodeInfo[YIndex, NIndex, []];
    until (sfaInvalid in Result.FoldAction) or
          (Result.FoldLvlStart <= EndNode.FoldLvlEnd);
  end;

  function FindMultiNode(OrigNode: TSynFoldNodeInfo;
                       var YIndex, NIndex: Integer): TSynFoldNodeInfo;
  var
    i: LongInt;
  begin
    i := NIndex;
    repeat
      dec(NIndex);
      Result := HL.FoldNodeInfo[YIndex, NIndex, []];
      if (sfaMarkup in Result.FoldAction) and
         (Result.LogXStart = OrigNode.LogXStart) and (Result.LogXEnd > 0)
      then
        exit;
    until (sfaInvalid in Result.FoldAction) or (Result.LogXStart <> OrigNode.LogXStart);
    NIndex := i;
    repeat
      inc(NIndex);
      Result := HL.FoldNodeInfo[YIndex, NIndex, []];
      if (sfaMarkup in Result.FoldAction) and
         (Result.LogXStart = OrigNode.LogXStart) and (Result.LogXEnd > 0)
      then
        exit;
    until (sfaInvalid in Result.FoldAction) or (Result.LogXStart <> OrigNode.LogXStart);
    Result.FoldAction := [sfaInvalid];
  end;

var
  LogCaretXY: TPoint;
  i, i2, i3, y, y1, y2: integer;
  Node1, Node2, Node3: TSynFoldNodeInfo;
begin
  Word1.Y := -1;
  Word2.Y := -1;
  Word3.Y := -1;
  if (not assigned(Lines)) or (not MarkupInfo.IsEnabled) or
     (PhysCaret.Y < 1) or (PhysCaret.Y > Lines.Count)  or (PhysCaret.X < 1)
  then
    Exit;

  if not (FHighlighter is TSynCustomFoldHighlighter) then
    exit;
  hl := TSynCustomFoldHighlighter(FHighlighter);
  LogCaretXY := Lines.PhysicalToLogicalPos(PhysCaret);
  y := LogCaretXY.Y - 1;
  LCnt := Lines.Count;
  HL.CurrentLines := Lines;
  i := 0;
  repeat
    Node1 := HL.FoldNodeInfo[y, i, []];
    inc(i);
  until (sfaInvalid in Node1.FoldAction) or
        ((Node1.LogXEnd >= LogCaretXY.X - 1) and (Node1.LogXEnd > 0));
  while not(Node1.FoldAction * [sfaInvalid, sfaMarkup] <> [])
        and (Node1.LogXStart <= LogCaretXY.X - 1) do
  begin
    Node1 := HL.FoldNodeInfo[y, i, []];
    inc(i);
  end;
  if (Node1.LogXStart > LogCaretXY.X - 1) or not(sfaMarkup in Node1.FoldAction) then
    exit;
  dec(i);

  if sfaOpen in Node1.FoldAction then begin
    y1 := y;
    Node2 := FindEndNode(Node1, y, i);
    if (sfaInvalid in Node2.FoldAction) or not(sfaMarkup in Node2.FoldAction) then
      exit;
    y2 := y;
    i2 := i;
    i3 := i;
    Node3 := FindMultiNode(Node2, y, i3);
  end else begin
    Node2 := Node1;
    i3 := i;
    Node3 := FindMultiNode(Node2, y, i3);
    y2 := y;
    i2 := i;
    Node1 := FindStartNode(Node2, y, i);
    if (sfaInvalid in Node1.FoldAction) or not(sfaMarkup in Node1.FoldAction) then
      exit;
    y1 := y;
  end;

  y := y2;
  if not(sfaInvalid in Node3.FoldAction) then
    Node3 := FindStartNode(Node3, y, i);

  Word1.Y  := y1 + 1;
  Word1.X  := Node1.LogXStart + 1;
  Word1.X2 := Node1.LogXEnd + 1;
  Word2.Y  := y2 + 1;
  Word2.X  := Node2.LogXStart + 1;
  Word2.X2 := Node2.LogXEnd + 1;
  if (sfaMarkup in Node3.FoldAction) and not(sfaInvalid in Node3.FoldAction) then
  begin
    Word3 := Word2;
    if i3 > i2 then begin
      Word2 := Word1;
      Word1.Y  := y + 1;
      Word1.X  := Node3.LogXStart + 1;
      Word1.X2 := Node3.LogXEnd + 1;
    end else begin
      Word2.Y  := y + 1;
      Word2.X  := Node3.LogXStart + 1;
      Word2.X2 := Node3.LogXEnd + 1;
    end;
  end;

  if Word1.Y > 0 then begin
    Word1.X  := Lines.LogicalToPhysicalPos(Point(Word1.X, Word1.Y)).X;
    Word1.X2 := Lines.LogicalToPhysicalPos(Point(Word1.X2, Word1.Y)).X;
  end;
  if Word2.Y > 0 then begin
    Word2.X  := Lines.LogicalToPhysicalPos(Point(Word2.X, Word2.Y)).X;
    Word2.X2 := Lines.LogicalToPhysicalPos(Point(Word2.X2, Word2.Y)).X;
  end;
  if Word3.Y > 0 then begin
    Word3.X  := Lines.LogicalToPhysicalPos(Point(Word3.X, Word3.Y)).X;
    Word3.X2 := Lines.LogicalToPhysicalPos(Point(Word3.X2, Word3.Y)).X;
  end;
end;

procedure TSynEditMarkupWordGroup.DoCaretChanged(Sender: TObject);
var
  C: TPoint;
begin
  if Caret = nil then exit;
  C := Caret.LineCharPos;
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

procedure TSynEditMarkupWordGroup.DoTextChanged(StartLine, EndLine: Integer);
begin
  InvalidateCurrentHighlight;
end;

procedure TSynEditMarkupWordGroup.DoMarkupChanged(AMarkup: TSynSelectedColor);
begin
  InvalidateCurrentHighlight;
end;

procedure TSynEditMarkupWordGroup.InvalidateCurrentHighlight;
var
  NewPos, NewAntiPos, NewMiddlePos : TWordPoint;
begin
  if Caret = nil then exit;
  FindMatchingWords(Caret.LineCharPos, NewPos, NewAntiPos, NewMiddlePos);

  // invalidate old highlighting, if changed
  if (FHighlightPos1.Y > 0)
  and (CompareWordPoints(FHighlightPos1, NewPos) <> 0)
  then
    InvalidateSynLines(FHighlightPos1.Y,FHighlightPos1.Y);

  if (FHighlightPos2.Y > 0)
  and (CompareWordPoints(FHighlightPos2, NewAntiPos) <> 0)
  then
    InvalidateSynLines(FHighlightPos2.Y,FHighlightPos2.Y);

  if (FHighlightPos3.Y > 0)
  and (CompareWordPoints(FHighlightPos3, NewMiddlePos) <> 0)
  then
    InvalidateSynLines(FHighlightPos3.Y,FHighlightPos3.Y);

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

function TSynEditMarkupWordGroup.GetMarkupAttributeAtRowCol(const aRow, aCol: Integer) : TSynSelectedColor;
begin
  Result := nil;
  if (FHighlightPos1.y = aRow) and
   (aCol >= FHighlightPos1.x) and (aCol < FHighlightPos1.X2) then
  begin
    Result := MarkupInfo;
    MarkupInfo.StartX := FHighlightPos1.x;
    MarkupInfo.EndX := FHighlightPos1.X2 - 1;
  end
  else
  if (FHighlightPos3.y = aRow) and
   (aCol >= FHighlightPos3.x) and (aCol < FHighlightPos3.X2) then
  begin
    Result := MarkupInfo;
    MarkupInfo.StartX := FHighlightPos3.x;
    MarkupInfo.EndX := FHighlightPos3.X2 - 1;
  end
  else
  if (FHighlightPos2.y = aRow) and
   (aCol >= FHighlightPos2.x) and (aCol < FHighlightPos2.X2) then
  begin
    Result := MarkupInfo;
    MarkupInfo.StartX := FHighlightPos2.x;
    MarkupInfo.EndX := FHighlightPos2.X2 - 1;
  end;
end;

function TSynEditMarkupWordGroup.GetNextMarkupColAfterRowCol(const aRow, aCol: Integer) : Integer;
  Procedure CheckCol(Column: Integer; var Result: Integer);
  begin
    if (Column <= aCol) or ((Result >= 0) and (Result < Column)) then exit;
    Result := Column;
  end;
begin
  Result := -1;
  if (FHighlightPos1.y = aRow) then begin
    CheckCol(FHighlightPos1.X, Result);
    CheckCol(FHighlightPos1.X2, Result);
  end;
  if (FHighlightPos3.y = aRow) then begin
    CheckCol(FHighlightPos3.X, Result);
    CheckCol(FHighlightPos3.X2, Result);
  end;
  if (FHighlightPos2.y = aRow) then begin
    CheckCol(FHighlightPos2.X, Result);
    CheckCol(FHighlightPos2.X2, Result);
  end;
end;

end.

