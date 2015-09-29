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
unit SynEditMarkupBracket;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, SynEditMarkup, SynEditMiscClasses, Controls, LCLProc;

type
  TSynEditBracketHighlightStyle = (
    sbhsLeftOfCursor,
    sbhsRightOfCursor,
    sbhsBoth
  );
  { TSynEditMarkupBracket }

  TSynEditMarkupBracket = class(TSynEditMarkup)
  private
    // Physical Position
    FBracketHighlightPos: TPoint;
    FBracketHighlightAntiPos: TPoint;
    FHighlightStyle: TSynEditBracketHighlightStyle;
    FNeedInvalidate: Boolean;
    procedure SetHighlightStyle(const AValue: TSynEditBracketHighlightStyle);
  protected
    procedure FindMatchingBracketPair(LogCaret: TPoint;
      var StartBracket, EndBracket: TPoint);
    procedure DoCaretChanged(Sender: TObject); override;
    procedure DoTopLineChanged(OldTopLine : Integer); override;
    procedure DoLinesInWindoChanged(OldLinesInWindow : Integer); override;
    procedure DoTextChanged(StartLine, EndLine, ACountDiff: Integer); override;
    procedure DoMarkupChanged(AMarkup: TSynSelectedColor); override;
    procedure DoEnabledChanged(Sender: TObject); override;
    procedure DoVisibleChanged(AVisible: Boolean); override;
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

    procedure InvalidateBracketHighlight;
    property HighlightStyle: TSynEditBracketHighlightStyle read FHighlightStyle write SetHighlightStyle;
  end;

implementation
uses
  SynEdit;

{ TSynEditMarkupBracket }

constructor TSynEditMarkupBracket.Create(ASynEdit : TSynEditBase);
begin
  inherited Create(ASynEdit);
  FBracketHighlightPos.Y := -1;
  FBracketHighlightAntiPos.Y := -1;
  FHighlightStyle := sbhsBoth;
  MarkupInfo.Foreground := clNone;
  MarkupInfo.Background := clNone;
  MarkupInfo.Style := [fsBold];
  MarkupInfo.StyleMask := [];
end;

procedure TSynEditMarkupBracket.DecPaintLock;
begin
  inherited DecPaintLock;
  if (FPaintLock = 0) and FNeedInvalidate then
    InvalidateBracketHighlight;
end;

procedure TSynEditMarkupBracket.SetHighlightStyle(
  const AValue: TSynEditBracketHighlightStyle);
begin
  if FHighlightStyle <> AValue then
  begin
    FHighlightStyle := AValue;
    InvalidateBracketHighlight;
  end;
end;

procedure TSynEditMarkupBracket.FindMatchingBracketPair(LogCaret: TPoint; var StartBracket,
  EndBracket: TPoint);
const
  Brackets: set of Char = ['(',')','{','}','[',']', '''', '"' ];
var
  StartLine: string;
  x: Integer;
begin
  StartBracket.Y := -1;
  EndBracket.Y := -1;
  if (LogCaret.Y < 1) or (LogCaret.Y > Lines.Count) or (LogCaret.X < 1) then
    Exit;

  StartLine := Lines[LogCaret.Y - 1];

  // check for bracket, left of cursor
  if (HighlightStyle in [sbhsLeftOfCursor, sbhsBoth]) and (LogCaret.x > 1) then
  begin
    x := Lines.LogicPosAddChars(StartLine, LogCaret.x, -1);
    if (x <= length(StartLine)) and (StartLine[x] in Brackets) then
    begin
      StartBracket := LogCaret;
      StartBracket.x := x;
      EndBracket := TCustomSynEdit(SynEdit).FindMatchingBracketLogical(StartBracket, False, False, False, False);
      if EndBracket.y < 0 then
        StartBracket.y := -1;
      Exit;
    end;
  end;

  // check for bracket after caret
  if (HighlightStyle in [sbhsRightOfCursor, sbhsBoth]) then
  begin
    x := LogCaret.x ;
    if (x <= length(StartLine)) and (StartLine[x] in Brackets) then
    begin
      StartBracket := LogCaret;
      EndBracket := TCustomSynEdit(SynEdit).FindMatchingBracketLogical(LogCaret, False, False, False, False);
      if EndBracket.y < 0 then
        StartBracket.y := -1;
    end;
  end;
end;

procedure TSynEditMarkupBracket.DoCaretChanged(Sender: TObject);
begin
  InvalidateBracketHighlight;
end;

procedure TSynEditMarkupBracket.DoTopLineChanged(OldTopLine: Integer);
begin
  InvalidateBracketHighlight;
end;

procedure TSynEditMarkupBracket.DoLinesInWindoChanged(OldLinesInWindow: Integer);
begin
  InvalidateBracketHighlight;
end;

procedure TSynEditMarkupBracket.DoTextChanged(StartLine, EndLine,
  ACountDiff: Integer);
begin
  InvalidateBracketHighlight;
end;

procedure TSynEditMarkupBracket.DoMarkupChanged(AMarkup: TSynSelectedColor);
begin
  InvalidateBracketHighlight;
end;

procedure TSynEditMarkupBracket.DoEnabledChanged(Sender: TObject);
begin
  InvalidateBracketHighlight;
end;

procedure TSynEditMarkupBracket.DoVisibleChanged(AVisible: Boolean);
begin
  inherited DoVisibleChanged(AVisible);
  if SynEdit.IsVisible then
    InvalidateBracketHighlight;
end;

procedure TSynEditMarkupBracket.InvalidateBracketHighlight;
var
  NewPos, NewAntiPos, SwapPos : TPoint;
begin
  FNeedInvalidate := True;
  if (Caret = nil) or (not SynEdit.HandleAllocated) or (FPaintLock > 0) or
     (not SynEdit.IsVisible)
  then
    exit;

  FNeedInvalidate := False;
  NewPos.Y:=-1;
  NewAntiPos.Y:=-1;
  if eoBracketHighlight in TCustomSynEdit(SynEdit).Options
  then FindMatchingBracketPair(Caret.LineBytePos, NewPos, NewAntiPos);

  // Always keep ordered
  if (NewAntiPos.Y > 0)
  and ((NewAntiPos.Y < NewPos.Y) or ((NewAntiPos.Y = NewPos.Y) and (NewAntiPos.X < NewPos.X)))
  then begin
    SwapPos    := NewAntiPos;
    NewAntiPos := NewPos;
    NewPos     := SwapPos;
  end;

  // invalidate old bracket highlighting, if changed
  if (FBracketHighlightPos.Y > 0)
  and ((FBracketHighlightPos.Y <> NewPos.Y) or (FBracketHighlightPos.X <> NewPos.X))
  then begin
    //DebugLn('TCustomSynEdit.InvalidateBracketHighlight A Y=',dbgs(FBracketHighlightPos));
    InvalidateSynLines(FBracketHighlightPos.Y,FBracketHighlightPos.Y);
  end;

  if (FBracketHighlightAntiPos.Y > 0)
  and (FBracketHighlightPos.Y <> FBracketHighlightAntiPos.Y)
  and ((FBracketHighlightAntiPos.Y <> NewAntiPos.Y) or (FBracketHighlightAntiPos.X <> NewAntiPos.X))
  then
    InvalidateSynLines(FBracketHighlightAntiPos.Y,FBracketHighlightAntiPos.Y);

  // invalidate new bracket highlighting, if changed
  if NewPos.Y>0 then begin
    //DebugLn('TCustomSynEdit.InvalidateBracketHighlight C Y=',dbgs(NewPos.Y),' X=',dbgs(NewPos.X),' Y=',dbgs(NewAntiPos.Y),' X=',dbgs(NewAntiPos.X));
    if ((FBracketHighlightPos.Y <> NewPos.Y) or (FBracketHighlightPos.X <> NewPos.X))
    then InvalidateSynLines(NewPos.Y, NewPos.Y);

    if ((NewPos.Y <> NewAntiPos.Y)
        or ((FBracketHighlightPos.Y = NewPos.Y) and (FBracketHighlightPos.X = NewPos.X))
       )
    and ((FBracketHighlightAntiPos.Y <> NewAntiPos.Y) or (FBracketHighlightAntiPos.X <> NewAntiPos.X))
    then InvalidateSynLines(NewAntiPos.Y, NewAntiPos.Y);
  end;
  FBracketHighlightPos     := NewPos;
  FBracketHighlightAntiPos := NewAntiPos;
//  DebugLn('TCustomSynEdit.InvalidateBracketHighlight C P=',dbgs(NewPos),' A=',dbgs(NewAntiPos), ' LP=',dbgs(fLogicalPos),' LA',dbgs(fLogicalAntiPos));
end;

function TSynEditMarkupBracket.GetMarkupAttributeAtRowCol(const aRow: Integer;
  const aStartCol: TLazSynDisplayTokenBound; const AnRtlInfo: TLazSynDisplayRtlInfo): TSynSelectedColor;
begin
  Result := nil;
  if ((FBracketHighlightPos.y = aRow) and  (FBracketHighlightPos.x = aStartCol.Logical))
  or ((FBracketHighlightAntiPos.y = aRow) and  (FBracketHighlightAntiPos.x = aStartCol.Logical))
  then begin
    Result := MarkupInfo;
    MarkupInfo.SetFrameBoundsLog(aStartCol.Logical, aStartCol.Logical + 1); // bracket is alvays 1 byte
  end;
end;

procedure TSynEditMarkupBracket.GetNextMarkupColAfterRowCol(const aRow: Integer;
  const aStartCol: TLazSynDisplayTokenBound; const AnRtlInfo: TLazSynDisplayRtlInfo; out ANextPhys,
  ANextLog: Integer);
begin
  ANextLog := -1;
  ANextPhys := -1;
  if (FBracketHighlightPos.y = aRow) then begin
    if  (FBracketHighlightPos.x > aStartCol.Logical )
    then ANextLog := FBracketHighlightPos.x
    else if  (FBracketHighlightPos.x + 1 > aStartCol.Logical )
    then ANextLog := FBracketHighlightPos.x + 1; // end of bracket
  end;
  if (FBracketHighlightAntiPos.y = aRow) then begin
    if  (FBracketHighlightAntiPos.x > aStartCol.Logical )
    and ((FBracketHighlightAntiPos.x < ANextLog) or (ANextLog < 0))
    then ANextLog := FBracketHighlightAntiPos.x
    else if  (FBracketHighlightAntiPos.x + 1 > aStartCol.Logical )
    and ((FBracketHighlightAntiPos.x + 1 < ANextLog) or (ANextLog < 0))
    then ANextLog := FBracketHighlightAntiPos.x + 1;
  end
end;

end.

