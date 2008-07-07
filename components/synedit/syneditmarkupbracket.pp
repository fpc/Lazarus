unit SynEditMarkupBracket;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, SynEditMarkup, SynEditMiscClasses, Controls, LCLProc;

type

  { TSynEditMarkupBracket }

  TSynEditMarkupBracket = class(TSynEditMarkup)
  private
    fBracketHighlightPos: TPoint;
    fBracketHighlightAntiPos: TPoint;
  public
    constructor Create(ASynEdit : TCustomControl);

    Function GetMarkupAttributeAtRowCol(aRow, aCol : Integer) : TSynSelectedColor; override;
    Function GetNextMarkupColAfterRowCol(aRow, aCol : Integer) : Integer; override;

    procedure InvalidateBracketHighlight;
  end;

implementation
uses SynEdit;


{ TSynEditMarkupBracket }


constructor TSynEditMarkupBracket.Create(ASynEdit : TCustomControl);
begin
  inherited Create(ASynEdit);
  fBracketHighlightPos.Y := -1;
  fBracketHighlightAntiPos.Y := -1;
  MarkupInfo.Foreground := clNone;
  MarkupInfo.Background := clNone;
  MarkupInfo.Style := [fsBold];
  MarkupInfo.StyleMask := [];
end;

procedure TSynEditMarkupBracket.InvalidateBracketHighlight;
var
  NewPos, NewAntiPos, SwapPos : TPoint;
begin
  NewPos.Y:=-1;
  NewAntiPos.Y:=-1;
  if eoBracketHighlight in TSynEdit(SynEdit).Options 
  then TSynEdit(SynEdit).FindMatchingBracketPair(TSynEdit(SynEdit).CaretXY, NewPos, NewAntiPos, false);

  // Always keep ordered
  if (NewAntiPos.Y > 0)
  and ((NewAntiPos.Y < NewPos.Y) or ((NewAntiPos.Y = NewPos.Y) and (NewAntiPos.X < NewPos.X)))
  then begin
    SwapPos    := NewAntiPos;
    NewAntiPos := NewPos;
    NewPos     := SwapPos;
  end;

  // invalidate old bracket highlighting, if changed
  if (fBracketHighlightPos.Y > 0)
  and ((fBracketHighlightPos.Y <> NewPos.Y) or (fBracketHighlightPos.X <> NewPos.X))
  then begin
    //DebugLn('TCustomSynEdit.InvalidateBracketHighlight A Y=',dbgs(fBracketHighlightPos));
    InvalidateSynLines(fBracketHighlightPos.Y,fBracketHighlightPos.Y);
  end;

  if (fBracketHighlightAntiPos.Y > 0)
  and (fBracketHighlightPos.Y <> fBracketHighlightAntiPos.Y)
  and ((fBracketHighlightAntiPos.Y <> NewAntiPos.Y) OR (fBracketHighlightAntiPos.X <> NewAntiPos.X))
  then
    InvalidateSynLines(fBracketHighlightAntiPos.Y,fBracketHighlightAntiPos.Y);

  // invalidate new bracket highlighting, if changed
  if NewPos.Y>0 then begin
    //DebugLn('TCustomSynEdit.InvalidateBracketHighlight C Y=',dbgs(NewPos.Y),' X=',dbgs(NewPos.X),' Y=',dbgs(NewAntiPos.Y),' X=',dbgs(NewAntiPos.X));
    if ((fBracketHighlightPos.Y <> NewPos.Y) or (fBracketHighlightPos.X <> NewPos.X))
    then InvalidateSynLines(NewPos.Y, NewPos.Y);

    if ((NewPos.Y <> NewAntiPos.Y)
        or ((fBracketHighlightPos.Y = NewPos.Y) and (fBracketHighlightPos.X = NewPos.X))
       )
    and ((fBracketHighlightAntiPos.Y <> NewAntiPos.Y) OR (fBracketHighlightAntiPos.X <> NewAntiPos.X))
    then InvalidateSynLines(NewAntiPos.Y, NewAntiPos.Y);
  end;
  fBracketHighlightPos     := NewPos;
  fBracketHighlightAntiPos := NewAntiPos;
end;



function TSynEditMarkupBracket.GetMarkupAttributeAtRowCol(aRow, aCol : Integer) : TSynSelectedColor;
begin
  Result := nil;
  if ((fBracketHighlightPos.y = aRow) and  (fBracketHighlightPos.x = aCol))
  or ((fBracketHighlightAntiPos.y = aRow) and  (fBracketHighlightAntiPos.x = aCol))
  then Result := MarkupInfo;
end;

function TSynEditMarkupBracket.GetNextMarkupColAfterRowCol(aRow, aCol : Integer) : Integer;
begin
  Result := -1;
  if (fBracketHighlightPos.y = aRow) then begin
    if  (fBracketHighlightPos.x > aCol)
    then Result := fBracketHighlightPos.x
    else if  (fBracketHighlightPos.x + 1 > aCol)
    then Result := fBracketHighlightPos.x + 1; // end of bracket
  end;
  if (fBracketHighlightAntiPos.y = aRow) then begin
    if  (fBracketHighlightAntiPos.x > aCol)
    and ((fBracketHighlightAntiPos.x < Result) or (Result < 0))
    then Result := fBracketHighlightAntiPos.x
    else if  (fBracketHighlightAntiPos.x + 1 > aCol)
    and ((fBracketHighlightAntiPos.x + 1 < Result) or (Result < 0))
    then Result := fBracketHighlightAntiPos.x + 1;
  end
end;

end.

