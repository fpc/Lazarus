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
unit SynEditMarkupSelection;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, Controls, LCLProc,
  SynEditMarkup, SynEditMiscClasses, SynEditPointClasses;

type

  { TSynEditMarkupSelection }

  TSynEditMarkupSelection = class(TSynEditMarkup)
  private
    FSelection: TSynEditSelection;
    FMarkupInfoIncr: TSynSelectedColor; // Markup during incremental search
    FMarkupInfoSelection: TSynSelectedColor; // Markup for normal Selection
    FUseIncrementalColor : Boolean;
    nSelStart, nSelEnd: integer; // start, end of selected area in current line (physical)
    procedure SetUseIncrementalColor(const AValue : Boolean);
    procedure MarkupChangedIntern(AMarkup: TObject);
  public
    constructor Create(ASynEdit : TSynEditBase; ASelection: TSynEditSelection);
    destructor Destroy; override;

    Procedure PrepareMarkupForRow(aRow : Integer); override;
    Function GetMarkupAttributeAtRowCol(const aRow, aCol : Integer) : TSynSelectedColor; override;
    Function GetNextMarkupColAfterRowCol(const aRow, aCol : Integer) : Integer; override;

    property UseIncrementalColor : Boolean read FUseIncrementalColor write SetUseIncrementalColor;
    property MarkupInfoSeletion : TSynSelectedColor read FMarkupInfoSelection;
    property MarkupInfoIncr : TSynSelectedColor read FMarkupInfoIncr;
  end;

implementation
uses SynEdit, SynEditTypes;

{ TSynEditMarkupSelection }

procedure TSynEditMarkupSelection.SetUseIncrementalColor(const AValue : Boolean);
begin
  if FUseIncrementalColor=AValue then exit;
  FUseIncrementalColor:=AValue;
  if FUseIncrementalColor
  then MarkupInfo.Assign(FMarkupInfoIncr)
  else MarkupInfo.Assign(FMarkupInfoSelection);
end;

procedure TSynEditMarkupSelection.MarkupChangedIntern(AMarkup : TObject);
begin
  if FUseIncrementalColor
  then MarkupInfo.Assign(FMarkupInfoIncr)
  else MarkupInfo.Assign(FMarkupInfoSelection);
end;

constructor TSynEditMarkupSelection.Create(ASynEdit : TSynEditBase; ASelection: TSynEditSelection);
begin
  inherited Create(ASynEdit);
  FSelection := ASelection;
  FMarkupInfoSelection := TSynSelectedColor.Create;
  FMarkupInfoSelection.OnChange := @MarkupChangedIntern;
  FMarkupInfoIncr := TSynSelectedColor.Create;
  FMarkupInfoIncr.OnChange := @MarkupChangedIntern;

  MarkupInfo.Style := [];
  MarkupInfo.StyleMask := [];
end;

destructor TSynEditMarkupSelection.Destroy;
begin
  inherited Destroy;
  FreeAndNil(FMarkupInfoIncr);
  FreeAndNil(FMarkupInfoSelection);
end;

procedure TSynEditMarkupSelection.PrepareMarkupForRow(aRow : Integer);
var
  p1, p2 : TPoint;
begin
  nSelStart := 0;
  nSelEnd := 0;

  if (not TSynEdit(SynEdit).HideSelection or TSynEdit(SynEdit).Focused) then begin
    p1 := FSelection.FirstLineBytePos;  // always ordered
    p2 := FSelection.LastLineBytePos;

    if (p1.y > aRow) or (p2.y < aRow) or not (FSelection.SelAvail) then
      exit;

    p1 := LogicalToPhysicalPos(p1);
    p2 := LogicalToPhysicalPos(p2);
    nSelStart := 1;
    nSelEnd := -1; // line end
    if (FSelection.ActiveSelectionMode = smColumn) then begin
      if (p1.X < p2.X) then begin
        nSelStart := p1.X;
        nSelEnd := p2.X;
      end else begin
        nSelStart := p2.X;
        nSelEnd := p1.X;
      end;
    end else if (FSelection.ActiveSelectionMode = smNormal) then begin
      if p1.y = aRow
      then nSelStart := p1.x;
      if p2.y = aRow
      then nSelEnd := p2.x;
    end;
  end;
  MarkupInfo.StartX := nSelStart;
  MarkupInfo.EndX := nSelEnd-1;
end;

function TSynEditMarkupSelection.GetMarkupAttributeAtRowCol(const aRow, aCol : Integer) : TSynSelectedColor;
begin
  result := nil;
  if (aCol >= nSelStart) and ((aCol < nSelEnd) or (nSelEnd < 0))
  then Result := MarkupInfo;
end;

function TSynEditMarkupSelection.GetNextMarkupColAfterRowCol(const aRow, aCol : Integer) : Integer;
begin
  result := -1;
  if (aCol < nSelStart)
  then Result := nSelStart;
  if (aCol < nSelEnd) and (aCol >= nSelStart)
  then result := nSelEnd;
end;

end.

