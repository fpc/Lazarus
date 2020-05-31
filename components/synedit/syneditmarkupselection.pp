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
    FColorTillEol: boolean; // colorize selection only till EOL
    FMarkupInfoIncr: TSynSelectedColor; // Markup during incremental search
    FMarkupInfoSelection: TSynSelectedColor; // Markup for normal Selection
    FUseIncrementalColor : Boolean;
    nSelStart, nSelEnd: integer; // start, end of selected area in current line (physical)
    procedure SetColorTillEol(AValue: boolean);
    procedure SetUseIncrementalColor(const AValue : Boolean);
    procedure MarkupChangedIntern(AMarkup: TObject);
  protected
    procedure DoMarkupChanged(AMarkup: TSynSelectedColor); override;
    procedure DoEnabledChanged(Sender: TObject); override;
  public
    constructor Create(ASynEdit : TSynEditBase; ASelection: TSynEditSelection);
    destructor Destroy; override;

    procedure PrepareMarkupForRow(aRow : Integer); override;
    function GetMarkupAttributeAtRowCol(const aRow: Integer;
                                        const aStartCol: TLazSynDisplayTokenBound;
                                        const AnRtlInfo: TLazSynDisplayRtlInfo): TSynSelectedColor; override;
    procedure GetNextMarkupColAfterRowCol(const aRow: Integer;
                                         const aStartCol: TLazSynDisplayTokenBound;
                                         const AnRtlInfo: TLazSynDisplayRtlInfo;
                                         out   ANextPhys, ANextLog: Integer); override;

    property ColorTillEol: boolean read FColorTillEol write SetColorTillEol;
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

procedure TSynEditMarkupSelection.SetColorTillEol(AValue: boolean);
begin
  if FColorTillEol = AValue then Exit;
  FColorTillEol := AValue;
  DoMarkupChanged(nil);
end;

procedure TSynEditMarkupSelection.MarkupChangedIntern(AMarkup : TObject);
begin
  if FUseIncrementalColor
  then MarkupInfo.Assign(FMarkupInfoIncr)
  else MarkupInfo.Assign(FMarkupInfoSelection);
end;

procedure TSynEditMarkupSelection.DoMarkupChanged(AMarkup: TSynSelectedColor);
var
  p1, p2 : TPoint;
begin
  inherited DoMarkupChanged(AMarkup);
  if (not FSelection.SelAvail) or (TCustomSynEdit(SynEdit).HideSelection and not TCustomSynEdit(SynEdit).Focused) then
    exit;

  p1 := FSelection.FirstLineBytePos;  // always ordered
  p2 := FSelection.LastLineBytePos;
  InvalidateSynLines(p1.y, p2.y);
end;

procedure TSynEditMarkupSelection.DoEnabledChanged(Sender: TObject);
begin
  DoMarkupChanged(nil);
end;

constructor TSynEditMarkupSelection.Create(ASynEdit : TSynEditBase; ASelection: TSynEditSelection);
begin
  inherited Create(ASynEdit);
  FSelection := ASelection;
  FMarkupInfoSelection := TSynSelectedColor.Create;
  FMarkupInfoSelection.OnChange := @MarkupChangedIntern;
  FMarkupInfoIncr := TSynSelectedColor.Create;
  FMarkupInfoIncr.OnChange := @MarkupChangedIntern;
  FColorTillEol := false;

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

  if (not TCustomSynEdit(SynEdit).HideSelection or TCustomSynEdit(SynEdit).Focused) then begin
    p1 := FSelection.FirstLineBytePos;  // always ordered
    p2 := FSelection.LastLineBytePos;

    if (p1.y > aRow) or (p2.y < aRow) or not (FSelection.SelAvail) then
      exit;

    nSelStart := 1;
    nSelEnd := -1; // line end
    if (FSelection.ActiveSelectionMode = smColumn) then begin
      nSelStart := FSelection.ColumnStartBytePos[aRow];
      nSelEnd   := FSelection.ColumnEndBytePos[aRow];
      //p1 := LogicalToPhysicalPos(p1);
      //p2 := LogicalToPhysicalPos(p2);
      //if (p1.X < p2.X) then begin
      //  nSelStart := p1.X;
      //  nSelEnd := p2.X;
      //end else begin
      //  nSelStart := p2.X;
      //  nSelEnd := p1.X;
      //end;
    end else if (FSelection.ActiveSelectionMode = smNormal) then begin
      if p1.y = aRow then begin
//        p1 := LogicalToPhysicalPos(p1);
        nSelStart := p1.x;
      end;
      if p2.y = aRow then begin
//        p2 := LogicalToPhysicalPos(p2);
        nSelEnd := p2.x;
      end;

      //colorize selected block only till EOL, not till edge of control
      if FColorTillEol then begin
        p2.x := Length(Lines[aRow-1]) + 1;
        p2.y := aRow;
//        p2 := LogicalToPhysicalPos(p2);
        if (nSelEnd = -1) then
          Inc(p2.x, 1);

        if (nSelEnd = -1) or (nSelEnd > p2.x) then
          nSelEnd := p2.x;
      end;

    end;
  end;
  MarkupInfo.SetFrameBoundsPhys(nSelStart, nSelEnd);
end;

function TSynEditMarkupSelection.GetMarkupAttributeAtRowCol(const aRow: Integer;
  const aStartCol: TLazSynDisplayTokenBound; const AnRtlInfo: TLazSynDisplayRtlInfo): TSynSelectedColor;
begin
  result := nil;

  if (nSelStart <= aStartCol.Logical) and ((nSelEnd > aStartCol.Logical) or (nSelEnd < 0)) then
    Result := MarkupInfo;

  //if AnRtlInfo.IsRtl then begin
  //  if ( ((nSelStart >= aStartCol.Physical) and (nSelStart < AnRtlInfo.PhysRight) ) or
  //        (nSelStart <= AnRtlInfo.PhysLeft)
  //     ) and
  //     ( ((nSelEnd < aStartCol.Physical) and (nSelEnd > AnRtlInfo.PhysLeft)) or
  //        (nSelEnd >= AnRtlInfo.PhysRight) or (nSelEnd < 0))
  //  then
  //    Result := MarkupInfo;
  //end else begin
  //  if (nSelStart <= aStartCol.Physical) and
  //    ((nSelEnd > aStartCol.Physical) or (nSelEnd < 0))
  //  then
  //    Result := MarkupInfo;
  //end;
end;

procedure TSynEditMarkupSelection.GetNextMarkupColAfterRowCol(const aRow: Integer;
  const aStartCol: TLazSynDisplayTokenBound; const AnRtlInfo: TLazSynDisplayRtlInfo; out ANextPhys,
  ANextLog: Integer);
begin
  ANextLog := -1;
  ANextPhys := -1;

  if nSelStart > aStartCol.Logical then
    ANextLog := nSelStart
  else
  if nSelEnd > aStartCol.Logical then
    ANextLog := nSelEnd;

  //if AnRtlInfo.IsRtl then begin
  //  if (nSelStart < aStartCol.Physical) then
  //    ANextPhys := nSelStart;
  //  if (nSelEnd < aStartCol.Physical) and (nSelEnd > 0)  and
  //     (  (nSelStart >= aStartCol.Physical) or
  //       ((nSelStart <= AnRtlInfo.PhysLeft) and (nSelStart > 0))  )
  //  then
  //    ANextPhys := nSelEnd;
  //end else begin
  //  if (nSelStart > aStartCol.Physical) then
  //    ANextPhys := nSelStart;
  //  if (nSelEnd > aStartCol.Physical) and (nSelStart <= aStartCol.Physical) then
  //    ANextPhys := nSelEnd;
  //end;
end;

end.

