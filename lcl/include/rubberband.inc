{%MainUnit ../RubberBand.pas}

{******************************************************************************
                              TCustomRubberBand
 ******************************************************************************

 *****************************************************************************
 *                                                                           *
 *  This file is part of the Lazarus Component Library (LCL)                 *
 *                                                                           *
 *  See the file COPYING.modifiedLGPL.txt, included in this distribution,    *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************
}

{ TCustomRubberBand }

function TCustomRubberBand.GetShape: TRubberBandShape;
begin
  Result := FShape;
end;

procedure TCustomRubberBand.SetShape(const AValue: TRubberBandShape);
begin
  if FShape = AValue then
    Exit;
  FShape := AValue;
  if HandleAllocated then
    TWsCustomRubberBandClass(WidgetsetClass).SetShape(Self, FShape);
end;

class procedure TCustomRubberBand.WSRegisterClass;
begin
  inherited WSRegisterClass;
  RegisterCustomRubberBand;
end;

constructor TCustomRubberBand.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FShape := rbsLine;
end;

