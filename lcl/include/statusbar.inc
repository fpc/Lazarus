// included by comctrls.pp
{
 *****************************************************************************
 *                                                                           *
 *  This file is part of the Lazarus Component Library (LCL)                 *
 *                                                                           *
 *  See the file COPYING.LCL, included in this distribution,                 *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************
}

{------------------------------------------------------------------------------}
{   TStatusBar Constructor                                                     }
{------------------------------------------------------------------------------}
constructor TStatusBar.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
//  fCompStyle := csStatusBar;
  ControlStyle:= [csCaptureMouse, csClickEvents, csDoubleClicks, csOpaque];
  Color := clBtnFace;
  Alignment:= taLeftJustify;
  BevelOuter:= bvLowered;
  BevelInner:= bvNone;
  if Owner is TControl then 
    Setbounds(0, TControl(AOwner).Height - 21, TControl(AOwner).Width, 20)
  else
    Setbounds(0, 0, 20, 20);
    
  Align:= alBottom;
  FPanels := TStatusPanels.Create(Self);
  FCanvas := TControlCanvas.Create;
  TControlCanvas(FCanvas).Control := Self;
  FSimplePanel:= True;
//  FSizeGrip := True;
end;

{------------------------------------------------------------------------------}
{   TStatusBar Destructor                                                      }
{------------------------------------------------------------------------------}
destructor TStatusBar.Destroy;
begin
  FPanels.Free;
  FCanvas.Free;
  inherited Destroy;
end;

{------------------------------------------------------------------------------}
{   TStatusBar SetSimpleText                                                   }
{------------------------------------------------------------------------------}
procedure TStatusBar.SetSimpleText(Value : String);
begin
   if FSimpleText <> value then
   begin
      Caption:= Value;
//      Invalidate;
   end;
end;

procedure TStatusBar.SetSimplePanel(Value : Boolean);
begin
  if FSimplePanel <> Value then
  begin
    FSimplePanel := Value;
    Invalidate;
  end;
end;

procedure TStatusBar.SetPanels(Value: TStatusPanels);
begin
  FPanels.Assign(Value);
end;

// included by comctrls.pp

