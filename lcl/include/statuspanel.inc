{------------------------------------------------------------------------------
   TStatusPanel Constructor
------------------------------------------------------------------------------

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
constructor TStatusPanel.Create(aCollection: TCollection);
begin
  FWidth := 50;
  FBevel := pbLowered;
  FParentBiDiMode := True;
  FAlignment := taLeftJustify;
  inherited Create(aCollection);
end;

procedure TStatusPanel.Assign(Source: TPersistent);
begin
  if Source is TStatusPanel then
  begin
    Text := TStatusPanel(Source).Text;
    Width := TStatusPanel(Source).Width;
    Alignment := TStatusPanel(Source).Alignment;
    Bevel := TStatusPanel(Source).Bevel;
    Style := TStatusPanel(Source).Style;
  end
  else inherited Assign(Source);
end;

function TStatusPanel.StatusBar: TStatusBar;
begin
  if (Collection is TStatusPanels) then
    Result:=TStatusPanels(Collection).StatusBar
  else
    Result:=nil;
end;

function TStatusPanel.GetDisplayName: string;
begin
  Result := Text;
  if Result = '' then Result := inherited GetDisplayName;
end;

procedure TStatusPanel.PanelChanged(const Parts: TPanelParts);
var
  TheStatusBar: TStatusBar;
begin
  TheStatusBar:=StatusBar;
  if TheStatusBar=nil then exit;
  TheStatusBar.InvalidatePanel(Index,Parts);
end;

procedure TStatusPanel.SetAlignment(Value: TAlignment);
begin
  if FAlignment <> Value then
  begin
    FAlignment := Value;
    Changed(False);
    PanelChanged([ppText]);
  end;
end;

procedure TStatusPanel.SetBevel(Value: TStatusPanelBevel);
begin
  if FBevel <> Value then
  begin
    FBevel := Value;
    Changed(False);
    PanelChanged([ppBorder]);
  end;
end;

procedure TStatusPanel.SetStyle(Value: TStatusPanelStyle);
begin
  if FStyle <> Value then
  begin
    FStyle := Value;
    Changed(False);
    PanelChanged([ppBorder]);
  end;
end;

procedure TStatusPanel.SetText(const Value: string);
begin
  if FText <> Value then
  begin
    FText := Value;
    Changed(False);
    PanelChanged([ppText]);
  end;
end;

procedure TStatusPanel.SetWidth(Value: Integer);
begin
  if FWidth <> Value then
  begin
    FWidth := Value;
    Changed(True);
    PanelChanged([ppWidth]);
  end;
end;

