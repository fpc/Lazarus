{%MainUnit ../comctrls.pp}

{------------------------------------------------------------------------------
   TStatusPanel Constructor
------------------------------------------------------------------------------

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
constructor TStatusPanel.Create(ACollection: TCollection);
var
  AStatusBar: TStatusBar;
begin
  FWidth := 50;
  FBevel := pbLowered;
  FParentBiDiMode := True;
  FAlignment := taLeftJustify;
  FIntfFlag := 0;
  inherited Create(ACollection);
  
  AStatusBar:=StatusBar;
  //debugln('TStatusPanel.Create A ',dbgsName(AStatusBar),' ',dbgs(csDesigning in AStatusBar.ComponentState));
  if (AStatusBar<>nil)
  and ([csLoading,csDestroying]*AStatusBar.ComponentState=[]) then begin
    if (csDesigning in AStatusBar.ComponentState)
    and AStatusBar.SimplePanel and (AStatusBar.Panels.Count=1) then begin
      // first added panel during designing
      AStatusBar.SimplePanel:=false;
    end else begin
      AStatusBar.InvalidatePanel(Index,[ppText]);
    end;
  end;
end;

destructor TStatusPanel.Destroy;
var
  OldStatusBar: TStatusBar;
begin
  //debugln('TStatusPanel.Destroy A ',dbgsName(Self),' StatusBar=',dbgsName(StatusBar));
  OldStatusBar:=StatusBar;
  inherited Destroy;
  if OldStatusBar<>nil then
    OldStatusBar.InvalidatePanel(-1,[ppText]);
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

procedure TStatusPanel.SetIndex(Value: Integer);
var
  OldIndex: LongInt;
  TheStatusBar: TStatusBar;
begin
  OldIndex:=Index;
  if OldIndex=Value then exit;
  //debugln('TStatusPanel.SetIndex A Test=',Text,' Old=',dbgs(OldIndex),' New=',dbgs(Value));
  inherited SetIndex(Value);
  TheStatusBar:=StatusBar;
  if TheStatusBar=nil then exit;
  TheStatusBar.InvalidatePanel(-1,[ppText]);
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

procedure TStatusPanel.SetText(const Value: TCaption);
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
