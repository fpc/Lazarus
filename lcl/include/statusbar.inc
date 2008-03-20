{%MainUnit ../comctrls.pp}
{
 *****************************************************************************
 *                                                                           *
 *  This file is part of the Lazarus Component Library (LCL)                 *
 *                                                                           *
 *  See the file COPYING.modifiedLGPL, included in this distribution,        *
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
constructor TStatusBar.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  fCompStyle := csStatusBar;
  FCanvas := TControlCanvas.Create;
  TControlCanvas(FCanvas).Control := Self;
  ControlStyle := [csCaptureMouse, csClickEvents, csDoubleClicks, csOpaque];
  FSimplePanel := True;
  FSizeGrip := True;
  FPanels := CreatePanels;
  Color := clBtnFace;
  Align := alBottom;
  AutoSize := True;
end;


{------------------------------------------------------------------------------}
{   TStatusBar SetSimpleText                                                   }
{------------------------------------------------------------------------------}
procedure TStatusBar.SetSimpleText(const Value : String);
begin
  if FSimpleText <> value then
  begin
    FSimpleText := Value;
    if HandleAllocated and FSimplePanel then 
      TWSStatusBarClass(WidgetSetClass).SetPanelText(Self, 0);
  end;
end;

procedure TStatusBar.SetSimplePanel(Value : Boolean);
begin
  if FSimplePanel <> Value then
  begin
    FSimplePanel := Value;
    //debugln('TStatusBar.SetSimplePanel FSimplePanel=',dbgs(FSimplePanel),' ',dbgsName(Self));
    UpdateHandleObject(-1);
  end;
end;

procedure TStatusBar.SetSizeGrip(const AValue: Boolean);
begin
  if FSizeGrip = AValue then
    Exit;
  FSizeGrip := AValue;
  if HandleAllocated then
    TWSStatusBarClass(WidgetSetClass).SetSizeGrip(Self, AValue);
end;

procedure TStatusBar.BoundsChanged;
begin
  inherited BoundsChanged;
  if HandleAllocated then
    TWSStatusBarClass(WidgetSetClass).SetSizeGrip(Self, SizeGrip);
end;

procedure TStatusBar.SetPanels(Value: TStatusPanels);
begin
  FPanels.Assign(Value);
end;

{------------------------------------------------------------------------------}
{   TStatusBar Destructor                                                      }
{------------------------------------------------------------------------------}
destructor TStatusBar.Destroy;
begin
  FreeThenNil(FCanvas);
  FreeThenNil(FPanels);
  inherited Destroy;
end;

procedure TStatusBar.CreateWnd;
begin
  inherited CreateWnd;
  if FHandleObjectNeedsUpdate then
    UpdateHandleObject(FHandleUpdatePanelIndex);
end;

procedure TStatusBar.DestroyWnd;
begin
  inherited DestroyWnd;
  FHandlePanelCount:=0;
  FHandleObjectNeedsUpdate:=false;
end;

procedure TStatusBar.Loaded;
begin
  inherited Loaded;
  if FHandleObjectNeedsUpdate then
    UpdateHandleObject(FHandleUpdatePanelIndex);
end;

procedure TStatusBar.UpdateHandleObject(PanelIndex: integer);
begin
  if (not HandleAllocated) or (csDestroying in ComponentState)
  or ((PanelIndex>0) and SimplePanel) then exit;

  if (csLoading in ComponentState) or (FUpdateLock>0) then begin
    //DebugLn('TStatusBar.UpdateHandleObject Caching FHandleObjectNeedsUpdate=',dbgs(FHandleObjectNeedsUpdate),' FHandleUpdatePanelIndex=',dbgs(FHandleUpdatePanelIndex),' PanelIndex=',dbgs(PanelIndex));
    if FHandleObjectNeedsUpdate then begin
      // combine multiple updates
      if (FHandleUpdatePanelIndex>=0)
      and (FHandleUpdatePanelIndex<>PanelIndex) then begin
        // at least 2 different panels need update => update all
        FHandleUpdatePanelIndex:=-1; // update all
      end;
    end else begin
      // start an update sequence
      FHandleObjectNeedsUpdate:=true;
      FHandleUpdatePanelIndex:=PanelIndex;
    end;
    exit;
  end;

  //DebugLn('TStatusBar.UpdateHandleObject A FHandlePanelCount=',dbgs(FHandlePanelCount),' PanelIndex=',dbgs(PanelIndex),' Panels.Count=',dbgs(Panels.Count),' SimplePanel=',dbgs(SimplePanel));
  if (FHandlePanelCount>PanelIndex) and (PanelIndex>=0) then begin
    // update one panel
    TWSStatusBarClass(WidgetSetClass).PanelUpdate(Self,PanelIndex);
  end else begin
    // update all panels
    //DebugLn('TStatusBar.UpdateHandleObject C update all panels');
    TWSStatusBarClass(WidgetSetClass).Update(Self);
    if SimplePanel then
      FHandlePanelCount := 1
    else
      FHandlePanelCount := Panels.Count;
  end;
  FHandleObjectNeedsUpdate := False;
end;

procedure TStatusBar.CalculatePreferredSize(var PreferredWidth,
  PreferredHeight: integer; WithThemeSpace: Boolean);
begin
  inherited CalculatePreferredSize(PreferredWidth, PreferredHeight, WithThemeSpace);
  PreferredWidth := 0;
  if PreferredHeight <= 0 then
    PreferredHeight := 25;
end;

function TStatusBar.ColorIsStored: boolean;
begin
  Result:=(Color<>clBtnFace);
  if Result and ParentColor and (Parent<>nil) then
    Result:=false;
end;

procedure TStatusBar.BeginUpdate;
begin
  inc(FUpdateLock);
  if FUpdateLock=1 then
    Panels.BeginUpdate;
end;

procedure TStatusBar.EndUpdate;
begin
  if FUpdateLock<=0 then RaiseGDBException('TStatusBar.EndUpdate');
  if FUpdateLock=1 then begin
    // end update in Panels before decreasing FUpdateLock, so that
    // multiple changes of Panels will be combined
    Panels.EndUpdate;
  end;
  dec(FUpdateLock);
  if (FUpdateLock=0) then begin
    if FHandleObjectNeedsUpdate then
      UpdateHandleObject(FHandleUpdatePanelIndex);
  end;
end;

function TStatusBar.SizeGripEnabled: Boolean;
begin
  Result := (Parent <> nil) and
            (Parent is TCustomForm) and
            (TCustomForm(Parent).BorderStyle in [bsSizeable, bsSizeToolWin]) and
            (Align = alBottom);
end;

function TStatusBar.UpdatingStatusBar: boolean;
begin
  Result:=FUpdateLock>0;
end;

{------------------------------------------------------------------------------
  procedure TStatusBar.InvalidatePanel(PanelIndex: integer;
    PanelParts: TPanelParts);
------------------------------------------------------------------------------}
procedure TStatusBar.InvalidatePanel(PanelIndex: integer;
  PanelParts: TPanelParts);
begin
  if (PanelParts=[]) then exit;
  UpdateHandleObject(PanelIndex);
end;

{------------------------------------------------------------------------------
  function TStatusBar.CreatePanel(): TStatusPanel;
------------------------------------------------------------------------------}
function TStatusBar.CreatePanel: TStatusPanel;
var
  AClass: TStatusPanelClass;
begin
  AClass := GetPanelClass;
  if Assigned(FOnCreatePanelClass) then
    OnCreatePanelClass(Self, AClass);
  Result := AClass.Create(Panels);
end;
{------------------------------------------------------------------------------
  function TStatusBar.CreatePanels(): TStatusPanels;
------------------------------------------------------------------------------}
function TStatusBar.CreatePanels: TStatusPanels;
begin
  Result := TStatusPanels.Create(Self);
end;
{------------------------------------------------------------------------------
  function TStatusBar.GetPanelClass(): TStatusPanelClass;
------------------------------------------------------------------------------}
function TStatusBar.GetPanelClass: TStatusPanelClass;
begin
  Result := TStatusPanel;
end;

// included by comctrls.pp
