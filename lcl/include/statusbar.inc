{%MainUnit ../comctrls.pp}
{
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

{------------------------------------------------------------------------------}
{   TStatusBar Constructor                                                     }
{------------------------------------------------------------------------------}
constructor TStatusBar.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FCompStyle := csStatusBar;
  FAutoHint := False;
  FCanvas := TControlCanvas.Create;
  TControlCanvas(FCanvas).Control := Self;
  ControlStyle := [csCaptureMouse, csClickEvents, csDoubleClicks, csOpaque];
  FSimplePanel := True;
  FSizeGrip := True;
  FPanels := CreatePanels;
  Color := {$ifdef UseCLDefault}clDefault{$else}clBtnFace{$endif};
  Align := alBottom;
  AutoSize := True;
end;


{------------------------------------------------------------------------------}
{   TStatusBar SetSimpleText                                                   }
{------------------------------------------------------------------------------}
procedure TStatusBar.SetSimpleText(const Value : TCaption);
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
    UpdateHandleObject(-1, AllPanelsParts);
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

class procedure TStatusBar.WSRegisterClass;
begin
  inherited WSRegisterClass;
  RegisterStatusBar;
end;

function TStatusBar.DoSetApplicationHint(AHintStr: String): Boolean;
begin
  Result := DoHint;
  if Result then
    Exit;
  if SimplePanel then
    SimpleText := AHintStr
  else
  if Panels.Count > 0 then
    Panels[0].Text := AHintStr;
  Result := True;
end;

function TStatusBar.DoHint: Boolean;
begin
  Result := Assigned(FOnHint);
  if Result then
    OnHint(Self);
end;

procedure TStatusBar.DrawPanel(Panel: TStatusPanel; const Rect: TRect);
begin
  if Assigned(FOnDrawPanel) then
    FOnDrawPanel(Self, Panel, Rect);
end;

procedure TStatusBar.LMDrawItem(var Message: TLMDrawItems);
var
  OldHandle: HDC;
begin
  with Message.DrawItemStruct^ do
  begin
    if Canvas.HandleAllocated then
      OldHandle := Canvas.Handle
    else
      OldHandle := 0;
    Canvas.Handle := _hDC;
    DrawPanel(Panels[itemID], rcItem);
    Canvas.Handle := OldHandle;
  end;
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
    UpdateHandleObject(FHandleUpdatePanelIndex, AllPanelsParts);
end;

procedure TStatusBar.DestroyWnd;
begin
  inherited DestroyWnd;
  if FCanvas <> nil then
    TControlCanvas(FCanvas).FreeHandle;
  FHandlePanelCount:=0;
  FHandleObjectNeedsUpdate:=false;
end;

procedure TStatusBar.Loaded;
begin
  inherited Loaded;
  if FHandleObjectNeedsUpdate then
    UpdateHandleObject(FHandleUpdatePanelIndex, AllPanelsParts);
end;

procedure TStatusBar.UpdateHandleObject(PanelIndex: integer; PanelParts: TPanelParts);
begin
  if (not HandleAllocated) or (csDestroying in ComponentState) or
     ((PanelIndex>0) and SimplePanel) then
    Exit;

  if (csLoading in ComponentState) or (FUpdateLock > 0) then
  begin
    //DebugLn('TStatusBar.UpdateHandleObject Caching FHandleObjectNeedsUpdate=',dbgs(FHandleObjectNeedsUpdate),' FHandleUpdatePanelIndex=',dbgs(FHandleUpdatePanelIndex),' PanelIndex=',dbgs(PanelIndex));
    if FHandleObjectNeedsUpdate then
    begin
      // combine multiple updates
      if (FHandleUpdatePanelIndex>=0) and (FHandleUpdatePanelIndex <> PanelIndex) then
        FHandleUpdatePanelIndex:=-1; // at least 2 different panels need update => update all
    end else
    begin
      // start an update sequence
      FHandleObjectNeedsUpdate := True;
      FHandleUpdatePanelIndex := PanelIndex;
    end;
    Exit;
  end;

  //DebugLn('TStatusBar.UpdateHandleObject A FHandlePanelCount=',dbgs(FHandlePanelCount),' PanelIndex=',dbgs(PanelIndex),' Panels.Count=',dbgs(Panels.Count),' SimplePanel=',dbgs(SimplePanel));
  if (FHandlePanelCount > PanelIndex) and (PanelIndex >= 0) then
  begin
    // update one panel
    TWSStatusBarClass(WidgetSetClass).PanelUpdate(Self, PanelIndex);
  end else
  begin
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
      UpdateHandleObject(FHandleUpdatePanelIndex, AllPanelsParts);
  end;
end;

function TStatusBar.ExecuteAction(ExeAction: TBasicAction): Boolean;
var
  HintAction: TCustomHintAction absolute ExeAction;
begin
  if AutoHint and (ExeAction is TCustomHintAction) then
    Result := DoSetApplicationHint(HintAction.Hint)
  else
    Result := inherited ExecuteAction(ExeAction);
end;

function TStatusBar.GetPanelIndexAt(X, Y: Integer): Integer;
var
  R, PanelRect: TRect;
  P: TPoint;
  i: integer;
begin
  Result := -1;
  if Panels.Count = 0 then
    Exit;
  R := GetChildsRect(False);
  P := Point(X, Y);
  if not PtInRect(R, P) then
    Exit;
  PanelRect := R;
  for i := 0 to Panels.Count - 1 do
  begin
    if i <> Panels.Count - 1 then
      PanelRect.Right := PanelRect.Left + Panels[i].Width
    else
      PanelRect.Right := R.Right;
    if PtInRect(PanelRect, P) then
      Exit(i);
    PanelRect.Left := PanelRect.Right;
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
  UpdateHandleObject(PanelIndex, PanelParts);
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
