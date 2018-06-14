{%MainUnit ../stdctrls.pp}
{  $Id$  }

{******************************************************************************
                                     TCustomMemo
 ******************************************************************************
 
 *****************************************************************************
  This file is part of the Lazarus Component Library (LCL)

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************
}

{off $DEFINE DEBUG_MEMO}

constructor TCustomMemo.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fCompStyle := csMemo;
  FWantReturns := True;
  FWantTabs := False;
  FWordWrap := True;
  FLines := TTextStrings.Create;
  FVertScrollbar := TMemoScrollBar.Create(Self, sbVertical);
  FHorzScrollbar := TMemoScrollBar.Create(Self, sbHorizontal);
  AutoSelect := False;
  AutoSize := False;
end;

destructor TCustomMemo.Destroy;
begin
  FreeThenNil(FLines);
  FreeThenNil(FVertScrollbar);
  FreeThenNil(FHorzScrollbar);
  inherited Destroy;
end;

procedure TCustomMemo.Append(const Value: String);
begin
  Lines.Add(Value);
end;

procedure TCustomMemo.ScrollBy(DeltaX, DeltaY: Integer);
begin
  ScrollBy_WS(DeltaX, DeltaY);
end;

procedure TCustomMemo.SetHorzScrollBar(const AValue: TMemoScrollBar);
begin
  if FHorzScrollBar=AValue then exit;
  FHorzScrollBar:=AValue;
end;

{------------------------------------------------------------------------------
  Setter for CaretPos
 ------------------------------------------------------------------------------}
procedure TCustomMemo.SetCaretPos(const Value: TPoint);
begin
  TWSCustomMemoClass(WidgetSetClass).SetCaretPos(Self, Value);
end;

procedure TCustomMemo.SetVertScrollBar(const AValue: TMemoScrollBar);
begin
  if FVertScrollBar=AValue then exit;
  FVertScrollBar:=AValue;
end;

procedure TCustomMemo.SetWantReturns(const AValue: Boolean);
begin
  if FWantReturns = AValue then
    Exit;
  FWantReturns := AValue;
  if HandleAllocated then
    TWSCustomMemoClass(WidgetSetClass).SetWantReturns(Self, AValue);
end;

class procedure TCustomMemo.WSRegisterClass;
begin
  inherited WSRegisterClass;
  RegisterCustomMemo;
end;

function TCustomMemo.CanShowEmulatedTextHint: Boolean;
begin
  Result :=
        Assigned(Lines) // CM_EXIT is sent in destroy -> this function is called in destructor when Lines are already destroyed
    and (Lines.Count = 0)
    and inherited CanShowEmulatedTextHint;
end;

procedure TCustomMemo.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  Params.Style := (Params.Style and not ES_AUTOHSCROLL) or ES_AUTOVSCROLL or
    ES_MULTILINE or ES_WANTRETURN;
  case ScrollBars of
    ssHorizontal, ssAutoHorizontal:
      Params.Style := Params.Style or WS_HSCROLL;
    ssVertical, ssAutoVertical:
      Params.Style := Params.Style or WS_VSCROLL;
    ssBoth, ssAutoBoth:
      Params.Style := Params.Style or WS_HSCROLL or WS_VSCROLL;
  end;
  if WordWrap then
    Params.Style := Params.Style and not WS_HSCROLL
  else
    Params.Style := Params.Style or ES_AUTOHSCROLL;
end;

procedure TCustomMemo.InitializeWnd;
var
  NewStrings : TStrings;
begin
  {$ifdef DEBUG_MEMO}
  DebugLn('[TCustomMemo.InitializeWnd] A ',FLines.ClassName);
  {$endif}
  // fetch/create the interface item list
  NewStrings := TWSCustomMemoClass(WidgetSetClass).GetStrings(Self);
  // copy the items (text)
  NewStrings.Assign(Lines);

  // free old items
  FLines.Free;

  // new item list is the interface item list
  FLines:= NewStrings;

  inherited InitializeWnd;
  {$ifdef DEBUG_MEMO}
  DebugLn('[TCustomMemo.InitializeWnd] END ',DbgSName(Self),' ',FLines.ClassName,' FLines.Count=',dbgs(FLines.Count));
  {$endif}
end;

procedure TCustomMemo.FinalizeWnd;
var
  NewStrings : TStrings;
begin
  if Assigned(FLines) then
  begin
    {$ifdef DEBUG_MEMO}
    DebugLn('[TCustomMemo.FinalizeWnd] A ',DbgSName(Self),' ',FLines.ClassName,' FLines.Count=',dbgs(FLines.Count));
    {$endif}
    // create internal item list
    NewStrings := TTextStrings.Create;

    // copy items (text+objects) from the interface items list
    NewStrings.Assign(Lines);

    // Delete the interface item list
    TWSCustomMemoClass(WidgetSetClass).FreeStrings(FLines);

    // new item list is the internal item list
    FLines := NewStrings;
    {$ifdef DEBUG_MEMO}
    DebugLn('[TCustomMemo.FinalizeWnd] END ',DbgSName(Self),' ',FLines.ClassName,' FLines.Count=',dbgs(FLines.Count));
    {$endif}
  end;
  inherited FinalizeWnd;
end;

function TCustomMemo.RealGetText: TCaption;
begin
  Result := Lines.Text;
  {$ifdef DEBUG_MEMO}
  debugln('TCustomMemo.RealGetText "',Result,'"');
  {$endif}
end;

procedure TCustomMemo.RealSetText(const Value: TCaption);
begin
  {$ifdef DEBUG_MEMO}
  debugln('TCustomMemo.RealSetText "',Value,'"');
  {$endif}
  Lines.Text := Value;
end;

function TCustomMemo.GetCachedText(var CachedText: TCaption): boolean;
begin
  Result:= false;
end;

{------------------------------------------------------------------------------
  Getter for CaretPos
 ------------------------------------------------------------------------------}
function TCustomMemo.GetCaretPos: TPoint;
begin
  Result := TWSCustomMemoClass(WidgetSetClass).GetCaretPos(Self);
end;

{------------------------------------------------------------------------------
  Prevents false firing of the OnEditingDone event if the memo accepts the
  RETURN key for new-line input.
 ------------------------------------------------------------------------------}
procedure TCustomMemo.KeyUpAfterInterface(var Key: Word; Shift: TShiftState);
begin
  if (Key = VK_RETURN) and FWantReturns then
    Key := 0;
  inherited;
end;

procedure TCustomMemo.SetLines(const Value: TStrings);
begin
  if (Value <> nil) then
    FLines.Assign(Value);
end;

procedure TCustomMemo.SetScrollBars(const Value: TScrollStyle);
begin
  if Value <> FScrollbars then begin
    FScrollbars:= Value;
    if HandleAllocated and (not (csLoading in ComponentState)) then
      TWSCustomMemoClass(WidgetSetClass).SetScrollbars(Self, Value);
  end;
end;

procedure TCustomMemo.Loaded;
begin
  inherited Loaded;

  if HandleAllocated then
  begin
    TWSCustomMemoClass(WidgetSetClass).SetScrollbars(Self, FScrollbars);
    TWSCustomMemoClass(WidgetSetClass).SetWordWrap(Self, FWordWrap);
  end;
end;

procedure TCustomMemo.CMWantSpecialKey(var Message: TCMWantSpecialKey);
begin
  case Message.CharCode of
    VK_RETURN: if WantReturns then Message.Result := 1;
    VK_TAB: if WantTabs then Message.Result := 1;
  else
    inherited;
  end;
end;

procedure TCustomMemo.WMGetDlgCode(var Message: TLMGetDlgCode);
begin
  inherited;
  if Message.CharCode = VK_TAB then
  begin
    if WantTabs then
      Message.Result := Message.Result or (DLGC_WANTTAB or DLGC_WANTALLKEYS)
    else
      Message.Result := Message.Result and not (DLGC_WANTTAB or DLGC_WANTALLKEYS);
  end;
  if Message.CharCode = VK_RETURN then
  begin
    if WantReturns then
      Message.Result := Message.Result or DLGC_WANTALLKEYS
    else
      Message.Result := Message.Result and not DLGC_WANTALLKEYS;
  end;
end;

class function TCustomMemo.GetControlClassDefaultSize: TSize;
begin
  Result.CX := 150;
  Result.CY := 90;
end;

procedure TCustomMemo.UTF8KeyPress(var UTF8Key: TUTF8Char);
begin
  inherited UTF8KeyPress(UTF8Key);
  if not WantReturns and (UTF8Key = #13) then
    UTF8Key := '';
end;

procedure TCustomMemo.SetWantTabs(const NewWantTabs: boolean);
begin
  if FWantTabs = NewWantTabs then exit;
  FWantTabs := NewWantTabs;
  if HandleAllocated then
    TWSCustomMemoClass(WidgetSetClass).SetWantTabs(Self, NewWantTabs);
end;

procedure TCustomMemo.SetWordWrap(const Value: boolean);
begin
  if Value <> FWordWrap then
  begin
    {$ifdef DEBUG_MEMO}
    DebugLn(['TCustomMemo.SetWordWrap ',Name,' Old=',FWordWrap,' New=',Value]);
    {$endif}
    FWordWrap := Value;
    if HandleAllocated and (not (csLoading in ComponentState)) then
      TWSCustomMemoClass(WidgetSetClass).SetWordWrap(Self, Value);
  end;    
end;

// included by stdctrls.pp

