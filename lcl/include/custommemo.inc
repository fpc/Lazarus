{%MainUnit ../stdctrls.pp}
{  $Id$  }

{******************************************************************************
                                     TCustomMemo
 ******************************************************************************
 
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

{------------------------------------------------------------------------------
  Method: TCustomMemo.Create
  Params: 
  Returns: 

  Constructor for the class
 ------------------------------------------------------------------------------}
constructor TCustomMemo.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fCompStyle := csMemo;
  FWantReturns := true;
  FWantTabs := false;
  FWordWrap := True;
  //FLines := TMemoStrings.Create(Self);
  FLines := TStringList.Create; //TODO finish TTextStrings and add them here instead of TStringList
  //TMemoStrings(FLines).MemoWidgetClass := TWSCustomMemoClass(WidgetSetClass);
  FVertScrollbar := TMemoScrollBar.Create(Self, sbVertical);
  FHorzScrollbar := TMemoScrollBar.Create(Self, sbHorizontal);
end;

{------------------------------------------------------------------------------
  Method: TCustomMemo.Destroy
  Params:  None
  Returns: Nothing

  Destructor for the class.
 ------------------------------------------------------------------------------}
destructor TCustomMemo.Destroy;
begin
  FreeThenNil(FLines);
  FreeThenNil(FVertScrollbar);
  FreeThenNil(FHorzScrollbar);
  inherited Destroy;
end;

{------------------------------------------------------------------------------
  Method: TCustomMemo.Append
  Params: 
  Returns: 

 ------------------------------------------------------------------------------}
procedure TCustomMemo.Append(const Value : String);
begin
  Lines.Add(Value);
end;

{------------------------------------------------------------------------------
  procedure TCustomMemo.SetHorzScrollBar(const AValue: TMemoScrollBar);
 ------------------------------------------------------------------------------}
procedure TCustomMemo.SetHorzScrollBar(const AValue: TMemoScrollBar);
begin
  if FHorzScrollBar=AValue then exit;
  FHorzScrollBar:=AValue;
end;

procedure TCustomMemo.SetAlignment(const AValue: TAlignment);
begin
  if FAlignment = AValue then
    exit;
  FAlignment := AValue;
  if HandleAllocated then
    TWSCustomMemoClass(WidgetSetClass).SetAlignment(Self, AValue);
end;

{------------------------------------------------------------------------------
  Setter for CaretPos
 ------------------------------------------------------------------------------}
procedure TCustomMemo.SetCaretPos(const Value: TPoint);
begin
  TWSCustomMemoClass(WidgetSetClass).SetCaretPos(Self, Value);
end;

{------------------------------------------------------------------------------
  procedure TCustomMemo.SetVertScrollBar(const AValue: TMemoScrollBar);
 ------------------------------------------------------------------------------}
procedure TCustomMemo.SetVertScrollBar(const AValue: TMemoScrollBar);
begin
  if FVertScrollBar=AValue then exit;
  FVertScrollBar:=AValue;
end;

procedure TCustomMemo.SetWantReturns(const AValue: Boolean);
begin
  if FWantReturns = AValue then
    exit;
  FWantReturns := AValue;
  if HandleAllocated then
    TWSCustomMemoClass(WidgetSetClass).SetWantReturns(Self, AValue);
end;

{------------------------------------------------------------------------------
  function TCustomMemo.StoreScrollBars: boolean;
 ------------------------------------------------------------------------------}
function TCustomMemo.StoreScrollBars: boolean;
begin
  Result:=true;
end;

procedure TCustomMemo.CreateHandle;
var
  NewStrings : TStrings;
begin
  //DebugLn('[TCustomMemo.CreateHandle] A ',FLines.ClassName);
  inherited CreateHandle;
  //DebugLn('[TCustomMemo.CreateHandle] B ',DbgSName(Self),' ',FLines.ClassName,' FLines.Count=',dbgs(FLines.Count));
  
  // fetch/create the interface item list
  NewStrings := TWSCustomMemoClass(WidgetSetClass).GetStrings(Self);
  // copy the items (text)
  NewStrings.Assign(Lines);

  // free old items
  FLines.Free;

  // new item list is the interface item list
  FLines:= NewStrings;

  //DebugLn('[TCustomMemo.CreateHandle] END ',DbgSName(Self),' ',FLines.ClassName,' FLines.Count=',dbgs(FLines.Count));
end;

procedure TCustomMemo.DestroyHandle;
var
  NewStrings : TStringList;

begin
  if Assigned(FLines) then begin
    // create internal item list
    NewStrings:= TStringList.Create;

    // copy items (text+objects) from the interface items list
    NewStrings.Assign(Lines);

    FLines.Free;
    // new item list is the internal item list
    FLines:= NewStrings;
  end;
  inherited DestroyHandle;
end;

function TCustomMemo.RealGetText: TCaption;
begin
  Result := Lines.Text;
  //debugln('TCustomMemo.RealGetText "',Result,'"');
end;

procedure TCustomMemo.RealSetText(const Value: TCaption);
begin
  //debugln('TCustomMemo.RealSetText "',Value,'"');
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
  Method: TCustomMemo.SetLines
  Params: 
  Returns: 

 ------------------------------------------------------------------------------}
procedure TCustomMemo.SetLines(const Value : TStrings);
begin
  if (Value <> nil) then
    FLines.Assign(Value);
end;

procedure TCustomMemo.SetSelText(const Val: string);
begin
  Lines.BeginUpdate;
  try
    inherited SetSelText(Val);
  finally
    Lines.EndUpdate;
  end;
end;

{------------------------------------------------------------------------------
  procedure TCustomMemo.SetScrollbars(const Value : TScrollStyle);
 ------------------------------------------------------------------------------}
procedure TCustomMemo.SetScrollbars(const Value : TScrollStyle);
begin
  if Value <> FScrollbars then begin
    FScrollbars:= Value;
    if HandleAllocated and (not (csLoading in ComponentState)) then
      TWSCustomMemoClass(WidgetSetClass).SetScrollbars(Self, Value);
  end;
end;

{------------------------------------------------------------------------------
  procedure TCustomMemo.Loaded;
 ------------------------------------------------------------------------------}
procedure TCustomMemo.Loaded;
begin
  inherited Loaded;

  if HandleAllocated then
  begin
    TWSCustomMemoClass(WidgetSetClass).SetScrollbars(Self, FScrollbars);
    TWSCustomMemoClass(WidgetSetClass).SetWordWrap(Self, FWordWrap);
  end;
end;

{------------------------------------------------------------------------------
  function TCustomMemo.WordWrapIsStored: boolean;
 ------------------------------------------------------------------------------}
function TCustomMemo.WordWrapIsStored: boolean;
begin
  Result:=not WordWrap;
end;

procedure TCustomMemo.ControlKeyDown(var Key: Word; Shift: TShiftState);
begin
  if not ReadOnly then
  begin
    if FWantReturns and (Key=VK_RETURN) and (Shift=[]) then
      exit;
    if FWantTabs and (Key=VK_TAB) and (Shift-[ssShift]=[]) then
      exit;
  end;
  inherited ControlKeyDown(Key, Shift);
end;

procedure TCustomMemo.CNChar(var Message: TLMKeyUp);
begin
  inherited CNChar(Message);
  
  if not FWantReturns and (Message.CharCode = VK_RETURN) then
  begin
    Message.CharCode := VK_UNKNOWN;
    Message.Result := 1;
  end;
end;

class function TCustomMemo.GetControlClassDefaultSize: TPoint;
begin
  Result.X:=150;
  Result.Y:=90;
end;

procedure TCustomMemo.SetWantTabs(const NewWantTabs: boolean);
begin
  if FWantTabs = NewWantTabs then exit;
  FWantTabs := NewWantTabs;
  if HandleAllocated then
    TWSCustomMemoClass(WidgetSetClass).SetWantTabs(Self, NewWantTabs);
end;

{------------------------------------------------------------------------------
  Method: TCustomMemo.SetWordWrap
  Params: 
  Returns: 

 ------------------------------------------------------------------------------}
procedure TCustomMemo.SetWordWrap(const Value : boolean);
begin
  if Value <> FWordWrap then begin
    //DebugLn('TCustomMemo.SetWordWrap ',Name,' Old=',FWordWrap,' New=',Value);
    FWordWrap := Value;
    if HandleAllocated and (not (csLoading in ComponentState)) then
      TWSCustomMemoClass(WidgetSetClass).SetWordWrap(Self, Value);
  end;    
end;

// included by stdctrls.pp

