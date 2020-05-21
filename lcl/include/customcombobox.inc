{%MainUnit ../stdctrls.pp}

{******************************************************************************
                               TCustomComboBox
 ******************************************************************************

 *****************************************************************************
  This file is part of the Lazarus Component Library (LCL)

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************

}

{------------------------------------------------------------------------------
  Method: TCustomComboBox.CreateWnd
  Params: ---
  Returns: Nothing

  Create the underlying interface-object.
 ------------------------------------------------------------------------------}
procedure TCustomComboBox.InitializeWnd;
var
  NewStrings: TStrings;
  ASelStart, ASelLength : integer;
begin
  inherited InitializeWnd;

  // get the interface based item list
  NewStrings:= TWSCustomComboBoxClass(WidgetSetClass).GetItems(Self);
  // then delete internal list
  if (FItems<>NewStrings) and (FItems<>nil) then
  begin
    NewStrings.Assign(FItems);
    FItems.Free;
  end;
  // and use the interface based list
  FItems := NewStrings;

  if (FItemIndex > -1) and (FItemIndex < FItems.Count) then
    TWSCustomComboBoxClass(WidgetSetClass).SetItemIndex(Self, FItemIndex);
  TWSCustomComboBoxClass(WidgetSetClass).SetStyle(Self, FStyle);
  TWSCustomComboBoxClass(WidgetSetClass).SetArrowKeysTraverseList(Self, FArrowKeysTraverseList);
  TWSCustomComboBoxClass(WidgetSetClass).SetMaxLength(Self, FMaxLength);
  TWSCustomComboBoxClass(WidgetSetClass).SetDropDownCount(Self, FDropDownCount);

  if FSelStart <> FSelLength then
  begin
    ASelStart:= FSelStart;
    ASelLength:= FSelLength;
    SelStart:= ASelStart;
    SelLength:= ASelLength;
  end;
end;

{------------------------------------------------------------------------------
  Method: TCustomComboBox.DestroyWnd
  Params: ---
  Returns: Nothing

  Destroy the underlying interface-object.
 ------------------------------------------------------------------------------}
procedure TCustomComboBox.DestroyWnd;
var
  NewStrings: TStrings;
begin
  if not HandleAllocated then RaiseGDBException('');
  // store itemindex
  FItemIndex := TWSCustomComboBoxClass(WidgetSetClass).GetItemIndex(Self);
  // create an internal list for storing items internally
  NewStrings := TStringList.Create;
  // copy from interface based list
  if FItems <> nil then
  begin
    NewStrings.Assign(FItems);
    // delete interface based list
    TWSCustomComboBoxClass(WidgetSetClass).FreeItems(FItems);
  end;
  // and use the internal list
  FItems := NewStrings;
  TStringList(FItems).Sorted := Sorted;

  inherited DestroyWnd;
end;

procedure TCustomComboBox.DoAutoAdjustLayout(
  const AMode: TLayoutAdjustmentPolicy; const AXProportion, AYProportion: Double
  );
begin
  inherited;

  if AMode in [lapAutoAdjustWithoutHorizontalScrolling, lapAutoAdjustForDPI] then
  begin
    if (Style in [csOwnerDrawFixed, csOwnerDrawVariable, csOwnerDrawEditableFixed, csOwnerDrawEditableVariable])
    and (FItemHeight > 0) then
      ItemHeight := Round(ItemHeight * AYProportion);
  end;
end;

{------------------------------------------------------------------------------
  procedure TCustomComboBox.DrawItem(Index: Integer; Rect: TRect;
    State: TOwnerDrawState);
 ------------------------------------------------------------------------------}
procedure TCustomComboBox.DrawItem(Index: Integer; ARect: TRect;
  State: TOwnerDrawState);
begin
  //TControlCanvas(FCanvas).UpdateTextFlags;
  if Assigned(FOnDrawItem) then
    FOnDrawItem(Self, Index, ARect, State)
  else
  begin
    if not (odBackgroundPainted in State) then
      FCanvas.FillRect(ARect);
    InternalDrawItem(Self, FCanvas, ARect, Items[Index])
  end;
end;

class function TCustomComboBox.GetControlClassDefaultSize: TSize;
begin
  Result.CX := 100;
  Result.CY := 25;
end;

procedure TCustomComboBox.DoEnter;
begin
  inherited DoEnter;
  //AutoSelect when DoEnter is fired by keyboard
  if (Style = csDropDownList) then Exit;//Non editable style
  if (FAutoSelect and not (csLButtonDown in ControlState)) then
  begin
   SelectAll;
   if (SelText = Text) then FAutoSelected := True;
  end;//End if (((Style = csDropDown) or.........
end;

procedure TCustomComboBox.DoExit;
begin
  FAutoSelected := False;
  inherited DoExit;
end;

{------------------------------------------------------------------------------
  Method: TCustomComboBox.SetSorted
  Params: val - true means "sort" the combo
  Returns: Nothing

  Set the "sorted" property of the combobox and Sort the current entries.
 ------------------------------------------------------------------------------}
procedure TCustomComboBox.SetSorted(Val : boolean);
begin
  if (Val <> FSorted) then
  begin
    FSorted := Val;
    UpdateSorted;
  end;
end;

{------------------------------------------------------------------------------
  Method: TCustomComboBox.SetMaxLength
  Params: val -
  Returns: Nothing

  Set the maximum length for user input.
 ------------------------------------------------------------------------------}
procedure TCustomComboBox.SetMaxLength(AValue: integer);
begin
  if AValue < 0 then
    AValue := 0;
  if AValue <> MaxLength then
  begin
    FMaxlength := AValue;
    if HandleAllocated then
      TWSCustomComboBoxClass(WidgetSetClass).SetMaxLength(Self, AValue);
  end;
end;

{------------------------------------------------------------------------------
  Method: TCustomComboBox.GetMaxLength
  Params: ---
  Returns: the maximum length of user input

  Get the maximum length for user input.
 ------------------------------------------------------------------------------}
function TCustomComboBox.GetMaxLength: integer;
begin
  if HandleAllocated then
    FMaxLength := TWSCustomComboBoxClass(WidgetSetClass).GetMaxLength(Self);
  Result := FMaxLength;
end;

{------------------------------------------------------------------------------
  Method: TCustomComboBox.LMChanged
  Params: msg -
  Returns: Nothing

  Call handler for "OnChange"-event if one is assigned.
 ------------------------------------------------------------------------------}
procedure TCustomComboBox.LMChanged(var Msg);
begin
  Change;
end;

{------------------------------------------------------------------------------
  procedure TCustomComboBox.Change;

  Called on change
 ------------------------------------------------------------------------------}
procedure TCustomComboBox.Change;
begin
  inherited Changed;
  if Assigned(FOnChange) then FOnChange(Self);
end;

{------------------------------------------------------------------------------
  procedure TCustomComboBox.Select;

  Called whenever User changes the ItemIndex
  For Delphi compatibility ignore when user unselects by changing Text.
 ------------------------------------------------------------------------------}
procedure TCustomComboBox.Select;
begin
  if Assigned(FOnSelect) and (ItemIndex >= 0) then
    FOnSelect(Self);
end;

{------------------------------------------------------------------------------
  procedure TCustomComboBox.DropDown;

  Called whenever the list popups.
 ------------------------------------------------------------------------------}
procedure TCustomComboBox.DropDown;
begin
  if Assigned(FOnDropDown) then FOnDropDown(Self);
end;

procedure TCustomComboBox.GetItems;
begin
  if Assigned(FOnGetItems) then FOnGetItems(Self);
end;

{------------------------------------------------------------------------------
  procedure TCustomComboBox.CloseUp;

  Called whenever the list hides.
 ------------------------------------------------------------------------------}
procedure TCustomComboBox.CloseUp;
begin
  if [csLoading,csDestroying,csDesigning]*ComponentState<>[] then exit;
  if Style in [csSimple, csDropDown, csOwnerDrawEditableFixed, csOwnerDrawEditableVariable] then
  begin
    EditingDone;
    FEditingDone := False;
  end;
  if Assigned(FOnCloseUp) then FOnCloseUp(Self);
  if FAutoSelect then
   begin
    SelectAll;
    if (SelText = Text) then FAutoSelected := True;
   end;//End if FAutoSelect
end;

{------------------------------------------------------------------------------
  procedure TCustomComboBox.AdjustDropDown;

 ------------------------------------------------------------------------------}
procedure TCustomComboBox.AdjustDropDown;
var
  Count, MinItemsWidth, MinItemsHeight: Integer;
begin
  if (not HandleAllocated) then exit;
  Count := Items.Count;
  if Count > DropDownCount then Count := DropDownCount;
  if Count < 1 then Count := 1;
  MinItemsWidth := ItemWidth;
  MinItemsHeight := Count * ItemHeight;
  SetComboMinDropDownSize(Handle, MinItemsWidth, MinItemsHeight, Count);
end;

{------------------------------------------------------------------------------
  procedure TCustomComboBox.IntfGetItems

  Called whenever the items can be just-in-time populated.
 ------------------------------------------------------------------------------}
procedure TCustomComboBox.IntfGetItems;
begin
  GetItems;
end;

{------------------------------------------------------------------------------
  Method: TCustomComboBox.GetSelText
  Params: ---
  Returns: selected text

  Returns the selected part of text-field.
 ------------------------------------------------------------------------------}
function TCustomComboBox.GetSelText: string;
begin
  //debugln('TCustomComboBox.GetSelText ');
  if FStyle in [csDropDown, csSimple] then
    Result:= UTF8Copy(Text, SelStart + 1, SelLength)
  else
    Result:= '';
end;

{------------------------------------------------------------------------------
  Method: TCustomComboBox.SetSelText
  Params: val - new string for text-field
  Returns: nothings

  Replace the selected part of text-field with "val".
 ------------------------------------------------------------------------------}
procedure TCustomComboBox.SetSelText(const Val: string);
var
  OldText, NewText: string;
  OldSelStart: integer;
begin
  //debugln('TCustomComboBox.SetSelText ',Val);
  if FStyle in [csDropDown, csSimple] then
  begin
    OldText := Text;
    OldSelStart := SelStart;
    NewText := UTF8Copy(OldText, 1, OldSelStart) +
               Val +
               UTF8Copy(OldText, OldSelStart + SelLength + 1, MaxInt);
    Text := NewText;
    SelStart := OldSelStart;
    SelLength := UTF8Length(Val);
  end;
end;

{------------------------------------------------------------------------------
  Method: TCustomComboBox.GetSelStart
  Params: ---
  Returns: starting index of selected text

  Returns starting index of selected text
 ------------------------------------------------------------------------------}
function TCustomComboBox.GetSelStart : integer;
begin
  if HandleAllocated then
    FSelStart := TWSCustomComboBoxClass(WidgetSetClass).GetSelStart(Self);
  Result := FSelStart;
end;

{------------------------------------------------------------------------------
  Method: TCustomComboBox.SetSelStart
  Params: val -
  Returns: nothing

  Sets starting index for selected text.
 ------------------------------------------------------------------------------}
procedure TCustomComboBox.SetSelStart(Val : integer);
begin
  FSelStart := Val;
  if HandleAllocated then
    TWSCustomComboBoxClass(WidgetSetClass).SetSelStart(Self, Val);
end;

{------------------------------------------------------------------------------
  Method: TCustomComboBox.GetSelLength
  Params: ---
  Returns: length of selected text

  Returns length of selected text
 ------------------------------------------------------------------------------}
function TCustomComboBox.GetSelLength : integer;
begin
  if HandleAllocated then
    FSelLength := TWSCustomComboBoxClass(WidgetSetClass).GetSelLength(Self);
  Result := FSelLength;
end;

{------------------------------------------------------------------------------
  Method: TCustomComboBox.SetSelLength
  Params: val -
  Returns: nothing

  Sets length of selected text.
 ------------------------------------------------------------------------------}
procedure TCustomComboBox.SetSelLength(Val : integer);
begin
  FSelLength := Val;
  if HandleAllocated then
    TWSCustomComboBoxClass(WidgetSetClass).SetSelLength(Self, Val);
end;

{------------------------------------------------------------------------------
  Method: TCustomComboBox.SelectAll
  Params: -
  Returns: nothing

  Select entire text.
 ------------------------------------------------------------------------------}
procedure TCustomComboBox.SelectAll;
var
  CurText: String;
begin
  //debugln('TCustomComboBox.SelectAll ');
  if (FStyle in [csDropDown, csSimple]) then
  begin
    CurText := Text;
    if (CurText <> '') then
    begin
      SetSelStart(0);
      SetSelLength(UTF8Length(CurText));
    end;
  end;
end;

{------------------------------------------------------------------------------
  Method: TCustomComboBox.SetStyle
  Params: val - new style for combobox
  Returns: nothing

  Sets a new style for the combobox.
 ------------------------------------------------------------------------------}
procedure TCustomComboBox.SetStyle(Val : TComboBoxStyle);
begin
  if Val <> FStyle then
  begin
    FStyle:= Val;
    if HandleAllocated and ([csLoading,csDestroying]*ComponentState=[]) then
      TWSCustomComboBoxClass(WidgetSetClass).SetStyle(Self, Val);
  end;
end;

procedure TCustomComboBox.SetArrowKeysTraverseList(Value : Boolean);
begin
  if Value <> FArrowKeysTraverseList then
  begin
    FArrowKeysTraverseList := Value;
    if HandleAllocated and ([csLoading, csDestroying] * ComponentState=[]) then
      TWSCustomComboBoxClass(WidgetSetClass).SetArrowKeysTraverseList(Self, Value);
   end;
end;

procedure TCustomComboBox.WMChar(var Message: TLMChar);
begin
  // all normal characters are handled by the ComboBox
  //debugln('TCustomEdit.WMChar ',DbgSName(Self),' ',dbgs(Message.CharCode));
  if (Message.CharCode in [ord('A')..ord('Z'),ord('a')..ord('z')]) then
    // eat normal keys, so they don't trigger accelerators
    Message.Result := 1
  else
    inherited WMChar(Message);
end;

procedure TCustomComboBox.SetCharCase(eccCharCase: TEditCharCase);
begin
 if (FCharCase <> eccCharCase) then
  begin
   FCharCase := eccCharCase;
   case FCharCase of
     ecUpperCase: Text := UTF8UpperCase(Text);
     ecLowerCase: Text := UTF8Lowercase(Text);
    end;
  end;
end;

class procedure TCustomComboBox.WSRegisterClass;
begin
  inherited WSRegisterClass;
  RegisterCustomComboBox;
  RegisterPropertyToSkip(TCustomComboBox, 'BevelInner', 'VCL compatibility property', '');
  RegisterPropertyToSkip(TCustomComboBox, 'BevelKind',  'VCL compatibility property', '');
  RegisterPropertyToSkip(TCustomComboBox, 'BevelOuter', 'VCL compatibility property', '');
  RegisterPropertyToSkip(TCustomComboBox, 'ImeMode',    'VCL compatibility property', '');
end;

procedure TCustomComboBox.CreateParams(var Params: TCreateParams);
const
  ComboBoxStyles: array[TComboBoxStyle] of dword = (
    CBS_DROPDOWN, CBS_SIMPLE, CBS_DROPDOWNLIST,
    CBS_OWNERDRAWFIXED or CBS_DROPDOWNLIST, CBS_OWNERDRAWVARIABLE or CBS_DROPDOWNLIST,
    CBS_OWNERDRAWFIXED or CBS_DROPDOWN, CBS_OWNERDRAWVARIABLE or CBS_DROPDOWN);
begin
  inherited CreateParams(Params);
  Params.Style := Params.Style or (WS_VSCROLL or CBS_AUTOHSCROLL or CBS_HASSTRINGS) or
    ComboBoxStyles[Style];

  if Sorted then
    Params.Style := Params.Style or CBS_SORT;
end;

procedure TCustomComboBox.KeyDown(var Key: Word; Shift: TShiftState);
var
  Skip, UserDropDown, PreventDropDown: Boolean;
begin
  FEditingDone := Key = VK_RETURN;
  Skip := False;
  UserDropDown := ((Shift *[ssAlt] = [ssAlt]) and (Key = VK_DOWN));
  if Style = csSimple then
    PreventDropDown := Key in [VK_RETURN, VK_ESCAPE]
  else
    PreventDropDown := Key in [VK_TAB, VK_RETURN, VK_ESCAPE];
  if PreventDropDown then
  begin
    DroppedDown := False;
  end;
  // if AutoDropDown then don't close DropDown, like in Delphi, issue #31247
  if AutoDropDown then
    PreventDropDown := PreventDropDown or (ssAlt in Shift)
                       or (Key in [VK_UNKNOWN..VK_MODECHANGE, VK_END..VK_LEFT,
                                   VK_RIGHT, VK_SELECT..VK_HELP,
                                   VK_LWIN..VK_SLEEP, VK_F1..VK_UNDEFINED]);
  if AutoDropDown or UserDropDown or FReturnArrowState then
  begin
    if PreventDropDown then
    begin
      if FReturnArrowState then
      begin
        ArrowKeysTraverseList := False; //we need?? this here, else we cannot traverse popup list
        FReturnArrowState := False;
      end;
    end
    else
    begin
      if not ArrowKeysTraverseList then
      begin
        ArrowKeysTraverseList := True; //we need?? this here, else we cannot traverse popup list
        FReturnArrowState := True;
        Skip := True;
      end;
      DroppedDown := True;
      if UserDropDown then
        Skip := True;
    end;
  end;
  if Skip then
    Key := VK_UNKNOWN
  else
    inherited KeyDown(Key, Shift);
end;

procedure TCustomComboBox.KeyUp(var Key: Word; Shift: TShiftState);
var
  iSelStart: Integer; // char position
  sCompleteText, sPrefixText, sResultText: string;
  Utf8TextLen: Integer;
begin
  inherited KeyUp(Key, Shift);
  //SelectAll when hitting return key for AutoSelect feature
  if (Key = VK_RETURN) then
  begin
    if ((cbactEnabled in FAutoCompleteText) and (Style <> csDropDownList)) then
    begin
      // Only happens with alpha-numeric keys and return key and editable Style
      SelectAll;
    end;
    if FAutoSelect then
    begin
     SelectAll;
     if (SelText = Text) then FAutoSelected := True;
    end;
  end
  else
  if ((cbactEnabled in FAutoCompleteText) and (Style <> csDropDownList)) then
  begin
    //Only happens with alpha-numeric keys and return key and editable Style
    //DebugLn(['TCustomComboBox.KeyUp ',Key,' ',IsEditableTextKey(Key)]);
    if IsEditableTextKey(Key) then
    begin
      iSelStart := SelStart;//Capture original cursor position
      //DebugLn(['TCustomComboBox.UTF8KeyPress SelStart=',SelStart,' Text=',Text]);
      //End of line completion
      Utf8TextLen := UTF8Length(Text);
      if (iSelStart < Utf8TextLen) and (cbactEndOfLineComplete in FAutoCompleteText) then
        Exit;
      sPrefixText := UTF8Copy(Text, 1, iSelStart);
      sCompleteText := GetCompleteText(Text, iSelStart,
                                       cbactSearchCaseSensitive in FAutoCompleteText,
                                       cbactSearchAscending in FAutoCompleteText, Items);
      //DebugLn(['TCustomComboBox.UTF8KeyPress sCompleteText=',sCompleteText,' Text=',Text]);
      if (sCompleteText <> Text) or (Utf8TextLen = 1) then
      begin
        sResultText := sCompleteText;
        if (cbactEndOfLineComplete in FAutoCompleteText)
        and (cbactRetainPrefixCase in FAutoCompleteText) then
        begin //Retain Prefix Character cases
          UTF8Delete(sResultText, 1, iSelStart);
          UTF8Insert(sPrefixText, sResultText, 1);
        end;
        if Utf8TextLen = 1 then
          Text := '';
        Text := sResultText;
        SelStart := iSelStart;
        SelLength := UTF8Length(Text);
        Select;
      end;
    end;
  end;
end;

procedure TCustomComboBox.UTF8KeyPress(var UTF8Key: TUTF8Char);
begin
  //First invoke OnUtf8KeyPress, or else CharCase may be reverted again
  inherited UTF8KeyPress(UTF8Key);
  //Convert character cases if FCharCase is not ecNormalCase
  case FCharCase of
    ecLowerCase: UTF8Key := UTF8LowerCase(UTF8Key);
    ecUpperCase: UTF8Key := UTF8UpperCase(UTF8Key);
  end;
end;

procedure TCustomComboBox.MouseUp(Button: TMouseButton; Shift:TShiftState;
  X, Y: Integer);
begin
  inherited MouseUp(Button, Shift, X, Y);
  //AutoSelect when left mouse is clicked for the 1st time after having focus
  if (Button = mbLeft) then
  begin
    if (FAutoSelect and not FAutoSelected) then
    begin
      SelectAll;
      if (SelText = Text) then FAutoSelected := True;
    end;
  end;
end;

{------------------------------------------------------------------------------
  function TCustomComboBox.SelectItem(const AnItem: String): Boolean;

  Selects the item with the Text of AnItem
 ------------------------------------------------------------------------------}
function TCustomComboBox.SelectItem(const AnItem: String): Boolean;
var
  i: integer;
  ValueChanged: boolean;
begin
  i:=Items.IndexOf(AnItem);
  if i>=0 then
  begin
    Result := True;
    ValueChanged := ItemIndex <> i;
    ItemIndex := i;
    Text := Items[i];
    if ValueChanged then
    begin
      Click;
      Select;
    end;
  end
  else
    Result := False;
end;

procedure TCustomComboBox.ShouldAutoAdjust(var AWidth, AHeight: Boolean);
begin
  AWidth := True;
  AHeight := not AutoSize;
end;

{------------------------------------------------------------------------------
  function TCustomComboBox.GetItemCount: Integer;

  Returns the number of items
 ------------------------------------------------------------------------------}
function TCustomComboBox.GetItemCount: Integer;
begin
  Result:=Items.Count;
end;

{------------------------------------------------------------------------------
  function TCustomComboBox.GetItemHeight: Integer;

  Gets default ItemHeight.
 ------------------------------------------------------------------------------}
function TCustomComboBox.GetItemHeight: Integer;
begin
  // FItemHeight is not initialized at class creating. we can, but with what value?
  // so, if it still uninitialized (=0), then we ask widgetset
  if (FStyle in [csOwnerDrawFixed, csOwnerDrawVariable,
                 csOwnerDrawEditableFixed, csOwnerDrawEditableVariable])
  and (FItemHeight > 0) or not HandleAllocated then
    Result := FItemHeight
  else
  begin
    Result := TWSCustomComboBoxClass(WidgetSetClass).GetItemHeight(Self);
    if (FItemHeight = 0) then
      FItemHeight := Result;
  end;
end;

{------------------------------------------------------------------------------
  procedure TCustomComboBox.SetDropDownCount(const AValue: Integer);

  Sets the number of items that fits into the drop down list.
 ------------------------------------------------------------------------------}
procedure TCustomComboBox.SetDropDownCount(const AValue: Integer);
begin
  if AValue = FDropDownCount then
    exit;
  FDropDownCount := AValue;
  if HandleAllocated then
    TWSCustomComboBoxClass(WidgetSetClass).SetDropDownCount(Self, AValue);
end;

{------------------------------------------------------------------------------
  procedure TCustomComboBox.SetItemHeight(const AValue: Integer);

  Sets default ItemHeight. 0 or negative values are ignored.
 ------------------------------------------------------------------------------}
procedure TCustomComboBox.SetItemHeight(const AValue: Integer);
begin
  if AValue = FItemHeight then
    exit;
  FItemHeight := AValue;
  if not HandleAllocated then
    exit;
  if Style in [csOwnerDrawFixed, csOwnerDrawVariable,
               csOwnerDrawEditableFixed, csOwnerDrawEditableVariable] then
    TWSCustomComboBoxClass(WidgetSetClass).SetItemHeight(Self, FItemHeight);
end;

{------------------------------------------------------------------------------
  function TCustomComboBox.GetDroppedDown: Boolean;

  Returns true, if list is shown.
 ------------------------------------------------------------------------------}
function TCustomComboBox.GetDroppedDown: Boolean;
begin
  if HandleAllocated then
    Result := TWSCustomComboBoxClass(WidgetSetClass).GetDroppedDown(Self)
  else
    Result := FDroppedDown;
end;

function TCustomComboBox.GetAutoComplete: boolean;
begin
  Result := cbactEnabled in AutoCompleteText;
end;

{------------------------------------------------------------------------------
  function TCustomComboBox.GetItemWidth: Integer;

  The ItemWidth is the minimum pixels, that is allocated for the items in the
  dropdown list.
 ------------------------------------------------------------------------------}
function TCustomComboBox.GetItemWidth: Integer;
begin
  Result:=FItemWidth;
end;

procedure TCustomComboBox.SetAutoComplete(const AValue: boolean);
begin
  if (cbactEnabled in FAutoCompleteText)=AValue then exit;
  if AValue then
    AutoCompleteText := AutoCompleteText + [cbactEnabled]
  else
    AutoCompleteText := AutoCompleteText - [cbactEnabled]
end;

procedure TCustomComboBox.SetDroppedDown(const AValue: Boolean);
begin
  if GetDroppedDown = AValue then
    Exit;
  if (not HandleAllocated) or (csLoading in ComponentState) then
    Exit;
  TWSCustomComboBoxClass(WidgetSetClass).SetDroppedDown(Self, AValue);
end;

{------------------------------------------------------------------------------
  procedure TCustomComboBox.SetItemWidth(const AValue: Integer);

  The ItemWidth is the minimum pixels, that is allocated for the items in the
  dropdown list.
 ------------------------------------------------------------------------------}
procedure TCustomComboBox.SetItemWidth(const AValue: Integer);
begin
  if FItemWidth=AValue then exit;
  FItemWidth:=AValue;
  AdjustDropDown;
end;

{------------------------------------------------------------------------------
  Method: TCustomComboBox.SetItems
  Params: value - stringlist with items for combobox
  Returns: nothing

  Assigns items for ComboBox from a stringlist.
 ------------------------------------------------------------------------------}
procedure TCustomComboBox.SetItems(const Value : TStrings);
begin
  if (Value <> FItems) then
    FItems.Assign(Value);
end;

{------------------------------------------------------------------------------
  Method: TCustomComboBox.Create
  Params: AOwner - owner of the object
  Returns: reference to the newly created object

  Creates the object.
 ------------------------------------------------------------------------------}
constructor TCustomComboBox.Create(TheOwner : TComponent);
begin
  inherited Create(TheOwner);

  fCompStyle := csComboBox;
  ControlStyle := ControlStyle - [csCaptureMouse];
  with GetControlClassDefaultSize do
    SetInitialBounds(0, 0, CX, CY);
  FItems := TStringlist.Create;
  FItemIndex := -1;
  FItemWidth := 0;
  FMaxLength := 0;
  FDropDownCount := 8;
  FCanvas := TControlCanvas.Create;
  TControlCanvas(FCanvas).Control := Self;
  ArrowKeysTraverseList := True;
  TabStop := True;
  ParentColor := False;
  FAutoCompleteText := DefaultComboBoxAutoCompleteText;
  FAutoSelect := True;
  FAutoSelected := False;
  FCharCase := ecNormal;
  
  { AutoSize must be true by default to provide good cross-platform
    development experience as some widgetsets (win32, wince) ignore the
    combobox height and others (gtk2) look ugly with a too small height }
  AutoSize := True;
end;

{------------------------------------------------------------------------------
  Method: TCustomComboBox.Destroy
  Params: ---
  Returns: nothing

  Destroys the object.
 ------------------------------------------------------------------------------}
destructor TCustomComboBox.Destroy;
begin
  if HandleAllocated then DestroyHandle;
  FCanvas.Free;
  FCanvas:=nil;
  FItems.Free;
  FItems:=nil;
  inherited Destroy;
end;

{------------------------------------------------------------------------------
  procedure TCustomComboBox.AddItem(const Item: String; AnObject: TObject);

  Adds an Item with an associated object to Items
 ------------------------------------------------------------------------------}
procedure TCustomComboBox.AddItem(const Item: String; AnObject: TObject);
begin
  Items.AddObject(Item,AnObject);
end;

{------------------------------------------------------------------------------
  procedure TCustomComboBox.AddHistoryItem(const Item: string;
    MaxHistoryCount: integer; SetAsText, CaseSensitive: boolean);

  Adds an Item as first item. Removes the Item from old positions and removes
  last item if history is full.
 ------------------------------------------------------------------------------}
procedure TCustomComboBox.AddHistoryItem(const Item: string;
  MaxHistoryCount: integer; SetAsText, CaseSensitive: boolean);
begin
  AddHistoryItem(Item,nil,MaxHistoryCount,SetAsText,CaseSensitive);
end;

{------------------------------------------------------------------------------
  procedure TCustomComboBox.AddHistoryItem(const Item: string;
    AnObject: TObject; MaxHistoryCount: integer;
    SetAsText, CaseSensitive: boolean);

  Adds an Item as first item. Removes the Item from old positions and removes
  last item if history is full.
 ------------------------------------------------------------------------------}
procedure TCustomComboBox.AddHistoryItem(const Item: string; AnObject: TObject;
  MaxHistoryCount: integer; SetAsText, CaseSensitive: boolean);
var i: integer;
begin
  // insert as first
  if (Items.Count=0)
  or (not CaseSensitive and (UTF8CompareText(Items[0],Item)<>0))
  or (CaseSensitive and (Items[0]<>Item)) then
  begin
    Items.InsertObject(0,Item,AnObject);
  end;
  // delete old
  for i:=Items.Count-1 downto 1 do begin
    if (not CaseSensitive and (UTF8CompareText(Items[i],Item)=0))
    or (CaseSensitive and (Items[i]=Item)) then
      Items.Delete(i);
  end;
  // delete overflow items
  while Items.Count>MaxHistoryCount do
    Items.Delete(Items.Count-1);
  // set as text
  if SetAsText then
    Text:=Item;
end;

{------------------------------------------------------------------------------
  procedure TCustomComboBox.Clear;

  Removes all Items
 ------------------------------------------------------------------------------}
procedure TCustomComboBox.Clear;
begin
  Items.Clear;
  Text:='';
end;

{------------------------------------------------------------------------------
  procedure TCustomComboBox.ClearSelection;

  Unselects all items.
 ------------------------------------------------------------------------------}
procedure TCustomComboBox.ClearSelection;
begin
  ItemIndex := -1;
end;

{------------------------------------------------------------------------------
  procedure TCustomComboBox.MeasureItem(Index: Integer; var TheHeight: Integer);

 ------------------------------------------------------------------------------}
procedure TCustomComboBox.MeasureItem(Index: Integer; var TheHeight: Integer);
begin
  if Assigned(OnMeasureItem) then
    OnMeasureItem(Self,Index,TheHeight);
end;

{------------------------------------------------------------------------------
  Method: TCustomComboBox.GetItemIndex
  Params: ---
  Returns: index of the currently selected item

  Returns index of the currently selected item in the combobox. -1 is returned
  if no item is currently selected.
 ------------------------------------------------------------------------------}
function TCustomComboBox.GetItemIndex : integer;
begin
  if not (csDestroyingHandle in ControlState) and HandleAllocated then
    // WidgetSet must be called even when csDestroying for at least TIniPropStorage's needs.
    FItemIndex:= TWSCustomComboBoxClass(WidgetSetClass).GetItemIndex(Self);
  Result := FItemIndex;
end;

{------------------------------------------------------------------------------
  Method: TCustomComboBox.RealSetText
  Params: AValue -
  Returns: nothing

  If the text AValue occurs in the list of strings, then sets the itemindex,
  otherwise does the default action
 ------------------------------------------------------------------------------}
procedure TCustomComboBox.RealSetText(const AValue: TCaption);
var
  I: integer;
begin
  // when items have same text, FItems.IndexOf(AValue) gives wrong index. Issue #28683.
  I := ItemIndex;
  if (I < 0) or (I >= FItems.Count) or (FItems[I] <> AValue) then
  begin
    I := FItems.IndexOf(AValue);
    if I >= 0 then
      ItemIndex := I
    else if (not (csLoading in ComponentState)) then
      ItemIndex := -1;
  end;
  inherited;
end;

{------------------------------------------------------------------------------
  Method: TCustomComboBox.SetItemIndex
  Params: Val -
  Returns: nothing

  Sets ths index of the currently selected item in the combobox.
 ------------------------------------------------------------------------------}
procedure TCustomComboBox.SetItemIndex(const Val : integer);
begin
  //if CompareText(Name,'TextToFindComboBox')=0 then
  //  debugln('TCustomComboBox.SetItemIndex A ',DbgSName(Self),' Text="',Text,'"');
  if Val < -1 then Exit;
  if Val = GetItemIndex then exit;
  if (Val >= Items.Count) and (not (csLoading in ComponentState)) then exit;

  FItemIndex := Val;
  if csLoading in ComponentState then Exit;
  
  if HandleAllocated then
    TWSCustomComboBoxClass(WidgetSetClass).SetItemIndex(Self, FItemIndex)
  else
  begin
    // remember text, in case one reads text without creating handle
    if Val = -1 then
      Text := ''
    else
      Text := FItems.Strings[FItemIndex];
  end;

  //if CompareText(Name,'TextToFindComboBox')=0 then
  //  debugln('TCustomComboBox.SetItemIndex END ',DbgSName(Self),' Text="',Text,'"');
end;

{------------------------------------------------------------------------------
  procedure TCustomComboBox.LMDrawListItem(var TheMessage : TLMDrawListItem);

  Handler for custom drawing items.
 ------------------------------------------------------------------------------}
procedure TCustomComboBox.LMDrawListItem(var TheMessage : TLMDrawListItem);
begin
  with TheMessage.DrawListItemStruct^ do
  begin
    FCanvas.Handle := DC;
    if Font<>nil then
    begin
      FCanvas.Font := Font;
      FCanvas.Font.PixelsPerInch := Font.PixelsPerInch;
    end;
    if Brush<>nil then
      FCanvas.Brush := Brush;
    if (ItemID <> UINT(-1)) and (odSelected in ItemState) then
    begin
      FCanvas.Brush.Color := clHighlight;
      FCanvas.Font.Color := clHighlightText
    end;
    DrawItem(ItemID, Area, ItemState);
    if odFocused in ItemState then
      {DrawFocusRect(hDC, rcItem)};
    FCanvas.Handle := 0;
  end;
end;

procedure TCustomComboBox.LMMeasureItem(var TheMessage: TLMMeasureItem);
var
  AHeight: Integer;
begin
  with TheMessage.MeasureItemStruct^ do
  begin
    // don't call ItemHeight since this cause recursieve message sending on windows
    if FItemHeight <> 0 then
      AHeight := FItemHeight
    else
      AHeight := ItemHeight;
    if FStyle in [csOwnerDrawVariable, csOwnerDrawEditableVariable] then
      MeasureItem(Integer(ItemId), AHeight);
    if AHeight > 0 then
      ItemHeight := AHeight;
  end;
end;

procedure TCustomComboBox.LMSelChange(var TheMessage);
begin
  if [csLoading, csDestroying, csDesigning] * ComponentState <> [] then
    exit;
  Select;
end;

procedure TCustomComboBox.KeyUpAfterInterface(var Key: Word; Shift: TShiftState);
begin
  inherited KeyUpAfterInterface(Key, Shift);
  if (Key = VK_RETURN) and (FEditingDone) then
    EditingDone;
  FEditingDone := False;
end;

{------------------------------------------------------------------------------
  procedure TCustomComboBox.CNCommand(var TheMessage: TLMCommand);

  Handler for various notifications.
 ------------------------------------------------------------------------------}
procedure TCustomComboBox.CNCommand(var TheMessage: TLMCommand);
begin
  case TheMessage.NotifyCode of
    CBN_DROPDOWN:
      begin
        FDroppedDown:=true;
        DropDown;
        AdjustDropDown;
      end;
    CBN_CLOSEUP:
      begin
        FDroppedDown:=false;
        CloseUp;
      end;
  end;
end;

{------------------------------------------------------------------------------
  procedure TCustomComboBox.UpdateSorted;
 ------------------------------------------------------------------------------}
procedure TCustomComboBox.UpdateSorted;
var
  lText: string;
  lIndex: integer;
begin
  if HandleAllocated then
    TWSCustomComboBoxClass(WidgetSetClass).Sort(Self, Items, FSorted)
  else if FItems is TStringList then
  begin
    // remember text
    lText := Text;
    TStringList(FItems).Sorted := FSorted;
    lIndex := FItems.IndexOf(lText);
    if lIndex >= 0 then
      ItemIndex := lIndex;
  end;
end;

procedure TCustomComboBox.CMWantSpecialKey(var Message: TCMWantSpecialKey);
begin
  {$ifdef darwin}
  // don't allow LCL to handle arrow keys for edit controls
  if Message.CharCode in [VK_LEFT, VK_RIGHT, VK_UP, VK_DOWN] then
    Message.Result := 1
  else
  {$endif}
    inherited;
end;

// included by stdctrls.pp
