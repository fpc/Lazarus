{%MainUnit ../stdctrls.pp}

{******************************************************************************
                               TCustomComboBox
 ******************************************************************************

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

{------------------------------------------------------------------------------
  Method: TCustomComboBox.CreateWnd
  Params: ---
  Returns: Nothing

  Create the underlying interface-object.
 ------------------------------------------------------------------------------}
procedure TCustomComboBox.CreateWnd;
var
  NewStrings: TStrings;
begin
  inherited CreateWnd;

  // get the interface based item list
  NewStrings:= TWSCustomComboBoxClass(WidgetSetClass).GetItems(Self);
  // then delete internal list
  if (FItems<>NewStrings) and (FItems<>nil) then begin
    NewStrings.Assign(FItems);
    FItems.Free;
  end;
  // and use the interface based list
  FItems:= NewStrings;

  if FItemIndex <> -1 then
    TWSCustomComboBoxClass(WidgetSetClass).SetItemIndex(Self, FItemIndex);
  TWSCustomComboBoxClass(WidgetSetClass).SetStyle(Self, FStyle);
  TWSCustomComboBoxClass(WidgetSetClass).SetArrowKeysTraverseList(Self, FArrowKeysTraverseList);
end;


{------------------------------------------------------------------------------
  Method: TCustomComboBox.DestroyWnd
  Params: ---
  Returns: Nothing

  Destroy the underlying interface-object.
 ------------------------------------------------------------------------------}
procedure TCustomComboBox.DestroyWnd;
var NewStrings : TStrings;
begin
  if not HandleAllocated then RaiseGDBException('');
  // store itemindex
  GetItemIndex;
  // create an internal list for storing items internally
  NewStrings:= TStringList.Create;
  // copy from interface based list
  if FItems<>nil then begin
    NewStrings.Assign(FItems);
    // delete interface based list
    FItems.Free;
  end;
  // and use the internal list
  FItems:= NewStrings;

  inherited DestroyWnd;
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
  else if not (odPainted in State) then
  begin
    FCanvas.FillRect(ARect);
    FCanvas.TextOut(ARect.Left + 2, ARect.Top, Items[Index]);
  end;
end;

{------------------------------------------------------------------------------
  Method: TCustomComboBox.InitializeWnd
  Params: ---
  Returns: Nothing

  Initialize window after it has been created.
 ------------------------------------------------------------------------------}

procedure TCustomComboBox.InitializeWnd;
var ASelStart, ASelLength : integer;
begin
  inherited InitializeWnd;
  if FSelStart <> FSelLength then begin
    ASelStart:= FSelStart;
    ASelLength:= FSelLength;
    SelStart:= ASelStart;
    SelLength:= ASelLength;
  end;
end;

{------------------------------------------------------------------------------
  Method: TCustomComboBox.SetSorted
  Params: val - true means "sort" the combo
  Returns: Nothing

  Set the "sorted" property of the combobox and Sort the current entries.
 ------------------------------------------------------------------------------}
procedure TCustomComboBox.SetSorted(Val : boolean);
begin
  if (Val <> FSorted) then begin
    FSorted:= Val;
    UpdateSorted;
  end;
end;

{------------------------------------------------------------------------------
  Method: TCustomComboBox.SetMaxLength
  Params: val -
  Returns: Nothing

  Set the maximum length for user input.
 ------------------------------------------------------------------------------}
procedure TCustomComboBox.SetMaxLength(Val : integer);
begin
  if Val < 0 then Val:= 0;
  if Val<>MaxLength then begin
    fMaxlength:=Val;
    if HandleAllocated then
      TWSCustomComboBoxClass(WidgetSetClass).SetMaxLength(Self, Val);
  end;
end;

{------------------------------------------------------------------------------
  Method: TCustomComboBox.GetMaxLength
  Params: ---
  Returns: the maximum length of user input

  Get the maximum length for user input.
 ------------------------------------------------------------------------------}
function TCustomComboBox.GetMaxLength : integer;
begin
  if HandleAllocated then
    fMaxLength := TWSCustomComboBoxClass(WidgetSetClass).GetMaxLength(Self);
  Result:=fMaxLength;
end;

{------------------------------------------------------------------------------
  Method: TCustomComboBox.DoChange
  Params: msg -
  Returns: Nothing

  Call handler for "OnChange"-event if one is assigned.
 ------------------------------------------------------------------------------}
procedure TCustomComboBox.LMChange(var Msg);
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

  Returns the selected part of text-field.
 ------------------------------------------------------------------------------}
procedure TCustomComboBox.Select;
begin
  if Assigned(FOnSelect) then
    FOnSelect(Self)
  else
    Change;
end;

{------------------------------------------------------------------------------
  procedure TCustomComboBox.DropDown;

  Called whenever the list popups.
 ------------------------------------------------------------------------------}
procedure TCustomComboBox.DropDown;
begin
  if Assigned(FOnDropDown) then FOnDropDown(Self);
end;

{------------------------------------------------------------------------------
  procedure TCustomComboBox.CloseUp;

  Called whenever the list hides.
 ------------------------------------------------------------------------------}
procedure TCustomComboBox.CloseUp;
begin
  if [csLoading,csDestroying,csDesigning]*ComponentState<>[] then exit;
  EditingDone;
  if Assigned(FOnCloseUp) then FOnCloseUp(Self);
end;

{------------------------------------------------------------------------------
  procedure TCustomComboBox.AdjustDropDown;

 ------------------------------------------------------------------------------}
procedure TCustomComboBox.AdjustDropDown;
var
  Count, MinItemsWidth, MinItemsHeight: Integer;
begin
  if (not HandleAllocated) or (not DroppedDown) then exit;
  Count := Items.Count;
  if Count > DropDownCount then Count := DropDownCount;
  if Count < 1 then Count := 1;
  MinItemsWidth:=ItemWidth;
  MinItemsHeight:=Count*ItemHeight;
  SetComboMinDropDownSize(Handle,MinItemsWidth,MinItemsHeight,Count);
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
    Result:= Copy(Text, SelStart, SelLength)
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
begin
  //debugln('TCustomComboBox.SetSelText ',Val);
  if FStyle in [csDropDown, csSimple] then begin
    OldText:=Text;
    NewText:=LeftStr(OldText,SelStart-1)+Val
               +RightStr(OldText,length(OldText)-SelStart-SelLength+1);
    Text:=NewText;
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
    fSelStart:=TWSCustomComboBoxClass(WidgetSetClass).GetSelStart(Self);
  Result:=fSelStart;
end;

{------------------------------------------------------------------------------
  Method: TCustomComboBox.SetSelStart
  Params: val -
  Returns: nothing

  Sets starting index for selected text.
 ------------------------------------------------------------------------------}
procedure TCustomComboBox.SetSelStart(Val : integer);
begin
  fSelStart:=Val;
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
    fSelLength := TWSCustomComboBoxClass(WidgetSetClass).GetSelLength(Self);
  Result:=fSelLength;
end;

{------------------------------------------------------------------------------
  Method: TCustomComboBox.SetSelLength
  Params: val -
  Returns: nothing

  Sets length of selected text.
 ------------------------------------------------------------------------------}
procedure TCustomComboBox.SetSelLength(Val : integer);
begin
  fSelLength:=Val;
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
  if (FStyle in [csDropDown, csSimple]) then begin
    CurText:=Text;
    if (CurText <> '') then begin
      SetSelStart(0);
      SetSelLength(Length(CurText));
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
  if Val <> FStyle then begin
    FStyle:= Val;
    if HandleAllocated and ([csLoading,csDestroying]*ComponentState=[]) then
      TWSCustomComboBoxClass(WidgetSetClass).SetStyle(Self, Val);
  end;
end;

procedure TCustomComboBox.SetArrowKeysTraverseList(Value : Boolean);
begin
  if Value <> FArrowKeysTraverseList then begin
    FArrowKeysTraverseList := Value;
    if HandleAllocated and ([csLoading,csDestroying]*ComponentState=[]) then
      TWSCustomComboBoxClass(WidgetSetClass).SetArrowKeysTraverseList(Self, Value);
   end;
end;

procedure TCustomComboBox.KeyDown(var Key: Word; Shift: TShiftState);
var
  skip : Boolean;
begin
  Skip := False;

  if AutoDropDown or ((Shift *[ssCtrl] = [ssCtrl]) and (Key = VK_DOWN))
      or FReturnArrowState then
  begin
    case Key of
      VK_TAB,VK_RETURN:
       begin

        if FReturnArrowState = True then
        begin
          SetArrowKeysTraverseList(False); //we need this here, else we cannot traverse popup list
          FReturnArrowState := False;
        end;

        DroppedDown := False;
       end;
    else
      begin
        if ArrowKeysTraverseList = False then
        begin
          SetArrowKeysTraverseList(True); //we need this here, else we cannot traverse popup list
          FReturnArrowState := True;
          Skip := True;
        end;
        //AutoDropDown := True;
        DroppedDown := True;

      end;
    end;
  end;
  
  if Style=csDropDownList then begin
    //debugln('TCustomComboBox.KeyDown ');
    // DropDownList: allow only navigation keys
    if not (Key in [VK_UP,VK_DOWN,VK_END,VK_HOME,VK_PRIOR,VK_NEXT,VK_TAB,
      VK_RETURN])
    then begin
      Key:=0;
    end;
  end;

  if Skip then
    Key := 0
  else
    inherited KeyDown(Key, Shift);
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
  if i>=0 then begin
    Result:=true;
    ValueChanged:=ItemIndex<>i;
    ItemIndex:=i;
    Text:=Items[i];
    if ValueChanged then begin
      Click;
      Select;
    end;
  end else
    Result:=false;
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
  Result:=FItemHeight;
end;

{------------------------------------------------------------------------------
  procedure TCustomComboBox.SetDropDownCount(const AValue: Integer);

  Sets the number of items that fits into the drop down list.
 ------------------------------------------------------------------------------}
procedure TCustomComboBox.SetDropDownCount(const AValue: Integer);
begin
  FDropDownCount:=AValue;
  // ToDo
end;

{------------------------------------------------------------------------------
  procedure TCustomComboBox.SetItemHeight(const AValue: Integer);

  Sets default ItemHeight. 0 or negative values are ignored.
 ------------------------------------------------------------------------------}
procedure TCustomComboBox.SetItemHeight(const AValue: Integer);
begin
  if AValue=FItemHeight then exit;
  FItemHeight:=AValue;
  // ToDo
end;

{------------------------------------------------------------------------------
  function TCustomComboBox.GetDroppedDown: Boolean;

  Returns true, if list is shown.
 ------------------------------------------------------------------------------}
function TCustomComboBox.GetDroppedDown: Boolean;
begin
  Result:=FDroppedDown;
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

{------------------------------------------------------------------------------
  function TCustomComboBox.GetDroppedDown: Boolean;


 ------------------------------------------------------------------------------}
procedure TCustomComboBox.SetDroppedDown(const AValue: Boolean);
begin
  if FDroppedDown=AValue then exit;
  if (not HandleAllocated) or (csLoading in ComponentState) then exit;
  ComboBoxDropDown(Handle,AValue);
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
procedure TCustomComboBox.SetItems(Value : TStrings);
begin
  if (Value <> FItems) then begin
    FItems.Assign(Value);
  end;
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
  SetInitialBounds(0,0,100,25);
  FItems := TStringlist.Create;
  FItemIndex:=-1;
  FDropDownCount:=8;
  FCanvas := TControlCanvas.Create;
  TControlCanvas(FCanvas).Control := Self;
  ArrowKeysTraverseList := True;
  TabStop := true;
  ParentColor := false;
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
  or (CaseSensitive and (AnsiCompareText(Items[0],Item)<>0))
  or (not CaseSensitive and (Items[0]<>Item)) then
  begin
    Items.InsertObject(0,Item,AnObject);
  end;
  // delete old
  for i:=Items.Count-1 downto 1 do begin
    if (CaseSensitive and (AnsiCompareText(Items[i],Item)=0))
    or (not CaseSensitive and (Items[i]=Item)) then
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
  if HandleAllocated then
    FItemIndex:= TWSCustomComboBoxClass(WidgetSetClass).GetItemIndex(Self);
  Result:=FItemIndex;
end;

{------------------------------------------------------------------------------
  Method: TCustomComboBox.SetItemIndex
  Params: Val -
  Returns: nothing

  Sets ths index of the currently selected item in the combobox.
 ------------------------------------------------------------------------------}
procedure TCustomComboBox.SetItemIndex(Val : integer);
begin
  if Val=GetItemIndex then exit;
  if Val>=Items.Count then exit;

  FItemIndex:= Val;
  //if (FItemIndex>=0) and (not (csLoading in ComponentState)) then
  //  Text:=FItems[FItemIndex];
  if HandleAllocated then
    TWSCustomComboBoxClass(WidgetSetClass).SetItemIndex(Self, FItemIndex);
end;

{------------------------------------------------------------------------------
  Procedure TCustomComboBox.LMDrawListItem(var TheMessage : TLMDrawListItem);

  Handler for custom drawing items.
 ------------------------------------------------------------------------------}
Procedure TCustomComboBox.LMDrawListItem(var TheMessage : TLMDrawListItem);
begin
  with TheMessage.DrawListItemStruct^ do
  begin
    FCanvas.Handle := DC;
    if Font<>nil then
      FCanvas.Font := Font;
    if Brush<>nil then
      FCanvas.Brush := Brush;
    if (ItemID >= 0) and (odSelected in ItemState) then
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
begin
  if not HandleAllocated then exit;
  TWSCustomComboBoxClass(WidgetSetClass).Sort(Self, Items, FSorted);
end;

// included by stdctrls.pp

{
  $Log$
  Revision 1.52  2005/02/04 15:36:50  mattias
  published TComboBox.ItemIndex  from Sergios

  Revision 1.51  2005/01/20 16:58:16  mattias
  published TComboBox.OnDbClick

  Revision 1.50  2005/01/13 18:39:45  mattias
  fixed getting ItemIndex before combobox destroys handle

  Revision 1.49  2004/11/27 22:29:56  vincents
  fixed setting itemindex, if it had been changed by the interface before

  Revision 1.48  2004/10/17 14:40:38  micha
  fix parentcolor initialization, in createwnd is too late

  Revision 1.47  2004/09/20 21:01:04  micha
  convert LM_SETPROPERTIES to interface methods for TCustomComboBox

  Revision 1.46  2004/09/07 10:26:16  micha
  fix logs to get rid of comment level 2 warning

  Revision 1.45  2004/09/07 09:44:46  micha
  convert lcl messages to new interface using methods: LM_G/SETSELSTART, LM_G/SETSELLEN, LM_G/SETLIMITTEXT

  Revision 1.44  2004/09/01 10:25:58  mattias
  added some project flags to start getting rid of TProjectType

  Revision 1.43  2004/08/30 10:49:20  mattias
  fixed focus catch for combobox csDropDownList

  Revision 1.42  2004/08/13 16:40:47  mazen
  + TCharater type used to allow UTF8 keyboard with gtk2

  Revision 1.41  2004/07/16 21:49:00  mattias
  added RTTI controls

  Revision 1.40  2004/07/10 18:17:30  mattias
  added Delphi ToDo support, Application.WndProc, small bugfixes  from Colin

  Revision 1.39  2004/05/30 08:53:27  mattias
  TComboBox.Clear now clears Text as well

  Revision 1.38  2004/04/10 17:58:57  mattias
  implemented mainunit hints for include files

  Revision 1.37  2004/02/23 08:19:04  micha
  revert intf split

  Revision 1.35  2004/02/13 18:21:31  mattias
  fixed combo chane

  Revision 1.34  2004/01/21 10:19:16  micha
  enable tabstops for controls; implement tabstops in win32 intf

  Revision 1.33  2003/10/16 23:54:27  marc
  Implemented new gtk keyevent handling

  Revision 1.32  2003/09/20 13:27:49  mattias
  varois improvements for ParentColor from Micha

  Revision 1.31  2003/08/28 09:10:00  mattias
  listbox and comboboxes now set sort and selection at handle creation

  Revision 1.30  2003/08/27 09:20:44  mattias
  added TFrame definition, no implementation

  Revision 1.29  2003/07/26 13:26:56  mattias
  fixed WindowProc

  Revision 1.28  2003/06/12 16:18:23  mattias
  applied TComboBox fix for grabbing keys from Yoyong

  Revision 1.27  2003/06/10 13:35:54  mattias
  implemented TComboBox dropdown from Yoyong

  Revision 1.26  2003/06/07 09:34:21  mattias
  added ambigius compiled unit test for packages

  Revision 1.25  2003/04/11 17:10:20  mattias
  added but not implemented ComboBoxDropDown

  Revision 1.24  2003/01/01 10:46:59  mattias
  fixes for win32 listbox/combobox from Karl Brandt

  Revision 1.23  2002/11/17 11:10:04  mattias
  TComboBox and TListBox accelerated and now supports objects

  Revision 1.22  2002/11/17 00:18:11  mattias
  fixed combobox createhandle

  Revision 1.21  2002/11/15 23:40:40  mattias
  added combobox createhandle old list assign

  Revision 1.20  2002/10/05 10:37:21  lazarus
  MG: fixed TComboBox.ItemIndex on CreateWnd

  Revision 1.19  2002/10/04 20:46:51  lazarus
  MG: improved TComboBox.SetItemIndex

  Revision 1.18  2002/10/04 14:24:14  lazarus
  MG: added DrawItem to TComboBox/TListBox

  Revision 1.17  2002/10/03 18:04:46  lazarus
  MG: started customdrawitem

  Revision 1.16  2002/10/03 14:47:30  lazarus
  MG: added TComboBox.OnPopup+OnCloseUp+ItemWidth

  Revision 1.15  2002/10/02 14:23:23  lazarus
  MG: added various history lists

  Revision 1.14  2002/09/16 16:06:21  lazarus
  MG: replaced halt with raiseexception

  Revision 1.13  2002/09/16 15:42:17  lazarus
  MG: fixed calling DestroyHandle if not HandleAllocated

  Revision 1.12  2002/08/31 11:37:09  lazarus
  MG: fixed destroying combobox

  Revision 1.11  2002/08/30 06:46:03  lazarus

  Use comboboxes. Use history. Prettify the dialog. Preselect text on show.
  Make the findreplace a dialog. Thus removing resiying code (handled by Anchors now anyway).
  Make Anchors work again and publish them for various controls.
  SelStart and Co. for TEdit, SelectAll procedure for TComboBox and TEdit.
  Clean up and fix some bugs for TComboBox, plus selection stuff.

  Revision 1.10  2002/08/29 00:07:01  lazarus
  MG: fixed TComboBox and InvalidateControl

  Revision 1.9  2002/08/27 18:45:13  lazarus
  MG: propedits text improvements from Andrew, uncapturing, improved comobobox

  Revision 1.8  2002/05/10 06:05:51  lazarus
  MG: changed license to LGPL

  Revision 1.7  2002/04/18 08:09:03  lazarus
  MG: added include comments

  Revision 1.6  2002/03/25 17:59:20  lazarus
  GTK Cleanup
  Shane

  Revision 1.5  2001/06/04 09:32:17  lazarus
  MG: fixed bugs and cleaned up messages

  Revision 1.4  2001/01/28 03:51:42  lazarus
  Fixed the problem with Changed for ComboBoxs
  Shane

  Revision 1.3  2000/11/29 21:22:35  lazarus
  New Object Inspector code
  Shane

  Revision 1.2  2000/07/23 19:03:10  lazarus
  changed some comments, stoppok

  Revision 1.1  2000/07/13 10:28:25  michael
  + Initial import

  Revision 1.4  2000/07/09 20:41:44  lazarus
  Added Attachsignals method to custombobobox, stoppok

  Revision 1.3  2000/06/29 21:08:07  lazarus
  some minor improvements &more comments, stoppok

}
