// included by stdctrls.pp

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
  Method: TCustomComboBox.CreateHandle
  Params: ---
  Returns: Nothing

  Create the underlying interface-object.
 ------------------------------------------------------------------------------}
procedure TCustomComboBox.CreateHandle;
var NewStrings : TStrings;
begin
  inherited CreateHandle;
  
  // get the interface based item list
  NewStrings:= TStrings(Pointer(CNSendMessage(LM_GETITEMS, Self, nil)));
  // then delete internal list
  if FItems<>NewStrings then
    FItems.Free;
  // and use the interface based list
  FItems:= NewStrings;
end;


{------------------------------------------------------------------------------
  Method: TCustomComboBox.DestroyHandle
  Params: ---
  Returns: Nothing

  Destroy the underlying interface-object.
 ------------------------------------------------------------------------------}
procedure TCustomComboBox.DestroyHandle;
var NewStrings : TStrings;
begin
  if not HandleAllocated then begin
    if (length(Name) div (length(Name) div 10000))=0 then ;
  end;
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
  
  inherited DestroyHandle;
end;

{------------------------------------------------------------------------------
  procedure TCustomComboBox.DrawItem(Index: Integer; Rect: TRect;
    State: TCustomDrawItemState);


 ------------------------------------------------------------------------------}
procedure TCustomComboBox.DrawItem(Index: Integer; Rect: TRect;
  State: TCustomDrawItemState);
begin
  //TControlCanvas(FCanvas).UpdateTextFlags;
  if Assigned(FOnDrawItem) then FOnDrawItem(Self, Index, Rect, State);
  {else
  begin
    FCanvas.FillRect(Rect);
    FCanvas.TextOut(Rect.Left + 2, Rect.Top, Items[Index]);
  end;}
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
var AMessage : TLMSort;
begin
  if (Val <> FSorted) then begin
    if HandleAllocated then begin
      with AMessage do begin
        Msg:= LM_SORT;
        List:= Items;
        IsSorted:= Val;
      end;

      CNSendMessage(LM_SORT, Self, @AMessage);
    end;
    FSorted:= Val;
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
      CNSendMessage(LM_SETLIMITTEXT, Self, @Val);
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
    fMaxLength := CNSendMessage(LM_GETLIMITTEXT, Self, nil);
  Result:=fMaxLength;
end;

{------------------------------------------------------------------------------
  Method: TCustomComboBox.DoChange
  Params: msg -
  Returns: Nothing

  Call handler for "OnChange"-event if one is assigned.
 ------------------------------------------------------------------------------}
procedure TCustomComboBox.DoChange(var Msg);
begin
  if Assigned(FOnChange) then FOnChange(Self);
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
  procedure TCustomComboBox.Loaded;

  Called after stream reading.
 ------------------------------------------------------------------------------}
procedure TCustomComboBox.Loaded;
begin
  inherited Loaded;
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

  Returns the selected part of text-field.
 ------------------------------------------------------------------------------}
procedure TCustomComboBox.DropDown;
begin
  if Assigned(FOnDropDown) then FOnDropDown(Self);
end;

{------------------------------------------------------------------------------
  procedure TCustomComboBox.CloseUp;

  Returns the selected part of text-field.
 ------------------------------------------------------------------------------}
procedure TCustomComboBox.CloseUp;
begin
  if Assigned(FOnCloseUp) then FOnCloseUp(Self);
end;

{------------------------------------------------------------------------------
  Method: TCustomComboBox.GetSelText
  Params: ---
  Returns: selected text

  Returns the selected part of text-field.
 ------------------------------------------------------------------------------}
function TCustomComboBox.GetSelText : string;
begin
  if FStyle < csDropDownList then
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
procedure TCustomComboBox.SetSelText(const Val : string);
var
  OldText, NewText: string;
begin
  if FStyle < csDropDownList then begin
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
    fSelStart:=CNSendMessage(LM_GETSELSTART, Self, nil);
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
    CNSendMessage(LM_SETSELSTART, Self, Pointer(Val));
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
    fSelLength := CNSendMessage(LM_GETSELLEN, Self, nil);
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
    CNSendMessage(LM_SETSELLEN, Self, Pointer(Val));
end;

{------------------------------------------------------------------------------
  Method: TCustomComboBox.SelectAll
  Params: -
  Returns: nothing

  Select entire text.
 ------------------------------------------------------------------------------}
procedure TCustomComboBox.SelectAll;
begin
  if (FStyle < csDropDownList) and (Text <> '') then begin
    SetSelStart(0);
    SetSelLength(Length(Text));
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
    // ToDo
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
constructor TCustomComboBox.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);

  fCompStyle := csComboBox;
  SetBounds(1,1,100,25);
  FItems := TStringlist.Create;
  FItemIndex:=-1;
  FDropDownCount:=8;
//   FItems:= TComboBoxStrings.Create;
//   TComboBoxStrings(FItems).ComboBox := Self;
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
  FItems.Free;
  FItems:=nil;
  inherited Destroy;
end;

{------------------------------------------------------------------------------
  procedure TCustomComboBox.AddItem(const Item: String; AObject: TObject);

  Adds an Item with an associated object to Items
 ------------------------------------------------------------------------------}
procedure TCustomComboBox.AddItem(const Item: String; AObject: TObject);
begin
  Items.AddObject(Item,AObject);
end;

{------------------------------------------------------------------------------
  procedure TCustomComboBox.Clear;

  Removes all Items
 ------------------------------------------------------------------------------}
procedure TCustomComboBox.Clear;
begin
  Items.Clear;
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
    FItemIndex:= CNSendMessage(LM_GETITEMINDEX, Self, nil);
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
  if FItemIndex = Val then exit;
  FItemIndex:= Val;
  if (FItemIndex>=0) and (not (csLoading in ComponentState)) then
    Text:=FItems[FItemIndex];
  if HandleAllocated then
    CNSendMessage(LM_SETITEMINDEX, Self, Pointer(FItemIndex));
end;

{------------------------------------------------------------------------------
  Procedure TCustomComboBox.CNDrawItems(var Message : TLMDrawItems);

  Handler for custom drawing items.
 ------------------------------------------------------------------------------}
Procedure TCustomComboBox.CNDrawItems(var Message : TLMDrawItems);
Begin
  // ToDo
end;

// included by stdctrls.pp

{
  $Log$
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
