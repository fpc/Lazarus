{******************************************************************************
                               TCustomComboBox
 ******************************************************************************}
{
  current design flaws:

   - unknown

  Delphi compatability:

   - GTK: does not support the different "styles"
   - GTK: Behaviour of OnClick event differs
   - lots of unknown issues

  TODO:

    - check for Delphi compatibility
    - lot's of testing
    - lot's of properties missing (OnClick....!!!!)
    
  Bugs:

    - unknwon
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
  NewStrings:= TStrings(Pointer(CNSendMessage(LM_GETITEMS, Self, nil)));
  NewStrings.Assign(FItems);
  FItems.Free;
  FItems:= NewStrings;
end;

{------------------------------------------------------------------------------
  Method:  TCustomComboBox.AttachSignals
  Params:  none
  Returns: Nothing

  Gets called after the window is created but before the controls are created 
  and the cached propeties are set. 
  This is the only place where a call to SetCallBack is made.
 ------------------------------------------------------------------------------}
procedure TCustomComboBox.AttachSignals; 
begin
   inherited AttachSignals;
   Assert(False, Format('Trace:[TCustomComboBox.AttachSignals] %s', [ClassName]));
   SetCallback (LM_CHANGED);
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
  NewStrings:= TStringList.Create;
  NewStrings.Assign(FItems);
  FItems.Free;
  FItems:= NewStrings;
  inherited DestroyHandle;
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
  if Val <> FSorted then begin
    with AMessage do begin
      Msg:= LM_SORT;
      List:= Items;
      IsSorted:= Val;
    end;
    CNSendMessage(LM_SORT, Self, @AMessage);
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
  HandleNeeded;
  CNSendMessage(LM_SETLIMITTEXT, Self, @Val);
end;

{------------------------------------------------------------------------------
  Method: TCustomComboBox.GetMaxLength
  Params: ---
  Returns: the maximum length of user input

  Get the maximum length for user input.
 ------------------------------------------------------------------------------}
function TCustomComboBox.GetMaxLength : integer;
begin
  HandleNeeded;
  Result:= CNSendMessage(LM_GETLIMITTEXT, Self, nil);
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
  Method: TCustomComboBox.GetSelText
  Params: ---
  Returns: selected text

  Returns the selected part of text-field.
 ------------------------------------------------------------------------------}
function TCustomComboBox.GetSelText : string;
begin
  if FStyle < csDropDownList then Result:= Copy(Text, SelStart + 1, SelLength)
  else Result:= '';
end;

{------------------------------------------------------------------------------
  Method: TCustomComboBox.SetSelText
  Params: val - new string for text-field
  Returns: nothings

  Replace the selected part of text-field with "val".
 ------------------------------------------------------------------------------}
procedure TCustomComboBox.SetSelText(Val : string);
begin
  if FStyle <> csDropDownList then begin
    { First delete the actual selection }
    Text:= Concat(Copy(Text, 1, SelStart), Val, Copy(Text, SelStart + SelLength + 1, Length(Text)));
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
  HandleNeeded;
  Result:= CNSendMessage(LM_GETSELSTART, Self, nil);
end;

{------------------------------------------------------------------------------
  Method: TCustomComboBox.SetSelStart
  Params: val -
  Returns: nothing

  Sets starting index for selected text.
 ------------------------------------------------------------------------------}
procedure TCustomComboBox.SetSelStart(Val : integer);
begin
  HandleNeeded;
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
  HandleNeeded;
  Result:= CNSendMessage(LM_GETSELLEN, Self, nil);
end;

{------------------------------------------------------------------------------
  Method: TCustomComboBox.SetSelLength
  Params: val -
  Returns: nothing

  Sets length of selected text.
 ------------------------------------------------------------------------------}
procedure TCustomComboBox.SetSelLength(Val : integer);
begin
  HandleNeeded;
  CNSendMessage(LM_SETSELLEN, Self, Pointer(Val));
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
  end;
end;

{------------------------------------------------------------------------------
  Method: TCustomComboBox.SetItems
  Params: value - stringlist with items for combobox
  Returns: nothing

  Assigns items for ComboBox from a stringlist.
 ------------------------------------------------------------------------------}
procedure TCustomComboBox.SetItems(Value : TStrings);
begin
  if Value <> FItems then begin
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
   FItems:= TStringList.Create;
end;

{------------------------------------------------------------------------------
  Method: TCustomComboBox.Destroy
  Params: ---
  Returns: nothing

  Destroys the object.
 ------------------------------------------------------------------------------}
destructor TCustomComboBox.Destroy;
begin
  FItems.Free;
  inherited Destroy;
end;

{------------------------------------------------------------------------------
  Method: TCustomComboBox.GetItemIndex
  Params: ---
  Returns: index of the currently selected item

  Returns index of the currently selected item in the combobox. -1 is returned
  if no item is currently selected.
 ------------------------------------------------------------------------------}
function TCustomComboBox.GetItemIndex : integer;
var Counter : integer;
    CacheText : string;
begin
  { I am not sure whether the item remains selected after the user pressed the item }
  {  Result:= CNSendMessage(LM_GETITEMINDEX, Self, nil);}
  CacheText:= Text;
  Result:= -1;
  for Counter:= 0 to Items.Count - 1 do begin
    if Items.Strings[Counter] = CacheText then begin
      Result:= Counter;
      Break;
    end;
  end;
end;

{------------------------------------------------------------------------------
  Method: TCustomComboBox.SetItemIndex
  Params: Val - 
  Returns: nothing

  Sets ths index of the currently selected item in the combobox.
 ------------------------------------------------------------------------------}
procedure TCustomComboBox.SetItemIndex(Val : integer);
begin
  if (Val < 0) or (Val > FItems.Count) then raise Exception.Create('Out of bounds in TCustomComboBox.SetItemIndex');
  HandleNeeded;
  CNSendMessage(LM_SETITEMINDEX, Self, Pointer(Val));
end;

{
  $Log$
  Revision 1.2  2000/07/23 19:03:10  lazarus
  changed some comments, stoppok

  Revision 1.1  2000/07/13 10:28:25  michael
  + Initial import

  Revision 1.4  2000/07/09 20:41:44  lazarus
  Added Attachsignals method to custombobobox, stoppok

  Revision 1.3  2000/06/29 21:08:07  lazarus
  some minor improvements &more comments, stoppok

}
