// included by win32int.pp

{******************************************************************************
                                 win32listsl.inc
                 TWin32ListStringList and TWin32CListStringList

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

{$IFOPT H+}
  {$DEFINE H_PLUS}
{$ELSE}
  {$H+}
  {$UNDEF H_PLUS}
{$ENDIF}

{*************************************************************}
{                      TWin32ListStringList methods           }
{*************************************************************}

{------------------------------------------------------------------------------
  Method: TWin32ListStringList.Create
  Params:
  Returns:

 ------------------------------------------------------------------------------}
Constructor TWin32ListStringList.Create(List : HWND; TheOwner: TWinControl);
Begin
  Inherited Create;
  If List = HWND(Nil) Then
    Raise Exception.Create('Unspecified list window');
    //Assert(False, 'Trace:Unspecified list window');
  FWin32List := List;
  FSender := TheOwner;

  //Set proper win32 flags for ComboBox/ListBox and get/set Combo Height
  InitFlags;
  // Determine if the list is sorted
  FSorted := (UINT(GetWindowLong(FWin32List, GWL_STYLE)) and FFlagSort <> 0);
End;

procedure TWin32ListStringList.InitFlags;
begin
  FFlagSort         := UINT(LBS_SORT);
  FFlagGetText      := UINT(LB_GETTEXT);
  FFlagGetTextLen   := UINT(LB_GETTEXTLEN);
  FFlagGetCount     := UINT(LB_GETCOUNT);
  FFlagResetContent := UINT(LB_RESETCONTENT);
  FFlagDeleteString := UINT(LB_DELETESTRING);
  FFlagInsertString := UINT(LB_INSERTSTRING);
  FFlagAddString    := UINT(LB_ADDSTRING);
  FFlagGetItemData  := UINT(LB_GETITEMDATA);
  FFlagSetItemData  := UINT(LB_SETITEMDATA);
  FFlagGetItemIndex := UINT(LB_GETCURSEL);
  FFlagSetItemIndex := UINT(LB_SETCURSEL);
  FFlagGetSelected  := UINT(LB_GETSEL);
  FFlagSetSelected  := UINT(LB_SETSEL);
  FFlagInitStorage  := UINT(LB_INITSTORAGE);
end;

{------------------------------------------------------------------------------
  Method: TWin32ListStringList.SetSorted
  Params:
  Returns:

 ------------------------------------------------------------------------------}
Procedure TWin32ListStringList.SetSorted(Val: Boolean);
Begin
  If Val <> FSorted Then
  Begin
    FSorted:= Val;
    Sort;
  End;
End;

{------------------------------------------------------------------------------
  Method: TWin32ListStringList.Sort
  Params:
  Returns:

 ------------------------------------------------------------------------------}
Procedure TWin32ListStringList.Sort;
Begin
  // The win api doesn't allow to change the sort on the fly,
  // so is needed to recreate the window
  RecreateWnd(FSender);
End;


{------------------------------------------------------------------------------
  Method: TWin32ListStringList.AddStrings
  Params:
  Returns:

 ------------------------------------------------------------------------------}
procedure TWin32ListStringList.AddStrings(TheStrings: TStrings);
var
  Counter: Integer;
  AnIndex: LongInt;
begin
  For Counter := 0 To TheStrings.Count - 1 Do
  Begin
    {$ifdef WindowsUnicodeSupport}
    if UnicodeEnabledOS then
     AnIndex := Windows.SendMessageW(FWin32List, FFlagAddString, 0,
      LPARAM(PWideChar(Utf8Decode(TheStrings[Counter])))) //Insert
    else
     AnIndex := Windows.SendMessage(FWin32List, FFlagAddString, 0,
      LPARAM(PChar(Utf8ToAnsi(TheStrings[Counter])))); //Insert
    {$else}
    AnIndex := Windows.SendMessage(FWin32List, FFlagAddString, 0,
     LPARAM(PChar(TheStrings[Counter]))); //Insert
    {$endif}
    PutObject(AnIndex, TheStrings.Objects[Counter]);
  end;
end;

{------------------------------------------------------------------------------
  Method: TWin32ListStringList.Add
  Params:
  Returns:

 ------------------------------------------------------------------------------}
function TWin32ListStringList.Add(const S: string): Integer;
begin
  Result := Count;
  Insert(Count, S);
  if FSorted then
    Result := FLastInsertedIndex;
end;

{------------------------------------------------------------------------------
  Method: TWin32ListStringList.Get
  Params:
  Returns:

 ------------------------------------------------------------------------------}
Function TWin32ListStringList.Get(Index: Integer): String;
Var
{$ifdef WindowsUnicodeSupport}
  s: string;
  w: widestring;
{$else}
  Item: PChar;
{$endif}
Begin
  If (Index < 0) Or (Index >= Count) Then
    Raise Exception.Create('Out of bounds.')
  Else
  Begin
    {$ifdef WindowsUnicodeSupport}
    if UnicodeEnabledOS then begin
      SetLength(w, Windows.SendMessageW(FWin32List, FFlagGetTextLen, Index, 0));
      Windows.SendMessageW(FWin32List, FFlagGetText, Index, LPARAM(PWideChar(w)));
      Result := UTF8Encode(w);
    end
    else
    begin
      SetLength(s, Windows.SendMessage(FWin32List, FFlagGetTextLen, Index, 0));
      Windows.SendMessage(FWin32List, FFlagGetText, Index, LPARAM(PChar(s)));
      Result := AnsiToUtf8(s);
    end;
    {$else}
    Getmem(Item, Windows.SendMessage(FWin32List, FFlagGetTextLen, Index, 0)+1);
    Windows.SendMessage(FWin32List, FFlagGetText, Index, LPARAM(Item));
    Result := Item;
    FreeMem(Item);
    {$endif}
  End;
End;

{------------------------------------------------------------------------------
  Method: TWin32ListStringList.GetCount
  Params:
  Returns:

 ------------------------------------------------------------------------------}
Function TWin32ListStringList.GetCount: Integer;
Begin
  Result := Windows.SendMessage(FWin32List, FFlagGetCount, 0, 0);
End;

{------------------------------------------------------------------------------
  Method: TWin32ListStringList.Clear
  Params:
  Returns:

 ------------------------------------------------------------------------------}
Procedure TWin32ListStringList.Clear;
Begin
  Windows.SendMessage(FWin32List, FFlagResetContent, 0, 0);
End;

{------------------------------------------------------------------------------
  Method: TWin32ListStringList.Delete
  Params:
  Returns:

 ------------------------------------------------------------------------------}
Procedure TWin32ListStringList.Delete(Index: Integer);
Begin
  Windows.SendMessage(FWin32List, FFlagDeleteString, Index, 0);
End;

{------------------------------------------------------------------------------
  Method: TWin32ListStringList.GetObject
  Params:
  Returns:

 ------------------------------------------------------------------------------}
Function TWin32ListStringList.GetObject(Index: Integer): TObject;
Begin
  HWND(Result) := Windows.SendMessage(FWin32List, FFlagGetItemData, Index, 0);
End;

{------------------------------------------------------------------------------
  Method: TWin32ListStringList.Insert
  Params:
  Returns:

 ------------------------------------------------------------------------------}
procedure TWin32ListStringList.Insert(Index: Integer; Const S: String);
begin
  FLastInsertedIndex := Index;
  if FSorted then
  begin
    {$ifdef WindowsUnicodeSupport}
    if UnicodeEnabledOS then
      FLastInsertedIndex := Windows.SendMessageW(FWin32List, FFlagAddString, 0, LPARAM(PWideChar(Utf8Decode(S))))
    else
      FLastInsertedIndex := Windows.SendMessage(FWin32List, FFlagAddString, 0, LPARAM(PChar(Utf8ToAnsi(S))));
    {$else}
    FLastInsertedIndex := Windows.SendMessage(FWin32List, FFlagAddString, 0, LPARAM(PChar(S)));
    {$endif}
  end
  else
  begin
    {$ifdef WindowsUnicodeSupport}
    if UnicodeEnabledOS then
      Windows.SendMessageW(FWin32List, FFlagInsertString, Index, LPARAM(PWideChar(Utf8Decode(S))))
    else
      Windows.SendMessage(FWin32List, FFlagInsertString, Index, LPARAM(PChar(Utf8ToAnsi(S))));
    {$else}
    Windows.SendMessage(FWin32List, FFlagInsertString, Index, LPARAM(PChar(S)));
    {$endif}
  end;
end;

procedure TWin32ListStringList.Put(Index: integer; const S: string);
var
  lItemIndex: integer;
  lSelected: boolean;
begin
  // remember selection
  lItemIndex := -1;
  if FFlagGetSelected <> 0 then
  begin
    lItemIndex := SendMessage(FWin32List, FFlagGetSelected, Index, 0);
    lSelected := lItemIndex > 0;
    if lItemIndex <> LB_ERR then
      lItemIndex := Index;
  end;
  if lItemIndex = -1 then
  begin
    lItemIndex := SendMessage(FWin32List, FFlagGetItemIndex, 0, 0);
    lSelected := lItemIndex >= 0;
  end;
  
  inherited;
 
  if lSelected then
  begin
    if (FFlagSetSelected = 0) 
      or (SendMessage(FWin32List, FFlagSetSelected, Windows.WParam(true), lItemIndex) = -1) then
    begin
      SendMessage(FWin32List, FFlagSetItemIndex, lItemIndex, 0);
    end;
  end;
end;
    
{------------------------------------------------------------------------------
  Method: TWin32ListStringList.PutObject
  Params:
  Returns:

 ------------------------------------------------------------------------------}
procedure TWin32ListStringList.PutObject(Index: Integer; AObject: TObject);
begin
  Windows.SendMessage(FWin32List, FFlagSetItemData, Index, LPARAM(AObject));
end;

procedure TWin32ListStringList.SetCapacity(NewCapacity: Integer);
begin
  Windows.SendMessage(FWin32List, FFlagInitStorage, NewCapacity, 0);
end;

procedure TWin32ListStringList.SetUpdateState(Updating: Boolean);
begin
  Windows.SendMessage(FWin32List, WM_SETREDRAW, WPARAM(not Updating), 0);
  if not Updating then
    Windows.InvalidateRect(FWin32List, nil, TRUE);
end;

{ TWin32ComboBoxStringList }

procedure TWin32ComboBoxStringList.InitFlags;
var
  R: TRect;
begin
  FFlagSort         := UINT(CBS_SORT);
  FFlagGetText      := UINT(CB_GETLBTEXT);
  FFlagGetTextLen   := UINT(CB_GETLBTEXTLEN);
  FFlagGetCount     := UINT(CB_GETCOUNT);
  FFlagResetContent := UINT(CB_RESETCONTENT);
  FFlagDeleteString := UINT(CB_DELETESTRING);
  FFlagInsertString := UINT(CB_INSERTSTRING);
  FFlagAddString    := UINT(CB_ADDSTRING);
  FFlagGetItemData  := UINT(CB_GETITEMDATA);
  FFlagSetItemData  := UINT(CB_SETITEMDATA);
  FFlagGetItemIndex := UINT(CB_GETCURSEL);
  FFlagSetItemIndex := UINT(CB_SETCURSEL);
  FFlagGetSelected  := UINT(0);
  FFlagSetSelected  := UINT(0);
  FFlagInitStorage  := UINT(CB_INITSTORAGE);
  //Get edit and item sizes
  Windows.GetClientRect(FWin32List, @R);
  FEditHeight := R.Bottom;
  FItemHeight := Windows.SendMessage(FWin32List, CB_GETITEMHEIGHT, 0, 0);
  FDropDownCount := TComboBox(FSender).DropDownCount;
  if FDropDownCount = 0 then
    FDropDownCount := 8;
end;

procedure TWin32ComboBoxStringList.Assign(Source: TPersistent);
var
  EditText: string;
  lItemIndex: integer;
begin
  if Source is TStrings then
  begin
    // save text in edit box, assigning strings clears the text
    TWin32WSCustomComboBox.GetText(FSender, EditText);

    inherited Assign(Source);
    
    // restore text in edit box
    UpdateComboHeight;
    TWin32WSCustomComboBox.SetText(FSender, EditText);
    lItemIndex := IndexOf(EditText);
    if lItemIndex <> -1 then
      TWin32WSCustomComboBox.SetItemIndex(TCustomComboBox(FSender), lItemIndex);
  end else
    inherited Assign(Source);
end;

function TWin32ComboBoxStringList.GetComboHeight: integer;
begin
  if (FSender is TCustomComboBox) and (TCustomComboBox(FSender).Style = csSimple) then
  begin
    // combobox workaround:
    // if style = csSimple FEditHeight = Edit Height + Listbox Height
    Result := FEditHeight;
  end else
  begin
    if Count = 0 then
    begin
      Result := FEditHeight + FItemHeight + 2;
    end else
    begin
      Result := FEditHeight + FDropDownCount * FItemHeight + 2;
    end;
  end;
end;

procedure TWin32ComboBoxStringList.UpdateComboHeight;
var
  Left, Top, Width, Height: integer;
begin
  Left := FSender.Left;
  Top := FSender.Top;
  Width := FSender.Width;
  Height := ComboHeight;
  LCLBoundsToWin32Bounds(FSender, Left, Top, Width, Height);
  MoveWindow(FSender.Handle, Left, Top, Width, Height, true);
  LCLControlSizeNeedsUpdate(FSender, true);
end;

procedure TWin32ComboBoxStringList.Clear;
begin
  inherited;
  UpdateComboHeight;
end;

procedure TWin32ComboBoxStringList.Delete(Index: integer); 
begin
  inherited Delete(Index);
  if Count <= 1 then
    UpdateComboHeight;
end;

procedure TWin32ComboBoxStringList.Insert(Index: integer; const S: string); 
begin
  inherited Insert(Index, S);
  if GetCount = 1 then
    UpdateComboHeight;
end;

    
{ TWin32CheckListBoxStrings }

constructor TWin32CheckListBoxStrings.Create(List : HWND; TheOwner: TWinControl);
begin
  inherited Create(List, TheOwner);
  with FDefaultItem do
  begin
    Checked := false;
    TheObject := nil;
  end;
end;

function TWin32CheckListBoxStrings.GetChecked(const Index: Integer): Boolean;
var
  Data: PWin32CheckListBoxItemRecord;
begin
  Data := GetItemRecord(Index, false);
  Result := Data^.Checked
end;

procedure TWin32CheckListBoxStrings.SetChecked(const Index: Integer;
  const AValue: Boolean);
var
  ItemRecord: PWin32CheckListBoxItemRecord;
begin
  ItemRecord := GetItemRecord(Index, true);
  ItemRecord^.Checked := AValue;
  SetItemRecord(Index, ItemRecord);
end;

function TWin32CheckListBoxStrings.GetItemRecord(const Index: Integer;
  const CreateNew: boolean): PWin32CheckListBoxItemRecord;
begin
  Result := PWin32CheckListBoxItemRecord(Windows.SendMessage(FWin32List, LB_GETITEMDATA, Index, 0));
  if (not Assigned(Result)) then
  begin
    if CreateNew then
    begin
      Result := new(PWin32CheckListBoxItemRecord);
      Result^ := FDefaultItem;
    end
    else Result := @FDefaultItem;
  end;
end;

procedure TWin32CheckListBoxStrings.SetItemRecord(const Index: Integer;
  ItemRecord: PWin32CheckListBoxItemRecord);
begin
  Windows.SendMessage(FWin32List, LB_SETITEMDATA, Index, LPARAM(ItemRecord));
end;

procedure TWin32CheckListBoxStrings.Clear;
begin
  DeleteItemRecords(FWin32List);
  inherited Clear;
end;

procedure TWin32CheckListBoxStrings.Delete(Index: Integer);
begin
  DeleteItemRecord(FWin32List, Index);
  inherited Delete(Index);
end;

function TWin32CheckListBoxStrings.GetObject(Index: Integer): TObject;
begin
  Result:= GetItemRecord(Index, false)^.TheObject;
end;

procedure TWin32CheckListBoxStrings.PutObject(Index: Integer; AObject: TObject);
var
  ItemRecord: PWin32CheckListBoxItemRecord;
begin
  ItemRecord := GetItemRecord(Index, true);
  ItemRecord^.TheObject := AObject;
  SetItemRecord(Index, ItemRecord);
end;

class procedure TWin32CheckListBoxStrings.DeleteItemRecords(const List: HWND);
var
  Index: Integer;
  ItemCount: Integer;
begin
  ItemCount := Windows.SendMessage(List, LB_GETCOUNT, 0, 0);
  for Index := 0 to ItemCount-1 do
    DeleteItemRecord(List, Index);
end;

class procedure TWin32CheckListBoxStrings.DeleteItemRecord(const List: HWND;const Index: integer);
var
  ItemRecord: PWin32CheckListBoxItemRecord;
begin
  ItemRecord := PWin32CheckListBoxItemRecord(Windows.SendMessage(List, LB_GETITEMDATA, Index, 0));
  if Assigned(ItemRecord)
    then Dispose(ItemRecord);
end;

{*************************************************************}
{                      TWin32CListStringList methods            }
{*************************************************************}

{------------------------------------------------------------------------------
  Method: TWin32CListStringList.Create
  Params:
  Returns:

 ------------------------------------------------------------------------------}
Constructor TWin32CListStringList.Create(List : HWND; TheOwner: TWinControl);
Begin
  Inherited Create;
  If List = HWND(Nil) Then
    Raise Exception.Create('Unspecified list widget');
  FWin32CList := List;
  FSorted := (GetWindowLong(FWin32CList, GWL_STYLE) and LBS_SORT <> 0);
  FSender:=TheOwner;
End;

{------------------------------------------------------------------------------
  Method: TWin32CListStringList.SetSorted
  Params:
  Returns:

 ------------------------------------------------------------------------------}
Procedure TWin32CListStringList.SetSorted(Val: Boolean);
Begin
  If Val <> FSorted Then
  Begin
    FSorted := Val;
    Sort;
  End;
End;

{------------------------------------------------------------------------------
  Method: TWin32CListStringList.Sort
  Params:
  Returns:

 ------------------------------------------------------------------------------}
Procedure TWin32CListStringList.Sort;
Begin
  // The win api doesn't allow to change the sort on the fly,
  // so is needed to recreate the window
  RecreateWnd(FSender);
End;

{------------------------------------------------------------------------------
  Method: TWin32CListStringList.Assign
  Params:
  Returns:

 ------------------------------------------------------------------------------}
Procedure TWin32CListStringList.Assign(Source: TPersistent);
Var
  Counter: Integer;
Begin
  { Do not call inherited Assign as it does things we do not want to happen }
  If Source Is TStrings Then
  Begin
    Clear;
    For Counter :=  0 To (TStrings(Source).Count - 1) Do
      InsertObject(0, TStrings(Source)[Counter], TStrings(Source).Objects[Counter]);
  End
  Else
    Inherited Assign(Source);
End;

{------------------------------------------------------------------------------
  Method: TWin32CListStringList.Clear
  Params:
  Returns:

 ------------------------------------------------------------------------------}
Procedure TWin32CListStringList.Clear;
Begin
  Windows.SendMessage(FWin32CList, LB_RESETCONTENT, 0, 0);
End;

{------------------------------------------------------------------------------
  Method: TWin32CListStringList.Delete
  Params:
  Returns:

 ------------------------------------------------------------------------------}
procedure TWin32CListStringList.Delete(Index: Integer);
begin
  Windows.SendMessage(FWin32CList, LB_DELETESTRING, Index, 0);
end;

{------------------------------------------------------------------------------
  Method: TWin32CListStringList.Get
  Params:
  Returns:

 ------------------------------------------------------------------------------}
Function TWin32CListStringList.Get(Index: Integer): String;
Var
  Item: PChar;
Begin
  If (Index < 0) Or (Index >= Count) Then
    Raise Exception.Create('Out of bounds.')
  Else
  Begin
  	Getmem(Item,Windows.SendMessage(FWin32CList,LB_GETTEXTLEN,Index,0)+1);
    Windows.SendMessage(FWin32CList, LB_GETTEXT, Index, LPARAM(Item));
    Result := StrPas(Item);
    Dispose(Item);
  End;
End;

{------------------------------------------------------------------------------
  Method: TWin32CListStringList.GetCount
  Params:
  Returns:

 ------------------------------------------------------------------------------}
Function TWin32CListStringList.GetCount: Integer;
Begin
  Result := Windows.SendMessage(FWin32CList, LB_GETCOUNT, 0, 0);
End;

{------------------------------------------------------------------------------
  Method: TWin32CListStringList.GetObject
  Params:
  Returns:

 ------------------------------------------------------------------------------}
Function TWin32CListStringList.GetObject(Index: Integer): TObject;
Begin
  HWND(Result) := Windows.SendMessage(FWin32CList, LB_GETITEMDATA, Index, 0);
End;

{------------------------------------------------------------------------------
  Method: TWin32CListStringList.Insert
  Params:
  Returns:

 ------------------------------------------------------------------------------}
Procedure TWin32CListStringList.Insert(Index: Integer; Const S: String);
Begin
  If FSorted Then
    Windows.SendMessage(FWin32CList,LB_ADDSTRING, 0, LPARAM(PChar(S)))
  Else
    Windows.SendMessage(FWin32CList,LB_INSERTSTRING, Index, LPARAM(PChar(S)));
End;

{------------------------------------------------------------------------------
  Method: TWin32CListStringList.PutObject
  Params:
  Returns:

 ------------------------------------------------------------------------------}
Procedure TWin32CListStringList.PutObject(Index: Integer; AObject: TObject);
Begin
  Windows.SendMessage(FWin32CList, LB_SETITEMDATA, Index, LPARAM(AObject));
End;

{$IFDEF H_PLUS}
  {$UNDEF H_PLUS}
{$ELSE}
  {$H-}
{$ENDIF}
