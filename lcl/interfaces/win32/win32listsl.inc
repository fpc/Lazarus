{%MainUnit win32int.pp}

{******************************************************************************
                                 win32listsl.inc
  TWin32ListStringList, TWin32ComboBoxStringList and TWin32CheckListBoxStrings

 ******************************************************************************

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
constructor TWin32ListStringList.Create(List : HWND; TheOwner: TWinControl);
begin
  inherited Create;
  if List = HWND(nil) then
    raise Exception.Create('Unspecified list window');
    //Assert(False, 'Trace:Unspecified list window');
  FWin32List := List;
  FSender := TheOwner;

  //Set proper win32 flags for ComboBox/ListBox and get/set Combo Height
  InitFlags;
  // Determine if the list is sorted
  FSorted := (UINT(GetWindowLong(FWin32List, GWL_STYLE)) and FFlagSort <> 0);
end;

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
procedure TWin32ListStringList.SetSorted(Val: Boolean);
begin
  if Val <> FSorted then
  begin
    FSorted:= Val;
    Sort;
  end;
end;

function TWin32ListStringList.SaveData(AIndex: Integer): Pointer;
begin
  Result := GetObject(AIndex);
end;

procedure TWin32ListStringList.RestoreData(AIndex: Integer; AData: Pointer);
begin
  PutObject(AIndex, TObject(AData));
end;

{------------------------------------------------------------------------------
  Method: TWin32ListStringList.Sort
  Params:
  Returns:

 ------------------------------------------------------------------------------}
procedure TWin32ListStringList.Sort;
begin
  // The win api doesn't allow to change the sort on the fly,
  // so is needed to recreate the window
  if not (csDestroyingHandle in FSender.ControlState) then
    RecreateWnd(FSender);
end;


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
  for Counter := 0 To TheStrings.Count - 1 Do
  begin
    {$ifdef WindowsUnicodeSupport}
    if UnicodeEnabledOS then
     AnIndex := Windows.SendMessageW(FWin32List, FFlagAddString, 0,
      LPARAM(PWideChar(UTF8ToUTF16(TheStrings[Counter])))) //Insert
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
function TWin32ListStringList.Get(Index: Integer): String;
Var
{$ifdef WindowsUnicodeSupport}
  s: string;
  w: widestring;
{$else}
  Item: PChar;
{$endif}
begin
  if (Index < 0) Or (Index >= Count) then
    raise Exception.Create('Out of bounds.')
  else
  begin
    {$ifdef WindowsUnicodeSupport}
    if UnicodeEnabledOS then
    begin
      SetLength(w, Windows.SendMessageW(FWin32List, FFlagGetTextLen, Index, 0));
      Windows.SendMessageW(FWin32List, FFlagGetText, Index, LPARAM(PWideChar(w)));
      Result := UTF16ToUTF8(w);
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
  end;
end;

{------------------------------------------------------------------------------
  Method: TWin32ListStringList.GetCount
  Params:
  Returns:

 ------------------------------------------------------------------------------}
function TWin32ListStringList.GetCount: Integer;
begin
  Result := Windows.SendMessage(FWin32List, FFlagGetCount, 0, 0);
end;

{------------------------------------------------------------------------------
  Method: TWin32ListStringList.Clear
  Params:
  Returns:

 ------------------------------------------------------------------------------}
procedure TWin32ListStringList.Clear;
begin
  Windows.SendMessage(FWin32List, FFlagResetContent, 0, 0);
end;

{------------------------------------------------------------------------------
  Method: TWin32ListStringList.Delete
  Params:
  Returns:

 ------------------------------------------------------------------------------}
procedure TWin32ListStringList.Delete(Index: Integer);
begin
  Windows.SendMessage(FWin32List, FFlagDeleteString, Index, 0);
end;

{------------------------------------------------------------------------------
  Method: TWin32ListStringList.GetObject
  Params:
  Returns:

 ------------------------------------------------------------------------------}
function TWin32ListStringList.GetObject(Index: Integer): TObject;
begin
  Result := TObject(PtrInt(Windows.SendMessage(FWin32List, FFlagGetItemData, Index, 0)));
end;

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
      FLastInsertedIndex := Windows.SendMessageW(FWin32List, FFlagAddString, 0, LPARAM(PWideChar(UTF8ToUTF16(S))))
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
      Windows.SendMessageW(FWin32List, FFlagInsertString, Index, LPARAM(PWideChar(UTF8ToUTF16(S))))
    else
      Windows.SendMessage(FWin32List, FFlagInsertString, Index, LPARAM(PChar(Utf8ToAnsi(S))));
    {$else}
    Windows.SendMessage(FWin32List, FFlagInsertString, Index, LPARAM(PChar(S)));
    {$endif}
  end;
end;

procedure TWin32ListStringList.Put(Index: integer; const S: string);
var
  lItemIndex: Integer;
  lSelected: Boolean;
  AData: Pointer;
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

  // preserve data
  AData := SaveData(Index);
  
  inherited;
 
  if AData <> nil then
    RestoreData(Index, AData);

  if lSelected then
  begin
    if (FFlagSetSelected = 0) or (SendMessage(FWin32List, FFlagSetSelected, Windows.WParam(true), lItemIndex) = -1) then
      SendMessage(FWin32List, FFlagSetItemIndex, lItemIndex, 0);
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
    Windows.RedrawWindow(FWin32List, nil, 0, RDW_INVALIDATE or RDW_ALLCHILDREN or RDW_ERASE)
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
  FDropDownCount := TCustomComboBox(FSender).DropDownCount;
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
    if EditText = '' then
     lItemIndex := -1
    else
      lItemIndex := IndexOf(EditText);
    if lItemIndex <> -1 then
      TWin32WSCustomComboBox.SetItemIndex(TCustomComboBox(FSender), lItemIndex);
  end
  else
    inherited Assign(Source);
end;

procedure TWin32ComboBoxStringList.SetDropDownCount(const AValue: integer);
begin
  FDropDownCount:=AValue;
  UpdateComboHeight;
end;

function TWin32ComboBoxStringList.GetComboHeight: integer;
begin
  if (FSender is TCustomComboBox) and (TCustomComboBox(FSender).Style = csSimple) then
  begin
    // combobox workaround:
    // if style = csSimple follow the LCL height.
    Result := FSender.Height;
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
var
  SaveText: String;
  SavePos, SaveLen: Integer;
begin
  if not TCustomComboBox(FSender).ReadOnly then
  begin
    SaveText := TCustomComboBox(FSender).Text;
    SavePos := TCustomComboBox(FSender).SelStart;
    SaveLen := TCustomComboBox(FSender).SelLength;
  end;
  inherited;
  UpdateComboHeight;
  if not TCustomComboBox(FSender).ReadOnly then
  begin
    TCustomComboBox(FSender).Text := SaveText;
    TCustomComboBox(FSender).SelStart := SavePos;
    TCustomComboBox(FSender).SelLength := SaveLen;
  end;
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

constructor TWin32CheckListBoxStrings.Create(List: HWND; TheOwner: TWinControl);
begin
  inherited Create(List, TheOwner);
  with FDefaultItem do
  begin
    State := cbUnchecked;
    Enabled := True;
    TheObject := nil;
  end;
end;

function TWin32CheckListBoxStrings.GetState(Index: Integer): TCheckBoxState;
var
  Data: PWin32CheckListBoxItemRecord;
begin
  Data := GetItemRecord(Index, false);
  Result := Data^.State;
end;

function TWin32CheckListBoxStrings.GetEnabled(Index: Integer): Boolean;
var
  Data: PWin32CheckListBoxItemRecord;
begin
  Data := GetItemRecord(Index, false);
  Result := Data^.Enabled;
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

procedure TWin32CheckListBoxStrings.SetEnabled(Index: Integer;
  const AValue: Boolean);
var
  ItemRecord: PWin32CheckListBoxItemRecord;
begin
  ItemRecord := GetItemRecord(Index, true);
  ItemRecord^.Enabled := AValue;
  SetItemRecord(Index, ItemRecord);
end;

procedure TWin32CheckListBoxStrings.SetItemRecord(const Index: Integer;
  ItemRecord: PWin32CheckListBoxItemRecord);
begin
  Windows.SendMessage(FWin32List, LB_SETITEMDATA, Index, LPARAM(ItemRecord));
end;

procedure TWin32CheckListBoxStrings.SetState(Index: Integer;
  const AValue: TCheckBoxState);
var
  ItemRecord: PWin32CheckListBoxItemRecord;
begin
  ItemRecord := GetItemRecord(Index, true);
  ItemRecord^.State := AValue;
  SetItemRecord(Index, ItemRecord);
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

function TWin32CheckListBoxStrings.SaveData(AIndex: Integer): Pointer;
var
  ItemRecord: PWin32CheckListBoxItemRecord;
begin
  ItemRecord := GetItemRecord(AIndex, False);
  if ItemRecord = nil then
    Result := nil
  else
  begin
    Result := new(PWin32CheckListBoxItemRecord);
    PWin32CheckListBoxItemRecord(Result)^ := ItemRecord^;
  end;
end;

procedure TWin32CheckListBoxStrings.RestoreData(AIndex: Integer; AData: Pointer);
var
  ItemRecord: PWin32CheckListBoxItemRecord absolute AData;
  OldRecord: PWin32CheckListBoxItemRecord;
begin
  if ItemRecord <> nil then
  begin
    OldRecord := GetItemRecord(AIndex, True);
    OldRecord^ := ItemRecord^;
    SetItemRecord(AIndex, OldRecord);
    Dispose(ItemRecord);
  end;
end;

class procedure TWin32CheckListBoxStrings.DeleteItemRecords(const List: HWND);
var
  Index: Integer;
  ItemCount: Integer;
begin
  ItemCount := Windows.SendMessage(List, LB_GETCOUNT, 0, 0);
  for Index := 0 to ItemCount - 1 do
    DeleteItemRecord(List, Index);
end;

class procedure TWin32CheckListBoxStrings.DeleteItemRecord(const List: HWND; const Index: integer);
var
  ItemRecord: PWin32CheckListBoxItemRecord;
begin
  ItemRecord := PWin32CheckListBoxItemRecord(Windows.SendMessage(List, LB_GETITEMDATA, Index, 0));
  if Assigned(ItemRecord) then
    Dispose(ItemRecord);
end;

{$IFDEF H_PLUS}
  {$UNDEF H_PLUS}
{$ELSE}
  {$H-}
{$ENDIF}
