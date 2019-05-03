{%MainUnit winceint.pp}

{******************************************************************************
                                 wincelistsl.inc
  TWinCEListStringList, TWinCEComboBoxStringList and TWinCECheckListBoxStrings

 ******************************************************************************

 *****************************************************************************
  This file is part of the Lazarus Component Library (LCL)

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************
}

{$IFOPT H+}
  {$DEFINE H_PLUS}
{$ELSE}
  {$H+}
  {$UNDEF H_PLUS}
{$ENDIF}

{*************************************************************}
{                      Default compare function               }
{*************************************************************}

function DefaultCompareFunc(A, B: HWND): Integer; CDecl;
Var
  AStr, BStr: PWideChar;
Begin
  AStr:=nil;
  BStr:=nil;
  GetWindowTextW(A, AStr, GetWindowTextLength(A) + 1);
  GetWindowTextW(B, BStr, GetWindowTextLength(B) + 1);
  Result := WideCompareStr(widestring(AStr), widestring(BStr));//roozbeh:does this work?!
end;

{*************************************************************}
{                      TWinCEListStringList methods           }
{*************************************************************}

{------------------------------------------------------------------------------
  Method: TWinCEListStringList.Create
  Params:
  Returns:

 ------------------------------------------------------------------------------}
constructor TWinCEListStringList.Create(List : HWND; TheOwner: TWinControl);
begin
  inherited Create;
  if List = HWND(nil) then
    Raise Exception.Create('Unspecified list window');
    //DebugLn('Trace:Unspecified list window');
  FWinCEList := List;
  FSender := TheOwner;

  //Set proper wince flags for ComboBox/ListBox and get/set Combo Height
  InitFlags;
  // Determine if the list is sorted
  FSorted := (UINT(GetWindowLong(FWinCEList, GWL_STYLE)) and FFlagSort <> 0);
end;

procedure TWinCEListStringList.InitFlags;
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
end;

{------------------------------------------------------------------------------
  Method: TWinCEListStringList.SetSorted
  Params:
  Returns:

 ------------------------------------------------------------------------------}
procedure TWinCEListStringList.SetSorted(Val: Boolean);
Begin
  If Val <> FSorted Then
  Begin
    FSorted:= Val;
    Sort;
  End;
End;

{------------------------------------------------------------------------------
  Method: TWinCEListStringList.Sort
  Params:
  Returns:

 ------------------------------------------------------------------------------}
procedure TWinCEListStringList.Sort;
Begin
  // The win api doesn't allow to change the sort on the fly,
  // so is needed to recreate the window
  RecreateWnd(FSender);
End;

{------------------------------------------------------------------------------
  Method: TWinCEListStringList.Assign
  Params:
  Returns:

 ------------------------------------------------------------------------------}
procedure TWinCEListStringList.Assign(Source: TPersistent);
Var
  S: TStrings;
  Counter: Integer;
  AnIndex: Integer;
  tmpStr : widestring;
Begin
  { Do not call inherited Assign as it does things we do not want to happen }
  If Source Is TStrings Then
  Begin
    S:= TStrings(Source);
    QuoteChar:=S.QuoteChar;
    Delimiter:=S.Delimiter;
    NameValueSeparator:=S.NameValueSeparator;
    Windows.SendMessage(FWinCEList, FFlagResetContent, 0, 0);
    For Counter := 0 To (TStrings(Source).Count - 1) Do
    Begin
      tmpStr := UTF8Decode(s[Counter]);
      AnIndex := Windows.SendMessageW(FWinCEList, FFlagAddString, 0,
       LPARAM(PWideChar(tmpStr))); //Insert
      PutObject(AnIndex, S.Objects[Counter]);
    end;
  End
  Else
    inherited Assign(Source);
End;


{------------------------------------------------------------------------------
  Method: TWinCEListStringList.Add
  Params:
  Returns:

 ------------------------------------------------------------------------------}
function TWinCEListStringList.Add(const S: string): Integer;
begin
  Result := Count;
  Insert(Count, S);
  if FSorted then
    Result := FLastInsertedIndex;
end;

{------------------------------------------------------------------------------
  Method: TWinCEListStringList.Get
  Params:
  Returns:

 ------------------------------------------------------------------------------}
function TWinCEListStringList.Get(Index: Integer): String;
Var
  w: widestring;
Begin
  If (Index < 0) Or (Index >= Count) Then
    Raise Exception.Create('Out of bounds.')
  Else
  Begin
    SetLength(w, Windows.SendMessageW(FWinCEList, FFlagGetTextLen, Index, 0));
    Windows.SendMessageW(FWinCEList, FFlagGetText, Index, LPARAM(PWideChar(w)));
    Result := UTF8Encode(w);
  End;
End;

{------------------------------------------------------------------------------
  Method: TWinCEListStringList.GetCount
  Params:
  Returns:

 ------------------------------------------------------------------------------}
function TWinCEListStringList.GetCount: Integer;
Begin
  Result := Windows.SendMessage(FWinCEList, FFlagGetCount, 0, 0);
End;

{------------------------------------------------------------------------------
  Method: TWinCEListStringList.Clear
  Params:
  Returns:

 ------------------------------------------------------------------------------}
procedure TWinCEListStringList.Clear;
Begin
  Windows.SendMessage(FWinCEList, FFlagResetContent, 0, 0);
End;

{------------------------------------------------------------------------------
  Method: TWinCEListStringList.Delete
  Params:
  Returns:

 ------------------------------------------------------------------------------}
procedure TWinCEListStringList.Delete(Index: Integer);
Begin
  Windows.SendMessage(FWinCEList, FFlagDeleteString, Index, 0);
End;

{------------------------------------------------------------------------------
  Method: TWinCEListStringList.GetObject
  Params:
  Returns:

 ------------------------------------------------------------------------------}
function TWinCEListStringList.GetObject(Index: Integer): TObject;
Begin
  HWND(Result) := Windows.SendMessageW(FWinCEList, FFlagGetItemData, Index, 0);
End;

{------------------------------------------------------------------------------
  Method: TWinCEListStringList.Insert
  Params:
  Returns:

 ------------------------------------------------------------------------------}
procedure TWinCEListStringList.Insert(Index: Integer; Const S: String);
Begin
  FLastInsertedIndex := Index;
  if FSorted then
    FLastInsertedIndex := Windows.SendMessageW(FWinCEList, FFlagAddString, 0, LPARAM(PWideChar(Utf8Decode(S))))
  else
    Windows.SendMessageW(FWinCEList, FFlagInsertString, Index, LPARAM(PWideChar(Utf8Decode(S))));
End;

procedure TWinCEListStringList.Put(Index: integer; const S: string);
var
  lItemIndex: integer;
  lSelected: boolean;
begin
  // remember selection
  lItemIndex := -1;
  if FFlagGetSelected <> 0 then
  begin
    lItemIndex := SendMessageW(FWinCEList, FFlagGetSelected, Index, 0);
    lSelected := lItemIndex > 0;
    if lItemIndex <> LB_ERR then
      lItemIndex := Index;
  end;
  if lItemIndex = -1 then
  begin
    lItemIndex := SendMessageW(FWinCEList, FFlagGetItemIndex, 0, 0);
    lSelected := true;
  end;
  
  inherited;
 
  if lSelected then
  begin
    if (FFlagSetSelected = 0) 
      or (SendMessageW(FWinCEList, FFlagSetSelected, Windows.WParam(true), lItemIndex) = -1) then
    begin
      SendMessageW(FWinCEList, FFlagSetItemIndex, lItemIndex, 0);
    end;
  end;
end;
    
{------------------------------------------------------------------------------
  Method: TWinCEListStringList.PutObject
  Params:
  Returns:

 ------------------------------------------------------------------------------}
procedure TWinCEListStringList.PutObject(Index: Integer; AObject: TObject);
Begin
  Windows.SendMessage(FWinCEList, FFlagSetItemData, Index, LPARAM(AObject));
End;

{ TWinCEComboBoxStringList }

procedure TWinCEComboBoxStringList.InitFlags;
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
  //Get edit and item sizes
  Windows.GetClientRect(FWinCEList,@R);
  FEditHeight := R.Bottom;
  FItemHeight := Windows.SendMessage(FWinCEList, CB_GETITEMHEIGHT, 0, 0);
  FDropDownCount := TComboBox(FSender).DropDownCount;
  If FDropDownCount = 0 then
    FDropDownCount := 8;
end;

procedure TWinCEComboBoxStringList.UpdateComboHeight;
var
  Left, Top, Width, Height: integer;
begin
  Left := FSender.Left;
  Top := FSender.Top;
  Width := FSender.Width;
  Height := ComboHeight;
  LCLBoundsToWin32Bounds(FSender, Left, Top);
  MoveWindow(FSender.Handle, Left, Top, Width, Height, true);
  LCLControlSizeNeedsUpdate(FSender, true);
end;

procedure TWinCEComboBoxStringList.Assign(Source: TPersistent);
var
  EditText: string;
  lItemIndex: integer;
begin
  if Source is TStrings then
  begin
    // save text in edit box, assigning strings clears the text
    TWinCEWSCustomComboBox.GetText(FSender, EditText);

    inherited Assign(Source);
    
    // restore text in edit box
    UpdateComboHeight;
    TWinCEWSCustomComboBox.SetText(FSender, EditText);
    lItemIndex := IndexOf(EditText);
    if lItemIndex <> -1 then
      TWinCEWSCustomComboBox.SetItemIndex(TCustomComboBox(FSender), lItemIndex);
  end else
    inherited Assign(Source);
end;

function TWinCEComboBoxStringList.GetComboHeight: integer;
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

procedure TWinCEComboBoxStringList.Clear;
var
  SaveText: String;
begin
  if not TCustomComboBox(FSender).ReadOnly then
    SaveText := TCustomComboBox(FSender).Text;
  inherited;
  UpdateComboHeight;
  if not TCustomComboBox(FSender).ReadOnly then
    TCustomComboBox(FSender).Text := SaveText;
end;

procedure TWinCEComboBoxStringList.Delete(Index: integer);
begin
  inherited Delete(Index);
  if Count <= 1 then
    UpdateComboHeight;
end;

procedure TWinCEComboBoxStringList.Insert(Index: integer; const S: string);
begin
  inherited Insert(Index, S);
  if GetCount = 1 then
    UpdateComboHeight;
end;

    
{ TWinCECheckListBoxStrings }

constructor TWinCECheckListBoxStrings.Create(List : HWND; TheOwner: TWinControl);
begin
  inherited Create(List, TheOwner);
  with FDefaultItem do
  begin
    State := cbUnchecked;
    TheObject := nil;
  end;
end;

function TWinCECheckListBoxStrings.GetState(AIndex: Integer): TCheckBoxState;
var
  Data: PWinCECheckListBoxItemRecord;
begin
  Data := GetItemRecord(AIndex, False);
  Result := Data^.State
end;

function TWinCECheckListBoxStrings.GetItemRecord(const Index: Integer;
  const CreateNew: boolean): PWinCECheckListBoxItemRecord;
begin
  Result := PWinCECheckListBoxItemRecord(Windows.SendMessage(FWinCEList, LB_GETITEMDATA, Index, 0));
  if (not Assigned(Result)) then begin
    if CreateNew then begin
      Result := new(PWinCECheckListBoxItemRecord);
      Result^ := FDefaultItem;
    end
    else Result := @FDefaultItem;
  end;
end;

procedure TWinCECheckListBoxStrings.SetItemRecord(const Index: Integer;
  ItemRecord: PWinCECheckListBoxItemRecord);
begin
  Windows.SendMessage(FWinCEList, LB_SETITEMDATA, Index, LPARAM(ItemRecord));
end;

procedure TWinCECheckListBoxStrings.SetState(AIndex: Integer;
  const AValue: TCheckBoxState);
var
  ItemRecord: PWinCECheckListBoxItemRecord;
begin
  ItemRecord := GetItemRecord(AIndex, True);
  ItemRecord^.State := AValue;
  SetItemRecord(AIndex, ItemRecord);
end;

procedure TWinCECheckListBoxStrings.Clear;
begin
  DeleteItemRecords(FWinCEList);
  inherited Clear;
end;

procedure TWinCECheckListBoxStrings.Delete(Index: Integer);
begin
  DeleteItemRecord(FWinCEList, Index);
  inherited Delete(Index);
end;

procedure TWinCECheckListBoxStrings.Move(CurIndex, NewIndex: Integer);
var
  AState: TCheckBoxState;
begin
  AState := State[CurIndex];
  inherited Move(CurIndex, NewIndex);
  State[NewIndex] := AState;
end;

function TWinCECheckListBoxStrings.GetObject(Index: Integer): TObject;
begin
  Result:= GetItemRecord(Index, false)^.TheObject;
end;

procedure TWinCECheckListBoxStrings.PutObject(Index: Integer; AObject: TObject);
var
  ItemRecord: PWinCECheckListBoxItemRecord;
begin
  ItemRecord := GetItemRecord(Index, true);
  ItemRecord^.TheObject := AObject;
  SetItemRecord(Index, ItemRecord);
end;

class procedure TWinCECheckListBoxStrings.DeleteItemRecords(const List: HWND);
var
  Index: Integer;
  ItemCount: Integer;
begin
  ItemCount := Windows.SendMessage(List, LB_GETCOUNT, 0, 0);
  for Index := 0 to ItemCount-1 do
    DeleteItemRecord(List, Index);
end;

class procedure TWinCECheckListBoxStrings.DeleteItemRecord(const List: HWND;const Index: integer);
var
  ItemRecord: PWinCECheckListBoxItemRecord;
begin
  ItemRecord := PWinCECheckListBoxItemRecord(Windows.SendMessage(List, LB_GETITEMDATA, Index, 0));
  if Assigned(ItemRecord)
    then Dispose(ItemRecord);
end;

{$IFDEF H_PLUS}
  {$UNDEF H_PLUS}
{$ELSE}
  {$H-}
{$ENDIF}




