// included by win32int.pp

{******************************************************************************
                                 win32listsl.inc
                 TWin32ListStringList and TWin32CListStringList

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

{$IFOPT H+}
  {$DEFINE H_PLUS}
{$ELSE}
  {$H+}
  {$UNDEF H_PLUS}
{$ENDIF}

{*************************************************************}
{                      Default compare function               }
{*************************************************************}

Function DefaultCompareFunc(A, B: HWND): Integer; CDecl;
Var
  AStr, BStr: PChar;
Begin
  AStr:=nil;
  BStr:=nil;
  GetWindowText(A, AStr, GetWindowTextLength(A) + 1);
  GetWindowText(B, BStr, GetWindowTextLength(B) + 1);
  Result := StrComp(AStr, BStr);
end;

Procedure SetComboHeight(Sender: TWinControl;
  ALeft, ATop, AWidth, AHeight:Integer);
begin
  LCLBoundsToWin32Bounds(Sender,ALeft, ATop, AWidth, AHeight);
  MoveWindow(Sender.Handle,ALeft,ATop,AWidth,AHeight,true);
  LCLControlSizeNeedsUpdate(Sender,true);
end;

{*************************************************************}
{                      TWin32ListStringList methods           }
{*************************************************************}

{------------------------------------------------------------------------------
  Method: TWin32ListStringList.Create
  Params:
  Returns:

 ------------------------------------------------------------------------------}
Constructor TWin32ListStringList.Create(List : HWND; TheOwner: TWinControl);
Var
  R:TRect;
Begin
  Inherited Create;
  If List = HWND(Nil) Then
    Raise Exception.Create('Unspecified list window');
    //Assert(False, 'Trace:Unspecified list window');
  FWin32List := List;
  FSender := TheOwner;

  //Set proper win32 flags for ComboBox/ListBox and get/set Combo Height
  case FSender.FCompStyle of
    csComboBox:
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
      //Get edit and item sizes
      Windows.GetClientRect(FWin32List,@R);
      FEditHeight := R.Bottom;
      FItemHeight := Windows.SendMessage(FWin32List, CB_GETITEMHEIGHT, 0, 0);
      FDDownCount := TComboBox(FSender).DropDownCount;
      If FDDownCount = 0 then
        FDDownCount := 8;
    end;
    csListBox,csCheckListBox:
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
    end;
  else
    raise Exception.Create('Win32ListStringList: Component type not detected');
  end;
  // Determine if the list is sorted
  FSorted := (UINT(GetWindowLong(FWin32List, GWL_STYLE)) and FFlagSort <> 0);
End;

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
  CNSendMessage(LM_RECREATEWND,FSender,Nil);
End;

{------------------------------------------------------------------------------
  Method: TWin32ListStringList.Assign
  Params:
  Returns:

 ------------------------------------------------------------------------------}
Procedure TWin32ListStringList.Assign(Source: TPersistent);
Var
  Counter: Integer;
  EditText: String;
Begin
  { Do not call inherited Assign as it does things we do not want to happen }
  If Source Is TStrings Then
  Begin
    // save any text in edit box
    if FSender.FCompStyle=csComboBox then
      TWin32WSComboBox.GetText(FSender, EditText);

    Windows.SendMessage(FWin32List, FFlagResetContent, 0, 0);
    For Counter := 0 To (TStrings(Source).Count - 1) Do
    Begin
      Windows.SendMessage(FWin32List, FFlagAddString, 0, LPARAM(PChar(TStrings(Source)[Counter]))); //Insert
      PutObject(Counter, TStrings(Source).Objects[Counter]);
    end;

    if FSender.FCompStyle=csComboBox Then
    begin
      SetComboHeight(FSender, FSender.Left, FSender.Top, FSender.Width, ComboHeight);
      // restore text in edit box
      TWin32WSCustomComboBox.SetText(FSender, EditText);
    end;
  End
  Else
    inherited Assign(Source);
End;

function TWin32ListStringList.GetComboHeight: integer;
begin
  if Count = 0 then
  begin
    Result := FEditHeight + FItemHeight + 2;
  end else begin
    Result := FEditHeight + FDDownCount*FItemHeight + 2;
  end;
end;

{------------------------------------------------------------------------------
  Method: TWin32ListStringList.Get
  Params:
  Returns:

 ------------------------------------------------------------------------------}
Function TWin32ListStringList.Get(Index: Integer): String;
Var
  Item: PChar;
Begin
  If (Index < 0) Or (Index >= Count) Then
    Raise Exception.Create('Out of bounds.')
  Else
  Begin
    Getmem(Item,Windows.SendMessage(FWin32List,FFlagGetTextLen,Index,0)+1);
    Windows.SendMessage(FWin32List, FFlagGetText, Index, LPARAM(Item));
  End;
  Result := StrPas(Item);
  Dispose(Item);
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
  if FSender.FCompStyle=csComboBox Then
    SetComboHeight(FSender,FSender.Left,FSender.Top,FSender.Width,
                   FEditHeight + FItemHeight + 2);
  Windows.SendMessage(FWin32List,FFlagResetContent, 0, 0);
End;

{------------------------------------------------------------------------------
  Method: TWin32ListStringList.Delete
  Params:
  Returns:

 ------------------------------------------------------------------------------}
Procedure TWin32ListStringList.Delete(Index: Integer);
Begin
  If (FSender.FCompStyle = csComboBox)
  and (GetCount <= 1) Then
    SetComboHeight(FSender,FSender.Left,FSender.Top,FSender.Width,
                   FEditHeight + FItemHeight + 2);
  Windows.SendMessage(FWin32List,FFlagDeleteString, Index, 0);
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
Procedure TWin32ListStringList.Insert(Index: Integer; Const S: String);
Begin
  If (FSender.FCompStyle = csComboBox)
  and (GetCount = 0) Then
    SetComboHeight(FSender,FSender.Left,FSender.Top,FSender.Width,
                   FEditHeight + FDDownCount*FItemHeight + 2);
  If FSorted Then
    Windows.SendMessage(FWin32List,FFlagAddString, 0, LPARAM(PChar(S)))
  Else
    Windows.SendMessage(FWin32List,FFlagInsertString, Index, LPARAM(PChar(S)));
End;

{------------------------------------------------------------------------------
  Method: TWin32ListStringList.PutObject
  Params:
  Returns:

 ------------------------------------------------------------------------------}
Procedure TWin32ListStringList.PutObject(Index: Integer; AObject: TObject);
Begin
  Windows.SendMessage(FWin32List, FFlagSetItemData, Index, LPARAM(AObject));
End;

{ TWin32CheckListBoxStrings }

constructor TWin32CheckListBoxStrings.Create(List : HWND; TheOwner: TWinControl);
begin
  inherited Create(List, TheOwner);
  with FDefaultItem do begin
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
  if (not Assigned(Result)) then begin
    if CreateNew then begin
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
  CNSendMessage(LM_RECREATEWND,FSender,Nil);
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

{ =============================================================================

  $Log$
  Revision 1.32  2004/09/10 14:38:29  micha
  convert lm_gettext to new interface methods
  remove lm_settext replacement settext methods in twidgetsets

  Revision 1.31  2004/09/10 09:43:13  micha
  convert LM_SETLABEL message to interface methods

  Revision 1.30  2004/05/08 12:32:32  micha
  fix combobox height; whatever the lcl passes as height for combobox, always calculate our own

  Revision 1.29  2004/02/23 08:19:04  micha
  revert intf split

  Revision 1.27  2003/12/29 14:22:22  micha
  fix a lot of range check errors win32

  Revision 1.26  2003/12/26 09:28:05  micha
  fix itemdata (string+object) for combobox (from vincent)

  Revision 1.25  2003/12/21 11:50:39  micha
  fix AV when retrieving object (from vincent)

  Revision 1.24  2003/12/15 22:04:05  micha
  nil check

  Revision 1.23  2003/12/15 21:57:16  micha
  checklistbox, implement object+checked; from vincent

  Revision 1.22  2003/11/25 21:20:38  micha
  implement tchecklistbox

  Revision 1.21  2003/11/18 07:20:40  micha
  added "included by" notice at top of file

  Revision 1.20  2003/10/02 11:18:09  mattias
  clean ups from Karl

  Revision 1.19  2003/09/20 13:27:49  mattias
  varois improvements for ParentColor from Micha

  Revision 1.18  2003/08/28 09:10:01  mattias
  listbox and comboboxes now set sort and selection at handle creation

  Revision 1.17  2003/08/27 09:33:26  mattias
  implements SET_LABEL from Micha

  Revision 1.16  2003/08/26 08:12:33  mattias
  applied listbox/combobox patch from Karl

  Revision 1.15  2003/08/23 21:17:09  mattias
  several fixes for the win32 intf, added pending OnResize events

  Revision 1.14  2003/08/23 11:30:51  mattias
  fixed SetComboHeight in win32 intf and finddeclaration of overloaded proc definition

  Revision 1.13  2003/08/13 21:23:10  mattias
  fixed log

  Revision 1.12  2003/08/13 16:26:07  mattias
  fixed combobox height from Karl

  Revision 1.10  2003/07/28 06:42:42  mattias
  removed debuggging SetName, Patch from Karl Brandt

  Revision 1.9  2003/06/28 16:20:19  mattias
  fixed some win32 intf warnings

  Revision 1.8  2003/01/01 10:46:59  mattias
  fixes for win32 listbox/combobox from Karl Brandt

  Revision 1.7  2002/11/15 23:43:54  mattias
  applied patch from Karl Brandt

  Revision 1.6  2002/05/10 07:43:48  lazarus
  MG: updated licenses

  Revision 1.5  2002/04/03 01:52:42  lazarus
  Keith: Removed obsolete code, in preperation of a pending TWin32Object cleanup

  Revision 1.4  2002/02/04 10:54:33  lazarus
  Keith:
    * Fixes for Win32
    * Added new listviewtest.pp example

  Revision 1.3  2002/02/01 10:13:09  lazarus
  Keith: Fixes for Win32

  Revision 1.2  2002/01/17 03:17:44  lazarus
  Keith: Fixed TCustomPage creation

  Revision 1.1  2002/01/06 23:09:52  lazarus
  MG: added missing files

}
