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
  GetWindowText(A, AStr, GetWindowTextLength(A) + 1);
  GetWindowText(B, BStr, GetWindowTextLength(B) + 1);
  Result := StrComp(AStr, BStr);
end;

{*************************************************************}
{                      TWin32ListStringList methods           }
{*************************************************************}

{------------------------------------------------------------------------------
  Method: TWin32ListStringList.Create
  Params: 
  Returns: 

 ------------------------------------------------------------------------------}
Constructor TWin32ListStringList.Create(List: HWND);
Begin
  Inherited Create;
  If List = HWND(Nil) Then
    Raise Exception.Create('Unspecified list window');
    //Assert(False, 'Trace:Unspecified list window');
  FWin32List := List;
  FSender := TControl(GetProp(FWin32List, 'Lazarus'));
  FOrigHeight := FSender.Height;
 
  //Set proper win32 flags for ComboBox/ListBox
  case FSender.FCompStyle of
    csComboBox:begin
    	       FFlagSort:=CBS_SORT;
               FFlagGetText:=CB_GETLBTEXT;
               FFlagGetTextLen:=CB_GETLBTEXTLEN;
               FFlagGetCount:=CB_GETCOUNT;
               FFlagResetContent:=CB_RESETCONTENT;
               FFlagDeleteString:=CB_DELETESTRING;
               FFlagInsertString:=CB_INSERTSTRING;
    	       end;
    csListBox:begin
    	      FFlagSort:=LBS_SORT;
              FFlagGetText:=LB_GETTEXT;
              FFlagGetTextLen:=LB_GETTEXTLEN;
              FFlagGetCount:=LB_GETCOUNT;
              FFlagResetContent:=LB_RESETCONTENT;
              FFlagDeleteString:=LB_DELETESTRING;
              FFlagInsertString:=LB_INSERTSTRING;
              end;
  else Raise Exception.Create('Win32ListStringList: Component type not detected');
  end;
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
  SetWindowLong(FWin32List, GWL_STYLE, GetWindowLong(FWin32List, GWL_STYLE) Or FFlagSort);
End;

{------------------------------------------------------------------------------
  Method: TWin32ListStringList.Assign
  Params: 
  Returns: 

 ------------------------------------------------------------------------------}
Procedure TWin32ListStringList.Assign(Source: TPersistent);
Var
  Counter: Integer;
Begin
  { Do not call inherited Assign as it does things we do not want to happen }
  If Source Is TStrings Then
  Begin
    Clear;
    For Counter := TStrings(Source).Count - 1 DownTo 0 Do 
      Insert(0, TStrings(Source)[Counter]);
  End
  Else
    inherited Assign(Source);
End;

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
  	Getmem(Item,SendMessage(FWin32List,FFlagGetTextLen,Index,0)+1);
    SendMessage(FWin32List, FFlagGetText, Index, LPARAM(Item));  
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
  Result := SendMessage(FWin32List, FFlagGetCount, 0, 0);
End;

{------------------------------------------------------------------------------
  Method: TWin32ListStringList.Clear
  Params: 
  Returns: 

 ------------------------------------------------------------------------------}
Procedure TWin32ListStringList.Clear;
Begin
  FSender.Height := FOrigHeight;
  SendMessage(FWin32List,FFlagResetContent, 0, 0);
End;  

{------------------------------------------------------------------------------
  Method: TWin32ListStringList.Delete
  Params: 
  Returns: 

 ------------------------------------------------------------------------------}
Procedure TWin32ListStringList.Delete(Index: Integer);
Begin
  If GetCount <> 0 Then
    FSender.Height := (FSender.Height - (FSender.Height Div GetCount));
  SendMessage(FWin32List,FFlagDeleteString, Index, 0);
End;

{------------------------------------------------------------------------------
  Method: TWin32ListStringList.Insert
  Params: 
  Returns: 

 ------------------------------------------------------------------------------}
Procedure TWin32ListStringList.Insert(Index: Integer; Const S: String);
Begin
  If (GetCount <> 0) 
  and (FSender.FCompStyle = csComboBox) Then
    FSender.Height := (FSender.Height + (FSender.Height Div GetCount));
  SendMessage(FWin32List,FFlagInsertString, Index, LPARAM(PChar(S)));
  If FSorted Then
    Sort;
End;

{*************************************************************}
{                      TWin32CListStringList methods            }
{*************************************************************}

{------------------------------------------------------------------------------
  Method: TWin32CListStringList.Create
  Params: 
  Returns: 

 ------------------------------------------------------------------------------}
Constructor TWin32CListStringList.Create(List: HWND);
Begin
  Inherited Create;
  If List = HWND(Nil) Then
    Raise Exception.Create('Unspecified list widget');
  FWin32CList := List;
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
    If Val Then
      Sort
    Else
      SetWindowLong(FWin32CList, GWL_STYLE, GetWindowLong(FWin32CList, GWL_STYLE) And Not LBS_SORT);
  End;
End;

{------------------------------------------------------------------------------
  Method: TWin32CListStringList.Sort
  Params: 
  Returns: 

 ------------------------------------------------------------------------------}
Procedure TWin32CListStringList.Sort;
Begin
  SetWindowLong(FWin32CList, GWL_STYLE, GetWindowLong(FWin32CList, GWL_STYLE) Or LBS_SORT);
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
    For Counter := TStrings(Source).Count - 1 DownTo 0 Do 
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
  SendMessage(FWin32CList, LB_RESETCONTENT, 0, 0);
End;

{------------------------------------------------------------------------------
  Method: TWin32CListStringList.Delete
  Params: 
  Returns: 

 ------------------------------------------------------------------------------}
procedure TWin32CListStringList.Delete(Index: Integer);
begin
  SendMessage(FWin32CList, LB_DELETESTRING, Index, 0);
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
  	Getmem(Item,SendMessage(FWin32CList,LB_GETTEXTLEN,Index,0)+1);
    SendMessage(FWin32CList, LB_GETTEXT, Index, LPARAM(Item));
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
  Result := SendMessage(FWin32CList, LB_GETCOUNT, 0, 0);
End;

{------------------------------------------------------------------------------
  Method: TWin32CListStringList.GetObject
  Params: 
  Returns: 

 ------------------------------------------------------------------------------}
Function TWin32CListStringList.GetObject(Index: Integer): TObject;
Begin
  HWND(Result) := SendMessage(FWin32CList, LB_GETITEMDATA, Index, 0);
End;

{------------------------------------------------------------------------------
  Method: TWin32CListStringList.Insert
  Params: 
  Returns: 

 ------------------------------------------------------------------------------}
Procedure TWin32CListStringList.Insert(Index: Integer; Const S: String);
Begin
  SendMessage(FWin32CList, LB_INSERTSTRING, Index, LPARAM(PChar(S)));
End;

{------------------------------------------------------------------------------
  Method: TWin32CListStringList.PutObject
  Params: 
  Returns: 

 ------------------------------------------------------------------------------}
Procedure TWin32CListStringList.PutObject(Index: Integer; AObject: TObject);
Begin
  SendMessage(FWin32CList, LB_SETITEMDATA, Index, LPARAM(AObject));
End;

{$IFDEF H_PLUS}
  {$UNDEF H_PLUS}
{$ELSE}
  {$H-}
{$ENDIF}

{ =============================================================================

  $Log$
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
  Keith: Fixed TPage creation

  Revision 1.1  2002/01/06 23:09:52  lazarus
  MG: added missing files

}
