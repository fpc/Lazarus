{******************************************************************************
                                  TScreen
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
  Method: TScreen.Create
  Params:  AOwner: the owner of the class
  Returns: Nothing

  Constructor for the class.
 ------------------------------------------------------------------------------}
constructor TScreen.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);
  FFonts := TStringlist.Create;
  TStringlist(FFonts).Sorted := True;
  FFormList := TList.Create;
  FPixelsPerInch:= ScreenInfo.PixelsPerInchX;
  FHintFont := TFont.Create;
//  FHintFont.Name := 'courier';
  FHintFont.Style := [];
  FHintFont.Size := 12;
  FHintFont.Color := clInfoText;
  FHintFont.Pitch := fpDefault;
  FSaveFocusedList := TList.Create;
end;

{------------------------------------------------------------------------------
  Method: TScreen.Destroy
  Params:  None
  Returns: Nothing

  Destructor for the class.
 ------------------------------------------------------------------------------}
Destructor TScreen.Destroy;
begin
  FreeThenNil(FHintFont);
  FreeThenNil(FFormList);
  FreeThenNil(FSaveFocusedList);
  FreeThenNil(FFonts);
  inherited Destroy;
end;

{------------------------------------------------------------------------------
  function TScreen.GetFonts : TStrings;
 ------------------------------------------------------------------------------}
function TScreen.GetFonts : TStrings;
begin
  Result := FFonts;
end;

{------------------------------------------------------------------------------
  procedure TScreen.CreateCursors;
 ------------------------------------------------------------------------------}
procedure TScreen.CreateCursors;
begin

end;

{------------------------------------------------------------------------------
  procedure TScreen.DeleteCursor(Index: Integer);
 ------------------------------------------------------------------------------}
procedure TScreen.DeleteCursor(Index: Integer);
var
  P, Q: PCursorRec;
begin
  P := FCursorList;
  Q := nil;
  while (P <> nil) and (P^.Index <> Index) do begin
    Q := P;
    P := P^.Next;
  end;
  if P <> nil then begin
    writeln('ToDo: TScreen.DeleteCursor');
    //DestroyCursor(P^.Handle);
    if Q = nil then
      FCursorList := P^.Next
    else
      Q^.Next := P^.Next;
    Dispose(P);
  end;
end;

{------------------------------------------------------------------------------
  procedure TScreen.DestroyCursors;
 ------------------------------------------------------------------------------}
procedure TScreen.DestroyCursors;
var
  P, Next: PCursorRec;
  //Hdl: THandle;
begin
  P := FCursorList;
  while P <> nil do begin
    writeln('ToDo: TScreen.DeleteCursor');
    //DestroyCursor(P^.Handle);
    Next := P^.Next;
    Dispose(P);
    P := Next;
  end;
  {Hdl := LoadCursor(0, IDC_ARROW);
  if Hdl <> FDefaultCursor then
    DestroyCursor(FDefaultCursor);}
end;

{------------------------------------------------------------------------------
  function TScreen.GetCursors(Index: Integer): HCURSOR;
 ------------------------------------------------------------------------------}
function TScreen.GetCursors(Index: Integer): HCURSOR;
var
  P: PCursorRec;
begin
  Result := 0;
  if Index <> crNone then
  begin
    P := FCursorList;
    while (P <> nil) and (P^.Index <> Index) do P := P^.Next;
    if P = nil then
      Result := FDefaultCursor
    else
      Result := P^.Handle;
  end;
end;

{------------------------------------------------------------------------------
  Function: TScreen.AddForm
  Params:   FForm: The form to be added
  Returns:  Nothing

  Do not use this procedure. This procedure is used by TScreen internally.
 ------------------------------------------------------------------------------}
procedure TScreen.AddForm(FForm: TCustomForm);
begin
  FFormList.Add(FForm);
end;


{------------------------------------------------------------------------------
  Function: TScreen.GetFormCount
  Params:   none
  Returns:  The count of forms. (TODO: discribe this better; my English is not perfect)

  Returns the count of forms. (TODO: discribe this better; my English is not perfect)
 ------------------------------------------------------------------------------}
Function TScreen.GetFormCount: Integer;
begin
  Result := FFormList.Count;
end;


{------------------------------------------------------------------------------
  Function: TScreen.GetForms
  Params:   IIndex: The index of the form
  Returns:  A form stored in FFormList

  This function is used by the Forms property.
 ------------------------------------------------------------------------------}
Function TScreen.GetForms(IIndex: Integer): TForm;
begin
  Result := TForm(FFormList.Items[IIndex]);
end;


{------------------------------------------------------------------------------
  Method:  TScreen.GetWidth
  Params:  none
  Returns: Screen Width

  returns the screen width
 ------------------------------------------------------------------------------}
Function TScreen.GetWidth : Integer;
begin
  Result := GetSystemMetrics(SM_CXSCREEN);
end;


{------------------------------------------------------------------------------
  Method:  TScreen.GetHeight
  Params:  none
  Returns: Screen Height

  Returns the Screen Height
 ------------------------------------------------------------------------------}
Function TScreen.GetHeight : Integer;
begin
  Result := GetSystemMetrics(SM_CYSCREEN);
end;

{------------------------------------------------------------------------------

  Function: TScreen.RemoveForm
  Params:   FForm: The form to be removed
  Returns:  Nothing

  Do not use this procedure. This procedure is used by TScreen internally.
 ------------------------------------------------------------------------------}
procedure TScreen.RemoveForm(FForm: TCustomForm);
begin
  FFormList.Remove(FForm);
end;

{------------------------------------------------------------------------------
  procedure TScreen.SetCursor(const AValue: TCursor);
 ------------------------------------------------------------------------------}
procedure TScreen.SetCursor(const AValue: TCursor);
//var
  //MousePos: TPoint;
  //Handle: HWND;
  //Code: Longint;
begin
  if AValue <> Cursor then begin
    FCursor := AValue;
    {if AValue = crDefault then begin
      // Reset the cursor to the default by sending a WM_SETCURSOR to the
      //  window under the cursor
      GetCursorPos(MousePos);
      Handle := WindowFromPoint(MousePos);
      if (Handle <> 0) and
        (GetWindowThreadProcessId(Handle, nil) = GetCurrentThreadId) then
      begin
        Code := SendMessage(Handle, WM_NCHITTEST, 0, LongInt(PointToSmallPoint(P)));
        SendMessage(Handle, WM_SETCURSOR, Handle, MakeLong(Code, WM_MOUSEMOVE));
        Exit;
      end;
    end;}
    LCLLinux.SetCursor(Cursors[FCursor]);
  end;
  Inc(FCursorCount);
end;

{------------------------------------------------------------------------------
  procedure TScreen.SetCursors(Index: Integer; const AValue: HCURSOR);
 ------------------------------------------------------------------------------}
procedure TScreen.SetCursors(Index: Integer; const AValue: HCURSOR);
begin
  {if Index = crDefault then
    if Handle = 0 then
      FDefaultCursor := LoadCursor(0, IDC_ARROW)
    else
      FDefaultCursor := Handle
  else if Index <> crNone then
  begin
    DeleteCursor(Index);
    if Handle <> 0 then InsertCursor(Index, Handle);
  end;}
end;

{------------------------------------------------------------------------------
  Function: TScreen.MyFunction
  Params:   AOwner: the owner of the class
  Returns:  String containing output from the function.

  Description of the function for the class.
 ------------------------------------------------------------------------------}
{function TScreen.MyFunction(AOwner: TComponent): String;
begin

end;}
