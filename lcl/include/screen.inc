{%MainUnit ../forms.pp}

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
  FCustomForms:=TList.Create;
  FCustomFormsZOrdered:=TList.Create;
  FFormList := TList.Create;
  FPixelsPerInch:= ScreenInfo.PixelsPerInchX;
  FHintFont := TFont.Create;
  FSaveFocusedList := TList.Create;
end;

{------------------------------------------------------------------------------
  Method: TScreen.Destroy
  Params:  None
  Returns: Nothing

  Destructor for the class.
 ------------------------------------------------------------------------------}
Destructor TScreen.Destroy;
var
  HandlerType: TScreenNotification;
begin
  for HandlerType:=Low(FHandlers) to High(FHandlers) do
    FreeThenNil(FHandlers[HandlerType]);
  FreeThenNil(FHintFont);
  FreeThenNil(FFormList);
  FreeThenNil(FCustomForms);
  FreeThenNil(FCustomFormsZOrdered);
  FreeThenNil(FSaveFocusedList);
  FreeThenNil(FFonts);
  inherited Destroy;
end;

{------------------------------------------------------------------------------
  function TScreen.CustomFormIndex(AForm: TCustomForm): integer;
 ------------------------------------------------------------------------------}
function TScreen.CustomFormIndex(AForm: TCustomForm): integer;
begin
  Result:=FCustomForms.Count-1;
  while (Result>=0) and (CustomForms[Result]<>AForm) do dec(Result);
end;

{------------------------------------------------------------------------------
  function TScreen.FormIndex(AForm: TForm): integer;
 ------------------------------------------------------------------------------}
function TScreen.FormIndex(AForm: TForm): integer;
begin
  Result:=FFormList.Count-1;
  while (Result>=0) and (Forms[Result]<>AForm) do dec(Result);
end;

{------------------------------------------------------------------------------
  function TScreen.CustomFormZIndex(AForm: TCustomForm): integer;
 ------------------------------------------------------------------------------}
function TScreen.CustomFormZIndex(AForm: TCustomForm): integer;
begin
  Result:=FCustomFormsZOrdered.Count-1;
  while (Result>=0) and (CustomFormsZOrdered[Result]<>AForm) do dec(Result);
end;

procedure TScreen.MoveFormToFocusFront(ACustomForm: TCustomForm);
begin
  if (Self=nil) or (ACustomForm=nil)
  or (csDestroying in ACustomForm.ComponentState) then
    RaiseGDBException('TScreen.MoveFormToFocusFront');
  FCustomForms.Remove(ACustomForm);
  FCustomForms.Insert(0, ACustomForm);
  if ACustomForm is TForm then
  begin
    Screen.FFormList.Remove(ACustomForm);
    Screen.FFormList.Insert(0, ACustomForm);
  end;
end;

procedure TScreen.MoveFormToZFront(ACustomForm: TCustomForm);
begin
  FCustomFormsZOrdered.Remove(ACustomForm);
  FCustomFormsZOrdered.Insert(0, ACustomForm);
end;

procedure TScreen.UpdateScreen;
var
  DC: HDC;
begin
  DC:=GetDC(0);
  FPixelsPerInch:=GetDeviceCaps(DC,LOGPIXELSX);
  ReleaseDC(0,DC);
end;

procedure TScreen.AddHandlerFormAdded(OnFormAdded: TScreenFormEvent);
begin
  AddHandler(snFormAdded,TMethod(OnFormAdded));
end;

procedure TScreen.RemoveHandlerFormAdded(OnFormAdded: TScreenFormEvent);
begin
  RemoveHandler(snFormAdded,TMethod(OnFormAdded));
end;

procedure TScreen.AddHandlerRemoveForm(OnRemoveForm: TScreenFormEvent);
begin
  AddHandler(snRemoveForm,TMethod(OnRemoveForm));
end;

procedure TScreen.RemoveHandlerRemoveForm(OnRemoveForm: TScreenFormEvent);
begin
  RemoveHandler(snRemoveForm,TMethod(OnRemoveForm));
end;

procedure TScreen.AddHandlerActiveControlChanged(
  OnActiveControlChanged: TScreenControlEvent);
begin
  AddHandler(snActiveControlChanged,TMethod(OnActiveControlChanged));
end;

procedure TScreen.RemoveHandlerActiveControlChanged(
  OnActiveControlChanged: TScreenControlEvent);
begin
  RemoveHandler(snActiveControlChanged,TMethod(OnActiveControlChanged));
end;

procedure TScreen.AddHandlerActiveFormChanged(
  OnActiveFormChanged: TScreenActiveFormChangedEvent);
begin
  AddHandler(snActiveFormChanged,TMethod(OnActiveFormChanged));
end;

procedure TScreen.RemoveHandlerActiveFormChanged(
  OnActiveFormChanged: TScreenActiveFormChangedEvent);
begin
  RemoveHandler(snActiveFormChanged,TMethod(OnActiveFormChanged));
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
  function TScreen.GetCustomFormCount: Integer;
 ------------------------------------------------------------------------------}
function TScreen.GetCustomFormCount: Integer;
begin
  Result:=FCustomForms.Count;
end;

{------------------------------------------------------------------------------
  function TScreen.GetCustomForms(Index: Integer): TCustomForm;
 ------------------------------------------------------------------------------}
function TScreen.GetCustomForms(Index: Integer): TCustomForm;
begin
  Result := TCustomForm(FCustomForms[Index]);
end;

{------------------------------------------------------------------------------
  function TScreen.GetCustomFormsZOrdered(Index: Integer): TCustomForm;
 ------------------------------------------------------------------------------}
function TScreen.GetCustomFormsZOrdered(Index: Integer): TCustomForm;
begin
  Result := TCustomForm(FCustomFormsZOrdered[Index]);
end;

{------------------------------------------------------------------------------
  Function: TScreen.AddForm
  Params:   FForm: The form to be added
  Returns:  Nothing

  Do not use this procedure. This procedure is used by TScreen internally.
 ------------------------------------------------------------------------------}
procedure TScreen.AddForm(AForm: TCustomForm);
var
  i: Integer;
begin
  FCustomForms.Add(AForm);
  FCustomFormsZOrdered.Add(AForm);
  if AForm is TForm then
  begin
    FFormList.Add(AForm);
    Application.UpdateVisible;
  end;
  i:=GetHandlerCount(snFormAdded);
  while GetNextHandlerIndex(snFormAdded,i) do
    TScreenFormEvent(FHandlers[snFormAdded][i])(Self,AForm);
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

Function TScreen.GetHintFont: TFont;
begin
  if not InterfaceObject.InitHintFont(FHintFont) then
  begin
//    FHintFont.Name := 'courier';
    FHintFont.Style := [];
    FHintFont.Size := 12;
    FHintFont.Color := clInfoText;
    FHintFont.Pitch := fpDefault;
  end;
  Result := FHintFont;
end;

{------------------------------------------------------------------------------

  Function: TScreen.RemoveForm
  Params:   FForm: The form to be removed
  Returns:  Nothing

 ------------------------------------------------------------------------------}
procedure TScreen.RemoveForm(AForm: TCustomForm);
var
  i: Integer;
begin
  i:=GetHandlerCount(snRemoveForm);
  while GetNextHandlerIndex(snRemoveForm,i) do
    TScreenFormEvent(FHandlers[snRemoveForm][i])(Self,AForm);
  FCustomForms.Remove(AForm);
  FCustomFormsZOrdered.Remove(AForm);
  FFormList.Remove(AForm);
  Application.UpdateVisible;
  //if (FCustomForms.Count = 0) and (Application.FHintWindow <> nil) then
  //  Application.FHintWindow.ReleaseHandle;
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
    LCLIntf.SetCursor(Cursors[FCursor]);
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
  procedure TScreen.UpdateLastActive;
 ------------------------------------------------------------------------------}
procedure TScreen.UpdateLastActive;

  procedure NotifyOnActiveFormChanged;
  var
    i: Integer;
    Handler: TScreenFormEvent;
  begin
    if Assigned(FOnActiveFormChange) then FOnActiveFormChange(Self);
    i:=GetHandlerCount(snActiveFormChanged);
    while GetNextHandlerIndex(snActiveFormChanged,i) do begin
      Handler:=TScreenFormEvent(FHandlers[snActiveFormChanged][i]);
      Handler(Self,FLastActiveCustomForm);
    end;
  end;

  procedure NotifyOnActiveControlChanged;
  var
    i: Integer;
    Handler: TScreenControlEvent;
  begin
    if Assigned(FOnActiveControlChange) then FOnActiveControlChange(Self);
    i:=GetHandlerCount(snActiveControlChanged);
    while GetNextHandlerIndex(snActiveControlChanged,i) do begin
      Handler:=TScreenControlEvent(FHandlers[snActiveControlChanged][i]);
      Handler(Self,FLastActiveControl);
    end;
  end;

begin
  if FLastActiveCustomForm <> FActiveCustomForm then
  begin
    FLastActiveCustomForm := FActiveCustomForm;
    NotifyOnActiveFormChanged;
  end;
  if FLastActiveControl <> FActiveControl then
  begin
    FLastActiveControl := FActiveControl;
    NotifyOnActiveControlChanged;
  end;
end;

{------------------------------------------------------------------------------
  procedure TScreen.AddHandler(HandlerType: TScreenNotification;
    const Handler: TMethod);
 ------------------------------------------------------------------------------}
procedure TScreen.AddHandler(HandlerType: TScreenNotification;
  const Handler: TMethod);
begin
  if Handler.Code=nil then RaiseGDBException('TScreen.AddHandler');
  if FHandlers[HandlerType]=nil then
    FHandlers[HandlerType]:=TMethodList.Create;
  FHandlers[HandlerType].Add(Handler);
end;

procedure TScreen.RemoveHandler(HandlerType: TScreenNotification;
  const Handler: TMethod);
begin
  FHandlers[HandlerType].Remove(Handler);
end;

function TScreen.GetHandlerCount(HandlerType: TScreenNotification): integer;
begin
  Result:=FHandlers[HandlerType].Count;
end;

function TScreen.GetNextHandlerIndex(HandlerType: TScreenNotification;
  var i: integer): boolean;
begin
  Result:=FHandlers[HandlerType].NextDownIndex(i);
end;

// included by forms.pp

