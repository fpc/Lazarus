{******************************************************************************
                                   TApplication
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


const
  NoInterfaceObject =
    'No interface object. '
    +'Plz check if "interfaces" was added to the programs uses section.';

  DefHintColor = clInfoBk;  { default hint window color }
  DefHintPause = 500;       { default pause before hint window displays (ms) }
  DefHintShortPause = 0;    { default reshow pause }
  DefHintHidePause = DefHintPause * 5; { default pause before hint is hidden }

function GetHintControl(Control: TControl): TControl;
begin
  Result := Control;
  while (Result <> nil) and (not Result.ShowHint) do
    Result := Result.Parent;
  if (Result <> nil)
  and ([csDesigning,csDestroying,csLoading]*Result.ComponentState<>[]) then
    Result := nil;
end;

function GetHintInfoAtMouse: THintInfoAtMouse;
begin
  if Mouse<>nil then begin
    Result.MousePos:=Mouse.CursorPos;
    Result.Control:=GetHintControl(FindLCLControl(Result.MousePos));
    Result.ControlHasHint:=
      (Result.Control<>nil)
      and (Application<>nil) and (Application.ShowHint)
      and (GetCapture=0)
      and (GetKeyState(VK_LBUTTON)=0)
      and (GetKeyState(VK_MBUTTON)=0)
      and (GetKeyState(VK_RBUTTON)=0);
    if Result.ControlHasHint then begin
      // if there is a modal form, then don't show hints for other forms
      if (Screen.FFocusedForm<>nil)
      and (fsModal in Screen.FFocusedForm.FormState)
      and (GetParentForm(Result.Control)<>Screen.FFocusedForm)
      then
        Result.ControlHasHint:=false;
    end;
  end else begin
    Result.MousePos:=Point(0,0);
    Result.Control:=nil;
    Result.ControlHasHint:=false;
  end;
end;

{------------------------------------------------------------------------------}
{       TApplication Constructor                                               }
{------------------------------------------------------------------------------}
constructor TApplication.Create(AOwner: TComponent);
begin
  Focusmessages := True;
  LCLProc.SendApplicationMessageFunction:=@SendApplicationMsg;

  FTerminate := False;
  FMainForm := nil;
  FMouseControl := nil;
  FHandle := 0;
  FHintColor := DefHintColor;
  FHintPause := DefHintPause;
  FHintShortCuts := True;
  FHintShortPause := DefHintShortPause;
  FHintHidePause := DefHintHidePause;
  FShowHint := true;
  FList := nil;
  FOnIdle := nil;
  FIcon := nil;
  ApplicationActionComponent:=Self;

  inherited Create(AOwner);
  // MG: if you prefer message boxes instead of terminal error messages uncomment
  // the following line
  //ExceptProc := @ExceptionOccurred;
end;

{------------------------------------------------------------------------------}
{       TApplication Destructor                                                }
{------------------------------------------------------------------------------}
destructor TApplication.Destroy;
begin
  CancelHint;
  ShowHint := False;
  ApplicationActionComponent:=nil;
  FreeThenNil(FIcon);
  FreeThenNil(FList);
  FreeThenNil(FOnIdleHandler);
  FreeThenNil(FOnIdleEndHandler);
  FreeThenNil(FOnUserInputHandler);
  inherited Destroy;
  LCLProc.SendApplicationMessageFunction:=nil;
end;

{------------------------------------------------------------------------------
       TApplication BringToFront
------------------------------------------------------------------------------}
procedure TApplication.BringToFront;
begin
  CNSendMessage(LM_BRINGTOFRONT,Self,nil);
end;


{------------------------------------------------------------------------------}
{       TApplication Messagebox     }
{------------------------------------------------------------------------------}
function TApplication.MessageBox(Text, Caption : PChar; Flags : Longint) : Integer;
begin
  if Assigned(MessageBoxFunction) then
    Result:=MessageBoxFunction(Text,Caption,Flags)
  else begin
    writeln('WARNING: TApplication.MessageBox: no MessageBoxFunction');
    writeln('  Caption="',Caption,'"');
    writeln('  Text="',Text,'"');
    writeln('  Flags=',HexStr(Cardinal(Flags),8));
    Result:=0;
  end;
end;

{------------------------------------------------------------------------------
       TApplication GetExename
------------------------------------------------------------------------------}
Function TApplication.GetEXEName: String;
Begin
  Result := ParamStr(0);
end;

{------------------------------------------------------------------------------
       TApplication Notification  "Performs Application Level Operations"
------------------------------------------------------------------------------}
procedure TApplication.Notification(AComponent : TComponent;
  Operation : TOperation);
begin
  if Operation = opRemove then begin
    if AComponent=FMouseControl then FMouseControl:=nil;
    if AComponent = MainForm then begin
      FMainForm:= nil;
      Terminate;
    end;
  end;
end;

{------------------------------------------------------------------------------
  Method: TApplication.ControlDestroyed
  Params: None
  Returns:  Nothing

  
 ------------------------------------------------------------------------------}
procedure TApplication.ControlDestroyed(AControl: TControl);
begin
  if AControl=FMouseControl then FMouseControl:=nil;
  if AControl = MainForm then FMainForm:= nil;
  if Screen.FActiveControl = AControl then Screen.FActiveControl := nil;
  if Screen.FActiveCustomForm = AControl then
  begin
    Screen.FActiveCustomForm := nil;
    Screen.FActiveForm := nil;
  end;
  if Screen.FFocusedForm = AControl then Screen.FFocusedForm := nil;
  if FHintControl = AControl then FHintControl:=nil;
  Screen.UpdateLastActive;
end;

{------------------------------------------------------------------------------
  TApplication ProcesssMessages  "Enter the messageloop and process until empty"
------------------------------------------------------------------------------}
procedure TApplication.ProcessMessages;
begin
  InterfaceObject.HandleEvents;
end;

{------------------------------------------------------------------------------
  TApplication HintMouseMEssage
------------------------------------------------------------------------------}
procedure TApplication.HintMouseMessage(Control : TControl;
  var AMessage : TLMessage);
begin
  // ToDo
end;


{------------------------------------------------------------------------------
       TApplication Initialize
       Makes a call to the coponent engine to provide any initialization that
       needs to occur.
------------------------------------------------------------------------------}
procedure TApplication.Initialize;
begin
  if (InterfaceObject=nil)
  or (AnsiCompareText(InterfaceObject.Classname,'TINTERFACEBASE')=0) then begin
    writeln('ERROR: ',NoInterfaceObject);
    raise Exception.Create(NoInterfaceObject);
  end;
  InterfaceObject.AppInit;
  CNSendMessage(LM_SCREENINIT, nil, @ScreenInfo);
  if LazarusResources.Find('MAINICON')<>nil then begin
    if FIcon=nil then begin
      FIcon:=TIcon.Create;
      FIcon.OnChange := @IconChanged;
    end;
    FIcon.LoadFromLazarusResource('MAINICON');
  end;
end;

{------------------------------------------------------------------------------
  Method: TApplication.MouseIdle
  Params: None
  Returns:  Nothing

  Handles mouse Idle  
 ------------------------------------------------------------------------------}
procedure TApplication.MouseIdle(const CurrentControl: TControl);
var
  CaptureControl: TControl;
  IsOther: Boolean;
begin
  CaptureControl := GetCaptureControl;
  if FMouseControl <> CurrentControl then
  begin
    IsOther:=((FMouseControl <> nil) and (CaptureControl = nil)) or
      ((CaptureControl <> nil) and (FMouseControl = CaptureControl));

    if IsOther and (FMouseControl<>nil) then
      FMouseControl.Perform(CM_MOUSELEAVE, 0, 0);
    FMouseControl := CurrentControl;
    if IsOther and (FMouseControl<>nil) then
      FMouseControl.Perform(CM_MOUSEENTER, 0, 0);
  end;
end;

{------------------------------------------------------------------------------
  Method: TApplication.Idle
  Params: None
  Returns:  Nothing

  Invoked when the application enters the idle state 
 ------------------------------------------------------------------------------}
procedure TApplication.Idle;
var
  P: TPoint;
  Done: Boolean;
  CurrentControl: TControl;
begin
  GetCursorPos(P);
  CurrentControl := FindDragTarget(P, True);
  if (CurrentControl <> nil) 
  and (csDesigning in CurrentControl.ComponentState) 
  then CurrentControl := nil;

  MouseIdle(CurrentControl);
  
  Done := True;
  if Assigned(FOnIdle) then FOnIdle(Self, Done);
  NotifyIdleHandler;
  if Done then begin
    // wait till something happens
    Include(FFlag,AppWaiting);
    Exclude(FFlag,AppIdleEndSent);
    InterfaceObject.WaitMessage;
    DoOnIdleEnd;
    Exclude(FFlag,AppWaiting);
  end;
end;

{------------------------------------------------------------------------------
  Method: TApplication.SetIcon
  Params: the new icon
 ------------------------------------------------------------------------------}
procedure TApplication.SetIcon(AValue: TIcon);
begin
  if FIcon=nil then begin
    FIcon:=TIcon.Create;
    FIcon.OnChange := @IconChanged;
  end;
  FIcon.Assign(AValue);
end;

{------------------------------------------------------------------------------
  procedure TApplication.SetShowHint(const AValue: Boolean);
 ------------------------------------------------------------------------------}
procedure TApplication.SetShowHint(const AValue: Boolean);
begin
  if FShowHint=AValue then exit;
  FShowHint:=AValue;
  if FShowHint then
  begin
    //
  end else
  begin
    FreeThenNil(FHintWindow);
  end;
end;

{------------------------------------------------------------------------------
  procedure TApplication.StopHintTimer;
 ------------------------------------------------------------------------------}
procedure TApplication.StopHintTimer;
begin
  if FHintTimer<>nil then
    FHintTimer.Enabled:=false;
  FHintTimerType:=ahtNone;
end;

{------------------------------------------------------------------------------
  procedure TApplication.NotifyIdleHandler;
 ------------------------------------------------------------------------------}
procedure TApplication.NotifyIdleHandler;
var
  i: integer;
begin
  if FOnIdleHandler=nil then exit;
  i:=FOnIdleHandler.Count-1;
  while (i>=0) and (FOnIdleHandler<>nil) do begin
    TNotifyEvent(FOnIdleHandler[i])(Self);
    dec(i);
    if (FOnIdleHandler<>nil) and (i>FOnIdleHandler.Count-1) then
      i:=FOnIdleHandler.Count-1;
  end;
end;

{------------------------------------------------------------------------------
  procedure TApplication.NotifyIdleEndHandler;

 ------------------------------------------------------------------------------}
procedure TApplication.NotifyIdleEndHandler;
var
  i: integer;
begin
  if FOnIdleEndHandler=nil then exit;
  i:=FOnIdleHandler.Count-1;
  while (i>=0) and (FOnIdleEndHandler<>nil) do begin
    TNotifyEvent(FOnIdleEndHandler[i])(Self);
    dec(i);
    if (FOnIdleEndHandler<>nil) and (i>FOnIdleEndHandler.Count-1) then
      i:=FOnIdleEndHandler.Count-1;
  end;
end;

{------------------------------------------------------------------------------
  function TApplication.IsHintMsg(var Msg: TMsg): Boolean;

 ------------------------------------------------------------------------------}
function TApplication.IsHintMsg(var Msg: TMsg): Boolean;
begin
  Result := False;
  {if (FHintWindow <> nil) and FHintWindow.IsHintMsg(Msg) then
    CancelHint;}
end;

{------------------------------------------------------------------------------
  procedure TApplication.DoOnMouseMove;

 ------------------------------------------------------------------------------}
procedure TApplication.DoOnMouseMove;
var
  Info: THintInfoAtMouse;
begin
  Info:=GetHintInfoAtMouse;
  //writeln('TApplication.DoOnMouseMove Info.ControlHasHint=',Info.ControlHasHint,' Type=',ord(FHintTimerType));
  if Info.ControlHasHint then begin
    case FHintTimerType of
    ahtNone,ahtShowHint:
      StartHintTimer(HintPause,ahtShowHint);
    ahtHideHint:
      ShowHintWindow(Info);
    else
      HideHint;
    end;
  end else begin
    HideHint;
  end;
end;

{------------------------------------------------------------------------------
  procedure TApplication.ShowHintWindow(const Info: THintInfoAtMouse);
 ------------------------------------------------------------------------------}
procedure TApplication.ShowHintWindow(const Info: THintInfoAtMouse);

  function GetCursorHeightMargin: integer;
  begin
    Result:=25;
  end;

var
  ClientOrigin, ParentOrigin: TPoint;
  HintInfo: THintInfo;
  CanShow: Boolean;
  HintWinRect: TRect;
  CurHeight: Integer;
begin
  if not FShowHint then exit;
  FHintControl:=Info.Control;

  CurHeight:=GetCursorHeightMargin;
  HintInfo.HintControl := FHintControl;
  HintInfo.HintPos := Info.MousePos;
  
  // to reduce flicker
  HintInfo.HintPos.X:=HintInfo.HintPos.X and (not $F);
  HintInfo.HintPos.Y:=HintInfo.HintPos.Y and (not $F);
  
  Inc(HintInfo.HintPos.Y, CurHeight);
  HintInfo.HintMaxWidth := Screen.Width;
  HintInfo.HintColor := FHintColor;
  HintInfo.CursorRect := FHintControl.BoundsRect;
  ClientOrigin := FHintControl.ClientOrigin;
  ParentOrigin.X := 0;
  ParentOrigin.Y := 0;
  if FHintControl.Parent <> nil then
    ParentOrigin := FHintControl.Parent.ClientOrigin;
  {else if (FHintControl is TWinControl) and
    (TWinControl(FHintControl).ParentWindow <> 0) then
    Windows.ClientToScreen(TWinControl(FHintControl).ParentWindow, ParentOrigin);}
  OffsetRect(HintInfo.CursorRect, ParentOrigin.X - ClientOrigin.X,
    ParentOrigin.Y - ClientOrigin.Y);
  HintInfo.CursorPos := FHintControl.ScreenToClient(Info.MousePos);
  HintInfo.HintStr := GetShortHint(Info.Control.Hint);
  HintInfo.ReshowTimeout := 0;
  HintInfo.HideTimeout := FHintHidePause;
  HintInfo.HintWindowClass := HintWindowClass;
  HintInfo.HintData := nil;
  CanShow := FHintControl.Perform(CM_HINTSHOW, 0, Longint(@HintInfo)) = 0;
  if CanShow and Assigned(FOnShowHint) then
    FOnShowHint(HintInfo.HintStr, CanShow, HintInfo);
  if CanShow and (FHintControl <> nil) and (HintInfo.HintStr <> '') then
  begin
    // create hint window
    if (FHintWindow<>nil) and (FHintWindow.ClassType<>HintInfo.HintWindowClass)
    then
      FreeThenNil(FHintWindow);
    if FHintWindow=nil then begin
      FHintWindow:=HintInfo.HintWindowClass.Create(Self);
      with FHintWindow do begin
        Visible := False;
        Caption := '';
        AutoHide := False;
      end;
    end;
    
    { make the hint have the same BiDiMode as the activating control }
    //FHintWindow.BiDiMode := FHintControl.BiDiMode;
    { calculate the width of the hint based on HintStr and MaxWidth }
    with HintInfo do
      HintWinRect := FHintWindow.CalcHintRect(HintMaxWidth, HintStr, HintData);
    OffsetRect(HintWinRect, HintInfo.HintPos.X, HintInfo.HintPos.Y);
    {if FHintWindow.UseRightToLeftAlignment then
      with HintWinRect do
      begin
        Dec(Left, FHintWindow.Canvas.TextWidth(HintInfo.HintStr) + 5);
        Dec(Right, FHintWindow.Canvas.TextWidth(HintInfo.HintStr) + 5);
      end;}

    { Convert the client's rect to screen coordinates }
    {with HintInfo do
    begin
      FHintCursorRect.TopLeft :=
        FHintControl.ClientToScreen(CursorRect.TopLeft);
      FHintCursorRect.BottomRight :=
        FHintControl.ClientToScreen(CursorRect.BottomRight);
    end;}

    FHintWindow.Color := HintInfo.HintColor;
    
    FHintWindow.ActivateHint(HintWinRect,HintInfo.HintStr);
    // start hide timer
    StartHintTimer(HintHidePause,ahtHideHint);
  end else
    HideHint;
  //writeln('TApplication.ShowHintWindow Info.ControlHasHint=',Info.ControlHasHint,' Type=',ord(FHintTimerType));
end;

{------------------------------------------------------------------------------
  procedure TApplication.StartHintTimer(Interval: integer;
    TimerType: TAppHintTimerType);
 ------------------------------------------------------------------------------}
procedure TApplication.StartHintTimer(Interval: integer;
  TimerType: TAppHintTimerType);
begin
  StopHintTimer;
  FHintTimerType:=TimerType;
  if Interval>0 then begin
    if FHintTimer=nil then
      FHintTimer:=TCustomTimer.Create(Self);
    FHintTimer.Interval:=Interval;
    FHintTimer.OnTimer:=@OnHintTimer;
    FHintTimer.Enabled:=true;
  end else begin
    OnHintTimer(Self);
  end
end;

{------------------------------------------------------------------------------
  procedure TApplication.OnHintTimer(Sender: TObject);
 ------------------------------------------------------------------------------}
procedure TApplication.OnHintTimer(Sender: TObject);
var
  Info: THintInfoAtMouse;
  OldHintTimerType: TAppHintTimerType;
begin
  //writeln('TApplication.OnHintTimer Type=',ord(FHintTimerType));
  OldHintTimerType:=FHintTimerType;
  StopHintTimer;
  case OldHintTimerType of
  
  ahtShowHint:
    begin
      Info:=GetHintInfoAtMouse;
      if Info.ControlHasHint then begin
        ShowHintWindow(Info);
      end else begin
        HideHint;
      end;
    end;

  else
    CancelHint;
  end;
end;

{------------------------------------------------------------------------------
  Method: TApplication.IconChanged
 ------------------------------------------------------------------------------}
procedure TApplication.IconChanged(Sender: TObject);
begin
  CNSendMessage(LM_SETFORMICON, Self, Pointer(GetIconHandle));
  // NotifyForms(CM_ICONCHANGED);
end;

{------------------------------------------------------------------------------
  Method: TApplication.GetIconHandle
  Returns: handle of default form icon
 ------------------------------------------------------------------------------}
function TApplication.GetIconHandle: HICON;
begin
  if FIcon<>nil then
    Result := FIcon.Handle
  else
    Result:=0;
end;

{------------------------------------------------------------------------------
  Method: TApplication.GetTitle
  Returns: title of application
 ------------------------------------------------------------------------------}
function TApplication.GetTitle: string;
var
  ext : string;
begin
  If FTitle = '' then begin
    Result := ExtractFileName(GetExeName);
    Ext := ExtractFileExt(Result);
    If Ext <> '' then
      Delete(Result, Length(Result) - Length(Ext) - 1, Length(Ext) + 1);
  end
  else
    Result := FTitle;
end;

{------------------------------------------------------------------------------
  Method: TApplication.HandleException
  Params: Sender
  Returns:  Nothing

  Handles all messages first then the Idle
 ------------------------------------------------------------------------------}
procedure TApplication.HandleException(Sender: TObject);
begin
  if GetCapture <> 0 then SendMessage(GetCapture, LM_CANCELMODE, 0, 0);
  if ExceptObject is Exception then begin
    if not (ExceptObject is EAbort) then
      if Assigned(FOnException) then
        FOnException(Sender, Exception(ExceptObject))
      else
        ShowException(Exception(ExceptObject));
  end else
    SysUtils.ShowException(ExceptObject, ExceptAddr);
end;

{------------------------------------------------------------------------------
  Method: TApplication.HandleMessage
  Params: None
  Returns:  Nothing

  Handles all messages first then the Idle 
 ------------------------------------------------------------------------------}
procedure TApplication.HandleMessage;
begin
  InterfaceObject.HandleEvents; // process all events
  if not FTerminate then Idle;
end;

{------------------------------------------------------------------------------
  function TApplication.IsWaiting: boolean;
------------------------------------------------------------------------------}
function TApplication.IsWaiting: boolean;
begin
  Result:=AppWaiting in FFlag;
end;

{------------------------------------------------------------------------------
  procedure TApplication.CancelHint;
------------------------------------------------------------------------------}
procedure TApplication.CancelHint;
begin
  if FHintTimer<>nil then FHintTimer.Enabled:=false;
  HideHint;
  if FHintControl <> nil then
  begin
    FHintControl := nil;
    //FHintActive := False;
    //UnhookHintHooks;
    //StopHintTimer;
  end;
end;

{------------------------------------------------------------------------------
  procedure TApplication.HideHint;
------------------------------------------------------------------------------}
procedure TApplication.HideHint;
begin
  if FHintWindow<>nil then
    FHintWindow.Visible:=false;
end;

{------------------------------------------------------------------------------
       TApplication Run
       MainForm is loaded and control is passed to event processor.
------------------------------------------------------------------------------}
procedure TApplication.Run;
begin
  if FMainForm <> nil
  then FMainForm.Show;
  repeat
    HandleMessage;
    if Assigned(FMainForm) and (FMainForm.ModalResult = mrCancel) 
    then Terminate; 
  until Terminated;
end;

{------------------------------------------------------------------------------}
{       TApplication WndPRoc                                                   }
{                                                                              }
{------------------------------------------------------------------------------}
procedure TApplication.WndProc(var AMessage : TLMessage);
begin

end;

procedure TApplication.SetHint(const AValue: string);
begin
  if FHint=AValue then exit;
  FHint:=AValue;
  if Assigned(FOnHint) then
    FOnHint(Self)
  else begin
    { Fire THintAction to anyone interested }
    {with THintAction.Create(Self) do
    begin
      Hint := Value;
      try
        Execute;
      finally
        Free;
      end;
    end;}
  end;
end;

procedure TApplication.SetHintColor(const AValue: TColor);
begin
  if FHintColor=AValue then exit;
  FHintColor:=AValue;
  if FHintWindow <> nil then
    FHintWindow.Color := FHintColor;
end;

procedure TApplication.DoOnIdleEnd;
begin
  if (AppIdleEndSent in FFlag) then exit;
  if Assigned(OnIdleEnd) then OnIdleEnd(Self);
  NotifyIdleEndHandler;
  Include(FFlag,AppIdleEndSent);
end;

{------------------------------------------------------------------------------}
{       TApplication ShowException                                             }
{------------------------------------------------------------------------------}
procedure TApplication.ShowException(E: Exception);
var
  Msg: string;
begin
  Msg := E.Message;
  if (Msg <> '') and (Msg[length(Msg)] > '.') then Msg := Msg + '.';
  MessageBox(PChar(Msg), PChar(GetTitle), MB_OK + MB_ICONERROR);
end;

{------------------------------------------------------------------------------}
{       TApplication Terminate                                                 }
{       Class is terminated and the component engine is shutdown               }
{------------------------------------------------------------------------------}
procedure TApplication.Terminate;
begin
  FTerminate := True;
  InterfaceObject.AppTerminate;
end;

{------------------------------------------------------------------------------
  procedure TApplication.NotifyUserInputHandler;

 ------------------------------------------------------------------------------}
procedure TApplication.NotifyUserInputHandler(Msg: Cardinal);
var
  i: integer;
begin
  case Msg of
  LM_MOUSEMOVE: DoOnMouseMove;
  else          CancelHint;
  end;

  if FOnUserInputHandler=nil then exit;
  i:=FOnUserInputHandler.Count-1;
  while (i>=0) and (FOnUserInputHandler<>nil) do begin
    TOnUserInputEvent(FOnUserInputHandler[i])(Self,Msg);
    dec(i);
    if (FOnUserInputHandler<>nil) and (i>FOnUserInputHandler.Count-1) then
      i:=FOnUserInputHandler.Count-1;
  end;
end;

procedure TApplication.AddOnIdleHandler(AnOnIdleHandler: TNotifyEvent);
begin
  if FOnIdleHandler=nil then
    FOnIdleHandler:=TMethodList.Create;
  FOnIdleHandler.Add(TMethod(AnOnIdleHandler));
end;

procedure TApplication.RemoveOnIdleHandler(AnOnIdleHandler: TNotifyEvent);
begin
  if FOnIdleHandler<>nil then
    FOnIdleHandler.Remove(TMethod(AnOnIdleHandler));
end;

procedure TApplication.AddOnIdleEndHandler(AnOnIdleEndHandler: TNotifyEvent);
begin
  if FOnIdleEndHandler=nil then
    FOnIdleEndHandler:=TMethodList.Create;
  FOnIdleEndHandler.Add(TMethod(AnOnIdleEndHandler));
end;

procedure TApplication.RemoveOnIdleEndHandler(AnOnIdleEndHandler: TNotifyEvent);
begin
  if FOnIdleEndHandler<>nil then
    FOnIdleEndHandler.Remove(TMethod(AnOnIdleEndHandler));
end;

procedure TApplication.AddOnUserInputHandler(
  AnOnUserInputHandler: TOnUserInputEvent);
begin
  if FOnUserInputHandler=nil then
    FOnUserInputHandler:=TMethodList.Create;
  FOnUserInputHandler.Add(TMethod(AnOnUserInputHandler));
end;

procedure TApplication.RemoveOnUserInputHandler(
  AnOnUserInputHandler: TOnUserInputEvent);
begin
  if FOnUserInputHandler<>nil then
    FOnUserInputHandler.Remove(TMethod(AnOnUserInputHandler));
end;

{------------------------------------------------------------------------------
  procedure TApplication.DoBeforeMouseMessage(CurMouseControl: TControl);
------------------------------------------------------------------------------}
procedure TApplication.DoBeforeMouseMessage(CurMouseControl: TControl);
begin
  if (FMouseControl<>CurMouseControl) then begin
    if (FMouseControl<>nil) then
      FMouseControl.Perform(CM_MOUSELEAVE, 0, 0);
    FMouseControl := CurMouseControl;
    if (FMouseControl<>nil) then
      FMouseControl.Perform(CM_MOUSEENTER, 0, 0);
  end;
end;

{------------------------------------------------------------------------------
       TApplication CreateForm
       Create a Form instance and sets the pointer to the internal form
       variable and loads the form into the application forms list
------------------------------------------------------------------------------}
procedure TApplication.CreateForm(InstanceClass: TComponentClass;
  var Reference);
var
  Instance: TComponent;
  ok: boolean;
begin
  // Allocate the form instance, without calling the constructor
  Instance := TComponent(InstanceClass.NewInstance);
  // set the Reference before the constructor is called, so that
  // events and constructors can refer to it
  TComponent(Reference) := Instance;
  ok:=false;
  try
    Instance.Create(Self);
    ok:=true;
  finally
    if not ok then
      TComponent(Reference) := nil;
  end;

  if (Instance is TForm) then begin
    if (FMainForm = nil) then begin
      TForm(Instance).HandleNeeded;
      FMainForm := TForm(Instance);
    end else begin
      if not assigned(FList) then
        FList := TList.Create;
      FList.Add(TForm(Instance));
    end;
  end;
end;

{------------------------------------------------------------------------------
  function TApplication.HandleAllocated: boolean;
  
  Checks if Handle is allocated.
------------------------------------------------------------------------------}
function TApplication.HandleAllocated: boolean;
begin
  Result:=FHandle<>0;
end;

{ =============================================================================

  $Log$
  Revision 1.44  2002/12/27 17:12:37  mattias
  added more Delphi win32 compatibility functions

  Revision 1.43  2002/12/17 12:04:30  mattias
  reduced flickering of hints

  Revision 1.42  2002/12/17 11:56:09  mattias
  fixed find declaration of non comment pos

  Revision 1.41  2002/02/09 01:48:23  mattias
  renamed TinterfaceObject.Init to AppInit and TWinControls can now contain childs in gtk

  Revision 1.40  2002/11/29 15:14:47  mattias
  replaced many invalidates by invalidaterect

  Revision 1.39  2002/11/23 13:48:43  mattias
  added Timer patch from Vincent Snijders

  Revision 1.38  2002/11/21 18:49:52  mattias
  started OnMouseEnter and OnMouseLeave

  Revision 1.37  2002/11/15 23:40:39  mattias
  added combobox createhandle old list assign

  Revision 1.36  2002/11/15 22:43:28  mattias
  added Delphis trick to set the form reference before the constructor is called

  Revision 1.35  2002/11/09 18:13:33  lazarus
  MG: fixed gdkwindow checks

  Revision 1.34  2002/11/09 15:02:06  lazarus
  MG: fixed LM_LVChangedItem, OnShowHint, small bugs

  Revision 1.33  2002/11/05 23:44:47  lazarus
  MG: implemented Application.OnShowHint

  Revision 1.32  2002/11/05 20:03:42  lazarus
  MG: implemented hints

  Revision 1.31  2002/11/02 22:25:36  lazarus
  MG: implemented TMethodList and Application Idle handlers

  Revision 1.30  2002/10/26 15:15:48  lazarus
  MG: broke LCL<->interface circles

  Revision 1.29  2002/10/26 11:05:59  lazarus
  MG: broke actnlist <-> forms circle

  Revision 1.28  2002/10/24 10:37:05  lazarus
  MG: broke dialogs.pp <-> forms.pp circle

  Revision 1.27  2002/10/24 10:15:24  lazarus
  MG: broke buttons.pp <-> forms.pp circle

  Revision 1.26  2002/10/23 20:47:26  lazarus
  AJ: Started Form Scrolling
      Started StaticText FocusControl
      Fixed Misc Dialog Problems
      Added TApplication.Title

  Revision 1.25  2002/08/31 11:37:09  lazarus
  MG: fixed destroying combobox

  Revision 1.24  2002/05/28 19:39:45  lazarus
  MG: added gtk rc file support and started stule dependent syscolors

  Revision 1.23  2002/05/24 07:16:31  lazarus
  MG: started mouse bugfix and completed Makefile.fpc

  Revision 1.22  2002/05/10 06:05:51  lazarus
  MG: changed license to LGPL

  Revision 1.21  2002/04/04 12:25:01  lazarus
  MG: changed except statements to more verbosity

  Revision 1.20  2002/03/16 21:40:54  lazarus
  MG: reduced size+move messages between lcl and interface

  Revision 1.19  2002/03/08 11:37:42  lazarus
  MG: outputfilter can now find include files

  Revision 1.18  2002/03/04 10:01:01  lazarus
  MG: fixed synedit crash on exit

  Revision 1.17  2002/01/27 23:35:33  lazarus
  MG: added error message, when lcl has abstract widget interface object

  Revision 1.16  2002/01/27 23:24:37  lazarus
  MG: added error message, when lcl founds no widget interface object

  Revision 1.15  2001/12/08 12:35:12  lazarus
  MG: added TApplication.ShowException

  Revision 1.14  2001/11/14 17:46:58  lazarus
  Changes to make toggling between form and unit work.
  Added BringWindowToTop
  Shane

  Revision 1.13  2001/11/05 18:18:19  lazarus
  added popupmenu+arrows to notebooks, added target filename

  Revision 1.12  2001/11/01 21:30:35  lazarus
  Changes to Messagebox.
  Added line to CodeTools to prevent duplicate USES entries.

  Revision 1.11  2001/11/01 18:48:52  lazarus
  Changed Application.Messagebox to use TMessageBox class.
  Added icon images for mtError and mtConfirmation
  Shane

  Revision 1.10  2001/10/31 22:12:12  lazarus
  MG: added ExceptProc to forms.pp

  Revision 1.9  2001/10/31 21:43:29  lazarus
  Added code for TApplication to get it ready to accept exceptions.
  Shane

  Revision 1.8  2001/10/15 13:11:28  lazarus
  MG: added complete code

  Revision 1.7  2001/07/01 23:33:13  lazarus
  MG: added WaitMessage and HandleEvents is now non blocking

  Revision 1.6  2001/06/28 18:15:03  lazarus
  MG: bugfixes for destroying controls

  Revision 1.5  2001/06/26 00:08:35  lazarus
  MG: added code for form icons from Rene E. Beszon

  Revision 1.4  2001/06/04 09:32:17  lazarus
  MG: fixed bugs and cleaned up messages

  Revision 1.3  2001/01/24 23:26:40  lazarus
  MWE:
    = moved some types to gtkdef
    + added WinWidgetInfo
    + added some initialization to Application.Create

  Revision 1.2  2000/09/10 19:58:47  lazarus
  MWE:
    * Updated makefiles for FPC release 1.0  binary units
    * Changed creation, now LCL unit distributions are possible
    * Moved interfaces.pp from LCL to interface dirs

  Revision 1.1  2000/07/13 10:28:24  michael
  + Initial import

  Revision 1.9  2000/06/13 20:50:42  lazarus
  MWE:
    - Started to remove obsolete/dead code/messages

  HJO:
    * Fixed messages in showmodal of 2nd form
    * Fixed modal result for button

  Revision 1.8  2000/05/25 19:34:31  lazarus
  MWE:
    * Fixed messagequeue.count bug in GTKObject.Destroy
      (thanks to Vincent Snijders)

}
