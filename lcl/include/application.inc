{%MainUnit ../forms.pp}
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
  DefHintColor = clInfoBk;  { default hint window color }
  DefHintPause = 500;       { default pause before hint window displays (ms) }
  DefHintShortPause = 0;    { default reshow pause }
  DefHintHidePause = DefHintPause * 5; { default pause before hint is hidden }

function FindApplicationComponent(const ComponentName: string): TComponent;
begin
  Result:=Application.FindComponent(ComponentName);
end;

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
      and ((GetKeyState(VK_LBUTTON) and $80)=0)
      and ((GetKeyState(VK_MBUTTON) and $80)=0)
      and ((GetKeyState(VK_RBUTTON) and $80)=0);
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

{------------------------------------------------------------------------------
       TApplication Constructor
------------------------------------------------------------------------------}
constructor TApplication.Create(AOwner: TComponent);
begin
  Focusmessages := True;
  LCLProc.SendApplicationMessageFunction:=@SendApplicationMsg;

  FMainForm := nil;
  FMouseControl := nil;
  FHandle := 0;
  FHintColor := DefHintColor;
  FHintPause := DefHintPause;
  FHintShortCuts := True;
  FHintShortPause := DefHintShortPause;
  FHintHidePause := DefHintHidePause;
  FShowHint := true;
  FFormList := nil;
  FOnIdle := nil;
  FIcon := nil;
  FNavigation := [anoTabToSelectNext,anoReturnForDefaultControl,
                  anoEscapeForCancelControl];
  ApplicationActionComponent:=Self;
  OnMenuPopupHandler:=@MenuPopupHandler;


  inherited Create(AOwner);
  CaptureExceptions:=true;
end;

{------------------------------------------------------------------------------
       TApplication Destructor
------------------------------------------------------------------------------}
destructor TApplication.Destroy;
var
  HandlerType: TApplicationHandlerType;
begin
  Include(FFlags,AppDestroying);
  DoFreeReleaseComponents;
  if OnMenuPopupHandler=@MenuPopupHandler then
    OnMenuPopupHandler:=nil;
  
  // shutting down
  CancelHint;
  ShowHint := False;

  // destroying
  ApplicationActionComponent:=nil;
  FreeThenNil(FIcon);
  FreeThenNil(FFormList);
  
  for HandlerType:=Low(TApplicationHandlerType) to High(TApplicationHandlerType)
  do
    FreeThenNil(FApplicationHandlers[HandlerType]);
  inherited Destroy;
  
  Include(FFlags,AppDoNotReleaseComponents);
  DoFreeReleaseComponents;

  // restore exception handling
  CaptureExceptions:=false;
  LCLProc.SendApplicationMessageFunction:=nil;
end;

{------------------------------------------------------------------------------
       TApplication BringToFront
------------------------------------------------------------------------------}
procedure TApplication.BringToFront;
begin
  InterfaceObject.AppBringToFront;
end;


{------------------------------------------------------------------------------}
{       TApplication Messagebox     }
{------------------------------------------------------------------------------}
function TApplication.MessageBox(Text, Caption : PChar; Flags : Longint) : Integer;
begin
  if Assigned(MessageBoxFunction) then
    Result:=MessageBoxFunction(Text,Caption,Flags)
  else begin
    DebugLn('WARNING: TApplication.MessageBox: no MessageBoxFunction');
    DebugLn('  Caption="',Caption,'"');
    DebugLn('  Text="',Text,'"');
    DebugLn('  Flags=',HexStr(Cardinal(Flags),8));
    Result:=0;
  end;
end;

{------------------------------------------------------------------------------
       TApplication GetExename
------------------------------------------------------------------------------}
Function TApplication.GetExeName: String;
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
  Method: TApplication.Minimize
  Params: None
  Returns: Nothing

  Minimizes the application.
 ------------------------------------------------------------------------------}
procedure TApplication.Minimize;
begin
  InterfaceObject.AppMinimize;
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
       Makes a call to the component engine to provide any initialization that
       needs to occur.
------------------------------------------------------------------------------}
procedure TApplication.Initialize;
begin
  inherited Initialize;
  // interface object and screen
  if (InterfaceObject=nil)
//  or (AnsiCompareText(InterfaceObject.Classname,'TWIDGETSET')=0) 
  or (InterfaceObject.ClassType = TWidgetSet) 
  then begin
    DebugLn('ERROR: ',rsNoInterfaceObject);
    raise Exception.Create(rsNoInterfaceObject);
  end;
  InterfaceObject.AppInit(ScreenInfo);
  ScreenInfo.Initialized:=true;
  Screen.UpdateScreen;
  // application icon
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
begin
  if FMouseControl <> CurrentControl then begin
    UpdateMouseControl(CurrentControl);
  end;
end;

procedure TApplication.SetCaptureExceptions(const AValue: boolean);
begin
  if FCaptureExceptions=AValue then exit;
  FCaptureExceptions:=AValue;
  if FCaptureExceptions then begin
    // capture exceptions
    // store old exceptproc
    if FOldExceptProc=nil then
      FOldExceptProc:=ExceptProc;
    ExceptProc:=@ExceptionOccurred;
  end else begin
    // do not capture exceptions
    if ExceptProc=@ExceptionOccurred then begin
      // restore old exceptproc
      ExceptProc:=FOldExceptProc;
      FOldExceptProc:=nil;
    end;
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
  Done: Boolean;
begin
  DoFreeReleaseComponents;
  MouseIdle(GetControlAtMouse);

  Done := True;
  if Assigned(FOnIdle) then FOnIdle(Self, Done);
  NotifyIdleHandler;
  if Done then begin
    // wait till something happens
    DoIdleActions;
    Include(FFlags,AppWaiting);
    Exclude(FFlags,AppIdleEndSent);
    InterfaceObject.WaitMessage;
    DoOnIdleEnd;
    Exclude(FFlags,AppWaiting);
  end;
end;

{------------------------------------------------------------------------------
  function TApplication.InvokeHelp(Command: Word; Data: Longint): Boolean;
 ------------------------------------------------------------------------------}
function TApplication.InvokeHelp(Command: Word; Data: Longint): Boolean;
var
  CallHelp: Boolean;
  //HelpHandle: HWND;
  ActiveForm: TCustomForm;
begin
  Result := False;
  CallHelp := True;
  ActiveForm := Screen.ActiveCustomForm;

  { let existing hooks get called, if any. }
  if Assigned(ActiveForm) and Assigned(ActiveForm.FOnHelp) then
    Result := ActiveForm.FOnHelp(Command, Data, CallHelp)
  else if Assigned(FOnHelp) then
    Result := FOnHelp(Command, Data, CallHelp);

  if CallHelp then begin
    if Assigned(ActiveForm) and ActiveForm.HandleAllocated
    and (ActiveForm.FHelpFile <> '') then
    begin
      //HelpHandle := ActiveForm.Handle;
      //if ValidateHelpSystem then
      //  Result := HelpSystem.Hook(Longint(HelpHandle), ActiveForm.FHelpFile,
      //                            Command, Data);
    end
    else
    if HelpFile <> '' then
    begin
      //HelpHandle := Handle;
      //if FMainForm <> nil then HelpHandle := FMainForm.Handle;
      //if ValidateHelpSystem then
      //  Result := HelpSystem.Hook(Longint(HelpHandle),FHelpFile,Command,Data);
    end else begin
      //if not FHandleCreated then
      //  PostMessage(FHandle, CM_INVOKEHELP, Command, Data);
    end;
  end;
end;

{------------------------------------------------------------------------------
  function TApplication.GetControlAtMouse: TControl;

 ------------------------------------------------------------------------------}
function TApplication.GetControlAtMouse: TControl;
var
  P: TPoint;
begin
  GetCursorPos(P);
  Result := FindControlAtPosition(P, True);
  if (Result <> nil) and (csDesigning in Result.ComponentState) then
    Result := nil;
end;

procedure TApplication.SetNavigation(const AValue: TApplicationNavigationOptions
  );
begin
  if FNavigation=AValue then exit;
  FNavigation:=AValue;
end;

{------------------------------------------------------------------------------
  procedure TApplication.UpdateMouseControl(NewMouseControl: TControl);

 ------------------------------------------------------------------------------}
procedure TApplication.UpdateMouseControl(NewMouseControl: TControl);
begin
  if FMouseControl=NewMouseControl then exit;
  {write('TApplication.UpdateMouseControl A ');
  if FMouseControl<>nil then
    write(' Old=',FMouseControl.Name,':',FMouseControl.ClassName)
  else
    write(' Old=nil');
  if NewMouseControl<>nil then
    DebugLn' New=',NewMouseControl.Name,':',NewMouseControl.ClassName)
  else
    DebugLn' New=nil');}
  if (FMouseControl<>nil) then begin
    //DebugLn' MOUSELEAVE=',FMouseControl.Name,':',FMouseControl.ClassName);
    FMouseControl.Perform(CM_MOUSELEAVE, 0, 0);
  end;
  FMouseControl := NewMouseControl;
  if (FMouseControl<>nil) then begin
    //DebugLn' MOUSEENTER=',FMouseControl.Name,':',FMouseControl.ClassName);
    FMouseControl.Perform(CM_MOUSEENTER, 0, 0);
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
  procedure TApplication.SetTitle(const AValue: String);
 ------------------------------------------------------------------------------}
procedure TApplication.SetTitle(const AValue: String);
begin
  inherited SetTitle(AValue);
  // ToDo: tell the interface
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
function TApplication.ValidateHelpSystem: Boolean;
begin
  Result:=false;
end;

{------------------------------------------------------------------------------
  procedure TApplication.NotifyIdleHandler;
 ------------------------------------------------------------------------------}
procedure TApplication.NotifyIdleHandler;
var
  i: integer;
begin
  i:=FApplicationHandlers[ahtIdle].Count;
  while FApplicationHandlers[ahtIdle].NextDownIndex(i) do
    TNotifyEvent(FApplicationHandlers[ahtIdle][i])(Self);
end;

{------------------------------------------------------------------------------
  procedure TApplication.NotifyIdleEndHandler;

 ------------------------------------------------------------------------------}
procedure TApplication.NotifyIdleEndHandler;
var
  i: integer;
begin
  i:=FApplicationHandlers[ahtIdleEnd].Count;
  while FApplicationHandlers[ahtIdleEnd].NextDownIndex(i) do
    TNotifyEvent(FApplicationHandlers[ahtIdleEnd][i])(Self);
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
  //DebugLn'TApplication.DoOnMouseMove Info.ControlHasHint=',Info.ControlHasHint,' Type=',ord(FHintTimerType));
  if FHintControl <> Info.Control then
  begin
    if Info.ControlHasHint then
    begin
      FHintControl := Info.Control;
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
  CanShow := FHintControl.Perform(CM_HINTSHOW, 0, LParam(@HintInfo)) = 0;
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
  //DebugLn'TApplication.ShowHintWindow Info.ControlHasHint=',Info.ControlHasHint,' Type=',ord(FHintTimerType));
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
  //DebugLn'TApplication.OnHintTimer Type=',ord(FHintTimerType));
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
  procedure TApplication.UpdateVisible;
 ------------------------------------------------------------------------------}
procedure TApplication.UpdateVisible;
begin

end;

{------------------------------------------------------------------------------
  procedure TApplication.DoIdleActions;
 ------------------------------------------------------------------------------}
procedure TApplication.DoIdleActions;
var
  i: Integer;
  CurForm: TCustomForm;
  AForm: TForm;
begin
  for i := 0 to Screen.CustomFormCount - 1 do begin
    CurForm:=Screen.CustomForms[I];
    if CurForm.HandleAllocated and CurForm.Visible and CurForm.Enabled then
      CurForm.UpdateActions;
  end;
  if FFormList<>nil then begin
    for i:=0 to FFormList.Count-1 do begin
      AForm:=TForm(FFormList[i]);
      if AForm.FormStyle=fsSplash then
        AForm.Hide;
    end;
  end;
end;

procedure TApplication.MenuPopupHandler(Sender: TObject);
begin
  HideHint;
end;

{------------------------------------------------------------------------------
  Method: TApplication.DoFreeReleaseComponents
  
  Free all components that were queued for freeing (ReleaseComponent)
 ------------------------------------------------------------------------------}
procedure TApplication.DoFreeReleaseComponents;
var
  AComponent: TComponent;
begin
  if FReleaseComponents=nil then exit;
  while FReleaseComponents.Count>0 do begin
    AComponent:=TComponent(FReleaseComponents[0]);
    FReleaseComponents.Delete(0);
    AComponent.Free;
  end;
  FreeThenNil(FReleaseComponents);
end;

{------------------------------------------------------------------------------
  Method: TApplication.IconChanged
 ------------------------------------------------------------------------------}
procedure TApplication.IconChanged(Sender: TObject);
begin
  DebugLn('TApplication.IconChanged - TODO: convert this message...no implementation in gtk or win32');
  // CNSendMessage(LM_SETFORMICON, Self, Pointer(GetIconHandle));
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
  Ext: string;
begin
  Result := inherited Title;
  If Result = '' then begin
    Result := ExtractFileName(GetExeName);
    Ext := ExtractFileExt(Result);
    If Ext <> '' then
      Delete(Result, Length(Result) - Length(Ext) + 1, Length(Ext));
  end;
end;

{------------------------------------------------------------------------------
  Method: TApplication.HandleException
  Params: Sender
  Returns:  Nothing

  Handles all messages first then the Idle
 ------------------------------------------------------------------------------}
procedure TApplication.HandleException(Sender: TObject);
begin
  if Self=nil then exit;
  if AppHandlingException in FFlags then begin
    // there was an exception during showing the exception -> break the circle
    DebugLn('TApplication.HandleException: ',
      'there was another exception during showing the first exception');
    HaltingProgram:=true;
    Halt;
  end;
  Include(FFlags,AppHandlingException);
  if StopOnException then
    inherited Terminate;
  // before we do anything, write it down
  if ExceptObject is Exception then begin
    DebugLn('TApplication.HandleException ',Exception(ExceptObject).Message);
  end else begin
    DebugLn('TApplication.HandleException Strange Exception ');
  end;
  // release capture and hide all forms with stay on top, so that
  // a message can be shown
  if GetCapture <> 0 then SendMessage(GetCapture, LM_CANCELMODE, 0, 0);
  HideAllFormsWithStayOnTop;
  // handle the exception
  if ExceptObject is Exception then begin
    if not (ExceptObject is EAbort) then
      if Assigned(OnException) then
        OnException(Sender, Exception(ExceptObject))
      else
        ShowException(Exception(ExceptObject));
  end else
    SysUtils.ShowException(ExceptObject, ExceptAddr);
  Exclude(FFlags,AppHandlingException);
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
  if not Terminated then Idle;
end;

{------------------------------------------------------------------------------
  function TApplication.HelpCommand(Command: Word; Data: Longint): Boolean;
------------------------------------------------------------------------------}
function TApplication.HelpCommand(Command: Word; Data: Longint): Boolean;
begin
  Result := InvokeHelp(Command, Data);
end;

{------------------------------------------------------------------------------
  function TApplication.HelpContext(Context: THelpContext): Boolean;
------------------------------------------------------------------------------}
function TApplication.HelpContext(Context: THelpContext): Boolean;
begin
  if ValidateHelpSystem then begin
    Result := true;
    //HelpSystem.ShowContextHelp(Context, GetCurrentHelpFile);
  end else
    Result := false;
end;

{------------------------------------------------------------------------------
  function TApplication.HelpJump(const JumpID: string): Boolean;
------------------------------------------------------------------------------}
function TApplication.HelpJump(const JumpID: string): Boolean;
begin
  if ValidateHelpSystem then begin
    Result := true;
    //HelpSystem.ShowTopicHelp(JumpID, GetCurrentHelpFile);
  end else
    Result := false;
end;

{------------------------------------------------------------------------------
  function TApplication.HelpKeyword(const Keyword: String): Boolean;
------------------------------------------------------------------------------}
function TApplication.HelpKeyword(const Keyword: String): Boolean;
begin
  if ValidateHelpSystem then begin
    Result := true;
    //HelpSystem.ShowHelp(Keyword, GetCurrentHelpFile);
  end else
    Result := false;
end;

{------------------------------------------------------------------------------
  procedure TApplication.HideAllFormsWithStayOnTop;
------------------------------------------------------------------------------}
procedure TApplication.HideAllFormsWithStayOnTop;
var
  i: Integer;
  AForm: TCustomForm;
begin
  if (Screen=nil) then exit;
  for i:=0 to Screen.CustomFormCount-1 do begin
    AForm:=Screen.CustomForms[i];
    if AForm.FormStyle in fsAllStayOnTop then begin
      DebugLn('TApplication.HideAllFormsWithStayOnTop ',AForm.Name,':',AForm.ClassName);
      AForm.Hide;
    end;
  end;
end;

{------------------------------------------------------------------------------
  function TApplication.IsWaiting: boolean;
------------------------------------------------------------------------------}
function TApplication.IsWaiting: boolean;
begin
  Result:=AppWaiting in FFlags;
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
    //FHintControl := nil;
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

  procedure RunMessage;
  begin
    HandleMessage;
    if Assigned(FMainForm) and (FMainForm.ModalResult = mrCancel)
    then Terminate;
  end;

begin
  if FMainForm <> nil then FMainForm.Show;
  repeat
    if CaptureExceptions then begin
      // run with try..except
      try
        RunMessage;
      except
        on E: Exception do HandleException(E);
      end;
    end else begin
      // run without try..except
      RunMessage;
    end;
  until Terminated;
end;

{------------------------------------------------------------------------------}
{       TApplication WndPRoc                                                   }
{                                                                              }
{------------------------------------------------------------------------------}
procedure TApplication.WndProc(var AMessage : TLMessage);
begin
  case AMessage.Msg of
    CM_ACTIONEXECUTE, CM_ACTIONUPDATE: AMessage.Result := LResult(DispatchAction(AMessage.Msg, TBasicAction(AMessage.LParam)));
  else
    Dispatch(AMessage);
  end;
end;

function TApplication.DispatchAction(Msg: Longint; Action: TBasicAction
  ): Boolean;
var
  Form: TCustomForm;
begin
  Form := Screen.ActiveForm;
  if (Form<>nil) and (Form.Perform(Msg, 0, PtrInt(Action)) = 1) then
    Result:=true
  else if (MainForm<>Form) and (MainForm<>nil)
    and (MainForm.Perform(Msg, 0, PtrInt(Action)) = 1)
  then
    Result:=true;
  // Disable action if no "user" handler is available
  if (not Result) and (Action is TCustomAction)
  and TCustomAction(Action).Enabled
  and TCustomAction(Action).DisableIfNoHandler then
    TCustomAction(Action).Enabled := Assigned(Action.OnExecute);
end;

procedure TApplication.AddHandler(HandlerType: TApplicationHandlerType;
  const Handler: TMethod; AsLast: Boolean);
begin
  if Handler.Code=nil then RaiseGDBException('TApplication.AddHandler');
  if FApplicationHandlers[HandlerType]=nil then
    FApplicationHandlers[HandlerType]:=TMethodList.Create;
  FApplicationHandlers[HandlerType].Add(Handler,AsLast);
end;

procedure TApplication.RemoveHandler(HandlerType: TApplicationHandlerType;
  const Handler: TMethod);
begin
  FApplicationHandlers[HandlerType].Remove(Handler);
end;

function TApplication.GetConsoleApplication: boolean;
begin
  Result:=false;
end;

procedure TApplication.SetHint(const AValue: string);
begin
  if FHint=AValue then exit;
  FHint:=AValue;
  if Assigned(FOnHint) then
    FOnHint(Self)
  else begin
    // Send THintAction
    {with THintAction.Create(Self) do begin
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
  if (AppIdleEndSent in FFlags) then exit;
  if Assigned(OnIdleEnd) then OnIdleEnd(Self);
  NotifyIdleEndHandler;
  Include(FFlags,AppIdleEndSent);
end;

{------------------------------------------------------------------------------
  function TApplication.GetCurrentHelpFile: string;
------------------------------------------------------------------------------}
function TApplication.GetCurrentHelpFile: string;
var
  ActiveForm: TCustomForm;
begin
  ActiveForm := Screen.ActiveCustomForm;
  if Assigned(ActiveForm) and (ActiveForm.FHelpFile <> '') then
    Result := ActiveForm.HelpFile
  else
    Result := HelpFile;
end;

{------------------------------------------------------------------------------
       TApplication ShowException
------------------------------------------------------------------------------}
procedure TApplication.ShowException(E: Exception);
var
  Msg: string;
  MsgResult: Integer;
begin
  if AppNoExceptionMessages in FFlags then exit;
  Msg := E.Message;
  if (Msg <> '') and (Msg[length(Msg)] <> '.') then Msg := Msg + '.';
  if (not Terminated)
  and (Self<>nil) then begin
    MsgResult:=MessageBox(PChar(Msg+#13#13'Press Ok to ignore.'
       +#13'Press Cancel to stop the program.'),PChar(GetTitle),
                          MB_OKCANCEL + MB_ICONERROR);
    if MsgResult<>mrOk then begin
      Include(FFlags,AppNoExceptionMessages);
      HaltingProgram:=true;
      Halt;
    end;
  end else
    inherited ShowException(E);
end;

{------------------------------------------------------------------------------}
{       TApplication Terminate                                                 }
{       Class is terminated and the component engine is shutdown               }
{------------------------------------------------------------------------------}
procedure TApplication.Terminate;
begin
  inherited Terminate;
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

  i:=FApplicationHandlers[ahtUserInput].Count;
  while FApplicationHandlers[ahtUserInput].NextDownIndex(i) do
    TOnUserInputEvent(FApplicationHandlers[ahtUserInput][i])(Self,Msg);
end;

procedure TApplication.NotifyKeyDownBeforeHandler(Sender: TObject;
  var Key: Word; Shift: TShiftState);
var
  i: Integer;
begin
  i:=FApplicationHandlers[ahtKeyDownBefore].Count;
  while FApplicationHandlers[ahtKeyDownBefore].NextDownIndex(i) do
    TKeyEvent(FApplicationHandlers[ahtKeyDownBefore][i])(Self,Key,Shift);
end;

procedure TApplication.NotifyKeyDownHandler(Sender: TObject;
  var Key: Word; Shift: TShiftState);
var
  i: Integer;
begin
  i:=FApplicationHandlers[ahtKeyDownAfter].Count;
  while FApplicationHandlers[ahtKeyDownAfter].NextDownIndex(i) do
    TKeyEvent(FApplicationHandlers[ahtKeyDownAfter][i])(Self,Key,Shift);
end;

procedure TApplication.ControlKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var
  AControl: TWinControl;
  Form: TCustomForm;
begin
  if Sender is TWinControl then begin
    AControl:=TWinControl(Sender);
    //debugln('TApplication.ControlKeyDown A ',Acontrol.Name);

    // handle tab keys
    if (Key=VK_Tab) and ((Shift-[ssShift])=[])
    and (anoTabToSelectNext in Navigation)
    and AControl.Focused then
    begin
      Key:=VK_UNKNOWN;
      AControl.PerformTab(not (ssShift in Shift));
    end;
    
    // check for special actions handled ourselves
    if (Shift = []) and ((Key = VK_RETURN) or (Key = VK_ESCAPE)) then
    begin
      Form := GetParentForm(AControl);
      //debugln('TApplication.ControlKeyDown B ',DbgSName(Acontrol),' ',DbgSName(Form),' ',dbgs(anoEscapeForCancelControl in Navigation));
      if Form<>nil then begin
        //debugln('TApplication.ControlKeyDown C Form.DefaultControl=',dbgs(Form.DefaultControl),' Form.CancelControl=',dbgs(Form.CancelControl));
        case Key of
          VK_RETURN:
            if (anoReturnForDefaultControl in Navigation)
            and (Form.DefaultControl <> nil) then
            begin
              Form.DefaultControl.ExecuteDefaultAction;
              Key := VK_UNKNOWN;
            end;

          VK_ESCAPE:
            if (anoEscapeForCancelControl in Navigation) then begin
              if (Form.CancelControl <> nil) then
              begin
                //debugln('TApplication.ControlKeyDown C ',Acontrol.Name);
                Form.CancelControl.ExecuteCancelAction;
                Key := VK_UNKNOWN;
              end;
            end;
            
        end;
      end;
    end;
  end;
end;

procedure TApplication.AddOnIdleHandler(Handler: TNotifyEvent;
  AsLast: Boolean);
begin
  AddHandler(ahtIdle,TMethod(Handler),AsLast);
end;

procedure TApplication.RemoveOnIdleHandler(Handler: TNotifyEvent);
begin
  RemoveHandler(ahtIdle,TMethod(Handler));
end;

procedure TApplication.AddOnIdleEndHandler(Handler: TNotifyEvent;
  AsLast: Boolean);
begin
  AddHandler(ahtIdleEnd,TMethod(Handler),AsLast);
end;

procedure TApplication.RemoveOnIdleEndHandler(Handler: TNotifyEvent);
begin
  RemoveHandler(ahtIdleEnd,TMethod(Handler));
end;

procedure TApplication.AddOnUserInputHandler(Handler: TOnUserInputEvent;
  AsLast: Boolean);
begin
  AddHandler(ahtUserInput,TMethod(Handler),AsLast);
end;

procedure TApplication.RemoveOnUserInputHandler(Handler: TOnUserInputEvent);
begin
  RemoveHandler(ahtUserInput,TMethod(Handler));
end;

procedure TApplication.AddOnKeyDownBeforeHandler(Handler: TKeyEvent;
  AsLast: Boolean);
begin
  AddHandler(ahtKeyDownBefore,TMethod(Handler),AsLast);
end;

procedure TApplication.RemoveOnKeyDownBeforeHandler(Handler: TKeyEvent);
begin
  RemoveHandler(ahtKeyDownBefore,TMethod(Handler));
end;

procedure TApplication.AddOnKeyDownHandler(Handler: TKeyEvent; AsLast: Boolean);
begin
  AddHandler(ahtKeyDownAfter,TMethod(Handler),AsLast);
end;

procedure TApplication.RemoveOnKeyDownHandler(Handler: TKeyEvent);
begin
  RemoveHandler(ahtKeyDownAfter,TMethod(Handler));
end;

procedure TApplication.RemoveAllHandlersOfObject(AnObject: TObject);
var
  HandlerType: TApplicationHandlerType;
begin
  for HandlerType:=Low(TApplicationHandlerType) to High(TApplicationHandlerType)
  do
    FApplicationHandlers[HandlerType].RemoveAllMethodsOfObject(AnObject);
end;

{------------------------------------------------------------------------------
  procedure TApplication.DoBeforeMouseMessage(CurMouseControl: TControl);
------------------------------------------------------------------------------}
procedure TApplication.DoBeforeMouseMessage(CurMouseControl: TControl);
begin
  UpdateMouseControl(GetControlAtMouse);
end;

function TApplication.IsShortcut(var Message: TLMKey): boolean;
begin
  Result := false;
  if Assigned(FOnShortcut) then
  begin
    FOnShortcut(Message, Result);
    if Result then
      exit;
  end;
  // only send shortcuts to main form if no form is showing modal
  //  or otherwise disabling the main form
  if Assigned(FMainForm) and IsWindowEnabled(FMainForm.Handle) then
    Result := FMainForm.IsShortcut(Message);
end;

{------------------------------------------------------------------------------
  TApplication CreateForm

  Note: The name is confusing and only kept for Delphi compatibility. It can
  create any kind of components.

  Create a Component instance and sets the pointer to the component variable
  and loads the component. If it is a form it will be added to the applications
  forms list
------------------------------------------------------------------------------}
procedure TApplication.CreateForm(InstanceClass: TComponentClass;
  var Reference);
var
  Instance: TComponent;
  ok: boolean;
  OldFindGlobalComponent: TFindGlobalComponent;
  AForm: TForm;
begin
  // Allocate the instance, without calling the constructor
  Instance := TComponent(InstanceClass.NewInstance);
  // set the Reference before the constructor is called, so that
  // events and constructors can refer to it
  TComponent(Reference) := Instance;

  OldFindGlobalComponent:=FindGlobalComponent;
  FindGlobalComponent:=@FindApplicationComponent;
  ok:=false;
  try
    Instance.Create(Self);
    ok:=true;
  finally
    if not ok then
      TComponent(Reference) := nil;
    FindGlobalComponent:=OldFindGlobalComponent;
  end;

  if (Instance is TForm) then begin
    AForm:=TForm(Instance);
    if (FMainForm = nil) and (AForm.FormStyle=fsNormal) then begin
      AForm.HandleNeeded;
      FMainForm := AForm;
    end else begin
      if not Assigned(FFormList) then
        FFormList := TList.Create;
      FFormList.Add(AForm);
    end;
    if AForm.FormStyle=fsSplash then begin
      // show the splash form and handle the paint message
      AForm.Show;
      AForm.Paint;
      ProcessMessages;
    end;
  end;
  {$IFNDEF AfterConstructionDataModuleNotWorking}
  if (Instance is TDataModule) then begin
    TDataModule(instance).AfterConstruction;
  end;
  {$ENDIF}
end;

procedure TApplication.ReleaseComponent(AComponent: TComponent);
begin
  if AppDoNotReleaseComponents in FFlags then
    raise Exception.Create('TApplication.ReleaseComponent already shut down');
  if FReleaseComponents=nil then
    FReleaseComponents:=TList.Create;
  if FReleaseComponents.IndexOf(AComponent)<0 then
    FReleaseComponents.Add(AComponent);
end;

function TApplication.ExecuteAction(ExeAction: TBasicAction): Boolean;
begin
  Result := False;
  if Assigned(FOnActionExecute) then FOnActionExecute(ExeAction, Result);
end;

function TApplication.UpdateAction(TheAction: TBasicAction): Boolean;
begin
  Result := False;
  if Assigned(FOnActionUpdate) then FOnActionUpdate(TheAction,Result);
end;

{------------------------------------------------------------------------------
  function TApplication.HandleAllocated: boolean;

  Checks if Handle is allocated.
------------------------------------------------------------------------------}
function TApplication.HandleAllocated: boolean;
begin
  Result:=FHandle<>0;
end;

// included by forms.pp

{ =============================================================================

  $Log$
  Revision 1.98  2005/02/17 18:48:24  mattias
  extented error message of TApplication

  Revision 1.97  2005/02/05 16:09:52  marc
  * first 64bit changes

  Revision 1.96  2005/02/05 09:05:50  micha
  add platform independent winapi function IsWindowEnabled

  Revision 1.95  2005/02/03 15:10:23  micha
  implement shortcut handling, tcustomlabel accelerator focuscontrol functionality

  Revision 1.94  2005/01/14 00:27:05  mattias
  fixed several dialogs to react on esacpe

  Revision 1.93  2004/12/22 19:56:44  mattias
  started TFont mirgration to fpCanvas font

  Revision 1.92  2004/09/18 10:52:48  micha
  convert LM_SCREENINIT message to interface method (integrated with TWidgetSet.AppInit(var ScreenInfo)

  Revision 1.91  2004/09/15 07:57:59  micha
  convert LM_SETFORMICON message to interface method

  Revision 1.90  2004/09/11 13:38:37  micha
  convert LM_BRINGTOFRONT message to interface method
  NOTE: was only used for tapplication, not from other controls

  Revision 1.89  2004/08/27 08:55:22  micha
  implement tapplication.minimize for win32, stub for gtk

  Revision 1.88  2004/08/26 22:12:39  mattias
  localized graphprop editor form and replaced opensavedialog with opensavepicturedialog

  Revision 1.87  2004/08/26 19:09:34  mattias
  moved navigation key handling to TApplication and added options for custom navigation

  Revision 1.86  2004/08/18 14:24:55  mattias
  implemented TCustomForm.Release

  Revision 1.85  2004/08/13 10:20:19  mattias
  fixed codetools ConstSet, implemented notifying TApplication whenmenu popups

  Revision 1.84  2004/08/09 21:12:43  mattias
  implemented FormStyle fsSplash for splash screens

  Revision 1.83  2004/08/04 10:35:38  mattias
  added workaround for not working TDataModule.AfterConstruction

  Revision 1.82  2004/07/25 01:04:45  mattias
  TXMLPropStorage basically working

  Revision 1.81  2004/07/10 18:17:30  mattias
  added Delphi ToDo support, Application.WndProc, small bugfixes  from Colin

  Revision 1.80  2004/05/21 18:12:17  mattias
  quick fixed crashing property overloading BorderStyle

  Revision 1.79  2004/05/11 11:42:26  mattias
  replaced writeln by debugln

  Revision 1.78  2004/04/10 17:58:56  mattias
  implemented mainunit hints for include files

  Revision 1.77  2004/03/05 00:14:02  marc
  * Renamed TInterfaceBase to TWidgetSet

  Revision 1.76  2004/02/28 00:34:35  mattias
  fixed CreateComponent for buttons, implemented basic Drag And Drop

  Revision 1.75  2004/02/23 23:15:13  mattias
  improved FindDragTarget

  Revision 1.74  2004/02/23 18:24:38  mattias
  completed new TToolBar

  Revision 1.73  2004/02/23 08:19:04  micha
  revert intf split

  Revision 1.71  2004/02/10 02:00:13  mattias
  activated Idle actions

  Revision 1.70  2004/02/02 17:39:10  mattias
  added TActionList - actions need testing

  Revision 1.69  2003/12/29 14:22:22  micha
  fix a lot of range check errors win32

  Revision 1.68  2003/12/25 14:17:07  mattias
  fixed many range check warnings

  Revision 1.67  2003/12/14 19:18:04  micha
  hint fixes: parentfont, font itself, showing/hiding + more

  Revision 1.66  2003/11/17 23:09:39  mattias
  started PixelsPerInch

  Revision 1.65  2003/11/17 22:53:26  mattias
  updated Makefiles

  Revision 1.64  2003/11/07 18:47:30  micha
  hintwindow fixes

  Revision 1.63  2003/10/21 18:44:10  mattias
  removed CustApp defines

  Revision 1.62  2003/08/22 13:51:25  mattias
  fixed TApplication.GetTitle

  Revision 1.61  2003/08/12 21:35:11  mattias
  TApplication now descends from TCustomApplication

  Revision 1.60  2003/08/09 16:30:33  mattias
  fixed LM_ShowModal for win32 intf from Karl

  Revision 1.59  2003/07/16 16:53:59  mattias
  added TApplication.CaptureExceptions

  Revision 1.58  2003/07/01 15:37:03  mattias
  fixed exception handling

  Revision 1.57  2003/06/23 09:42:09  mattias
  fixes for debugging lazarus

  Revision 1.56  2003/06/02 21:37:30  mattias
  fixed debugger stop

  Revision 1.55  2003/05/31 10:07:33  mattias
  changed projects forms into components

  Revision 1.54  2003/05/30 08:10:52  mattias
  added try except to Application.Run, message on changing debugger items during compile

  Revision 1.53  2003/05/18 10:42:58  mattias
  implemented deleting empty submenus

  Revision 1.52  2003/04/29 19:00:43  mattias
  added package gtkopengl

  Revision 1.51  2003/04/20 07:36:29  mattias
  fixed loading form name

  Revision 1.50  2003/04/11 10:31:57  mattias
  added Sender to Application OnKeyDownHandler

  Revision 1.49  2003/04/11 10:23:23  mattias
  added Application OnKeyDownHandler

  Revision 1.48  2003/04/11 09:32:20  mattias
  added some help stuff

  Revision 1.47  2003/03/11 07:46:43  mattias
  more localization for gtk- and win32-interface and lcl

  Revision 1.46  2003/01/13 20:39:19  mattias
  changed the no interfaceobject message

  Revision 1.45  2003/01/04 11:58:32  mattias
  added Windows menu to IDE

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
