(******************************************************************************
                                   TApplication
 ******************************************************************************)


{------------------------------------------------------------------------------}
{       TApplication Constructor                                               }
{------------------------------------------------------------------------------}
constructor TApplication.Create(AOwner: TComponent);
begin
  FTerminate := False;
  FMainForm := nil;
  FMouseControl := nil;
  FHandle := 0;
  FList := nil;
  FOnIdle := nil;
  FIcon := nil;
   
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
  FIcon.Free;
  FIcon:=nil;
  FList.Free;
  inherited Destroy;
end;

{------------------------------------------------------------------------------
       TApplication BringToFront
------------------------------------------------------------------------------}
procedure TApplication.BringToFront;
begin
  CNSendMessage(LM_BRINGTOFRONT,Self,nil);
end;

{------------------------------------------------------------------------------}
{       TApplication DefaultOnClick                                            }
{------------------------------------------------------------------------------}
procedure TApplication.DefaultOnClick(Sender : TObject);
begin
  TForm(TButton(Sender).Parent).ModalResult := TButton(Sender).ModalResult;
end;

{------------------------------------------------------------------------------}
{       TApplication Messagebox     }
{------------------------------------------------------------------------------}
function TApplication.MessageBox(Text, Caption : PChar; Flags : Longint) : Integer;
var
  DlgType : TMsgDlgType;
  Buttons : TMsgDlgButtons;
begin
//This uses TMessageBox class in MessageDialogs.inc
if (Flags and MB_RETRYCANCEL) = MB_RETRYCANCEL then
   Buttons := [mbREtry, mbCancel]
else
if (Flags and MB_YESNO) = MB_YESNO then
   Buttons := [mbYes, mbNo]
else
if (Flags and MB_YESNOCANCEL) = MB_YESNOCANCEL then
   Buttons := [mbYes, mbNo, mbCancel]
   else
if (Flags and MB_ABORTRETRYIGNORE) = MB_ABORTRETRYIGNORE then
   Buttons := [mbAbort, mbRetry, mbIgnore]
   else
if (Flags and MB_OKCANCEL) = MB_OKCANCEL then
   Buttons := [mbOK,mbCancel]
   else
if (Flags and MB_OK) = MB_OK then
   Buttons := [mbOK]
   else
   Buttons := [mbOK];

   
if (Flags and MB_ICONQUESTION) = MB_ICONQUESTION then
    DlgTYpe := mtConfirmation
    else
if (Flags and MB_ICONINFORMATION) = MB_ICONINFORMATION then
    DlgTYpe := mtInformation
    else
if (Flags and MB_ICONERROR) = MB_ICONERROR then
    DlgTYpe := mtError
    else
if (Flags and MB_ICONWARNING) = MB_ICONWARNING then
    DlgTYpe := mtWarning
    else
    DlgTYpe := mtCustom;


  Result := MessageDlg(Caption,Text,DlgType,Buttons,0);

end;

{------------------------------------------------------------------------------}
{       TApplication GetExename                                                }
{------------------------------------------------------------------------------}
Function TApplication.GetEXEName: String;
Begin
  Result := ParamStr(0);
end;

{------------------------------------------------------------------------------}
{       TApplication Notification  "Performs Application Level Operations"     }
{------------------------------------------------------------------------------}
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
end;

{------------------------------------------------------------------------------}
{       TApplication ProcesssMessage  "Process a message (MSg)"     }
{------------------------------------------------------------------------------}
(*
Function TApplication.ProcessMessage(Var Msg : TMsg) : Boolean;
Begin
//TODO: Finish TApplication.ProcessMessage(msg : TMsg);
Result := False;
if PeekMessage(Msg, 0,0,0,PM_REMOVE) then
  Begin
  Result := true;
  end;
end;
*)

{------------------------------------------------------------------------------}
{       TApplication ProcesssMessages  "Enter the messageloop and process until empty"     }
{------------------------------------------------------------------------------}
procedure TApplication.ProcessMessages;
begin
  InterfaceObject.HandleEvents;
end;

{------------------------------------------------------------------------------}
{       TApplication HintMouseMEssage     }
{------------------------------------------------------------------------------}
procedure TApplication.HintMouseMessage(Control : TControl; var Message : TLMessage);
begin
//TODO: Needs to be finished
end;


{------------------------------------------------------------------------------}
{       TApplication Initialize                                                }
{       Makes a call to the coponent engine to provide any initialization that }
{       needs to occur.                                                        }
{------------------------------------------------------------------------------}
procedure TApplication.Initialize;
begin
  if AnsiCompareText(InterfaceObject.Classname,'TINTERFACEBASE')=0 then begin
    writeln('ERROR: no widget interface object. Did u compile an abstract LCL?');
    raise Exception.Create(
      'No widget interface object. Did u compile an abstract LCL?');
  end;
  InterfaceObject.Init;
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
    if CaptureControl = nil
    then IsOther := FMouseControl <> nil
    else IsOther := FMouseControl = CaptureControl;
    
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
  if Done then
    // wait till something happens
    InterfaceObject.WaitMessage;
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
begin
  if FMainForm<>nil then
    Result:=FMainForm.Caption
  else
    Result:='';
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
  Idle;
end;

{------------------------------------------------------------------------------}
{       TApplication Run                                                       }
{       MainForm is loaded and control is passed to event processor.           }
{------------------------------------------------------------------------------}
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
procedure TApplication.wndproc(var Message : TLMessage);
begin
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

{------------------------------------------------------------------------------}
{       TApplication CreateForm                                                }
{       Create a Form instance and sets the pointer to the internal form       }
{       variable and loads the form into the application forms list            }
{------------------------------------------------------------------------------}
procedure TApplication.CreateForm(NewForm : TFormClass; var ref);
var ok: boolean;
begin
  ok:=false;
  try
    TForm(ref) := NewForm.Create(Self);
    ok:=true;
  finally
    if not ok then TObject(ref):=nil;
  end;

  if FMainForm = nil then begin
    FMainForm := TForm(ref);
  end else begin
    if not assigned(FList) then
      FList := TList.Create;
    FList.Add(TForm(ref));
  end;
end;

{ =============================================================================

  $Log$
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
