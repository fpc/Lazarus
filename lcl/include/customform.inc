// included by forms.pp

{******************************************************************************
                                     TCustomForm
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

{ $DEFINE CHECK_POSITION}

{------------------------------------------------------------------------------}
{       TCustomForm ClientWndProc                                              }
{------------------------------------------------------------------------------}
Procedure TCustomForm.ClientWndProc(var Message: TLMessage);

  procedure Default;
    begin
      {
      with Message do
      Result := CallWindowProc(FDefClientProc, ClientHandle, Msg, wParam, lParam);
     }
    end;

begin
  with Message do
    case Msg of
      LM_NCHITTEST:
        begin
          Default;
          if Result = HTCLIENT then Result := HTTRANSPARENT;
        end;
      LM_ERASEBKGND:
        begin
          // Not sure if this will work real good.
          Canvas.FillRect(ClientRect);
          Result := 1;
        end;
    else
      Default;
    end;
end;

procedure TCustomForm.CloseModal;
var
  CloseAction: TCloseAction;
begin
  try
    CloseAction := caNone;
    if CloseQuery then
    begin
      CloseAction := caHide;
      DoClose(CloseAction);
    end;
    case CloseAction of
      caNone: ModalResult := 0;
      //caFree: Release;
    end;
  except
    ModalResult := 0;
    Application.HandleException(Self);
  end;
end;

{------------------------------------------------------------------------------
  Method: TCustomForm.BeforeDestruction
  Params:  None
  Returns: Nothing

  Gets called before the destruction of the object
 ------------------------------------------------------------------------------}
procedure TCustomForm.BeforeDestruction;
begin
  DoDestroy;
  inherited BeforeDestruction;
end;

{------------------------------------------------------------------------------
  Method: TCustomForm.Destroy
  Params:  None
  Returns: Nothing

  Destructor for the class.
 ------------------------------------------------------------------------------}
destructor TCustomForm.Destroy;
begin
  //writeln('[TCustomForm.Destroy] A ',Name,':',ClassName);
  Assert(False, Format('Trace: [TCustomForm.Destroy] %s', [ClassName]));
  FMenu.Free;
  FMenu:=nil;
  FCanvas.Free;
  FCanvas:=nil;
  FIcon.Free;
  FIcon:=nil;
  //writeln('[TCustomForm.Destroy] B ',Name,':',ClassName);
  inherited Destroy;
  //writeln('[TCustomForm.Destroy] END ',Name,':',ClassName);
end;

{------------------------------------------------------------------------------
  Method: TCustomForm.FocusControl
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
Procedure TCustomForm.FocusControl(WinControl : TWinControl);
Begin
  FActiveControl := WinControl;
  if HandleAllocated and Visible then
    LCLLinux.SetFocus(WinControl.Handle);
End;


{------------------------------------------------------------------------------
  Method: TCustomForm.Notification
 ------------------------------------------------------------------------------}
Procedure TCustomForm.Notification(AComponent : TComponent;
  Operation : TOperation);
Begin
  inherited Notification(AComponent,Operation);
  if FDesigner <> nil then FDesigner.Notification(AComponent,Operation);
End;

{------------------------------------------------------------------------------
  Method: TCustomForm.IconChanged
 ------------------------------------------------------------------------------}
procedure TCustomForm.IconChanged(Sender: TObject);
begin
  if HandleAllocated {and (BorderStyle<>bsDialog)} then
    CNSendMessage(LM_SETFORMICON,Self,Pointer(GetIconHandle));
end;

{------------------------------------------------------------------------------
  Method: TCustomForm.SetIcon
  Params: the new icon
 ------------------------------------------------------------------------------}
procedure TCustomForm.SetIcon(AValue: TIcon);
begin
  if FIcon=nil then begin
    FIcon:=TIcon.Create;
    FIcon.OnChange := @IconChanged;
  end;
  FIcon.Assign(AValue);
end;

{------------------------------------------------------------------------------
  Method: TCustomForm.IsIconStored
  Returns: if form icon should be stored in the stream
 ------------------------------------------------------------------------------}
function TCustomForm.IsIconStored: Boolean;
begin
  Result := IsForm and (Icon<>nil);
end;

{------------------------------------------------------------------------------
  Method: TCustomForm.GetIconHandle
  Returns: handle of form icon 
 ------------------------------------------------------------------------------}
function TCustomForm.GetIconHandle: HICON;
begin
//writeln('[TCustomForm.GetIconHandle] ',ClassName,' ',FIcon<>nil);
  if (FIcon<>nil) and not Icon.Empty then
    Result := FIcon.Handle
  else
    Result := Application.GetIconHandle;
end;

{------------------------------------------------------------------------------
  Method: TCustomForm.SetFocus
 ------------------------------------------------------------------------------}
Procedure TCustomForm.SetFocus;
Begin
//writeln('[TCustomForm.SetFocus] A ',Classname);
//if not(Visible and Enabled) then Exit;
  CNSendMessage(LM_SETFOCUS,Self,nil);
//writeln('[TCustomForm.SetFocus] END ',Classname);
end;

{------------------------------------------------------------------------------}
{       TCustomForm SetVisible                                                 }
{------------------------------------------------------------------------------}
Procedure TCustomForm.SetVisible(Value : boolean);
Begin
//writeln('[TCustomForm.SetVisible] START ',ClassName,' ',Value);
  if fsCreating in FFormState then
    if Value then
      Include(FFormState, fsVisible)
    else
      Exclude(FFormState, fsVisible)
  else
  begin
    inherited Visible := Value;
  end;
//writeln('[TCustomForm.SetVisible] END ',ClassName,' ',Visible);
end;

{------------------------------------------------------------------------------
  Method: TCustomForm.WMDestroy
  Params:   Msg: The destroy message
  Returns:  nothing

  Destroy event handler.
 ------------------------------------------------------------------------------}
procedure TCustomForm.WMDestroy(var Message: TLMDestroy);
begin
  Assert(False, Format('Trace: [TCustomForm.LMDestroy] %s', [ClassName]));
  
  // First set FHandle to 0, the window doesn't exist anymore.
  inherited WMDestroy(Message);
  Free;
end;

{------------------------------------------------------------------------------
  Method: TCustomForm.WMShowWindow
  Params:   Msg: The showwindow message
  Returns:  nothing

  ShowWindow event handler.
 ------------------------------------------------------------------------------}
procedure TCustomForm.WMShowWindow(var message: TLMShowWindow);
const
  SHOW_TEXT: array[Boolean] of string = ('Hide', 'Show');
begin
  Assert(False, Format('Trace: [TCustomForm.LMShowWindow] %s %s', [SHOW_TEXT[Message.Show], ClassName]));

  Include(FFormState, fsShowing);
  try
    if Message.Show 
    then DoShow
    else DoHide;
  finally
    Exclude(FFormState, fsShowing);
  end;
end;

{------------------------------------------------------------------------------
  Method: TCustomForm.WMActivate
  Params:   Msg: When the form is Activated
  Returns:  nothing

  Activate event handler.
 ------------------------------------------------------------------------------}
procedure TCustomForm.WMActivate(var Message : TLMActivate);
begin
  Activate;
end;

{------------------------------------------------------------------------------
  Method: TCustomForm.WMDeactivate
  Params: Msg: When the form is deactivated (loses focus within application)
  Returns: nothing

  Form deactivation (losing focus within application)  event handler.
 ------------------------------------------------------------------------------}
procedure TCustomForm.WMDeactivate(var Message : TLMActivate);
begin
  Deactivate;
end;

{------------------------------------------------------------------------------
  Method: TCustomForm.Activate
  Params: none
  Returns:  nothing

  Activation form methode event handler.
 ------------------------------------------------------------------------------}
procedure TCustomForm.Activate;
begin
  if Assigned(FOnActivate) then FOnActivate(Self);
end;

{------------------------------------------------------------------------------
  Method: TCustomForm.Deactivate
  Params: none
  Returns: nothing

  Form deactivation (losing focus within application)  event handler.
 ------------------------------------------------------------------------------}
procedure TCustomForm.Deactivate;
begin
  if Assigned(FOnDeactivate) then FOnDeactivate(Self);
end;

{------------------------------------------------------------------------------
  Method: TCustomForm.WMPaint
  Params:   Msg: The paint message
  Returns:  nothing

  Paint event handler.
 ------------------------------------------------------------------------------}
procedure TCustomForm.WMPaint(var Message: TLMPaint);
begin
//writeln('[TCustomForm.WMPaint] ',Name,':',ClassName);
  Assert(False, Format('Trace: [TCustomForm.LMPaint] %s', [ClassName]));
  
  Include(FControlState, csCustomPaint);
  try
    ControlState := ControlState + [csCustomPaint];
    inherited WMPaint(Message);
    ControlState := ControlState - [csCustomPaint];
  finally
    Exclude(FControlState, csCustomPaint);
  end;
//writeln('[TCustomForm.WMPaint] END ',Name,':',ClassName);
end;                          


{------------------------------------------------------------------------------
  Method: TCustomForm.WMSize
  Params:   Msg: The Size message
  Returns:  nothing

  Resize event handler.
 ------------------------------------------------------------------------------}
procedure TCustomForm.WMSize(var Message: TLMSize);
begin
{$IFDEF CHECK_POSITION}
Writeln('[TCustomForm.WMSize] Name=',Name,' Class=',ClassName,' Message.Width=',Message.Width,' Message.Height=',Message.Height);
{$ENDIF}
  Assert(False, 'Trace:WMSIZE in TCustomForm');
  if not (csDesigning in ComponentState) then
    Case Message.SizeType of
       SIZENORMAL : FWindowState := wsNormal;
       SIZEICONIC : FWIndowState := wsMinimized;
       SIZEFULLSCREEN : FWindowstate := wsMaximized;
      end;

  inherited WMSize(Message);
End;

{------------------------------------------------------------------------------
  Method: TCustomForm.DoCreate
  Params:   none
  Returns:  nothing

  Calls user handler  
 ------------------------------------------------------------------------------}
procedure TCustomForm.DoCreate;
begin
  BeginUpdateBounds;
  if Assigned(FOnCreate) then FOnCreate(Self);
  EndUpdateBounds;
end;

{------------------------------------------------------------------------------
  Method: TCustomForm.DoClose
  Params:   none
  Returns:  nothing

  Calls user handler  
 ------------------------------------------------------------------------------}
procedure TCustomForm.DoClose(var Action: TCloseAction);
begin
  if Assigned(FOnClose) then FOnClose(Self, Action);
end;

{------------------------------------------------------------------------------
  Method: TCustomForm.DoDestroy
  Params:   none
  Returns:  nothing

  Calls user handler  
 ------------------------------------------------------------------------------}
procedure TCustomForm.DoDestroy;
begin
  if Assigned(FOnDestroy) then FOnDestroy(Self);
end;

{------------------------------------------------------------------------------
  Method: TCustomForm.DoHide
  Params:   none
  Returns:  nothing

  Calls user handler  
 ------------------------------------------------------------------------------}
procedure TCustomForm.DoHide;
begin
  if Assigned(FOnHide) then FOnHide(Self);
end;

{------------------------------------------------------------------------------
  Method: TCustomForm.DoShow
  Params:   none
  Returns:  nothing

  Calls user handler  
 ------------------------------------------------------------------------------}
procedure TCustomForm.DoShow;
begin
  if Assigned(FOnShow) then FOnShow(Self);
end;

{------------------------------------------------------------------------------
  Method: TCustomForm.GetChildren
  Params:   Proc - see fcl/inc/writer.inc
            Root
  Returns:  nothing

  Adds component to children list which have no parent.
    (TWinControl only lists components with parents)
 ------------------------------------------------------------------------------}
procedure TCustomForm.GetChildren(Proc: TGetChildProc; Root: TComponent);
var
  I: Integer;
  OwnedComponent: TComponent;
begin
  inherited GetChildren(Proc, Root);
  if Root = Self then
    for I := 0 to ComponentCount - 1 do begin
      OwnedComponent := Components[I];
      if OwnedComponent.HasParent = False
        then Proc(OwnedComponent);
    end;
end;

{------------------------------------------------------------------------------
  Method: TCustomForm.Paint
  Params:   none
  Returns:  nothing

  Calls user handler  
 ------------------------------------------------------------------------------}
Procedure TCustomForm.Paint;                                            
begin                                                                                             
  if Assigned (FOnPaint) and not(Isresizing) then FOnPaint(Self);
end;                                                                                              

{------------------------------------------------------------------------------
  Method: TCustomForm.PaintWindow
  Params:   none
  Returns:  nothing

  Calls user handler
 ------------------------------------------------------------------------------}
Procedure TCustomForm.PaintWindow(DC : Hdc);
begin
//  FCanvas.Lock;
  try
    FCanvas.Handle := DC;
//writeln('[TCustomForm.PaintWindow] ',ClassName,' DC=',HexStr(DC,8),'  ',HexStr(FCanvas.Handle,8));
    try
      if FDesigner <> nil then FDesigner.PaintGrid else Paint;
    finally
      FCanvas.Handle := 0;
    end;
  finally
//    FCanvas.Unlock;
  end;
end;


{------------------------------------------------------------------------------
  Method: TCustomForm.RequestAlign
  Params:   none
  Returns:  nothing

  Calls user handler
 ------------------------------------------------------------------------------}
Procedure TCustomForm.RequestAlign;
Begin
  if Parent = nil then begin
    //Screen.AlignForm(Self);
  end
  else
  inherited RequestAlign;
end;

{------------------------------------------------------------------------------}
{       TCustomForm SetDesigner                                          }
{------------------------------------------------------------------------------}
Procedure TCustomForm.SetDesigner(Value : TIDesigner);
Begin
  FDesigner := Value;
end;

{------------------------------------------------------------------------------}
{       TCustomForm ValidateRename                                             }
{  if AComponent is nil, then the name of Self is changed                      }
{------------------------------------------------------------------------------}
procedure TCustomForm.ValidateRename(AComponent: TComponent;
  const CurName, NewName: String);
begin
  inherited ValidateRename(AComponent, CurName, NewName);
  if FDesigner <> nil then
    FDesigner.ValidateRename(AComponent, CurName, NewName);
end;

{------------------------------------------------------------------------------}
{       TCustomForm WndProc                                                    }
{------------------------------------------------------------------------------}
procedure TCustomForm.WndProc(Var TheMessage : TLMessage);
var
  FocusHandle : HWND;
//  SaveIndex : Integer;
  MenuItem : TMenuItem;
//  Canvas2 : TCanvas;
//  DC: HDC;

begin
// Assert(False, 'Trace:-----------------IN TCUSTOMFORM WNDPROC-------------------');
  with TheMessage do
  case Msg of
    LM_ACTIVATE, LM_SETFOCUS, LM_KILLFOCUS:
      begin
        if not FocusMessages then Exit;
        if (MSg = LM_SetFocus) and not (csDesigning in ComponentState) 
        then begin
          FocusHandle := 0;
          if FormStyle = fsMDIFORM 
          then begin
          end
          else begin
            if (FActiveControl <> nil) and (FActiveControl <> Self) 
            then FocusHandle := FActiveControl.Handle;
          end;
          if FocusHandle <> 0 
          then begin
          //writeln('[TCustomForm.WndPRoc] A ',FActiveControl.ClassName);
            LCLLinux.SetFocus(FocusHandle);
            Exit;
          end;
          TheMessage.Result:=0;
        end;
      end;
    CM_EXIT:  
      begin
        //TODO: deal with docking                if HostDockSite <> nil then DeActivate;
      end;
    CM_ENTER:  
      begin
        //TODO: Deal with docking                if HostDockSite <> nil then Activate;
      end;
    LM_WINDOWPOSCHANGING:
      if ([csLoading, csDesigning] * ComponentState = [csLoading]) 
      then begin
        if  (Position in [poDefault, poDefaultPosOnly]) 
        and (WindowState <> wsMaximized) 
        then
          with PWindowPos(TheMessage.lParam)^ do
            flags := flags or SWP_NOMOVE;
        
        if  (Position in [poDefault, poDefaultSizeOnly]) 
        and (BorderStyle in [bsSizeable, bsSizeToolWin]) 
        then
          with PWindowPos(TheMessage.lParam)^ do
            flags := flags or SWP_NOSIZE;
      end;
    LM_DRAWITEM:
      with PDrawItemStruct(TheMessage.LParam)^ do
      begin
        if (CtlType = ODT_MENU) and Assigned(Menu) 
        then begin
          MenuItem := Menu.FindItem(itemID, fkCommand);
          if MenuItem <> nil 
          then begin
            {Canvas2 := TControlCanvas.Create;
            with Canvas2 do
            try
              SaveIndex := SaveDC(hDC);
              try
                Handle := hDC;
                Font := Screen.MenuFont;
                Menus.DrawMenuItem(MenuItem, Canvas2, rcItem,
                  TOwnerDrawState(LongRec(itemState).Lo));
              finally
                Handle := 0;
riteln('[TCustomForm.WndPRoc] 1');
                RestoreDC(hDC, SaveIndex)
              end;
            finally
              Free;
            end;
            }
            Exit;
          end;
        end;
      end;
{
    LM_MEASUREITEM:
      with PMeasureItemStruct(Message.LParam)^ do
      begin 
        if (CtlType = ODT_MENU) and Assigned(Menu) 
        then begin
          MenuItem := Menu.FindItem(itemID, fkCommand);
          if MenuItem <> nil 
          then begin
            DC := GetWindowDC(Handle);
            try
              Canvas2 := TControlCanvas.Create;
              with Canvas2 do
              try
                SaveIndex := SaveDC(DC);
                try
                  Handle := DC;
                  Font := Screen.MenuFont;
                  TMenuItemAccess(MenuItem).MeasureItem(Canvas2,
                    Integer(itemWidth), Integer(itemHeight));
                finally
                  Handle := 0;
                  RestoreDC(DC, SaveIndex);
                end;
              finally
                Canvas2.Free;
              end;
            finally
              ReleaseDC(Handle, DC);
            end;
          
            Exit;
          end;
        end;
      end;
}
    LM_SHOWWINDOW:  
      begin
        Assert(False, 'Trace:LM_SHOWWINDOW RECEIVED!!!!!!!!!!!');
      end;
    end;
  inherited WndProc(TheMessage);
end;


{------------------------------------------------------------------------------}
{       TCustomForm SetMenu                                                      }
{------------------------------------------------------------------------------}
Procedure TCustomForm.SetMenu(Value : TMainMenu);
Begin
  //TODO: Finish SETMenu
  if FMenu=Value then exit;
  if FMenu<>nil then FMenu.Parent:=nil;
  FMenu := Value;
  if FMenu<>nil then FMenu.Parent:=Self;
end;

{------------------------------------------------------------------------------}
{       TCustomForm SetBorderStyle                                                      }
{------------------------------------------------------------------------------}
Procedure TCustomForm.SetBorderStyle(Value : TFormBorderStyle);
Begin
  //TODO: Finish SETBORDERSTYLE
  FBorderStyle := Value;
end;

{------------------------------------------------------------------------------}
{       TCustomForm UpdateWindowState                                                      }
{------------------------------------------------------------------------------}
Procedure TCustomForm.UpdateWindowState;
Begin
//TODO: Finish UpdateWindowState
  Assert(False, 'Trace:TODO: [TCustomForm.UpdateWindowState]');
end;

{------------------------------------------------------------------------------}
{       TCustomForm SetWindowState                                                      }
{------------------------------------------------------------------------------}
Procedure TCustomForm.SetWindowState(Value : TWindowState);
Begin
//TODO: Finish SETWINDOWSTATE
FWindowState := Value;
  Assert(False, 'Trace:TODO: [TCustomForm.SetWindowState]');
end;

{------------------------------------------------------------------------------}
{       TCustomForm SetActiveControl                                                      }
{------------------------------------------------------------------------------}
Procedure TCustomForm.SetActiveControl(Value : TWinControl);
Begin
//TODO: Finish SETACTIVECONTROL
  FActiveControl := Value;
  Assert(False, 'Trace:TODO: [TCustomForm.SetActiveCOntrol]');
end;

{------------------------------------------------------------------------------}
{       TCustomForm SetFormStyle                                                      }
{------------------------------------------------------------------------------}
Procedure TCustomForm.SetFormStyle(Value : TFormStyle);
Begin
  //TODO: Finish SETFORMSTYLE
  FFormStyle := Value;
  Assert(False, 'Trace:TODO: [TCustomForm.SetFormStyle]');
end;

{------------------------------------------------------------------------------}
{       TCustomForm SetPosition                                                      }
{------------------------------------------------------------------------------}
procedure TCustomForm.SetPosition(Value : TPosition);
begin
  if Value <> FPosition then begin
    FPosition := Value;
    UpdateControlState;
  end;    
end;

{------------------------------------------------------------------------------}
{       TCustomForm Constructor                                                      }
{------------------------------------------------------------------------------}
constructor TCustomForm.Create(AOwner : TComponent);
begin
//writeln('[TCustomForm.Create] A Class=',Classname);
  try
    CreateNew(AOwner, 1);
//writeln('[TCustomForm.Create] B Class=',Classname);
    if (ClassType <> TForm) and not (csDesigning in ComponentState) then
    begin
      Include(FFormState, fsCreating);
      try
//writeln('[TCustomForm.Create] C Class=',Classname);
        if not InitResourceComponent(Self, TForm) then begin
          //writeln('[TCustomForm.Create] Resource '''+ClassName+''' not found');
          //Writeln('This is for information purposes only.   This is not critical at this time.');
          // MG:  Ignoring is best at the moment. (Delphi raises an exception.)
        end;
//writeln('[TCustomForm.Create] D Class=',Classname);
        DoCreate;
//writeln('[TCustomForm.Create] E Class=',Classname);
      finally
        Exclude(FFormState, fsCreating);
      end;
    end;
  finally
  end;
//writeln('[TCustomForm.Create] END Class=',Classname);
end;

constructor TCustomForm.CreateNew(AOwner: TComponent; Num : Integer);
Begin
//writeln('[TCustomForm.CreateNew] Class=',Classname);
  FBorderStyle:= bsSizeable;
  inherited Create(AOwner);
  fCompStyle:= csForm;

  FFormState := [];
  FMenu	:= nil;	

  ControlStyle := ControlStyle + [csAcceptsControls, csCaptureMouse,
                                  csClickEvents, csSetCaption, csDoubleClicks];
  Left := 0;
  Top := 0;
  Width := 320;
  Height := 240;
  Visible := False;
  ParentColor := False;
  ParentFont := False;
  Ctl3D := True;
//  FBorderIcons := [biSystemMenu, biMinimize, biMaximize];
  FBorderStyle := bsSizeable;
  FWindowState := wsNormal;
//  FDefaultMonitor := dmActiveForm;
  FIcon := nil;
//  FInCMParentBiDiModeChanged := False;
  {apply a drawing surface}
  FCanvas := TControlCanvas.Create;
  FCanvas.Control := Self;
  FKeyPreview :=  False;
  Color := clBtnface;
//  FPixelsPerInch := Screen.PixelsPerInch;
//  FPrintScale := poProportional;
//  FloatingDockSiteClass := TWinControlClass(ClassType);
  Screen.AddForm(Self);
End;

{------------------------------------------------------------------------------}
{  TCustomForm CreateParams                                                    }
{------------------------------------------------------------------------------}
procedure TCustomForm.CreateParams(var Params : TCreateParams);
begin
  inherited CreateParams(Params);
  with Params do
  begin
    if (Parent = nil) {and (ParentWindow = 0)} then
    begin
//    WndParent := Application.Handle;
      { TODO : No application handle }
      Style := Style and not Cardinal(WS_CHILD or WS_GROUP or WS_TABSTOP);
    end;
  end;
end;


{------------------------------------------------------------------------------}
{       TCustomForm Method Close                                               }
{------------------------------------------------------------------------------}
Procedure TCustomForm.Close;
var
  CloseAction: TCloseAction;
begin
  Assert(False, Format('Trace:[TCustomForm.Close] %s', [ClassName]));
  CloseAction := caHide;
  DoClose(CloseAction);
  if CloseAction <> caNone 
  then begin
    if Application.MainForm = Self 
    then Application.Terminate
    else begin
      case CloseAction of
        caHide : 
          begin
        	  if Visible 
        	  then Assert(False, 'Trace:Performing Hide')
        	  else Assert(False, 'Trace:They say it is not visible !!!');
        	end;  
      	caMinimize : 
      	  begin
      	    Assert(False, 'Trace:Performing minimize');
      	  end;
      else 
        Assert(False, 'Trace:Performing free');
      end;	
      case CloseAction of
        caHide: Hide;
        caMinimize: WindowState := wsMinimized;
      else 
        {Release =}Free;
      end;
    end;  
  end;

end;

{------------------------------------------------------------------------------}
{       TCustomForm Method CloseQuery                                          }
{------------------------------------------------------------------------------}
function TCustomForm.CloseQuery : boolean;
//var i : integer;
begin
  { Query children forms whether we can close }
  if FormStyle = fsMDIForm then begin
{    for i:= 0 to MDIChildCount - 1 do begin
      if not MDIChildren[i].CloseQuery then begin
        Result:= false;
	Exit;
      end;	
    end;}
  end;    
  Result:= true;
  if Assigned(FOnCloseQuery) then FOnCloseQuery(Self, Result);
  if Result then Assert(False, 'Trace:CloseQuery returns true')
  else Assert(False, 'Trace:CloseQuery returns false');
end;

{------------------------------------------------------------------------------}
{       TCustomForm Method WMCloseQuery                                        }
{------------------------------------------------------------------------------}
procedure TCustomForm.WMCloseQuery(var Message : TLMessage);
begin
  if CloseQuery then Close;
  { Always return 0, because we destroy the window ourselves }
  Message.Result:= 0;
end;

{------------------------------------------------------------------------------}
{       TCustomForm Method Hide                                                }
{------------------------------------------------------------------------------}
procedure TCustomForm.Hide;
begin
  Visible := False;
end;

{------------------------------------------------------------------------------}
{       TCustomForm Method GetCanvas  "Returns the drawing surface"            }
{------------------------------------------------------------------------------}
function TCustomForm.GetCanvas: TControlCanvas;
begin
  result := FCanvas;
end;

{------------------------------------------------------------------------------}
{       TCustomForm Method IsForm                                              }
{------------------------------------------------------------------------------}
function TCustomForm.IsForm: Boolean;
begin
//TODO:
Result := True;
end;

{------------------------------------------------------------------------------}
{       TCustomForm Method SetFocusedControl                                              }
{------------------------------------------------------------------------------}
function TCustomForm.SetFocusedControl(Control : TWinControl): Boolean;
begin
//TODO: SetFocusedControl
  Result := True;
end;

{------------------------------------------------------------------------------}
{       TCustomForm Method WantChildKey            }
{------------------------------------------------------------------------------}
function TCustomForm.WantChildKey(Child : TControl; 
  var Message : TLMessage):Boolean;
begin
  Result := False;
end;

{------------------------------------------------------------------------------
  Method:  TCustomForm.CreateWnd
  Params:  None
  Returns: Nothing

  Creates the interface object.
 ------------------------------------------------------------------------------}
procedure TCustomForm.CreateWnd;
begin
//writeln('TCustomForm.CreateWnd START ',ClassName);
  inherited CreateWnd;
  CNSendMessage(LM_SETFORMICON, Self, Pointer(GetIconHandle));

  Assert(False, 'Trace:[TCustomForm.CreateWnd] FMenu.HandleNeeded');  
  if FMenu <> nil then FMenu.HandleNeeded;
//writeln('TCustomForm.CreateWnd END ',ClassName);
end;

{------------------------------------------------------------------------------
  Method:  TCustomForm.UpdateShowing
  Params:  None
  Returns: Nothing

  Here the initial form left and top are determined.
 ------------------------------------------------------------------------------}
procedure TCustomForm.UpdateShowing;
var 
  X, Y : integer;
begin
  {$IFDEF CHECK_POSITION}
  writeln('[TCustomForm.UpdateShowing] A  Class=',Name,':',Classname,' Pos=',Left,',',Top,' Visible=',Visible);
  {$ENDIF}
  { If the the form is about to show, calculate its metrics }
  if Visible then begin
    // first make sure X and Y are assigned
    X := Left;
    Y := Top;
    
    if  (Position = poMainFormCenter) 
    and (FormStyle = fsMDIChild) 
    and (Self <> Application.Mainform) 
    then begin
      X:= (Application.Mainform.ClientWidth - Width) div 2;
      Y:= (Application.Mainform.ClientHeight - Height) div 2;
    end 
    else begin
      case Position of
        //TODO:poDefault, poDefaultPosOnly, poDefaultSizeOnly       
        poScreenCenter, poDesktopCenter : begin
          X:= (Screen.Width - Width) div 2;
          Y:= (Screen.Height - Height) div 2;
        end;
        poMainFormCenter : if (Self <> Application.MainForm) then begin
          X:= ((Application.MainForm.Width - Width) div 2) + Application.MainForm.Left;
          Y:= ((Application.MainForm.Height - Height) div 2) + Application.MainForm.Top;
        end;
        poOwnerFormCenter : if (Owner is TCustomForm) then begin
          X:= ((TCustomForm(Owner).Width - Width) div 2) + TCustomForm(Owner).Left;
          Y:= ((TCustomForm(Owner).Height - Height) div 2) + TCustomForm(Owner).Top;
        end;
      end;
    end;  
    if X < 0 then X := 0;
    if Y < 0 then Y := 0;
    SetBounds(X, Y, Width, Height);
  end;
  {$IFDEF CHECK_POSITION}
  writeln('[TCustomForm.UpdateShowing] B  ',Name,':',Classname,' Pos=',Left,',',Top);
  {$ENDIF}
  inherited UpdateShowing;
  {$IFDEF CHECK_POSITION}
  writeln('[TCustomForm.UpdateShowing] END  ',Name,':',Classname,' Pos=',Left,',',Top);
  {$ENDIF}
end;

{------------------------------------------------------------------------------}
{  TCustomForm ShowModal                                                       }
{------------------------------------------------------------------------------}
Function TCustomForm.ShowModal : Integer;
{var
  WindowList: Pointer;
  SaveFocusCount: Integer;
  SaveCursor: TCursor;
  SaveCount: Integer;
  ActiveWindow: HWnd;}
begin
  //writeln('[TCustomForm.ShowModal] START ',Classname);
  if Visible or not Enabled or (fsModal in FFormState) or
    (FormStyle = fsMDIChild) then
    raise EInvalidOperation.Create('TCustomForm.ShowModal impossible');
  { TODO : This has to be changed by WM_VISIBLECHANGED. Implement appropriate callback !!! }
  //Kill capture when opening another dialog
  if GetCapture <> 0 then SendMessage(GetCapture,LM_CANCELMODE,0,0);
  ReleaseCapture;

  Include(FFormState, fsModal);
  {ActiveWindow := GetActiveWindow;
  SaveFocusCount := FocusCount;
  Screen.FSaveFocusedList.Insert(0, Screen.FFocusedForm);
  Screen.FFocusedForm := Self;
  SaveCursor := Screen.Cursor;
  Screen.Cursor := crDefault;
  SaveCount := Screen.FCursorCount;
  WindowList := DisableTaskWindows(0);}
  ModalResult := 0;

  try
    Show;
    try
      CNSendMessage(LM_SHOWMODAL, Self, nil);
      repeat
        { Delphi calls Application.HandleMessage
          But HandleMessage processes all pending events and then calls idle,
          which will wait for new messages. Under Win32 there always a next
          message, so it works there. The LCL is OS independent, and so it uses
          a better way: }
        InterfaceObject.HandleEvents; // process all events
        if Application.FTerminate then
          ModalResult := mrCancel
        else if ModalResult <> 0 then
          CloseModal;
        if ModalResult<>0 then break;
        Application.Idle;
      until ModalResult <> 0;
      
      Result := ModalResult;
      //if GetActiveWindow <> Handle then ActiveWindow := 0;
    finally
      Hide;
    end;
  finally
    {if Screen.FCursorCount = SaveCount then
      Screen.Cursor := SaveCursor
    else Screen.Cursor := crDefault;
    EnableTaskWindows(WindowList);
    if Screen.FSaveFocusedList.Count > 0 then
    begin
      Screen.FFocusedForm := Screen.FSaveFocusedList.First;
      Screen.FSaveFocusedList.Remove(Screen.FFocusedForm);
    end else Screen.FFocusedForm := nil;
    if ActiveWindow <> 0 then SetActiveWindow(ActiveWindow);
    FocusCount := SaveFocusCount;}
    Exclude(FFormState, fsModal);
  end;
end;

{ =============================================================================

  $Log$
  Revision 1.57  2002/09/09 06:27:06  lazarus
  Form deactivation fixes.

  Revision 1.56  2002/09/03 20:02:01  lazarus
  Intermediate UI patch to show a bug.

  Revision 1.55  2002/09/03 11:32:49  lazarus

  Added shortcut keys to labels
  Support for alphabetically sorting the properties
  Standardize message and add shortcuts ala Kylix
  Published BorderStyle, unpublished BorderWidth
  ShowAccelChar and FocusControl
  ShowAccelChar and FocusControl for TLabel, escaped ampersands now work.

  Revision 1.54  2002/09/03 08:07:19  lazarus
  MG: image support, TScrollBox, and many other things from Andrew

  Revision 1.53  2002/08/31 11:37:09  lazarus
  MG: fixed destroying combobox

  Revision 1.52  2002/08/30 12:32:20  lazarus
  MG: MoveWindowOrgEx, Splitted FWinControls/FControls, TControl drawing, Better DesignerDrawing, ...

  Revision 1.51  2002/08/24 12:54:59  lazarus
  MG: fixed mouse capturing, OI edit focus

  Revision 1.50  2002/08/17 15:45:32  lazarus
  MG: removed ClientRectBugfix defines

  Revision 1.49  2002/07/05 09:09:20  lazarus
  MG: fixed TCustomForm.ShowModal reacting to ModalResult

  Revision 1.48  2002/06/19 19:46:09  lazarus
  MG: Form Editing: snapping, guidelines, modified on move/resize, creating components in csDesigning, ...

  Revision 1.47  2002/05/30 21:17:27  lazarus
  lcl/controls.pp

  Revision 1.46  2002/05/15 05:58:17  lazarus
  MG: added TMainMenu.Parent

  Revision 1.45  2002/05/13 15:26:13  lazarus
  MG: fixed form positioning when show, hide, show

  Revision 1.44  2002/05/10 06:05:51  lazarus
  MG: changed license to LGPL

  Revision 1.43  2002/05/09 12:41:28  lazarus
  MG: further clientrect bugfixes

  Revision 1.42  2002/04/27 18:56:50  lazarus
  MG: started component renaming

  Revision 1.41  2002/04/27 15:35:50  lazarus
  MG: fixed window shrinking

  Revision 1.40  2002/04/26 15:31:06  lazarus
  MG: made ShowModal more dlephi compatible

  Revision 1.39  2002/04/26 12:26:50  lazarus
  MG: improved clean up

  Revision 1.38  2002/03/25 17:59:20  lazarus
  GTK Cleanup
  Shane

  Revision 1.37  2002/03/18 11:44:41  lazarus
  MG: TForm.Position will now considered before creating form on 0,0

  Revision 1.36  2002/03/16 21:40:55  lazarus
  MG: reduced size+move messages between lcl and interface

  Revision 1.35  2002/03/13 22:48:16  lazarus
  Constraints implementation (first cut) and sizig - moving system rework to
  better match Delphi/Kylix way of doing things (the existing implementation
  worked by acident IMHO :-)

  Revision 1.34  2002/01/01 15:50:14  lazarus
  MG: fixed initial component aligning

  Revision 1.33  2001/12/28 15:12:02  lazarus
  MG: LM_SIZE and LM_MOVE messages are now send directly, not queued

  Revision 1.32  2001/12/20 14:41:20  lazarus
  Fixed setfocus for TComboBox and TMemo
  Shane

  Revision 1.31  2001/12/19 10:59:12  lazarus
  MG: changes for fpc 1.1

  Revision 1.30  2001/11/10 10:48:00  lazarus
  MG: fixed set formicon on invisible forms

  Revision 1.29  2001/10/16 20:01:28  lazarus
  MG: removed splashform fix, because of the unpredictable side effects

  Revision 1.28  2001/10/10 17:55:04  lazarus
  MG: fixed caret lost, gtk cleanup, bracket lvls, bookmark saving

  Revision 1.27  2001/10/07 07:28:33  lazarus
  MG: fixed setpixel and TCustomForm.OnResize event

  Revision 1.26  2001/10/03 17:34:26  lazarus
  MG: activated TCustomForm.OnCreate event

  Revision 1.24  2001/07/10 13:25:49  lazarus
  MG: repaints reduced

  Revision 1.22  2001/06/28 18:15:03  lazarus
  MG: bugfixes for destroying controls

  Revision 1.21  2001/06/26 00:08:35  lazarus
  MG: added code for form icons from Rene E. Beszon

  Revision 1.20  2001/06/14 14:57:58  lazarus
  MG: small bugfixes and less notes

  Revision 1.19  2001/05/31 13:57:28  lazarus
  MG: added environment option OpenLastProjectAtStart

  Revision 1.18  2001/03/31 13:35:23  lazarus
  MG: added non-visual-component code to IDE and LCL

  Revision 1.17  2001/03/26 14:58:31  lazarus
  MG: setwindowpos + bugfixes

  Revision 1.15  2001/03/19 14:41:56  lazarus
  MG: fixed many unreleased DC and GDIObj bugs

  Revision 1.13  2001/02/28 13:17:33  lazarus
  Added some debug code for the top,left reporting problem.
  Shane

  Revision 1.12  2001/02/06 20:59:17  lazarus
  Trying to get the last control of the last form focused when a dialog closes.
  Still working on it.
  Shane

  Revision 1.11  2001/02/02 20:13:39  lazarus
  Codecompletion changes.
  Added code to Uniteditor for code completion.

  Also, added code to gtkobject.inc so forms now get keypress events.
  Shane

  Revision 1.10  2001/02/01 16:45:20  lazarus
  Started the code completion.
  Shane

  Revision 1.9  2001/01/12 18:46:50  lazarus
  Named the speedbuttons in MAINIDE and took out some writelns.
  Shane

  Revision 1.8  2001/01/03 18:44:54  lazarus
  The Speedbutton now has a numglyphs setting.
  I started the TStringPropertyEditor

  Revision 1.7  2000/12/19 18:43:13  lazarus
  Removed IDEEDITOR.  This causes the PROJECT class to not function.
  Saving projects no longer works.

  I added TSourceNotebook and TSourceEditor.  They do all the work for saving/closing/opening units.  Somethings work but they are in early development.
  Shane

  Revision 1.6  2000/11/30 21:43:38  lazarus
  Changed TDesigner.  It's now notified when a control is added to it's CustomForm.
  It's created in main.pp when New Form is selected.

  Shane

  Revision 1.5  2000/11/21 17:33:37  lazarus
  Added TCustomForm.Notification so the TDesigner is notified of actions.

  Added more code for getting info via RTTI
  Shane

  Revision 1.4  2000/08/14 12:31:12  lazarus
  Minor modifications for SynEdit .
  Shane

  Revision 1.3  2000/08/09 14:15:04  lazarus
  Changed the TCUstomForm create function.  I am getting it ready to read the resources to auto-create the controls...

  Anslo changes TScreen.AddForm and TScreen.RemoveForm.  They were being passed TFOrm's instead of TCustomForms.
  Shane

  Revision 1.2  2000/07/23 19:01:33  lazarus
  menus will be destroyed now, stoppok

  Revision 1.1  2000/07/13 10:28:25  michael
  + Initial import

  Revision 1.5  2000/05/09 02:07:40  lazarus
  Replaced writelns with Asserts.                          CAW

  Revision 1.4  2000/05/03 17:19:29  lazarus
  Added the TScreem forms code by hongli@telekabel.nl
  Shane

  Revision 1.3  2000/04/10 14:03:07  lazarus
  Added SetProp and GetProp winapi calls.
  Added ONChange to the TEdit's published property list.
  Shane

  Revision 1.2  2000/04/07 16:59:55  lazarus
  Implemented GETCAPTURE and SETCAPTURE along with RELEASECAPTURE.
  Shane

  Revision 1.1  2000/04/02 20:49:56  lazarus
  MWE:
    Moved lazarus/lcl/*.inc files to lazarus/lcl/include

  Revision 1.37  2000/03/30 18:07:53  lazarus
  Added some drag and drop code
  Added code to change the unit name when it's saved as a different name.  Not perfect yet because if you are in a comment it fails.

  Shane

  Revision 1.36  2000/03/15 20:15:31  lazarus
  MOdified TBitmap but couldn't get it to work
  Shane

  Revision 1.35  2000/03/03 20:22:03  lazarus
  Trying to add TBitBtn
  Shane

  Revision 1.34  2000/03/01 00:41:02  lazarus
  MWE:
    Fixed updateshowing problem
    Added some debug code to display the name of messages
    Did a bit of cleanup in main.pp to get the code a bit more readable
      (my editor does funny things with tabs if the indent differs)

  Revision 1.33  2000/02/28 19:16:04  lazarus
  Added code to the FILE CLOSE to check if the file was modified.  HAven't gotten the application.messagebox working yet though.  It won't stay visible.
  Shane

  Revision 1.32  2000/02/28 00:15:54  lazarus
  MWE:
    Fixed creation of visible componets at runtime. (when a new editor
      was created it didn't show up)
    Made the hiding/showing of controls more delphi compatible

  Revision 1.31  2000/02/24 21:15:30  lazarus
  Added TCustomForm.GetClientRect and RequestAlign to try and get the controls to align correctly when a MENU is present.  Not Complete yet.

  Fixed the bug in TEdit that caused it not to update it's text property.  I will have to
  look at TMemo to see if anything there was affected.

  Added SetRect to WinAPI calls
  Added AdjustWindowRectEx to WINAPI calls.
  Shane

  Revision 1.30  2000/02/23 14:19:09  lazarus
  Fixed the conflicts caused when two people worked on the ShowModal method for CustomForm and CustomDialog at the same time.
  Shane

  Revision 1.29  2000/02/22 22:19:49  lazarus
  TCustomDialog is a descendant of TComponent.
  Initial cuts a form's proper Close behaviour.

  Revision 1.28  2000/02/22 17:32:49  lazarus
  Modified the ShowModal call.
  For TCustomForm is simply sets the visible to true now and adds fsModal to FFormState.  In gtkObject.inc FFormState is checked.  If it contains fsModal then either gtk_grab_add or gtk_grab_remove is called depending on the value of VISIBLE.

  The same goes for TCustomDialog (open, save, font, color).
  I moved the Execute out of the individual dialogs and moved it into TCustomDialog and made it virtual because FONT needs to set some stuff before calling the inherited execute.
  Shane

  Revision 1.27  2000/02/19 18:11:59  lazarus
  More work on moving, resizing, forms' border style etc.

  Revision 1.26  2000/02/18 19:38:52  lazarus
  Implemented TCustomForm.Position
  Better implemented border styles. Still needs some tweaks.
  Changed TComboBox and TListBox to work again, at least partially.
  Minor cleanups.

  Revision 1.25  2000/01/03 00:19:21  lazarus
  MWE:
    Added keyup and buttonup events
    Added LM_MOUSEMOVE callback
    Started with scrollbars in editor

  Revision 1.24  1999/12/22 01:16:03  lazarus
  MWE:
    Changed/recoded keyevent callbacks
    We Can Edit!
    Commented out toolbar stuff

  Revision 1.23  1999/12/14 00:16:43  lazarus
  MWE:
    Renamed LM... message handlers to WM... to be compatible and to
      get more edit parts to compile
    Started to implement GetSystemMetrics
    Removed some Lazarus specific parts from mwEdit

  Revision 1.22  1999/12/10 00:47:01  lazarus
  MWE:
    Fixed some samples
    Fixed Dialog parent is no longer needed
    Fixed (Win)Control Destruction
    Fixed MenuClick

  Revision 1.21  1999/12/08 00:56:07  lazarus
  MWE:
    Fixed menus. Events aren't enabled yet (dumps --> invalid typecast ??)

  Revision 1.20  1999/12/07 01:19:25  lazarus
  MWE:
    Removed some double events
    Changed location of SetCallBack
    Added call to remove signals
    Restructured somethings
    Started to add default handlers in TWinControl
    Made some parts of TControl and TWinControl more delphi compatible
    ... and lots more ...

  Revision 1.19  1999/11/23 22:06:27  lazarus
  Minor changes to get it running again with the latest compiler.  There is something wrong with the compiler that is preventing certain things from working.
  Shane

  Revision 1.18  1999/11/04 21:52:08  lazarus
  wndproc being used a little
  Shane

  Revision 1.17  1999/11/02 16:02:34  lazarus
  Added a bunch of wndproc stuff and a lot of functions that really don't do a thing at this point.
  Shane

  Revision 1.16  1999/11/01 09:53:16  lazarus
  MWE: Implemented HandleNeeded/CreateHandle/CreateWND
       Now controls are created on demand. A call to CreateComponent shouldn't
       be needed. It is now part of CreateWnd

  Revision 1.15  1999/10/28 23:48:57  lazarus
  MWE: Added new menu classes and started to use handleneeded

  Revision 1.14  1999/10/28 20:37:34  lazarus
  TCustomForm.ClientWndProc added.
  Shane

  Revision 1.13  1999/10/27 17:27:07  lazarus
  Added alot of changes and TODO: statements
  shane

  Revision 1.12  1999/10/27 12:53:23  lazarus
  Added LCLLinux.pp and removed Linux.pp
  Also, added the TCustomForm.ISFORM function.
  Shane

  Revision 1.11  1999/09/21 23:46:53  lazarus
  *** empty log message ***

  Revision 1.10  1999/08/07 17:59:16  lazarus

        buttons.pp   the DoLeave and DoEnter were connected to the wrong
                     event.

        The rest were modified to use the new CNSendMEssage function.   MAH

  Revision 1.9  1999/08/02 01:13:32  lazarus
  Added new colors and corrected BTNFACE
  Need the TSCrollbar class to go further with the editor.
  Mouse doesn't seem to be working correctly yet when I click on the editor window

  Revision 1.8  1999/08/01 21:46:24  lazarus
  Modified the GETWIDTH and GETHEIGHT of TFOnt so you can use it to calculate the length in Pixels of a string.  This is now used in the editor.

  Shane

  Revision 1.7  1999/07/31 06:39:20  lazarus

       Modified the IntCNSendMEssage3 to include a data variable. It isn't used
       yet but will help in merging the Message2 and Message3 features.

       Adjusted TColor routines to match Delphi color format

       Added a TGdkColorToTColor routine in gtkproc.inc

       Finished the TColorDialog added to comDialog example.        MAH

 }
