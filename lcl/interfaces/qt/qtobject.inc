{
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
//---------------------------------------------------------------

procedure EventTrace(message : string; data : pointer);
begin
end;


function TQtWidgetSet.GetText(Sender: TControl; var Text: String): Boolean;
begin
end;
procedure TQtWidgetSet.SetLabel(Sender : TObject; Data : Pointer);
var
pLabel: pchar;
lSelf : TWinControl;
begin
   lSelf := Sender as TWinControl;
   pLabel := pchar(Data);

   SetWidgetText(lSelf.Handle,pLabel);
end;

function  TQtWidgetSet.IntSendMessage3(LM_Message : Integer; Sender : TObject;data : pointer) : integer;
var
lParent: TWinControl;
lSelf: TWinControl;

begin
    // Assert(False, 'Trace:' + IntToStr(LM_Message));
    case LM_Message of
     LM_SetLabel :
      begin
         SetLabel(Sender,Data);
      end;
     LM_ADDCHILD:
      begin
       if assigned((Sender as TWinControl).Parent) then
         begin
           lSelf := Sender as TWinControl;
           lParent := (Sender as TWinControl).Parent;
           ReparentWidget(lParent.Handle,lSelf.Handle);
         end;
      end;

      LM_Create :
      begin
        Assert(False, 'Trace:Calling CreateComponent');
        CreateComponent(Sender);
        Assert(False, 'Trace:Called CreateComponent');
      end;

      LM_SHOWHIDE:
      begin
       ShowHide(Sender);
      end;

      LM_SetSize :
      begin
        lSelf := Sender as TWinControl;
      //  Assert(False, 'Trace:' + IntTostr(lSelf.Handle));
        MoveWidget(lSelf.Handle, pTRect(Data)^.Left, pTRect(Data)^.Top);
        ResizeWidget(lSelf.Handle, pTRect(Data)^.Right,pTRect(Data)^.Bottom);
        SetWidgetText(lSelf.Handle,'test');
      end;




    end;//end case
end;

procedure TQtWidgetSet.CreateComponent(Sender : TObject);
var
CompStyle: integer;
lHandle: integer;
begin
  lHandle := -1;

  if (Sender is TControl) then
     CompStyle := TControl(Sender).FCompStyle;


  case CompStyle of
    csForm:
       begin
        Assert(False, 'Trace:CREATE FORM');
        lHandle := CreateWidget(WIDGET);
        SetData(lHandle,Sender);
       end;

    csButton:
       begin
        lHandle := CreateWidget(WIDGET_PUSH_BUTTON);
        SetData(lHandle,Sender);
       end;
  end;//case

  if (lHandle > -1) then
    if (Sender is TWinControl) then
       TWinControl(Sender).Handle := THandle(lhandle);
end;

procedure TQtWidgetSet.ShowHide(Sender : TObject);
begin
 //put in hide code once the interface supports it
 if TControl(Sender).Visible then
    ShowWidget(TWinControl(Sender).Handle);
end;


procedure TQtWidgetSet.SetCallback(Msg : LongInt; Sender : TObject);
var
lHandle : integer;
begin

  if not  assigned(Sender) then
    exit;

  lHandle := (Sender as TWinControl).Handle;

  case Msg of
    LM_LBUTTONDOWN,
    LM_RBUTTONDOWN,
    LM_MBUTTONDOWN,
    LM_MOUSEWHEEL :
    begin
     Assert(False, 'Trace:BUTTON DOWN CONNECT REQUEST');
     HookMousePressedEvent(lHandle,@QTMousePressedEvent);
      // ConnectSignal(gFixed, 'button-press-event', @gtkmousebtnpress, GDK_BUTTON_PRESS_MASK);
    end;

    LM_LBUTTONUP,
    LM_RBUTTONUP,
    LM_MBUTTONUP:
    begin
    // HookMousePressedEvent(lHandle,@QTMousePressedEvent);
    end;
  end;//case


end;

procedure TQtWidgetSet.RemoveCallbacks(Sender : TObject);
begin
end;
procedure TQtWidgetSet.DoEvents;
begin
 Assert(False, 'Trace:*******QT ENGINE HAS ENTERED EVENT ITTERATOR*******');
end;
procedure TQtWidgetSet.AppProcessMessages;
begin
 MainLoop;
end;
procedure TQtWidgetSet.AppTerminate;
begin
Assert(False, 'Trace:*******APP TERMINATE CALLED*****');
 Shutdown;
end;
procedure TQtWidgetSet.Init;
begin
 InitializeEngine;
end;
//------------------------------------------------------------------------
