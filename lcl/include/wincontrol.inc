(******************************************************************************
                                  TWinControl
 ******************************************************************************)

{$IFOPT C-}
// Uncomment for local trace
//  {$C+}
//  {$DEFINE ASSERT_IS_ON}
{$ENDIF}

{------------------------------------------------------------------------------}
{  TWinControl AdjustSize                                                      }
{------------------------------------------------------------------------------}
procedure TWinControl.AdjustSize;
begin
  inherited AdjustSize;
  if HandleAllocated then RequestAlign;
end;

{------------------------------------------------------------------------------}
{  TWinControl AdjustClientRect                                                      }
{------------------------------------------------------------------------------}
Procedure TWinControl.AdjustClientRect(var Rect: TRect);
Begin
//Not used.  It's a virtual procedure that should be overriden.
end;

{------------------------------------------------------------------------------}
{  TWinControl AlignControls                                                      }
{------------------------------------------------------------------------------}
procedure TWinControl.AlignControls(AControl : TControl; var Rect : TRect);
var
  AlignList: TList;

  function AlignWork: Boolean;
  var
    I: Integer;
  begin
    Result := True;
    for I := ControlCount - 1 downto 0 do
      if (Controls[I].Align <> alNone) or
         (Controls[I].Anchors <> [akLeft, akTop]) then Exit;
    Result := False;
  end;

  function Anchored(Align: TAlign; Anchors: TAnchors): Boolean;
  begin
    case Align of
      alLeft: Result := akLeft in Anchors;
      alTop: Result := akTop in Anchors;
      alRight: Result := akRight in Anchors;
      alBottom: Result := akBottom in Anchors;
      alClient: Result := Anchors = [akLeft, akTop, akRight, akBottom];
    else
      Result := False;
    end;
  end;

  procedure DoPosition(Control: TControl; AAlign: TAlign);
  var
    Left2, Top2, Width2, Height2: Integer;
    R: TRect;
  begin
    with Rect do
    begin
      { Just recalculate the anchors }
      if (AAlign = alNone) or (Control.Anchors <> AnchorAlign[AAlign]) then
        with Control do
        begin
          Width2 := Parent.FLastResize.X + FLastWidth;
          Height2 := Parent.FLastResize.Y + FLastHeight;
          R := BoundsRect;
          if not (akLeft in Anchors) then
            if not (akRight in Anchors) then
              OffsetRect(R, Parent.FLastResize.X div 2, 0)
            else
              OffsetRect(R, Parent.FLastResize.X, 0)
          else if akRight in Anchors then
            R.Right := R.Left + Width2;
          if not (akTop in Anchors) then
            if not (akBottom in Anchors) then
              OffsetRect(R, 0, Parent.FLastResize.Y div 2)
            else
              OffsetRect(R, 0, Parent.FLastResize.Y)
          else if akBottom in Anchors then
            R.Bottom := R.Top + Height2;
          BoundsRect := R;
          FLastWidth := Width2;
          FLastHeight := Height2;
          if AAlign = alNone then Exit;
        end;

      { Realign }
      Width2 := Right - Left;
      if (Width2 < 0) or (AAlign in [alLeft, alRight]) then
        Width2 := Control.Width;
      Height2 := Bottom - Top;
      if (Height2 < 0) or (AAlign in [alTop, alBottom]) then
        Height2 := Control.Height;
      Left2 := Left;
      Top2 := Top;
      case AAlign of
        alTop:
          Inc(Top, Height2);
        alBottom:
          begin
            Dec(Bottom, Height2);
            Top2 := Bottom;
          end;
        alLeft:
          Inc(Left, Width2);
        alRight:
          begin
            Dec(Right, Width2);
            Left2 := Right;
          end;
      end;
    end;
    if (Control.Left <> Left2) or (Control.Top <> Top2) or (Control.Width <> Width2) or (Control.Height <> Height2) then begin
      Control.SetBounds(Left2, Top2, Width2, Height2);
    end;  

    {Sometimes the control doesn't resize.  This will verifiy that it is the size it is assigned to be}
    if (Control.Width <> Width2) or (Control.Height <> Height2) then
      with Rect do
        case AAlign of
          alTop: Dec(Top, Height2 - Control.Height);
          alBottom: Inc(Bottom, Height2 - Control.Height);
          alLeft: Dec(Left, Width2 - Control.Width);
          alRight: Inc(Right, Width2 - Control.Width);
          alClient:
            begin
              Inc(Right, Width2 - Control.Width);
              Inc(Bottom, Height2 - Control.Height);
            end;
        end;
  end;

  function InsertBefore(Control1, Control2: TControl; AAlign: TAlign): Boolean;
  begin
    Result := False;
    case AAlign of
      alTop: Result := Control1.Top < Control2.Top;
      alBottom: Result := (Control1.Top + Control1.Height) >= (Control2.Top + Control2.Height);
      alLeft: Result := Control1.Left < Control2.Left;
      alRight: Result := (Control1.Left + Control1.Width) >= (Control2.Left + Control2.Width);
    end;
  end;

  procedure DoAlign(AAlign: TAlign);
  var
    I, X: Integer;
    Control: TControl;
  begin
    AlignList.Clear;
    if (AControl <> nil) and
       ((AAlign = alNone)
         or AControl.Visible
	 or (csDesigning in AControl.ComponentState)
	 and not (csNoDesignVisible in AControl.ControlStyle))
       and (AControl.Align = AAlign) then
       
      AlignList.Add(AControl);

    for I := 0 to ControlCount - 1 do
    begin
      Control := Controls[I];
      if (Control.Align = AAlign) and ((AAlign = alNone) or (Control.Visible or (Control.ControlStyle * [csAcceptsControls, csNoDesignVisible] =
          [csAcceptsControls, csNoDesignVisible])) or (csDesigning in Control.ComponentState) and not (csNoDesignVisible in Control.ControlStyle)) then
      begin
        if Control = AControl then Continue;
        X := 0;
        while (X < AlignList.Count) and not InsertBefore(Control, TControl(AlignList[X]), AAlign) do 
              Inc(X);
        AlignList.Insert(X, Control);
      end;
    end;
    for I := 0 to AlignList.Count - 1 do
      DoPosition(TControl(AlignList[I]), AAlign);
  end;

begin
  if AlignWork then
  begin
    AdjustClientRect(Rect);
    AlignList := TList.Create;
    try
      DoAlign(alTop);
      DoAlign(alBottom);
      DoAlign(alLeft);
      DoAlign(alRight);
      DoAlign(alClient);
      DoAlign(alNone);// Anchored controls are not currently used in Lazarus, but in the future, this will move them
    finally
      AlignList.Free;
    end;
  end;
  FLastResize.X := 0;
  FLastResize.Y := 0;

  if Showing then AdjustSize;
end;

{------------------------------------------------------------------------------}
{  TWinControl BroadCast                                                      }
{------------------------------------------------------------------------------}
Procedure TWinControl.BroadCast(var Message);
var
  I: Integer;
begin
  for I := 0 to ControlCount - 1 do
  begin
    Controls[I].WindowProc(TLMessage(Message));
    if TLMessage(Message).Result <> 0 then Exit;
  end;
end;

{------------------------------------------------------------------------------}
{  TWinControl CMDrag                                                      }
{------------------------------------------------------------------------------}
Procedure TWinControl.CMDrag(var MEssage: TCMDrag);
Begin
 with Message, DragRec^ do
   Begin
   case DragMessage of
         dmDragEnter, dmDragLEave,dmDragMOve, dmDragDrop :
                                   if target <> nil then TControl(target).DoDragMsg(Message);
         dmFindTarget:begin
                       Writeln('dmFindTarget');
                       result := longint(ControlatPos(ScreentoClient(pos),False));
                       if Result = 0 then Result := longint(Self);
                      end;
         end;//case


   end;

end;

{------------------------------------------------------------------------------}
{  TWinControl CreateSubClass                                                      }
{------------------------------------------------------------------------------}
procedure TWinControl.CreateSubClass(var Params: TCreateParams; ControlClassName: PChar);
(*
const
  CS_OFF = CS_OWNDC or CS_CLASSDC or CS_PARENTDC or CS_GLOBALCLASS;
  CS_ON = CS_VREDRAW or CS_HREDRAW;
var
  SaveInstance: THandle;
begin
  if ControlClassName <> nil then
    with Params do
    begin
      SaveInstance := WindowClass.hInstance;
      if not GetClassInfo(HInstance, ControlClassName, WindowClass) and
        not GetClassInfo(0, ControlClassName, WindowClass) and
        not GetClassInfo(MainInstance, ControlClassName, WindowClass) then
        GetClassInfo(WindowClass.hInstance, ControlClassName, WindowClass);
      WindowClass.hInstance := SaveInstance;
      WindowClass.style := WindowClass.style and not CS_OFF or CS_ON;
    end;
*)
begin
  // TODO: implement missing funcs
end;


{------------------------------------------------------------------------------}
{  TWinControl DisableAlign}
{------------------------------------------------------------------------------}
procedure TWinControl.DisableAlign;
begin
  Inc(FAlignLevel);
End;

{------------------------------------------------------------------------------}
{  TWinControl EnableAlign}
{------------------------------------------------------------------------------}
procedure TWinControl.EnableAlign;
begin
  Dec(FAlignLevel);
  if FAlignLevel = 0 then begin
    if csAlignmentNeeded in ControlState then ReAlign;
     FLastResize.X := 0;
     FLastresize.Y := 0;
  end;
end;

{------------------------------------------------------------------------------}
{  TWinControl GetChildren                                                     }
{------------------------------------------------------------------------------}
Procedure TWinControl.GetChildren(Proc: TGetChildProc; Root : TComponent);
var
I : Integer;
Control : TControl;
Begin
  for I := 0 to ControlCount-1 do
      Begin
        Control := Controls[i];
        if Control.Owner = Root then Proc(Control);
      end;
End;

{------------------------------------------------------------------------------}
{  TWinControl GetClientOrigin                                                      }
{------------------------------------------------------------------------------}
Function TWinControl.GetClientOrigin: TPoint;
Begin
  Result.X := 0;
  Result.Y := 0;
  LCLLinux.ClientToScreen(Handle,Result);
  Assert(False, Format('Trace:[TWinControl.GetClientOrigin] %s --> (%d, %d)',  [Classname, Result.X, Result.Y]));
end;


{------------------------------------------------------------------------------}
{  TWinControl InvalidateRect                                                      }
{------------------------------------------------------------------------------}
Procedure TWinControl.InvalidateRect(Sender : TObject; Rect : TRect; Value : Boolean);
Begin
  Assert(False,'Trace:TWinControl InvalidateRect');
  if HandleAllocated
  then CNSendMessage(LM_InvalidateRect,Self,@Rect);
end;

{------------------------------------------------------------------------------}
{  TWinControl ReCreateWnd                                                      }
{------------------------------------------------------------------------------}
Procedure TWinControl.ReCreateWnd;
Begin
//TODO: 
//  if HandleAllocated
//  then sendmessage
end;

{------------------------------------------------------------------------------}
{  TWinControl SetBorderWidth                                                      }
{------------------------------------------------------------------------------}
Procedure TWinControl.SetBorderWidth(value : TBorderWidth);
Begin
//TODO: SETBORDERWIDTH - Not sure if anything more is needed here
FBorderWidth := Value;
end;

{------------------------------------------------------------------------------}
{  TWinControl GetTabOrder                                                      }
{------------------------------------------------------------------------------}
Function TWinControl.GetTabOrder : TTabOrder;
Begin
  if FParent <> nil 
  then Result := FTabOrder //TODO:get this from parent tablist
  else Result := -1;
end;

{------------------------------------------------------------------------------}
{  TWinControl UpdateShowing                                                      }
{------------------------------------------------------------------------------}
procedure TWinControl.UpdateShowing;
var
  bShow: Boolean;
  n: Integer;
begin
  bShow := (FVisible or (csDesigning in ComponentState) and not (csNoDesignVisible in ControlStyle))
    and not (csReadingState in ControlState);

  if bShow then
  begin
    if FHandle = 0 then CreateHandle;
    if FWinControls <> nil 
    then begin
      for n := 0 to FWinControls.Count - 1 do
        TWinControl(FWinControls[n]).UpdateShowing;
    end;
  end;


  if FHandle <> 0 
  then begin
    if FShowing <> bShow 
    then begin
      FShowing := bShow;
      try
        Perform(CM_SHOWINGCHANGED, 0, 0);
      except
        FShowing := not bShow;
        raise;
      end;
    end;
  end;

end;
{------------------------------------------------------------------------------}
{  TWinControl UpdateTabOrder                                                      }
{------------------------------------------------------------------------------}
Procedure TWinControl.UpdateTabOrder(Value : TTabOrder);
Begin
  Assert(False, 'Trace:TODO:[TWinControl.UpdateTabOrder]');
end;

{------------------------------------------------------------------------------}
{  TWinControl Focused                                                      }
{------------------------------------------------------------------------------}
Function TWinControl.Focused : Boolean;
Begin
  Assert(False, 'Trace:TODO:[TWinControl.Focused]');
  Result := True;
end;

{------------------------------------------------------------------------------}
{  TWinControl FindChildControl                                               }
{------------------------------------------------------------------------------}
function TWinControl.FindChildControl(ControlName: string): TControl;
var
  I: Integer;
begin
  Result := nil;
  if FWinControls <> nil then
    for I := 0 to FWinControls.Count - 1 do
      if CompareText(TWinControl(FWinControls[I]).Name, ControlName) = 0 then
      begin
        Result := TControl(FWinControls[I]);
        Exit;
      end;
end;

{------------------------------------------------------------------------------}
{  TWinControl IsControlMouseMsg                                               }
{------------------------------------------------------------------------------}
function TWinControl.IsControlMouseMsg(var Message : TLMMouse) : Boolean;
var
  Control : TControl;
  P : TPoint;
begin
//  WriteLN(Format('[TWinControl.IsControlMouseMsg] %s', [ClassName]));
  if GetCapture = Handle 
  then begin
//    WriteLN(Format('[TWinControl.IsControlMouseMsg] %s --> We are capture', [ClassName]));
    Control := nil;
{    if CaptureControl <> nil
    then WriteLN(Format('[TWinControl.IsControlMouseMsg] %s --> CaptureControl = %s', [ClassName, CaptureControl.ClassName]));
}
    if (CaptureControl <> nil) 
    and (CaptureControl.Parent = Self) 
    then Control := CaptureControl;
  end
  else Control := ControlAtPos(SmallPointtoPoint(Message.Pos),False);

    if CaptureControl <> nil
    then WriteLN(Format('[TWinControl.IsControlMouseMsg] %s --> CaptureControl = %s', [ClassName, CaptureControl.ClassName]));

{if Control <> nil then
  Writeln('---------------COntrol is present.  Its '+TCOntrol(Control).name)
  else
  Writeln('ISCONTROLMOUSEMSG - Control=nil');
}
  Result := False;
  if Control <> nil 
  then begin
//    Writeln('Control <> nil');
    P.X := Message.XPos - Control.Left;
    P.Y := Message.YPos - Control.Top;
//    writeln('P.x and P.y = '+inttostr(p.x)+'  '+inttostr(p.y));
//    WriteLN(Format('[TWinControl.IsControlMouseMsg] %s --> perform message', [Control.ClassName]));
    Control.Perform(Message.Msg, Message.Keys, LongInt(PointtoSmallPoint(P)));
//    Writeln('done');
    Result := True;
  end;
end;

procedure TWinControl.PaintHandler(var Message: TLMPaint);
var
  I, Clip, SaveIndex: Integer;
  DC: HDC;
  PS: TPaintStruct; //defined in LCLLinux.pp
begin
  Assert(False, Format('Trace:> [TWinControl.PaintHandler] %s --> Msg.DC: 0x%x', [ClassName, Message.DC]));
  DC := Message.DC;
  if DC = 0 then DC := BeginPaint(Handle, PS);
  try
    if FControls = nil then PaintWindow(DC) else
    begin
      SaveIndex := SaveDC(DC);
      Clip := SimpleRegion;
      for I := 0 to FControls.Count - 1 do
        with TControl(FControls[I]) do
          if (Visible or (csDesigning in ComponentState) and
            not (csNoDesignVisible in ControlStyle)) and
            (csOpaque in ControlStyle) then
          begin
            Clip := ExcludeClipRect(DC, Left, Top, Left + Width, Top + Height);
            if Clip = NullRegion then Break;
          end;
      if Clip <> NullRegion then PaintWindow(DC);
      RestoreDC(DC, SaveIndex);
    end;
    PaintControls(DC, nil);
  finally
    if Message.DC = 0 then EndPaint(Handle, PS);
  end;
  Assert(False, Format('Trace:< [TWinControl.PaintHandler] %s', [ClassName]));
end;


procedure TWinControl.PaintControls(DC: HDC; First: TControl);
var
  I, Count, SaveIndex: Integer;
  FrameBrush: HBRUSH;
  TempCOntrol : TCOntrol;
begin
  if FControls <> nil then
  begin
    I := 0;
    if First <> nil then
    begin
      I := FControls.IndexOf(First);
      if I < 0 then I := 0;
    end;
    Count := FControls.Count;
    while I < Count do
    begin
      TempCOntrol := TControl(FControls.Items[I]);
      with (TempControl) do
        if (Visible or (csDesigning in ComponentState) and
          not (csNoDesignVisible in ControlStyle)) and
          RectVisible(DC, Rect(Left, Top, Left + Width, Top + Height)) then
        begin
          if csPaintCopy in Self.ControlState then
            Include(FControlState, csPaintCopy);
          SaveIndex := SaveDC(DC);
          MoveWindowOrg(DC, Left, Top);
          IntersectClipRect(DC, 0, 0, Width, Height);
          Perform(LM_PAINT, DC, 0);
          RestoreDC(DC, SaveIndex);
          Exclude(FControlState, csPaintCopy);
        end;
      Inc(I);
    end;
  end;
  if FWinControls <> nil then
    for I := 0 to FWinControls.Count - 1 do
      with TWinControl(FWinControls.Items[I]) do
        if FCtl3D and (csFramed in ControlStyle) and
          (Visible or (csDesigning in ComponentState) and
          not (csNoDesignVisible in ControlStyle)) then
        begin
         //TODO: CreateSolidBrush and FrameRect
         {FrameBrush := CreateSolidBrush(ColorToRGB(clBtnShadow));
          FrameRect(DC, Rect(Left - 1, Top - 1, Left + Width, Top + Height),
            FrameBrush);
          DeleteObject(FrameBrush);
          FrameBrush := CreateSolidBrush(ColorToRGB(clBtnHighlight));
          FrameRect(DC, Rect(Left, Top, Left + Width + 1, Top + Height + 1),
            FrameBrush);
          DeleteObject(FrameBrush);
          }
        end;
end;

procedure TWinControl.PaintWindow(DC: HDC);
var
  Message: TLMessage;
begin
  Message.Msg := LM_PAINT;
  Message.WParam := DC;
  Message.LParam := 0;
  Message.Result := 0;
  DefaultHandler(Message);
end;


{------------------------------------------------------------------------------}
{  TWinControl ControlAtPos                                                      }
{------------------------------------------------------------------------------}
Function TWinControl.ControlAtPos(const Pos : TPoint; AllowDisabled : Boolean): TControl;
var
  I : Integer;
  P : TPoint;
Begin
//  Assert(False, Format('Trace:[TWinControl.ControlAtPos] %s(%s) --> Pos: (%d, %d)', [ClassName, Name, Pos.X, Pos.Y]));
  Result :=  nil;
  if FControls <> nil 
  then
    for I := FControls.Count-1 downto 0 do
    begin
      Result := TControl(FControls.Items[I]);
      with Result do
      begin
        P := Point(Pos.X - Left, Pos.Y - Top);
        //MWE: rewrote it a bit to get it more readable
        if  PtInRect(ClientRect,P) 
        and (   
             (
              (csDesigning in ComponentState) 
              and 
              (Visible or not (csNoDesignVisible in ControlStyle))
             )
             or
             (
              (Visible)
              and 
              (Enabled or AllowDisabled) 
              and 
              (Perform(CM_HITTEST, 0, LongInt(PointtoSmallPoint(P))) <> 0)
             )
            ) 
        then Exit;
      end;
      Result :=  nil;
    end;
  //endif
end;

{------------------------------------------------------------------------------}
{  TWinControl DestroyHandle                                                      }
{------------------------------------------------------------------------------}
procedure TWinControl.DestroyHandle;
var i : integer;
begin
  { Destroy all children handles, too }
  { If we don't do that, GTK does this without notification for us and we crash }
  { TODO : We can enable HandleAllocated condition only when all controls, especially
    TNotebook / TPage set their Handles correctly, i.e. mirror the GTK behavior }
//  if HandleAllocated then begin
    if FWinControls <> nil then begin
      for i:= 0 to FWinControls.Count - 1 do begin
        TWinControl(FWinControls[i]).DestroyHandle;
      end;	
    end;
    DestroyWnd;
//  end;
end;


{------------------------------------------------------------------------------}
{  TWinControl WndPRoc                                                      }
{------------------------------------------------------------------------------}
Procedure TWinControl.WndPRoc(Var Message : TLMessage);
Var
 Form: TCustomForm;
 KeyState: TKeyboardState;
 WHeelMsg : TCMMouseWheel;
Begin
//  Assert(False, Format('Trace:[TWinControl.WndPRoc] %s(%s) --> Message = %d', [ClassName, Name, Message.Msg]));
  case Message.Msg of
    LM_SETFOCUS:
      begin
        Assert(False, Format('Trace:[TWinControl.WndPRoc] %s --> LM_SETFOCUS', [ClassName]));
        Form := GetPArentForm(Self);
        if (Form <> nil) and not Form.SetFocusedControl(Self) then Exit;
      end;
    LM_KILLFOCUS: 
      begin
        Assert(False, Format('Trace:[TWinControl.WndPRoc] %s --> _KILLFOCUS', [ClassName]));
        if csFocusing in ControlState then Exit;
      end;
    LM_NCHITTEST:
      begin
        inherited WndPRoc(Message);
        if (Message.Result = HTTRANSPARENT) 
        and (ControlAtPos(ScreenToClient(SmallPointToPoint(TLMNCHitTest(Message).Pos)), False) <> nil) 
        then  Message.Result := HTCLIENT;
        Exit;
      end;
    LM_MOUSEFIRST..LM_MOUSELAST:
       if IsControlMouseMSG(TLMMOUSE(Message)) then Exit;
    LM_KEYFIRST..LM_KEYLAST:
      if Dragging then Exit;
    LM_CANCELMODE:
      if (GetCapture = Handle) 
      and (CaptureControl <> nil) 
      and (CaptureControl.Parent = Self) 
      then CaptureControl.Perform(LM_CANCELMODE,0,0);
    else
//TODO:Implement TMOUSE
 {    with Mouse do
     if WheelPresent and (RegWheelMessage <> 0) and (Message.Msg = RegWheelMessage) then
        Begin
          GetKeyboardState(KeyState);
          with WheelMsg do
          Begin
            Msg := Message.Msg;
            ShiftState := KeyboardStateToShiftState(KeyState);
            WheelData :=Message.WParam;
            Pos := TSmallPoint(Message.LPaream);
          end;
          MouseWheelHandler(TMessage(WheelMsg));
          Exit;
        end;
}       end;

        Inherited WndProc(Message);

end;

{------------------------------------------------------------------------------
  Method:  TWinControl.MainWndProc
  Params:  Message:
  Returns: Nothing

  Description of the procedure for the class.
 ------------------------------------------------------------------------------}
Procedure TWinControl.MainWndPRoc(Var Message : TLMessage);
Begin
  Assert(False, Format('Trace:[TWinControl.MainWndPRoc] %s(%s) --> Message = %d', [ClassName, Name, Message.Msg]));
end;

{------------------------------------------------------------------------------}
{  TWinControl SetFocus                                                      }
{------------------------------------------------------------------------------}
procedure TWinControl.SetFocus;
begin
  if Visible and HandleAllocated
  then LCLLinux.SetFocus(Handle);
end;

{------------------------------------------------------------------------------}
{  TWinControl SetTabOrder                                                      }
{------------------------------------------------------------------------------}
Procedure TWinControl.SetTabOrder(Value : TTabOrder);
Begin
FTabOrder := Value;
end;

{------------------------------------------------------------------------------}
{  TWinControl SetParentCtl3D                                                      }
{------------------------------------------------------------------------------}
Procedure TWinControl.SetParentCtl3D(value : Boolean);
Begin
if FParentCtl3D <> Value then
   Begin
        FParentCtl3D := Value;
        if FParent <> nil then
                Begin
                //Sendmessage to do something?
                End;
   end;
end;


{------------------------------------------------------------------------------}
{  TWinControl KeyDown                                                      }
{------------------------------------------------------------------------------}
Procedure TWinControl.KeyDown(var Key: Word; shift : TShiftState);
Begin
  if Assigned(FOnKeyDown) then FOnKeyDown(Self, Key, Shift);
end;

{------------------------------------------------------------------------------}
{  TWinControl KeyUp                                                      }
{------------------------------------------------------------------------------}
Procedure TWinControl.KeyUp(var Key: Word; shift : TShiftState);
begin
  if Assigned(FOnKeyUp) then FOnKeyUp(Self, Key, Shift);
end;

{------------------------------------------------------------------------------}
{  TWinControl KeyPress                                                      }
{------------------------------------------------------------------------------}
Procedure TWinControl.KeyPress(var Key: Char);
begin
  if Assigned(FOnKeyPress) then FOnKeyPress(Self, Key);
end;

{------------------------------------------------------------------------------}
{  TWinControl DoKeyDown                                                      }
{------------------------------------------------------------------------------}
function TWinControl.DoKeyDown(Var Message : TLMKey): Boolean;
var
  F: TCustomForm;
  ShiftState: TShiftState;
begin
  Result := True;
  F := GetParentForm(Self);
  if  (F <> nil) 
  and (F <> Self) 
  and (F.KeyPreview)
  and (TWinControl(F).DoKeyDown(Message)) then Exit;
  
  with Message do
  begin
    ShiftState := KeyDataToShiftState(KeyData);
    //ShiftState := [];
    if not (csNoStdEvents in ControlStyle) 
    then begin
      KeyDown(CharCode, ShiftState);
      if CharCode = 0 then Exit;
    end;
  end;
  Result := False;
end;

{------------------------------------------------------------------------------}
{  TWinControl DoKeyPress                                                      }
{------------------------------------------------------------------------------}
Function TWinControl.DoKeyPress(Var Message : TLMKey): Boolean;
var
  F: TCustomForm;
  C: Char;
begin
  Result := True;
  F := GetParentForm(Self);
  if  (F <> nil) 
  and (F <> Self) 
  and (F.KeyPreview) 
  and (TWinControl(F).DoKeyPress(Message)) then Exit;
  
  if not (csNoStdEvents in ControlStyle) 
  then with Message do
  begin
    C := Char(CharCode);
    KeyPress(C);
    CharCode := Ord(C);
    if Char(CharCode) = #0 then Exit;
  end;
  
  Result := False;
End;

{------------------------------------------------------------------------------}
{  TWinControl DoKeyUp                                                      }
{------------------------------------------------------------------------------}
Function TWinControl.DoKeyUp(Var Message : TLMKey): Boolean;
var
  F: TCustomForm;
  ShiftState: TShiftState;
begin
  Result := True;
  F := GetParentForm(Self);
  if  (F <> nil) 
  and (F <> Self) 
  and (F.KeyPreview) 
  and (TWinControl(F).DoKeyUp(Message)) then Exit;
  
  with Message do
  begin
    ShiftState := KeyDataToShiftState(KeyData);
    ShiftState := [];
    if not (csNoStdEvents in ControlStyle) 
    then begin
      KeyUp(CharCode, ShiftState);
      if CharCode = 0 then Exit;
    end;
    
    // TODO
    //if (CharCode = VK_APPS) and not (ssAlt in ShiftState) then
    //  CheckMenuPopup(SmallPoint(0, 0));
  end;
  Result := False;
end;

{------------------------------------------------------------------------------}
{  TWinControl CreateParams                                                      }
{------------------------------------------------------------------------------}
procedure TWinControl.CreateParams(var Params : TCreateParams);
begin
  FillChar(Params, SizeOf(Params),0);
  with Params do
  begin
    Caption := @FText;
    Style := WS_CHILD or WS_CLIPSIBLINGS;
    if (Parent <> nil) then WndParent := Parent.Handle;
  end;
end;

{------------------------------------------------------------------------------}
{  TWinControl Invalidate                                                      }
{------------------------------------------------------------------------------}
Procedure TWinControl.Invalidate;
Begin
  if HandleAllocated
  then CNSendMessage(LM_Invalidate,Self,Nil);
end;

{------------------------------------------------------------------------------}
{  TWinControl Repaint                                                         }
{------------------------------------------------------------------------------}
Procedure TWinControl.Repaint;
Begin
  if HandleAllocated
  then begin
    CNSendMessage(LM_PAINT, Self, nil);
  end;
end;


{------------------------------------------------------------------------------}
{  TWinControl DoMouseWheel  "Event Handler"                                     }
{------------------------------------------------------------------------------}
function TWinControl.DoMouseWheel(Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint): Boolean;
begin
  Result := False;
  
  if Assigned(FOnMouseWheel) 
  then FOnMouseWheel(Self, Shift, WheelDelta, MousePos, Result);
  
  if not Result 
  then begin
    if WheelDelta < 0 
    then Result := DoMouseWheelDown(Shift, MousePos)
    else Result := DoMouseWheelUp(Shift, MousePos);
  end;
end;

{------------------------------------------------------------------------------}
{  TWinControl DoMouseWheelDown  "Event Handler"                                     }
{------------------------------------------------------------------------------}
function TWinControl.DoMouseWheelDown(Shift: TShiftState; MousePos: TPoint): Boolean;
begin
   Result := False;
   if Assigned(FOnMouseWheelDown) then
     FOnMouseWheelDown(Self, Shift, MousePos, Result);
end;

{------------------------------------------------------------------------------}
{  TWinControl DoMouseWheelUp  "Event Handler"                                     }
{------------------------------------------------------------------------------}
function TWinControl.DoMouseWheelUp(Shift: TShiftState; MousePos: TPoint): Boolean;
begin
   Result := False;
   if Assigned(FOnMouseWheelUp) then
     FOnMouseWheelUp(Self, Shift, MousePos, Result);
end;

{------------------------------------------------------------------------------}
{  TWinControl Insert                                                      }
{------------------------------------------------------------------------------}
procedure TWinControl.Insert(AControl : TControl);
begin
  if AControl <> nil then
  begin
    if AControl = Self 
    then begin
      Assert(False, 'Trace:[TControl.SetParent] EInvalidOperation --> AControl = Self');
      raise EInvalidOperation.Create('A control can''t have itself as parent');
    end;

//    Assert(False, Format('Trace:[TWinControl.Insert] %s(%s) --> Insert: %s(%s)', [ClassName, Name, AControl.ClassName, AControl.Name]));
    if AControl is TWinControl then
    begin
      ListAdd(FWInControls, ACOntrol);
      ListAdd(FTabList, AControl);
    end;
//    else
      ListAdd(FControls, AControl);
//todo: MAKE COMPATIBLE --> a control is in wincontrols or in controls but not both
    AControl.FParent := Self;
  end;

End;

{------------------------------------------------------------------------------}
{  TWinControl ReAlign                                                          }
{------------------------------------------------------------------------------}
procedure TWinControl.ReAlign;
begin
  AlignControl(nil);
end;

{------------------------------------------------------------------------------}
{  TWinControl Remove                                                          }
{------------------------------------------------------------------------------}
Procedure TWinControl.Remove(AControl : TControl);
begin
  if AControl <> nil then
  begin
    Assert(False, Format('trace:[TWinControl.Remove] %s(%S) --> Remove: %s(%s)', [ClassName, Name, AControl.ClassName, AControl.Name]));
    if AControl is TWinControl then
    begin
      ListRemove(FTabList, AControl);
      ListRemove(FWInControls, ACOntrol);
    end;
//    else
      Listremove(FControls, AControl);
    AControl.FParent := Nil;
  end;
End;


{------------------------------------------------------------------------------}
{  TWinControl RemoveFocus                                                      }
{------------------------------------------------------------------------------}
Procedure TWinControl.RemoveFocus(Removing : Boolean);
Begin
//TODO: FINISH TWINCONTROL.REMOVEFOCUS
end;

{------------------------------------------------------------------------------}
{  TWinControl UpdateControlState                                                      }
{------------------------------------------------------------------------------}
procedure TWinControl.UpdateControlState;
var Control : TWinControl;
begin
  Control:= Self;
  { If any of the parent is not visible, exit }
  while Control.Parent <> nil do 
  begin
    Control:= Control.Parent;
    if not Control.Showing then Exit;
  end;

  if (Control is TCustomForm)
  or (Control.FParentWindow <> 0) 
  then UpdateShowing;


end;

{------------------------------------------------------------------------------}
{  TWinControl InsertControl                                                      }
{------------------------------------------------------------------------------}
Procedure TWinControl.InsertControl(AControl : TControl);
Begin
 AControl.ValidateContainer(Self);
  Perform(CM_CONTROLLISTCHANGE, Integer(AControl), Integer(True));
  Insert(AControl);
  if not (csReadingState in AControl.ControlState) then
  begin
    AControl.Perform(CM_PARENTCOLORCHANGED, 0, 0);
    AControl.Perform(CM_PARENTFONTCHANGED, 0, 0);
    AControl.Perform(CM_PARENTSHOWHINTCHANGED, 0, 0);
    AControl.Perform(CM_PARENTBIDIMODECHANGED, 0, 0);
    if AControl is TWinControl then
    begin
      AControl.Perform(CM_PARENTCTL3DCHANGED, 0, 0);
      UpdateControlState;
    end else
      if HandleAllocated then AControl.Invalidate;
    AlignControl(AControl);
  end;
  Perform(CM_CONTROLCHANGE, Integer(AControl), Integer(True));


End;

{------------------------------------------------------------------------------}
{  TWinControl removeControl                                                      }
{------------------------------------------------------------------------------}
Procedure TWinControl.RemoveControl(AControl : TControl);
Begin
  Perform(CM_CONTROLCHANGE, Integer(AControl), Integer(False));
  if AControl is TWinControl then
    with TWinControl(AControl) do
    begin
      RemoveFocus(True);
      DestroyHandle;
    end
  else
    if HandleAllocated then
      AControl.InvalidateControl(AControl.Visible, False);
  Remove(AControl);
//  Perform(CM_CONTROLLISTCHANGE, Integer(AControl), Integer(False));
//  Realign;
End;

{------------------------------------------------------------------------------}
{  TWinControl AlignControl                                                      }
{------------------------------------------------------------------------------}
procedure TWinControl.AlignControl(AControl : TControl);
var
  Num : Integer;
  Rect: TRect;
begin
  if not HandleAllocated or (csDestroying in ComponentState) then Exit;
  if FAlignLevel <> 0 then
    Include(FControlState, csAlignmentNeeded)
  else begin
    DisableAlign;
    try
      Rect:= GetClientRect;
      AlignControls(AControl, Rect);
    finally
      Exclude(FControlState, csAlignmentNeeded);
      EnableAlign;
    end;
  end;
{
case AControl.Align of
        alClient :   Begin
Assert(False,'Trace:Alignment is alClient') ;
                        AControl.Left := TControl(Owner).Left+1;
                        AControl.Top := TControl(Owner).Top+1;
                        AControl.Width := TControl(Owner).Width-1;
                        AControl.Height := TControl(Owner).Height-1;
                end;
        alNone : Begin
                  {put nothing in here}

                End;
        alBottom : Begin
                        AControl.Left := TControl(Owner).Left+1;
                        AControl.Top :=  TControl(Owner).Height - AControl.Height - 1;
                        AControl.Width := TControl(Owner).Width-1;
                   end;

        alTop : Begin
                        AControl.Width := TControl(Owner).Width-1;
                        AControl.Left := 1;
                        AControl.Top := 1;

                   end;

        alRight : Begin
                        AControl.Left := TControl(Owner).Width - AControl.Width - 1;
                        AControl.Height := TControl(Owner).Height -1;
                        AControl.Top := 1;
                   end;

        alLeft : Begin
                        AControl.Left := 1;
                        AControl.Height := TControl(Owner).Height -1;
                        AControl.Top := 1;
                   end;

        end;
}
End;


{------------------------------------------------------------------------------}
{  TWinControl GetControl                                                      }
{------------------------------------------------------------------------------}
function TWinControl.GetControl(const Index: Integer): TControl;
begin
  Result := TControl(FControls.Items[Index]);
end;

{------------------------------------------------------------------------------}
{  TWinControl GetControlCount                                                 }
{------------------------------------------------------------------------------}
function TWinControl.GetControlCount: Integer;
begin
  if FControls = nil 
  then Result := 0
  else Result := FControls.Count;
end;

{------------------------------------------------------------------------------}
{  TWinControl GetHandle                                                 }
{------------------------------------------------------------------------------}
function TWinControl.GetHandle: HWND;
begin
  if not HandleAllocated then Assert(False, Format('Trace:[TWinControl.GetHandle] %s(%s)', [ClassNAme, Name]));
  HandleNeeded;
  Result := FHandle;
end;


{------------------------------------------------------------------------------
  Method: TWinControl.Create
  Params:  None
  Returns: Nothing

  Contructor for the class.
 ------------------------------------------------------------------------------}
constructor TWinControl.Create(AOwner : TComponent);
begin
Assert(False,'Trace:in TWinControl Constructor');
  inherited Create(AOwner);
  FCompStyle := csFixed;
  FBrush := nil;

Assert(False,'Trace:TWinControl Constructor inherited create complete');
end;

{------------------------------------------------------------------------------}
{  TWinControl CreateParented                                                  }
{------------------------------------------------------------------------------}
constructor TWinControl.CreateParented(ParentWindow: hwnd);
begin
   FParentWindow := ParentWindow;
   inherited Create(nil);
end;

{------------------------------------------------------------------------------}
{  TWinControl CreateParentedControl                                           }
{------------------------------------------------------------------------------}
class function TWinControl.CreateParentedControl(ParentWindow : hwnd): TWinControl;
begin
end;

{------------------------------------------------------------------------------}
{  TWinControl Hide                                                            }
{------------------------------------------------------------------------------}
procedure TWinControl.Hide;
begin
   Visible := False;
end;

{------------------------------------------------------------------------------}
{  TWinControl Show                                                            }
{------------------------------------------------------------------------------}
procedure TWinControl.Show;
begin
   Assert(False,Format('Trace: [TWinControl.Show] %s(%s)', [ClassName, Name]));
   Visible := True;
end;

{------------------------------------------------------------------------------
  Method: TWinControl.Destroy
  Params:  None
  Returns: Nothing

  Destructor for the class.
 ------------------------------------------------------------------------------}
destructor TWinControl.Destroy;
var
  n: Integer;
  Control: TControl;
begin
  DestroyHandle; 

  n := ControlCount;

  while n > 0 do
  begin
    Control := Controls[n - 1];
    Remove(Control);
    // don't free the control just set parent to nil
    // controls are freed by the owner
    //Control.Free;
    Control.Parent := nil;
    n := ControlCount;
  end;

  FBrush.Free;
  inherited Destroy;
end;

{------------------------------------------------------------------------------
  Method: TWinControl.DoEnter
  Params: none
  Returns: Nothing

  Call user's callback for OnEnter.
 ------------------------------------------------------------------------------}
procedure TWinControl.DoEnter;
begin
  if Assigned(FOnEnter) then FOnEnter(Self);
end;

{------------------------------------------------------------------------------
  Method: TWinControl.DoExit
  Params: none
  Returns: Nothing

  Call user's callback for OnExit.
 ------------------------------------------------------------------------------}
procedure TWinControl.DoExit;
begin
  if Assigned(FOnExit) then FOnExit(Self);
end;

{------------------------------------------------------------------------------
  Method: TWinControl.CMEnabledChanged
  Params: Message
  Returns: Nothing

  Called when enabled is changed. Takes action to enable control
 ------------------------------------------------------------------------------}
procedure TWinControl.CMEnabledChanged(var Message: TLMEssage);
begin
  if not Enabled and (Parent <> nil) 
  then RemoveFocus(False);
  
  if HandleAllocated 
  and not (csDesigning in ComponentState) 
  then EnableWindow(FHandle, Enabled);
end;

{------------------------------------------------------------------------------
  Method: TWinControl.WMSetFocus
  Params: Message
  Returns: Nothing

  SetFocus event handler
 ------------------------------------------------------------------------------}
Procedure TWinControl.WMSetFocus(var Message : TLMSetFocus);
Begin
  Assert(False, Format('Trace:TODO: [TWinControl.LMSetFocus] %s', [ClassName]));
end;

{------------------------------------------------------------------------------
  Method: TWinControl.WMPaint
  Params:   Msg: The paint message
  Returns:  nothing

  Paint event handler.
 ------------------------------------------------------------------------------}
procedure TWinControl.WMPaint(var Msg: TLMPaint);
var
  dc,Memdc : hdc;
  MemBitmap, OldBitmap : HBITMAP;
  PS : TPaintStruct;
  I : Integer;
begin
  Assert(False, Format('Trace:> [TWinControl.WMPaint] %s Msg.DC: 0x%x', [ClassName, Msg.DC]));
  if (Msg.DC <> 0) then
  begin
    if not (csCustomPaint in ControlState) and (ControlCount = 0) then
      begin
      // no inherited method to call...
      end
    else
      PaintHandler(Msg);
  end
  else begin
    DC := GetDC(0);
    MemBitmap := CreateCompatibleBitmap(DC, ClientRect.Right, ClientRect.Bottom);
    ReleaseDC(0, DC);
    MemDC := CreateCompatibleDC(0);
    OldBitmap := SelectObject(MemDC, MemBitmap);
    try
      DC := BeginPaint(Handle, PS);
      //ToDO:define wm_erasebkgnd
//      Perform(WM_ERASEBKGND, MemDC, MemDC);
      Msg.DC := MemDC;
      WMPaint(Msg);
      Msg.DC := 0;
      //TODO:bitblt
      BitBlt(DC, 0, 0, ClientRect.Right, ClientRect.Bottom, MemDC, 0, 0, SRCCOPY);
      EndPaint(Handle, PS);
    finally
      SelectObject(MemDC, OldBitmap);
      DeleteDC(MemDC);
      DeleteObject(MemBitmap);
    end;
  end;
  Assert(False, Format('Trace:< [TWinControl.WMPaint] %s', [ClassName]));
end;

{------------------------------------------------------------------------------
  Method: TWinControl.WMDestroy
  Params:   Msg: The destroy message
  Returns:  nothing

  event handler.
 ------------------------------------------------------------------------------}
procedure TWinControl.WMDestroy(var Message: TLMDestroy);
begin            
  Assert(False, Format('Trace: [TWinControl.LMDestroy] %s', [ClassName]));
  
  // Our widget/window doesn't exist anymore
  FHandle := 0;
end;                          

{------------------------------------------------------------------------------
  Method: TWinControl.WMSize
  Params:   Msg: The message
  Returns:  nothing

  event handler.
 ------------------------------------------------------------------------------}
procedure TWinControl.WMSize(Var Message : TLMSize);
begin
  Assert(False, Format('Trace:[TWinControl.WMSize] %s', [ClassName]));
  { Just coordinate the bounds }
  FWidth  := Message.Width;
  FHeight := Message.Height;
//  Realign;
  if not (csLoading in ComponentState) then Resize;
end;

{------------------------------------------------------------------------------
  Method: TWinControl.LMMove
  Params:   Msg: The message
  Returns:  nothing

  event handler.
 ------------------------------------------------------------------------------}
procedure TWinControl.WMMove(var Message: TLMMove);
begin
  { Just sync the coordinates }
  FLeft := Message.XPos;
  FTop  := Message.YPos;
  { TODO : When anchors are implemented, update its rules instead }
  RequestAlign;
end;                          

{------------------------------------------------------------------------------
  Method: TWinControl.WMNofity
  Params:   Msg: The message
  Returns:  nothing

  event handler.
 ------------------------------------------------------------------------------}
procedure TWInControl.WMNotify(var Message: TLMNotify);
Begin
if not DoControlMsg(Message.NMHdr^.hwndfrom,Message) then exit;
//Inherited ;
end;

{------------------------------------------------------------------------------
  Method: TWinControl.LMKillFocus
  Params:   Msg: The message
  Returns:  nothing

  event handler.
 ------------------------------------------------------------------------------}
procedure TWinControl.WMKillFocus(var Message: TLMKillFocus);
begin            
  Assert(False, Format('Trace: TODO: [TWinControl.LMKillFocus] %s', [ClassName]));
end;                          

{------------------------------------------------------------------------------
  Method: TWinControl.WMShowWindow
  Params:   Msg: The message
  Returns:  nothing

  event handler.
 ------------------------------------------------------------------------------}
procedure TWinControl.WMShowWindow(var Message: TLMShowWindow);
begin            
  Assert(False, Format('Trace: TODO: [TWinControl.LMShowWindow] %s', [ClassName]));
end;                          

{------------------------------------------------------------------------------
  Method: TWinControl.WMEnter
  Params:   Msg: The message
  Returns:  nothing

  event handler.
 ------------------------------------------------------------------------------}
procedure TWinControl.WMEnter(var Message: TLMEnter);
begin            
  Assert(False, Format('Trace: TODO: [TWinControl.LMEnter] %s', [ClassName]));
end;                          

{------------------------------------------------------------------------------
  Method: TWinControl.WMEraseBkgnd
  Params:   Msg: The message
  Returns:  nothing

  event handler.
 ------------------------------------------------------------------------------}
procedure TWinControl.WMEraseBkgnd(var Message: TLMEraseBkgnd);
begin            
  Assert(False, Format('Trace: TODO: [TWinControl.LMEraseBkgnd] %s', [ClassName]));
end;                          

{------------------------------------------------------------------------------
  Method: TWinControl.WMExit
  Params:   Msg: The message
  Returns:  nothing

  event handler.
 ------------------------------------------------------------------------------}
procedure TWinControl.WMExit(var Message: TLMExit);
begin            
  Assert(False, Format('Trace: TODO: [TWinControl.LMExit] %s', [ClassName]));
end;                          

{------------------------------------------------------------------------------
  Method: TWinControl.WMMouseWheel
  Params:   Msg: The message
  Returns:  nothing

  event handler.
 ------------------------------------------------------------------------------}
procedure TWinControl.WMMouseWheel(var Message: TLMMouseEvent);
Var
  Button : TMouseButton;
  MousePos : TPoint;
  Shift : TShiftState;
begin            
  Assert(False, Format('Trace: [TWinControl.LMMouseWheel] %s', [ClassName]));

  MousePos.X := Message.X;
  MousePos.Y := Message.Y;

  { always the middle button }
  Shift := [ssMiddle];

  DoMouseWheel(Shift, Message.WheelDelta, MousePos);
end;                          

{------------------------------------------------------------------------------
  Method: TWinControl.WMChar
  Params:   Msg: The message
  Returns:  nothing

  event handler.
 ------------------------------------------------------------------------------}
procedure TWinControl.WMChar(var Message: TLMChar);
begin            
  Assert(False, Format('Trace:[TWinControl.WMChar] %s', [ClassName]));
  if not DoKeyPress(Message) then {inherited}; // there is nothing to inherit
end;                          

{------------------------------------------------------------------------------
  Method: TWinControl.WMKeyDown
  Params:   Msg: The message
  Returns:  nothing

  event handler.
 ------------------------------------------------------------------------------}
Procedure TWinControl.WMKeyDown(Var Message : TLMKeyDown);
Begin
  Assert(False, Format('Trace:[TWinControl.WMKeyDown] %s', [ClassName]));
  if not DoKeyDown(Message) then {inherited} ;  // there is nothing to inherit
end;

{------------------------------------------------------------------------------
  Method: TWinControl.WMKeyUp
  Params:   Msg: The message
  Returns:  nothing

  event handler.
 ------------------------------------------------------------------------------}
Procedure TWinControl.WMKeyUp(Var Message : TLMKeyUp);
Begin
  Assert(False, Format('Trace:[TWinControl.WMKeyUp] %s', [ClassName]));
  if not DoKeyUp(Message) then {inherited}; // there is nothing to inherit
end;

{------------------------------------------------------------------------------
  Method: TWinControl.WMWindowPosChanged
  Params:   Msg: The message
  Returns:  nothing

  event handler.
 ------------------------------------------------------------------------------}
procedure TWinControl.WMWindowPosChanged(var Message : TLMWindowPosChanged);
begin

  if Message.WindowPos <> nil 
  then with Message.WindowPos^ do 
  begin
    FLeft := X;
    FWidth := cX; 
    FTop := Y;
    FHeight := cY;
    Assert(False, Format('Trace:[TWinControl.WMWindowPosChanged] %s --> Message.WindowPos(%d, %d)(%d, %d)', [ClassName, X, Y, cx, cy]));
  end
  else Assert(False, Format('Trace:[TWinControl.WMWindowPosChanged] %s --> Message.WindowPos = nil', [ClassName]));


  inherited WMWindowPosChanged(Message);
end;


{------------------------------------------------------------------------------
  Function: TWinControl.HandleAllocated
  Params:   None
  Returns:  True is handle is allocated

  Checks if a handle is allocated. I.E. if the control is created
 ------------------------------------------------------------------------------}
function TWinControl.HandleAllocated : Boolean;
begin
   HandleAllocated := (FHandle <> 0);
end;

{------------------------------------------------------------------------------
  Method:  TWinControl.CreateHandle
  Params:  None
  Returns: Nothing

  Creates the handle ( = object).
 ------------------------------------------------------------------------------}
procedure TWinControl.CreateHandle;
begin
  if (not HandleAllocated) then CreateWnd;
end;

{------------------------------------------------------------------------------
  Method:  TWinControl.CreateWnd
  Params:  None
  Returns: Nothing

  Creates the interface object.
 ------------------------------------------------------------------------------}
procedure TWinControl.CreateWnd;
var
  Params: TCreateParams;
  n: Integer;
begin
  if (FCompstyle = csNone) then 
  begin
    WriteLn(Format('WARNING: [TWinControl.CreateWnd] %s --> FCompstyle = csNone', [ClassName]));
    Exit;
  end;

  CreateParams(Params);
  with Params do begin
    if (WndParent = 0) and (Style and WS_CHILD <> 0) then begin
//      raise EInvalidOperation.CreateFmt('Control ''%s'' has no parent', [Name]);
      exit;
    end;
    Assert((parent <> nil) or (WndParent = 0), 'TODO: find parent if parent=nil and WndParent <> 0');
  end;
  
  CreateComponent(nil);
  if Parent <> nil then AddControl;
  
  AttachSignals;
  InitializeWnd;  

  if FWInControls <> nil then begin
    for n := 0 to FWInControls.Count - 1 do
    with TWinControl(FWInControls.Items[n]) do
    if Visible then begin
      HandleNeeded;
    end;
  end;
end;

{------------------------------------------------------------------------------
  Method:  TWinControl.InitializeWnd
  Params:  none
  Returns: Nothing

  Gets called after the window is created, but before the owned controls are 
  created. Place cached property code here.
 ------------------------------------------------------------------------------}
procedure TWinControl.InitializeWnd;
var 
  R: TRect;
  pStr: PChar;
begin
  Assert(False, Format('Trace:[TWinControl.InitializeWnd]  %s', [ClassName]));
  // set all cached properties
  
  // MWE:All of this should be handled to the interface create routine
  Assert(False, 'Trace:TODO: [TWinControl.InitializeWnd] move this code to the interface');
  
  R:= Rect(Left, Top, Width, Height);
  CNSendMessage(LM_SETSIZE, Self, @R);
  CNSendMessage(LM_SHOWHIDE, Self, nil);
  CNSendMessage(LM_SETCOLOR, Self, nil);
  
  EnableWindow(Handle, Enabled);
 
  //We shouldn't NEED to create our own PCHAR.  We should be able
  //to typecast Caption as a PCHAR but it doesn't work.
  pStr := StrAlloc(Length(FCaption) + 1);
  try
    StrPCopy(pStr, FCaption);
    SetTextBuf(pStr);
  finally
    strDispose(pStr);
  end;
  Assert(False, 'Trace:SETPROP**********************************************');

 SetProp(Handle,'WinControl',TWinControl(Self));
 SetProp(Handle,'Control',TControl(Self));

end;

{------------------------------------------------------------------------------
  Method:  TWinControl.AttachSignals
  Params:  none
  Returns: Nothing

  Gets called after the window is created but before the controls are created 
  and the cached propeties are set. 
  This is the only place where a call to SetCallBack is made.
 ------------------------------------------------------------------------------}
procedure TWinControl.AttachSignals; 
begin
  Assert(False, Format('trace:[TWinControl.AttachSignals] %s', [ClassName]));
  // Attach callbacks
  SetCallback(LM_DESTROY);
  SetCallback(LM_SHOWWINDOW);
  SetCallback(LM_FOCUS);
//Obsolete ??  SetCallback(LM_SIZEALLOCATE); 
  SetCallback(LM_WINDOWPOSCHANGED);
  SetCallback(LM_PAINT);
  SetCallback(LM_EXPOSEEVENT);
//SetCallback(LM_CONFIGUREEVENT); // In TCustomForm
  SetCallback(LM_KEYDOWN);
  SetCallback(LM_KEYUP);
  SetCallback(LM_CHAR);
  SetCallback(LM_MOUSEMOVE);
  SetCallback(LM_LBUTTONDOWN); 
  SetCallback(LM_LBUTTONUP); 
  SetCallback(LM_RBUTTONDOWN);
  SetCallback(LM_RBUTTONUP);
  SetCallback(LM_MBUTTONDOWN);
  SetCallback(LM_MBUTTONUP); 
  SetCallback(LM_MOUSEWHEEL);

  if FCompStyle = csFixed 
  then begin
    SetCallback(LM_HSCROLL); 
    SetCallback(LM_VSCROLL); 
  end;
  

{  *** These need to be implemented yet
   hide
   state-changed
}
end;

{------------------------------------------------------------------------------
  Method:  TWinControl.DetachSignals
  Params:  none
  Returns: Nothing

  Gets called the moment the window is about to be destroyed. All callbacks 
  should be removed here.
 ------------------------------------------------------------------------------}
procedure TWinControl.DetachSignals; 
begin
  RemoveCallbacks;
end;

{------------------------------------------------------------------------------
  Method:  TWinControl.DestroyWnd
  Params:  None
  Returns: Nothing

  Creates the interface object.
 ------------------------------------------------------------------------------}
procedure TWinControl.DestroyWnd; 
begin
  if FHandle <> 0 
  then begin
    DetachSignals;
    DestroyComponent;
    FHandle := 0;
  end;
end;

{------------------------------------------------------------------------------
  Method:  TWinControl.HandleNeeded
  Params:  AOwner: the owner of the class
  Returns: Nothing

  Description of the procedure for the class.
 ------------------------------------------------------------------------------}
procedure TWinControl.HandleNeeded;
begin
  if not HandleAllocated then
  begin
    if Parent = Self 
    then begin
      Assert(False, Format('Trace:[TWinControl.HandleNeeded] Sombody set Parent := Self in %s. DONT DO THAT !!', [Classname]));
    end
    else begin
      if (Parent <> nil) 
      then  Parent.HandleNeeded;
    end;
    CreateHandle;
  end;
end;

{------------------------------------------------------------------------------
  Method:  TWinControl.SetBounds
  Params:  aLeft, aTop, aWidth, aHeight
  Returns: Nothing

  Sets the bounds of the control.
 ------------------------------------------------------------------------------}
procedure TWinControl.SetBounds(aLeft, aTop, aWidth, aHeight : integer);
var
  R : TRect;
begin
  if (ALeft = Left) and (ATop = Top) and (AWidth = Width) and (AHeight = Height) then Exit;

  if HandleAllocated
  then begin
    R:= Rect(ALeft, ATop, AWidth, AHeight);
    CNSendMessage(LM_SetSize, Self, @R);
    FLeft := aLeft;
    FTop := aTop;
    FWidth := aWidth;
    FHeight := aHeight;
  end
  else inherited SetBounds(aLeft, aTop, aWidth, aHeight);
end;

{------------------------------------------------------------------------------
  Method:  TWinControl.Text
  Params:  Value: the text to be set
  Returns: Nothing

  Sets the text/caption of a control
 ------------------------------------------------------------------------------}
procedure TWinControl.SetText(const Value: TCaption);
begin
  if HandleAllocated
  then inherited SetText(Value)
  else FCaption := Value;
end;

{------------------------------------------------------------------------------
  Method:  TWinControl.SetHint
  Params:  Value: the text of the hint to be set
  Returns: Nothing

  Sets the hint text of a control
 ------------------------------------------------------------------------------}
procedure TWinControl.SetHint(const Value: String);
begin
  if FHint <> Value then
  begin
    inherited SetHint(Value);
    if HandleAllocated then InterfaceObject.UpdateHint(Self);
  end;
end;

{------------------------------------------------------------------------------
  Method:  TWinControl.GetDeviceContext
  Params:  WindowHandle: the windowhandle of this control
  Returns: a Devicecontext

  Get the devicecontext for this WinControl.
 ------------------------------------------------------------------------------}
function TWinControl.GetDeviceContext(var WindowHandle: HWnd): HDC;
begin
(*
  if Handle = 0 
  then begin
    // we aren't created yet
    Result := 0;
    WindowHandle := 0;
  end
  else begin
    Result := GetDC(FHandle);
    if Result = 0 
    then raise EOutOfResources.Create('Error creating device context');
  end;
  WindowHandle := FHandle;
(*)
  Result := GetDC(Handle);
  if Result = 0 
  then raise EOutOfResources.Create('Error creating device context');

  WindowHandle := FHandle;
//*)  
end;

{------------------------------------------------------------------------------
  Method:  TWinControl.CMVisibleChanged
  Params:  Message : not used
  Returns: nothing

  Performs actions when visibility has changed
 ------------------------------------------------------------------------------}
procedure TWinControl.CMVisibleChanged(var Message : TLMessage);
begin

  if not FVisible and (Parent <> nil)
  then RemoveFocus(False);
  
  if not (csDesigning in ComponentState) 
  or (csNoDesignVisible in ControlStyle) 
  then UpdateControlState;
end;

{------------------------------------------------------------------------------
  Method:  TWinControl.CMShowHintChanged
  Params:  Message : not used
  Returns: nothing

  Notifies that the hint visibility is changed
 ------------------------------------------------------------------------------}
procedure TWinControl.CMShowHintChanged(var Message: TLMessage);
begin
  if HandleAllocated then InterfaceObject.UpdateHint(Self);
end;

{------------------------------------------------------------------------------
  Method:  TWinControl.CMShowingChanged
  Params:  Message : not used
  Returns: nothing

  Shows or hides a control
 ------------------------------------------------------------------------------}
procedure TWinControl.CMShowingChanged(var Message: TLMessage);
begin
{  if (TWinControl(Self).HandleAllocated) or (Self is TCustomForm) then}
     CNSendMessage(LM_ShowHide, Self, nil);
  
//  SetWindowPos(FHandle, 0, 0, 0, 0, 0, ShowFlags[FShowing]);
end;
{------------------------------------------------------------------------------
  Method:  TWinControl.ShowControl
  Params:  AControl: Control to show
  Returns: nothing

  Askes the parent to show ourself.
 ------------------------------------------------------------------------------}
procedure TWinControl.ShowControl(AControl: TControl);
begin
  if Parent <> nil then Parent.ShowControl(Self);
end;


{$IFDEF ASSERT_IS_ON}
  {$UNDEF ASSERT_IS_ON}
  {$C-}
{$ENDIF}
{ =============================================================================

  $Log$
  Revision 1.9  2001/01/15 18:25:51  lazarus
  Fixed a stupid error I caused by using a variable as an index in main.pp and this variable sometimes caused an exception because the index was out of range.
  Shane

  Revision 1.8  2001/01/12 20:22:09  lazarus
  Shiftstate fixed so it reports ssCtrl and ssShift now.
  You can use Shift-Ctrl-Up and Down to jump to procedures in the code explorer.
  Shane

  Revision 1.7  2001/01/09 18:23:21  lazarus
  Worked on moving controls.  It's just not working with the X and Y coord's I'm getting.
  Shane

  Revision 1.6  2000/12/29 18:33:54  lazarus
  TStatusBar's create and destroy were not set to override TWinControls so they were never called.
  Shane

  Revision 1.5  2000/12/29 13:14:05  lazarus
  Using the lresources.pp and registering components.
  This is a major change but will create much more flexibility for the IDE.
  Shane

  Revision 1.4  2000/12/20 17:35:58  lazarus
  Added GetChildren
  Shane

  Revision 1.3  2000/09/10 23:08:30  lazarus
  MWE:
    + Added CreateCompatibeleBitamp function
    + Updated TWinControl.WMPaint
    + Added some checks to avoid gtk/gdk errors
    - Removed no fixed warning from GetDC
    - Removed some output

  Revision 1.2  2000/07/30 21:48:32  lazarus
  MWE:
    = Moved ObjectToGTKObject to GTKProc unit
    * Fixed array checking in LoadPixmap
    = Moved LM_SETENABLED to API func EnableWindow and EnableMenuItem
    ~ Some cleanup

  Revision 1.1  2000/07/13 10:28:28  michael
  + Initial import

  Revision 1.16  2000/07/09 20:18:56  lazarus
  MWE:
    + added new controlselection
    + some fixes
    ~ some cleanup

  Revision 1.15  2000/06/28 13:11:37  lazarus
  Fixed TNotebook so it gets page change events.  Shane

  Revision 1.14  2000/06/19 18:21:22  lazarus
  Spinedit was never getting created
  Shane

  Revision 1.13  2000/06/16 13:33:21  lazarus
  Created a new method for adding controls to the toolbar to be dropped onto the form!
  Shane

  Revision 1.12  2000/06/01 21:53:19  lazarus
  MWE:
    + Added check for HandleCreated in CMShowHintChanged

  Revision 1.11  2000/05/27 22:20:55  lazarus
  MWE & VRS:
    + Added new hint code

  Revision 1.10  2000/05/17 22:34:07  lazarus
  MWE:
    * Fixed Sizing & events

  Revision 1.9  2000/05/14 21:56:11  lazarus
  MWE:
    + added local messageloop
    + added PostMessage
    * fixed Peekmessage
    * fixed ClientToScreen
    * fixed Flat style of Speedutton (TODO: Draw)
    + Added TApplicatio.OnIdle

  Revision 1.8  2000/05/10 22:52:58  lazarus
  MWE:
    = Moved some global api stuf to gtkobject

  Revision 1.7  2000/05/09 12:52:03  lazarus
  *** empty log message ***

  Revision 1.6  2000/05/09 02:07:40  lazarus
  Replaced writelns with Asserts.                          CAW

  Revision 1.5  2000/05/08 16:07:32  lazarus
  fixed screentoclient and clienttoscreen
  Shane

  Revision 1.4  2000/04/10 15:05:30  lazarus
  Modified the way the MOuseCapture works.
  Shane

  Revision 1.2  2000/04/07 16:59:55  lazarus
  Implemented GETCAPTURE and SETCAPTURE along with RELEASECAPTURE.
  Shane

  Revision 1.1  2000/04/02 20:49:57  lazarus
  MWE:
    Moved lazarus/lcl/*.inc files to lazarus/lcl/include

  Revision 1.77  2000/03/30 18:07:55  lazarus
  Added some drag and drop code
  Added code to change the unit name when it's saved as a different name.  Not perfect yet because if you are in a comment it fails.

  Shane

  Revision 1.76  2000/03/21 23:47:33  lazarus
  MWE:
    + Added TBitmap.MaskHandle & TGraphic.Draw & TBitmap.Draw

  Revision 1.75  2000/03/15 00:51:58  lazarus
  MWE:
    + Added LM_Paint on expose
    + Added forced creation of gdkwindow if needed
    ~ Modified DrawFrameControl
    + Added BF_ADJUST support on DrawEdge
    - Commented out LM_IMAGECHANGED in TgtkObject.IntSendMessage3
       (It did not compile)

  Revision 1.74  2000/03/14 19:49:05  lazarus
  Modified the painting process for TWincontrol.  Now it runs throug it's FCONTROLS list and paints all them
  Shane

  Revision 1.73  2000/03/09 23:48:10  lazarus
  MWE:
    * Fixed colorcache
    * Fixed black window in new editor
    ~ Did some cosmetic stuff

  From Peter Dyson <peter@skel.demon.co.uk>:
    + Added Rect api support functions
    + Added the start of ScrollWindowEx

  Revision 1.72  2000/03/08 23:57:39  lazarus
  MWE:
    Added SetSysColors
    Fixed TEdit text bug (thanks to hans-joachim ott <hjott@compuserve.com>)
    Finished GetKeyState
    Added changes from Peter Dyson <peter@skel.demon.co.uk>
    - a new GetSysColor
    - some improvements on ExTextOut

  Revision 1.71  2000/03/03 22:58:27  lazarus
  MWE:
    Fixed focussing problem.
      LM-FOCUS was bound to the wrong signal
    Added GetKeyState api func.
      Now LCL knows if shift/trl/alt is pressed (might be handy for keyboard
      selections ;-)

  Revision 1.70  2000/03/01 00:41:03  lazarus
  MWE:
    Fixed updateshowing problem
    Added some debug code to display the name of messages
    Did a bit of cleanup in main.pp to get the code a bit more readable
      (my editor does funny things with tabs if the indent differs)

  Revision 1.69  2000/02/28 00:15:54  lazarus
  MWE:
    Fixed creation of visible componets at runtime. (when a new editor
      was created it didn't show up)
    Made the hiding/showing of controls more delphi compatible

  Revision 1.68  2000/02/26 23:31:50  lazarus
  MWE:
    Fixed notebook crash on insert
    Fixed loadfont problem for win32 (tleast now a fontname is required)

  Revision 1.67  2000/02/25 19:28:34  lazarus
  Played with TNotebook to see why it crashes when I add a tab and the tnotebook is showing.  Havn't figured it out
  Shane

  Revision 1.66  2000/02/22 23:26:13  lazarus
  MWE: Fixed cursor movement in editor
       Started on focus problem

  Revision 1.65  2000/02/22 22:19:50  lazarus
  TCustomDialog is a descendant of TComponent.
  Initial cuts a form's proper Close behaviour.

  Revision 1.64  2000/02/22 17:32:49  lazarus
  Modified the ShowModal call.
  For TCustomForm is simply sets the visible to true now and adds fsModal to FFormState.  In gtkObject.inc FFormState is checked.  If it contains fsModal then either gtk_grab_add or gtk_grab_remove is called depending on the value of VISIBLE.

  The same goes for TCustomDialog (open, save, font, color).
  I moved the Execute out of the individual dialogs and moved it into TCustomDialog and made it virtual because FONT needs to set some stuff before calling the inherited execute.
  Shane

  Revision 1.63  2000/02/20 20:13:47  lazarus
  On my way to make alignments and stuff work :-)

  Revision 1.62  2000/02/19 18:11:59  lazarus
  More work on moving, resizing, forms' border style etc.

  Revision 1.61  2000/02/18 19:38:53  lazarus
  Implemented TCustomForm.Position
  Better implemented border styles. Still needs some tweaks.
  Changed TComboBox and TListBox to work again, at least partially.
  Minor cleanups.

  Revision 1.60  2000/01/18 21:47:00  lazarus
  Added OffSetRec

  Revision 1.59  2000/01/10 00:07:13  lazarus
  MWE:
    Added more scrollbar support for TWinControl
    Most signals for TWinContorl are jet connected to the wrong widget
      (now scrolling window, should be fixed)
    Added some cvs entries

  Revision 1.58  2000/01/04 21:00:34  lazarus
  *** empty log message ***

  Revision 1.57  2000/01/03 00:19:21  lazarus
  MWE:
    Added keyup and buttonup events
    Added LM_MOUSEMOVE callback
    Started with scrollbars in editor

  Revision 1.56  2000/01/02 00:29:27  lazarus
  Stoppok:
    - safety check if fCompStyle <> csNone before call to CreateHandle

  Revision 1.55  1999/12/31 14:58:01  lazarus
  MWE:
    Set unkown VK_ codesto 0
    Added pfDevice support for bitmaps

  Revision 1.54  1999/12/23 21:48:13  lazarus
  *** empty log message ***

  Revision 1.52  1999/12/22 01:16:04  lazarus
  MWE:
    Changed/recoded keyevent callbacks
    We Can Edit!
    Commented out toolbar stuff

  Revision 1.51  1999/12/21 21:35:54  lazarus
  committed the latest toolbar code.  Currently it doesn't appear anywhere and I have to get it to add buttons correctly through (I think) setstyle.  I think I'll implement the LM_TOOLBARINSERTBUTTON call there.
  Shane

  Revision 1.50  1999/12/21 00:07:06  lazarus
  MWE:
    Some fixes
    Completed a bit of DraWEdge

  Revision 1.49  1999/12/20 21:01:14  lazarus
  Added a few things for compatability with Delphi and TToolbar
  Shane

  Revision 1.48  1999/12/18 18:27:32  lazarus
  MWE:
    Rearranged some events to get a LM_SIZE, LM_MOVE and LM_WINDOWPOSCHANGED
    Initialized the TextMetricstruct to zeros to clear unset values
    Get mwEdit to show more than one line
    Fixed some errors in earlier commits

  Revision 1.47  1999/12/14 21:16:26  lazarus
  Added Autosize to TControl
  Shane

  Revision 1.46  1999/12/14 00:16:43  lazarus
  MWE:
    Renamed LM... message handlers to WM... to be compatible and to
      get more edit parts to compile
    Started to implement GetSystemMetrics
    Removed some Lazarus specific parts from mwEdit

  Revision 1.45  1999/12/10 00:47:01  lazarus
  MWE:
    Fixed some samples
    Fixed Dialog parent is no longer needed
    Fixed (Win)Control Destruction
    Fixed MenuClick

  Revision 1.44  1999/12/08 21:42:37  lazarus
  Moved more messages over to wndproc.
  Shane

  Revision 1.43  1999/12/08 00:56:07  lazarus
  MWE:
    Fixed menus. Events aren't enabled yet (dumps --> invalid typecast ??)

  Revision 1.42  1999/12/07 01:19:26  lazarus
  MWE:
    Removed some double events
    Changed location of SetCallBack
    Added call to remove signals
    Restructured somethings
    Started to add default handlers in TWinControl
    Made some parts of TControl and TWinControl more delphi compatible
    ... and lots more ...


  Revision 1.41  1999/12/03 00:26:47  lazarus
  MWE:
    fixed control location
    added gdiobject reference counter

  Revision 1.40  1999/12/02 19:00:59  lazarus
  MWE:
    Added (GDI)Pen
    Changed (GDI)Brush
    Changed (GDI)Font (color)
    Changed Canvas to use/create pen/brush/font
    Hacked mwedit to allow setting the number of chars (till it get a WM/LM_SIZE event)
    The editor shows a line !

  Revision 1.39  1999/11/30 21:30:06  lazarus
  Minor Issues
  Shane

  Revision 1.38  1999/11/25 23:45:08  lazarus
  MWE:
    Added font as GDIobject
    Added some API testcode to testform
    Commented out some more IFDEFs in mwCustomEdit

  Revision 1.37  1999/11/19 01:09:43  lazarus
  MWE:
    implemented TCanvas.CopyRect
    Added StretchBlt
    Enabled creation of TCustomControl.Canvas
    Added a temp hack in TWinControl.Repaint to get a LM_PAINT

  Revision 1.36  1999/11/17 01:16:40  lazarus
  MWE:
    Added some more API stuff
    Added an initial TBitmapCanvas
    Added some DC stuff
    Changed and commented out, original gtk linedraw/rectangle code. This
      is now called through the winapi wrapper.

  Revision 1.35  1999/11/05 17:48:17  lazarus
  Added a mwedit1 component to lazarus (MAIN.PP)
  It crashes on create.
  Shane

  Revision 1.34  1999/11/04 21:52:08  lazarus
  wndproc being used a little
  Shane

  Revision 1.33  1999/11/01 01:28:30  lazarus
  MWE: Implemented HandleNeeded/CreateHandle/CreateWND
       Now controls are created on demand. A call to CreateComponent shouldn't
       be needed. It is now part of CreateWnd

  Revision 1.32  1999/10/30 17:03:15  lazarus
  MWE: Typo

  Revision 1.31  1999/10/30 16:42:12  lazarus
  MWE: Moved the Parent <> self check to the Parent property

  Revision 1.30  1999/10/30 12:30:02  peter
    * fixed some stupid crashes

  Revision 1.29  1999/10/28 23:48:57  lazarus
  MWE: Added new menu classes and started to use handleneeded

  Revision 1.28  1999/10/28 19:25:10  lazarus
  Added a ton of messaging stuff
  Shane

  Revision 1.27  1999/10/28 17:17:43  lazarus
  Removed references to FCOmponent.
  Shane

  Revision 1.26  1999/10/27 17:27:08  lazarus
  Added alot of changes and TODO: statements
  shane

  Revision 1.25  1999/10/25 21:07:49  lazarus
  Many changes for compatability made again..

  Shane

  Revision 1.24  1999/10/25 17:38:52  lazarus
  More stuff added for compatability.  Most stuff added was put in the windows.pp file.  CONST scroll bar messages and such.  2 functions were also added to that unit that needs to be completed.
  Shane

  Revision 1.23  1999/10/25 15:33:54  lazarus
  Added a few more procedures for compatability.
  Shane

  Revision 1.22  1999/10/22 18:56:36  lazarus
  Fixed a linking error in wincontrol.inc
  Shane

  Revision 1.21  1999/10/22 18:39:43  lazarus
  Added kEYUP- KeyPress - Keydown, etc.

  Shane

  Revision 1.20  1999/10/20 21:08:16  lazarus
  added OnDblClick, OnShowHint, OnParentShowHint, etc for compatability.

  Revision 1.18  1999/09/30 21:59:03  lazarus
  MWE: Fixed TNoteBook problems
       Modifications: A few
       - Removed some debug messages
       + Added some others
       * changed fixed widged of TPage. Code is still broken.
       + TWinControls are also added to the Controls collection
       + Added TControl.Controls[] property

  Revision 1.17  1999/09/26 13:30:15  lazarus

     Implemented OnEnter & OnExit events for TTrackbar. These properties
     and handler functions have been added to TWincontrol, two new
     callbacks have been added to gtkcallback.
      stoppok

  Revision 1.16  1999/09/17 23:12:58  lazarus
  *** empty log message ***

  Revision 1.15  1999/09/15 03:17:32  lazarus
  Changes to Editor.pp
  If the text was actually displayed, then it would work better.  :-)

  Revision 1.14  1999/09/15 02:14:44  lazarus
  *** empty log message ***

  Revision 1.13  1999/09/11 12:16:16  lazarus
  Fixed a bug in key press evaluation. Initial cut at Invalidate problem.

  Revision 1.12  1999/08/26 23:36:03  peter
    + paintbox
    + generic keydefinitions and gtk conversion
    * gtk state -> shiftstate conversion

  Revision 1.11  1999/08/24 21:26:53  lazarus
  *** empty log message ***

  Revision 1.9  1999/08/16 15:48:50  lazarus
  Changes by file:
       Control: TCOntrol-Function GetRect added
                         ClientRect property added
                TImageList - Added Count
                TWinControl- Function Focused added.
      Graphics: TCanvas - CopyRect added - nothing finished on it though
                          Draw added - nothing finiushed on it though
                clbtnhighlight and clbtnshadow added.  Actual color values not right.
               IMGLIST.PP and IMGLIST.INC files added.

   A few other minor changes for compatability added.

    Shane

  Revision 1.8  1999/08/12 18:36:58  lazarus
  Added a bunch of "stuff" for compatablility.  Not sure if it'll all compile yet, will look at that shortly.

  Revision 1.7  1999/08/11 20:41:35  lazarus

  Minor changes and additions made.  Lazarus may not compile due to these changes

  Revision 1.6  1999/08/07 17:59:25  lazarus

        buttons.pp   the DoLeave and DoEnter were connected to the wrong
                     event.

        The rest were modified to use the new SendMessage function.   MAH

  Revision 1.5  1999/07/31 14:27:04  peter
    * mouse fixes
    * wheel support

  Revision 1.4  1999/07/31 06:39:32  lazarus

       Modified the IntSendMessage3 to include a data variable. It isn't used
       yet but will help in merging the Message2 and Message3 features.

       Adjusted TColor routines to match Delphi color format

       Added a TGdkColorToTColor routine in gtkproc.inc

       Finished the TColorDialog added to comDialog example.        MAH

 }
