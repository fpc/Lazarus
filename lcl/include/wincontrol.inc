// included by controls.pp

{******************************************************************************
                                  TWinControl
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

{$IFOPT C-}
// Uncomment for local trace
//  {$C+}
//  {$DEFINE ASSERT_IS_ON}
{$ENDIF}

{ $DEFINE CHECK_POSITION}
{ $DEFINE VerboseMouseBugfix}

{------------------------------------------------------------------------------}
{  TWinControl AdjustSize                                                      }
{------------------------------------------------------------------------------}
procedure TWinControl.AdjustSize;
begin
  inherited AdjustSize;
  if HandleAllocated then RequestAlign;
end;

{------------------------------------------------------------------------------}
{  TWinControl AdjustClientRect                                                }
{------------------------------------------------------------------------------}
Procedure TWinControl.AdjustClientRect(var Rect: TRect);
Begin
//Not used.  It's a virtual procedure that should be overriden.
end;

{------------------------------------------------------------------------------}
{  TWinControl AlignControls                                                   }
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
    begin
      if (Controls[I].Align <> alNone) or
         (Controls[I].Anchors <> [akLeft, akTop]) then Exit;
    end;	 
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
    {$IFDEF CHECK_POSITION}
    with Control do
      writeln('[TWinControl.AlignControls.DoPosition] A Control=',Name,':',ClassName,' ',Left,',',Top,',',Width,',',Height,' recalculate the anchors=',(AAlign = alNone) or (Control.Anchors <> AnchorAlign[AAlign]));
    {$ENDIF}
    with Rect do
    begin
      { Just recalculate the anchors
        
        Use Anchors to ensure that a control maintains its current position 
        relative to an edge of its parent, even if the parent is resized. When
        its parent is resized, the control holds its position relative to the 
        edges to which it is anchored.
        If a control is anchored to opposite edges of its parent, the control 
        stretches when its parent is resized. For example, if a control has its 
        Anchors property set to [akLeft,akRight], the control stretches when the 
        width of its parent changes.
        Anchors is enforced only when the parent is resized. Thus, for example, 
        if a control is anchored to opposite edges of a form at design time and 
        the form is created in a maximized state, the control is not stretched 
        because the form is not resized after the control is created.
      }
      if (AAlign = alNone) or (Control.Anchors <> AnchorAlign[AAlign]) then 
        with Control do
        begin
          if FLastWidth = 0 then 
	    Width2 := Parent.FLastResize.X + Width
	  else
	    Width2 := Parent.FLastResize.X + FLastWidth;
	  if FLastHeight = 0 then
	    Height2 := Parent.FLastResize.Y + Height
	  else
            Height2 := Parent.FLastResize.Y + FLastHeight;
	    
          //writeln('  FLastWidth=',FLastWidth,' akLeft=',akLeft in Anchors,' akRight=',akRight in Anchors,' akTop=',akTop in Anchors);
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
          //with R do writeln(' R=',Left,',',Top,',',Right,',',Bottom);
          BoundsRect := R;
          FLastWidth := Width2;
          FLastHeight := Height2;
          if AAlign = alNone then Exit;
        end;

      { Realign 

        Use Align to align a control to the top, bottom, left, or right of a 
        form or panel and have it remain there even if the size of the form, 
        panel, or component that contains the control changes. When the parent 
        is resized, an aligned control also resizes so that it continues to span 
        the top, bottom, left, or right edge of the parent.
      }
      Width2 := Right - Left;
      //writeln('  Realign Width2=',Width2,' ',AAlign in [alLeft, alRight],' ',Control.Width);
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
    if (Control.Left <> Left2) or (Control.Top <> Top2)
    or (Control.Width <> Width2) or (Control.Height <> Height2) then begin
      {$IFDEF CHECK_POSITION}
      with Control do
        writeln('[TWinControl.AlignControls.DoPosition] B1 Control=',Name,':',ClassName,' ',Left2,',',Top2,',',Width2,',',Height2);
      {$ENDIF}
      Control.SetBounds(Left2, Top2, Width2, Height2);
      {$IFDEF CHECK_POSITION}
      with Control do
        writeln('[TWinControl.AlignControls.DoPosition] B2 Control=',Name,':',ClassName,' ',Left,',',Top,',',Width,',',Height);
      {$ENDIF}
    end;  

    {Sometimes the control doesn't resize.  This will verifiy that it is the
      size it is assigned to be}
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
    {$IFDEF CHECK_POSITION}
    with Control do
      writeln('[TWinControl.AlignControls.DoPosition] END Control=',Name,':',ClassName,' ',Left,',',Top,',',Width,',',Height);
    {$ENDIF}
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
    if (AControl <> nil)
      and (AControl.Align = AAlign)
      and ((AAlign = alNone) or AControl.Visible
        or (csDesigning in AControl.ComponentState)
        and not (csNoDesignVisible in AControl.ControlStyle))
    then
      AlignList.Add(AControl);

    for I := 0 to ControlCount - 1 do
    begin
      Control := Controls[I];

      if (Control.Align = AAlign) 
        and ((AAlign = alNone)
          or (Control.Visible
            or (Control.ControlStyle * [csAcceptsControls, csNoDesignVisible] =
            [csAcceptsControls, csNoDesignVisible]))
          or (csDesigning in Control.ComponentState)
          and not (csNoDesignVisible in Control.ControlStyle)) then
      begin
        if Control = AControl then Continue;

        X := 0;
        while (X < AlignList.Count) 
        and not InsertBefore(Control, TControl(AlignList[X]), AAlign) do 
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
    FAdjustClientRectRealized:=Rect;
    {$IFDEF VerboseClientRectBugFix}
    writeln('[TWinControl.AlignControls] ',Name,':',Classname,' ',Left,',',Top,',',Width,',',Height,' ClientRect=',Rect.Left,',',Rect.Top,',',Rect.Right,',',Rect.Bottom);
    {$ENDIF}
    AlignList := TList.Create;
    try
      DoAlign(alTop);
      DoAlign(alBottom);
      DoAlign(alLeft);
      DoAlign(alRight);
      DoAlign(alClient);
      DoAlign(alNone);
    finally
      AlignList.Free;
    end;
  end;
  FLastResize.X := 0;
  FLastResize.Y := 0;

  if Showing then AdjustSize;
end;

Procedure TWinControl.DoAutoSize;
var
  I : Integer;
  NewBounds : TRect;
begin
  If AutoSize and not AutoSizing then
    If csAcceptsControls in ControlStyle then
      If ControlCount > 0 then begin
        AutoSizing := True;
        NewBounds := Rect(High(Integer),High(Integer),0,0);
        For I := 0 to ControlCount - 1 do
          If Controls[I].Visible then
            With NewBounds do begin
              Left := Min(Controls[I].Left, Left);
              Top := Min(Controls[I].Top, Top);
              Right := Max(Controls[I].Left + Controls[I].Width, Right);
              Bottom := Max(Controls[I].Top + Controls[I].Height, Bottom);
            end;
        For I := 0 to ControlCount - 1 do
          If Controls[I] <> nil then begin
            If Controls[I].Visible then begin
              Controls[I].Left := Controls[I].Left - NewBounds.Left;
              Controls[I].Top := Controls[I].Top - NewBounds.Top;
            end;
          end;
        ClientWidth := NewBounds.Right - NewBounds.Left;
        ClientHeight := NewBounds.Bottom - NewBounds.Top;
        AutoSizing := False;
      end;
end;

{------------------------------------------------------------------------------}
{  TWinControl BroadCast                                                       }
{------------------------------------------------------------------------------}
procedure TWinControl.BoundsChanged;
begin
  inherited BoundsChanged;
  Realign;
end;

{------------------------------------------------------------------------------}
{  TWinControl BroadCast                                                       }
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
{  TWinControl CanFocus                                                        }
{------------------------------------------------------------------------------}
Function TWinControl.CanFocus : Boolean;
var
  Control: TWinControl;
  Form: TCustomForm;
begin
  Result := False;
  //Verify that every parent is enabled and visible before returning true.
  Form := GetParentForm(Self);
  if Form <> nil then
  begin
    Control := Self;
    while Control <> Form do
    begin
      if not (Control.FVisible and Control.Enabled) then Exit;
      Control := Control.Parent;
    end;
    Result := True;
  end;
end;

{------------------------------------------------------------------------------}
{  TWinControl CMDrag                                                          }
{------------------------------------------------------------------------------}
Procedure TWinControl.CMDrag(var Message: TCMDrag);
Begin
  with Message, DragRec^ do
  Begin
  case DragMessage of
    dmDragEnter, dmDragLeave,dmDragMove, dmDragDrop :
      if target <> nil then TControl(target).DoDragMsg(Message);
    dmFindTarget:
      begin
        Writeln('dmFindTarget');
        Result := longint(ControlatPos(ScreentoClient(pos),False));
        if Result = 0 then Result := longint(Self);
      end;
    end;//case
  end;
end;

{------------------------------------------------------------------------------}
{  TWinControl CreateSubClass                                                      }
{------------------------------------------------------------------------------}
procedure TWinControl.CreateSubClass(var Params: TCreateParams;
  ControlClassName: PChar);
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
{  TWinControl DisableAlign                                                    }
{------------------------------------------------------------------------------}
procedure TWinControl.DisableAlign;
begin
  Inc(FAlignLevel);
End;

{-------------------------------------------------------------------------------
  TWinControl DoAdjustClientRectChange
  
  Asks the interface if clientrect has changed since last AlignControl
  and calls AlignControl(nil) on change.
-------------------------------------------------------------------------------}
procedure TWinControl.DoAdjustClientRectChange;
var r: TRect;
begin
  if (csLoading in ComponentState) then exit;
  r:=GetClientRect;
  AdjustClientRect(r);
//writeln(' TWinControl.DoAdjustClientRectChange ',Name,':',ClassName,' ',r.Right,',',r.Bottom);
  if (r.Left<>FAdjustClientRectRealized.Left)
  or (r.Top<>FAdjustClientRectRealized.Top)
  or (r.Right<>FAdjustClientRectRealized.Right)
  or (r.Bottom<>FAdjustClientRectRealized.Bottom)
  then begin
    // client rect changed since last AlignControl
    {$IFDEF VerboseClientRectBugFix}
    writeln('UUU TWinControl.DoAdjustClientRectChange ClientRect changed ',Name,':',ClassName,
      ' Old=',FAdjustClientRectRealized.Right,'x',FAdjustClientRectRealized.Bottom,
      ' New=',r.RIght,'x',r.Bottom);
    {$ENDIF}
    AlignControl(nil);
    Resize;
  end;
end;

{-------------------------------------------------------------------------------
  TWinControl DoConstraintsChange
  Params: Sender : TObject
  
  Call inherited, then send the constraints to the interface
-------------------------------------------------------------------------------}
procedure TWinControl.DoConstraintsChange(Sender : TObject);
begin
  inherited DoConstraintsChange(Sender);
  SendMsgToInterface(LM_SETGEOMETRY, Self, nil);
end;

{-------------------------------------------------------------------------------
  TWinControl InvalidateClientRectCache

  The clientrect is cached. Call this procedure to invalidate the cache, so that
  next time the clientrect is fetched from the interface.
-------------------------------------------------------------------------------}
procedure TWinControl.InvalidateClientRectCache;
begin
  {$IFDEF VerboseClientRectBugFix}
  writeln('[TWinControl.InvalidateClientRectCache] ',Name,':',ClassName);
  {$ENDIF}
  Include(FFlags,wcfClientRectNeedsUpdate);
end;

{-------------------------------------------------------------------------------
  TWinControl DoSetBounds
  Params: ALeft, ATop, AWidth, AHeight : integer
  
  Anticipate the new clientwidth/height and call inherited
  
  Normally the clientwidth/clientheight is adjusted automatically by the
  interface. But it is up to interface when this will be done. The gtk for
  example just puts resize requests into a queue. The LCL resizes the childs
  just after this procedure due to the clientrect. On complex forms with lots
  of nested controls, this results in thousands of resizes.
  Changing the clientrect in the LCL to the most probable size reduce
  unneccessary resizes.
-------------------------------------------------------------------------------}
procedure TWinControl.DoSetBounds(ALeft, ATop, AWidth, AHeight : integer);
begin
  if wcfClientRectNeedsUpdate in FFlags then begin
    GetClientRect;
  end;
  {$IFDEF VerboseClientRectBugFix}
  write('[TWinControl.DoSetBounds] ',Name,':',ClassName,' OldClient=',FClientWidth,',',FClientHeight,
  ' OldHeight=',FHeight,' NewHeight=',AHeight);
  {$ENDIF}
  inc(FClientWidth,AWidth-FWidth);
  if (FClientWidth<0) then FClientWidth:=0;
  inc(FClientHeight,AHeight-FHeight);
  if (FClientHeight<0) then FClientHeight:=0;
  {$IFDEF VerboseClientRectBugFix}
  writeln(' NewClient=',FClientWidth,',',FClientHeight);
  {$ENDIF}

  inherited DoSetBounds(ALeft,ATop,AWidth,AHeight);
end;

{------------------------------------------------------------------------------}
{  TWinControl EnableAlign                                                     }
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
{       TWinControl.CanTab
}
{------------------------------------------------------------------------------}
Function TWinControl.CanTab: Boolean;
begin
  Result := True;
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

{-------------------------------------------------------------------------------
  TWinControl GetClientOrigin
  Result:  TPoint
  
  returns the screen coordinate of the topleft coordinate 0,0 of the client area
  Note that this value is the position as stored in the interface and is not
  always consistent with the LCL. When a control is moved, the LCL sets the
  bounds to the wanted position and sends a move message to the interface. It is
  up to the interface to handle moves instantly or queued.
-------------------------------------------------------------------------------}
Function TWinControl.GetClientOrigin: TPoint;
Begin
  Result.X := 0;
  Result.Y := 0;
//writeln('[TWinControl.GetClientOrigin] ',Name,':',ClassName,'  ',Handle);
  LCLLinux.ClientToScreen(Handle,Result);
  Assert(False, Format('Trace:[TWinControl.GetClientOrigin] %s --> (%d, %d)',  [Classname, Result.X, Result.Y]));
end;

function TWinControl.GetClientRect: TRect;

  procedure StoreClientRect(NewClientRect: TRect);
  begin
    if wcfClientRectNeedsUpdate in FFlags then begin
      FClientWidth:=NewClientRect.Right;
      FClientHeight:=NewClientRect.Bottom;
      {$IFDEF VerboseClientRectBugFix}
      writeln('StoreClientRect ',Name,':',ClassName,' ',FClientWidth,',',FClientHeight);
      {$ENDIF}
      Exclude(FFlags,wcfClientRectNeedsUpdate);
    end;
  end;

var //r: TRect;
  InterfaceWidth, InterfaceHeight: integer;
begin
  if not HandleAllocated then begin
    Result:=inherited GetClientRect;
    StoreClientRect(Result);
  end else if wcfClientRectNeedsUpdate in FFlags then begin
    // update clientrect from interface
    LCLLinux.GetClientRect(Handle, Result);
    // the LCL is not always in sync with the interface
    // -> adjust client rect based on LCL bounds
    LCLLinux.GetWindowSize(Handle, InterfaceWidth, InterfaceHeight);
    Result.Right:=Width-(InterfaceWidth-Result.Right);
    Result.Bottom:=Height-(InterfaceHeight-Result.Bottom);
    StoreClientRect(Result);

    {r:=inherited GetClientRect;
    if (r.Left<>Result.Left)
    or (r.Top<>Result.Top)
    or (r.Right<>Result.Right)
    or (r.Bottom<>Result.Bottom) then begin
      //writeln(' TWinControl.GetClientRect ',Name,':',ClassName,
      //  ' Old=',r.Left,',',r.Top,',',r.Right,',',r.Bottom,
      //  ' New=',Result.Left,',',Result.Top,',',Result.Right,',',Result.Bottom
      //  );
    end;}
    
  end else begin
    Result:=Rect(0,0,FClientWidth,FClientHeight);
  end;
end;

{------------------------------------------------------------------------------
  function TWinControl.GetChildsRect(Scrolled: boolean): TRect;

  Returns the Client rectangle relative to the controls left, top.
  If Scrolled is true, the rectangle is moved by the current scrolling values
  (for an example see TScrollingWincontrol).
------------------------------------------------------------------------------}
function TWinControl.GetChildsRect(Scrolled: boolean): TRect;
var
  ScrolledOffset: TPoint;
begin
  if HandleAllocated then begin
    LCLLinux.GetClientBounds(Handle,Result);
    if Scrolled then begin
      ScrolledOffset:=GetClientScrollOffset;
      inc(Result.Left,ScrolledOffset.X);
      inc(Result.Top,ScrolledOffset.Y);
      inc(Result.Right,ScrolledOffset.X);
      inc(Result.Bottom,ScrolledOffset.Y);
    end;
  end else
    Result:=inherited GetChildsRect(Scrolled);
end;

{------------------------------------------------------------------------------}
{  TWinControl ReCreateWnd                                                     }
{------------------------------------------------------------------------------}
Procedure TWinControl.ReCreateWnd;
Begin
//send a message to inform the interface that we need to destroy and recreate this control
  if HandleAllocated then
    CNSendMessage(LM_RECREATEWND,Self,Nil);
end;

{------------------------------------------------------------------------------}
{  TWinControl SetBorderWidth                                                  }
{------------------------------------------------------------------------------}
Procedure TWinControl.SetBorderWidth(value : TBorderWidth);
Begin
  //TODO: SETBORDERWIDTH - Not sure if anything more is needed here
  FBorderWidth := Value;
end;

Procedure TWinControl.SetZOrder(Topmost: Boolean);
const
  WindowPos: array[Boolean] of Word = (HWND_BOTTOM, HWND_TOP);
var
  i: integer;
begin
  if FParent <> nil then
  begin
    if TopMost then
      i := FParent.FWinControls.Count - 1
    else
      i := 0;
    if FParent.FControls <> nil then inc(i, FParent.FControls.Count);
    SetZOrderPosition(i);
  end
  else if FHandle <> 0 then begin
    SetWindowPos(FHandle, WindowPos[TopMost], 0, 0, 0, 0,
      SWP_NOMOVE + SWP_NOSIZE);
  end;
end;

procedure TWinControl.SendMoveSizeMessages(SizeChanged, PosChanged: boolean);
var
  SizeMsg : TLMSize;
  MoveMsg : TLMMove;
begin
  if (not HandleAllocated)
  or ((not SizeChanged) and (not PosChanged)) then exit;

  { TODO : Replace this with LCL functions eventually.
      Right now we have to call these messages as some
      descendents incorrectly depend on it, so force it
      down their throat }
      
  Perform(LM_WindowposChanged, 0, 0);

  if SizeChanged then begin
    with SizeMsg do begin
      Msg:= LM_SIZE;
      SizeType:= 6; // force realign
      Width:= FWidth;
      Height:= FHeight;
      {$IFDEF CHECK_POSITION}
      writeln(' [TControl.ChangeBounds] ',Name,':',ClassName,' SizeMsg Width=',Width,' Height=',Height);
      {$ENDIF}
    end;
    WindowProc(TLMessage(SizeMsg));
  end;

  if PosChanged then begin
    with MoveMsg do begin
      Msg:= LM_MOVE;
      MoveType:= 1;
      XPos:= FLeft;
      YPos:= FTop;
      {$IFDEF CHECK_POSITION}
      writeln(' [TControl.ChangeBounds] ',Name,':',ClassName,' MoveMsg XPos=',XPos,' YPos=',YPos);
      {$ENDIF}
    end;
    WindowProc(TLMessage(MoveMsg));
  end;
end;

{------------------------------------------------------------------------------}
{  TWinControl UpdateShowing                                                   }
{------------------------------------------------------------------------------}
procedure TWinControl.UpdateShowing;
var
  bShow: Boolean;
  n: Integer;
  ok: boolean;
begin
  bShow := (FVisible or (csDesigning in ComponentState)
          and not (csNoDesignVisible in ControlStyle))
          and not (csReadingState in ControlState);

  if bShow then begin
    if not HandleAllocated then CreateHandle;
    if FWinControls <> nil 
    then begin
      for n := 0 to FWinControls.Count - 1 do
        TWinControl(FWinControls[n]).UpdateShowing;
    end;
  end;

  if HandleAllocated then begin
    if FShowing <> bShow 
    then begin
      FShowing := bShow;
      ok:=false;
      try
        Perform(CM_SHOWINGCHANGED, 0, 0);
        ok:=true;
      finally
        if not ok then
          FShowing := not bShow;
      end;
    end;
  end;
end;

{-------------------------------------------------------------------------------
  procedure TWinControl.SetZOrderPosition(Position: Integer);
  
-------------------------------------------------------------------------------}
procedure TWinControl.SetZOrderPosition(Position: Integer);
var
  I, Count: Integer;
  Pos: HWND;
begin
  if FParent <> nil then
  begin
    if FParent.FControls <> nil then
      Dec(Position, FParent.FControls.Count);
    I := FParent.FWinControls.IndexOf(Self);
    if I >= 0 then
    begin
      Count := FParent.FWinControls.Count;
      if Position < 0 then Position := 0;
      if Position >= Count then Position := Count - 1;
      if Position <> I then
      begin
        FParent.FWinControls.Delete(I);
        FParent.FWinControls.Insert(Position, Self);
      end;
    end;
    if FHandle <> 0 then
    begin
      if Position = 0 then Pos := HWND_BOTTOM
      else if Position = FParent.FWinControls.Count - 1 then Pos := HWND_TOP
      else if Position > I then
        Pos := TWinControl(FParent.FWinControls[Position + 1]).Handle
      else if Position < I then
        Pos := TWinControl(FParent.FWinControls[Position]).Handle
      else Exit;
      SetWindowPos(FHandle, Pos, 0, 0, 0, 0, SWP_NOMOVE + SWP_NOSIZE);
    end;
  end;
end;

{------------------------------------------------------------------------------}
{  TWinControl Focused                                                         }
{------------------------------------------------------------------------------}
Function TWinControl.Focused : Boolean;
Begin
  Result := CanTab and ((FHandle <> 0) and (GetFocus = FHandle));
end;

{------------------------------------------------------------------------------}
{  TWinControl FindChildControl                                                }
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
{  TWinControl FindNextControl                                                 }
{------------------------------------------------------------------------------}
Function TWinControl.FindNextControl(CurrentControl: TControl;
  GoForward, CheckTabStop, CheckParent, OnlyWinControls: Boolean) : TControl;
var
  List : TList;
  Next : TControl;
  I, J : Longint;
begin
  Try
    Result := nil;
    List := TList.Create;
    GetTabOrderList(List);
    If List.Count > 0 then begin
      J := List.IndexOf(CurrentControl) + 1;
      If J >= List.Count then
        J := -1
      else
        Dec(J);
      I := J;
      Repeat
        If GoForward then
          Inc(I);
        If (I < List.Count) and (List[I] <> nil) then begin
          Next := TControl(List[I]);
          If ((Not CheckTabStop or Next.TabStop) and
            (not CheckParent or (Next.Parent = Self)))
            and (Next.Enabled and Next.Visible)
          then begin
            if not OnlyWinControls or (Next is TWinControl) then
              Result := Next;
          end;
        end;
      until (Result <> nil) or (I = J) or (I >= List.Count);
    end;
  finally
    List.Free;
  end;
end;

{------------------------------------------------------------------------------
  function TWinControl.FindNextControl(CurrentControl: TWinControl; GoForward,
    CheckTabStop, CheckParent: Boolean): TWinControl;
------------------------------------------------------------------------------}
function TWinControl.FindNextControl(CurrentControl: TWinControl; GoForward,
  CheckTabStop, CheckParent: Boolean): TWinControl;
begin
  Result:=TWinControl(FindNextControl(CurrentControl,
                                      GoForward,CheckTabStop,CheckParent,true));
end;

{------------------------------------------------------------------------------
  TWinControl GetTabOrderList
------------------------------------------------------------------------------}
Procedure TWinControl.GetTabOrderList(List : TList);
var
  I : Integer;
  Control : TControl;
begin
  If FTabList <> nil then
    For I := 0 to FTabList.Count - 1 do begin
      Control := TControl(FTabList[I]);
      If Control.CanTab and Control.TabStop then
        List.Add(Control);
      If Control is TWinControl then
        TWinControl(Control).GetTabOrderList(List);
    end;
end;

{------------------------------------------------------------------------------}
{  TWinControl IsControlMouseMsg                                               }
{------------------------------------------------------------------------------}
function TWinControl.IsControlMouseMsg(var TheMessage : TLMMouse) : Boolean;
var
  Control : TControl;
  P : TPoint;
  ClientBounds: TRect;
begin
  if GetCapture = Handle
  then begin
    Control := nil;
    if (CaptureControl <> nil)
    and (CaptureControl.Parent = Self) 
    then Control := CaptureControl;
  end
  else begin
    Control := ControlAtPos(SmallPointToPoint(TheMessage.Pos),False,True,False);
  end;

  Result := False;
  if Control <> nil then
  begin
    // map mouse coordinates to control
    P.X := TheMessage.XPos - Control.Left;
    P.Y := TheMessage.YPos - Control.Top;
    if (Control is TWinControl) and TWinControl(Control).HandleAllocated then
    begin
      // map coordinates to client area of control
      LCLLinux.GetClientBounds(TWinControl(Control).Handle,ClientBounds);
      dec(P.X,ClientBounds.Left);
      dec(P.Y,ClientBounds.Top);
      {$IFDEF VerboseMouseBugfix}
      writeln('TWinControl.IsControlMouseMsg ',Name,' -> ',Control.Name,
      ' MsgPos=',TheMessage.Pos.X,',',TheMessage.Pos.Y,
      ' Control=',Control.Left,',',Control.Top,
      ' ClientBounds=',ClientBounds.Left,',',ClientBounds.Top,
      ' Scrolled=',GetClientScrollOffset.X,',',GetClientScrollOffset.Y,
      ' P=',P.X,',',P.Y
      );
      {$ENDIF}
    end;
    Control.Perform(TheMessage.Msg, TheMessage.Keys,
                    LongInt(PointtoSmallPoint(P)));
    Result := True;
  end;
end;

procedure TWinControl.SetColor(Value: TColor);
begin
  if Value=Color then exit;
  inherited SetColor(Value);
  if HandleAllocated and (not (csLoading in ComponentState)) then
    CNSendMessage(LM_SETCOLOR, Self, nil)
  else
    Include(FFlags,wcfColorChanged);
end;

procedure TWinControl.PaintHandler(var TheMessage: TLMPaint);

  function ControlMustBeClipped(AControl: TControl): boolean;
  begin
    with AControl do
      Result:=(Visible or (csDesigning in ComponentState)
              and not (csNoDesignVisible in ControlStyle))
              and (csOpaque in ControlStyle);
  end;

var
  I, Clip, SaveIndex: Integer;
  DC: HDC;
  PS: TPaintStruct; //defined in LCLLinux.pp
  ControlsNeedsClipping: boolean;
begin
//writeln('[TWinControl.PaintHandler] ',Name,':',ClassName,'  DC=',HexStr(Message.DC,8));
  Assert(False, Format('Trace:> [TWinControl.PaintHandler] %s --> Msg.DC: 0x%x', [ClassName, TheMessage.DC]));
  DC := TheMessage.DC;
  if DC = 0 then DC := BeginPaint(Handle, PS);
  try
    // check if controls needs clipping
    ControlsNeedsClipping:=false;
    if FControls<>nil then
      for I := 0 to FControls.Count - 1 do
        if ControlMustBeClipped(TControl(FControls[I])) then begin
          ControlsNeedsClipping:=true;
          break;
        end;
    // exclude controls and send new paint message
    if not ControlsNeedsClipping then
      PaintWindow(DC)
    else
    begin
      SaveIndex := SaveDC(DC);
      Clip := SimpleRegion;
      for I := 0 to FControls.Count - 1 do
        if ControlMustBeClipped(TControl(FControls[I])) then
          with TControl(FControls[I]) do begin
            Clip := ExcludeClipRect(DC, Left, Top, Left + Width, Top + Height);
            if Clip = NullRegion then Break;
          end;
      if Clip <> NullRegion then PaintWindow(DC);
      RestoreDC(DC, SaveIndex);
    end;
    // paint controls
    PaintControls(DC, nil);
  finally
    if TheMessage.DC = 0 then EndPaint(Handle, PS);
  end;
  Assert(False, Format('Trace:< [TWinControl.PaintHandler] %s', [ClassName]));
//writeln('[TWinControl.PaintHandler] END  ',Name,':',ClassName,'  DC=',HexStr(Message.DC,8));
end;

procedure TWinControl.PaintControls(DC: HDC; First: TControl);
var
  I, Count, SaveIndex: Integer;
//  FrameBrush: HBRUSH;
  TempControl : TCOntrol;
begin
//writeln('[TWinControl.PaintControls] ',Name,':',ClassName,'  DC=',HexStr(DC,8));
  // Controls that are not TWinControl, have no handle of their own, and so
  // they are repainted as part of the parent:
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
      TempControl := TControl(FControls.Items[I]);
      with (TempControl) do
        if (Visible or (csDesigning in ComponentState)
        and not (csNoDesignVisible in ControlStyle))
        and RectVisible(DC, Rect(Left, Top, Left + Width, Top + Height)) then
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
  // draw specials for the wincontrols:
  if FWinControls <> nil then
    for I := 0 to FWinControls.Count - 1 do
      with TWinControl(FWinControls.Items[I]) do
        if FCtl3D and (csFramed in ControlStyle)
        and (Visible or (csDesigning in ComponentState)
        and not (csNoDesignVisible in ControlStyle)) then
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
//writeln('[TWinControl.PaintControls] END ',Name,':',ClassName,'  DC=',HexStr(DC,8));
end;

procedure TWinControl.PaintWindow(DC: HDC);
var
  Message: TLMessage;
begin
//writeln('[TWinControl.PaintWindow] ',Name,':',Classname,'  DC=',HexStr(DC,8));
  Message.Msg := LM_PAINT;
  Message.WParam := DC;
  Message.LParam := 0;
  Message.Result := 0;
  DefaultHandler(Message);
end;


{------------------------------------------------------------------------------
  TWinControl ControlAtPos
  Params: const Pos : TPoint
          AllowDisabled: Boolean
  Results: TControl

  Searches a child (not grand child) control, which client area contains Pos.
  Pos is relative to the ClientOrigin.
------------------------------------------------------------------------------}
Function TWinControl.ControlAtPos(const Pos : TPoint;
  AllowDisabled : Boolean): TControl;
Begin
  Result := ControlAtPos(Pos,AllowDisabled,false,true);
end;

{------------------------------------------------------------------------------
  TWinControl ControlAtPos
  Params: const Pos : TPoint
          AllowDisabled, AllowWinControls: Boolean
  Results: TControl
  
  Searches a child (not grand child) control, which client area contains Pos.
  Pos is relative to the ClientOrigin.
------------------------------------------------------------------------------}
Function TWinControl.ControlAtPos(const Pos : TPoint;
  AllowDisabled, AllowWinControls: Boolean): TControl;
begin
  Result := ControlAtPos(Pos,AllowDisabled,AllowWinControls,true);
end;

{------------------------------------------------------------------------------
  TWinControl ControlAtPos
  Params: const Pos : TPoint
          AllowDisabled, AllowWinControls: Boolean
  Results: TControl

  Searches a child (not grand child) control, which contains Pos.
  Pos is relative to the ClientOrigin.
  If AllowDisabled is true it will also search in disabled controls.
  If AllowWinControls is true it will also search in the child wincontrols.
  If OnlyClientAreas is true then the only the client areas are compared.
------------------------------------------------------------------------------}
function TWinControl.ControlAtPos(const Pos: TPoint; AllowDisabled,
  AllowWinControls, OnlyClientAreas: Boolean): TControl;
var
  I: Integer;
  P: TPoint;
  LControl: TControl;
  ClientBounds: TRect;

  function GetControlAtPos(AControl: TControl): Boolean;
  var
    ControlPos: TPoint;
    ControlClientBounds: TRect;
  begin
    with AControl do
    begin
      // MG: Delphi checks for PtInRect(ClientRect,P). But the client area is
      //     not always at 0,0, so I guess this is a bug in the VCL.

      ControlPos:=Point(P.X-Left,P.Y-Top);
      Result:=(ControlPos.X>=0) and (ControlPos.Y>=0)
              and (ControlPos.X<Width) and (ControlPos.Y<Height);
      if Result and OnlyClientAreas then begin
        ControlClientBounds:=GetChildsRect(false);
        Result:=PtInRect(ControlClientBounds,ControlPos);
      end;
      
      Result:= Result
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
                    (Perform(CM_HITTEST, 0,
                             LongInt(PointtoSmallPoint(ControlPos))) <> 0)
                   )
                  );
      {$IFDEF VerboseMouseBugfix}
      {writeln('GetControlAtPos ',Name,
      ' Pos=',Pos.X,',',Pos.Y,
      ' P=',P.X,',',P.Y,
      ' ClientBounds=',ClientBounds.Left,',',ClientBounds.Top,',',ClientBounds.Right,',',ClientBounds.Bottom,
      ' OnlyCl=',OnlyClientAreas,
      ' Result=',Result);}
      {$ENDIF}
      if Result then
        LControl := AControl;
    end;
  end;

var
  ScrolledOffset: TPoint;
begin
  // check if Pos in visible client area
  ClientBounds:=GetClientRect;
  if not PtInRect(ClientBounds,Pos) then begin
    Result:=nil;
    exit;
  end;
  
  // map Pos to logical client area
  ScrolledOffset:=GetClientScrollOffset;
  P:=Point(Pos.X+ScrolledOffset.X,Pos.Y+ScrolledOffset.Y);

  LControl := nil;
  // check wincontrols
  if AllowWinControls and (FWinControls <> nil) then
    for I := FWinControls.Count - 1 downto 0 do
      if GetControlAtPos(TControl(FWinControls[I])) then
        Break;
  // check controls
  if (FControls <> nil) and (LControl = nil) then
    for I := FControls.Count - 1 downto 0 do
      if GetControlAtPos(TControl(FControls[I])) then
        Break;
  Result := LControl;
end;

{-------------------------------------------------------------------------------
  function TWinControl.GetControlIndex(AControl: TControl): integer;
  
  
-------------------------------------------------------------------------------}
function TWinControl.GetControlIndex(AControl: TControl): integer;
begin
  if FControls<>nil then
    Result:=FControls.IndexOf(AControl)
  else
    Result:=-1;
  if (Result<0) and (FWinControls<>nil) then begin
    Result:=FWinControls.IndexOf(AControl);
    if (Result>=0) and (FControls<>nil) then
      inc(Result,FControls.Count);
  end;
end;

{-------------------------------------------------------------------------------
  function TWinControl.GetControlIndex(AControl: TControl): integer;


-------------------------------------------------------------------------------}
procedure TWinControl.SetControlIndex(AControl: TControl; NewIndex: integer);
begin
  if AControl<>nil then
    AControl.SetZOrderPosition(NewIndex);
end;

{------------------------------------------------------------------------------}
{  TWinControl DestroyHandle                                                   }
{------------------------------------------------------------------------------}
procedure TWinControl.DestroyHandle;
var i : integer;
begin
  if not HandleAllocated then begin
    writeln('Warning: TWinControl.DestroyHandle ',Name,':',ClassName,' Handle not Allocated');
    // create an gdb catchable exception:
    // if (length(Name) div (length(Name) div 10000))=0 then ;
  end;

  { Destroy all children handles, too }
  { If we don't do that, GTK does this without notification for us and we crash }
  { TODO : We can enable HandleAllocated condition only when all controls, especially
    TNotebook / TPage set their Handles correctly, i.e. mirror the GTK behavior }
//  if HandleAllocated then begin
    if FWinControls <> nil then begin
      for i:= 0 to FWinControls.Count - 1 do begin
        //writeln('  i=',i);
        //writeln('  ',TWinControl(FWinControls[i]).Name,':',TWinControl(FWinControls[i]).ClassName);
        if TWinControl(FWinControls[i]).HandleAllocated then
          TWinControl(FWinControls[i]).DestroyHandle;
      end;	
    end;
    DestroyWnd;
//  end;
end;

{------------------------------------------------------------------------------
  TWinControl WndPRoc
------------------------------------------------------------------------------}
Procedure TWinControl.WndProc(Var Message: TLMessage);
Var
  Form: TCustomForm;
//  KeyState: TKeyboardState;
//  WheelMsg : TCMMouseWheel;
Begin
//  Assert(False, Format('Trace:[TWinControl.WndPRoc] %s(%s) --> Message = %d', [ClassName, Name, Message.Msg]));
  case Message.Msg of
  
  LM_SETFOCUS:
    begin
      Assert(False, Format('Trace:[TWinControl.WndPRoc] %s --> LM_SETFOCUS', [ClassName]));
      Form := GetParentForm(Self);
      {$IFDEF VerboseFocus}
      writeln('TWinControl.WndProc A ',Name,':',ClassName);
      {$ENDIF}
      if (Form <> nil) and not Form.SetFocusedControl(Self) then Exit;
      Message.Result:=0;
    end;
    
  LM_KILLFOCUS:
    begin
      Assert(False, Format('Trace:[TWinControl.WndPRoc] %s --> _KILLFOCUS', [ClassName]));
      if csFocusing in ControlState then Exit;
      Message.Result:=0;
    end;
    
  LM_NCHITTEST:
    begin
      inherited WndPRoc(Message);
      if (Message.Result = HTTRANSPARENT)
      and (ControlAtPos(ScreenToClient(SmallPointToPoint(TLMNCHitTest(Message).Pos)), False) <> nil)
      then  Message.Result := HTCLIENT;
      Exit;
    end;
    
  LM_MOUSEFIRST..LM_MOUSELAST,LM_MOUSEFIRST2..LM_MOUSELAST2:
     begin
       {$IFDEF VerboseMouseBugfix}
       writeln('TWinControl.WndPRoc A ',Name,':',ClassName);
       {$ENDIF}
       if IsControlMouseMSG(TLMMouse(Message)) then
         Exit;
       {$IFDEF VerboseMouseBugfix}
       writeln('TWinControl.WndPRoc B ',Name,':',ClassName);
       {$ENDIF}
     end;
     
  LM_KEYFIRST..LM_KEYLAST:
    if Dragging then Exit;
    
  LM_CANCELMODE:
    if (GetCapture = Handle)
    and (CaptureControl <> nil)
    and (CaptureControl.Parent = Self)
    then CaptureControl.Perform(LM_CANCELMODE,0,0);
    
  else
//TODO:Implement TMOUSE
{  with Mouse do
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
}
  end;

  inherited WndProc(Message);
end;

{------------------------------------------------------------------------------
  Method:  TWinControl.MainWndProc
  Params:  Message:
  Returns: Nothing

  Description of the procedure for the class.
 ------------------------------------------------------------------------------}
Procedure TWinControl.MainWndProc(Var Message : TLMessage);
Begin
  Assert(False, Format('Trace:[TWinControl.MainWndPRoc] %s(%s) --> Message = %d', [ClassName, Name, Message.Msg]));
end;

{------------------------------------------------------------------------------}
{  TWinControl SetFocus                                                      }
{------------------------------------------------------------------------------}
procedure TWinControl.SetFocus;
var
  Form : TCustomForm;
begin
  {$IFDEF VerboseFocus}
  writeln('[TWinControl.SetFocus] ',Name,':',ClassName,' Visible=',Visible,' HandleAllocated=',HandleAllocated);
  {$ENDIF}
  Form := GetParentForm(Self);
  if Form <> nil then
    Form.FocusControl(Self)
  else if Visible and HandleAllocated then
    LCLLinux.SetFocus(Handle);
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

{------------------------------------------------------------------------------
  TWinControl KeyDown
------------------------------------------------------------------------------}
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
//  Writeln('name is ',self.name);
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
      if CharCode = VK_UNKNOWN then Exit;
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
      raise EInvalidOperation.Create(rsAControlCanNotHaveItselfAsParent);
    end;

    if AControl is TWinControl then
    begin
      ListAdd(FWinControls, AControl);
      ListAdd(FTabList, AControl);
    end else
      ListAdd(FControls, AControl);

    AControl.FParent := Self;

    If (csDesigning in ComponentState) and not
       (csLoading in ComponentState)
    then
      If AControl.CanTab then
        AControl.TabStop := True;
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
    end else
      Listremove(FControls, AControl);
    AControl.FParent := Nil;
  end;
End;


{------------------------------------------------------------------------------}
{  TWinControl RemoveFocus                                                      }
{------------------------------------------------------------------------------}
Procedure TWinControl.RemoveFocus(Removing : Boolean);
  //TODO: FINISH TWINCONTROL.REMOVEFOCUS
var
  Form: TCustomForm;
begin
  Form := GetParentForm(Self);
  if Form <> nil then ;//Form.DefocusControl(Self, Removing);
end;

{------------------------------------------------------------------------------}
{  TWinControl UpdateControlState                                                      }
{------------------------------------------------------------------------------}
procedure TWinControl.UpdateControlState;
var AWinControl : TWinControl;
begin
  AWinControl:= Self;
  { If any of the parent is not visible, exit }
  while AWinControl.Parent <> nil do
  begin
    AWinControl:= AWinControl.Parent;
    if (not AWinControl.Showing) or (not AWinControl.HandleAllocated) then Exit;
  end;

  if (AWinControl is TCustomForm)
  or (AWinControl.FParentWindow <> 0)
  then UpdateShowing;
end;

{------------------------------------------------------------------------------}
{  TWinControl InsertControl                                                   }
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
{  TWinControl removeControl                                                   }
{------------------------------------------------------------------------------}
Procedure TWinControl.RemoveControl(AControl : TControl);
Begin
  Perform(CM_CONTROLCHANGE, Integer(AControl), Integer(False));
  if AControl is TWinControl then
    with TWinControl(AControl) do
    begin
      RemoveFocus(True);
      if HandleAllocated then DestroyHandle;
    end
  else
    if HandleAllocated then
      AControl.InvalidateControl(AControl.Visible, False, True);
  Remove(AControl);
//  Perform(CM_CONTROLLISTCHANGE, Integer(AControl), Integer(False));
  Realign;
End;

{------------------------------------------------------------------------------}
{  TWinControl AlignControl                                                    }
{------------------------------------------------------------------------------}
procedure TWinControl.AlignControl(AControl : TControl);
var
//  Num : Integer;
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
                  //put nothing in here

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
var
  N: Integer;
begin
  if FControls <> nil then
    N := FControls.Count
  else
    N := 0;
  if Index < N then
    Result := TControl(FControls[Index])
  else
    Result := TControl(FWinControls[Index - N]);
end;

{------------------------------------------------------------------------------}
{  TWinControl GetControlCount                                                 }
{------------------------------------------------------------------------------}
function TWinControl.GetControlCount: Integer;
begin
  Result := 0;
  if FControls <> nil then Inc(Result, FControls.Count);
  if FWinControls <> nil then Inc(Result, FWinControls.Count);
end;

{------------------------------------------------------------------------------}
{  TWinControl GetHandle                                                       }
{------------------------------------------------------------------------------}
function TWinControl.GetHandle: HWND;
begin
  if not HandleAllocated then Assert(False, Format('Trace:[TWinControl.GetHandle] %s(%s)', [ClassNAme, Name]));
  HandleNeeded;
  Result := FHandle;
end;

{------------------------------------------------------------------------------
  TWinControl SetHandle
  Params:  NewHandle
  Returns: Nothing
-------------------------------------------------------------------------------}
procedure TWincontrol.SetHandle(NewHandle: HWND);
begin
  FHandle:=NewHandle;
end;

{------------------------------------------------------------------------------
  Method: TWinControl.Create
  Params:  None
  Returns: Nothing

  Contructor for the class.
 ------------------------------------------------------------------------------}
constructor TWinControl.Create(TheOwner : TComponent);
begin
  inherited Create(TheOwner);
  FCompStyle := csFixed;
  FBrush := nil;
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
  // ToDo
  Result:=nil;
end;

{------------------------------------------------------------------------------}
{  TWinControl Hide                                                            }
{------------------------------------------------------------------------------}
procedure TWinControl.Hide;
begin
   Visible := False;
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
  //writeln('[TWinControl.Destroy] A  ',Name,':',ClassName);
  if HandleAllocated then
    DestroyHandle;
  //writeln('[TWinControl.Destroy] B  ',Name,':',ClassName);

  //for n:=0 to ComponentCount-1 do
  //  writeln('  n=',n,' ',Components[n].ClassName);
  
  n := ControlCount;

  while n > 0 do
  begin
    Control := Controls[n - 1];
    //writeln('[TWinControl.Destroy] C  ',Name,':',ClassName,' ',Control.Name,':',Control.ClassName);
    Remove(Control);
    // don't free the control just set parent to nil
    // controls are freed by the owner
    //Control.Free;
    Control.Parent := nil;
    n := ControlCount;
  end;

  FBrush.Free;
  FBrush:=nil;
  //writeln('[TWinControl.Destroy] D  ',Name,':',ClassName);
  inherited Destroy;
  //writeln('[TWinControl.Destroy] END  ',Name,':',ClassName);
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
  DoEnter;
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
begin
//writeln('[TWinControl.WMPaint] ',Name,':',ClassName,'  ',HexStr(Msg.DC,8));
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
//writeln('[TWinControl.WMPaint] END ',Name,':',ClassName);
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
  //writeln('TWinControl.WMDestroy ',Name,':',ClassName);
  // Our widget/window doesn't exist anymore
  FHandle := 0;
end;                          

{------------------------------------------------------------------------------
  Method: TWinControl.WMMove
  Params:   Msg: The message
  Returns:  nothing

  event handler.
 ------------------------------------------------------------------------------}
procedure TWinControl.WMMove(var Message: TLMMove);
begin
  if Message.MoveType=Move_SourceIsInterface then begin
    // interface widget has moved
    FBoundsRealized:=Bounds(Message.XPos,Message.YPos,
                            FBoundsRealized.Right-FBoundsRealized.Left,
                            FBoundsRealized.Bottom-FBoundsRealized.Top);
  end;
  inherited WMMove(Message);
end;

{------------------------------------------------------------------------------
  Method: TWinControl.WMSize
  Params:   Message: TLMSize
  Returns:  nothing

  Event handler for size messages. This is called, whenever width, height,
  clientwidth or clientheight have changed.
  If the source of the message is the interface, the new size is stored
  in FBoundsRealized to avoid sending a size message back to the interface.
 ------------------------------------------------------------------------------}
procedure TWinControl.WMSize(var Message: TLMSize);
begin
  if Message.SizeType=Size_SourceIsInterface then begin
    // interface widget has resized
    FBoundsRealized.Right:=FBoundsRealized.Left+Message.Width;
    FBoundsRealized.Bottom:=FBoundsRealized.Top+Message.Height;
  end;
  inherited WMSize(Message);
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
  DoExit;
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
begin
  Assert(False, Format('Trace:[TWinControl.WMKeyDown] %s', [ClassName]));
  if not DoKeyDown(Message) then begin
    {inherited} ;  // there is nothing to inherit
  end;
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
  Function: TWinControl.HandleAllocated
  Params:   None
  Returns:  True is handle is allocated

  Checks if a handle is allocated. I.E. if the control is mapped
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
  
{  procedure WriteClientRect(const Prefix: string);
  var r: TRect;
  begin
    if (ClassName<>'TGROUPBOX') or (not HandleAllocated) then exit;
    LCLLinux.GetClientRect(Handle,r);
    writeln('WriteClientRect ',Prefix,' ',Name,':',ClassName,' r=',r.Right,',',r.Bottom);
  end;}
  
begin
  //writeln('[TWinControl.CreateWnd] START ',Name,':',Classname);
  if (FCompstyle = csNone) then 
  begin
    WriteLn(Format('WARNING: [TWinControl.CreateWnd] %s --> FCompstyle = csNone', [ClassName]));
    Exit;
  end;

  CreateParams(Params);
  with Params do begin
    if (WndParent = 0) and (Style and WS_CHILD <> 0) then
      RaiseGDBException('TWinControl.CreateWnd: no parent '+Name+':'+ClassName);
    Assert((parent <> nil) or (WndParent = 0), 'TODO: find parent if parent=nil and WndParent <> 0');
  end;
  
  CreateComponent(nil);
  //WriteClientRect('A');
  if Parent <> nil then AddControl;
  
  //WriteClientRect('B');
  InitializeWnd;

  //writeln('[TWinControl.CreateWnd] ',Name,':',ClassName,' ',Left,',',Top,',',Width,',',Height);
  //WriteClientRect('C');
  RequestAlign;
  
  if FWinControls <> nil then begin
    for n := 0 to FWinControls.Count - 1 do
      with TWinControl(FWinControls.Items[n]) do
        if Visible then HandleNeeded;
  end;
  //writeln('[TWinControl.CreateWnd] END ',Name,':',Classname);
  //WriteClientRect('D');
end;

{------------------------------------------------------------------------------
  procedure TWinControl.CreateComponent(TheOwner : TComponent);
------------------------------------------------------------------------------}
procedure TWinControl.CreateComponent(TheOwner : TComponent);
begin
  if HandleAllocated then
    RaiseGDBException('TWinControl.CreateComponent: Handle already created');
  CNSendMessage(LM_CREATE, Self, nil);
  if not HandleAllocated then
    RaiseGDBException('TWinControl.CreateComponent: Handle creation failed');
end;

{------------------------------------------------------------------------------
  TWinControl Destroy Component
------------------------------------------------------------------------------}
procedure TWinControl.DestroyComponent;
begin
  CNSendMessage(LM_DESTROY, Self, nil);
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
begin
  Assert(False, Format('Trace:[TWinControl.InitializeWnd]  %s', [ClassName]));
  // set all cached properties
  
  // MWE:All of this should be handled to the interface create routine
  Assert(False, 'Trace:TODO: [TWinControl.InitializeWnd] move this code to the interface');
  
  R:= Rect(Left, Top, Width, Height);
//writeln('[TWinControl.InitializeWnd] ',Name,':',ClassName,' ',Left,',',Top,',',Width,',',Height);
  FBoundsRealized:=Bounds(Left, Top, Width, Height);
  CNSendMessage(LM_SETSIZE, Self, @R);
  CNSendMessage(LM_SHOWHIDE, Self, nil);
  CNSendMessage(LM_SETCOLOR, Self, nil);
  Exclude(FFlags,wcfColorChanged);
  
  EnableWindow(Handle, Enabled);
 
  SetTextBuf(PChar(FCaption));
  Assert(False, 'Trace:SETPROP**********************************************');

  SetProp(Handle,'WinControl',TWinControl(Self));
  SetProp(Handle,'Control',TControl(Self));
end;

{------------------------------------------------------------------------------
  procedure TWinControl.Loaded;
 ------------------------------------------------------------------------------}
procedure TWinControl.Loaded;
begin
  inherited Loaded;
  if wcfColorChanged in FFlags then begin
    CNSendMessage(LM_SETCOLOR, Self, nil);
    Exclude(FFlags,wcfColorChanged);
  end;
end;

{------------------------------------------------------------------------------
  Method:  TWinControl.DestroyWnd
  Params:  None
  Returns: Nothing

  Creates the interface object.
 ------------------------------------------------------------------------------}
procedure TWinControl.DestroyWnd; 
begin
  if HandleAllocated then begin
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
      Assert(False, Format('Trace:[TWinControl.HandleNeeded] Somebody set Parent := Self in %s. DONT DO THAT !!', [Classname]));
    end
    else begin
      if (Parent <> nil) then
        Parent.HandleNeeded;
    end;
    CreateHandle;
  end;
end;

{------------------------------------------------------------------------------
  Method: TControl.BeginUpdateBounds
  Params:  None
  Returns: Nothing

  increases the BoundsLockCount
 ------------------------------------------------------------------------------}
procedure TWinControl.BeginUpdateBounds;
begin
  inc(FBoundsLockCount);
end;

{------------------------------------------------------------------------------
  Method: TControl.EndUpdateBounds
  Params:  None
  Returns: Nothing

  decreases the BoundsLockCount
 ------------------------------------------------------------------------------}
procedure TWinControl.EndUpdateBounds;
begin
  if BoundsLockCount<=0 then exit;
  dec(FBoundsLockCount);
  if BoundsLockCount=0 then begin
    SetBounds(Left,Top,Width,Height);
  end;
end;

{------------------------------------------------------------------------------
  Method: TControl.GetIsResizing
  Params:  None
  Returns: Nothing

  decreases the BoundsLockCount
 ------------------------------------------------------------------------------}
function TWinControl.GetIsResizing: boolean;
begin
  Result:=BoundsLockCount>0;
end;

{------------------------------------------------------------------------------
  Method:  TWinControl.SetBounds
  Params:  aLeft, aTop, aWidth, aHeight
  Returns: Nothing

  Sets the bounds of the control.
 ------------------------------------------------------------------------------}
procedure TWinControl.SetBounds(aLeft, aTop, aWidth, aHeight : integer);
var
  NewBounds, OldBounds, R: TRect;
  
  function CompareRect(R1, R2: PRect): Boolean;
  begin
    Result:=(R1^.Left=R2^.Left) and (R1^.Top=R2^.Top) and
            (R1^.Bottom=R2^.Bottom) and (R1^.Right=R2^.Right);
    {if not Result then begin
      writeln(' DIFFER: ',R1^.Left,',',R1^.Top,',',R1^.Right,',',R1^.Bottom
        ,' <> ',R2^.Left,',',R2^.Top,',',R2^.Right,',',R2^.Bottom);
    end;}
  end;

begin
  {$IFDEF CHECK_POSITION}
  writeln('[TWinControl.SetBounds] START ',Name,':',ClassName,
  ' Old=',Left,',',Top,',',Width,',',Height,
  ' -> New=',ALeft,',',ATop,',',AWidth,',',AHeight,
  ' Lock=',BoundsLockCount,
  ' Realized=',FBoundsRealized.Left,',',FBoundsRealized.Top,
  ',',FBoundsRealized.Right-FBoundsRealized.Left,',',FBoundsRealized.Bottom-FBoundsRealized.Top
  );
  {$ENDIF}
  if BoundsLockCount<>0 then exit;
  OldBounds:=BoundsRect;
  NewBounds:=Bounds(ALeft, ATop, AWidth, AHeight);
  if CompareRect(@NewBounds,@OldBounds) then begin
    // LCL bounds are already up2date -> check realized bounds
    if HandleAllocated
    and (not CompareRect(@NewBounds,@FBoundsRealized)) then begin
      // the bounds were not yet send to the InterfaceObject -> send them
      BeginUpdateBounds;
      try
        {$IFDEF CHECK_POSITION}
        writeln('[TWinControl.SetBounds] RealizeBounds A ',Name,':',ClassName,
        ' OldRelBounds=',FBoundsRealized.Left,',',FBoundsRealized.Top,',',FBoundsRealized.Right,',',FBoundsRealized.Bottom,
        ' -> NewBounds=',NewBounds.Left,',',NewBounds.Top,',',NewBounds.Right,',',NewBounds.Bottom);
        {$ENDIF}
        FBoundsRealized:=NewBounds;
        R:=Rect(Left,Top,Width,Height);
        CNSendMessage(LM_SetSize, Self, @R);
      finally
        EndUpdateBounds;
      end;
    end;
  end else begin
    // LCL bounds are not up2date -> process new bounds
    BeginUpdateBounds;
    try
      {$IFDEF CHECK_POSITION}
      writeln('[TWinControl.SetBounds] Set LCL Bounds ',Name,':',ClassName,
      ' OldBounds=',Left,',',Top,',',Left+Width,',',Top+Height,
      ' -> New=',ALeft,',',ATop,',',ALeft+AWidth,',',ATop+AHeight);
      {$ENDIF}
      inherited SetBounds(ALeft, ATop, AWidth, AHeight);
      NewBounds:=Bounds(Left, Top, Width, Height);

      if HandleAllocated
      and (not CompareRect(@NewBounds,@FBoundsRealized)) then
      begin
        // the bounds were not yet send to the InterfaceObject -> send them
        {$IFDEF CHECK_POSITION}
        writeln('[TWinControl.SetBounds] RealizeBounds B ',Name,':',ClassName,
        ' OldRelBounds=',FBoundsRealized.Left,',',FBoundsRealized.Top,',',FBoundsRealized.Right,',',FBoundsRealized.Bottom,
        ' -> NewBounds=',NewBounds.Left,',',NewBounds.Top,',',NewBounds.Right,',',NewBounds.Bottom);
        {$ENDIF}
        FBoundsRealized:=NewBounds;
        R:=Rect(Left,Top,Width,Height);
        CNSendMessage(LM_SetSize, Self, @R);
      end;
    finally
      EndUpdateBounds;
    end;
  end;
end;

{------------------------------------------------------------------------------
  Method:  TWinControl.SetText
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
//writeln('[TWinControl.GetDeviceContext] ',ClassName,' DC=',HexStr(Cardinal(Result),8),' Handle=',HexStr(Cardinal(FHandle),8));
  if Result = 0 
  then raise EOutOfResources.CreateFmt(rsErrorCreatingDeviceContext, [Name,
    ClassName]);

  WindowHandle := FHandle;
//*)  
end;

{------------------------------------------------------------------------------
  Method:  TWinControl.CMVisibleChanged
  Params:  Message : not used
  Returns: nothing

  Performs actions when visibility has changed
 ------------------------------------------------------------------------------}
procedure TWinControl.CMVisibleChanged(var TheMessage : TLMessage);
begin
  if not FVisible and (Parent <> nil)
  then RemoveFocus(False);
  
  if not (csDesigning in ComponentState) 
  or (csNoDesignVisible in ControlStyle) 
  then
    UpdateControlState;
end;

{------------------------------------------------------------------------------
  Method:  TWinControl.CMShowingChanged
  Params:  Message : not used
  Returns: nothing

  Shows or hides a control
 ------------------------------------------------------------------------------}
procedure TWinControl.CMShowingChanged(var Message: TLMessage);
begin
  if HandleAllocated then
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
  Revision 1.125  2003/04/04 09:19:22  mattias
  activated TDataSource

  Revision 1.124  2003/03/28 19:39:54  mattias
  started typeinfo for double extended

  Revision 1.123  2003/03/25 10:45:41  mattias
  reduced focus handling and improved focus setting

  Revision 1.122  2003/03/18 13:04:25  mattias
  improved focus debugging output

  Revision 1.121  2003/03/17 08:51:09  mattias
  added IsWindowVisible

  Revision 1.120  2003/03/11 07:46:44  mattias
  more localization for gtk- and win32-interface and lcl

  Revision 1.119  2003/03/09 17:44:12  mattias
  finshed Make Resourcestring dialog and implemented TToggleBox

  Revision 1.118  2003/02/26 12:44:52  mattias
  readonly flag is now only saved if user set

  Revision 1.117  2003/02/09 18:07:42  mattias
  cleanup

  Revision 1.116  2003/01/24 13:53:53  mattias
  fixed TRadioGroup.Items editing in IDE

  Revision 1.115  2003/01/18 21:31:43  mattias
  fixed scrolling offset of TScrollingWinControl

  Revision 1.114  2003/01/01 13:04:26  mattias
  clean ups

  Revision 1.113  2003/01/01 13:01:01  mattias
  fixed setcolor for streamed components

  Revision 1.112  2002/12/29 18:13:38  mattias
  identifier completion: basically working, still hidden

  Revision 1.111  2002/12/28 17:43:43  mattias
  fixed FindControl and searching overloaded procs

  Revision 1.110  2002/12/28 12:42:38  mattias
  focus fixes, reduced lpi size

  Revision 1.109  2002/12/27 17:46:04  mattias
  fixed SetColor

  Revision 1.108  2002/12/27 17:12:38  mattias
  added more Delphi win32 compatibility functions

  Revision 1.107  2002/02/09 01:48:23  mattias
  renamed TinterfaceObject.Init to AppInit and TWinControls can now contain childs in gtk

  Revision 1.106  2002/12/04 20:39:15  mattias
  patch from Vincent: clean ups and fixed crash on destroying window

  Revision 1.105  2002/12/03 09:28:31  mattias
  cleaned up

  Revision 1.104  2002/11/30 08:35:42  mattias
  TCustomForm.WMDestroy does not Free anymore

  Revision 1.103  2002/11/29 15:14:48  mattias
  replaced many invalidates by invalidaterect

  Revision 1.102  2002/11/27 15:40:36  mattias
  fixed resize request

  Revision 1.101  2002/11/21 18:49:53  mattias
  started OnMouseEnter and OnMouseLeave

  Revision 1.100  2002/11/10 21:49:28  lazarus
  MG: added smart hints in edit mode

  Revision 1.99  2002/11/09 18:13:33  lazarus
  MG: fixed gdkwindow checks

  Revision 1.98  2002/11/03 22:40:28  lazarus
  MG: fixed ControlAtPos

  Revision 1.97  2002/11/01 14:40:31  lazarus
  MG: fixed mouse coords on scrolling wincontrols

  Revision 1.96  2002/10/31 22:14:16  lazarus
  MG: fixed GetClipBox when clipping region invalid

  Revision 1.95  2002/10/31 21:29:47  lazarus
  MG: implemented TControlScrollBar.Size

  Revision 1.94  2002/10/26 15:15:49  lazarus
  MG: broke LCL<->interface circles

  Revision 1.93  2002/10/09 10:22:54  lazarus
  MG: fixed client origin coordinates

  Revision 1.92  2002/10/08 22:32:27  lazarus
  MG: fixed cool little bug (menu double attaching bug)

  Revision 1.91  2002/10/04 20:46:51  lazarus
  MG: improved TComboBox.SetItemIndex

  Revision 1.90  2002/10/04 14:24:15  lazarus
  MG: added DrawItem to TComboBox/TListBox

  Revision 1.89  2002/09/29 15:08:38  lazarus
  MWE: Applied patch from "Andrew Johnson" <aj_genius@hotmail.com>
    Patch includes:
      -fixes Problems with hiding modal forms
      -temporarily fixes TCustomForm.BorderStyle in bsNone
      -temporarily fixes problems with improper tabbing in TSynEdit

  Revision 1.88  2002/09/27 20:52:23  lazarus
  MWE: Applied patch from "Andrew Johnson" <aj_genius@hotmail.com>

  Here is the run down of what it includes -

   -Vasily Volchenko's Updated Russian Localizations

   -improvements to GTK Styles/SysColors
   -initial GTK Palette code - (untested, and for now useless)

   -Hint Windows and Modal dialogs now try to stay transient to
    the main program form, aka they stay on top of the main form
    and usually minimize/maximize with it.

   -fixes to Form BorderStyle code(tool windows needed a border)

   -fixes DrawFrameControl DFCS_BUTTONPUSH to match Win32 better
    when flat

   -fixes DrawFrameControl DFCS_BUTTONCHECK to match Win32 better
    and to match GTK theme better. It works most of the time now,
    but some themes, noteably Default, don't work.

   -fixes bug in Bitmap code which broke compiling in NoGDKPixbuf
    mode.

   -misc other cleanups/ fixes in gtk interface

   -speedbutton's should now draw correctly when flat in Win32

   -I have included an experimental new CheckBox(disabled by
    default) which has initial support for cbGrayed(Tri-State),
    and WordWrap, and misc other improvements. It is not done, it
    is mostly a quick hack to test DrawFrameControl
    DFCS_BUTTONCHECK, however it offers many improvements which
    can be seen in cbsCheck/cbsCrissCross (aka non-themed) state.

   -fixes Message Dialogs to more accurately determine
    button Spacing/Size, and Label Spacing/Size based on current
    System font.
   -fixes MessageDlgPos, & ShowMessagePos in Dialogs
   -adds InputQuery & InputBox to Dialogs

   -re-arranges & somewhat re-designs Control Tabbing, it now
    partially works - wrapping around doesn't work, and
    subcontrols(Panels & Children, etc) don't work. TabOrder now
    works to an extent. I am not sure what is wrong with my code,
    based on my other tests at least wrapping and TabOrder SHOULD
    work properly, but.. Anyone want to try and fix?

   -SynEdit(Code Editor) now changes mouse cursor to match
    position(aka over scrollbar/gutter vs over text edit)

   -adds a TRegion property to Graphics.pp, and Canvas. Once I
    figure out how to handle complex regions(aka polygons) data
    properly I will add Region functions to the canvas itself
    (SetClipRect, intersectClipRect etc.)

   -BitBtn now has a Stored flag on Glyph so it doesn't store to
    lfm/lrs if Glyph is Empty, or if Glyph is not bkCustom(aka
    bkOk, bkCancel, etc.) This should fix most crashes with older
    GDKPixbuf libs.

  Revision 1.87  2002/09/16 15:42:17  lazarus
  MG: fixed calling DestroyHandle if not HandleAllocated

  Revision 1.86  2002/09/10 06:49:19  lazarus
  MG: scrollingwincontrol from Andrew

  Revision 1.85  2002/09/05 12:11:43  lazarus
  MG: TNotebook is now streamable

  Revision 1.84  2002/09/03 08:07:20  lazarus
  MG: image support, TScrollBox, and many other things from Andrew

  Revision 1.83  2002/09/01 16:11:22  lazarus
  MG: double, triple and quad clicks now works

  Revision 1.82  2002/08/31 11:37:09  lazarus
  MG: fixed destroying combobox

  Revision 1.81  2002/08/30 12:32:21  lazarus
  MG: MoveWindowOrgEx, Splitted FWinControls/FControls, TControl drawing, Better DesignerDrawing, ...

  Revision 1.80  2002/08/30 06:46:03  lazarus

  Use comboboxes. Use history. Prettify the dialog. Preselect text on show.
  Make the findreplace a dialog. Thus removing resiying code (handled by Anchors now anyway).
  Make Anchors work again and publish them for various controls.
  SelStart and Co. for TEdit, SelectAll procedure for TComboBox and TEdit.
  Clean up and fix some bugs for TComboBox, plus selection stuff.

  Revision 1.79  2002/08/25 14:32:11  lazarus
  MG: calendar now ignores double clicks

  Revision 1.78  2002/08/24 12:54:59  lazarus
  MG: fixed mouse capturing, OI edit focus

  Revision 1.77  2002/08/24 06:51:22  lazarus
  MG: from Andrew: style list fixes, autosize for radio/checkbtns

  Revision 1.76  2002/08/17 15:45:33  lazarus
  MG: removed ClientRectBugfix defines

  Revision 1.75  2002/08/05 07:43:28  lazarus
  MG: fixed BadCursor bug and Circle Reference of FixedWidget of csPanel

  Revision 1.74  2002/06/21 15:41:56  lazarus
  MG: moved RectVisible, ExcludeClipRect and IntersectClipRect to interface dependent functions

  Revision 1.73  2002/06/19 19:46:09  lazarus
  MG: Form Editing: snapping, guidelines, modified on move/resize, creating components in csDesigning, ...

  Revision 1.72  2002/06/11 13:41:09  lazarus
  MG: fixed mouse coords and fixed mouse clicked thru bug

  Revision 1.71  2002/05/28 15:05:59  lazarus
  MG: reduced output

  Revision 1.70  2002/05/28 14:58:30  lazarus
  MG: added scrollbars for TListView

  Revision 1.69  2002/05/27 14:38:34  lazarus
  MG; fixed find declaration of overloaded procs and expression input types

  Revision 1.68  2002/05/24 07:16:32  lazarus
  MG: started mouse bugfix and completed Makefile.fpc

  Revision 1.67  2002/05/13 14:47:00  lazarus
  MG: fixed client rectangles, TRadioGroup, RecreateWnd

  Revision 1.66  2002/05/12 04:56:20  lazarus
  MG: client rect bugs nearly completed

  Revision 1.65  2002/05/10 06:05:56  lazarus
  MG: changed license to LGPL

  Revision 1.64  2002/05/09 12:41:28  lazarus
  MG: further clientrect bugfixes

  Revision 1.63  2002/05/06 08:50:36  lazarus
  MG: replaced logo, increased version to 0.8.3a and some clientrectbugfix

  Revision 1.62  2002/04/28 14:10:31  lazarus
  MG: fixes for saving resource files

  Revision 1.61  2002/04/27 15:35:50  lazarus
  MG: fixed window shrinking

  Revision 1.60  2002/04/24 16:11:17  lazarus
  MG: started new client rectangle

  Revision 1.59  2002/04/22 13:07:45  lazarus
  MG: fixed AdjustClientRect of TGroupBox

  Revision 1.58  2002/04/18 08:13:36  lazarus
  MG: added include comments

  Revision 1.56  2002/04/04 12:25:01  lazarus
  MG: changed except statements to more verbosity

  Revision 1.55  2002/03/31 23:20:38  lazarus
  MG: fixed initial size of TPage

  Revision 1.54  2002/03/29 14:32:49  lazarus
  MG: further internationalization

  Revision 1.53  2002/03/25 17:59:20  lazarus
  GTK Cleanup
  Shane

  Revision 1.52  2002/03/16 21:40:55  lazarus
  MG: reduced size+move messages between lcl and interface

  Revision 1.51  2002/03/14 23:25:52  lazarus
  MG: fixed TBevel.Create and TListView.Destroy

  Revision 1.50  2002/03/13 22:48:16  lazarus
  Constraints implementation (first cut) and sizig - moving system rework to
  better match Delphi/Kylix way of doing things (the existing implementation
  worked by acident IMHO :-)

  Revision 1.49  2002/01/21 14:17:47  lazarus
  MG: added find-block-start and renamed find-block-other-end

  Revision 1.48  2002/01/01 15:50:15  lazarus
  MG: fixed initial component aligning

  Revision 1.47  2001/12/31 22:43:00  lazarus
  Added a TViewColumn editor to be used in the object inspector as TViewColumn's property editor.
  Shane

  Revision 1.46  2001/12/28 15:12:02  lazarus
  MG: LM_SIZE and LM_MOVE messages are now send directly, not queued

  Revision 1.45  2001/12/20 14:41:20  lazarus
  Fixed setfocus for TComboBox and TMemo
  Shane

  Revision 1.44  2001/12/08 08:54:45  lazarus
  MG: added TControl.Refresh

  Revision 1.43  2001/12/07 20:12:15  lazarus
  Added a watch dialog.
  Shane

  Revision 1.42  2001/11/10 10:48:00  lazarus
  MG: fixed set formicon on invisible forms

  Revision 1.41  2001/11/09 19:14:23  lazarus
  HintWindow changes
  Shane

  Revision 1.40  2001/10/31 16:29:22  lazarus
  Fixed the gtk mousemove bug where the control gets the coord's based on it's parent instead of itself.
  Shane

  Revision 1.39  2001/10/10 17:55:04  lazarus
  MG: fixed caret lost, gtk cleanup, bracket lvls, bookmark saving

  Revision 1.38  2001/10/07 07:28:33  lazarus
  MG: fixed setpixel and TCustomForm.OnResize event

  Revision 1.37  2001/10/03 17:34:27  lazarus
  MG: activated TCustomForm.OnCreate event

  Revision 1.35  2001/08/07 11:05:51  lazarus
  MG: small bugfixes

  Revision 1.34  2001/06/27 21:43:23  lazarus
  MG: added project bookmark support

  Revision 1.33  2001/06/15 10:31:06  lazarus
  MG: set longstrings as default

  Revision 1.32  2001/06/14 14:57:59  lazarus
  MG: small bugfixes and less notes

  Revision 1.31  2001/06/05 10:32:05  lazarus
  MG: small bugfixes for bitbtn, handles

  Revision 1.30  2001/05/16 10:00:00  lazarus
  MG: fixed wrong page index in editor closing

  Revision 1.29  2001/05/13 22:07:08  lazarus
  Implemented BringToFront / SendToBack.

  Revision 1.28  2001/04/02 14:45:26  lazarus
  MG: bugfixes for TBevel

  Revision 1.27  2001/03/27 14:27:43  lazarus
  Changes from Nagy Zsolt
  Shane

  Revision 1.26  2001/03/26 14:58:31  lazarus
  MG: setwindowpos + bugfixes

  Revision 1.24  2001/03/19 14:38:39  lazarus
  MG: fixed many unreleased DC and GDIObj bugs

  Revision 1.21  2001/03/12 12:17:02  lazarus
  MG: fixed random function results

  Revision 1.20  2001/02/28 13:17:33  lazarus
  Added some debug code for the top,left reporting problem.
  Shane

  Revision 1.19  2001/02/06 20:59:17  lazarus
  Trying to get the last control of the last form focused when a dialog closes.
  Still working on it.
  Shane

  Revision 1.18  2001/02/06 18:19:37  lazarus
  Shane

  Revision 1.17  2001/02/06 14:52:47  lazarus
  Changed TSpeedbutton in gtkobject so it erases itself when it's set to visible=false;
  Shane

  Revision 1.15  2001/02/04 04:18:12  lazarus
  Code cleanup and JITFOrms bug fix.
  Shane

  Revision 1.14  2001/02/01 19:34:50  lazarus
  TScrollbar created and a lot of code added.

  It's cose to working.
  Shane

  Revision 1.13  2001/01/30 18:15:02  lazarus
  Added code for TStatusBar
  I'm now capturing WMPainT and doing the drawing myself.
  Shane

  Revision 1.12  2001/01/28 21:06:07  lazarus
  Changes for TComboBox events KeyPress Focus.
  Shane

  Revision 1.11  2001/01/23 23:33:54  lazarus
  MWE:
    - Removed old LM_InvalidateRect
    - did some cleanup in old  code
    + added some comments  on gtkobject data (gtkproc)

  Revision 1.10  2001/01/18 13:27:31  lazarus
  Minor changees
  Shane

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
