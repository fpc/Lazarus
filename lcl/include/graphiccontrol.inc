{%MainUnit ../controls.pp}

{******************************************************************************
                                     TGraphicControl
 ******************************************************************************
 
 *****************************************************************************
  This file is part of the Lazarus Component Library (LCL)

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************
}

{------------------------------------------------------------------------------
  Method:  TGraphicControl.Create
  Params:  none
  Returns: Nothing

  Constructor for the class.
 ------------------------------------------------------------------------------}
constructor TGraphicControl.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FCanvas := TControlCanvas.Create;
  TControlCanvas(FCanvas).Control := Self;
end;

{------------------------------------------------------------------------------
  Method: TGraphicControl.Destroy
  Params:  None
  Returns: Nothing

  Destructor for the class.
 ------------------------------------------------------------------------------}
destructor TGraphicControl.Destroy;
begin
  FreeAndNil(FCanvas);
  inherited Destroy;
end;

{------------------------------------------------------------------------------
  Method: TGraphicControl.WMPaint
  Params:   Msg: The paint message
  Returns:  nothing

  Paint event handler.
 ------------------------------------------------------------------------------}
procedure TGraphicControl.WMPaint(var Message: TLMPaint);
begin
  if Message.DC <> 0 then
  begin
    Canvas.Lock;
    try
      //debugln('TGraphicControl.WMPaint A ',DbgSName(Self));
      Canvas.Handle := Message.DC;
      try
        Paint;
      finally
        Canvas.Handle := 0;
      end;
    finally
      Canvas.Unlock;
    end;
  end;
end;

class procedure TGraphicControl.WSRegisterClass;
begin
  inherited WSRegisterClass;
  RegisterGraphicControl;
end;

procedure TGraphicControl.FontChanged(Sender: TObject);
begin
  Canvas.Font.BeginUpdate;
  try
    Canvas.Font.PixelsPerInch := Font.PixelsPerInch;
    Canvas.Font := Font;
  finally
    Canvas.Font.EndUpdate;
  end;
  inherited FontChanged(Sender);
end;

{------------------------------------------------------------------------------
  Method: TGraphicControl.Paint
  Params:   none
  Returns:  nothing

  Default paint handler. Derived classed should paint themselves 
 ------------------------------------------------------------------------------}
procedure TGraphicControl.Paint;
begin
  //DebugLn(Format('Trace:[TGraphicControl.Paint] %s', [ClassName]));
  if Assigned(FOnPaint) then FOnPaint(Self);
end;

procedure TGraphicControl.DoOnChangeBounds;
begin
  inherited DoOnChangeBounds;
  // reset canvas handle in next access
  TControlCanvas(Canvas).FreeHandle;
end;

procedure TGraphicControl.DoOnParentHandleDestruction;
begin
  inherited;
  TControlCanvas(Canvas).FreeHandle;
end;

procedure TGraphicControl.CMCursorChanged(var Message: TLMessage);
var
  Pt: TPoint;
  Ct: TControl;
begin
  if not Visible then exit;
  if Parent <> nil then begin
    // execute only if the cursor is actually over the control
    Pt := Parent.ScreenToControl(Mouse.CursorPos);
    Ct := Parent.ControlAtPos(Pt, True);
    if (Self = Ct) then
      SetTempCursor(FCursor);
  end;
end;

// included by controls.pp

