{%MainUnit ../controls.pp}

{******************************************************************************
                                     TGraphicControl
 ******************************************************************************
 
 *****************************************************************************
 *                                                                           *
 *  This file is part of the Lazarus Component Library (LCL)                 *
 *                                                                           *
 *  See the file COPYING.modifiedLGPL.txt, included in this distribution,    *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
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
  Canvas.Font:=Font;
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
  Assert(False, Format('Trace:[TGraphicControl.Paint] %s', [ClassName]));
  if Assigned(FOnPaint) then FOnPaint(Self);
end;

procedure TGraphicControl.DoOnChangeBounds;
begin
  inherited DoOnChangeBounds;
  // reset canvas handle in next access
  if Canvas.HandleAllocated then
    TControlCanvas(Canvas).FreeHandle;
end;

procedure TGraphicControl.DoOnParentHandleDestruction;
begin
  inherited;
  if Canvas.HandleAllocated then
    TControlCanvas(Canvas).FreeHandle;
end;

procedure TGraphicControl.Update;
var
  R: TRect;
  Pt: TPoint;
begin
  if Assigned(Parent) and Parent.HandleAllocated and Parent.Visible then
  begin
    R := ClientRect;
    Pt := Parent.GetClientScrollOffset;
    OffsetRect(R, Left - Pt.X, Top - Pt.Y);
    R.Right := R.Right + Pt.X;
    R.Bottom := R.Bottom + Pt.Y;
    LCLIntf.InvalidateRect(Parent.Handle, @R, True);
  end;
end;

