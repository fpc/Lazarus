{%MainUnit ../controls.pp}

{******************************************************************************
                                     TCustomControl
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
  Method:  TCustomControl.Create
  Params:  none
  Returns: Nothing

  Constructor for the class.
 ------------------------------------------------------------------------------}
constructor TCustomControl.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FCanvas := TControlCanvas.Create;
  TControlCanvas(FCanvas).Control := Self;
  // ----PREVIOUS COMMENT-----
  // custom controls are painted by the LCL, not the interface, so enable
  // double buffering. It's up to the interface to do the actual
  // doublebuffering.
  // ----Micha's comment----
  // If double buffering has to be enabled before LCL can draw anything,
  // something is wrong. In the overridden Paint method you should always be
  // able to draw, independent of DoubleBuffered setting.
  //
  // Disable the switch by default: 
  //  1) it greatly improves resizing speed (it doesn't feel like java then ;) )
  //  2) VCL compatible
  //  3) users of LCL should enable switch per control if they so desire
  DoubleBuffered:=false;
end;

{------------------------------------------------------------------------------
  Method: TCustomControl.Destroy
  Params:  None
  Returns: Nothing

  Destructor for the class.
 ------------------------------------------------------------------------------}
Destructor TCustomControl.Destroy;
begin
  FCanvas.Free;
  FCanvas:=nil;
  inherited Destroy;
end;

procedure TCustomControl.DestroyComponent;
begin
  if FCanvas<>nil then
    TControlCanvas(FCanvas).FreeHandle;
  inherited DestroyComponent;
end;

{------------------------------------------------------------------------------
  Method: TCustomControl.Paint
  Params:   none
  Returns:  nothing

  Default paint handler. Derived classed should paint themselves
 ------------------------------------------------------------------------------}
Procedure TCustomControl.Paint;
begin
  if Assigned(FOnPaint) then FOnPaint(Self);
end;

{------------------------------------------------------------------------------
  Method: TCustomControl.LMPaint
  Params:   Msg: The paint message
  Returns:  nothing

  Paint event handler.
 ------------------------------------------------------------------------------}
procedure TCustomControl.WMPaint(var Message: TLMPaint);
begin
  if (csDestroying in ComponentState) or (not HandleAllocated) then exit;
  Include(FControlState, csCustomPaint);
  inherited WMPaint(Message);
  Exclude(FControlState, csCustomPaint);
end;

{------------------------------------------------------------------------------
  Method: TCustomControl.PaintWindow
  Params:   DC: The device context to paint on
  Returns:  nothing

  This is a plug-in in TWinControl to get the DC, assign it to our canvas and
  call the paint method for descendents to do the actual painting
 ------------------------------------------------------------------------------}
procedure TCustomControl.PaintWindow(DC: HDC);
var
  DCChanged: boolean;
begin
  DCChanged:=(not FCanvas.HandleAllocated) or (FCanvas.Handle<>DC);
  if DCChanged then
    FCanvas.Handle := DC;
  try
    Paint;
  finally
    if DCChanged then FCanvas.Handle := 0;
  end;
end;

// included by controls.pp

{
  $Log$
  Revision 1.12  2005/01/22 22:26:16  mattias
  added sprite example

  Revision 1.11  2004/04/10 17:58:57  mattias
  implemented mainunit hints for include files

  Revision 1.10  2004/03/17 09:44:31  micha
  disable double buffering to improve speed

  Revision 1.9  2004/01/10 22:34:20  mattias
  started double buffering for gtk intf

  Revision 1.8  2004/01/03 23:14:59  mattias
  default font can now change height and fixed gtk crash

  Revision 1.7  2004/01/03 21:06:05  micha
  - fix win32/checklistbox
  - implement proper lcl to interface move/size notify via setwindowpos
  - fix treeview to use inherited canvas from customcontrol
  - implement double buffering in win32

  Revision 1.6  2003/06/20 12:56:53  mattias
  reduced paint messages on destroy

  Revision 1.5  2002/08/25 14:32:10  lazarus
  MG: calendar now ignores double clicks

  Revision 1.4  2002/05/17 10:45:23  lazarus
  MG: finddeclaration for stupid things like var a:a;

  Revision 1.3  2002/05/10 06:05:51  lazarus
  MG: changed license to LGPL

  Revision 1.2  2001/03/21 00:20:29  lazarus
  MG: fixed memory leaks

  Revision 1.1  2000/07/13 10:28:25  michael
  + Initial import

  Revision 1.1  2000/04/02 20:49:56  lazarus
  MWE:
    Moved lazarus/lcl/*.inc files to lazarus/lcl/include

  Revision 1.8  1999/12/14 00:16:43  lazarus
  MWE:
    Renamed LM... message handlers to WM... to be compatible and to
      get more edit parts to compile
    Started to implement GetSystemMetrics
    Removed some Lazarus specific parts from mwEdit

  Revision 1.7  1999/12/07 01:19:25  lazarus
  MWE:
    Removed some double events
    Changed location of SetCallBack
    Added call to remove signals
    Restructured somethings
    Started to add default handlers in TWinControl
    Made some parts of TControl and TWinControl more delphi compatible
    ... and lots more ...


}
