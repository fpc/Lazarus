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
  inherited Destroy;         
end;                                                                                              

{------------------------------------------------------------------------------
  Method: TCustomControl.Paint
  Params:   none
  Returns:  nothing

  Default paint handler. Derived classed should paint themselves 
 ------------------------------------------------------------------------------}
Procedure TCustomControl.Paint;                                            
begin                                                                                             
end;                                                                                              

{------------------------------------------------------------------------------
  Method: TCustomControl.LMPaint
  Params:   Msg: The paint message
  Returns:  nothing

  Paint event handler.
 ------------------------------------------------------------------------------}
procedure TCustomControl.WMPaint(var Message: TLMPaint);
begin
  Include(FControlState, csCustomPaint);
  inherited WMPaint(Message);
  Paint; // TODO move this to the PaintWindow function;
  Exclude(FControlState, csCustomPaint);
end;                          
 
                                                                    

{ =============================================================================

  $Log$
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
