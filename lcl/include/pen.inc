{******************************************************************************
                                     TPen
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
  Method: TPen.SetColor
  Params: Value: the new value
  Returns:  nothing

  Sets the style of a pen
 ------------------------------------------------------------------------------}
Procedure TPen.SetColor(Value : TColor);
begin
  if FPenData.Color <> value 
  then begin
    FreeHandle;
    FPenData.Color := value;
    Changed;
  end;
end;

{------------------------------------------------------------------------------
  Method: TPen.SetStyle
  Params: Value: the new value
  Returns:  nothing

  Sets the style of a pen
 ------------------------------------------------------------------------------}
Procedure TPen.SetStyle(Value : TPenStyle);
begin
  if FPenData.Style <> Value
  then begin
    FreeHandle;
    FPenData.Style := Value;
    Changed;
  end;
end;

{------------------------------------------------------------------------------
  Method: TPen.SetMode
  Params: Value: the new value
  Returns:  nothing

  Sets the Mode of a pen
 ------------------------------------------------------------------------------}
Procedure TPen.SetMode(Value : TPenMode);
begin
  if FMode <> Value
  then begin
    FMode := Value;
    Changed;
  end;
end;

{------------------------------------------------------------------------------
  Method: TPen.SetWidth
  Params: Value: the new value
  Returns:  nothing

  Sets the style of a pen
 ------------------------------------------------------------------------------}
Procedure TPen.SetWidth(Value : Integer);
begin
  if FPenData.Width <> Value 
  then begin
    FreeHandle;
    FPenData.Width := Value;
    Changed;
  end;
end;

{------------------------------------------------------------------------------
  Method:  TPen.Create
  Params:  none
  Returns: Nothing

  Constructor for the class.
 ------------------------------------------------------------------------------}
constructor TPen.Create;
begin
  inherited Create;
  with FPenData do
  begin
    Width := 1;
    Handle := 0;
    Style := psSolid;
    Color := clBlack;
  end;
  FMode := pmCopy;  
end;

{------------------------------------------------------------------------------
  Method: TPen.Destroy
  Params:  None
  Returns: Nothing

  Destructor for the class.
 ------------------------------------------------------------------------------}
destructor TPen.Destroy;
begin
  FreeHandle;
  inherited Destroy;
end;

{------------------------------------------------------------------------------
  Method: TPen.Assign
  Params: Source: Another pen
  Returns:  nothing

  Copies the source pen to itself
 ------------------------------------------------------------------------------}
Procedure TPen.Assign(Source : Tpersistent);
begin
  if Source is TPen
  then begin
    Width := TPen(Source).Width;
    Color := TPen(Source).Color;
    Style := TPen(Source).Style;
  end
  else
    inherited Assign(Source);
end;

{------------------------------------------------------------------------------
  Method: TPen.SetHandle
  Params:   a pen handle
  Returns:  nothing

  sets the pen to an external created pen 
 ------------------------------------------------------------------------------}
procedure TPen.SetHandle(const Value: HPEN);
begin
  if FPenData.Handle <> Value 
  then begin
    FreeHandle;
    FPenData.Handle := Value;
    //TODO: query new parameters
    Changed;
  end;
end;

{------------------------------------------------------------------------------
  Function: TPen.GetHandle
  Params:   none
  Returns:  a handle to a pen gdiobject

  Creates a pen if needed
 ------------------------------------------------------------------------------}
function TPen.GetHandle: HPEN;
const
  PEN_STYLES: array[TPenStyle] of Word = (PS_SOLID, PS_DASH, PS_DOT, PS_DASHDOT, PS_DASHDOTDOT, PS_NULL,PS_INSIDEFRAME);
var
  LogPen: TLogPen;
begin
  if FPenData.Handle = 0
  then begin
    with LogPen do
    begin
      lopnStyle := PEN_STYLES[FPenData.Style];
      lopnWidth.X := FPenData.Width;
      lopnColor := FPenData.Color;
    end;
    FPenData.Handle := CreatePenIndirect(LogPen);
  end;
  
  Result := FPenData.Handle;
end;

{------------------------------------------------------------------------------
  Method:  TPen.FreeHandle
  Params:  none
  Returns: Nothing

  Frees a penhandle if needed
 ------------------------------------------------------------------------------}
procedure TPen.FreeHandle;
begin
  if FPenData.Handle <> 0 
  then begin
    //TODO: what if a pen is currently selected
    DeleteObject(FPenData.Handle);
    FPenData.Handle := 0;
  end;
end;

{ =============================================================================

  $Log$
  Revision 1.4  2002/10/31 04:27:59  lazarus
  AJ: added TShape

  Revision 1.3  2002/09/18 17:07:25  lazarus
  MG: added patch from Andrew

  Revision 1.2  2002/05/10 06:05:55  lazarus
  MG: changed license to LGPL

  Revision 1.1  2000/07/13 10:28:27  michael
  + Initial import

  Revision 1.1  2000/04/02 20:49:56  lazarus
  MWE:
    Moved lazarus/lcl/*.inc files to lazarus/lcl/include

  Revision 1.2  1999/12/02 19:00:59  lazarus
  MWE:
    Added (GDI)Pen
    Changed (GDI)Brush
    Changed (GDI)Font (color)
    Changed Canvas to use/create pen/brush/font
    Hacked mwedit to allow setting the number of chars (till it get a WM/LM_SIZE event)
    The editor shows a line !


}
