{%MainUnit ../graphics.pp}
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

{ TPenHandleCache }

procedure TPenHandleCache.RemoveItem(Item: TResourceCacheItem);
begin
  if Item=nil then
    RaiseGDBException('TPenHandleCache.RemoveItem');
  DeleteObject(Item.Handle);
  inherited RemoveItem(Item);
end;

constructor TPenHandleCache.Create;
begin
  inherited Create(SizeOf(TLogPen));
end;

{ TPen }

{------------------------------------------------------------------------------
  Method: TPen.SetColor
  Params: Value: the new value
  Returns:  nothing

  Sets the style of a pen
 ------------------------------------------------------------------------------}
Procedure TPen.SetColor(Value : TColor);
begin
  if FColor <> value
  then begin
    FreeHandle;
    FColor := value;
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
  if FStyle <> Value
  then begin
    FreeHandle;
    FStyle := Value;
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
  if Width <> Value
  then begin
    FreeHandle;
    FWidth := Value;
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
  FWidth := 1;
  FHandle := 0;
  FStyle := psSolid;
  FColor := clBlack;
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
  if FHandle <> Value
  then begin
    FreeHandle;
    FHandle := Value;
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
  PEN_STYLES: array[TPenStyle] of Word = (
    PS_SOLID,PS_DASH,PS_DOT,PS_DASHDOT,PS_DASHDOTDOT,PS_NULL,PS_INSIDEFRAME);
var
  LogPen: TLogPen;
  CachedPen: TBlockResourceCacheDescriptor;
begin
  if FHandle = 0
  then begin
    FillChar(LogPen,SizeOf(LogPen),0);
    with LogPen do
    begin
      lopnStyle := PEN_STYLES[FStyle];
      lopnWidth.X := FWidth;
      lopnColor := FColor;
    end;
    CachedPen:=PenResourceCache.FindDescriptor(@LogPen);
    if CachedPen<>nil then begin
      CachedPen.Item.IncreaseRefCount;
      FHandle := CachedPen.Item.Handle;
    end else begin
      FHandle := CreatePenIndirect(LogPen);
      PenResourceCache.AddResource(FHandle,@LogPen);
    end;
    FPenHandleCached:=true;
  end;

  Result := FHandle;
end;

{------------------------------------------------------------------------------
  Method:  TPen.FreeHandle
  Params:  none
  Returns: Nothing

  Frees a penhandle if needed
 ------------------------------------------------------------------------------}
procedure TPen.FreeHandle;
begin
  if FHandle <> 0
  then begin
    // Changing triggers deselecting the current handle
    Changing;
    if FPenHandleCached then begin
      PenResourceCache.FindItem(FHandle).DecreaseRefCount;
      FPenHandleCached:=false;
    end else
      DeleteObject(FHandle);
    FHandle := 0;
  end;
end;

{$IFDEF UseFPCanvas}
procedure TPen.DoAllocateResources;
begin
  inherited DoAllocateResources;
  GetHandle;
end;

procedure TPen.DoDeAllocateResources;
begin
  FreeHandle;
  inherited DoDeAllocateResources;
end;

procedure TPen.DoCopyProps(From: TFPCanvasHelper);
begin
  if From is TPen then begin
    FreeHandle;
    inherited DoCopyProps(From);
    //TODO: query new parameters
    Changed;
  end else
    inherited DoCopyProps(From);
end;
{$ENDIF}

{ =============================================================================

  $Log$
  Revision 1.15  2004/12/22 23:54:21  mattias
  started TControl.AnchorSide

  Revision 1.14  2004/12/22 19:56:44  mattias
  started TFont mirgration to fpCanvas font

  Revision 1.13  2004/11/07 01:36:18  mattias
  fixed cleaning up unused resource cache item lists

  Revision 1.12  2004/11/07 01:10:05  mattias
  fixed double calling destructor for resource cache items

  Revision 1.11  2004/08/11 22:05:07  mattias
  fixed brush handle cache size

  Revision 1.10  2004/08/11 20:57:09  mattias
  moved intfstrconsts.pp to lclstrconsts.pas, implemented TPenHandleCache

  Revision 1.9  2004/04/10 17:58:57  mattias
  implemented mainunit hints for include files

  Revision 1.8  2003/12/26 10:16:54  mattias
  changed TColorRef from longword to longint

  Revision 1.7  2003/12/23 11:16:41  mattias
  started key combinations, fixed some range check errors

  Revision 1.6  2003/12/02 12:25:17  micha
  try: gdi memory leak fix for pen

  Revision 1.5  2002/08/18 04:57:01  mattias
  fixed csDashDot

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
