{%MainUnit ../graphics.pp}
{******************************************************************************
                                     TBrush
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

{ TBrushHandleCache }

procedure TBrushHandleCache.RemoveItem(Item: TResourceCacheItem);
begin
  //debugln('TBrushHandleCache.RemoveItem ',HexStr(Cardinal(Item.Handle),8));
  DeleteObject(Item.Handle);
  inherited RemoveItem(Item);
end;

constructor TBrushHandleCache.Create;
begin
  inherited Create(SizeOf(TBrushData));
end;


{ TBrush }

{------------------------------------------------------------------------------
  Method: TBrush.SetColor
  Params: Value: the new value
  Returns:  nothing

  Sets the style of a brush
 ------------------------------------------------------------------------------}
Procedure TBrush.SetColor(Value : TColor);
begin
  if FBrushData.Color <> Value
  then begin
    FreeHandle;
    FBrushData.Color := Value;
    Changed;
  end;
end;

{------------------------------------------------------------------------------
  Method: TBrush.SetStyle
  Params: Value: the new value
  Returns:  nothing

  Sets the style of a brush
 ------------------------------------------------------------------------------}
Procedure TBrush.SetStyle(Value : TBrushStyle);
begin
  if FBrushData.Style <> Value
  then begin
    FreeHandle;
    FBrushData.Style := Value;
    Changed;
  end;
end;

{------------------------------------------------------------------------------
  Method: TBrush.SetBitmap
  Params: Value: the new value
  Returns:  nothing

  Sets the style of a brush
 ------------------------------------------------------------------------------}
Procedure TBrush.SetBitmap(Value : TBitmap);
begin
  if FBrushData.Bitmap <> Value
  then begin
    FreeHandle;
    FBrushData.Bitmap := Value;
    Changed;
  end;
end;

{------------------------------------------------------------------------------
  Method:  TBrush.Create
  Params:  none
  Returns: Nothing

  Constructor for the class.
 ------------------------------------------------------------------------------}
constructor TBrush.Create;
begin
  inherited Create;
  with FBrushData do
  begin
    Bitmap := nil;
    Handle := 0;
    Color := clWhite;
    Style := bsSolid;
  end;
end;

{------------------------------------------------------------------------------
  Method: TBrush.Destroy
  Params:  None
  Returns: Nothing

  Destructor for the class.
 ------------------------------------------------------------------------------}
destructor TBrush.Destroy;
begin
  FreeHandle;
  inherited Destroy;
end;

{------------------------------------------------------------------------------
  Method: TBrush.Assign
  Params: Source: Another brush
  Returns:  nothing

  Copies the source brush to itself
 ------------------------------------------------------------------------------}
Procedure TBrush.Assign(Source : Tpersistent);
begin
  if Source is TBrush
  then begin
    Bitmap := TBrush(Source).Bitmap;
    Color := TBrush(Source).Color;
    Style := TBrush(Source).Style;
  end
  else
    inherited Assign(Source);
end;

{------------------------------------------------------------------------------
  Method: TBrush.SetHandle
  Params:   a brush handle
  Returns:  nothing

  sets the brush to an external created brush
 ------------------------------------------------------------------------------}
procedure TBrush.SetHandle(const Value: HBRUSH);
begin
  if FBrushData.Handle <> Value
  then begin
    FreeHandle;
    FBrushData.Handle := Value;
    //TODO: query new parameters
    Changed;
  end;
end;

{------------------------------------------------------------------------------
  Function: TBrush.GetHandle
  Params:   none
  Returns:  a handle to a brush gdiobject

  Creates a brush if needed
 ------------------------------------------------------------------------------}
function TBrush.GetHandle: HBRUSH;
var
  LogBrush: TLogBrush;
  CachedBrush: TBlockResourceCacheDescriptor;
begin
  if FBrushData.Handle = 0
  then begin
    FillChar(LogBrush,SizeOf(LogBrush),0);
    with LogBrush do
    begin
      if FBrushData.Bitmap <> nil
      then begin
        lbStyle := BS_PATTERN;
        //FBrushData.Bitmap.HandleType := bmDDB;
        lbHatch := FBrushData.Bitmap.Handle;
      end else
      begin
        lbHatch := 0;
        case FBrushData.Style of
          bsSolid: lbStyle := BS_SOLID;
          bsClear: lbStyle := BS_HOLLOW;
        else
          lbStyle := BS_HATCHED;
          lbHatch := Ord(FBrushData.Style) - Ord(bsHorizontal);
        end;
      end;
      lbColor := ColorRef(FBrushData.Color);
    end;
    CachedBrush:=BrushResourceCache.FindDescriptor(@LogBrush);
    if CachedBrush<>nil then begin
      CachedBrush.Item.IncreaseRefCount;
      FBrushData.Handle := CachedBrush.Item.Handle;
    end else begin
      FBrushData.Handle := CreateBrushIndirect(LogBrush);
      BrushResourceCache.AddResource(FBrushData.Handle,@LogBrush);
    end;
    FBrushHandleCached:=true;
  end;

  Result := FBrushData.Handle;
end;

{------------------------------------------------------------------------------
  Method:  TBrush.FreeHandle
  Params:  none
  Returns: Nothing

  Frees a brushhandle if needed
 ------------------------------------------------------------------------------}
procedure TBrush.FreeHandle;
begin
  if FBrushData.Handle <> 0
  then begin
    //TODO: what if a brush is currently selected
    if FBrushHandleCached then begin
      BrushResourceCache.FindItem(FBrushData.Handle).DecreaseRefCount;
      FBrushHandleCached:=false;
    end else
      DeleteObject(FBrushData.Handle);
    FBrushData.Handle := 0;
  end;
end;

{ =============================================================================

  $Log$
  Revision 1.9  2004/08/11 21:10:30  mattias
  implemented TBrushHandleCache

  Revision 1.8  2004/08/11 20:57:09  mattias
  moved intfstrconsts.pp to lclstrconsts.pas, implemented TPenHandleCache

  Revision 1.7  2004/04/10 17:58:56  mattias
  implemented mainunit hints for include files

  Revision 1.6  2004/03/06 17:12:18  mattias
  fixed CreateBrushIndirect

  Revision 1.5  2003/12/23 11:16:41  mattias
  started key combinations, fixed some range check errors

  Revision 1.4  2002/10/31 04:27:59  lazarus
  AJ: added TShape

  Revision 1.3  2002/09/18 17:07:24  lazarus
  MG: added patch from Andrew

  Revision 1.2  2002/05/10 06:05:51  lazarus
  MG: changed license to LGPL

  Revision 1.1  2000/07/13 10:28:24  michael
  + Initial import

  Revision 1.4  2000/05/10 22:52:57  lazarus
  MWE:
    = Moved some global api stuf to gtkobject

  Revision 1.3  2000/05/09 02:07:40  lazarus
  Replaced writelns with Asserts.                          CAW

  Revision 1.2  2000/05/08 15:56:58  lazarus
  MWE:
    + Added support for mwedit92 in Makefiles
    * Fixed bug # and #5 (Fillrect)
    * Fixed labelsize in ApiWizz
    + Added a call to the resize event in WMWindowPosChanged

  Revision 1.1  2000/04/02 20:49:55  lazarus
  MWE:
    Moved lazarus/lcl/*.inc files to lazarus/lcl/include

  Revision 1.5  2000/03/06 00:05:05  lazarus
  MWE: Added changes from Peter Dyson <peter@skel.demon.co.uk> for a new
    release of mwEdit (0.92)

  Revision 1.4  1999/12/02 19:00:59  lazarus
  MWE:
    Added (GDI)Pen
    Changed (GDI)Brush
    Changed (GDI)Font (color)
    Changed Canvas to use/create pen/brush/font
    Hacked mwedit to allow setting the number of chars (till it get a WM/LM_SIZE event)
    The editor shows a line !


}
