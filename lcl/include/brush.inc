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
  DeleteObject(Item.Handle);
  inherited RemoveItem(Item);
end;

constructor TBrushHandleCache.Create;
begin
  inherited Create(SizeOf(TLogBrush));
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
  if FColor <> Value
  then begin
    FreeHandle;
    {$IFDEF UseFPCanvas}
    SetColor(Value,TColorToFPColor(Value));
    {$ELSE}
    FColor := Value;
    Changed;
    {$ENDIF}
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
  if Style <> Value then begin
    FreeHandle;
    {$IFDEF UseFPCanvas}
    inherited SetStyle(Value);
    {$ELSE}
    FStyle := Value;
    {$ENDIF}
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
  if FBitmap <> Value
  then begin
    FreeHandle;
    FBitmap := Value;
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
  FBitmap := nil;
  FHandle := 0;
  FColor := clWhite;
  {$IFDEF UseFPCanvas}
  DelayAllocate:=true;
  inherited SetStyle(bsSolid);
  {$ELSE}
  FStyle := bsSolid;
  {$ENDIF}
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
Procedure TBrush.Assign(Source: TPersistent);
begin
  if Source is TBrush
  then begin
    Bitmap := TBrush(Source).Bitmap;
    {$IFDEF UseFPCanvas}
    SetColor(TBrush(Source).Color,TFPCanvasHelper(Source).FPColor);
    {$ELSE}
    Color := TBrush(Source).Color;
    {$ENDIF}
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
  if FHandle <> Value
  then begin
    FreeHandle;
    FHandle := Value;
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
  if FHandle = 0
  then begin
    FillChar(LogBrush,SizeOf(LogBrush),0);
    with LogBrush do
    begin
      if FBitmap <> nil
      then begin
        lbStyle := BS_PATTERN;
        //FBrushData.Bitmap.HandleType := bmDDB;
        lbHatch := FBitmap.Handle;
      end else
      begin
        lbHatch := 0;
        case Style of
          bsSolid: lbStyle := BS_SOLID;
          bsClear: lbStyle := BS_HOLLOW;
        else
          lbStyle := BS_HATCHED;
          lbHatch := Ord(Style) - Ord(bsHorizontal);
        end;
      end;
      lbColor := ColorRef(FColor);
    end;
    CachedBrush:=BrushResourceCache.FindDescriptor(@LogBrush);
    if CachedBrush<>nil then begin
      CachedBrush.Item.IncreaseRefCount;
      FHandle := CachedBrush.Item.Handle;
    end else begin
      FHandle := CreateBrushIndirect(LogBrush);
      BrushResourceCache.AddResource(FHandle,@LogBrush);
    end;
    FBrushHandleCached:=true;
  end;

  Result := FHandle;
end;

{------------------------------------------------------------------------------
  Method:  TBrush.FreeHandle
  Params:  none
  Returns: Nothing

  Frees a brushhandle if needed
 ------------------------------------------------------------------------------}
procedure TBrush.FreeHandle;
begin
  if FHandle <> 0
  then begin
    if FBrushHandleCached then begin
      BrushResourceCache.FindItem(FHandle).DecreaseRefCount;
      FBrushHandleCached:=false;
    end else
      DeleteObject(FHandle);
    FHandle := 0;
  end;
end;

procedure TBrush.DoChange(var Msg);
begin
  Changed;
end;

{$IFDEF UseFPCanvas}
procedure TBrush.DoAllocateResources;
begin
  inherited DoAllocateResources;
  GetHandle;
end;

procedure TBrush.DoDeAllocateResources;
begin
  FreeHandle;
  inherited DoDeAllocateResources;
end;

procedure TBrush.DoCopyProps(From: TFPCanvasHelper);
begin
  if From is TBrush then begin
    FreeHandle;
    inherited DoCopyProps(From);
    //TODO: query new parameters
    Changed;
  end else
    inherited DoCopyProps(From);
end;

procedure TBrush.SetColor(const NewColor: TColor; const NewFPColor: TFPColor);
begin
  if (NewColor=Color) and (NewFPColor=FPColor) then exit;
  FColor:=NewColor;
  inherited SetFPColor(NewFPColor);
  Changed;
end;

procedure TBrush.SetFPColor(const AValue: TFPColor);
begin
  if FPColor=AValue then exit;
  SetColor(FPColorToTColor(AValue),AValue);
end;
{$ENDIF}

{ =============================================================================

  $Log$
  Revision 1.16  2005/01/10 18:44:44  mattias
  implemented the fpCanvas support for the LCL - Compile with -dUseFPCanvas

  Revision 1.15  2005/01/08 15:06:06  mattias
  fixed TabOrder dialog for new TabOrder

  Revision 1.14  2005/01/07 21:02:59  mattias
  TFont, TBrush, TPen can now be used with fpCanvas

  Revision 1.13  2004/12/22 23:54:21  mattias
  started TControl.AnchorSide

  Revision 1.12  2004/11/07 01:10:05  mattias
  fixed double calling destructor for resource cache items

  Revision 1.11  2004/10/01 13:16:44  mattias
  fixed unselecting TCanvas objects

  Revision 1.10  2004/08/11 22:05:07  mattias
  fixed brush handle cache size

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
