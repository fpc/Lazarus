{%MainUnit ../graphics.pp}

{******************************************************************************
                                     TREGION
 ******************************************************************************

 *****************************************************************************
 *                                                                           *
 *  This file is part of the Lazarus Component Library (LCL)                 *
 *                                                                           *
 *  See the file COPYING.modifiedLGPL, included in this distribution,        *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************
}

{------------------------------------------------------------------------------
  Method:  TRegion.Create
  Params:  none
  Returns: Nothing

  Constructor for the class.
 ------------------------------------------------------------------------------}
constructor TRegion.Create;
begin
  inherited Create;
  FillChar(FRegionData,SizeOf(FRegionData), 0);
end;

{------------------------------------------------------------------------------
  Method: TRegion.Destroy
  Params:  None
  Returns: Nothing

  Destructor for the class.
 ------------------------------------------------------------------------------}
destructor TRegion.Destroy;
begin
  FreeHandle;
  inherited Destroy;
end;

{------------------------------------------------------------------------------
  Method: TRegion.Assign
  Params: Source: Another brush
  Returns:  nothing

  Copies the source brush to itself
 ------------------------------------------------------------------------------}
Procedure TRegion.Assign(Source : Tpersistent);
begin
  if Source is TRegion
  then begin
    Self.ClipRect := TRegion(Source).ClipRect;
    //TODO : Get Polygon INFO
  end;

  inherited Assign(Source);
end;

{------------------------------------------------------------------------------
  Method: TRegion.SetHandle
  Params:   a region handle
  Returns:  nothing

  sets the region to an external created region
 ------------------------------------------------------------------------------}
procedure TRegion.SetHandle(const Value: HRGN);
begin
  if FRegionData.Handle <> Value
  then begin
    FreeHandle;
    FRegionData.Handle := Value;
    FRegionData.Rect := GetClipRect;
    //TODO: query polygon params
    Changed;
  end;
end;

{------------------------------------------------------------------------------
  Function: TRegion.GetHandle
  Params:   none
  Returns:  a handle to a Region gdiobject

  Creates a region if needed
 ------------------------------------------------------------------------------}
function TRegion.GetHandle: HRGN;
var
  FillMode: integer;
begin
  If FRegiondata.Handle = 0 then
    With FRegionData do
      if (Polygon <> nil) or (((Rect.Right - Rect.Left) <> 0) or ((Rect.Bottom - Rect.Top) <> 0))
      then
        If Polygon <> nil then begin
          if Winding then
            FillMode:=LCLType.Winding
          else
            FillMode:=Alternate;
          FRegionData.Handle := CreatePolygonRgn(Polygon, NumPoints, FillMode);
        end else
          With Rect do
            FRegionData.Handle := CreateRectRgn(Left, Top, Right, Bottom);

  Result := FRegionData.Handle;
end;

{------------------------------------------------------------------------------
  Method:  TRegion.FreeHandle
  Params:  none
  Returns: Nothing

  Frees a brushhandle if needed
 ------------------------------------------------------------------------------}
procedure TRegion.FreeHandle;
begin
  if FRegionData.Handle <> 0
  then begin
    DeleteObject(FRegionData.Handle);
    FRegionData.Handle := 0;
  end;
end;

{------------------------------------------------------------------------------
  Method: TRegion.SetClipRect
  Params: Value: the new value
  Returns:  nothing

  Sets/clips the region to a rectangle value
 ------------------------------------------------------------------------------}
procedure TRegion.SetClipRect(Value : TRect);
begin
  if FRegionData.Rect <> Value
  then begin
    FreeHandle;
    FRegionData.Rect := Value;
    If FRegionData.Polygon <> nil then
      ReallocMem(FRegionData.Polygon, 0);

    FRegionData.Polygon := nil;
    FRegionData.NumPoints := 0;
    FRegionData.Winding := False;
    
    Changed;
  end;
end;

{------------------------------------------------------------------------------
  Method: TRegion.GetClipRect
  Params: none
  Returns:  current rect

  Gets the regions current clip rect
 ------------------------------------------------------------------------------}
Function TRegion.GetClipRect : TRect;
begin
  If FRegionData.Handle <> 0 then
    GetRgnBox(FRegionData.Handle, @Result)
  else
    If FRegionData.Polygon <> nil then begin
      GetHandle;
      If FRegionData.Handle <> 0 then
        Result := GetClipRect
      else
        Result := FRegionData.Rect;
      FreeHandle;
    end
    else
      Result := FRegionData.Rect;
end;

// included by graphics.pp

