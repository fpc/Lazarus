{%MainUnit ../graphics.pp}

{******************************************************************************
                                     TREGION
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
  Method:  TRegion.Create
  Params:  none
  Returns: Nothing

  Constructor for the class.
 ------------------------------------------------------------------------------}
constructor TRegion.Create;
begin
  inherited Create;
  FillChar(FRegionData, SizeOf(FRegionData), 0);
end;

{------------------------------------------------------------------------------
  Method: TRegion.Destroy
  Params:  None
  Returns: Nothing

  Destructor for the class.
 ------------------------------------------------------------------------------}
destructor TRegion.Destroy;
begin
  FreeReference;
  inherited Destroy;
end;

{------------------------------------------------------------------------------
  Method: TRegion.Assign
  Params: Source: Another brush
  Returns:  nothing

  Copies the source brush to itself
 ------------------------------------------------------------------------------}
procedure TRegion.Assign(Source : Tpersistent);
begin
  if Source is TRegion
  then begin
    Self.ClipRect := TRegion(Source).ClipRect;
    //TODO : Get Polygon INFO
    Exit;
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
  if HRGN(FRegionData.Reference.Handle) = Value then Exit;

  FreeReference;
  FRegionData.Reference._lclHandle := TLCLHandle(Value);
  FRegionData.Rect := GetClipRect;
  //TODO: query polygon params
  Changed;
end;

{------------------------------------------------------------------------------
  Function: TRegion.GetHandle
  Params:   none
  Returns:  a handle to a Region gdiobject

  Creates a region if needed
 ------------------------------------------------------------------------------}
function TRegion.GetHandle: HRGN;
begin
  Result := HRGN(Reference.Handle);
end;

function TRegion.GetReference: TWSRegionReference;
begin
  ReferenceNeeded;
  Result := FRegionData.Reference;
end;

procedure TRegion.ReferenceNeeded;
var
  FillMode: integer;
begin
  if FRegionData.Reference.Allocated then Exit;

  if FRegionData.Polygon <> nil
  then begin
    if FRegionData.Winding
    then FillMode := Winding
    else FillMode := Alternate;
    
    FRegionData.Reference._lclHandle := TLCLHandle(CreatePolygonRgn(
      FRegionData.Polygon, FRegionData.NumPoints, FillMode));
    Exit;
  end;
    
  with FRegionData.Rect do
  begin
    if (Right - Left <> 0) or (Bottom - Top <> 0)
    then begin
      FRegionData.Reference._lclHandle := TLCLHandle(CreateRectRgn(Left, Top, Right, Bottom));
      Exit;
    end;
  end;
  
end;

{------------------------------------------------------------------------------
  Method:  TRegion.FreeReference
  Params:  none
  Returns: Nothing

  Frees a region handle if needed
 ------------------------------------------------------------------------------}

procedure TRegion.FreeReference;
begin
  if not FRegionData.Reference.Allocated then Exit;

  DeleteObject(HGDIOBJ(FRegionData.Reference.Handle));
  FRegionData.Reference._lclHandle := 0;
end;

{------------------------------------------------------------------------------
  Method: TRegion.SetClipRect
  Params: Value: the new value
  Returns:  nothing

  Sets/clips the region to a rectangle value
 ------------------------------------------------------------------------------}
procedure TRegion.SetClipRect(Value : TRect);
begin
  if FRegionData.Rect = Value then Exit;

  FreeReference;
  FRegionData.Rect := Value;
  if FRegionData.Polygon <> nil then
    ReallocMem(FRegionData.Polygon, 0);

  FRegionData.Polygon := nil;
  FRegionData.NumPoints := 0;
  FRegionData.Winding := False;
    
  Changed;
end;

{------------------------------------------------------------------------------
  Method: TRegion.GetClipRect
  Params: none
  Returns:  current rect

  Gets the regions current clip rect
 ------------------------------------------------------------------------------}
function TRegion.GetClipRect : TRect;
begin
  if FRegionData.Reference.Allocated
  then begin
    GetRgnBox(HRGN(FRegionData.Reference.Handle), @Result);
  end
  else begin
    if FRegionData.Polygon <> nil
    then begin
      ReferenceNeeded;
      if FRegionData.Reference.Allocated
      then Result := GetClipRect
      else Result := FRegionData.Rect;
      //FreeReference; // ??? why free (mwe)
    end
    else
      Result := FRegionData.Rect;
  end;
end;

// included by graphics.pp

