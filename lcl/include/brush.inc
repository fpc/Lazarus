{%MainUnit ../graphics.pp}
{******************************************************************************
                                     TBrush
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
  if FColor <> Value then SetColor(Value,TColorToFPColor(Value));
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
    inherited SetStyle(Value);
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
  DelayAllocate:=true;
  inherited SetStyle(bsSolid);
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
    SetColor(TBrush(Source).Color,TFPCanvasHelper(Source).FPColor);
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
    CachedBrush := BrushResourceCache.FindDescriptor(@LogBrush);
    if CachedBrush <> nil then
    begin
      CachedBrush.Item.IncreaseRefCount;
      FHandle := CachedBrush.Item.Handle;
    end else
    begin
      if LogBrush.lbStyle <> BS_PATTERN then
        FHandle := CreateBrushIndirect(LogBrush)
      else
        FHandle := CreatePatternBrush(LogBrush.lbHatch);
      BrushResourceCache.AddResource(FHandle,@LogBrush);
    end;
    FBrushHandleCached := True;
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
  FreeHandle;
  FColor:=NewColor;
  inherited SetFPColor(NewFPColor);
  Changed;
end;

procedure TBrush.SetFPColor(const AValue: TFPColor);
begin
  if FPColor=AValue then exit;
  SetColor(FPColorToTColor(AValue),AValue);
end;
