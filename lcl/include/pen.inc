{%MainUnit ../graphics.pp}
{******************************************************************************
                                     TPen
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
    SetColor(Value,TColorToFPColor(Value));
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
  if Style <> Value
  then begin
    FreeHandle;
    inherited SetStyle(Value);
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
  if Mode <> Value
  then begin
    FreeHandle;
    inherited SetMode(Value);
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
    inherited SetWidth(Value);
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
  FHandle := 0;
  DelayAllocate:=true;
  inherited SetWidth(1);
  inherited SetStyle(psSolid);
  inherited SetMode(pmCopy);
  inherited SetFPColor(colBlack);
  Color := clBlack;
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
    SetColor(TPen(Source).Color,TFPCanvasHelper(Source).FPColor);
    Style := TPen(Source).Style;
    Mode := TPEn(Source).Mode;
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
    ps_Solid, ps_Dash, ps_Dot, ps_DashDot, ps_DashDotDot, ps_insideFrame,
    ps_Solid,{ ToDo ps_Pattern,}
    ps_NULL
    );
var
  LogPen: TLogPen;
  CachedPen: TBlockResourceCacheDescriptor;
begin
  if FHandle = 0
  then begin
    FillChar(LogPen,SizeOf(LogPen),0);
    with LogPen do
    begin
      lopnStyle := PEN_STYLES[Style];
      lopnWidth.X := Width;
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

procedure TPen.SetColor(const NewColor: TColor; const NewFPColor: TFPColor);
begin
  if (NewColor=Color) and (NewFPColor=FPColor) then exit;
  FreeHandle;
  FColor:=NewColor;
  inherited SetFPColor(NewFPColor);
  Changed;
end;

procedure TPen.SetFPColor(const AValue: TFPColor);
begin
  if FPColor=AValue then exit;
  SetColor(FPColorToTColor(AValue),AValue);
end;

