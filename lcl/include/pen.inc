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
procedure TPen.SetColor(Value : TColor);
begin
  if FColor <> Value then
    SetColor(Value, TColorToFPColor(Value));
end;

{------------------------------------------------------------------------------
  Method: TPen.SetStyle
  Params: Value: the new value
  Returns:  nothing

  Sets the style of a pen
 ------------------------------------------------------------------------------}
procedure TPen.SetStyle(Value : TPenStyle);
begin
  if Style <> Value then
  begin
    FreeReference;
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
procedure TPen.SetMode(Value : TPenMode);
begin
  if Mode <> Value then
  begin
    FreeReference;
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
procedure TPen.SetWidth(Value : Integer);
begin
  if Width <> Value then
  begin
    FreeReference;
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
  DelayAllocate := True;
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
  FreeReference;
  inherited Destroy;
end;

{------------------------------------------------------------------------------
  Method: TPen.Assign
  Params: Source: Another pen
  Returns:  nothing

  Copies the source pen to itself
 ------------------------------------------------------------------------------}
procedure TPen.Assign(Source : Tpersistent);
begin
  if Source is TPen then
  begin
    Width := TPen(Source).Width;
    SetColor(TPen(Source).Color, TFPCanvasHelper(Source).FPColor);
    Style := TPen(Source).Style;
    Mode := TPen(Source).Mode;
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
  if FReference.Handle = Value then Exit;

  FreeReference;
  FReference._lclHandle := TLCLHandle(Value);
  Changed;
end;

{------------------------------------------------------------------------------
  Function: TPen.GetHandle
  Params:   none
  Returns:  a handle to a pen gdiobject

  Creates a pen if needed
 ------------------------------------------------------------------------------}
function TPen.GetHandle: HPEN;
begin
  Result := Reference.Handle;
end;

function TPen.GetReference: TWSPenReference;
begin
  ReferenceNeeded;
  Result := FReference;
end;

procedure TPen.ReferenceNeeded;
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
  if FReference.Allocated then Exit;

  FillChar(LogPen, SizeOf(LogPen), 0);
  with LogPen do
  begin
    lopnStyle := PEN_STYLES[Style];
    lopnWidth.X := Width;
    lopnColor := FColor;
  end;

  CachedPen := PenResourceCache.FindDescriptor(@LogPen);
  if CachedPen <> nil then
  begin
    CachedPen.Item.IncreaseRefCount;
    FReference._lclHandle := CachedPen.Item.Handle;
  end else
  begin
    FReference._lclHandle := TLCLHandle(CreatePenIndirect(LogPen));
    PenResourceCache.AddResource(FReference.Handle, @LogPen);
  end;
  FPenHandleCached := True;
end;

{------------------------------------------------------------------------------
  Method:  TPen.FreeReference
  Params:  none
  Returns: Nothing

  Frees a pen handle if needed
 ------------------------------------------------------------------------------}

procedure TPen.FreeReference;
begin
  if not FReference.Allocated then Exit;

  Changing;
  if FPenHandleCached then
  begin
    PenResourceCache.FindItem(FReference.Handle).DecreaseRefCount;
    FPenHandleCached := False;
  end else
    DeleteObject(FReference.Handle);
  FReference._lclHandle := 0;
end;

procedure TPen.DoAllocateResources;
begin
  inherited DoAllocateResources;
  GetReference;
end;

procedure TPen.DoDeAllocateResources;
begin
  FreeReference;
  inherited DoDeAllocateResources;
end;

procedure TPen.DoCopyProps(From: TFPCanvasHelper);
begin
  if From is TPen then
  begin
    FreeReference;
    inherited DoCopyProps(From);
    //TODO: query new parameters
    Changed;
  end else
    inherited DoCopyProps(From);
end;

procedure TPen.SetColor(const NewColor: TColor; const NewFPColor: TFPColor);
begin
  if (NewColor = Color) and (NewFPColor = FPColor) then Exit;
  FreeReference;
  FColor := NewColor;
  inherited SetFPColor(NewFPColor);
  Changed;
end;

procedure TPen.SetFPColor(const AValue: TFPColor);
begin
  if FPColor <> AValue then
    SetColor(FPColorToTColor(AValue), AValue);
end;

