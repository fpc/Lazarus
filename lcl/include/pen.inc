{%MainUnit ../graphics.pp}
{******************************************************************************
                                     TPen
 ******************************************************************************

 *****************************************************************************
 *                                                                           *
 *  This file is part of the Lazarus Component Library (LCL)                 *
 *                                                                           *
 *  See the file COPYING.modifiedLGPL.txt, included in this distribution,        *
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
  if Item = nil then
    RaiseGDBException('TPenHandleCache.RemoveItem');
  DeleteObject(HGDIOBJ(Item.Handle));
  inherited RemoveItem(Item);
end;

constructor TPenHandleCache.Create;
begin
  inherited Create(SizeOf(TExtLogPen));
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
  FEndCap := pecRound;
  FJoinStyle := pjsRound;
  FGeometric := False;
  FPattern := nil;
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
var
  APen: TPen absolute Source;
begin
  if Source is TPen then
  begin
    Width := APen.Width;
    SetColor(APen.Color, TFPCanvasHelper(Source).FPColor);
    Style := APen.Style;
    Mode := APen.Mode;
    Geometric := APen.Geometric;
    JoinStyle := APen.JoinStyle;
    EndCap := APen.EndCap;
  end
  else
    inherited Assign(Source);
end;

function TPen.GetPattern: TPenPattern;
begin
  Result := FPattern;
end;

procedure TPen.SetPattern(APattern: TPenPattern);

  function PatternsDiffer: Boolean;
  var
    l1, l2, m: integer;
  begin
    l1 := Length(FPattern);
    l2 := Length(APattern);
    m := min(l1, l2);
    Result := (l1 <> l2) or
              ((m > 0) and not CompareMem(@APattern[0], @FPattern[0], m * SizeOf(LongWord)));
  end;

begin
  if PatternsDiffer then
  begin
    FreeReference;
    FPattern := APattern;
    Changed;
  end;
end;

{------------------------------------------------------------------------------
  Method: TPen.SetHandle
  Params:   a pen handle
  Returns:  nothing

  sets the pen to an external created pen
 ------------------------------------------------------------------------------}
procedure TPen.SetHandle(const Value: HPEN);
begin
  if HPEN(FReference.Handle) = Value then Exit;

  FreeReference;
  FReference._lclHandle := TLCLHandle(Value);
  Changed;
end;

procedure TPen.SetJoinStyle(const AValue: TPenJoinStyle);
begin
  if JoinStyle <> AValue then
  begin
    FreeReference;
    FJoinStyle := AValue;
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
begin
  Result := HPEN(Reference.Handle);
end;

function TPen.GetReference: TWSPenReference;
begin
  ReferenceNeeded;
  Result := FReference;
end;

procedure TPen.ReferenceNeeded;
const
  PEN_STYLES: array[TPenStyle] of DWord = (
 { psSolid       } PS_SOLID,
 { psDash        } PS_DASH,
 { psDot         } PS_DOT,
 { psDashDot     } PS_DASHDOT,
 { psDashDotDot  } PS_DASHDOTDOT,
 { psinsideFrame } PS_INSIDEFRAME,
 { psPattern     } PS_USERSTYLE,
 { psClear       } PS_NULL
  );

  PEN_GEOMETRIC: array[Boolean] of DWord = (
  { false }  PS_COSMETIC,
  { true  }  PS_GEOMETRIC
  );

  PEN_ENDCAP: array[TPenEndCap] of DWord = (
  { pecRound  } PS_ENDCAP_ROUND,
  { pecSquare } PS_ENDCAP_SQUARE,
  { pecFlat   } PS_ENDCAP_FLAT
  );

  PEN_JOIN: array[TPenJoinStyle] of DWord = (
  { pjsRound } PS_JOIN_ROUND,
  { pjsBevel } PS_JOIN_BEVEL,
  { pjsMiter } PS_JOIN_MITER
  );
var
  ALogPen: TLogPen;
  AExtPen: TExtLogPen;
  ALogBrush: TLogBrush;
  CachedPen: TBlockResourceCacheDescriptor;
begin
  if FReference.Allocated then Exit;

  FillChar(AExtPen, SizeOf(AExtPen), 0);
  with AExtPen do
  begin
    elpPenStyle := PEN_STYLES[Style] or PEN_GEOMETRIC[Geometric];
    if Geometric then
      elpPenStyle := elpPenStyle or PEN_ENDCAP[EndCap] or PEN_JOIN[JoinStyle];
    elpWidth := Width;
    elpBrushStyle := BS_SOLID;
    elpColor := FColor;
  end;

  CachedPen := PenResourceCache.FindDescriptor(@AExtPen);
  if CachedPen <> nil then
  begin
    CachedPen.Item.IncreaseRefCount;
    FReference._lclHandle := CachedPen.Item.Handle;
  end else
  begin
    // choose which function to use: CreatePenIndirect or ExtCreatePen
    if ((AExtPen.elpPenStyle and PS_STYLE_MASK) = AExtPen.elpPenStyle) and (AExtPen.elpPenStyle <> PS_USERSTYLE) then
    begin
      // simple pen
      ALogPen.lopnStyle := AExtPen.elpPenStyle;
      ALogPen.lopnWidth := Point(AExtPen.elpWidth, 0);
      ALogPen.lopnColor := AExtPen.elpColor;
      FReference._lclHandle := TLCLHandle(CreatePenIndirect(ALogPen));
    end
    else
    begin
      // extended pen
      ALogBrush.lbStyle := AExtPen.elpBrushStyle;
      ALogBrush.lbColor := AExtPen.elpColor;
      ALogBrush.lbHatch := AExtPen.elpHatch;
      if (Style = psPattern) and (Length(FPattern) > 0) then
        FReference._lclHandle := TLCLHandle(ExtCreatePen(AExtPen.elpPenStyle, AExtPen.elpWidth, ALogBrush, Length(FPattern), @FPattern[0]))
      else
        FReference._lclHandle := TLCLHandle(ExtCreatePen(AExtPen.elpPenStyle, AExtPen.elpWidth, ALogBrush, 0, nil));
    end;
    PenResourceCache.AddResource(FReference.Handle, @AExtPen);
  end;
  FPenHandleCached := True;
end;

procedure TPen.SetEndCap(const AValue: TPenEndCap);
begin
  if EndCap <> AValue then
  begin
    FreeReference;
    FEndCap := AValue;
    Changed;
  end;
end;

procedure TPen.SetGeometric(const AValue: Boolean);
begin
  if Geometric <> AValue then
  begin
    FreeReference;
    FGeometric := AValue;
    Changed;
  end;
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
    DeleteObject(HGDIOBJ(FReference.Handle));
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
var
  APen: TPen absolute From;
begin
  if From is TPen then
  begin
    FreeReference;
    inherited DoCopyProps(From);
    FGeometric := APen.Geometric;
    FEndCap := APen.EndCap;
    FJoinStyle := APen.JoinStyle;
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

