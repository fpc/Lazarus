{%MainUnit ../buttons.pp}

{
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
       TCustomBitBtn Constructor
------------------------------------------------------------------------------}
constructor TCustomBitBtn.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FCompStyle := csBitBtn;
  FKind := bkCustom;
  FLayout := blGlyphLeft;
  FSpacing := 3;
  fMargin:=-1;
  FButtonGlyph := TButtonGlyph.Create;
  FButtonGlyph.OnChange := @GlyphChanged;
  Align := alNone;
  BorderSpacing.InnerBorder:=4;
  SetInitialBounds(0,0,75,30);
  RealizeKind;
end;

{------------------------------------------------------------------------------
       TCustomBitBtn destructor
------------------------------------------------------------------------------}
destructor TCustomBitBtn.Destroy;
Begin
  FreeThenNil(FButtonGlyph);
  inherited Destroy;
end;

Procedure TCustomBitBtn.Click;
var
  Form : TCustomForm;
Begin
  if FKind = bkClose then Begin
    Form := GetParentForm(Self);
    if Form <> nil then begin
      Form.Close;
      exit;
    end;
  end;
  inherited Click;
End;

Function TCustomBitBtn.GetGlyph : TBitmap;
Begin
  Result := FButtonGlyph.Glyph;
end;

function TCustomBitBtn.GetNumGlyphs: Integer;
begin
  Result := FButtonGlyph.FNumGlyphs;
end;

Function TCustomBitBtn.IsGlyphStored: Boolean;
begin
  Result := (Kind = bkCustom) and (FButtonGlyph.Glyph <> nil)
            and (not FButtonGlyph.Glyph.Empty)
            and (FButtonGlyph.Glyph.Width>0) and (FButtonGlyph.Glyph.Height>0);
end;

Procedure TCustomBitBtn.SetGlyph(AValue: TBitmap);
Begin
  FButtonGlyph.Glyph := AValue;
  InvalidatePreferredSize;
  AdjustSize;
end;

procedure TCustomBitBtn.GlyphChanged(Sender: TObject);
begin
  if HandleAllocated 
  then TWSBitBtnClass(WidgetSetClass).SetGlyph(Self, Glyph);
  InvalidatePreferredSize;
  AdjustSize;
end;

procedure TCustomBitBtn.ActionChange(Sender: TObject; CheckDefaults: Boolean);

  procedure CopyImage(ImageList: TCustomImageList; Index: Integer);
  var
    CurGlyph: TBitmap;
  begin
    CurGlyph:=Glyph;
    with CurGlyph do begin
      // ToDo: transparency
      Width := ImageList.Width;
      Height := ImageList.Height;
      Canvas.Brush.Color := clMaroon; // whatever
      Canvas.FillRect(Rect(0,0,Width, Height));
      ImageList.Draw(Canvas,0,0,Index,true);
    end;
  end;

begin
  inherited ActionChange(Sender,CheckDefaults);
  if Sender is TCustomAction then begin
    with TCustomAction(Sender) do begin
      if (Glyph.Empty) and (ActionList <> nil) and (ActionList.Images <> nil)
      and (ImageIndex >= 0) and (ImageIndex < ActionList.Images.Count) then
        CopyImage(ActionList.Images,ImageIndex);
    end;
  end;
end;

procedure TCustomBitBtn.SetKind(AValue: TBitBtnKind);
Begin
  if FKind = AValue then Exit;
  FKind := AValue;
  if FKind = bkCustom then Exit;
  
  RealizeKind;
end;

procedure TCustomBitBtn.SetLayout(AValue: TButtonLayout);
Begin
  if FLayout = AValue then Exit;
  FLayout := AValue;
  if HandleAllocated 
  then TWSBitBtnClass(WidgetSetClass).SetLayout(Self, FLayout);
  AdjustSize;
end;

procedure TCustomBitBtn.SetMargin(const AValue: integer);
begin
  if FMargin = AValue then Exit;
  FMargin := AValue;
  if HandleAllocated 
  then TWSBitBtnClass(WidgetSetClass).SetMargin(Self, FMargin);
  AdjustSize;
end;

procedure TCustomBitBtn.SetNumGlyphs(AValue: Integer);
begin
  if AValue < 0 then AValue := 1;
  if AValue > 4 then AValue := 4;

  if AValue <> FButtonGlyph.NumGlyphs then
  Begin
    FButtonGlyph.NumGlyphs :=  TNumGlyphs(AValue);
    Invalidate;
  end;
end;

Procedure TCustomBitBtn.SetSpacing(AValue: Integer);
Begin
  if (FSpacing = AValue) or (AValue < -1) then Exit;
  FSpacing := AValue;
  if HandleAllocated 
  then TWSBitBtnClass(WidgetSetClass).SetSpacing(Self, FSpacing);
  AdjustSize;
end;

procedure TCustomBitBtn.RealizeKind;
var
  GlyphValid: Boolean;
  CustomGlyph: TBitmap;
begin
  if (Kind<>bkCustom) 
  then begin
    GlyphValid:=false;
    
    // first let the user override
    if GetDefaultBitBtnGlyph<>nil then begin
      CustomGlyph:=GetDefaultBitBtnGlyph(Kind);
      if CustomGlyph<>nil then begin
        Glyph.Assign(CustomGlyph);
        CustomGlyph.Free;
        GlyphValid:=true;
      end;
    end;

    // then ask the widgetset
    if not GlyphValid then begin
      Glyph.Handle := LoadStockPixmap(BitBtnImages[FKind]);
      GlyphValid:=true;
    end;
  end;

  if not (csLoading in ComponentState) 
  then begin
    Caption := GetCaptionOfKind(fKind);
    ModalResult := BitBtnModalResults[FKind];
    Default := FKind in [bkOk,bkYes];
    Cancel := FKind in [bkCancel,bkNo];
  end;
end;

{ Return the caption associated with the akind value.
  This function replaces BitBtnCaption const because the localizing
  dont work with an const array }
function TCustomBitBtn.GetCaptionOfKind(aKind: TBitBtnKind): String;
begin
  Result:='';
  case aKind of
    bkOK       : Result:=rsmbOK;
    bkCancel   : Result:=rsmbCancel;
    bkHelp     : Result:=rsmbHelp;
    bkYes      : Result:=rsmbYes;
    bkNo       : Result:=rsmbNo;
    bkClose    : Result:=rsmbClose;
    bkAbort    : Result:=rsmbAbort;
    bkRetry    : Result:=rsmbRetry;
    bkIgnore   : Result:=rsmbIgnore;
    bkAll      : Result:=rsmbAll;
    bkNoToAll  : Result:=rsmbNoToAll;
    bkYesToAll : Result:=rsmbYesToAll;
  end;
end;

procedure TCustomBitBtn.InitializeWnd;
begin
  inherited;
  TWSBitBtnClass(WidgetSetClass).SetGlyph(Self, Glyph);
  TWSBitBtnClass(WidgetSetClass).SetLayout(Self, FLayout);  
  TWSBitBtnClass(WidgetSetClass).SetMargin(Self, FMargin);  
  TWSBitBtnClass(WidgetSetClass).SetSpacing(Self, FSpacing);  
end;

procedure TCustomBitBtn.TextChanged;
begin
  inherited TextChanged;
  AdjustSize;
end;

function TCustomBitBtn.IsBorderSpacingInnerBorderStored: Boolean;
begin
  Result:=BorderSpacing.InnerBorder<>4;
end;

// included by buttons.pp
