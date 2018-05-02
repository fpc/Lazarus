{%MainUnit ../buttons.pp}

{
 *****************************************************************************
  This file is part of the Lazarus Component Library (LCL)

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************
}

type
  { TGlyphBitmap }

  TGlyphBitmap = class(TBitmap)
  private
    FOwner: TButtonGlyph;
  protected
    procedure SetMasked(AValue: Boolean); override;
    procedure SetTransparent(AValue: Boolean); override;
  public
    procedure Assign(ASource: TPersistent); override;
    constructor Create(AOwner: TButtonGlyph); reintroduce;
  end;

procedure TGlyphBitmap.Assign(ASource: TPersistent);
begin
  inherited Assign(ASource);
  if FOwner = nil then Exit;
  if FOwner.FTransparentMode = gtmGlyph then Exit;
  inherited SetTransparent(FOwner.FTransparentMode = gtmTransparent);
end;

constructor TGlyphBitmap.Create(AOwner: TButtonGlyph);
begin
  FOwner := AOwner;
  inherited Create;
  inherited SetTransparent(True);
end;

procedure TGlyphBitmap.SetMasked(AValue: Boolean);
begin
  if (FOwner = nil)
  or (FOwner.FTransparentMode = gtmGlyph)
  then inherited SetMasked(AValue)
  else inherited SetMasked(FOwner.FTransparentMode = gtmTransparent);
end;

procedure TGlyphBitmap.SetTransparent(AValue: Boolean);
begin
  if (FOwner = nil)
  or (FOwner.FTransparentMode = gtmGlyph)
  then inherited SetTransparent(AValue)
  else inherited SetTransparent(FOwner.FTransparentMode = gtmTransparent);
end;
  


{------------------------------------------------------------------------------}
{       TButtonGlyph Constructor                                               }
{------------------------------------------------------------------------------}
constructor TButtonGlyph.Create;
begin
  FImagesCache := nil;
  FIsDesigning := False;
  FShowMode:= gsmApplication;
  FOriginal := TGlyphBitmap.Create(Self);
  FOriginal.OnChange := @GlyphChanged;
  FNumGlyphs := Low(TNumGlyphs);
  FExternalImageIndex := -1;
end;

{------------------------------------------------------------------------------
       TButtonGlyph destructor
------------------------------------------------------------------------------}
destructor TButtonGlyph.Destroy;
begin
  if FImagesCache <> nil then
  begin
    FImagesCache.UnregisterListener(Self);
    FImagesCache := nil; // cache can free on unregister
  end;
  FOriginal.Free;
  FOriginal := nil;
  inherited Destroy;
end;

procedure TButtonGlyph.DoChange;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TButtonGlyph.GetImageIndexAndEffect(State: TButtonState;
  APPI: Integer; const ACanvasScaleFactor: Double; out
  AImageResolution: TScaledImageListResolution; out AIndex: Integer; out
  AEffect: TGraphicsDrawEffect);
var
  AStoredState: TButtonState;
  AUseAutoEffects: Integer;
  AImages: TCustomImageList;
const
  CEffects: array[Boolean, TButtonState] of TGraphicsDrawEffect =
    ((gdeNormal, gdeDisabled, gdeNormal, gdeNormal, gdeNormal),
     (gdeNormal, gdeDisabled, gdeShadowed, gdeNormal, gdeHighlighted));
begin
  AUseAutoEffects := ThemeServices.GetOption(toUseGlyphEffects);
  if Assigned(FExternalImages) and CanShow then
  begin
    AImages := FExternalImages;
    AIndex := FExternalImageIndex;
    AEffect := CEffects[AUseAutoEffects > 0, State];
    AImageResolution := AImages.ResolutionForPPI[FExternalImageWidth, APPI, ACanvasScaleFactor];
  end else
  if (FLCLGlyphResourceName<>'') and CanShow then
  begin
    AImages := LCLGlyphs;
    AIndex := LCLGlyphs.GetImageIndex(FLCLGlyphResourceName);
    AEffect := CEffects[AUseAutoEffects > 0, State];
    AImageResolution := AImages.ResolutionForPPI[0, APPI, ACanvasScaleFactor];
  end else
  begin
    AImages := FImages;
    if AImages<>nil then
      AImageResolution := AImages.ResolutionForPPI[0, 96, 1]
    else
      AImageResolution := TScaledImageListResolution.Create(nil, 1);
    AStoredState := bsUp;
    AEffect := gdeNormal;
    case State of
      bsDisabled:
        if NumGlyphs > 1 then
          AStoredState := State
        else
          AEffect := gdeDisabled;
      bsDown:
        if NumGlyphs > 2 then
          AStoredState := State
        else if AUseAutoEffects > 0 then
          AEffect := gdeShadowed;
      bsExclusive:
        if NumGlyphs > 3 then
          AStoredState := State;
      bsHot:
        if NumGlyphs > 4 then
          AStoredState := State
        else if AUseAutoEffects > 0 then
          AEffect := gdeHighlighted;
    end;
    AIndex := FImageIndexes[AStoredState];
  end;
end;

function TButtonGlyph.GetNumGlyphs: TNumGlyphs;
begin
  if Assigned(FExternalImages) then
    Result := 1
  else
    Result := FNumGlyphs;
end;

{------------------------------------------------------------------------------
       TButtonGlyph SetGlyph
------------------------------------------------------------------------------}
procedure TButtonGlyph.SetGlyph(Value : TBitmap);
var
  GlyphCount : integer;
begin
  if FOriginal = Value then
    exit;
  if FOriginal = nil then
    FOriginal := TGlyphBitmap.Create(Self)
  else
  begin
    FExternalImages := nil;
    FExternalImageIndex := -1;
  end;
  FOriginal.OnChange := nil;
  FOriginal.Assign(Value);
  FOriginal.OnChange := @GlyphChanged;
  FNumGlyphs := 1;
  if not FOriginal.Empty then
    ClearLCLGlyph;
  if FOriginal.Height > 0 then
  begin
    if FOriginal.Width mod FOriginal.Height = 0 then
    begin
      GlyphCount := FOriginal.Width div FOriginal.Height;
      if GlyphCount > High(TNumGlyphs) then
        GlyphCount := Low(TNumGlyphs);
      FNumGlyphs := TNumGlyphs(GlyphCount);
    end;
  end;
  Refresh;
end;

procedure TButtonGlyph.SetLCLGlyphName(const ALCLGlyphName: string);
begin
  if FLCLGlyphResourceName = ALCLGlyphName then Exit;

  FLCLGlyphResourceName := ALCLGlyphName;
  if FLCLGlyphResourceName<>'' then
  begin
    ClearImages;
    FExternalImages := nil;
    FExternalImageIndex := -1;
    FExternalImageWidth := 0;
  end;
  DoChange;
end;

procedure TButtonGlyph.SetShowMode(const AValue: TGlyphShowMode);
begin
  if FShowMode = AValue then Exit;
  FShowMode := AValue;
  if not IsDesigning then
    Refresh;
end;

function TButtonGlyph.GetHeight: Integer;
begin
  if FImages <> nil then
    Result := FImages.Height
  else
    Result := 0;
end;

function TButtonGlyph.GetWidth: Integer;
begin
  if FImages <> nil then
    Result := FImages.Width
  else
    Result := 0;
end;

function SystemShowButtonGlyphs: Boolean; inline;
begin
  Result := ThemeServices.GetOption(toShowButtonImages) = 1;
  {$ifdef Windows}
  // force False on windows since gtk and qt can return True
    Result := False;
  {$endif}
end;

procedure TButtonGlyph.GlyphChanged(Sender: TObject);
begin
  if FImagesCache <> nil then
  begin
    FImagesCache.UnregisterListener(Self);
    FImagesCache := nil; // cache can free on unregister
    ClearImages;
  end;

  if CanShow and (FOriginal.Width > 0) and (FOriginal.Height > 0) then
  begin
    FImagesCache := GetImageListCache;
    FImagesCache.RegisterListener(Self);
    FImagesCache.RegisterBitmap(Self, FOriginal, NumGlyphs);
    FExternalImageIndex := -1;
    FExternalImages := nil;
    ClearLCLGlyph;
  end;

  if Sender = FOriginal then
    DoChange;
end;

{------------------------------------------------------------------------------
       TButtonGlyph Draw
------------------------------------------------------------------------------}
function TButtonGlyph.Draw(Canvas: TCanvas; const Client: TRect;
  const Offset: TPoint; State: TButtonState; Transparent: Boolean; BiDiFlags,
  PPI: Longint; const ScaleFactor: Double): TRect;
var
  ImgRes: TScaledImageListResolution;
  ImgID: integer;
  AEffect: TGraphicsDrawEffect;
begin
  Result := Client;
  if (FOriginal = nil) then
    exit;

  if (Client.Left >= Client.Right) or (Client.Top >= Client.Bottom) then
    Exit;
  
  GetImageIndexAndEffect(State, PPI, ScaleFactor, ImgRes, ImgID, AEffect);
  if (ImgRes.Resolution=nil) or (ImgID<0) then
    Exit;

  ImgRes.Draw(Canvas,
    Client.Left + Offset.X, Client.Top + Offset.y, ImgID, AEffect);

  // ToDo: VCL returns the text rectangle
end;

function TButtonGlyph.Draw(Canvas: TCanvas; const Client: TRect;
  const Offset: TPoint; State: TButtonState; Transparent: Boolean;
  BiDiFlags: Longint): TRect;
begin
  Result := Draw(Canvas, Client, Offset, State, Transparent, BiDiFlags, 96, 1);
end;

procedure TButtonGlyph.Refresh;
begin
  GlyphChanged(FOriginal);
end;

procedure TButtonGlyph.SetExternalImageIndex(const AExternalImageIndex: Integer);
begin
  if FExternalImageIndex = AExternalImageIndex then Exit;
  FExternalImageIndex := AExternalImageIndex;
  if FExternalImageIndex>=0 then
    ClearLCLGlyph;
  DoChange;
end;

procedure TButtonGlyph.SetExternalImages(const AExternalImages: TCustomImageList
  );
begin
  if FExternalImages = AExternalImages then Exit;
  FExternalImages := AExternalImages;
  if FExternalImages<>nil then
    ClearLCLGlyph;
  DoChange;
end;

procedure TButtonGlyph.SetExternalImageWidth(const AExternalImageWidth: Integer
  );
begin
  if FExternalImageWidth = AExternalImageWidth then Exit;
  FExternalImageWidth := AExternalImageWidth;
  DoChange;
end;


{------------------------------------------------------------------------------
       TButtonGlyph SetNumGlyphs
------------------------------------------------------------------------------}
procedure TButtonGlyph.SetNumGlyphs(Value : TNumGlyphs);
begin
  if Value <> FNumGlyphs then
  begin
    FNumGlyphs := Value;
    Refresh;
  end;
end;

procedure TButtonGlyph.SetTransparentMode(AValue: TGlyphTransparencyMode);
begin
  if AValue = FTransparentMode then Exit;
  FTransparentMode := AValue;
  if FTransparentMode = gtmGlyph then Exit;
  FOriginal.Transparent := FTransparentMode = gtmTransparent;
end;

procedure TButtonGlyph.ClearImages;
var
  i: TButtonState;
begin
  FImages := nil;
  for i := Low(TButtonState) to High(TButtonState) do
    FImageIndexes[i] := -1;
end;

procedure TButtonGlyph.ClearLCLGlyph;
begin
  FLCLGlyphResourceName := '';
end;

function TButtonGlyph.QueryInterface(constref iid: TGuid; out obj): longint; {$IFDEF WINDOWs}stdcall{$ELSE}cdecl{$ENDIF};
begin
  if GetInterface(iid, obj) then
    Result := 0
  else
    Result := E_NOINTERFACE;
end;

function TButtonGlyph._AddRef: longint; {$IFDEF WINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
begin
  Result := -1;
end;

function TButtonGlyph._Release: longint; {$IFDEF WINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
begin
  Result := -1;
end;

procedure TButtonGlyph.CacheSetImageList(AImageList: TCustomImageList);
begin
  FImages := AImageList;
end;

function TButtonGlyph.CanShow: Boolean;
begin
  Result := True;
  if IsDesigning then
    Exit;
  case ShowMode of
    gsmAlways:
      Result := True;
    gsmNever:
      Result := False;
    gsmApplication:
      begin
        case Application.ShowButtonGlyphs of
          sbgAlways: Result := True;
          sbgNever: Result := False;
          sbgSystem: Result := SystemShowButtonGlyphs;
        end;
      end;
    gsmSystem:
      Result := SystemShowButtonGlyphs;
  end;
end;

function TButtonGlyph.CanShowGlyph: Boolean;
begin
  Result :=
       ((FImages <> nil) and (FImageIndexes[Low(TButtonState)]>=0))
    or (FLCLGlyphResourceName <> '')
    or ((FExternalImages <> nil) and (FExternalImageIndex > -1));
end;

procedure TButtonGlyph.CacheSetImageIndex(AIndex, AImageIndex: Integer);
begin
  if (AIndex >= ord(Low(TButtonState))) and (AIndex <= Ord(High(TButtonState))) then
    FImageIndexes[TButtonState(AIndex)] := AImageIndex;
end;

// included by buttons.pp
