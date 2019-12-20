{%MainUnit gtkint.pp}
{ $Id$ }
{******************************************************************************
                 All GTK interface communication implementations.
                   Initial Revision  : Sun Nov 23 23:53:53 2003


  !! Keep alphabetical !!

  Support routines go to gtkproc.pp

 ******************************************************************************
 Implementation
 ******************************************************************************

 *****************************************************************************
  This file is part of the Lazarus Component Library (LCL)

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************
}


//##apiwiz##sps##   // Do not remove

procedure TGTKWidgetSet.DrawDefaultDockImage(AOldRect, ANewRect: TRect;
  AOperation: TDockImageOperation);
const
  LineWidth = 2;
var
  Mask: PGdkBitmap;
  gc: PGdkGC;
  dx, dy: integer;
  AColor: TGdkColor;
{$ifdef GTK_2_10}
  Colormap: PGdkColormap;
  Screen: PGdkScreen;
{$endif}
begin
  dx := ANewRect.Right - ANewRect.Left;
  dy := ANewRect.Bottom - ANewRect.Top;
  if dx < 0 then
    dx := 0;
  if dy < 0 then
    dy := 0;
  if FDockImage = nil then
  begin
    // dock image is just a window without title
    FDockImage := gtk_window_new(GTK_WINDOW_POPUP);
    gtk_window_set_default_size(PGtkWindow(FDockImage),
      dx, dy);
    gtk_widget_realize(FDockImage);
    gdk_window_set_decorations(FDockImage^.window, 0);
    gdk_window_set_functions(FDockImage^.window, GDK_FUNC_RESIZE or GDK_FUNC_CLOSE);
    SetWidgetColor(FDockImage, clNone,
      {$ifdef gtk1}clBlue{$else}clGradientActiveCaption{$endif}, [GTK_STATE_NORMAL]);
    {$ifdef GTK_2_10}
    // attemp to make window semi-transparent
    Screen := gtk_widget_get_screen(FDockImage);
    Colormap := gdk_screen_get_rgba_colormap(Screen);
    if (Colormap <> nil) and gdk_screen_is_composited(Screen) then
      gtk_widget_set_colormap(FDockImage, Colormap);
    {$endif}
  end;

  gdk_window_move_resize(FDockImage^.window, ANewRect.Left, ANewRect.Top,
    dx, dy);
  if (dx > 0) and (dy > 0) then
  begin
    // create a hole inside window
    Mask := gdk_pixmap_new(nil, dx, dy, 1);
    gc := gdk_gc_new(Mask);
    AColor.pixel := 1;
    gdk_gc_set_foreground(gc, @AColor);
    gdk_draw_rectangle(Mask, gc, 1, 0, 0, dx, dy);
    AColor.pixel := 0;
    gdk_gc_set_foreground(gc, @AColor);
    gdk_draw_rectangle(Mask, gc, 1, LineWidth, LineWidth,
      dx - LineWidth * 2, dy - LineWidth * 2);
    gdk_gc_unref(gc);
    gtk_widget_shape_combine_mask(FDockImage, Mask, 0, 0);
    gdk_pixmap_unref(Mask);
  end;
  case AOperation of
    disShow: gtk_widget_show(FDockImage);
    disHide: gtk_widget_hide(FDockImage);
  end;
end;

procedure TGTKWidgetSet.DrawGrid(DC: HDC; const R: TRect; DX, DY: Integer);
var
  X, Y: Integer;
  W, H: Integer;
  SavedDC: Integer;
  Context: TGtkDeviceContext absolute DC;
  DCOrigin: TPoint;
begin
  if (Context = nil) or (Context.Drawable = nil) then Exit;

  DCOrigin := Context.Offset;
  SavedDC := SaveDC(DC);
  try
    Context.SelectPenProps;
    if not (dcfPenSelected in Context.Flags) then Exit;

    if Context.IsNullPen then Exit;
  
    W := (R.Right - R.Left - 1) div DX;
    H := (R.Bottom - R.Top - 1) div DY;

    for Y := 0 to H do
    begin
      for X := 0 to W do
      begin
        gdk_draw_point(Context.Drawable, Context.GC,
          DCOrigin.X + R.Left + X * DX, DCOrigin.Y + R.Top + Y * DY);
      end;
    end;
  finally
    RestoreDC(DC, SavedDC);
  end;
end;

{------------------------------------------------------------------------------
  function TGtkWidgetSet.ExtUTF8Out(DC: HDC; X, Y: Integer; Options: Longint;
    Rect: PRect; Str: PChar; Count: Longint; Dx: PInteger): Boolean;

  As ExtTextOut except that Str is treated as UTF8
 ------------------------------------------------------------------------------}
function TGtkWidgetSet.ExtUTF8Out(DC: HDC; X, Y: Integer; Options: Longint;
  Rect: PRect; Str: PChar; Count: Longint; Dx: PInteger): Boolean;
var
  IsDBCSFont: Boolean;
  NewCount: Integer;
begin
  UpdateDCTextMetric(TGtkDeviceContext(DC));
  IsDBCSFont:=TGtkDeviceContext(DC).DCTextMetric.IsDoubleByteChar;
  if IsDBCSFont then begin
    NewCount:=Count*2;
    if FExtUTF8OutCacheSize<NewCount then begin
      ReAllocMem(FExtUTF8OutCache,NewCount);
      FExtUTF8OutCacheSize:=NewCount;
    end;
    NewCount:=UTF8ToDoubleByte(Str,Count,FExtUTF8OutCache)*2;
    //debugln('TGtkWidgetSet.ExtUTF8Out Count=',dbgs(Count),' NewCount=',dbgs(NewCount));
    Result:=ExtTextOut(DC,X,Y,Options,Rect,FExtUTF8OutCache,NewCount,Dx);
  end else begin
    Result:=ExtTextOut(DC,X,Y,Options,Rect,Str,Count,Dx);
  end;
end;

function TGtkWidgetSet.TextUTF8Out(DC: HDC; X, Y: Integer; Str: PChar; Count: Longint): Boolean;
var
  IsDBCSFont: Boolean;
  NewCount: Integer;
begin
  UpdateDCTextMetric(TGtkDeviceContext(DC));
  IsDBCSFont:=TGtkDeviceContext(DC).DCTextMetric.IsDoubleByteChar;
  if IsDBCSFont then begin
    NewCount:=Count*2;
    if FExtUTF8OutCacheSize<NewCount then begin
      ReAllocMem(FExtUTF8OutCache,NewCount);
      FExtUTF8OutCacheSize:=NewCount;
    end;
    NewCount:=UTF8ToDoubleByte(Str,Count,FExtUTF8OutCache)*2;
    Result:=TextOut(DC,X,Y,FExtUTF8OutCache,NewCount);
  end else begin
    Result:=TextOut(DC,X,Y,Str,Count);
  end;
end;

{------------------------------------------------------------------------------
  function TGTKWidgetSet.FontIsMonoSpace(Font: HFont): boolean;

  True if font characters have all the same width.
 ------------------------------------------------------------------------------}
function TGTKWidgetSet.FontIsMonoSpace(Font: HFont): boolean;
begin
  Result:=IsValidGDIObject(Font)
          and FontIsMonoSpaceFont(PGdiObject(Font)^.GDIFontObject);
end;

{------------------------------------------------------------------------------
  Function: GetAcceleratorString
  Params: AVKey:
          AShiftState:
  Returns:

 ------------------------------------------------------------------------------}
function TGtkWidgetSet.GetAcceleratorString(const AVKey: Byte;
  const AShiftState: TShiftState): String;
begin
  Result:=inherited GetAcceleratorString(AVKey,AShiftState);
end;

{------------------------------------------------------------------------------
  Function: RawImage_CreateBitmap
  Params: ARawImage:
          ABitmap:
          AMask:
          ASkipMask: When set, no mask is created
  Returns:

 ------------------------------------------------------------------------------}
function TGtkWidgetSet.RawImage_CreateBitmaps(const ARawImage: TRawImage;
  out ABitmap, AMask: HBitmap; ASkipMask: Boolean): boolean;
type
  TFourBytes = packed record
    B0,B1,B2,B3: Byte;
  end;

var
  GdiObject: PGDIObject absolute ABitmap;
  GdiMaskObject: PGDIObject absolute AMask;
  Desc: TRawImageDescription absolute ARawImage.Description;
  ImgData: Pointer absolute ARawImage.Data;
  ImgMask: Pointer absolute ARawImage.Mask;
  ImgWidth: Cardinal absolute ARawImage.Description.Width;
  ImgHeight: Cardinal absolute ARawImage.Description.Height;
  ImgDepth: Byte absolute ARawImage.Description.Depth;
  ImgDataSize: PtrUInt absolute ARawImage.DataSize;
  Drawable: PGdkDrawable;
  Pixbuf, TmpPixBuf: PGdkPixbuf;
  GC: PGdkGC;
  Visual: PGdkVisual;
  GdkImage: PGdkImage;
  RowStride: Cardinal;
  Ridx, Gidx, Bidx, Aidx: Byte;
  Data: Pointer;
  Src, Dst, SrcRowPtr, DstRowPtr: PByte;
  x, y: Cardinal;
  CreateWithAlpha: boolean;
begin
  Result := False;
  ABitmap := 0;
  AMask := 0;
  CreateWithAlpha := True;

  if ImgWidth = 0 then Exit;
  if ImgHeight = 0 then Exit;

  try
    {$IFDEF VerboseRawImage}
    DebugLn('TGtkWidgetSet.CreateBitmapFromRawImage A ',
      ' ASkipMask='+dbgs(ASkipMask),
      ' Depth='+dbgs(Desc.Depth),
      ' Width='+dbgs(Desc.Width),
      ' Height='+dbgs(Desc.Height),
      ' Data='+DbgS(ARawImage.Data),
      ' DataSize='+dbgs(ARawImage.DataSize)+
      ' Mask='+DbgS(ARawImage.Mask)+
      ' MaskSize='+dbgs(ARawImage.MaskSize)+
      ' Palette='+DbgS(ARawImage.Palette)+
      ' PaletteSize='+dbgs(ARawImage.PaletteSize)+
      ' BitsPerPixel='+dbgs(Desc.BitsPerPixel)+
      '');
    {$ENDIF}

    // ToDo: check description

    GdiObject := NewGDIObject(gdiBitmap);
    GdiObject^.GDIBitmapType := gbPixmap;
    GdiObject^.Depth := ImgDepth;

    // create Pixmap from data
    if ImgDepth = 1
    then begin
      // create a GdkBitmap
      if ImgData <> nil
      then Drawable := gdk_bitmap_create_from_data(nil, ImgData, ImgWidth, ImgHeight)
      else Drawable := gdk_pixmap_new(nil, ImgWidth, ImgHeight, 1);

      GdiObject^.GDIBitmapObject := Drawable;
      GdiObject^.GDIBitmapType := gbBitmap;
    end
    else begin
      if (ImgData <> nil) and (ImgDepth = 32)
      then begin
        case Desc.LineEnd of
          rileQWordBoundary: begin
            RowStride := ImgWidth;
            if ImgWidth and 1 <> 0 then Inc(RowStride);
            RowStride := RowStride shl 2;
          end;
          rileDQWordBoundary: begin
            RowStride := ImgWidth shr 1;
            if ImgWidth and 3 <> 0 then Inc(RowStride);
            RowStride := RowStride shl 3;
          end;
        else
          RowStride := ImgWidth shl 2;
        end;

        // check if the pixels are in order, pixbuf expects them in R-G-B-A
        Desc.GetRGBIndices(Ridx, Gidx, Bidx, Aidx);

        if (Ridx <> 0) or (Gidx <> 1) or (Bidx <> 2) or (AIdx <> 3)
        then begin
          // put components in right order
          GetMem(Data, ImgDataSize);
          DstRowPtr := Data;
          SrcRowPtr := ImgData;
          y := ImgHeight;
          while y > 0 do
          begin
            Src := SrcRowPtr;
            Dst := DstRowPtr;
            x := ImgWidth;
            while x > 0 do
            begin
              Dst[0] := Src[Ridx];
              Dst[1] := Src[Gidx];
              Dst[2] := Src[Bidx];
              Dst[3] := Src[Aidx];

              Inc(Src, 4);
              Inc(Dst, 4);
              Dec(x);
            end;
            Inc(SrcRowPtr, Rowstride);
            Inc(DstRowPtr, Rowstride);
            Dec(y);
          end;
        end
        else begin
          // components are in place

          // gtkPixbuf doesn't like invalid dataSize/MaskSize < 32. issue #8553.
          if (ARawImage.MaskSize > 0) and (ImgDepth = 32) then
          begin
            CreateWithAlpha := Trunc(ARawImage.DataSize / ARawImage.MaskSize) = 32;
            {$IFDEF VerboseRawImage}
            if not CreateWithAlpha then
              DebugLn('TGtkWidgetSet.CreateBitmapFromRawImage B WARNING: This image have invalid DataSize / MaskSize.');
            {$ENDIF}
          end;
          Data := ImgData;
        end;

        TmpPixBuf := gdk_pixbuf_new_from_data(Data, GDK_COLORSPACE_RGB, CreateWithAlpha,
          8, ImgWidth, ImgHeight, RowStride, nil, nil);
        // we need to copy our pixbuf into a new one to allow data deallocation
        Pixbuf := gdk_pixbuf_copy(TmpPixBuf);
        gdk_pixbuf_unref(TmpPixBuf);
        GdiObject^.GDIBitmapType := gbPixbuf;
        GdiObject^.GDIPixbufObject := Pixbuf;
        if Data <> ImgData
        then FreeMem(Data);
        GdiObject^.visual := gdk_visual_get_system();
        gdk_visual_ref(GdiObject^.visual);
        //DbgDumpPixbuf(Pixbuf, 'CreateBitmaps (32)');
      end
      else begin
        // check if the depth is supported
        Visual := gdk_visual_get_best_with_depth(Min(ImgDepth, 24));
        // try some alternative (I'm not sure if we should fail here instead)
        // if we don't have a visual we cannot draw anyway
        //if Visual = nil
        //then Visual := gdk_visual_get_best;
        if Visual = nil
        then Exit; // this depth is not supported

        Drawable := gdk_pixmap_new(nil, ImgWidth, ImgHeight, Visual^.depth);

        // create a GdkPixmap
        if ImgData <> nil
        then begin
          { The gdk_pixmap_create_from_data creates only a two-color pixmap so we can not use it }

          GdkImage := gdk_image_new(GDK_IMAGE_FASTEST, Visual, ImgWidth, ImgHeight);

          {$ifdef VerboseRawImage}
          //DebugLn('TGtkWidgetSet.CreateBitmapFromRawImage GdkImage: ',
          //  ' BytesPerLine=',dbgs(GdkImage^.bpl),
          //  ' BitsPerPixel=',dbgs(GetPGdkImageBitsPerPixel(GdkImage)),
          //  ' ByteOrder=',dbgs({$ifdef Gtk1}GdkImage^.byte_order{$else}ord(GdkImage^.byte_order){$endif}),
          //  '');
          {$endif}

          if ARawImage.Description.BitsPerPixel <> GetGdkImageBitsPerPixel(GdkImage)
          then begin
            RaiseGDBException('TGtkWidgetSet.CreateBitmapFromRawImage Incompatible BitsPerPixel');
          end;
          if ImgDataSize <> GdkImage^.bpl * ImgHeight
          then begin
            RaiseGDBException('TGtkWidgetSet.CreateBitmapFromRawImage Incompatible DataSize');
          end;

          System.Move(ImgData^, GdkImage^.mem^, ImgDataSize);
          if ImgDepth = 1
          then CheckGdkImageBitOrder(GdkImage, GdkImage^.mem, ImgDataSize);
          GC := gdk_gc_new(Drawable);
          gdk_draw_image(Drawable, GC, GdkImage, 0, 0, 0, 0, ImgWidth, ImgHeight);
          gdk_gc_unref(GC);
          gdk_image_destroy(GdkImage);

          //DbgDumpPixmap(Drawable, 'CreateBitmaps');
        end;

        GdiObject^.GDIPixmapObject.Image := Drawable;
        GdiObject^.Visual := gdk_window_get_visual(Drawable);
        gdk_visual_ref(GdiObject^.Visual);
      end;
    end;
    
    if ASkipMask
    then begin
      Result := True;
      Exit;
    end;

    // create mask

    {$IFDEF VerboseRawImage}
    DebugLn('TGtkWidgetSet.CreateBitmapFromRawImage creating mask .. ');
    {$ENDIF}

    if ARawImage.IsMasked(False)
    then Drawable := gdk_bitmap_create_from_data(nil, ImgMask, ImgWidth, ImgHeight)
    else begin
      Drawable := gdk_pixmap_new(nil, ImgWidth, ImgHeight, 1);
      // clear drawable, the contents of a new pixmap are indefined
      GC := gdk_gc_new(Drawable);
      gdk_draw_rectangle(Drawable, GC, 1, 0, 0, ImgWidth, ImgHeight);
      gdk_gc_unref(GC);
    end;

    GdiMaskObject := NewGDIObject(gdiBitmap);
    GdiMaskObject^.Depth := 1;
    GdiMaskObject^.GDIBitmapType := gbBitmap;
    GdiMaskObject^.GDIBitmapObject := Drawable;

    //DbgDumpBitmap(Drawable, 'CreateBitmaps - Mask');

    Result := True;
  except
    DeleteObject(ABitmap);
    ABitmap := 0;
    DeleteObject(AMask);
    AMask := 0;
  end;
end;

{------------------------------------------------------------------------------
  Function: RawImage_DescriptionFromBitmap
  Params: Bitmap: HBITMAP;
          Desc: PRawImageDescription
  Returns: boolean;

 ------------------------------------------------------------------------------}
function TGtkWidgetSet.RawImage_DescriptionFromBitmap(ABitmap: HBITMAP; out ADesc: TRawImageDescription): boolean;
var
  GDIObject: PGDIObject absolute ABitmap;
begin
  Result := False;
  if not IsValidGDIObject(ABitmap)
  then begin
    DebugLn('WARNING: [TGtkWidgetSet.GetBitmapRawImageDescription] invalid Bitmap!');
    exit;
  end;

  case GDIObject^.GDIBitmapType of
    gbBitmap:
      Result := RawImage_DescriptionFromDrawable(ADesc,
        GdiObject^.GDIBitmapObject, False);
    gbPixmap:
      Result := RawImage_DescriptionFromDrawable(ADesc,
        GdiObject^.GDIPixmapObject.Image, GdiObject^.GDIPixmapObject.Mask <> nil);
    gbPixbuf:
      Result := RawImage_DescriptionFromPixbuf(ADesc, GdiObject^.GDIPixbufObject);
  else
    DebugLn('WARNING: [TGtkWidgetSet.RawImage_DescriptionFromBitmap] Unknown GDIBitmapType');
    Exit;
  end;
end;

{------------------------------------------------------------------------------
  function RawImage_DescriptionFromDevice
  Params: DC: HDC;
          Desc: PRawImageDescription
  Returns: boolean;

  Retrieves the information about the structure of the supported image data.
 ------------------------------------------------------------------------------}
function TGtkWidgetSet.RawImage_DescriptionFromDevice(ADC: HDC; out ADesc: TRawImageDescription): boolean;
var
  DevCon: TGtkDeviceContext absolute ADC;

  Drawable: PGdkDrawable;
  UseAlpha: Boolean;
begin
  UseAlpha := False;
  if IsValidDC(ADC)
  then begin
    Drawable := DevCon.Drawable;
    if DevCon.CurrentBitmap <> nil
    then begin
      case DevCon.CurrentBitmap^.GDIBitmapType of
        gbBitmap: Drawable := DevCon.CurrentBitmap^.GDIBitmapObject;
        gbPixmap: begin
          Drawable := DevCon.CurrentBitmap^.GDIPixmapObject.Image;
          UseAlpha := DevCon.CurrentBitmap^.GDIPixmapObject.Mask <> nil;
        end;
        gbPixbuf: begin
          Result := RawImage_DescriptionFromPixbuf(ADesc, DevCon.CurrentBitmap^.GDIPixbufObject);
          Exit;
        end;
      end;
    end;
  end
  else
    Drawable := nil;
  
  Result := RawImage_DescriptionFromDrawable(ADesc, Drawable, UseAlpha);
end;

{------------------------------------------------------------------------------
  Function: RawImage_QueryDescription
  Params: AFlags:
          ADesc:
  Returns:

 ------------------------------------------------------------------------------}
function TGtkWidgetSet.RawImage_QueryDescription(AFlags: TRawImageQueryFlags; var ADesc: TRawImageDescription): Boolean;
var
  Desc: TRawImageDescription;
begin
  if riqfGrey in AFlags
  then begin
    DebugLn('TGtkWidgetSet.RawImage_QueryDescription: riqfGrey not (yet) supported');
    Exit(False);
  end;
  
  if riqfPalette in AFlags
  then begin
    DebugLn('TGtkWidgetSet.RawImage_QueryDescription: riqfPalette not (yet) supported');
    Exit(False);
  end;

  Desc.Init;
  Result := RawImage_DescriptionFromDrawable(Desc, nil, riqfAlpha in AFlags);
  if not Result then Exit;

  if not (riqfUpdate in AFlags) then
    ADesc.Init;

  // if there's mask gtk assumes it's rgba (not XBM format).issue #12362
  if (riqfUpdate in AFlags) and (riqfMono in AFlags) and (riqfMask in AFlags) then
      AFlags := AFlags - [riqfMono] + [riqfRgb];

  if riqfMono in AFlags
  then begin
    ADesc.Format := ricfGray;
    ADesc.Depth := 1;
    ADesc.BitOrder := Desc.MaskBitOrder;
    ADesc.ByteOrder := riboLSBFirst;
    ADesc.LineOrder := Desc.LineOrder;
    ADesc.LineEnd := Desc.MaskLineEnd;
    ADesc.BitsPerPixel := Desc.MaskBitsPerPixel;
    ADesc.RedPrec := 1;
    ADesc.RedShift := Desc.MaskShift;
    // in theory only redshift is used, but if someone reads it as color thsi works too.
    ADesc.GreenPrec := 1;
    ADesc.GreenShift := Desc.MaskShift;
    ADesc.BluePrec := 1;
    ADesc.BlueShift := Desc.MaskShift;
  end
(*
  //TODO
  else if riqfGrey in AFlags
  then begin
    ADesc.Format := ricfGray;
    ADesc.Depth := 8;
    ADesc.BitOrder := Desc.BitOrder;
    ADesc.ByteOrder := Desc.ByteOrder;
    ADesc.LineOrder := Desc.LineOrder;
    ADesc.LineEnd := Desc.LineEnd;
    ADesc.BitsPerPixel := 8;
    ADesc.RedPrec := 8;
    ADesc.RedShift := 0;
  end
*)
  else if riqfRGB in AFlags
  then begin
    ADesc.Format := ricfRGBA;
    ADesc.Depth := Desc.Depth;
    ADesc.BitOrder := Desc.BitOrder;
    ADesc.ByteOrder := Desc.ByteOrder;
    ADesc.LineOrder := Desc.LineOrder;
    ADesc.LineEnd := Desc.LineEnd;
    ADesc.BitsPerPixel := Desc.BitsPerPixel;
    ADesc.RedPrec := Desc.RedPrec;
    ADesc.RedShift := Desc.RedShift;
    ADesc.GreenPrec := Desc.GreenPrec;
    ADesc.GreenShift := Desc.GreenShift;
    ADesc.BluePrec := Desc.BluePrec;
    ADesc.BlueShift := Desc.BlueShift;
  end;

  if riqfAlpha in AFlags
  then begin
    ADesc.AlphaPrec := Desc.AlphaPrec;
    ADesc.AlphaShift := Desc.AlphaShift;
  end;

  if riqfMask in AFlags
  then begin
    ADesc.MaskBitsPerPixel := Desc.MaskBitsPerPixel;
    ADesc.MaskShift := Desc.MaskShift;
    ADesc.MaskLineEnd := Desc.MaskLineEnd;
    ADesc.MaskBitOrder := Desc.MaskBitOrder;
  end;

(*
  //TODO
  if riqfPalette in AFlags
  then begin
    ADesc.PaletteColorCount := Desc.PaletteColorCount;
    ADesc.PaletteBitsPerIndex := Desc.PaletteBitsPerIndex;
    ADesc.PaletteShift := Desc.PaletteShift;
    ADesc.PaletteLineEnd := Desc.PaletteLineEnd;
    ADesc.PaletteBitOrder := Desc.PaletteBitOrder;
    ADesc.PaletteByteOrder := Desc.PaletteByteOrder;
  end;
*)
end;

{------------------------------------------------------------------------------
  function TGtkWidgetSet.GetRawImageFromBitmap(SrcBitmap, SrcMaskBitmap: HBITMAP;
    const SrcRect: TRect; var NewRawImage: TRawImage): boolean; override;
 ------------------------------------------------------------------------------}
function TGtkWidgetSet.RawImage_FromBitmap(out ARawImage: TRawImage; ABitmap, AMask: HBITMAP; ARect: PRect): Boolean;
var
  GdiBitmap: PGDIObject absolute ABitmap;
  GdiMask: PGDIObject absolute AMask;
  Drawable: PGdkDrawable;
  Bitmap: PGdkBitmap;
begin
  Result := false;
  {$IFDEF VerboseRawImage}
  DebugLn('TGtkWidgetSet.GetRawImageFromBitmap A');
  {$ENDIF}
  ARawImage.Init;

  if not IsValidGDIObject(ABitmap)
  then begin
    DebugLn('WARNING: [TGtkWidgetSet.RawImage_FromBitmap] invalid Bitmap!');
    exit;
  end;
  if (AMask <> 0) and not IsValidGDIObject(AMask)
  then begin
    DebugLn('WARNING: [TGtkWidgetSet.RawImage_FromBitmap] invalid Mask');
    exit;
  end;

  try
    // get rawimage for Bitmap
    case GdiBitmap^.GDIBitmapType of
      gbBitmap: begin
        Drawable := GdiBitmap^.GDIBitmapObject;
        Bitmap := nil;
      end;
      gbPixmap: begin
        Drawable := GdiBitmap^.GDIPixmapObject.Image;
        Bitmap := GdiBitmap^.GDIPixmapObject.Mask;
      end;
      gbPixbuf: begin
        Result := RawImage_FromPixbuf(ARawImage, GdiBitmap^.GDIPixbufObject, ARect);
        Exit;
      end;
    else
      DebugLn('WARNING: [TGtkWidgetSet.RawImage_FromBitmap] Unknown GDIBitmapType');
      Exit;
    end;
    {$IFDEF VerboseRawImage}
    DebugLn('TGtkWidgetSet.RawImage_FromBitmap A GdkPixmap=',DbgS(Drawable),' SrcMaskBitmap=',DbgS(Bitmap));
    {$ENDIF}

    //DbgDumpPixmap(Drawable, 'RawImage_FromBitmap - drawable');
    //DbgDumpBitmap(Bitmap, 'RawImage_FromBitmap - alpha');

    Result := RawImage_FromDrawable(ARawImage, Drawable, Bitmap, ARect);
    if Result and (AMask <> 0)
    then begin
      if GdiMask^.GDIBitmapType <> gbBitmap
      then begin
        DebugLn('WARNING: [TGtkWidgetSet.RawImage_FromBitmap] Unsupported GDIBitmapType for mask');
        Exit;
      end;
      
      Bitmap := GdiMask^.GDIBitmapObject;
      RawImage_AddMask(ARawImage, Bitmap, ARect);
      //DbgDumpBitmap(Bitmap, 'RawImage_FromBitmap - mask');
    end
    else
      ARawImage.Description.MaskBitsPerPixel := 0;

    if not Result
    then DebugLn('WARNING: [TGtkWidgetSet.RawImage_FromBitmap] unable to GetRawImageFromGdkWindow Image');

  except
    ARawImage.FreeData;
  end;
end;

{------------------------------------------------------------------------------
  function TGtkWidgetSet.GetRawImageFromDevice(SrcDC: HDC; const SrcRect: TRect;
    var NewRawImage: TRawImage): boolean;

 ------------------------------------------------------------------------------}
function TGtkWidgetSet.RawImage_FromDevice(out ARawImage: TRawImage; ADC: HDC; const ARect: TRect): boolean;
var
  DevCtx: TGtkDeviceContext absolute ADC;
  DCOrigin: TPoint;
  R: TRect;
  Drawable: PGdkDrawable;
begin
  Result := False;
  if not IsValidDC(ADC)
  then begin
    DebugLn('WARNING: TGtkWidgetSet.GetRawImageFromDevice invalid SrcDC');
    Exit(False);
  end;

  DCOrigin := DevCtx.Offset;
  {$IFDEF VerboseRawImage}
  DebugLn('TGtkWidgetSet.GetRawImageFromDevice A DCOrigin=',dbgs(DCOrigin.X),',',dbgs(DCOrigin.Y),' SrcRect=',dbgs(ARect.Left),',',dbgs(ARect.Top),',',dbgs(ARect.Right),',',dbgs(ARect.Bottom));
  {$ENDIF}
  R := ARect;
  OffSetRect(R, DCOrigin.x, DCOrigin.y);

  Drawable := DevCtx.Drawable;
  if Drawable = nil
  then begin
    // get screen shot
    {$IFDEF Gtk1}
    exit;
    {$ELSE}
    Drawable := gdk_screen_get_root_window(gdk_screen_get_default);
    {$ENDIF}
  end;
  Result := RawImage_FromDrawable(ARawImage, Drawable, nil, @R);
end;

{------------------------------------------------------------------------------
  Function: GetControlConstraints
  Params: Constraints: TObject
  Returns: true on success

  Updates the constraints object (e.g. TSizeConstraints) with interface specific
  bounds.
 ------------------------------------------------------------------------------}
function TGtkWidgetSet.GetControlConstraints(Constraints: TObject): boolean;
var
  SizeConstraints: TSizeConstraints absolute Constraints;
  Widget: PGtkWidget;
  MinWidth: Integer;
  MinHeight: Integer;
  MaxWidth: Integer;
  MaxHeight: Integer;
begin
  Result := True;

  if Constraints is TSizeConstraints then
  begin
    MinWidth := 1;
    MinHeight := 1;
    MaxWidth := 0;
    MaxHeight := 0;

    if (SizeConstraints.Control=nil) then exit;

    if SizeConstraints.Control is TScrollBar then begin
      // TScrollBar
      if TScrollBar(SizeConstraints.Control).Kind=sbHorizontal then begin
        Widget:=GetStyleWidget(lgsHorizontalScrollbar);
        MinHeight:=Widget^.requisition.Height;
        MaxHeight:=MinHeight;
      end else begin
        Widget:=GetStyleWidget(lgsVerticalScrollbar);
        MinWidth:=Widget^.requisition.Width;
        MaxWidth:=MinWidth;
      end;
      //DebugLn('TGtkWidgetSet.GetControlConstraints A '+dbgs(MinWidth)+','+dbgs(MinHeight),' ',dbgs(TScrollBar(SizeConstraints.Control).Kind=sbHorizontal),' ',TScrollBar(SizeConstraints.Control).Name);
    end
    else if SizeConstraints.Control is TCustomSplitter then begin
      // TCustomSplitter
      if TCustomSplitter(SizeConstraints.Control).ResizeAnchor in [akTop,akBottom] then
      begin
        Widget:=GetStyleWidget(lgsHorizontalPaned);
        MinHeight:=Widget^.requisition.Height;
        MaxHeight:=MinHeight;
      end else begin
        Widget:=GetStyleWidget(lgsVerticalPaned);
        MinWidth:=Widget^.requisition.Width;
        MaxWidth:=MinWidth;
      end;
    end
    else if SizeConstraints.Control is TCustomMemo then begin
      // TCustomMemo
      Widget:=GetStyleWidget(lgsHorizontalScrollbar);
      MinHeight:=Widget^.requisition.Height+20;
      Widget:=GetStyleWidget(lgsVerticalScrollbar);
      MinWidth:=Widget^.requisition.Width+20;
    end
    else if SizeConstraints.Control is TCustomTrackBar then begin
      // TCustomTrackBar
      if TCustomTrackBar(SizeConstraints.Control).Orientation=trHorizontal then
      begin
        Widget:=GetStyleWidget(lgsHScale);
        MinHeight:=Widget^.requisition.height;
      end else begin
        Widget:=GetStyleWidget(lgsVScale);
        MinWidth:=Widget^.requisition.width;
      end;
      //DebugLn(['TGtkWidgetSet.GetControlConstraints ',DbgSName(SizeConstraints.Control),' ',MinWidth,',',MinHeight]);
    end;

    SizeConstraints.SetInterfaceConstraints(MinWidth,MinHeight,
                                            MaxWidth,MaxHeight);
  end;
end;

{------------------------------------------------------------------------------
  function TGtkWidgetSet.GetLCLOwnerObject(Handle: HWnd): TObject;

 ------------------------------------------------------------------------------}
function TGtkWidgetSet.GetLCLOwnerObject(Handle: HWnd): TObject;
begin
  if Handle<>0 then
    Result:=GetNearestLCLObject(PGtkWidget(Handle))
  else
    Result:=nil;
end;

//##apiwiz##eps##   // Do not remove, no wizard declaration after this line


function waithandle_iocallback(source: PGIOChannel; condition: TGIOCondition;
  data: gpointer): gboolean; cdecl;
var
  lEventHandler: PWaitHandleEventHandler absolute data;
begin
  //debugln('waithandle_iocallback lEventHandler=',HexStr(Cardinal(lEventHandler),8));
  lEventHandler^.OnEvent(lEventHandler^.UserData, condition);
  Result := true;
end;

function TGtkWidgetSet.AddEventHandler(AHandle: THandle; AFlags: dword;
  AEventHandler: TWaitHandleEvent; AData: PtrInt): PEventHandler;
var
  giochannel: pgiochannel;
  lEventHandler: PWaitHandleEventHandler;
begin
  if AEventHandler = nil then exit;
  New(lEventHandler);
  giochannel := g_io_channel_unix_new(AHandle);
  lEventHandler^.Handle := AHandle;
  lEventHandler^.UserData := AData;
  lEventHandler^.GIOChannel := giochannel;
  lEventHandler^.OnEvent := AEventHandler;
  lEventHandler^.GSourceID := g_io_add_watch(giochannel,
    AFlags, @waithandle_iocallback, lEventHandler);
  //debugln('TGtkWidgetSet.AddEventHandler lEventHandler=',HexStr(Cardinal(lEventHandler),8),' AHandle=',dbgs(lEventHandler^.Handle));
  lEventHandler^.PrevHandler := nil;
  lEventHandler^.NextHandler := FWaitHandles;
  if FWaitHandles <> nil then
    FWaitHandles^.PrevHandler := lEventHandler;
  FWaitHandles := lEventHandler;
  Result := lEventHandler;
end;


procedure TGtkWidgetSet.RemoveEventHandler(var AHandler: PEventHandler);
var
  lEventHandler: PWaitHandleEventHandler absolute AHandler;
begin
  if AHandler = nil then exit;
  g_source_remove(lEventHandler^.GSourceID);
  { channel will be freed with ref count drops to 0 }
  g_io_channel_unref(lEventHandler^.GIOChannel);
  if lEventHandler^.PrevHandler = nil then
    FWaitHandles := lEventHandler^.NextHandler
  else
    lEventHandler^.PrevHandler^.NextHandler := lEventHandler^.NextHandler;
  if lEventHandler^.NextHandler <> nil then
    lEventHandler^.NextHandler^.PrevHandler := lEventHandler^.PrevHandler;
  //debugln('TGtkWidgetSet.RemoveEventHandler lEventHandler=',HexStr(Cardinal(lEventHandler),8),' AHandle=',dbgs(lEventHandler^.Handle));
  Dispose(lEventHandler);
  AHandler := nil;
end;

procedure TGtkWidgetSet.SetEventHandlerFlags(AHandler: PEventHandler; NewFlags: dword);
var
  lEventHandler: PWaitHandleEventHandler absolute AHandler;
begin
  if AHandler = nil then exit;
  g_source_remove(lEventHandler^.GSourceID);
  lEventHandler^.GSourceID := g_io_add_watch(lEventHandler^.GIOChannel,
    NewFlags, @waithandle_iocallback, lEventHandler);
  //debugln('TGtkWidgetSet.SetEventHandlerFlags lEventHandler=',HexStr(Cardinal(lEventHandler),8),' AHandle=',dbgs(lEventHandler^.Handle));
end;

type
  PPipeEventInfo = ^TPipeEventInfo;
  TPipeEventInfo = record
    Handler: PEventHandler;
    UserData: PtrInt;
    OnEvent: TPipeEvent;
  end;

function TGtkWidgetSet.AddPipeEventHandler(AHandle: THandle;
  AEventHandler: TPipeEvent; AData: PtrInt): PPipeEventHandler;
var
  lPipeEventInfo: PPipeEventInfo;
begin
  if AEventHandler = nil then exit;
  New(lPipeEventInfo);
  lPipeEventInfo^.UserData := AData;
  lPipeEventInfo^.OnEvent := AEventHandler;
  lPipeEventInfo^.Handler := AddEventHandler(AHandle, G_IO_IN or G_IO_HUP or G_IO_OUT,
    @HandlePipeEvent, PtrUInt(lPipeEventInfo));
  Result := lPipeEventInfo;
end;

procedure TGtkWidgetSet.HandlePipeEvent(AData: PtrInt; AFlags: dword);
var
  lPipeEventInfo: PPipeEventInfo absolute AData;
  lReasons: TPipeReasons;
begin
  lReasons := [];
  if AFlags and G_IO_IN = G_IO_IN then
    Include(lReasons, prDataAvailable);
  if AFlags and G_IO_OUT = G_IO_OUT then
    Include(lReasons, prCanWrite);
  if AFlags and G_IO_HUP = G_IO_HUP then
    Include(lReasons, prBroken);

  lPipeEventInfo^.OnEvent(lPipeEventInfo^.UserData, lReasons);
end;

procedure TGtkWidgetSet.RemovePipeEventHandler(var AHandler: PPipeEventHandler);
var
  lPipeEventInfo: PPipeEventInfo absolute AHandler;
begin
  if AHandler = nil then exit;
  RemoveEventHandler(lPipeEventInfo^.Handler);
  Dispose(lPipeEventInfo);
  AHandler := nil;
end;

{$ifdef UNIX}
function TGtkWidgetSet.AddProcessEventHandler(AHandle: THandle;
  AEventHandler: TChildExitEvent; AData: PtrInt): PProcessEventHandler;
var
  lHandler: PChildSignalEventHandler;
begin
  if AEventHandler = nil then exit(nil);
  New(lHandler);
  lHandler^.PID := TPid(AHandle);
  lHandler^.UserData := AData;
  lHandler^.OnEvent := AEventHandler;
  lHandler^.PrevHandler := nil;
  lHandler^.NextHandler := FChildSignalHandlers;
  if FChildSignalHandlers <> nil then
    FChildSignalHandlers^.PrevHandler := lHandler;
  FChildSignalHandlers := lHandler;
  Result := lHandler;
end;

procedure TGtkWidgetSet.RemoveProcessEventHandler(var AHandler: PProcessEventHandler);
var
  lHandler: PChildSignalEventHandler absolute AHandler;
begin
  if AHandler = nil then exit;
  if lHandler^.PrevHandler = nil then
    FChildSignalHandlers := lHandler^.NextHandler
  else
    lHandler^.PrevHandler^.NextHandler := lHandler^.NextHandler;
  if lHandler^.NextHandler <> nil then
    lHandler^.NextHandler^.PrevHandler := lHandler^.PrevHandler;
  Dispose(lHandler);
  AHandler := nil;
end;
{$else}
{$IFDEF VerboseGtkToDos}{$warning TGtkWidgetSet.RemoveProcessEventHandler and TGtkWidgetSet.AddProcessEventHandler not implemented on this OS}{$ENDIF}
//PChildSignalEventHandler is only defined on unix
function TGtkWidgetSet.AddProcessEventHandler(AHandle: THandle;
  AEventHandler: TChildExitEvent; AData: PtrInt): PProcessEventHandler;
begin
  Result := nil;
end;

procedure TGtkWidgetSet.RemoveProcessEventHandler(var AHandler: PProcessEventHandler);
begin
end;
{$endif}

