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


//##apiwiz##sps##   // Do not remove

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
  function TGTKWidgetSet.FontCanUTF8(Font: HFont): boolean;
  
  True if font recognizes Unicode UTF8 encoding.
 ------------------------------------------------------------------------------}
function TGTKWidgetSet.FontCanUTF8(Font: HFont): boolean;
begin
  Result:=IsValidGDIObject(Font)
    {$IFDEF Gtk1}
    and FontIsDoubleByteCharsFont(PGdiObject(Font)^.GDIFontObject)
    {$ENDIF}
    ;
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
  Result:='';
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
  
const
                        // riboLSBFirst, riboMSBFirst
  COMPONENT_MASK: array[TRawImageByteOrder] of Byte = (0, 3);
  
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
  Bitmap: PGdkBitmap;
  Pixbuf: PGdkPixbuf;
  GC: PGdkGC;
  Visual: PGdkVisual;
  GdkImage: PGdkImage;
  RowStride: Cardinal;
  Ridx, Gidx, Bidx, Aidx: Byte;
  Data: Pointer;
  Src, Dst, SrcRowPtr, DstRowPtr: PByte;
  x, y: Cardinal;
begin
  Result := False;
  ABitmap := 0;
  AMask := 0;

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
    Bitmap := nil;

    case ImgDepth of
      1: begin
        // create a GdkBitmap
        if ImgData <> nil
        then Drawable := gdk_bitmap_create_from_data(nil, ImgData, ImgWidth, ImgHeight)
        else Drawable := gdk_pixmap_new(nil, ImgWidth, ImgHeight, 1);

        GdiObject^.GDIBitmapObject := Drawable;
        GdiObject^.GDIBitmapType := gbBitmap;
      end;
      32: begin
        if ImgData = nil
        then begin
          Drawable := gdk_pixmap_new(nil, ImgWidth, ImgHeight, ImgDepth);
        end
        else begin
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
          Ridx := (Desc.RedShift shr 3) xor COMPONENT_MASK[Desc.ByteOrder];
          Gidx := (Desc.GreenShift shr 3) xor COMPONENT_MASK[Desc.ByteOrder];
          Bidx := (Desc.BlueShift shr 3) xor COMPONENT_MASK[Desc.ByteOrder];
          Aidx := (Desc.AlphaShift shr 3) xor COMPONENT_MASK[Desc.ByteOrder];
          
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
            Data := ImgData;
          end;

          Pixbuf := gdk_pixbuf_new_from_data(Data, GDK_COLORSPACE_RGB, True, 8, ImgWidth, ImgHeight, RowStride, nil, nil);
          // DbgDumpPixbuf(Pixbuf, 'CreateBitmaps (32)');
          gdk_pixbuf_render_pixmap_and_mask(Pixbuf, Drawable, Bitmap, $80);
          gdk_pixbuf_unref(Pixbuf);
          if Data <> ImgData
          then FreeMem(Data);
        end;

        GdiObject^.GDIPixmapObject.Image := Drawable;
        GdiObject^.GDIPixmapObject.Mask := Bitmap;
        GdiObject^.Visual := gdk_window_get_visual(Drawable);
        gdk_visual_ref(GdiObject^.Visual);
        
        //DbgDumpPixmap(Drawable, 'CreateBitmaps (32)');
        //DbgDumpBitmap(Bitmap, 'CreateBitmaps (32)');
      end;
    else
      // create a GdkPixmap
      if ImgData <> nil
      then begin
        { The gdk_pixmap_create_from_data creates only a two-color pixmap so we cant use it }

        Drawable := gdk_pixmap_new(nil, ImgWidth, ImgHeight, ImgDepth);
        // Create a GdkImage, copy our data into it and create a pixmap from it
        Visual := gdk_visual_get_best_with_depth(ImgDepth);
        if Visual = nil
        then Exit; // this depth is not supported
        
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
      end
      else begin
        Drawable := gdk_pixmap_new(nil, ImgWidth, ImgHeight, ImgDepth);
      end;

      GdiObject^.GDIPixmapObject.Image := Drawable;
      GdiObject^.Visual := gdk_window_get_visual(Drawable);
      gdk_visual_ref(GdiObject^.Visual);
    end;
    
    if ASkipMask
    then begin
      Result := True;
      Exit;
    end;


    // create mask
    if ARawImage.IsMasked(False)
    then begin
      {$IFDEF VerboseRawImage}
      DebugLn('TGtkWidgetSet.CreateBitmapFromRawImage creating mask .. ');
      {$ENDIF}

      Drawable := gdk_bitmap_create_from_data(nil, ImgMask, ImgWidth, ImgHeight);

      GdiMaskObject := NewGDIObject(gdiBitmap);
      GdiMaskObject^.Depth := 1;
      GdiMaskObject^.GDIBitmapType := gbBitmap;
      GdiMaskObject^.GDIBitmapObject := Drawable;

      //DbgDumpBitmap(Drawable, 'CreateBitmaps - Mask');
    end;
    
    Result := true;
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
  Drawable: PGdkDrawable;
  CustomAlpha: Boolean;
begin
  Result := false;
  if not IsValidGDIObject(ABitmap)
  then begin
    DebugLn('WARNING: [TGtkWidgetSet.GetBitmapRawImageDescription] invalid Bitmap!');
    exit;
  end;

  case GDIObject^.GDIBitmapType of
    gbBitmap: begin
      Drawable := GdiObject^.GDIBitmapObject;
      CustomAlpha := False;
    end;
    gbPixmap: begin
      Drawable := GdiObject^.GDIPixmapObject.Image;
      CustomAlpha := GdiObject^.GDIPixmapObject.Mask <> nil;
    end;
  else
    DebugLn('WARNING: [TGtkWidgetSet.GetBitmapRawImageDescription] GDI_RGBImage not implemented');
    Exit;
  end;
  Result := RawImage_DescriptionFromDrawable(ADesc, Drawable, CustomAlpha);
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
      end;
    end;
  end
  else Drawable := nil;
  
  Result := RawImage_DescriptionFromDrawable(ADesc, Drawable, UseAlpha);
end;

{------------------------------------------------------------------------------
  function TGtkWidgetSet.GetRawImageFromBitmap(SrcBitmap, SrcMaskBitmap: HBITMAP;
    const SrcRect: TRect; var NewRawImage: TRawImage): boolean; override;
 ------------------------------------------------------------------------------}
function TGtkWidgetSet.RawImage_FromBitmap(out ARawImage: TRawImage; ABitmap, AMask: HBITMAP; const ARect: TRect): Boolean;
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
    end;

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
  Result := RawImage_FromDrawable(ARawImage, Drawable, nil, R);
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
  SizeConstraints: TSizeConstraints;
  Widget: PGtkWidget;
  MinWidth: Integer;
  MinHeight: Integer;
  MaxWidth: Integer;
  MaxHeight: Integer;
begin
  Result:=true;
  if Constraints is TSizeConstraints then begin
    MinWidth:=1;
    MinHeight:=1;
    MaxWidth:=0;
    MaxHeight:=0;
    SizeConstraints:=TSizeConstraints(Constraints);

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

{------------------------------------------------------------------------------
  function TGtkWidgetSet.IntfSendsUTF8KeyPress: boolean;

 ------------------------------------------------------------------------------}
function TGtkWidgetSet.IntfSendsUTF8KeyPress: boolean;
begin
  Result:=true;
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
  if AEventHandler = nil then exit;
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
{$warning TGtkWidgetSet.RemoveProcessEventHandler and TGtkWidgetSet.AddProcessEventHandler not implemented on this OS}
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

