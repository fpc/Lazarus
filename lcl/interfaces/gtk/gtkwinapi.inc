(******************************************************************************
                         All GTK Winapi implementations.
                   Initial Revision  : Sat Nov 13 12:53:53 1999


  !! Keep alphabetical !!

  Support routines go to gtkproc.pp

 ******************************************************************************
 Implementation
 ******************************************************************************)

{$IFOPT C-}
// Uncomment for local trace
//  {$C+}
//  {$DEFINE ASSERT_IS_ON}
{$ENDIF}

const
  SYes = 'Yes';
  SNo = 'No';
  SOK = 'OK';
  SCancel = 'Cancel';
  SAbort = 'Abort';
  SRetry = 'Retry';
  SIgnore = 'Ignore';
  
const
  BOOL_TEXT: array[Boolean] of string = ('False', 'True');  

//##apiwiz##sps##   // Do not remove

{------------------------------------------------------------------------------
  Function: BitBlt
  Params:  DestDC:                The destination devicecontext
           X, Y:                  The left/top corner of the destination rectangle
           Width, Height:         The size of the destination rectangle
           SrcDC:                 The source devicecontext 
           XSrc, YSrc:            The left/top corner of the source rectangle   
           Rop:                   The raster operation to be performed 
  Returns: True if succesful

  The BitBlt function copies a bitmap from a source context into a destination 
  context using the specified raster operation.
 ------------------------------------------------------------------------------}
function TgtkObject.BitBlt(DestDC: HDC; X, Y, Width, Height: Integer; 
  SrcDC: HDC; XSrc, YSrc: Integer; Rop: DWORD): Boolean; 
type
  TBltFunction = function: Boolean;

  function DrawableToDrawable: Boolean;
  begin
    gdk_draw_pixmap(PDeviceContext(DestDC)^.Drawable, PDeviceContext(DestDC)^.GC,
      PDeviceContext(SrcDC)^.Drawable,  XSrc, YSrc, X, Y, Width, Height);
    Result:=false
  end;

  function PixmapToDrawable: Boolean;
  begin
    Result:=false;
  end;

  function ImageToImage: Boolean;
  begin
    Result:=false;
  end;

  function ImageToDrawable: Boolean;
  begin
    Result:=false;
  end;

  function ImageToBitmap: Boolean;
  begin
    Result:=false;
  end;

  function PixmapToImage: Boolean;
  begin
    Result:=false;
  end;

  function PixmapToBitmap: Boolean;
  begin
    Result:=false;
  end;

  function BitmapToImage: Boolean;
  begin
    Result:=false;
  end;

  function BitmapToPixmap: Boolean;
  begin
    Result:=false;
  end;

  function Unsupported: Boolean;
  begin
    Result:=false;
  end;

  //----------
  function NoDrawableToNoDrawable: Boolean;
  const //            FROM            TO             
    BLT_MATRIX: array[TGDIBitmapType, TGDIBitmapType] of TBltFunction = (
      (@DrawableToDrawable, @BitmapToPixmap, @BitmapToImage),
      (@PixmapToBitmap, @DrawableToDrawable, @PixmapToImage),
      (@ImageToBitmap, @ImageToDrawable, @ImageToImage)
    );
  begin
    Result := BLT_MATRIX[
      PDeviceContext(SrcDC)^.CurrentBitmap^.GDIBitmapType,
      PDeviceContext(DestDC)^.CurrentBitmap^.GDIBitmapType
    ]();
  end;
   
  function NoDrawableToDrawable: Boolean;
  const
    BLT_FUNCTION: array[TGDIBitmapType] of TBltFunction = (
      @PixmapToDrawable, @PixmapToDrawable, @ImageToDrawable
    );
  begin
    Result := BLT_FUNCTION[PDeviceContext(SrcDC)^.CurrentBitmap^.GDIBitmapType]();
  end;

  function DrawableToNoDrawable: Boolean;
  const
    BLT_FUNCTION: array[TGDIBitmapType] of TBltFunction = (
      @Unsupported, @Unsupported, @Unsupported
    );
  begin
    Result := BLT_FUNCTION[PDeviceContext(DestDC)^.CurrentBitmap^.GDIBitmapType]();
  end;
  
const //                 FROM     TO             
  DRAWABLE_MATRIX: array[Boolean, Boolean] of TBltFunction = (
    (@NoDrawableToNoDrawable, @NoDrawableToDrawable),
    (@DrawableToNoDrawable, @DrawableToDrawable)
  );
begin
  Assert(False, Format('trace:  [TgtkObject.BitBlt] DestDC:0x%x; X:%d, Y:%d, Width:%d, Height:%d; SrcDC:0x%x; XSrc:%d, YSrc:%d, Rop:0x%x', [DestDC, X, Y, Width, Height, SrcDC, XSrc, YSrc, Rop]));
  Result := IsValidDC(DestDC) and IsValidDC(SrcDC);
  if Result
  then begin
    gdk_gc_set_function(PDeviceContext(DestDC)^.GC, GDK_COPY);
    // TODO: Add ROP
    
    
    // ----------------------------------
    // MWE: Temporary commented out due to compiler problems
    // The called functions can't access local vars outside 
    // themselves when they are called through a const or a var. 
    // Since only DrawableToDrawable is implemented, 
    // it is for the time beeing handled by an if statement
    // ----------------------------------
    (*
    
    Result := DRAWABLE_MATRIX[
      PDeviceContext(SrcDC)^.Drawable <> nil,
      PDeviceContext(DestDC)^.Drawable <> nil
    ](); 
    
    *)
    // ----------------------------------
    // MWE: Begin of temporary part
    // ----------------------------------
    if  (PDeviceContext(SrcDC)^.Drawable <> nil)
    and (PDeviceContext(DestDC)^.Drawable <> nil) 
    then Result := DrawableToDrawable
    else Result := False;
    // ----------------------------------
    // MWE: End of temporary part
    // ----------------------------------
  end;
end;

{------------------------------------------------------------------------------
  Function: CallNextHookEx
  Params:  none
  Returns: Nothing


 ------------------------------------------------------------------------------}
function TgtkObject.CallNextHookEx(hhk : HHOOK; ncode : Integer; wParam, lParam : Longint) : Integer;
begin
result := 0;
//TODO: Does anything need to be done here?
Assert(False, 'Trace:!!!!!!!!!!!!!!!!!!');
Assert(False, 'Trace:!!!!!!!!!!!!!!!!!!');
Assert(False, 'Trace:TODO: CALLNEXTHOOKEX in gtkwinapi.inc');
Assert(False, 'Trace:!!!!!!!!!!!!!!!!!!');
Assert(False, 'Trace:!!!!!!!!!!!!!!!!!!');

end;

{------------------------------------------------------------------------------
  Function: CallWindowProc
  Params: lpPrevWndFunc:
          Handle:
          Msg:
          wParam:
          lParam:
  Returns:

 ------------------------------------------------------------------------------}
Function TGTKObject.CallWindowProc(lpPrevWndFunc : TFarProc; Handle : HWND; Msg : UINT; wParam ,lParam : LongInt) : Integer;
var
  Proc : TWndMethod;
  Mess : TLMessage;
  P : Pointer;
begin
  Result := -1;
  if Handle = 0 then Exit;
        Result := -1;
        P := nil;
        P := gtk_object_get_data(pgtkobject(Handle),'WNDPROC');
        if P <> nil then Proc := TWndMethod(P^)
           else
            Exit;
        Mess.msg := msg;
        Mess.LParam := LParam;
        Mess.WParam := WParam;
        Proc(Mess);
        Result := Mess.Result;

end;

{------------------------------------------------------------------------------
  Function: ClientToScreen
  Params:  none
  Returns: Nothing
 ------------------------------------------------------------------------------}
Function TgtkObject.ClienttoScreen(Handle : HWND; var P : TPoint) : Boolean;
var
  X, Y: Integer;
  Widget: PGTKWidget;
Begin
  if Handle = 0
  then begin
    X := 0;
    Y := 0;
  end
  else begin
    Widget := GetFixedWidget(pgtkwidget(Handle));
    if Widget = nil then Widget := pgtkwidget(Handle); 
    gdk_window_get_origin(Widget^.Window, @X, @Y);
  end;

  // Todo: calculate offset, since platform specific
  Inc(P.X, X);  
  Inc(P.Y, Y); 
  
  Assert(False, Format('Trace:  [GTKObject.ClientToScreen] Handle: 0x%x --> (%d, %d)', [Integer(Handle), P.X, P.y]));
  Result := True;
  
end;


{------------------------------------------------------------------------------
  Function: CreateBitmap
  Params:  none
  Returns: Nothing


 ------------------------------------------------------------------------------}
function TgtkObject.CreateBitmap(Width, Height: Integer; Planes, BitCount: Longint; BitmapBits: Pointer): HBITMAP;
var
  GdiObject: PGdiObject;
  RawImage: PGDIRawImage;
begin
  Assert(False, Format('Trace:> [TgtkObject.CreateBitmap] Width: %d, Height: %d, Planes: %d, BitCount: %d, BitmapBits: 0x%x', [Width, Height, Planes, BitCount, Longint(BitmapBits)]));

  if (BitCount < 1) or (Bitcount > 32) 
  then begin
    Result := 0;
    WriteLn(Format('ERROR: [TgtkObject.CreateBitmap] Illegal depth %d', [BitCount]));
    Exit;
  end;
  
  GdiObject := NewGDIObject(gdiBitmap);
  
  // if the bitcount is the system depth create a Pixmap
  // if depth is 1 then a Bitmap
  // else an image
  
  if BitCount = gdk_visual_get_system^.Depth
  then begin
    Assert(False, Format('Trace:  [TgtkObject.CreateBitmap] gbPixmap', []));
    GdiObject^.GDIBitmapType := gbPixmap;
    GdiObject^.GDIPixmapObject := gdk_pixmap_new(nil, Width, Height, BitCount);
  end 
  else if Bitcount = 1 
  then begin
    Assert(False, Format('Trace:  [TgtkObject.CreateBitmap] gbBitmap', []));
    GdiObject^.GDIBitmapType := gbBitmap;
    GdiObject^.GDIBitmapObject := gdk_pixmap_new(nil, Width, Height, BitCount);
  end
  else begin
    Assert(False, Format('Trace:  [TgtkObject.CreateBitmap] gbImage', []));
    GdiObject^.GDIBitmapType := gbImage;
    GdiObject^.GDIRawImageObject := NewGDIRawImage(Width, Height, BitCount);
  end;
  
  Result := HBITMAP(GdiObject);
  Assert(False, Format('Trace:< [TgtkObject.CreateBitmap] --> 0x%x', [Integer(Result)]));
end;


{------------------------------------------------------------------------------
  Function:  CreateBrushIndirect
  Params:  none
  Returns: Nothing


 ------------------------------------------------------------------------------}
function TgtkObject.CreateBrushIndirect(const LogBrush: TLogBrush): HBRUSH;
const
  HATCH_NULL      : array[0..7] of Byte = ($00, $00, $00, $00, $00, $00, $00, $00);
  HATCH_BDIAGONAL : array[0..7] of Byte = ($80, $40, $20, $10, $08, $04, $02, $01);
  HATCH_CROSS     : array[0..7] of Byte = ($22, $22, $FF, $22, $22, $22, $FF, $22);
  HATCH_DIAGCROSS : array[0..7] of Byte = ($81, $42, $24, $18, $18, $24, $42, $81);
  HATCH_FDIAGONAL : array[0..7] of Byte = ($01, $02, $04, $08, $10, $20, $40, $80);
  HATCH_HORIZONTAL: array[0..7] of Byte = ($00, $00, $00, $00, $FF, $00, $00, $00);
  HATCH_VERTICAL  : array[0..7] of Byte = ($08, $08, $08, $08, $08, $08, $08, $08);
var
  GObject: PGdiObject;
  sError: String;
begin
  Assert(False, Format('Trace:> [TgtkObject.CreateBrushIndirect]  Style: %d, Color: %8x', [LogBrush.lbStyle, LogBrush.lbColor]));

  sError := '';
  GObject := NewGDIObject(gdiBrush);

  with LogBrush do
  begin
    case lbStyle of
 //     BS_HOLLOW,         // Hollow brush.
      BS_NULL:           // Same as BS_HOLLOW.
        begin
          GObject^.GDIBrushFill := GDK_STIPPLED;
          GObject^.GDIBrushPixmap := gdk_bitmap_create_from_data(nil, @HATCH_NULL, 8, 8);
        end;
      BS_SOLID:          // Solid brush.
        begin
          GObject^.GDIBrushFill := GDK_SOLID;
        end;
      BS_HATCHED:        // Hatched brush.
        begin
          GObject^.GDIBrushFill := GDK_STIPPLED;
          case lbHatch of
            HS_BDIAGONAL:  GObject^.GDIBrushPixmap := gdk_bitmap_create_from_data(nil, @HATCH_BDIAGONAL, 8, 8);
            HS_CROSS:      GObject^.GDIBrushPixmap := gdk_bitmap_create_from_data(nil, @HATCH_CROSS, 8, 8);
            HS_DIAGCROSS:  GObject^.GDIBrushPixmap := gdk_bitmap_create_from_data(nil, @HATCH_DIAGCROSS, 8, 8);
            HS_FDIAGONAL:  GObject^.GDIBrushPixmap := gdk_bitmap_create_from_data(nil, @HATCH_FDIAGONAL, 8, 8);
            HS_HORIZONTAL: GObject^.GDIBrushPixmap := gdk_bitmap_create_from_data(nil, @HATCH_HORIZONTAL, 8, 8);
            HS_VERTICAL:   GObject^.GDIBrushPixmap := gdk_bitmap_create_from_data(nil, @HATCH_VERTICAL, 8, 8);
            else
              sError := Format('WARNING: [TgtkObject.CreateBrushIndirect] Got unsupported Hatch %d', [lbHatch]);
          end;

        end;
      BS_DIBPATTERN,     // A pattern brush defined by a device-independent bitmap (DIB) specification. If lbStyle is BS_DIBPATTERN, the lbHatch member contains a handle to a packed DIB.Windows 95: Creating brushes from bitmaps or DIBs larger than 8x8 pixels is not supported. If a larger bitmap is given, only a portion of the bitmap is used.
      BS_DIBPATTERN8X8,  // Same as BS_DIBPATTERN.
      BS_DIBPATTERNPT,   // A pattern brush defined by a device-independent bitmap (DIB) specification. If lbStyle is BS_DIBPATTERNPT, the lbHatch member contains a pointer to a packed DIB.
      BS_PATTERN,        // Pattern brush defined by a memory bitmap.
      BS_PATTERN8X8:     // Same as BS_PATTERN.
        begin
          GObject^.GDIBrushFill := GDK_TILED;
          if IsValidGDIObject(lbHatch) and (PGdiObject(lbHatch)^.GDIType = gdiBitmap)
          then GObject^.GDIBrushPixmap := PGdiObject(lbHatch)^.GDIBitmapObject
          else sError := 'WARNING: [TgtkObject.CreateBrushIndirect] Got unsupported bitmap';
        end;
      else
        sError := Format('WARNING: [TgtkObject.CreateBrushIndirect] Got unsupported Style %d', [lbStyle]);
    end;

    with GObject^.GDIBrushColor do
    begin
       Red :=   ((lbColor shl 8) and $00FF00) or ((lbColor       ) and $0000FF);
       Green := ((lbColor      ) and $00FF00) or ((lbColor shr 8 ) and $0000FF);
       Blue :=  ((lbColor shr 8) and $00FF00) or ((lbColor shr 16) and $0000FF);
    end;
    gdk_colormap_alloc_color(gdk_colormap_get_system, @GObject^.GDIBrushColor, False, True);
    with GObject^.GDIBrushColor do
      Assert(False, Format('Trace:  [TgtkObject.CreateBrushIndirect]  Allocated R: %2x, G: %2x, B: %2x', [Red, Green, Blue]));
  end;
  if sError = ''
  then Result := HBRUSH(GObject)
  else begin
    Assert(False, 'Trace:' + sError);
    Result := 0;
    Dispose(GObject);
  end;
  Assert(False, Format('Trace:< [TgtkObject.CreateBrushIndirect] Got --> %x', [Result]));
end;

{------------------------------------------------------------------------------
  Function: CreateCaret
  Params:  none
  Returns: Nothing


 ------------------------------------------------------------------------------}
function TgtkObject.CreateCaret(Handle: HWND; Bitmap: hBitmap; Width, Height: Integer): Boolean;
var
  GTKObject: PGTKObject;
  BMP: PGDKPixmap;
begin
  Assert(False, 'Trace:TODO: [TgtkObject.CreateCaret] Finish');

  GTKObject := PGTKObject(Handle);
  Result := GTKObject <> nil;

  if Result
  then begin
    if gtk_type_is_a(gtk_object_type(GTKObject), GTKAPIWidget_GetType)
    then begin
      if IsValidGDIObjectType(Bitmap, gdiBitmap)
      then BMP := PGdiObject(Bitmap)^.GDIBitmapObject
      else BMP := nil;
      GTKAPIWidget_CreateCaret(PGTKAPIWidget(GTKObject), Width, Height, BMP);
    end
//    else if // TODO: other widgettypes
    else begin
      Result := False;
    end;
  end
  else Assert(False, 'Trace:WARNING: [TgtkObject.CreateCaret] Got null HWND');

end;

{------------------------------------------------------------------------------
  Function: CreateCompatibleBitmap
  Params: DC:
          Width:
          Height:
  Returns:

  Creates a bitmap compatible with the specified device context.
 ------------------------------------------------------------------------------}
function TGTKObject.CreateCompatibleBitmap(DC: HDC; Width, Height: Integer): HBITMAP;
var
  visual: PGDKVisual;
begin
  Assert(False, Format('Trace:> [TgtkObject.CreateCompatibleBitmap] DC: 0x%x, W: %d, H: %d', [DC, Width, Height]));
  
  if (IsValidDC(DC) and (PDeviceContext(DC)^.Drawable <> nil))
  then visual := gdk_window_get_visual(Pointer(PDeviceContext(DC)^.Drawable))
  else visual := gdk_visual_get_system;
  
  if Visual <> nil 
  then Result := CreateBitmap(Width, Height, 1, Visual^.Depth, nil)
  else Result := 0;
  
  Assert(False, Format('Trace:< [TgtkObject.CreateCompatibleBitmap] DC: 0x%x --> 0x%x', [DC, Result]));
end;

{------------------------------------------------------------------------------
  Function: CreateCompatibleDC
  Params:  none
  Returns: Nothing


 ------------------------------------------------------------------------------}
function TgtkObject.CreateCompatibleDC(DC: HDC): HDC;
var
  pNewDC: PDeviceContext;
begin
  Result := 0;
  pNewDC := NewDC;

  // dont copy
  // In a compatible DC you have to select a bitmap into it
(*
  if IsValidDC(DC)
  then with PDeviceContext(DC)^ do
  begin
    pNewDC^.hWnd := hWnd;
    pNewDC^.Drawable := Drawable;
    pNewDC^.GC := gdk_gc_new(Drawable);
  end
  else begin
    // We can't do anything yet
    // Wait till a bitmap get selected
  end;
*)

  // Maybe copy these ??
  pNewDC^.CurrentFont := CreateDefaultFont;
  pNewDC^.CurrentBrush := CreateDefaultBrush;
  pNewDC^.CurrentPen := CreateDefaultPen;

  Result := HDC(pNewDC);
  Assert(False,Format('trace:  [TgtkObject.CreateCompatibleDC] DC: 0x%x --> 0x%x', [Integer(DC), Integer(Result)]));
end;

{------------------------------------------------------------------------------
  Function: CreateFontIndirect
  Params:  none
  Returns: Nothing


 ------------------------------------------------------------------------------}
function TgtkObject.CreateFontIndirect(const LogFont: TLogFont): HFONT;
var
  GdiObject: PGdiObject;
  S: String;
  FontNameRegistry, Foundry, FamilyName, WeightName,
  Slant, SetwidthName, AddStyleName, PixelSize,
  PointSize, ResolutionX, ResolutionY, Spacing, AverageWidth,
  CharSetRegistry, CharSetCoding: string;
  n: Integer;

  procedure LoadFont;
  var
    pStr: PChar;
  begin
    S := Format('%s-%s-%s-%s-%s-%s-%s-%s-%s-%s-%s-%s-%s-%s-%s',
      [FontNameRegistry, Foundry, FamilyName, WeightName,
       Slant, SetwidthName, AddStyleName, PixelSize,
       PointSize, ResolutionX, ResolutionY, Spacing, AverageWidth,
       CharSetRegistry, CharSetCoding
      ]);

    GDIObject := NewGDIObject(gdiFont);
    pStr := StrAlloc(Length(S) + 1);
    try
      StrPCopy(pStr, S);
      GdiObject^.GDIFontObject := gdk_font_load(pStr);
    finally
      StrDispose(pStr);
    end;
  end;

begin
  // For info about xlfd see: http://wwwinfo.cern.ch/umtf/working-groups/X11/fonts/hp_xlfd.html
  // Lets fill in all the xlfd parts. Assume we have scalable fonts

  Result := 0;

  with LogFont do
  begin

    FontNameRegistry := '';
    Foundry := '*';

    if lfFaceName[0] = #0
    then begin
      Assert(false,'ERROR: [TgtkObject.CreateFontIndirect] No fontname');
      Exit;
    end;

    FamilyName := StrPas(lfFaceName); //StringReplace(FaceName, ' ', '*');

    Assert(False, Format('trace:  [TgtkObject.CreateFontIndirect] Name: %s, Height: %d', [FamilyName, lfHeight]));

    // calculate weight offset.
    //        API                 XLFD
    // --------------------- --------------
    // Weight=400 --> normal normal
    // Weight=700 --> bold   normal+4000 (or bold in non scalable fonts)
    //
    // So in API the offset for normal = 400 and an increase of 300 equals to
    // an offset of 4000
    case lfWeight of
      0: WeightName := '*';
      FW_NORMAL: WeightName := 'normal';
      FW_MEDIUM: WeightName := 'medium';
      FW_BOLD:   WeightName := 'bold';
      FW_BLACK:  WeightName := 'black';

      else begin
        n := ((lfWeight - FW_NORMAL) * 4000) div (FW_BOLD - FW_NORMAL);
        if n = 0
        then WeightName := 'normal'
        else if n > 0
        then WeightName := Format('normal+%d', [n])
        else WeightName := Format('normal%d', [n]);
      end;
    end;

    // TODO: find out if escapement has something to do with slant
    if lfItalic = 0 then Slant := 'r' else Slant := 'i';

    SetwidthName := '*';

    // calculate Style name extentions (=rotation)
    //        API                 XLFD
    // --------------------- --------------
    // Orientation 1/10 deg  1/64 deg
    if lfOrientation = 0
    then AddStyleName := '*'
    else begin
      n := (lfOrientation * 64) div 10;
      if n >= 0
      then AddStyleName := Format('+%d', [n])
      else AddStyleName := Format('+%d', [n]);
    end;

    // TODO: make more accurate (implement the meaning of
    //       positive and negative heigtht values.
    PixelSize := IntToStr(Abs(lfHeight));

    // Since we use pixelsize, it isn't allowed to give a value here
    PointSize := '*';

    // Use the default
    ResolutionX := '*';
    ResolutionY := '*';

    Spacing := '*';

    // calculate AverageWidth
    //        API                 XLFD
    // --------------------- --------------
    // Widht pixel           1/10 pixel
    if lfWidth = 0
    then AverageWidth := '*'
    else AverageWidth := InttoStr(lfWidth * 10);


    CharSetRegistry := '*';

    // TODO: Match charset.
    CharSetCoding := '*';
  end;

  LoadFont;
  if GdiObject^.GDIFontObject = nil
  then begin
    if (WeightName='normal') then begin
      WeightName:='medium';
      LoadFont;
    end else if (WeightName='bold') then begin
      WeightName:='black';
      LoadFont;
    end;
  end;

  if GdiObject^.GDIFontObject = nil
  then begin
    // try all weights
    WeightName := '*';
    LoadFont;
  end;

  if GdiObject^.GDIFontObject = nil
  then begin
    // try all weights
    WeightName := '*';
    LoadFont;
  end;

  if GdiObject^.GDIFontObject = nil
  then begin
    // try all slant
    Slant := '*';
    LoadFont;
  end;

  if GdiObject^.GDIFontObject = nil
  then begin
    // try all Familys
    FamilyName := '*';
    LoadFont;
  end;

  if GdiObject^.GDIFontObject = nil
  then begin
    // try all Foundrys
    Foundry := '*';
    LoadFont;
  end;

  if GdiObject^.GDIFontObject = nil
  then begin
    FGDIObjects.Remove(GdiObject);
    Dispose(GdiObject);
    Result := 0;
  end
  else begin
    GdiObject^.LogFont := LogFont;
    Result := HFONT(GdiObject);
  end;

  if Result = 0
  then WriteLn(Format('WARNING: [TgtkObject.CreateFontIndirect] NOT found XLFD: <%s>', [S]))
  else Assert(False, Format('Trace:  [TgtkObject.CreateFontIndirect] found XLFD: <%s>', [S]));
end;

{------------------------------------------------------------------------------
  Function: CreatePenIndirect
  Params:  none
  Returns: Nothing


 ------------------------------------------------------------------------------}
function TgtkObject.CreatePenIndirect(const LogPen: TLogPen): HPEN;
var
  GObject: PGdiObject;
begin
  Assert(False, 'trace:[TgtkObject.CreatePenIndirect]');
  GObject := NewGDIObject(gdiPen);

  with LogPen do
  begin
    GObject^.GDIPenStyle := lopnStyle;
    GObject^.GDIPenWidth := lopnWidth.X;

//    with GObject^.GDIPenColor do
//    begin
//      Red :=   ((lopnColor shl 8) and $00FF00) or ((lopnColor       ) and $0000FF);
//      Green := ((lopnColor      ) and $00FF00) or ((lopnColor shr 8 ) and $0000FF);
//      Blue :=  ((lopnColor shr 8) and $00FF00) or ((lopnColor shr 16) and $0000FF);
//    end;
//    gdk_colormap_alloc_color(gdk_colormap_get_system, @GObject^.GDIPenColor, False, True);
    GObject^.GDIPenColor := AllocGDKColor(lopnColor);
  end;

  Result := HPEN(GObject);
end;

{------------------------------------------------------------------------------
  Function: CreatePixmapIndirect
  Params:  Data: Raw pixmap data
  Returns: Handle to LCL bitmap

  Creates a bitmap from raw pixmap data.
 ------------------------------------------------------------------------------}
function TgtkObject.CreatePixmapIndirect(const Data: Pointer; const TransColor: Longint): HBITMAP;
var
  GdiObject: PGdiObject;
  GDKColor: TGDKCOlor;
  P: Pointer;
begin
  GdiObject := NewGDIObject(gdiBitmap); 
  if TransColor >= 0 
  then begin
    GDKColor := AllocGDKColor(TransColor);
    p := @GDKColor;
  end
  else p := nil;
  GdiObject^.GDIBitmapObject := gdk_pixmap_colormap_create_from_xpm_d(nil, gdk_colormap_get_system, @(GdiObject^.GDIBitmapMaskObject), p, data);
  Result := HBITMAP(GdiObject);
end;

{------------------------------------------------------------------------------
  Function: CreateRectRgn
  Params:  none
  Returns: Nothing


 ------------------------------------------------------------------------------}
function TgtkObject.CreateRectRgn(X1,Y1,X2,Y2 : Integer): HRGN;
Begin
//TODO: CREATERECTRGN in gtkwinapi.inc
Assert(False, 'Trace:!!!!!!!!!!!!!!!!!!');
Assert(False, 'Trace:!!!!!!!!!!!!!!!!!!');
Assert(False, 'Trace:TODO: CREATERECTRGN in gtkwinapi.inc');
Assert(False, 'Trace:!!!!!!!!!!!!!!!!!!');
Assert(False, 'Trace:!!!!!!!!!!!!!!!!!!');

result := -1;

end;

{------------------------------------------------------------------------------
  Function: DeleteDC
  Params:  none
  Returns: Nothing


 ------------------------------------------------------------------------------}
function TgtkObject.DeleteDC(hDC: HDC): Boolean;
begin
  // TODO:
  // for now it's just the same, however CreateDC/ReleaseDC
  // and GetDC/ReleaseDC are couples
  // we should use gdk_new_gc for create and gtk_new_gc for Get
  Result:= (ReleaseDC(0, hDC) = 1);
end;

{------------------------------------------------------------------------------
  Function: DeleteObject
  Params:  none
  Returns: Nothing


 ------------------------------------------------------------------------------}
function TgtkObject.DeleteObject(GDIObject: HGDIOBJ): Boolean;
begin
  { Find out if we want to release internal GDI object }
  Result:= IsValidGDIObject(GDIObject);
  if Result or (PGdiObject(GDIObject) <> nil) 
  then 
    with PGdiObject(GDIObject)^ do
    begin
      case GDIType of
        gdiFont:
          begin
            if Result then gdk_font_unref(GDIFontObject);
          end;
        gdiBrush:
          begin
            if Result and (GDIBrushPixmap <> nil) 
            then gdk_bitmap_unref(GDIBrushPixmap);
            
            gdk_colormap_free_colors(gdk_colormap_get_system, @GDIBrushColor, 1);
          end;
        gdiBitmap:
          begin
            if Result and (GDIBitmapObject <> nil)
            then gdk_bitmap_unref(GDIBitmapObject);
          end;
        gdiPen:
          begin
            gdk_colormap_free_colors(gdk_colormap_get_system, @GDIPenColor, 1);
          end;
        else begin
          Result:= false;
          Assert(False, 'Trace:TODO : Unimplemented GDI object in delete object');
          Exit;
        end;
      end;
    end;
    
  { Dispose of the GDI object }
  if PGDIObject(GDIObject) <> nil
  then begin
    FGDIObjects.Remove(PGDIObject(GDIObject));
    Dispose(PGDIObject(GDIObject));
  end;
end;

{------------------------------------------------------------------------------
  Function: DestroyCaret
  Params:  none
  Returns: Nothing


 ------------------------------------------------------------------------------}
function TgtkObject.DestroyCaret: Boolean;
Begin
  Assert(False, 'Trace:TODO: [TgtkObject.DestroyCaret]');
  //TODO: Implement this;
  Result := False;
end;

{------------------------------------------------------------------------------
  Function: DrawFrameControl
  Params:
  Returns:


 ------------------------------------------------------------------------------}
function TgtkObject.DrawFrameControl(DC: HDC; var Rect : TRect; uType, uState : Cardinal) : Boolean;
const
  ADJUST_FLAG: array[Boolean] of Integer = (0, BF_ADJUST);
  PUSH_EDGE_FLAG: array[Boolean] of Integer = (EDGE_RAISED, EDGE_SUNKEN);
  PUSH_EDGE_FLAG2: array[Boolean] of Integer = (0, BF_FLAT);
begin
  case uType of
    DFC_CAPTION:
      begin  //all draw CAPTION commands here
      end;
    DFC_MENU:
      begin
      end;
    DFC_SCROLL:
      begin
      end;
    DFC_BUTTON:
      begin
        Assert(False, Format('Trace:  [TgtkObject.DrawFrameControl] DFC_BUTTON --> draw rect = %d,%d,%d,%d',[Rect.Left,Rect.Top,REct.Right,REct.Bottom]));
        //figure out the style first
        case uState and $1F of
          DFCS_BUTTONRADIOIMAGE:
            begin
              Assert(False, 'Trace:State ButtonRadioImage');
            end;
          DFCS_BUTTONRADIOMASK:
            begin
              Assert(False, 'Trace:State ButtonRadioMask');
            end;
          DFCS_BUTTONRADIO:
            begin
              Assert(False, 'Trace:State ButtonRadio');
            end;
          DFCS_BUTTON3STATE:
            begin
              Assert(False, 'Trace:State Button3State');
            end;
          DFCS_BUTTONPUSH:
            begin
              Assert(False, 'Trace:DFCS_BUTTONPUSH in uState');
              Result := DrawEdge(DC, Rect, PUSH_EDGE_FLAG[(uState and DFCS_PUSHED) <> 0], BF_RECT or ADJUST_FLAG[(uState and DFCS_ADJUSTRECT) <> 0]);
            end;
          DFCS_BUTTONCHECK:
            begin
              Assert(False, 'Trace:State ButtonCheck');
              Result := DrawEdge(DC, Rect, PUSH_EDGE_FLAG2[(uState and DFCS_FLAT) <> 0], BF_RECT or ADJUST_FLAG[(uState and DFCS_ADJUSTRECT) <> 0]);
              if (uState and DFCS_CHECKED) <> 0 then
                 Begin
                 //TODO:write the code to draw a check inside the box defined by Rect

                 end;
            end;
        else  
          WriteLn(Format('ERROR: [TgtkObject.DrawFrameControl] Unknown State 0x%x', [uState]));
        end;
      end;
  else  
    WriteLn(Format('ERROR: [TgtkObject.DrawFrameControl] Unknown type %d', [uType]));
  end;
end;

{------------------------------------------------------------------------------
  Function: DrawEdge
  Params:
  Returns:

  Draws one or more edges of a rectangle, not including the 
  right and bottom edge.
 ------------------------------------------------------------------------------}
function TgtkObject.DrawEdge(DC: HDC; var Rect: TRect; edge: Cardinal; grfFlags: Cardinal): Boolean;
Var
  InnerTL, OuterTL,
  InnerBR, OuterBR: TGDKColor;
  BInner, BOuter: Boolean;
  Width, Height: Integer;
  R: TRect;
begin
  Assert(False, Format('trace:> [TgtkObject.DrawEdge] DC:0x%x, Rect = %d,%d,%d,%d', [DC, Rect.Left, Rect.Top,Rect.Right, Rect.Bottom]));
  Result := IsValidDC(DC);
  if Result
  then with PDeviceContext(DC)^ do
  begin
    if GC = nil
    then begin
      Assert(False, 'Trace:[TgtkObject.DrawEdge] Uninitialized GC');
      Result := False;
    end
    else begin
      R := Rect;
      Dec(R.Right);
      Dec(R.Bottom);
      
      BInner := False;
      BOuter := False;

      // TODO: changeThis to real colors
      if (edge and BDR_RAISEDINNER) = BDR_RAISEDINNER
      then begin
        InnerTL := AllocGDKColor(GetSysColor(COLOR_BTNHIGHLIGHT));
        InnerBR := AllocGDKColor(GetSysColor(COLOR_BTNSHADOW));
//        gdk_color_white(gdk_colormap_get_system, @InnerTL);
//        gdk_color_black(gdk_colormap_get_system, @InnerBR);
        BInner := True;
      end;
      if (edge and BDR_SUNKENINNER) = BDR_SUNKENINNER
      then begin
        InnerTL := AllocGDKColor(GetSysColor(COLOR_BTNSHADOW));
        InnerBR := AllocGDKColor(GetSysColor(COLOR_BTNHIGHLIGHT));
//        gdk_color_black(gdk_colormap_get_system, @InnerTL);
//        gdk_color_white(gdk_colormap_get_system, @InnerBR);
        BInner := True;
      end;
      if (edge and BDR_RAISEDOUTER) = BDR_RAISEDOUTER
      then begin
        OuterTL := AllocGDKColor(GetSysColor(COLOR_BTNFACE));
        OuterBR := AllocGDKColor(GetSysColor(COLOR_WINDOWFRAME));
//        gdk_color_white(gdk_colormap_get_system, @OuterTL);
//        gdk_color_black(gdk_colormap_get_system, @OuterBR);
        BOuter := True;
      end;
      if (edge and BDR_SUNKENOUTER) = BDR_SUNKENOUTER
      then begin
        OuterTL := AllocGDKColor(GetSysColor(COLOR_WINDOWFRAME));
        OuterBR := AllocGDKColor(GetSysColor(COLOR_BTNFACE));
//        gdk_color_black(gdk_colormap_get_system, @OuterTL);
//        gdk_color_white(gdk_colormap_get_system, @OuterBR);
        BOuter := True;
      end;

      gdk_gc_set_fill(GC, GDK_SOLID);

      // Draw outer rect
      if Bouter
      then with R do
      begin
        gdk_gc_set_foreground(GC, @OuterTL);
        if (grfFlags and BF_TOP) = BF_TOP
        then gdk_draw_line(Drawable, GC, Left, Top, Right, Top);
        if (grfFlags and BF_LEFT) = BF_LEFT
        then gdk_draw_line(Drawable, GC, Left, Top, Left, Bottom);

        gdk_gc_set_foreground(GC, @OuterBR);
        if (grfFlags and BF_BOTTOM) = BF_BOTTOM
        then gdk_draw_line(Drawable, GC, Left, Bottom, Right + 1, Bottom);
        if (grfFlags and BF_RIGHT) = BF_RIGHT
        then gdk_draw_line(Drawable, GC, Right, Top, Right, Bottom + 1);

        InflateRect(R, -1, -1);
      end;

      // Draw inner rect
      if BInner
      then with R do
      begin

        gdk_gc_set_foreground(GC, @InnerTL);
        if (grfFlags and BF_TOP) = BF_TOP
        then gdk_draw_line(Drawable, GC, Left, Top, Right, Top);
        if (grfFlags and BF_LEFT) = BF_LEFT
        then gdk_draw_line(Drawable, GC, Left, Top, Left, Bottom);

        gdk_gc_set_foreground(GC, @InnerBR);
        if (grfFlags and BF_BOTTOM) = BF_BOTTOM
        then gdk_draw_line(Drawable, GC, Left, Bottom, Right + 1, Bottom);
        if (grfFlags and BF_RIGHT) = BF_RIGHT
        then gdk_draw_line(Drawable, GC, Right, Top, Right, Bottom + 1);
  
        InflateRect(R, -1, -1);
      end;

//      gdk_colormap_free_colors(gdk_colormap_get_system, @OuterTL, 1);
//      gdk_colormap_free_colors(gdk_colormap_get_system, @OuterBR, 1);
//      gdk_colormap_free_colors(gdk_colormap_get_system, @InnerTL, 1);
//      gdk_colormap_free_colors(gdk_colormap_get_system, @InnerBR, 1);
      
      //Draw interiour
      if (grfFlags and BF_MIDDLE) = BF_MIDDLE
      then begin
        Width := R.Right - R.Left + 1;
        Height := R.Bottom - R.Top + 1;
        SelectGDKBrushProps(DC);
        gdk_draw_rectangle(Drawable, GC, 1, R.Left, R.Top, Width, Height);
      end;

      // adjust rect if needed
      if (grfFlags and BF_ADJUST) = BF_ADJUST
      then Rect := R;

      Result := True;
    end;
  end;
  Assert(False, Format('trace:< [TgtkObject.DrawEdge] DC:0x%x, Rect = %d,%d,%d,%d', [DC, Rect.Left, Rect.Top,Rect.Right, Rect.Bottom]));
end;

{------------------------------------------------------------------------------
  Function: EmptyClipBoard
  Params: none
  Returns:

 ------------------------------------------------------------------------------}
Function TGTKObject.EmptyClipBoard : Boolean;
begin
  // Your code here
  Result:=false;
end;

{------------------------------------------------------------------------------
  Function: EnableMenuItem
  Params: hMenu:
          uIDEnableItem:
  Returns:

 ------------------------------------------------------------------------------}
function TGTKObject.EnableMenuItem(hMenu: HMENU; uIDEnableItem: Integer;
  bEnable: Boolean): Boolean;
begin
  // Your code here
  Result:=false;
end;

{------------------------------------------------------------------------------
  Function: EnableScrollBar
  Params:  Wnd, wSBflags, wArrows
  Returns: Nothing


 ------------------------------------------------------------------------------}
function TgtkObject.EnableScrollBar(Wnd: HWND; wSBflags, wArrows: Cardinal): Boolean;
begin
  Assert(False, 'Trace:TODO: [TgtkObject.EnableScrollBar]');
  //TODO: Implement this;
  Result := False;
end;

{------------------------------------------------------------------------------
  Function: EnableWindow
  Params: hWnd:
          bEnable:
  Returns:

 ------------------------------------------------------------------------------}
function TGTKObject.EnableWindow(hWnd: HWND; bEnable: Boolean): Boolean;
begin
  Assert(False, Format('Trace:  [TGTKObject.EnableWindow] hWnd: 0x%x, Enable: %s', [hwnd, BOOL_TEXT[bEnable]]));
  if hWnd <> 0 
  then gtk_widget_set_sensitive(pgtkwidget(hWnd), bEnable);
  Result:=false;
end;

{------------------------------------------------------------------------------
  Function: ExtTextOut
  Params:  none
  Returns: Nothing


 ------------------------------------------------------------------------------}
function TgtkObject.ExtTextOut(DC: HDC; X, Y: Integer; Options: Longint; Rect: PRect; Str: PChar; Count: Longint; Dx: PInteger): Boolean;
var
  pStr: PChar;
  Width, Height: Integer;
begin
  Assert(False, Format('trace:> [TgtkObject.ExtTextOut] DC:0x%x, X:%d, Y:%d, Options:%d, Str:''%s'', Count: %d', [DC, X, Y, Options, Str, Count]));
  Result := IsValidDC(DC);
  if Result
  then with PDeviceContext(DC)^ do
  begin
    if GC = nil
    then begin
      WriteLn('WARNING: [TgtkObject.ExtTextOut] Uninitialized GC');
      Result := False;
    end
    else if (CurrentFont = nil) or (CurrentFont^.GDIFontObject = nil)
    then begin
      WriteLn('WARNING: [TgtkObject.ExtTextOut] Missing font');
      Result := False;
    end
    else begin
      // TODO: implement other parameters.
      pStr := StrAlloc(Count + 1);
      try
        StrLCopy(pStr, Str, Count);
        pStr[Count] := #0;
        if (Options and ETO_OPAQUE) <> 0 then
        begin
          Width := Rect^.Right - Rect^.Left;
          Height := Rect^.Bottom - Rect^.Top;
          //SelectGDKBrushProps(DC);
          gdk_gc_set_fill(GC, GDK_SOLID);
          gdk_gc_set_foreground(GC, @CurrentBackColor);
          gdk_draw_rectangle(Drawable, GC, 1, Rect^.Left, Rect^.Top, Width, Height);
        end;
        if (Options and ETO_CLIPPED) <> 0 then
        begin
          X := Rect^.Left;
          Y := Rect^.Top;
        end;
        SelectGDKTextProps(DC);
        gdk_draw_text(Drawable, CurrentFont^.GDIFontObject, GC,
            X, Y + 10 {TODO: query font height}, pStr, Count);
      finally
        StrDispose(pStr);
      end;
    end;
  end;
  Assert(False, Format('trace:< [TgtkObject.ExtTextOut] DC:0x%x, X:%d, Y:%d, Options:%d, Str:''%s'', Count: %d', [DC, X, Y, Options, Str, Count]));
end;

{------------------------------------------------------------------------------
  Function: FillRect
  Params:  none
  Returns: Nothing

  The FillRect function fills a rectangle by using the specified brush.
  This function includes the left and top borders, but excludes the right and
  bottom borders of the rectangle.
 ------------------------------------------------------------------------------}
function TgtkObject.FillRect(DC: HDC; const Rect: TRect; Brush: HBRUSH): Boolean;
var
  Width, Height: Integer;
  OldCurrentBrush: PGdiObject;
begin
  Assert(False, Format('trace:> [TgtkObject.FillRect] DC:0x%x; Rect: ((%d,%d)(%d,%d)); brush: %x', [Integer(DC), Rect.left, rect.top, rect.right, rect.bottom, brush]));
  Result := IsValidDC(DC) and IsValidGDIObject(Brush);
  if Result
  then with PDeviceContext(DC)^ do
  begin
    if GC = nil
    then begin
      WriteLn('WARNING: [TgtkObject.FillRect] Uninitialized GC');
      Result := False;
    end
    else begin
      Width := Rect.Right - Rect.Left;
      Height := Rect.Bottom - Rect.Top;
      // Temporary hold the old brush to 
      // replace it with the given brush
      OldCurrentBrush := CurrentBrush;
      CurrentBrush := PGdiObject(Brush);
      SelectGDKBrushProps(DC);
      gdk_draw_rectangle(Drawable, GC, 1, Rect.Left, Rect.Top, Width, Height);
      // Restore current brush
      CurrentBrush := OldCurrentBrush;

      Result := True;
    end;
  end;
  Assert(False, Format('trace:< [TgtkObject.FillRect] DC:0x%x; Rect: ((%d,%d)(%d,%d)); brush: %x', [Integer(DC), Rect.left, rect.top, rect.right, rect.bottom, brush]));
end;

{------------------------------------------------------------------------------
  Function: GetActiveWindow
  Params: none
  Returns:

 ------------------------------------------------------------------------------}
Function TGTKObject.GetActiveWindow : HWND;
begin
  // ToDo
//  Result := gdk_Window_Get_Toplevel;
  Result:=0;
end;

{------------------------------------------------------------------------------
  Function: GetCapture
  Params:  none
  Returns: Nothing


 ------------------------------------------------------------------------------}
function TgtkObject.GetCapture: HWND;
begin
  Result := MCaptureHandle;
end;

{------------------------------------------------------------------------------
  Function: GetCaretPos
  Params:  none
  Returns: Nothing


 ------------------------------------------------------------------------------}
function TgtkObject.GetCaretPos(var lpPoint: TPoint): Boolean;
var
  FocusObject: PGTKObject;
  modmask : TGDKModifierType;
begin
{  Assert(False, 'Trace:TODO: [TgtkObject.GetCaretPos] finish');

  FocusObject := PGTKObject(GetFocus);
  Result := FocusObject <> nil;

  if Result
  then begin
//    Assert(False, Format('Trace:[TgtkObject.GetCaretPos] Got focusObject 0x%x --> %s', [Integer(FocusObject), gtk_type_name(FocusObject^.Klass^.theType)]));

    if gtk_type_is_a(gtk_object_type(FocusObject), GTKAPIWidget_GetType)
    then begin
      GTKAPIWidget_GetCaretPos(PGTKAPIWidget(FocusObject), lpPoint.X, lpPoint.Y);
    end
//    else if // TODO: other widgettypes
    else begin
      Result := False;
    end;
  end
  else WriteLn('[TgtkObject.GetCaretPos] got focusObject nil');
 }

Assert(False, 'Trace:GetCaretPos');
gdk_window_get_pointer(nil,@lpPoint.X,@lpPoint.y,@modmask);
Assert(False, 'Trace:GetCaretPos');
Result := True;
end;

{------------------------------------------------------------------------------
  Function: GetCharABCWidths        pbd
  Params:  Don't care yet
  Returns: False so that the font cache in the newest mwEdit will use
           TextMetrics info which is working already


 ------------------------------------------------------------------------------}
function TgtkObject.GetCharABCWidths(DC: HDC; p2, p3: UINT; const ABCStructs): Boolean;
begin
  Result := False;
end;

{------------------------------------------------------------------------------
  Function: GetClientRect
  Params: handle:
          Result:
  Returns:

 ------------------------------------------------------------------------------}
Function TGTKObject.GetClientRect(handle : HWND; var Rect : TRect) : Boolean;
var
  requisition : TgtkRequisition;
begin
  result := False;
  if Handle = 0 then Exit;
  Result := False;
  Try
   Rect.Left := 0;
   Rect.Top := 0;
   gtk_Widget_size_request(PgtkWidget(handle),@requisition);
   Rect.Right := requisition.width;
   Rect.Bottom := requisition.Height;
//   Writeln('Width / Height = '+Inttostr(REct.Right)+'/'+Inttostr(Rect.Bottom));
  except
   Result := False;
  end;

end;

{------------------------------------------------------------------------------
  Function: GetDC
  Params:  none
  Returns: Nothing


 ------------------------------------------------------------------------------}
function TgtkObject.GetDC(hWnd: HWND): HDC;
var
  p: PDeviceContext;
  pFixed: PGTKFixed;
  GdiObject: PGdiObject;
  Values: TGdkGCValues;
  Color: TGdkColor;
  nIndex: Integer;
begin
  Assert(False, Format('trace:> [TgtkObject.GetDC] hWND: 0x%x', [hWnd]));
  p := nil;

  if hWnd = 0
  then begin
    P := NewDC;
    p^.hWnd := hWnd;
    FillChar(Values, SizeOf(Values), #0);
  end
  else begin
    pFixed := GetFixedWidget(Pointer(hWnd));
    if pFixed = nil
    then begin
      Assert(False, 'trace:WARNING: [TgtkObject.GetDC] Window has no fixed, using window itself');
      pFixed := Pointer(hWnd);
    end;

    // create a new devicecontext for this window
    P := NewDC;
    p^.hWnd := hWnd;
//(*      
    if PGTKFixed(pFixed)^.Container.Widget.Window = nil
    then begin
      Assert(False, 'Trace:[TgtkObject.GetDC] Force widget creation');
      //force creation
      gtk_widget_realize(PGTKWidget(pFixed));
    end;
//*)
    p^.Drawable := PGTKFixed(pFixed)^.Container.Widget.Window;
    p^.GC := gdk_gc_new(p^.Drawable);

    gdk_gc_set_function(p^.GC, GDK_COPY);

    gdk_gc_get_values(p^.GC, @Values);
  end;

  if p <> nil
  then begin
    if Values.Font <> nil
    then begin
      New(GdiObject);
      GdiObject^.GDIType := gdiFont;
      GdiObject^.GDIFontObject := Values.Font;
      gdk_font_ref(Values.Font);
    end
    else GdiObject := CreateDefaultFont;

    p^.CurrentFont := GdiObject;
    p^.CurrentBrush := CreateDefaultBrush;
    p^.CurrentPen := CreateDefaultPen;
  end;
  Result := HDC(p);
  Assert(False, Format('trace:< [TgtkObject.GetDC] Got 0x%x', [Result]));
end;

{------------------------------------------------------------------------------
  Function: GetFocus
  Params:  none
  Returns: The handle of the window with focus

  The GetFocus function retrieves the handle of the window that has the focus.
 ------------------------------------------------------------------------------}
function TgtkObject.GetFocus: HWND;
var
  List: PGList;
  Widget: PGTKWidget;
  Window: PGTKWindow;
begin
  List := gdk_window_get_toplevels;

  while List <> nil do
  begin
    if (List^.Data <> nil)
    then begin
      gdk_window_get_user_data(PGDKWindow(List^.Data), @Window);
      if gtk_is_window(Window)
      then begin
        Widget := Window^.focus_widget;

        if (Widget <> nil) and gtk_widget_has_focus(Widget)
        then begin
          Result := HWND(GetMainWidget(Widget));
          Exit;
        end;
      end;
    end;
    list := g_list_next(list);
  end;

  // If we are here we didn't find anything
  Result := 0;
end;

{------------------------------------------------------------------------------
  Function: GetKeyState
  Params:  nVirtKey: The requested key
  Returns: If the function succeeds, the return value specifies the status of
           the given virtual key. If the high-order bit is 1, the key is down;
           otherwise, it is up. If the low-order bit is 1, the key is toggled.

  The GetKeyState function retrieves the status of the specified virtual key.
 ------------------------------------------------------------------------------}
function TgtkObject.GetKeyState(nVirtKey: Integer): Smallint;
const
  KEYSTATE: array[Boolean] of Smallint = (0, -32768 { $8000});
  TOGGLESTATE: array[Boolean] of Smallint = (0, 1);
begin
  case nVirtKey of
    VK_LSHIFT:   nVirtKey := VK_SHIFT;
    VK_LCONTROL: nVirtKey := VK_CONTROL;
    VK_LMENU:    nVirtKey := VK_MENU;
  end;
  Result := KEYSTATE[FKeyStateList.IndexOf(Pointer(nVirtKey)) <> -1];

  // try extended keys
  if Result = 0 
  then begin
    nVirtKey := nVirtKey or KEYMAP_EXTENDED;
    Result := KEYSTATE[FKeyStateList.IndexOf(Pointer(nVirtKey)) <> -1];
  end;
  
  // add toggle
  if Result <> 0 
  then Result := Result or TOGGLESTATE[FKeyStateList.IndexOf(Pointer(nVirtKey or KEYMAP_TOGGLE)) <> -1];

  //Assert(False, Format('Trace:[TgtkObject.GetKeyState] %d -> 0x%x', [nVirtKey, Result]));
end;

{------------------------------------------------------------------------------
  Function: GetObject
  Params:  none
  Returns: Nothing


 ------------------------------------------------------------------------------}
function TgtkObject.GetObject(GDIObj: HGDIOBJ; BufSize: Integer; Buf: Pointer): Integer;
begin
  Assert(False, 'trace:[TgtkObject.GetObject]');
  Result := 0;
  if IsValidGDIObject(GDIObj)
  then begin
    case PGDIObject(GDIObj)^.GDIType of
      gdiBitmap:
        begin
          Assert(False, 'Trace:TODO: [TgtkObject.GetObject] gdiBitmap');
        end;
      gdiBrush:
        begin
          Assert(False, 'Trace:TODO: [TgtkObject.GetObject] gdiBrush');
        end;
      gdiFont:
        begin
          if Buf = nil then Result := SizeOf(PGDIObject(GDIObj)^.LogFont)
          else begin
            if BufSize >= SizeOf(PGDIObject(GDIObj)^.LogFont)
            then begin
              PLogfont(Buf)^ := PGDIObject(GDIObj)^.LogFont;
              Result:= SizeOf(TLogFont);
            end;
          end;
        end;
      gdiPen:
        begin
          Assert(False, 'Trace:TODO: [TgtkObject.GetObject] gdiPen');
        end;
      gdiRegion:
        begin
          Assert(False, 'Trace:TODO: [TgtkObject.GetObject] gdiRegion');
        end;
    else
      WriteLn(Format('WARNING: [TgtkObject.GetObject] Unknown type %d', [Integer(PGDIObject(GDIObj)^.GDIType)]));
    end;
  end;
end;

{------------------------------------------------------------------------------
  Function: GetParent
  Params: Handle:
  Returns:

 ------------------------------------------------------------------------------}
Function TGTKObject.GetParent(Handle : HWND): HWND;
var
p : pgtkwidget;
begin
p := (pgtkWidget(Handle)^.parent);
   result := longint(p);
end;


{------------------------------------------------------------------------------
  Function: GetProp
  Params: Handle: Str
  Returns: Pointer

 ------------------------------------------------------------------------------}
Function TgtkObject.GetProp(Handle : hwnd; Str : PChar): Pointer;
Begin
result := gtk_object_get_data(pgtkobject(Handle),Str);
end;


{------------------------------------------------------------------------------
  Function: GetScrollInfo
  Params:  Handle, BarFlag, ScrollInfo
  Returns: Nothing


 ------------------------------------------------------------------------------}
function TgtkObject.GetScrollInfo(Handle: HWND; BarFlag: Integer; var ScrollInfo: TScrollInfo): Boolean;
begin
  Assert(False, 'Trace:TODO: [TgtkObject.GetScrollInfo]');
  Result := False;
end;

{------------------------------------------------------------------------------
  Function: GetStockObject
  Params:
  Returns: Nothing


 ------------------------------------------------------------------------------}
function TgtkObject.GetStockObject(Value: Integer): LongInt;
begin
  Assert(False, Format('Trace:> [TgtkObject.GetStockObject] %d', [Value]));
  Result := 0;
  case Value of
    BLACK_BRUSH:         // Black brush.
      Result := FStockBlackBrush;
    DKGRAY_BRUSH:        // Dark gray brush.
      Result := FStockDKGrayBrush;
    GRAY_BRUSH:          // Gray brush.
      Result := FStockGrayBrush;
    LTGRAY_BRUSH:        // Light gray brush.
      Result := FStockLtGrayBrush;
    NULL_BRUSH:          // Null brush (equivalent to HOLLOW_BRUSH).
      Result := FStockNullBrush;
    WHITE_BRUSH:         // White brush.
      Result := FStockWhiteBrush;
(*
    BLACK_PEN:           // Black pen.
      begin
      end;  
    NULL_PEN:            // Null pen.
      begin
      end;  
    WHITE_PEN:           // White pen.
      begin
      end;  
    ANSI_FIXED_FONT:     // Fixed-pitch (monospace) system font.
      begin
      end;  
    ANSI_VAR_FONT:       // Variable-pitch (proportional space) system font.
      begin
      end;  
    DEVICE_DEFAULT_FONT: // Device-dependent font.
      begin
      end;  
    DEFAULT_GUI_FONT:    // Default font for user interface objects such as menus and dialog boxes.
      begin
      end;  
    OEM_FIXED_FONT:      // Original equipment manufacturer (OEM) dependent fixed-pitch (monospace) font.
      begin
      end;  
    SYSTEM_FONT:         // System font. By default, Windows uses the system font to draw menus, dialog box controls, and text. In Windows versions 3.0 and later, the system font is a proportionally spaced font; earlier versions of Windows used a monospace system font.
      begin
      end;  
    SYSTEM_FIXED_FONT:   // Fixed-pitch (monospace) system font used in Windows versions earlier than 3.0. This stock object is provided for compatibility with earlier versions of Windows.
      begin
      end;  
    DEFAULT_PALETTE:     // Default palette. This palette consists of the static colors in the system palette.
      begin
      end;  
*)
  else
    Assert(False, Format('Trace:TODO: [TgtkObject.GetStockObject] Implement value: %d', [Value]));
  end;
  Assert(False, Format('Trace:< [TgtkObject.GetStockObject] %d --> 0x%x', [Value, Result]));
end;

{------------------------------------------------------------------------------
  Function: GetSysColor
  Params:   index to the syscolors array
  Returns:  RGB value

 ------------------------------------------------------------------------------}

function TgtkObject.GetSysColor(nIndex: Integer): DWORD;
begin
  if (nIndex < 0) or (nIndex > MAX_SYS_COLORS) 
  then begin
    Result := 0;
    // raise an exception
    WriteLn(Format('ERROR: [TgtkObject.GetSysColor] Bad Value: %8x Valid Range between 0 and %d', [nIndex, MAX_SYS_COLORS]));
  end
  else Result := SysColorMap[nIndex];
  //Assert(False, Format('Trace:[TgtkObject.GetSysColor] Index %d --> %8x', [nIndex, Result]));
end;

{------------------------------------------------------------------------------
  Function: GetSystemMetrics
  Params:
  Returns: Nothing


 ------------------------------------------------------------------------------}
function TgtkObject.GetSystemMetrics(nIndex: Integer): Integer;
begin
  Assert(False, Format('Trace:> [TgtkObject.GetSystemMetrics] %d', [nIndex]));
  case nIndex of
    SM_ARRANGE:
      begin
        Assert(False, 'Trace:TODO: [TgtkObject.GetSystemMetrics] --> SM_ARRANGE          ');
      end;
    SM_CLEANBOOT:
      begin
        Assert(False, 'Trace:TODO: [TgtkObject.GetSystemMetrics] --> SM_CLEANBOOT        ');
      end;
    SM_CMOUSEBUTTONS:
      begin
        Assert(False, 'Trace:TODO: [TgtkObject.GetSystemMetrics] --> SM_CMOUSEBUTTONS    ');
      end;
    SM_CXBORDER:
      begin
        Assert(False, 'Trace:TODO: [TgtkObject.GetSystemMetrics] --> SM_CXBORDER         ');
      end;
    SM_CYBORDER:
      begin
        Assert(False, 'Trace:TODO: [TgtkObject.GetSystemMetrics] --> SM_CYBORDER         ');
      end;
    SM_CXCURSOR:
      begin
        Assert(False, 'Trace:TODO: [TgtkObject.GetSystemMetrics] --> SM_CXCURSOR         ');
      end;
    SM_CYCURSOR:
      begin
        Assert(False, 'Trace:TODO: [TgtkObject.GetSystemMetrics] --> SM_CYCURSOR         ');
      end;
    SM_CXDOUBLECLK:
      begin
        Assert(False, 'Trace:TODO: [TgtkObject.GetSystemMetrics] --> SM_CXDOUBLECLK      ');
      end;
    SM_CYDOUBLECLK:
      begin
        Assert(False, 'Trace:TODO: [TgtkObject.GetSystemMetrics] --> SM_CYDOUBLECLK      ');
      end;
    SM_CXDRAG:
      begin
        Result := 2;
      end;
    SM_CYDRAG:
      begin
        Result := 2;
      end;
    SM_CXEDGE:
      begin
        Assert(False, 'Trace:TODO: [TgtkObject.GetSystemMetrics] --> SM_CXEDGE           ');
      end;
    SM_CYEDGE:
      begin
        Assert(False, 'Trace:TODO: [TgtkObject.GetSystemMetrics] --> SM_CYEDGE           ');
      end;
    SM_CXFIXEDFRAME:
      begin
        Assert(False, 'Trace:TODO: [TgtkObject.GetSystemMetrics] --> SM_CXFIXEDFRAME     ');
      end;
    SM_CYFIXEDFRAME:
      begin
        Assert(False, 'Trace:TODO: [TgtkObject.GetSystemMetrics] --> SM_CYFIXEDFRAME     ');
      end;
    SM_CXFULLSCREEN:
      begin
        Assert(False, 'Trace:TODO: [TgtkObject.GetSystemMetrics] --> SM_CXFULLSCREEN     ');
      end;
    SM_CYFULLSCREEN:
      begin
        Assert(False, 'Trace:TODO: [TgtkObject.GetSystemMetrics] --> SM_CYFULLSCREEN     ');
      end;
    SM_CXHSCROLL:
      begin
        Assert(False, 'Trace:TODO: [TgtkObject.GetSystemMetrics] --> SM_CXHSCROLL        ');
      end;
    SM_CYHSCROLL:
      begin
        Assert(False, 'Trace:TODO: [TgtkObject.GetSystemMetrics] --> SM_CYHSCROLL        ');
      end;
    SM_CXHTHUMB:
      begin
        Assert(False, 'Trace:TODO: [TgtkObject.GetSystemMetrics] --> SM_CXHTHUMB         ');
      end;
    SM_CXICON:
      begin
        Assert(False, 'Trace:TODO: [TgtkObject.GetSystemMetrics] --> SM_CXICON           ');
      end;
    SM_CYICON:
      begin
        Assert(False, 'Trace:TODO: [TgtkObject.GetSystemMetrics] --> SM_CYICON           ');
      end;
    SM_CXICONSPACING:
      begin
        Assert(False, 'Trace:TODO: [TgtkObject.GetSystemMetrics] --> SM_CXICONSPACING    ');
      end;
    SM_CYICONSPACING:
      begin
        Assert(False, 'Trace:TODO: [TgtkObject.GetSystemMetrics] --> SM_CYICONSPACING    ');
      end;
    SM_CXMAXIMIZED:
      begin
        Assert(False, 'Trace:TODO: [TgtkObject.GetSystemMetrics] --> SM_CXMAXIMIZED      ');
      end;
    SM_CYMAXIMIZED:
      begin
        Assert(False, 'Trace:TODO: [TgtkObject.GetSystemMetrics] --> SM_CYMAXIMIZED      ');
      end;
    SM_CXMAXTRACK:
      begin
        Assert(False, 'Trace:TODO: [TgtkObject.GetSystemMetrics] --> SM_CXMAXTRACK       ');
      end;
    SM_CYMAXTRACK:
      begin
        Assert(False, 'Trace:TODO: [TgtkObject.GetSystemMetrics] --> SM_CYMAXTRACK       ');
      end;
    SM_CXMENUCHECK:
      begin
        Assert(False, 'Trace:TODO: [TgtkObject.GetSystemMetrics] --> SM_CXMENUCHECK      ');
      end;
    SM_CYMENUCHECK:
      begin
        Assert(False, 'Trace:TODO: [TgtkObject.GetSystemMetrics] --> SM_CYMENUCHECK      ');
      end;
    SM_CXMENUSIZE:
      begin
        Assert(False, 'Trace:TODO: [TgtkObject.GetSystemMetrics] --> SM_CXMENUSIZE       ');
      end;
    SM_CYMENUSIZE:
      begin
        Assert(False, 'Trace:TODO: [TgtkObject.GetSystemMetrics] --> SM_CYMENUSIZE       ');
      end;
    SM_CXMIN:
      begin
        Assert(False, 'Trace:TODO: [TgtkObject.GetSystemMetrics] --> SM_CXMIN            ');
      end;
    SM_CYMIN:
      begin
        Assert(False, 'Trace:TODO: [TgtkObject.GetSystemMetrics] --> SM_CYMIN            ');
      end;
    SM_CXMINIMIZED:
      begin
        Assert(False, 'Trace:TODO: [TgtkObject.GetSystemMetrics] --> SM_CXMINIMIZED      ');
      end;
    SM_CYMINIMIZED:
      begin
        Assert(False, 'Trace:TODO: [TgtkObject.GetSystemMetrics] --> SM_CYMINIMIZED      ');
      end;
    SM_CXMINSPACING:
      begin
        Assert(False, 'Trace:TODO: [TgtkObject.GetSystemMetrics] --> SM_CXMINSPACING     ');
      end;
    SM_CYMINSPACING:
      begin
        Assert(False, 'Trace:TODO: [TgtkObject.GetSystemMetrics] --> SM_CYMINSPACING     ');
      end;
    SM_CXMINTRACK:
      begin
        Assert(False, 'Trace:TODO: [TgtkObject.GetSystemMetrics] --> SM_CXMINTRACK       ');
      end;
    SM_CYMINTRACK:
      begin
        Assert(False, 'Trace:TODO: [TgtkObject.GetSystemMetrics] --> SM_CYMINTRACK       ');
      end;
    SM_CXSCREEN:
      begin
        result := gdk_Screen_Width;
      end;
    SM_CYSCREEN:
      begin
        result := gdk_Screen_Height;
      end;
    SM_CXSIZE:
      begin
        Assert(False, 'Trace:TODO: [TgtkObject.GetSystemMetrics] --> SM_CXSIZE           ');
      end;
    SM_CYSIZE:
      begin
        Assert(False, 'Trace:TODO: [TgtkObject.GetSystemMetrics] --> SM_CYSIZE           ');
      end;
    SM_CXSIZEFRAME:
      begin
        Assert(False, 'Trace:TODO: [TgtkObject.GetSystemMetrics] --> SM_CXSIZEFRAME      ');
      end;
    SM_CYSIZEFRAME:
      begin
        Assert(False, 'Trace:TODO: [TgtkObject.GetSystemMetrics] --> SM_CYSIZEFRAME      ');
      end;
    SM_CXSMICON:
      begin
        Assert(False, 'Trace:TODO: [TgtkObject.GetSystemMetrics] --> SM_CXSMICON         ');
      end;
    SM_CYSMICON:
      begin
        Assert(False, 'Trace:TODO: [TgtkObject.GetSystemMetrics] --> SM_CYSMICON         ');
      end;
    SM_CXSMSIZE:
      begin
        Assert(False, 'Trace:TODO: [TgtkObject.GetSystemMetrics] --> SM_CXSMSIZE         ');
      end;
    SM_CYSMSIZE:
      begin
        Assert(False, 'Trace:TODO: [TgtkObject.GetSystemMetrics] --> SM_CYSMSIZE         ');
      end;
    SM_CXVSCROLL:
      begin
        Assert(False, 'Trace:TODO: [TgtkObject.GetSystemMetrics] --> SM_CXVSCROLL        ');
      end;
    SM_CYVSCROLL:
      begin
        Assert(False, 'Trace:TODO: [TgtkObject.GetSystemMetrics] --> SM_CYVSCROLL        ');
      end;
    SM_CYCAPTION:
      begin
        Assert(False, 'Trace:TODO: [TgtkObject.GetSystemMetrics] --> SM_CYCAPTION        ');
      end;
    SM_CYKANJIWINDOW:
      begin
        Assert(False, 'Trace:TODO: [TgtkObject.GetSystemMetrics] --> SM_CYKANJIWINDOW    ');
      end;
    SM_CYMENU:
      begin
        Assert(False, 'Trace:TODO: [TgtkObject.GetSystemMetrics] --> SM_CYMENU           ');
      end;
    SM_CYSMCAPTION:
      begin
        Assert(False, 'Trace:TODO: [TgtkObject.GetSystemMetrics] --> SM_CYSMCAPTION      ');
      end;
    SM_CYVTHUMB:
      begin
        Assert(False, 'Trace:TODO: [TgtkObject.GetSystemMetrics] --> SM_CYVTHUMB         ');
      end;
    SM_DBCSENABLED:
      begin
        Assert(False, 'Trace:TODO: [TgtkObject.GetSystemMetrics] --> SM_DBCSENABLED      ');
      end;
    SM_DEBUG:
      begin
        Assert(False, 'Trace:TODO: [TgtkObject.GetSystemMetrics] --> SM_DEBUG            ');
      end;
    SM_MENUDROPALIGNMENT:
      begin
        Assert(False, 'Trace:TODO: [TgtkObject.GetSystemMetrics] --> SM_MENUDROPALIGNMENT');
      end;
    SM_MIDEASTENABLED:
      begin
        Assert(False, 'Trace:TODO: [TgtkObject.GetSystemMetrics] --> SM_MIDEASTENABLED   ');
      end;
    SM_MOUSEPRESENT:
      begin
        Assert(False, 'Trace:TODO: [TgtkObject.GetSystemMetrics] --> SM_MOUSEPRESENT     ');
      end;
    SM_MOUSEWHEELPRESENT:
      begin
        Assert(False, 'Trace:TODO: [TgtkObject.GetSystemMetrics] --> SM_MOUSEWHEELPRESENT');
      end;
    SM_NETWORK:
      begin
        Assert(False, 'Trace:TODO: [TgtkObject.GetSystemMetrics] --> SM_NETWORK          ');
      end;
    SM_PENWINDOWS:
      begin
        Assert(False, 'Trace:TODO: [TgtkObject.GetSystemMetrics] --> SM_PENWINDOWS       ');
      end;
    SM_SECURE:
      begin
        Assert(False, 'Trace:TODO: [TgtkObject.GetSystemMetrics] --> SM_SECURE           ');
      end;
    SM_SHOWSOUNDS:
      begin
        Assert(False, 'Trace:TODO: [TgtkObject.GetSystemMetrics] --> SM_SHOWSOUNDS       ');
      end;
    SM_SLOWMACHINE:
      begin
        Assert(False, 'Trace:TODO: [TgtkObject.GetSystemMetrics] --> SM_SLOWMACHINE      ');
      end;
    SM_SWAPBUTTON:
      begin
        Assert(False, 'Trace:TODO: [TgtkObject.GetSystemMetrics] --> SM_SWAPBUTTON       ');
      end;
    else Result := 0;
  end;
  Assert(False, Format('Trace:< [TgtkObject.GetSystemMetrics] %d --> 0x%x (%d)', [nIndex, Result, Result]));
end;

{------------------------------------------------------------------------------
  Function: GetTextExtentPoint
  Params:  none
  Returns: Nothing


 ------------------------------------------------------------------------------}
function TgtkObject.GetTextExtentPoint(DC: HDC; Str: PChar; Count: Integer; var Size: TSize): Boolean;
var
  lbearing, rbearing, width, ascent,descent: LongInt;
begin
  Assert(False, 'trace:> [TgtkObject.GetTextExtentPoint]');
  Result := IsValidDC(DC);
  if Result
  then with PDeviceContext(DC)^ do
  begin
    {if GC = nil
    then begin
      Assert(False, 'Trace:[TgtkObject.GetTextExtentPoint] Uninitialized GC');
      Result := False;
    end
    else} if (CurrentFont = nil) or (CurrentFont^.GDIFontObject = nil)
    then begin
      WriteLn('WARNING: [TgtkObject.GetTextExtentPoint] Missing font');
      Result := False;
    end
    else begin
      gdk_text_extents(CurrentFont^.GDIFontObject, Str, Count, @lbearing, @rBearing, @width, @ascent, @descent);
      Size.cX := Width;
      Size.cY := ascent + descent;
    end;
  end;
  Assert(False, 'trace:< [TgtkObject.GetTextExtentPoint]');
end;

{------------------------------------------------------------------------------
  Function: GetTextMetrics
  Params:  none
  Returns: Nothing


 ------------------------------------------------------------------------------}
function TgtkObject.GetTextMetrics(DC: HDC; var TM: TTextMetric): Boolean;
var
  lbearing, rbearing, dummy: LongInt;
begin
  Assert(False, Format('Trace:> TODO FINISH[TgtkObject.GetTextMetrics] DC: 0x%x', [DC]));

  Result := IsValidDC(DC);
  if Result then with PDeviceContext(DC)^ do begin
    if (CurrentFont = nil) or (CurrentFont^.GDIFontObject = nil)
    then begin
      WriteLn('WARNING: [TgtkObject.GetTGextMetrics] Missing font');
      Result := False;
    end
    else with TM do begin
      FillChar(TM, SizeOf(TM), 0);

      gdk_text_extents(CurrentFont^.GDIFontObject, '{g|h_}', 1, @lbearing, @rBearing, @dummy, @tmAscent, @tmDescent);
      tmHeight := tmAscent + tmDescent + 2; //todo EXACT MEASUREMENT
      tmAveCharWidth := gdk_char_width(CurrentFont^.GDIFontObject, 'x'); // avarage is mostly measured by the x
      tmMaxCharWidth := gdk_char_width(CurrentFont^.GDIFontObject, 'W'); // temp hack
    end;
  end;
  Assert(False, Format('Trace:< TODO FINISH[TgtkObject.GetTextMetrics] DC: 0x%x', [DC]));
end;

{------------------------------------------------------------------------------
  Function: GetWindowLong
  Params:  none
  Returns: Nothing
 ------------------------------------------------------------------------------}
Function TgtkObject.GetWindowLong(Handle : hwnd; int : Integer): Longint;
var
  Data : Tobject;
  P : Pointer;
begin
  //TODO:Started but not finished
  Assert(False, Format('Trace:> [TgtkObject.GETWINDOWLONG] HWND: 0x%x, int: 0x%x (%d)', [Handle, int, int]));

  case int of
    GWL_WNDPROC :
      begin
        Result := Longint(gtk_object_get_data(pgtkobject(Handle),'WNDPROC'));
      end;
    GWL_HINSTANCE :
      begin
        Result := Longint(gtk_object_get_data(pgtkobject(Handle),'HINSTANCE'));
      end;
    GWL_HWNDPARENT :
      begin
        P := gtk_object_get_data(pgtkobject(Handle),'HWNDPARENT');
        if P = nil then Result := 0 else Result := LongInt(p);
      end;

{    GWL_WNDPROC :
      begin
        Data := GetLCLObject(Pointer(Handle));
        if Data is TControl
        then Result := Longint(@(TControl(Data).WindowProc));
        // TODO fix this, a method pointer (2 pointers) cant be casted to a longint
      end;
}
{    GWL_HWNDPARENT :
      begin
        Data := GetLCLObject(Pointer(Handle));
        if (Data is TWinControl)
        then Result := Longint(TWincontrol(Data).Handle)
        else Result := 0;
      end;
 }
    GWL_STYLE :
      begin
        Result := Longint(gtk_object_get_data(pgtkobject(Handle),'Style'));
      end;
    GWL_EXSTYLE :
      begin
        Result := Longint(gtk_object_get_data(pgtkobject(Handle),'ExStyle'));
      end;
    GWL_USERDATA :
      begin
        Result := Longint(gtk_object_get_data(pgtkobject(Handle),'Userdata'));
      end;
    GWL_ID :
      begin
        Result := Longint(gtk_object_get_data(pgtkobject(Handle),'ID'));
      end;
    else Result := 0;
  end; //case

  Assert(False, Format('Trace:< [TgtkObject.GETWINDOWLONG] HWND: 0x%x, int: 0x%x (%d) --> 0x%x (%d)', [Handle, int, int, Result, Result]));
end;

{------------------------------------------------------------------------------
  Function: GetWindowOrgEx
  Params:  none
  Returns: Nothing
 ------------------------------------------------------------------------------}
function TgtkObject.GetWindowOrgEx(dc : hdc; var P : TPoint): Integer;
begin

  Assert(False, 'Trace:**********************************');
  gdk_window_get_deskrelative_origin(pgtkwidget(PdeviceContext(dc)^.hwnd)^.window, @P.X, @P.Y);
  Assert(False, format('Trace:DeskRelative Origin is: %d,%d',[P.x,P.y]));
  gdk_window_get_root_origin(pgtkwidget(PdeviceContext(dc)^.hwnd)^.window, @P.X, @P.Y);
  Assert(False, format('Trace:Root Origin is: %d,%d',[P.x,P.y]));
  gdk_window_get_origin(pgtkwidget(PdeviceContext(dc)^.hwnd)^.window, @P.X, @P.Y);
  Assert(False, format('Trace:Standard Origin is: %d,%d',[P.x,P.y]));
  Assert(False, 'Trace:**********************************');
  result := 1;
end;


{------------------------------------------------------------------------------
  Function: GetWindowRect
  Params:  none
  Returns: Nothing
 ------------------------------------------------------------------------------}
function TgtkObject.GetWindowRect(Handle: hwnd; var Rect: TRect): Integer;
var
  X, Y, W, H: Integer;
  Widget: PGTKWidget;
  Sender : TObject;
begin
Writeln('GetWindowRect');
  result := 0; //default
  if Handle <> 0 then
  begin
    Widget := GetFixedWidget(pgtkwidget(Handle));
    if (Widget = nil) then Widget := pgtkwidget(Handle);
    if Widget <> nil then
       begin
        try
         if Widget^.Window <> nil then
         Begin
           gdk_window_get_origin(Widget^.Window, @X, @Y);
           gdk_window_get_size(Widget^.Window, @W, @H);
         end
         else
         Begin
          sender := TObject(Gtk_Object_Get_Data(pGTKObject(widget),'Sender'));
          if (sender is TControl) then
               begin
                 writeln('****************SENDCER IS TCONTROL********************');
                 X := TControl(sender).Left;
                 Y := TControl(sender).Top;
                 W := TControl(sender).Width;
                 Y := TControl(sender).Height;
               end
               else
                Begin
                 X := 0;
                 Y := 0;
                 W := 100;
                 Y := 200;
                end;
         end;

Writeln('SetRect');
         SetRect(Rect, X, Y, X + W, Y + H);
Writeln('SetRect Done');
         result := -1;
        except
        end;
       end
       else
        Result := 0;
  end;
Writeln('GetWindowRect DONE');
end;


{------------------------------------------------------------------------------
  Function: HideCaret
  Params:  none
  Returns: Nothing


 ------------------------------------------------------------------------------}
function TgtkObject.HideCaret(hWnd: HWND): Boolean;
var
  GTKObject: PGTKObject;
begin
  Assert(False, Format('Trace:  [TgtkObject.HideCaret] HWND: 0x%x', [hWnd]));
//TODO: [TgtkObject.HideCaret] Finish (in gtkwinapi.inc)

  GTKObject := PGTKObject(HWND);
  Result := GTKObject <> nil;

  if Result
  then begin
    if gtk_type_is_a(gtk_object_type(GTKObject), GTKAPIWidget_GetType)
    then begin
      GTKAPIWidget_HideCaret(PGTKAPIWidget(GTKObject));
    end
//    else if // TODO: other widgettypes
    else begin
      Result := False;
    end;
  end
  else WriteLn('WARNING: [TgtkObject.HideCaret] Got null HWND');

end;

{------------------------------------------------------------------------------
  Function: InvalidateRect
  Params: aHandle:
          Rect:
          bErase:
  Returns:

 ------------------------------------------------------------------------------}
function TGTKObject.InvalidateRect(aHandle : HWND; Rect : pRect; bErase : Boolean) : Boolean;
var
  gdkRect : TGDKRectangle;
begin
  // Todo: Erase  before invalidating if bErase is true
//  Writeln(format('Rect = %d,%d,%d,%d',[rect^.left,rect^.top,rect^.Right,rect^.Bottom]));
  Result := True;
  try
    gdkRect.X := Rect^.Left;
    gdkRect.Y := Rect^.Top;
    gdkRect.Width := (Rect^.Right - Rect^.Left);
    gdkRect.Height := (Rect^.Bottom - Rect^.Top);
    gtk_widget_draw(PgtkWidget(aHandle), @gdkRect);
  except
    Result := False;
  end;

end;

{------------------------------------------------------------------------------
  Function: LineTo
  Params:  none
  Returns: Nothing


 ------------------------------------------------------------------------------}
function TgtkObject.LineTo(DC: HDC; X, Y: Integer): Boolean;
begin
  Assert(False, Format('trace:> [TgtkObject.LineTo] DC:0x%x, X:%d, Y:%d', [DC, X, Y]));
  Result := IsValidDC(DC);
  if Result
  then with PDeviceContext(DC)^ do
  begin
    if GC = nil
    then begin
      WriteLn('WARNING: [TgtkObject.LineTo] Uninitialized GC');
      Result := False;
    end
    else begin
      SelectGDKPenProps(DC);
      gdk_draw_line(Drawable, GC, PenPos.X, PenPos.Y, X, Y);
      PenPos:= Point(X, Y);
      Result := True;
    end;
  end;
  Assert(False, Format('trace:< [TgtkObject.LineTo] DC:0x%x, X:%d, Y:%d', [DC, X, Y]));
end;

{------------------------------------------------------------------------------
  Function: MaskBlt
  Params:  DestDC:                The destination devicecontext
           X, Y:                  The left/top corner of the destination rectangle
           Width, Height:         The size of the destination rectangle
           SrcDC:                 The source devicecontext 
           XSrc, YSrc:            The left/top corner of the source rectangle   
           Mask:                  The handle of a monochrome bitmap
           XMask, YMask:          The left/top corner of the mask rectangle
           Rop:                   The raster operation to be performed 
  Returns: True if succesful

  The MaskBlt function copies a bitmap from a source context into a destination 
  context using the specified mask and raster operation.
 ------------------------------------------------------------------------------}
function TgtkObject.MaskBlt(DestDC: HDC; X, Y, Width, Height: Integer; 
  SrcDC: HDC; XSrc, YSrc: Integer; Mask: HBITMAP; XMask, YMask: Integer; 
  Rop: DWORD): Boolean; 
begin
  Result:=false;
end;

{------------------------------------------------------------------------------
  Function: MessageBox
  Params:  hWnd:                  The handle of parent window
  Returns: 0 if not successful (out of memory), otherwise one of the defined value :
      IDABORT, IDCANCEL, IDIGNORE, IDNO, IDOK, IDRETRY, IDYES

 The MessageBox function displays a modal dialog, with text and caption defined,
 and includes buttons.
 ------------------------------------------------------------------------------}

function MessageButtonClicked(Widget : PGtkWidget; data: gPointer) : GBoolean; cdecl;
begin
  if Integer(data^) = 0 then
    Integer(data^):= Integer(gtk_object_get_data(PGtkObject(Widget), 'modal_result'));
  Result:=false;
end;

function MessageBoxClosed(Widget : PGtkWidget; Event : PGdkEvent; data: gPointer) : GBoolean; cdecl;
var ModalResult : integer;
begin
  { We were requested by window manager to close }
  if Integer(data^) = 0 then begin
    ModalResult:= Integer(gtk_object_get_data(PGtkObject(Widget), 'modal_result'));
    { Don't allow to close if we don't have a default return value }
    Result:= (ModalResult = 0);
    if not Result then Integer(data^):= ModalResult
    else WriteLn('Do not close !!!');
  end else Result:= false;    
end;

function TgtkObject.MessageBox(hWnd: HWND; lpText, lpCaption: PChar; uType : Cardinal): integer;
var Dialog, ALabel : PGtkWidget;
    ButtonCount, DefButton, ADialogResult : Integer;
    DialogType : Cardinal;

    procedure CreateButton(const ALabel : PChar; const RetValue : integer);
    var AButton : PGtkWidget;
    begin
      AButton:= gtk_button_new_with_label(ALabel);
      Inc(ButtonCount);
      if ButtonCount = DefButton then begin
        gtk_window_set_focus(PGtkWindow(Dialog), AButton);
      end;
      { If there is the Cancel button, allow the dialog to close }
      if RetValue = IDCANCEL then begin
        gtk_object_set_data(PGtkObject(Dialog), 'modal_result', Pointer(IDCANCEL));
      end;
      gtk_object_set_data(PGtkObject(AButton), 'modal_result', Pointer(RetValue));
      gtk_signal_connect(PGtkObject(AButton), 'clicked', TGtkSignalFunc(@MessageButtonClicked), @ADialogResult);
      gtk_container_add (PGtkContainer(PGtkDialog(Dialog)^.action_area), AButton);
    end;

begin
  ButtonCount:= 0;
  { Determine which is the default button }
  DefButton:= ((uType and $00000300) shr 8) + 1;
  Assert(False, 'Trace:Default button is ' + IntToStr(DefButton));

  ADialogResult:= 0;
  Dialog:= gtk_dialog_new;
  gtk_signal_connect(PGtkObject(Dialog), 'delete-event', TGtkSignalFunc(@MessageBoxClosed), @ADialogResult);
  gtk_window_set_default_size(PGtkWindow(Dialog), 100, 100);
  ALabel:= gtk_label_new(lpText);
  gtk_container_add (PGtkContainer(PGtkDialog(Dialog)^.vbox), ALabel);    
  DialogType:= (uType and $0000000F);
  if DialogType = MB_OKCANCEL 
  then begin
    CreateButton(SOK, IDOK);
    CreateButton(SCancel, IDCANCEL);
  end 
  else begin
    if DialogType = MB_ABORTRETRYIGNORE 
    then begin
      CreateButton(SAbort, IDABORT);
      CreateButton(SRetry, IDRETRY);
      CreateButton(SIgnore, IDIGNORE);
    end 
    else begin
      if DialogType = MB_YESNOCANCEL 
      then begin
        CreateButton(SYes, IDYES);
        CreateButton(SNo, IDNO);
        CreateButton(SCancel, IDCANCEL);
      end 
      else begin
        if DialogType = MB_YESNO 
        then begin
          CreateButton(SYes, IDYES);
          CreateButton(SNo, IDNO);
        end 
        else begin
          if DialogType = MB_RETRYCANCEL 
          then begin
            CreateButton(SRetry, IDRETRY);
            CreateButton(SCancel, IDCANCEL);
          end 
          else begin
            { We have no buttons to show. Create the default of OK button }
            CreateButton(SOK, IDOK);
          end;  
        end;
      end;
    end;
  end;
  gtk_window_set_title(PGtkWindow(Dialog), lpCaption);
  gtk_window_set_position(PGtkWindow(Dialog), GTK_WIN_POS_CENTER);
  gtk_window_set_modal(PGtkWindow(Dialog), true);  
  gtk_widget_show_all(Dialog);
  while ADialogResult = 0 do begin
    Application.ProcessMessages;
  end;
  gtk_widget_destroy(Dialog);
  Result:= ADialogResult;
end;

{------------------------------------------------------------------------------
  Function: MoveToEx
  Params:  none
  Returns: Nothing


 ------------------------------------------------------------------------------}
function TgtkObject.MoveToEx(DC: HDC; X, Y: Integer; OldPoint: PPoint): Boolean;
begin
  Assert(False, Format('trace:> [TgtkObject.MoveToEx] DC:0x%x, X:%d, Y:%d', [DC, X, Y]));
  Result := IsValidDC(DC);
  if Result
  then with PDeviceContext(DC)^ do
  begin
    if OldPoint <> nil then OldPoint^ := PenPos;
    PenPos := Point(X, Y);
  end;
  Assert(False, Format('trace:< [TgtkObject.MoveToEx] DC:0x%x, X:%d, Y:%d', [DC, X, Y]));
end;


{------------------------------------------------------------------------------
  Function: PeekMessage
  Params:  lpMsg        - Where it should put the message
           Handle       - Handle of the window (thread)
           wMsgFilterMin- Lowest MSG to grab
           wMsgFilterMax- Highest MSG to grab
           wRemoveMsg   - Should message be pulled out of the queue

  Returns: Boolean if an event was there
 ------------------------------------------------------------------------------}
function TgtkObject.PeekMessage(var lpMsg: TMsg; Handle : HWND; wMsgFilterMin, wMsgFilterMax, wRemoveMsg: UINT): Boolean;
var
  Message: PMSG;
begin 
  //TODO Filtering
  
  Result := FMessageQueue.Count > 0;
  if Result 
  then begin
    Message := FMessageQueue.Items[0];
    lpMsg := Message^;
    if (wRemoveMsg and PM_REMOVE) = PM_REMOVE 
    then FMessageQueue.Delete(0);
  end;

end;

{------------------------------------------------------------------------------
  Function: PostMessage
  Params: hWnd:
          Msg:
          wParam:
          lParam:
  Returns: True if succesful

  The PostMessage function places (posts) a message in the message queue and 
  then returns without waiting.
 ------------------------------------------------------------------------------}
function TGTKObject.PostMessage(hWnd: HWND; Msg: Cardinal; wParam: LongInt; lParam: LongInt): Boolean;
var
  Message: PMsg;
begin
  New(Message);
  Message^.HWnd := hWnd;
  Message^.Message := Msg;
  Message^.WParam := WParam;
  Message^.LParam := LParam;
//  Message^.Time := 
  FMessageQueue.Add(Message);
  Result := True;  
end;

{------------------------------------------------------------------------------
  Function: RealizePalette
  Params:  none
  Returns: Nothing


 ------------------------------------------------------------------------------}
function TgtkObject.RealizePalette(DC: HDC): Cardinal;
begin
  Assert(False, 'Trace:TODO: [TgtkObject.RealizePalette]');
  //TODO: Implement this;
  Result := 0;
end;

{------------------------------------------------------------------------------
  Function: Rectangle
  Params:  none
  Returns: Nothing

  The Rectangle function draws a rectangle. The rectangle is outlined by using
  the current pen and filled by using the current brush.
 ------------------------------------------------------------------------------}
function TgtkObject.Rectangle(DC: HDC; X1, Y1, X2, Y2: Integer): Boolean;
var
  Width, Height: Integer;
begin
  Assert(False, Format('trace:> [TgtkObject.Rectangle] DC:0x%x, X1:%d, Y1:%d, X2:%d, Y2:%d', [DC, X1, Y1, X2, Y2]));
  Result := IsValidDC(DC);
  if Result
  then with PDeviceContext(DC)^ do
  begin
    if GC = nil
    then begin
      WriteLn('WARNING: [TgtkObject.Rectangle] Uninitialized GC');
      Result := False;
    end
    else begin
      Width := X2 - X1;
      Height := Y2 - Y1;
      // first draw interior in brush color
      SelectGDKBrushProps(DC);
      gdk_draw_rectangle(Drawable, GC, 1, X1, Y1, Width, Height);

      // Draw outline
      SelectGDKPenProps(DC);
      gdk_draw_rectangle(Drawable, GC, 0, X1, Y1, Width, Height);
      Result := True;
    end;
  end;
  Assert(False, Format('trace:< [TgtkObject.Rectangle] DC:0x%x, X1:%d, Y1:%d, X2:%d, Y2:%d', [DC, X1, Y1, X2, Y2]));
end;

{------------------------------------------------------------------------------
  Function: ReleaseCapture
  Params:  none
  Returns: True if succesful

  The ReleaseCapture function releases the mouse capture from a window 
  and restores normal mouse input processing.
 ------------------------------------------------------------------------------}
function TgtkObject.ReleaseCapture: Boolean;
begin
  SetCapture(0);
  Result := True;
end;

{------------------------------------------------------------------------------
  Function: ReleaseDC
  Params:  none
  Returns: Nothing


 ------------------------------------------------------------------------------}
function TgtkObject.ReleaseDC(hWnd: HWND; hDC: HDC): Integer;
var
  nIndex: Integer;
  pDC: PDeviceContext;
begin
  Assert(False, Format('trace:> [TgtkObject.ReleaseDC] DC:0x%x', [hDC]));
  Result := 0;

  if {(hWnd <> 0) and} (hDC <> 0)
  then begin
    nIndex := FDeviceContexts.IndexOf(Pointer(hDC));
    if nIndex <> -1
    then begin
      pDC := PDeviceContext(hDC);
      { Release all graphic objects }
      DeleteObject(HGDIObj(pDC^.CurrentBrush));
      DeleteObject(HGDIObj(pDC^.CurrentPen));
      DeleteObject(HGDIObj(pDC^.CurrentFont));
      DeleteObject(HGDIObj(pDC^.CurrentBitmap));
      try
        { On root window, we don't allocate a graphics context }
        if pDC^.GC <> nil then gdk_gc_unref(pDC^.GC);
      except
        on Exception do; //Nothing, just try to unref it
                         //(it segfaults if the window doesnt exist anymore :-)
      end;
      FDeviceContexts.Delete(nIndex);
      Dispose(pDC);
      Result := 1;
    end;
  end;
  Assert(False, Format('trace:< [TgtkObject.ReleaseDC] FDeviceContexts[%d] DC:0x%x', [nIndex, hDC]));
end;

{------------------------------------------------------------------------------
  Function: RestoreDC
  Params:  none
  Returns: Nothing


-------------------------------------------------------------------------------}
function TgtkObject.RestoreDC(DC: HDC; SavedDC: Integer): Boolean;
  function CountSaved(pDC: PDeviceContext): Integer;
  begin
    Result := 0;
    while pDC^.SavedContext <> nil do
    begin
      Inc(Result);
      pDC := pDC^.SavedContext;
    end;
  end;

var
  pDC, pSaved: PDeviceContext;
  count: Integer;
begin
  Assert(False, Format('Trace:> [TgtkObject.RestoreDC] DC:0x%x, SavedDC: %d', [DC, SavedDC]));
  Result := IsValidDC(DC) and (SavedDC <> 0);
  if Result 
  then begin
    pDC := PDeviceContext(DC);
    Count := CountSaved(pDC);
    Result := (Abs(SavedDC) <= Count);
    
    if SavedDC > 0 then Dec(SavedDc, Count + 1); // make relative
    
    while (SavedDC < 0) and (pDC <> nil) and Result do
    begin
      Assert(False, Format('Trace:< [TgtkObject.RestoreDC] Unwinding#: %d', [SavedDC]));
      pSaved := pDC^.SavedContext;
      Inc(SavedDC);
      // TODO copy bitmap allso 
      
      pDC^.SavedContext := pSaved^.SavedContext;
      pSaved^.SavedContext := nil;
      
      Result := CopyDCData(pDC, pSaved);

      //prevent deleting of copied objects;
      if pSaved^.CurrentBitmap = pDC^.CurrentBitmap 
      then pSaved^.CurrentBitmap := nil;  
      if pSaved^.CurrentFont = pDC^.CurrentFont 
      then pSaved^.CurrentFont := nil;    
      if pSaved^.CurrentPen = pDC^.CurrentPen 
      then pSaved^.CurrentPen := nil;     
      if pSaved^.CurrentBrush = pDC^.CurrentBrush 
      then pSaved^.CurrentBrush := nil;   
      

      DeleteDC(HGDIOBJ(pSaved)); 
      // fornow  unref GC
(*
      if OldDC^.GC <> nil 
      then begin 
        gdk_gc_unref(OldDC^.GC);
        OldDC^.GC := nil;
      end;
*)
    end;
  end;
  Assert(False, Format('Trace:< [TgtkObject.RestoreDC] DC:0x%x, Saved: %d --> %s', [Integer(DC), SavedDC, BOOL_TEXT[Result]]));
end;


{------------------------------------------------------------------------------
  Function: SaveDc
  Params:  DC: a DC to save
  Returns: 0 if the functions fails otherwise a positive integer identifing 
           the saved DC

  The SaveDC function saves the current state of the specified device 
  context (DC) by copying its elements to a context stack.
-------------------------------------------------------------------------------}
function TgtkObject.SaveDC(DC: HDC): Integer;
var
  pDC, pSavedDC: PDeviceContext;
begin
  Assert(False, Format('Trace:> [TgtkObject.SaveDC]  0x%x', [Integer(DC)]));
  
  Result := 0;
  if IsValidDC(DC)
  then begin
    pDC := PDeviceContext(DC);
    pSavedDC := NewDC;
    CopyDCData(pSavedDC, pDC);
    pDC^.SavedContext:= pSavedDC;
    // count saved DCs
    repeat
      Inc(Result);
      pDC := pDC^.SavedContext;
    until pDC^.SavedContext = nil;
  end;
  
  Assert(False, Format('Trace:< [TgtkObject.SaveDC]  0x%x --> %d', [Integer(DC), Result]));
end;

{------------------------------------------------------------------------------
  Function: ScreenToClient
  Params: Handle:
          P:
  Returns:

 ------------------------------------------------------------------------------}
Function TGTKObject.ScreenToClient(Handle : HWND; var P : TPoint) : Integer;
var
  X, Y: Integer;
  Widget: PGTKWidget;
Begin

  if Handle = 0
  then begin
    X := 0;
    Y := 0;
  end
  else
   begin
    Widget := GetFixedWidget(pgtkwidget(Handle));
    if Widget = nil then
       Widget := pgtkwidget(Handle);
    if Widget = nil then
      begin
        X := 0;
        Y := 0;
      end
      else
        gdk_window_get_origin(Widget^.Window, @X, @Y);
   end;

  dec(P.X, X);
  dec(P.Y, Y);
  Result := -1;
end;

{------------------------------------------------------------------------------
  Function: ScrollWindowEx
  Params:  hWnd:       handle of window to scroll
           dx:         horizontal amount to scroll
           dy:         vertical amount to scroll
           prcScroll:  pointer to scroll rectangle
           prcClip:    pointer to clip rectangle
           hrgnUpdate: handle of update region
           prcUpdate:  pointer to update rectangle
           flags:      scrolling flags
           
  Returns: True if succesfull;

  The ScrollWindowEx function scrolls the content of the specified window's 
  client area
 ------------------------------------------------------------------------------}
function TgtkObject.ScrollWindowEx(hWnd: HWND; dx, dy: Integer; prcScroll, prcClip: PRect; hrgnUpdate: HRGN; prcUpdate: PRect; flags: UINT): Boolean; 
begin
  Result := False;
end;

{------------------------------------------------------------------------------
  Function: SelectObject
  Params:  none
  Returns: Nothing


 ------------------------------------------------------------------------------}
function TgtkObject.SelectObject(DC: HDC; GDIObj: HGDIOBJ): HGDIOBJ;
var
  Color: TGdkColor;
begin
  //TODO: Finish this;
  Assert(False, Format('trace:> [TgtkObject.SelectObject] DC: 0x%x', [DC]));

  Result := 0;
  if IsValidDC(DC) and IsValidGDIObject(GDIObj)
  then begin
    case PGdiObject(GDIObj)^.GDIType of
      gdiBitmap:
        with PDeviceContext(DC)^ do
        begin
          Assert(False, Format('trace:  [TgtkObject.SelectObject] DC: 0x%x, Type: Bitmap', [DC]));
          Result := HBITMAP(CurrentBitmap);
          CurrentBitmap := PGDIObject(GDIObj);
          if GC <> nil then gdk_gc_unref(GC);
          with PGdiObject(GDIObj)^ do
            case GDIBitmapType of
              gbPixmap: Drawable := GDIPixmapObject;
              gbBitmap: Drawable := GDIBitmapObject;
              gbImage:  Drawable := nil;//GDIRawImageObject;
            else
              Drawable := nil;
            end;
          GC := gdk_gc_new(Drawable);

          gdk_gc_set_function(GC, GDK_COPY);

        end;
      gdiBrush:
        with PDeviceContext(DC)^, PGdiObject(GDIObj)^ do
        begin
          Assert(False, Format('trace:  [TgtkObject.SelectObject] DC: 0x%x, Type: Brush', [DC]));
          Result := HBRUSH(CurrentBrush);
          CurrentBrush := PGDIObject(GDIObj);
          if GC <> nil
          then begin
            gdk_gc_set_fill(GC, GDIBrushFill);
            case GDIBrushFill of
              GDK_STIPPLED: gdk_gc_set_stipple(GC, GDIBrushPixMap);
              GDK_TILED:    gdk_gc_set_tile(GC, GDIBrushPixMap);
            end;
          end;
        end;
      gdiFont:
        with PDeviceContext(DC)^ do
        begin
          Assert(False, Format('trace:  [TgtkObject.SelectObject] DC: 0x%x, Type: Font', [DC]));
          Result := HFONT(CurrentFont);
          CurrentFont := PGDIObject(GDIObj);
          if GC <> nil
          then begin
            gdk_gc_set_font(GC, PGdiObject(GDIObj)^.GDIFontObject);
          end;
        end;
      gdiPen:
        with PDeviceContext(DC)^ do
        begin
          Result := HPEN(CurrentPen);
          CurrentPen := PGDIObject(GDIObj);
          if GC <> nil then SelectGDKPenProps(DC);
        end;
      gdiRegion:
      begin
        Assert(False, Format('Trace:TODO: [TgtkObject.SelectObject] DC: 0x%x, Type: Region', [DC]));
      end;
    end;
  end;
  Assert(False, Format('trace:< [TgtkObject.SelectObject] DC: 0x%x  --> 0x%x', [DC, Result]));
end;

{------------------------------------------------------------------------------
  Function: SelectPalette
  Params:  none
  Returns: Nothing


 ------------------------------------------------------------------------------}
function TgtkObject.SelectPalette(DC: HDC; Palette: HPALETTE; ForceBackground: Boolean): HPALETTE;
begin
  Assert(False, 'Trace:TODO: [TgtkObject.SelectPalette]');
  //TODO: Implement this;
  Result := 0;
end;

{------------------------------------------------------------------------------
  Function: SendMessage
  Params: hWnd:
          Msg:
          wParam:
          lParam:
  Returns:

  The SendMessage function sends the specified message to a window or windows. 
  The function calls the window procedure for the specified window and does 
  not return until the window procedure has processed the message.
 ------------------------------------------------------------------------------}
function TGTKObject.SendMessage(hWnd: HWND; Msg: Cardinal; wParam: LongInt; lParam: LongInt): Integer;
var
  Message: TLMessage;
begin
  Message.Msg := Msg;
  Message.WParam := WParam;
  Message.LParam := LParam;
  Message.Result := 0;
  
  Result := DeliverMessage(GetLCLObject(Pointer(hWnd)), Message);
end;

{------------------------------------------------------------------------------
  Function: SetBkColor        pbd
  Params:  DC:    Device context to change the text background color
           Color: RGB Tuple
  Returns: Old Background color


 ------------------------------------------------------------------------------}
function TgtkObject.SetBKColor(DC: HDC; Color: TColorRef): TColorRef;
const
  HI_MASK = LongWord($FF00);
  LO_MASK = LongWord($FF);
begin
  Assert(False, Format('trace:> [TgtkObject.SetBKColor] DC: 0x%x Color: %8x', [Integer(DC), Color]));
  Result := CLR_INVALID;
  if IsValidDC(DC)
  then begin
    with PDeviceContext(DC)^, CurrentBackColor do
    begin
      Result := ((Red and HI_MASK) shr 8) or (Green and HI_MASK) or ((Blue and HI_MASK) shl 8);

      if Result <> Color
      then begin

        gdk_colormap_free_colors(gdk_colormap_get_system, @CurrentBackColor, 1);

        Red :=   ((Color shl 8) and HI_MASK) or ((Color       ) and LO_MASK);
        Green := ((Color      ) and HI_MASK) or ((Color shr 8 ) and LO_MASK);
        Blue :=  ((Color shr 8) and HI_MASK) or ((Color shr 16) and LO_MASK);

        gdk_colormap_alloc_color(gdk_colormap_get_system, @CurrentBackColor, False, True);
      end;
    end;

  end;
  Assert(False, Format('trace:< [TgtkObject.SetBKColor] DC: 0x%x Color: %8x --> %8x', [Integer(DC), Color, Result]));
end;

{------------------------------------------------------------------------------
  Function: SetBkMode
  Params: DC:
          bkMode:
  Returns:

 ------------------------------------------------------------------------------}
Function TGTKObject.SetBkMode(DC: HDC; bkMode : Integer) : Integer;
begin
  // Your code here
  Result:=0;
end;

{------------------------------------------------------------------------------
  Function: SetCapture
  Params:  Value: Handle of window to capture
  Returns: Nothing


 ------------------------------------------------------------------------------}
function TgtkObject.SetCapture(Value: Longint): Longint;
var
  Sender : TObject;
begin
  Assert(False, Format('Trace:> [TgtkObject.SetCapture] 0x%x', [Value]));
  
  //CaptureHandle is defined in gtkint.pp pivate var definition.
  
  //MWE: there are some problems with grabbing the pointer and tabs
  //     so back to gtk_grab

  if MCaptureHandle <> 0 
  //then gdk_pointer_ungrab(0); 
  then gtk_grab_remove(pgtkwidget(MCaptureHandle));
  
  //
  Result := MCaptureHandle;
  MCaptureHandle := Value;

  if MCaptureHandle <> 0
  then begin
    WriteLN(Format('[TgtkObject.SetCapture] Current widget 0x%p', [gtk_grab_get_current]));
    gtk_grab_add(Pointer(MCaptureHandle));
    WriteLN(Format('[TgtkObject.SetCapture] handle: 0x%p gtk: 0x%p', [Pointer(MCaptureHandle), gtk_grab_get_current]));


//    gtk_grab_add(pGTKWidget(FCaptureHandle));
{
    if gdk_pointer_grab(PGTKWidget(Value)^.Window, gtk_False, 
      GDK_POINTER_MOTION_MASK or GDK_POINTER_MOTION_HINT_MASK or
      GDK_BUTTON_MOTION_MASK or GDK_BUTTON1_MOTION_MASK or
      GDK_BUTTON2_MOTION_MASK or GDK_BUTTON3_MOTION_MASK or
      GDK_BUTTON_PRESS_MASK or GDK_BUTTON_RELEASE_MASK,
      PGTKWidget(Value)^.Window, nil, 0) <> 0
    then begin
      FCaptureHandle := 0;
      Result := 0;
      assert(False, Format('trace:[TgtkObject.SetCapture] 0x%x failed', [Value]));
    end;
}
 Writeln('SetCapture result is '+inttostr(result));
   if MCaptureHandle <>  0 then
            SendMessage(MCaptureHandle, LM_CAPTURECHANGED, 0, Result);
  end;
     
  Assert(False, Format('Trace:< [TgtkObject.SetCapture] 0x%x --> 0x%x', [Value, Result]));
end;

{------------------------------------------------------------------------------
  Function: SetCaretPos
  Params:  none
  Returns: Nothing


 ------------------------------------------------------------------------------}
function TgtkObject.SetCaretPos(X, Y: Integer): Boolean;
var
  FocusObject: PGTKObject;
begin
  FocusObject := PGTKObject(GetFocus);
  Result := FocusObject <> nil;
  
  if Result
  then begin
//    Assert(False, Format('Trace:[TgtkObject.SetCaretPos] got focusObject 0x%x --> %s', [Integer(FocusObject), gtk_type_name(FocusObject^.Klass^.theType)]));

    if gtk_type_is_a(gtk_object_type(FocusObject), GTKAPIWidget_GetType)
    then begin
      GTKAPIWidget_SetCaretPos(PGTKAPIWidget(FocusObject), X, Y);
    end
//    else if // TODO: other widgettypes
    else begin
      Result := False;
    end;
  end;
end;

{------------------------------------------------------------------------------
  Function: SetFocus
  Params:  hWnd: Handle of new focus window
  Returns: The old focus window

  The SetFocus function sets the keyboard focus to the specified window
 ------------------------------------------------------------------------------}
function TgtkObject.SetFocus(hWnd: HWND): HWND;
var
  TopLevel: PGTKWidget;
begin
//writeln('TgtkObject.SetFocus A ',hWnd);
  if hwnd = 0
  then
    Result := 0
  else begin
//writeln('TgtkObject.SetFocus B');
    Result := GetFocus;
//writeln('TgtkObject.SetFocus C');
    TopLevel := gtk_widget_get_toplevel(PGTKWidget(hWND));
//writeln('TgtkObject.SetFocus D');
    if gtk_type_is_a(gtk_object_type(PGTKObject(TopLevel)), gtk_window_get_type)
    then gtk_window_set_focus(PGTKWindow(TopLevel), PGTKWidget(hWND));
  end;

//writeln('TgtkObject.SetFocus End');
end;

Function TgtkObject.SetProp(Handle: hwnd; Str : PChar; Data : Pointer) : Boolean;
Begin
  gtk_object_set_data(pGTKObject(handle),Str,data);
  // ToDo
  Result:=false;
end;

{------------------------------------------------------------------------------
  Function: SetScrollInfo
  Params:  none
  Returns: The old position value


 ------------------------------------------------------------------------------}
function TgtkObject.SetScrollInfo(Handle : HWND; SBStyle : Integer; ScrollInfo: TScrollInfo; bRedraw : Boolean): Integer;
const
  POLICY: array[BOOLEAN] of TGTKPolicyType = (GTK_POLICY_NEVER, GTK_POLICY_ALWAYS);
var
  Adjustment: PGtkAdjustment;
begin
//  Assert(False, 'Trace:[TgtkObject.SetScrollInfo]');
  with ScrollInfo do Assert(False, Format('Trace:> [TgtkObject.SetScrollInfo] Mask:0x%x, Min:%d, Max:%d, Page:%d, Pos:%d', [fMask, nMin, nMax, nPage, nPos]));

  Result := 0;
  if (Handle <> 0)
  then begin
    case SBStyle of
      SB_HORZ:
        if gtk_type_is_a(gtk_object_type(PGTKObject(Handle)), gtk_scrolled_window_get_type) then
           Adjustment := gtk_scrolled_window_get_hadjustment(PGTKScrolledWindow(Handle))
        else
        if gtk_type_is_a(gtk_object_type(PGTKObject(Handle)), gtk_hscrollbar_get_type) then
           Adjustment :=  PgtkhScrollBar(handle)^.Scrollbar.Range.Adjustment;
      SB_VERT:
        if gtk_type_is_a(gtk_object_type(PGTKObject(Handle)), gtk_scrolled_window_get_type) then
           Adjustment := gtk_scrolled_window_get_vadjustment(PGTKScrolledWindow(Handle))
        else
        if gtk_type_is_a(gtk_object_type(PGTKObject(Handle)), gtk_vscrollbar_get_type) then
           Adjustment :=  PgtkvScrollBar(handle)^.Scrollbar.Range.Adjustment;
      SB_CTL:
        if gtk_type_is_a(gtk_object_type(PGTKObject(Handle)), gtk_range_get_type)
        then Adjustment := gtk_range_get_adjustment(PGTKRange(Handle));
      else
        Adjustment := nil;
    end;
    if Adjustment <> nil
    then with ScrollInfo, Adjustment^ do begin
      Result := Round(Value);
      if (fMask and SIF_POS) <> 0
      then Value := nPos;
      if (fMask and SIF_RANGE) <> 0
      then begin
        Lower := nMin;
        Upper := nMax;
      end;
      if (fMask and SIF_PAGE) <> 0
      then begin
        Page_Size := nPage;
        Page_Increment := nPage;
      end;

      // do we have to set this allways ?
      if bRedraw then
         begin
{}
         if  (Handle <> 0) then
            begin
            if gtk_type_is_a(gtk_object_type(PGTKObject(Handle)), gtk_scrolled_window_get_type) then
             begin
               if SBStyle in [SB_BOTH, SB_HORZ]
                 then gtk_object_set(PGTKObject(Handle), 'hscrollbar_policy', [POLICY[bRedraw], nil]);
               if SBStyle in [SB_BOTH, SB_VERT]
                 then gtk_object_set(PGTKObject(Handle), 'vscrollbar_policy', [POLICY[bRedraw], nil]);
             end
            else
            begin
             if (SBSTYLE = SB_CTL) and gtk_type_is_a(gtk_object_type(PGTKObject(Handle)), gtk_widget_get_type) then
                 gtk_widget_show(PGTKWidget(Handle))
                 else
                 gtk_widget_hide(PGTKWidget(Handle))
            end;
            end;
{}

         gtk_adjustment_changed(Adjustment);
         end;
    end;
  end;
  with ScrollInfo do Assert(False, Format('Trace:> [TgtkObject.SetScrollInfo] --> %d', [Result]));
end;

{------------------------------------------------------------------------------
  Function: SetSysColors
  Params:  cElements: the number of elements
           lpaElements: array with element numbers
           lpaRgbValues: array with colors
  Returns: 0 if unsuccesful 

  The SetSysColors function sets the colors for one or more display elements.
 ------------------------------------------------------------------------------}
function TgtkObject.SetSysColors(cElements: Integer; const lpaElements; const lpaRgbValues): Boolean; 
type
  TLongArray = array[0..0] of Longint;
  PLongArray = ^TLongArray;
var
  n: Integer;
  Element: LongInt;
begin
  Result := False;
  if cElements > MAX_SYS_COLORS then Exit;
  
  for n := 0 to cElements - 1 do
  begin
    Element := PLongArray(lpaElements)^[n];
    if (Element > MAX_SYS_COLORS)
    or (Element < 0)
    then Exit;
    SysColorMap[PLongArray(lpaElements)^[n]] := PLongArray(lpaRgbValues)^[n];
    //Assert(False, Format('Trace:[TgtkObject.SetSysColor] Index %d (%8x) --> %8x', [PLongArray(lpaElements)^[n], SysColorMap[PLongArray(lpaElements)^[n]], PLongArray(lpaRgbValues)^[n]]));
  end;
  
  //TODO send WM_SYSCOLORCHANGE
  Result := True;
end;

{------------------------------------------------------------------------------
  Function: SetTextCharacterExtra
  Params: _hdc:
          nCharExtra:
  Returns:

 ------------------------------------------------------------------------------}
Function TGTKObject.SetTextCharacterExtra(_hdc : hdc; nCharExtra : Integer):Integer;
begin
  // Your code here
  Result:=0;
end;

{------------------------------------------------------------------------------
  Function: SetTextColor
  Params:  hdc: Identifies the device context.
           Color: Specifies the color of the text.
  Returns: The previous color if succesful, CLR_INVALID otherwise

  The SetTextColor function sets the text color for the specified device
  context to the specified color.
 ------------------------------------------------------------------------------}
function TgtkObject.SetTextColor(DC: HDC; Color: TColorRef): TColorRef;
const
  HI_MASK = LongWord($FF00);
  LO_MASK = LongWord($FF);
begin
  Assert(False, Format('trace:> [TgtkObject.SetTextColor] DC: 0x%x Color: %8x', [Integer(DC), Color]));
  Result := CLR_INVALID;
  if IsValidDC(DC)
  then begin
    with PDeviceContext(DC)^, CurrentTextColor do
    begin
      Result := ((Red and HI_MASK) shr 8) or (Green and HI_MASK) or ((Blue and HI_MASK) shl 8);

      if Result <> Color
      then begin

        gdk_colormap_free_colors(gdk_colormap_get_system, @CurrentTextColor, 1);

        Red :=   ((Color shl 8) and HI_MASK) or ((Color       ) and LO_MASK);
        Green := ((Color      ) and HI_MASK) or ((Color shr 8 ) and LO_MASK);
        Blue :=  ((Color shr 8) and HI_MASK) or ((Color shr 16) and LO_MASK);

        gdk_colormap_alloc_color(gdk_colormap_get_system, @CurrentTextColor, False, True);
      end;
    end;

  end;
  Assert(False, Format('trace:< [TgtkObject.SetTextColor] DC: 0x%x Color: %8x --> %8x', [Integer(DC), Color, Result]));
end;

{------------------------------------------------------------------------------
  Procedure: SetWindowLong
  Params:  none
  Returns: Nothing


 ------------------------------------------------------------------------------}
function TgtkObject.SetWindowLong(Handle: HWND; Idx: Integer; NewLong: Longint): LongInt;
begin
  //TODO: Finish this;
  Assert(False, Format('Trace:> [TgtkObject.SETWINDOWLONG] HWND: 0x%x, idx: 0x%x(%d), Value: 0x%x(%d)', [Handle, idx, idx, newlong, newlong]));
  Result:=0;

  case idx of
    GWL_WNDPROC :
      begin
        gtk_object_set_data(pgtkobject(Handle),'WNDPROC',pointer(NewLong));
      end;
    GWL_HINSTANCE :
      begin
        gtk_object_set_data(pgtkobject(Handle),'HINSTANCE',pointer(NewLong));
      end;
    GWL_HWNDPARENT :
      begin
        gtk_object_set_data(pgtkobject(Handle),'HWNDPARENT',pointer(NewLong));
      end;
    GWL_STYLE :
      begin
        gtk_object_set_data(pgtkobject(Handle),'Style',pointer(NewLong));
      end;
    GWL_EXSTYLE :
      begin
        gtk_object_set_data(pgtkobject(Handle),'ExStyle',pointer(NewLong));
      end;
    GWL_USERDATA :
      begin
        gtk_object_set_data(pgtkobject(Handle),'Userdata',pointer(NewLong));
      end;
    GWL_ID :
      begin
        gtk_object_set_data(pgtkobject(Handle),'ID',pointer(NewLong));
      end;
  end; //case
  Assert(False, Format('Trace:< [TgtkObject.SETWINDOWLONG] HWND: 0x%x, idx: 0x%x(%d), Value: 0x%x(%d) --> 0x%x(%d)', [Handle, idx, idx, newlong, newlong, Result, Result]));
end;

Function TgtkObject.SetWindowOrgEx(dc : hdc; NewX, NewY : Integer; Var lpPoint : TPoint) : Boolean;
begin
gdk_window_move(pgtkwidget(PdeviceContext(dc)^.hwnd)^.window,Newx,Newy);
//the following throws an error when compiling
{
if lpPoint <> nil then
   Begin
     lpPoint.X := NewX;
     lpPoint.Y := NewY;
   end;
}

Result := True;
end;

{------------------------------------------------------------------------------
  Function: ShowCaret
  Params:  none
  Returns: Nothing


 ------------------------------------------------------------------------------}
function TgtkObject.ShowCaret(hWnd: HWND): Boolean;
var
  GTKObject: PGTKObject;
begin
//TODO: [TgtkObject.ShowCaret] Finish (in gtkwinapi.inc)
  Assert(False, Format('Trace:> [TgtkObject.ShowCaret] HWND: 0x%x', [hWnd]));

  GTKObject := PGTKObject(HWND);
  Result := GTKObject <> nil;

  if Result
  then begin
    if gtk_type_is_a(gtk_object_type(GTKObject), GTKAPIWidget_GetType)
    then begin
      GTKAPIWidget_ShowCaret(PGTKAPIWidget(GTKObject));
    end
//    else if // TODO: other widgettypes
    else begin
      Result := False;
    end;
  end
  else WriteLn('WARNING: [TgtkObject.ShowCaret] Got null HWND');

  Assert(False, Format('Trace:< [TgtkObject.ShowCaret] HWND: 0x%x --> %s', [hWnd, BOOL_TEXT[Result]]));
end;

{------------------------------------------------------------------------------
  Function:  ShowScrollBar
  Params:  Wnd, wBar, bShow
  Returns: Nothing


 ------------------------------------------------------------------------------}
function TgtkObject.ShowScrollBar(Handle: HWND; wBar: Integer; bShow: Boolean): Boolean;
const
  POLICY: array[BOOLEAN] of TGTKPolicyType = (GTK_POLICY_NEVER, GTK_POLICY_ALWAYS);
begin
  Assert(False, 'trace:[TgtkObject.ShowScrollBar]');
  Result:=false;
{  Result := (Handle <> 0);
  if Result
  then begin
    if gtk_type_is_a(gtk_object_type(PGTKObject(Handle)), gtk_scrolled_window_get_type)
    then begin
      if wBar in [SB_BOTH, SB_HORZ]
      then gtk_object_set(PGTKObject(Handle), 'hscrollbar_policy', [POLICY[bShow], nil]);
      if wBar in [SB_BOTH, SB_VERT]
      then gtk_object_set(PGTKObject(Handle), 'vscrollbar_policy', [POLICY[bShow], nil]);
    end
    else begin
      if (wBar = SB_CTL) and gtk_type_is_a(gtk_object_type(PGTKObject(Handle)), gtk_widget_get_type)
      then begin
        if bShow
        then gtk_widget_show(PGTKWidget(Handle))
        else gtk_widget_hide(PGTKWidget(Handle));
      end;
    end;
  end;
}
end;

{------------------------------------------------------------------------------
  Function: StretchBlt
  Params:  DestDC:                The destination devicecontext
           X, Y:                  The left/top corner of the destination rectangle
           Width, Height:         The size of the destination rectangle
           SrcDC:                 The source devicecontext 
           XSrc, YSrc:            The left/top corner of the source rectangle   
           SrcWidth, SrcHeight:   The size of the source rectangle
           Rop:                   The raster operation to be performed 
  Returns: True if succesful

  The StretchBlt function copies a bitmap from a source rectangle into a 
  destination rectangle using the specified raster operation. If needed it 
  resizes the bitmap to fit the dimensions of the destination rectangle. 
  Sizing is done according to the stretching mode currently set in the 
  destination device context.
 ------------------------------------------------------------------------------}
function TgtkObject.StretchBlt(DestDC: HDC; X, Y, Width, Height: Integer; SrcDC: HDC; XSrc, YSrc, SrcWidth, SrcHeight: Integer; Rop: Cardinal): Boolean;
var
  pixmap : PgdkPixmap;
  pixmapwid : pgtkWidget;
begin
  Assert(True, Format('trace:> [TgtkObject.StretchBlt] DestDC:0x%x; X:%d, Y:%d, Width:%d, Height:%d; SrcDC:0x%x; XSrc:%d, YSrc:%d, SrcWidth:%d, SrcHeight:%d; Rop:0x%x', [DestDC, X, Y, Width, Height, SrcDC, XSrc, YSrc, SrcWidth, SrcHeight, Rop]));
  Result := IsValidDC(DestDC) and IsValidDC(SrcDC);
  if Result
  then begin
    gdk_gc_set_function(PDeviceContext(DestDC)^.GC, GDK_COPY);
    // TODO: Add scaling and ROP
    //first create a pixmap with transparency
{  THIS is test code for transparency
    pixmap := pgdkPixmap(PgdiObject(Srcdc)^.GDIBitmapObject);
    if PgdiObject(SRCdc)^.GDIBitmapMaskObject <> nil then
    pixmapwid := gtk_pixmap_new(pixmap,PgdiObject(SRCdc)^.GDIBitmapMAskObject);

    gdk_draw_pixmap(PDeviceContext(DestDC)^.Drawable, PDeviceContext(DestDC)^.GC, PgdkDrawable(pixmapwid^.window),
      XSrc, YSrc, X, Y, Width, Height);
}

    gdk_draw_pixmap(PDeviceContext(DestDC)^.Drawable, PDeviceContext(DestDC)^.GC, PDeviceContext(SrcDC)^.Drawable,
      XSrc, YSrc, X, Y, Width, Height);

  end;
  Assert(True, Format('trace:< [TgtkObject.StretchBlt] DestDC:0x%x --> %s', [DestDC, BOOL_TEXT[Result]]));
end;

{------------------------------------------------------------------------------
  Function: StretchMaskBlt
  Params:  DestDC:                The destination devicecontext
           X, Y:                  The left/top corner of the destination rectangle
           Width, Height:         The size of the destination rectangle
           SrcDC:                 The source devicecontext 
           XSrc, YSrc:            The left/top corner of the source rectangle   
           SrcWidth, SrcHeight:   The size of the source rectangle
           Mask:                  The handle of a monochrome bitmap
           XMask, YMask:          The left/top corner of the mask rectangle
           Rop:                   The raster operation to be performed 
  Returns: True if succesful

  The StretchMaskBlt function copies a bitmap from a source rectangle into a 
  destination rectangle using the specified mask and raster operation. If needed
  it resizes the bitmap to fit the dimensions of the destination rectangle. 
  Sizing is done according to the stretching mode currently set in the 
  destination device context.
 ------------------------------------------------------------------------------}
function TgtkObject.StretchMaskBlt(DestDC: HDC; X, Y, Width, Height: Integer;
  SrcDC: HDC; XSrc, YSrc, SrcWidth, SrcHeight: Integer; Mask: HBITMAP;
  XMask, YMask: Integer; Rop: DWORD): Boolean; 
begin
  Result:=false;
end;

{------------------------------------------------------------------------------
  Function: TextOut
  Params: DC:
          X:
          Y:
          Str:
          Count:
  Returns:

 ------------------------------------------------------------------------------}
Function TGTKObject.TextOut(DC: HDC; X,Y : Integer; Str : Pchar; Count: Integer) : Boolean;
begin
  // Your code here
  Result:=false;
end;

{------------------------------------------------------------------------------
  Function: WindowFromPoint
  Params: Point: Specifies the x and y Coords
  Returns: The handle of the gtkwidget.  If none exist, then NULL is returned.

 ------------------------------------------------------------------------------}
Function TGTKObject.WindowFromPoint(Point : TPoint) : HWND;
var
ev : TgdkEvent;
Window : PgdkWindow;
Widget : PgtkWidget;
begin
  // Check the state of the widget.  IF it's hidden or disabled, don't return it's handle!
  Result := 0;

  Window := gdk_window_at_pointer(@Point.x,@Point.Y);
  if window <> nil then
     Begin
     ev.any.window := Window;
     Widget := gtk_get_event_widget(@ev);
     if widget <> nil then Result := Longint(widget);
     Assert(False, format('Trace:Result = [%d]',[Result]));
     end
     else
     Assert(False, 'Trace:Result = nil');


end;

//##apiwiz##eps##   // Do not remove

{$IFDEF ASSERT_IS_ON}
  {$UNDEF ASSERT_IS_ON}
  {$C-}
{$ENDIF}

{ =============================================================================

  $Log$
  Revision 1.22  2001/03/12 12:17:02  lazarus
  MG: fixed random function results

  Revision 1.21  2001/02/20 16:53:27  lazarus
  Changes for wordcompletion and many other things from Mattias.
  Shane

  Revision 1.20  2001/02/16 19:13:31  lazarus
  Added some functions
  Shane

  Revision 1.19  2001/02/06 18:19:38  lazarus
  Shane

  Revision 1.18  2001/02/04 04:18:12  lazarus
  Code cleanup and JITFOrms bug fix.
  Shane

  Revision 1.17  2001/02/01 19:34:50  lazarus
  TScrollbar created and a lot of code added.

  It's cose to working.
  Shane

  Revision 1.16  2001/01/23 23:33:55  lazarus
  MWE:
    - Removed old LM_InvalidateRect
    - did some cleanup in old  code
    + added some comments  on gtkobject data (gtkproc)

  Revision 1.15  2001/01/23 19:01:10  lazarus
  Fixxed bug in RestoreDC
  Shane

  Revision 1.12  2001/01/12 18:46:50  lazarus
  Named the speedbuttons in MAINIDE and took out some writelns.
  Shane

  Revision 1.11  2001/01/04 16:12:54  lazarus
  Removed some writelns and changed the property editor for TStrings a bit.
  Shane

  Revision 1.10  2001/01/03 18:44:54  lazarus
  The Speedbutton now has a numglyphs setting.
  I started the TStringPropertyEditor

  Revision 1.9  2000/10/09 22:50:33  lazarus
  MWE:
    * fixed some selection code
    + Added selection sample

  Revision 1.8  2000/09/10 23:08:31  lazarus
  MWE:
    + Added CreateCompatibeleBitamp function
    + Updated TWinControl.WMPaint
    + Added some checks to avoid gtk/gdk errors
    - Removed no fixed warning from GetDC
    - Removed some output

  Revision 1.7  2000/08/14 12:31:12  lazarus
  Minor modifications for SynEdit .
  Shane

  Revision 1.6  2000/08/11 14:59:09  lazarus
  Adding all the Synedit files.
  Changed the GDK_KEY_PRESS and GDK_KEY_RELEASE stuff to fix the problem in the editor with the shift key being ignored.
  Shane

  Revision 1.5  2000/08/10 18:56:24  lazarus
  Added some winapi calls.
  Most don't have code yet.
  SetTextCharacterExtra
  CharLowerBuff
  IsCharAlphaNumeric
  Shane

  Revision 1.4  2000/08/07 17:06:39  lazarus
  Slight modification to CreateFontIndirect.
  I check to see if the GdiObject^.GDIFontObject is nil.  If so After the code to retry the weight and slant I added code to retry the Family and Foundry.
  Shane

  Revision 1.3  2000/07/30 21:48:34  lazarus
  MWE:
    = Moved ObjectToGTKObject to GTKProc unit
    * Fixed array checking in LoadPixmap
    = Moved LM_SETENABLED to API func EnableWindow and EnableMenuItem
    ~ Some cleanup

  Revision 1.2  2000/07/23 10:53:41  lazarus
  workaround for possible compiler bug (KEYSTATE), stoppok

  Revision 1.1  2000/07/13 10:28:30  michael
  + Initial import

  Revision 1.17  2000/07/09 20:18:56  lazarus
  MWE:
    + added new controlselection
    + some fixes
    ~ some cleanup

  Revision 1.16  2000/06/04 10:00:33  lazarus
  MWE:
    * Fixed bug #6.

  Revision 1.15  2000/05/30 22:28:41  lazarus
  MWE:
    Applied patches from Vincent Snijders:
    + Added GetWindowRect
    * Fixed horz label alignment
    + Added vert label alignment

  Revision 1.14  2000/05/14 21:56:12  lazarus
  MWE:
    + added local messageloop
    + added PostMessage
    * fixed Peekmessage
    * fixed ClientToScreen
    * fixed Flat style of Speedutton (TODO: Draw)
    + Added TApplicatio.OnIdle

  Revision 1.13  2000/05/11 22:04:16  lazarus
  MWE:
    + Added messagequeue
    * Recoded SendMessage and Peekmessage
    + Added postmessage
    + added DeliverPostMessage

  Revision 1.12  2000/05/10 22:52:59  lazarus
  MWE:
    = Moved some global api stuf to gtkobject

  Revision 1.11  2000/05/10 02:32:34  lazarus
  Put ERRORs and WARNINGs back to writelns.                CAW

  Revision 1.10  2000/05/10 01:45:12  lazarus
  Replaced writelns with Asserts.
  Put ERROR and WARNING messages back to writelns.            CAW

  Revision 1.9  2000/05/09 18:37:02  lazarus
  *** empty log message ***

  Revision 1.8  2000/05/08 16:07:32  lazarus
  fixed screentoclient and clienttoscreen
  Shane



  Revision 1.7  2000/05/08 15:56:59  lazarus
  MWE:
    + Added support for mwedit92 in Makefiles
    * Fixed bug # and #5 (Fillrect)
    * Fixed labelsize in ApiWizz
    + Added a call to the resize event in WMWindowPosChanged

  Revision 1.6  2000/05/08 12:54:20  lazarus
  Removed some writeln's
  Added alignment for the TLabel.  Isn't working quite right.
  Added the shell code for WindowFromPoint and GetParent.
  Added FindLCLWindow
  Shane


  Revision 1.5  2000/05/03 00:27:05  lazarus
  MWE:
    + First rollout of the API wizzard.

  Revision 1.4  2000/04/10 14:03:07  lazarus
  Added SetProp and GetProp winapi calls.
  Added ONChange to the TEdit's published property list.
  Shane

  Revision 1.3  2000/04/07 16:59:55  lazarus
  Implemented GETCAPTURE and SETCAPTURE along with RELEASECAPTURE.
  Shane

  Revision 1.2  2000/03/31 18:41:03  lazarus
  Implemented MessageBox / Application.MessageBox calls. No icons yet, though...

  Revision 1.1  2000/03/30 22:51:43  lazarus
  MWE:
    Moved from ../../lcl

  Revision 1.62  2000/03/30 21:57:44  lazarus
  MWE:
    + Added some general functions to Get/Set the Main/Fixed/CoreChild
      widget
    + Started with graphic scalig/depth stuff. This is way from finished

  Hans-Joachim Ott <hjott@compuserve.com>:
    + Added some improvements for TMEMO

  Revision 1.61  2000/03/30 18:07:54  lazarus
  Added some drag and drop code
  Added code to change the unit name when it's saved as a different name.  Not perfect yet because if you are in a comment it fails.

  Shane

  Revision 1.60  2000/03/28 22:47:49  lazarus
  MWE:
    Started with the blt function family

  Revision 1.59  2000/03/22 18:49:51  lazarus
  Initial work for getting transparent speedbutton glyphs
  Shane

  Revision 1.58  2000/03/22 17:09:30  lazarus
  *** empty log message ***

  Revision 1.57  2000/03/19 23:01:43  lazarus
  MWE:
    = Changed splashscreen loading/colordepth
    = Chenged Save/RestoreDC to platform  dependent, since they are
      relative to a DC

  Revision 1.56  2000/03/17 19:19:58  lazarus
  Added Hans Ott's code for TMemo
  Shane

  Revision 1.55  2000/03/17 17:07:00  lazarus
  Added images to speedbuttons
  Shane

  Revision 1.54  2000/03/16 23:58:46  lazarus
  MWE:
    Added TPixmap for XPM support

  Revision 1.53  2000/03/15 20:15:32  lazarus
  MOdified TBitmap but couldn't get it to work
  Shane

  Revision 1.52  2000/03/15 01:09:59  lazarus
  MWE:
    + Removed comment on LM_IMAGECHANGED in TgtkObject.IntSendMessage3
       it does compile (compiler hickup ?)

  Revision 1.51  2000/03/15 00:51:58  lazarus
  MWE:
    + Added LM_Paint on expose
    + Added forced creation of gdkwindow if needed
    ~ Modified DrawFrameControl
    + Added BF_ADJUST support on DrawEdge
    - Commented out LM_IMAGECHANGED in TgtkObject.IntSendMessage3
       (It did not compile)

  Revision 1.50  2000/03/14 21:18:23  lazarus
  Added the ability to click on the speedbuttons
  Shane

  Revision 1.48  2000/03/10 18:31:10  lazarus
  Added TSpeedbutton code
  Shane

  Revision 1.47  2000/03/09 23:47:58  lazarus
  MWE:
    * Fixed colorcache
    * Fixed black window in new editor
    ~ Did some cosmetic stuff

  From Peter Dyson <peter@skel.demon.co.uk>:
    + Added Rect api support functions
    + Added the start of ScrollWindowEx

  Revision 1.46  2000/03/08 23:57:38  lazarus
  MWE:
    Added SetSysColors
    Fixed TEdit text bug (thanks to hans-joachim ott <hjott@compuserve.com>)
    Finished GetKeyState
    Added changes from Peter Dyson <peter@skel.demon.co.uk>
    - a new GetSysColor
    - some improvements on ExTextOut

  Revision 1.45  2000/03/07 16:52:58  lazarus
  Fixxed a problem with the main.pp unit determining a new files FORM name.
  Shane

  Revision 1.44  2000/03/06 00:05:05  lazarus
  MWE: Added changes from Peter Dyson <peter@skel.demon.co.uk> for a new
    release of mwEdit (0.92)

  Revision 1.43  2000/03/03 22:58:26  lazarus
  MWE:
    Fixed focussing problem.
      LM-FOCUS was bound to the wrong signal
    Added GetKeyState api func.
      Now LCL knows if shift/trl/alt is pressed (might be handy for keyboard
      selections ;-)

  Revision 1.42  2000/02/26 23:31:50  lazarus
  MWE:
    Fixed notebook crash on insert
    Fixed loadfont problem for win32 (tleast now a fontname is required)

  Revision 1.41  2000/02/22 23:26:13  lazarus
  MWE: Fixed cursor movement in editor
       Started on focus problem

  Revision 1.40  2000/02/22 21:51:40  lazarus
  MWE: Removed some double (or triple) event declarations.
       The latest compiler doesn't like it

  Revision 1.39  2000/02/18 19:38:53  lazarus
  Implemented TCustomForm.Position
  Better implemented border styles. Still needs some tweaks.
  Changed TComboBox and TListBox to work again, at least partially.
  Minor cleanups.

  Revision 1.38  2000/01/31 20:00:21  lazarus
  Added code for Application.ProcessMessages.  Needs work.
  Added TScreen.Width and TScreen.Height.  Added the code into
  GetSystemMetrics for these two properties.
  Shane

  Revision 1.37  2000/01/26 19:16:24  lazarus
  Implemented TPen.Style properly for GTK. Done SelectObject for pen objects.
  Misc bug fixes.
  Corrected GDK declaration for gdk_gc_set_slashes.

  Revision 1.36  2000/01/25 23:51:14  lazarus
  MWE:
    Added more Caret functionality.
    Removed old ifdef stuff from the editor

  Revision 1.35  2000/01/25 22:04:27  lazarus
  MWE:
    The first primitive Caret functions are getting visible

  Revision 1.34  2000/01/25 00:38:25  lazarus
  MWE:
    Added GetFocus

  Revision 1.33  2000/01/22 20:07:47  lazarus
  Some cleanups. It needs much more cleanup than this.
  Worked around a compiler bug (?) in mwCustomEdit.
  Reverted some changes to font generation and increased font size.

  Revision 1.32  2000/01/18 22:18:35  lazarus

  Moved bitmap creation into appropriate place. Cleaned up a bit.
  Finished DeleteObject procedure.

  Revision 1.31  2000/01/18 21:47:00  lazarus
  Added OffSetRec

  Revision 1.30  2000/01/17 23:33:08  lazarus
  MWE:
    fixed: nil pointer reference in DeleteObject
    fixed: some trace info didn't start with 'trace:'

  Revision 1.29  2000/01/17 20:36:25  lazarus
  Fixed Makefile again.
  Made implementation of TScreen and screen info saner.
  Began to implemented DeleteObject in GTKWinAPI.
  Fixed a bug in GDI allocation which in turn fixed A LOT of other bugs :-)

  Revision 1.28  2000/01/16 23:23:07  lazarus
  MWE:
    Added/completed scrollbar API funcs

  Revision 1.27  2000/01/14 21:47:04  lazarus
  Commented out SHOWCARET.  Not sure how to implement yet.  Seems like I may need to draw it myself and therefore will need to create a timer and draw a line, then copy the pixmap over the line to erase it.......not sure yet.
  Shane

  Revision 1.26  2000/01/13 22:44:05  lazarus
  MWE:
    Created/updated net gtkwidget for TWinControl decendants
      also improved foccusing on such a control

  Revision 1.25  2000/01/12 22:13:07  lazarus
  Modified ShowCaret.  Still not working.
  Shane

  Revision 1.24  2000/01/11 20:50:32  lazarus
  Added some code for SETCURSOR.  Doesn't work perfect yet but getting there.
  Shane

  Revision 1.22  2000/01/10 21:24:12  lazarus
  Minor cleanup and changes.

  Revision 1.21  2000/01/07 21:14:13  lazarus
  Added code for getwindowlong and setwindowlong.
  Shane

  Revision 1.20  1999/12/21 21:35:54  lazarus
  committed the latest toolbar code.  Currently it doesn't appear anywhere and I have to get it to add buttons correctly through (I think) setstyle.  I think I'll implement the LM_TOOLBARINSERTBUTTON call there.
  Shane

  Revision 1.19  1999/12/21 00:37:19  lazarus
  MWE:
    Fixed SetTextColor

  Revision 1.18  1999/12/21 00:07:06  lazarus
  MWE:
    Some fixes
    Completed a bit of DraWEdge

  Revision 1.17  1999/12/20 21:01:13  lazarus
  Added a few things for compatability with Delphi and TToolbar
  Shane

  Revision 1.16  1999/12/18 18:27:32  lazarus
  MWE:
    Rearranged some events to get a LM_SIZE, LM_MOVE and LM_WINDOWPOSCHANGED
    Initialized the TextMetricstruct to zeros to clear unset values
    Get mwEdit to show more than one line
    Fixed some errors in earlier commits

  Revision 1.15  1999/12/14 21:07:12  lazarus
  Added more stuff for TToolbar
  Shane

  Revision 1.14  1999/12/14 01:08:56  lazarus
  MWE:
    Started GetTextMetrics

  Revision 1.13  1999/12/14 00:16:43  lazarus
  MWE:
    Renamed LM... message handlers to WM... to be compatible and to
      get more edit parts to compile
    Started to implement GetSystemMetrics
    Removed some Lazarus specific parts from mwEdit

  Revision 1.12  1999/12/06 20:41:14  lazarus
  Miinor debugging changes.
  Shane

  Revision 1.11  1999/12/03 00:26:47  lazarus
  MWE:
    fixed control location
    added gdiobject reference counter

  Revision 1.10  1999/12/02 19:00:59  lazarus
  MWE:
    Added (GDI)Pen
    Changed (GDI)Brush
    Changed (GDI)Font (color)
    Changed Canvas to use/create pen/brush/font
    Hacked mwedit to allow setting the number of chars (till it get a WM/LM_SIZE event)
    The editor shows a line !

  Revision 1.9  1999/11/29 00:46:47  lazarus
  MWE:
    Added TBrush as gdiobject
    commented out some more mwedit MWE_FPC ifdefs

  Revision 1.8  1999/11/25 23:45:08  lazarus
  MWE:
    Added font as GDIobject
    Added some API testcode to testform
    Commented out some more IFDEFs in mwCustomEdit

  Revision 1.7  1999/11/19 01:09:43  lazarus
  MWE:
    implemented TCanvas.CopyRect
    Added StretchBlt
    Enabled creation of TCustomControl.Canvas
    Added a temp hack in TWinControl.Repaint to get a LM_PAINT

  Revision 1.6  1999/11/18 00:13:08  lazarus
  MWE:
    Partly Implemented SelectObject
    Added  ExTextOut
    Added  GetTextExtentPoint
    Added  TCanvas.TextExtent/TextWidth/TextHeight
    Added  TSize and HPEN

  Revision 1.5  1999/11/17 01:16:40  lazarus
  MWE:
    Added some more API stuff
    Added an initial TBitmapCanvas
    Added some DC stuff
    Changed and commented out, original gtk linedraw/rectangle code. This
      is now called through the winapi wrapper.

  Revision 1.4  1999/11/16 01:32:22  lazarus
  MWE:
    Added some more DC functionality

}

