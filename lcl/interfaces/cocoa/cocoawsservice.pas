unit CocoaWSService;

{$mode objfpc}{$H+}
{$modeswitch objectivec2}
{$include cocoadefines.inc}

interface

uses
  Classes, SysUtils, Types, LCLType, LCLProc,
  Forms, Menus, GraphType,
  LCLIntf, IntfGraphics,
  MacOSAll, CocoaAll,
  CocoaGDIObjects, CocoaMenus, CocoaConst, CocoaUtils, Cocoa_Extra;

type

  { TCocoaWidgetSetState }

  TCocoaWidgetSetState = class
  public
    KeyWindow: NSWindow;
    KillingFocus: Boolean;
    CaptureControl: HWND;
    FSendingScrollWheelCount: Integer;
  public
    function isSendingScrollWheelFromInterface: Boolean;
  end;

  { TCocoaWidgetSetGDIObject }

  TCocoaWidgetSetGDIObject = class
  public
    nullBrush: HBRUSH;
    blackBrush: HBRUSH;
    ltGrayBrush: HBRUSH;
    grayBrush: HBRUSH;
    dkGrayBrush: HBRUSH;
    whiteBrush: HBRUSH;

    nullPen: HPEN;
    blackPen: HPEN;
    whitePen: HPEN;

    systemFont: HFONT;
    fixedFont: HFONT;

    sysColorBrushes: array[0..MAX_SYS_COLORS] of HBrush;
  private
    procedure initStockItems;
    procedure freeStockItems;
    procedure freeSysColorBrushes;
  public
    constructor Create;
    destructor Destroy; override;
  end;

  { TCocoaWidgetSetService }

  TCocoaWidgetSetService = class

  // on MacOS, the system notifies the APP to open the files by calling
  // TAppDelegate.application_openFiles(). for example, double-click
  // the associated files in Finder to open the APP.
  // at this time, the MainForm may not have been created, and the notifies
  // information about the files will be lost.
  // including Lazarus IDE itself will also be affected by this issue on startup.
  // so save it in _waitingDropFiles first, DropWaitingFiles() will be called
  // in TCocoaWidgetSet.AppRun().
  private
    _readyDropFiles: Boolean;
    _waitingDropFiles: NSMutableArray;
  public
    procedure setReadyDropFiles; inline;
    procedure dropWaitingFiles;
    procedure tryDropFiles( const filenames: NSArray );

  // collecting objects that needs to be released AFTER an event
  // has been processed
  private
    _waitingReleasedLCLObjects: TList;
  public
    procedure addToBeReleasedLCLObjects(const obj: TObject);
    procedure releaseWaitingLCLObjects(const fromIdx: integer);
    function countWaitingReleasedLCLObjects: Integer;

  // Cocoa Autorelease Main Pool
  private
    _autoreleaseMainPool : NSAutoreleasePool;
  public
    procedure initAutoreleaseMainPool;
    procedure finalAutoreleaseMainPool;

  // util
  public
    function deleteObject(GDIObject: HGDIOBJ): Boolean;

  // callback
  public
    procedure OnWakeMainThread(Sender: TObject);

  public
    constructor Create;
    destructor Destroy; override;
  end;

  { TCocoaWidgetSetGDIService }

  TCocoaWidgetSetGDIService = class
  public
    function fillLCLImageDescription(out ADesc: TRawImageDescription; ABitmap: TCocoaBitmap): Boolean;
    function createLCLRawImage(out ARawImage: TRawImage; ABitmap, AMask: TCocoaBitmap; ARect: PRect = nil): Boolean;
    function getCocoaBitmapType(ADesc: TRawImageDescription; out bmpType: TCocoaBitmapType): Boolean;
    function getPixelData(AImage: CGImageRef; out bitmapByteCount: PtrUInt): Pointer;
    function create32BitAlphaCocoaBitmap(ABitmap, AMask: TCocoaBitmap): TCocoaBitmap;
  end;

  { TCocoaWidgetSetMenuService }

  TCocoaWidgetSetMenuService = class
  private
    _mainFormMenu: NSMenu;
  public
    mainMenuEnabled: Boolean; // the latest main menu status
    prevMenu : NSMenu;
    prevLCLMenu : TMenu;
    currentLCLMenu: TMenu;
    prevMenuEnabled: Boolean; // previous mainmenu status
  public
    procedure DoSetMainMenu(AMenu: NSMenu; ALCLMenu: TMenu);
    procedure SetMainMenu(const AMenu: HMENU; const ALCLMenu: TMenu);
  public
    destructor Destroy; override;
  end;

var
  CocoaWidgetSetState: TCocoaWidgetSetState;
  CocoaWidgetSetService: TCocoaWidgetSetService;
  CocoaWidgetSetGDIService: TCocoaWidgetSetGDIService;
  CocoaWidgetSetMenuService: TCocoaWidgetSetMenuService;

implementation

{ TCocoaWidgetSetState }

function TCocoaWidgetSetState.isSendingScrollWheelFromInterface: Boolean;
begin
  Result:= self.FSendingScrollWheelCount > 0;
end;

{ TCocoaWidgetSetGDIObject }

procedure TCocoaWidgetSetGDIObject.initStockItems;
var
  LogBrush: TLogBrush;
  logPen: TLogPen;
  pool: NSAutoreleasePool;
begin
  FillChar(LogBrush, SizeOf(TLogBrush),0);
  LogBrush.lbStyle := BS_NULL;
  self.nullBrush := HBrush(TCocoaBrush.Create(LogBrush, True));

  LogBrush.lbStyle := BS_SOLID;
  LogBrush.lbColor := $000000;
  self.blackBrush := HBrush(TCocoaBrush.Create(LogBrush, True));

  LogBrush.lbColor := $C0C0C0;
  self.ltGrayBrush := HBrush(TCocoaBrush.Create(LogBrush, True));

  LogBrush.lbColor := $808080;
  self.grayBrush := HBrush(TCocoaBrush.Create(LogBrush, True));

  LogBrush.lbColor := $404040;
  self.dkGrayBrush := HBrush(TCocoaBrush.Create(LogBrush, True));

  LogBrush.lbColor := $FFFFFF;
  self.whiteBrush := HBrush(TCocoaBrush.Create(LogBrush, True));

  LogPen.lopnStyle := PS_NULL;
  LogPen.lopnWidth := Types.Point(0, 0); // create cosmetic pens
  LogPen.lopnColor := $FFFFFF;
  self.nullPen := HPen(TCocoaPen.Create(LogPen, True));

  LogPen.lopnStyle := PS_SOLID;
  self.whitePen := HPen(TCocoaPen.Create(LogPen, True));

  LogPen.lopnColor := $000000;
  self.blackPen := HPen(TCocoaPen.Create(LogPen, True));

  self.systemFont := HFont(TCocoaFont.CreateDefault(True));
  pool := NSAutoreleasePool.alloc.init;
  self.fixedFont := HFont(TCocoaFont.Create(NSFont.userFixedPitchFontOfSize(0), True));
  pool.release;
end;

procedure TCocoaWidgetSetGDIObject.freeStockItems;

  procedure DeleteAndNilObject(var h: HGDIOBJ);
  begin
    if h <> 0 then
      TCocoaGDIObject(h).Global := False;
      CocoaWidgetSetService.deleteObject(h);
    h := 0;
  end;

begin
  DeleteAndNilObject(self.nullBrush);
  DeleteAndNilObject(self.blackBrush);
  DeleteAndNilObject(self.ltGrayBrush);
  DeleteAndNilObject(self.grayBrush);
  DeleteAndNilObject(self.dkGrayBrush);
  DeleteAndNilObject(self.whiteBrush);

  DeleteAndNilObject(self.nullPen);
  DeleteAndNilObject(self.blackPen);
  DeleteAndNilObject(self.whitePen);

  DeleteAndNilObject(self.fixedFont);
  DeleteAndNilObject(self.systemFont);
end;

procedure TCocoaWidgetSetGDIObject.freeSysColorBrushes;

  procedure DeleteAndNilObject(var h: HBrush);
  begin
    if h <> 0 then
    begin
      TCocoaBrush(h).Free;
      h := 0;
    end;
  end;

var
  i: integer;
begin
  for i := Low(self.sysColorBrushes) to High(self.sysColorBrushes) do
    DeleteAndNilObject(self.sysColorBrushes[i]);
end;

constructor TCocoaWidgetSetGDIObject.Create;
begin
  self.initStockItems;
end;

destructor TCocoaWidgetSetGDIObject.Destroy;
begin
  self.freeStockItems;
  self.freeSysColorBrushes;
end;

{ TCocoaWidgetSetService }

procedure TCocoaWidgetSetService.setReadyDropFiles;
begin
  _readyDropFiles:= True;
end;

procedure TCocoaWidgetSetService.dropWaitingFiles;
var
  lFiles: array of string;
  lNSStr: NSString;
  i: Integer;
begin
  if _waitingDropFiles.count = 0 then
    exit;

  SetLength(lFiles, _waitingDropFiles.count);
  for i := 0 to _waitingDropFiles.count-1 do
  begin
    lNSStr := NSString(_waitingDropFiles.objectAtIndex(i));
    lFiles[i] := NSStringToString(lNSStr);
  end;
  Application.IntfDropFiles(lFiles);
  if Application.MainForm<>nil then
    Application.MainForm.IntfDropFiles(lFiles);

  _waitingDropFiles.removeAllObjects;
end;

procedure TCocoaWidgetSetService.tryDropFiles(const filenames: NSArray);
begin
  _waitingDropFiles.addObjectsFromArray(filenames);
  if _readyDropFiles then
    dropWaitingFiles;
end;

procedure TCocoaWidgetSetService.addToBeReleasedLCLObjects(const obj: TObject);
begin
  // let's try to find an object. Do not add a duplicate
  if (_waitingReleasedLCLObjects.IndexOf(Obj)>=0) then Exit;
  _waitingReleasedLCLObjects.Add(obj);
end;

procedure TCocoaWidgetSetService.releaseWaitingLCLObjects(const fromIdx: integer);
var
  i  : integer;
begin
  for i := fromIdx to _waitingReleasedLCLObjects.Count - 1 do
  begin
    TObject(_waitingReleasedLCLObjects[i]).Free;
    _waitingReleasedLCLObjects[i]:=nil;
  end;
  _waitingReleasedLCLObjects.Pack;
end;

function TCocoaWidgetSetService.countWaitingReleasedLCLObjects: Integer;
begin
  Result := _waitingReleasedLCLObjects.Count;
end;

procedure TCocoaWidgetSetService.initAutoreleaseMainPool;
begin
  // MacOSX 10.6 reports a lot of warnings during initialization process
  // adding the autorelease pool for the whole Cocoa widgetset
  _autoreleaseMainPool:= NSAutoreleasePool.alloc.init;
end;

procedure TCocoaWidgetSetService.finalAutoreleaseMainPool;
begin
  if Assigned(_autoreleaseMainPool) then begin
    _autoreleaseMainPool.release;
    _autoreleaseMainPool := nil;
  end;
end;

function TCocoaWidgetSetService.deleteObject(GDIObject: HGDIOBJ): Boolean;
const
  SName = 'TCocoaWidgetSetService.deleteObject';
var
  gdi: TCocoaGDIObject;
begin
  Result := False;
  if GDIObject = 0 then
    Exit(True);

  gdi := CheckGDIOBJ(GdiObject);

  if not Assigned(gdi) then
  begin
    DebugLn(SName, ' Error - GDIObject: ' +  DbgSName(gdi) + ' is unknown!');
    Exit;
  end;

  if gdi.Global then
  begin
    // global brushes can be cached, so just exit here since we will free the resource later on
    //DebugLn(SName, ' Error - GDIObject: ' +  DbgSName(gdi) + ' is global!');
    Exit;
  end;

  if gdi.RefCount <> 1 then
  begin
    DebugLn(SName, 'Error - GDIObject: ' + DbgSName(gdi) + ' is still selected!');
    Exit;
  end;

  gdi.Destroy;
  Result := True;
end;

procedure TCocoaWidgetSetService.OnWakeMainThread(Sender: TObject);
begin
  NSApp.performSelectorOnMainThread_withObject_waitUntilDone(
       ObjCSelector('lclSyncCheck:'), nil, false);
end;

constructor TCocoaWidgetSetService.Create;
begin
  _waitingDropFiles:= NSMutableArray.new;
  _waitingReleasedLCLObjects := TList.Create;
end;

destructor TCocoaWidgetSetService.Destroy;
begin
  _waitingDropFiles.release;
  _waitingReleasedLCLObjects.Free;
end;

{ TCocoaWidgetSetGDIService }

{------------------------------------------------------------------------------
  Method:  TCocoaWidgetSetGDIService.fillLCLImageDescription

  Creates a rawimage description for a cocoabitmap
 ------------------------------------------------------------------------------}
function TCocoaWidgetSetGDIService.fillLCLImageDescription(out
  ADesc: TRawImageDescription; ABitmap: TCocoaBitmap): Boolean;
var
  Prec, Shift: Byte;
  BPR: Integer;
begin
  ADesc.Init;

  case ABitmap.BitmapType of
    cbtMono, cbtGray: ADesc.Format := ricfGray;
  else
    ADesc.Format := ricfRGBA;
  end;

  ADesc.Width := Round(ABitmap.image.size.width);
  ADesc.Height := Round(ABitmap.image.size.Height);

  //ADesc.PaletteColorCount := 0;

  ADesc.BitOrder := riboReversedBits;
  ADesc.ByteOrder := riboMSBFirst;

  BPR := ABitmap.BytesPerRow;
  if BPR and $F = 0 then ADesc.LineEnd := rileDQWordBoundary     // 128bit aligned
  else if BPR and $7 = 0 then ADesc.LineEnd := rileQWordBoundary //  64bit aligned
  else if BPR and $3 = 0 then ADesc.LineEnd := rileWordBoundary  //  32bit aligned
  else if BPR and $1 = 0 then ADesc.LineEnd := rileByteBoundary  //   8bit aligned
  else ADesc.LineEnd := rileTight;

  ADesc.LineOrder := riloTopToBottom;
  ADesc.BitsPerPixel := ABitmap.BitsPerPixel;

  ADesc.MaskBitOrder := riboReversedBits;
  ADesc.MaskBitsPerPixel := 1;
  ADesc.MaskLineEnd := rileByteBoundary;
  // ADesc.MaskShift := 0;

  ADesc.Depth := ABitmap.Depth;
  Prec := ABitmap.BitsPerSample;

  ADesc.RedPrec := Prec;
  ADesc.GreenPrec := Prec;
  ADesc.BluePrec := Prec;

  // gray or mono
  if ADesc.Format = ricfGray then begin
    Result := true;
    Exit;
  end;

  // alpha
  if ABitmap.BitmapType in [cbtARGB, cbtRGBA] then
    ADesc.AlphaPrec := Prec;

  case ABitmap.BitmapType of
    cbtRGB: begin
      Shift := 24 - Prec;
      ADesc.RedShift := Shift;
      Dec(Shift, Prec);
      ADesc.GreenShift := Shift;
      Dec(Shift, Prec);
      ADesc.BlueShift := Shift;
    end;
    cbtARGB: begin
      Shift := 32 - Prec;
      ADesc.AlphaShift := Shift;
      Dec(Shift, Prec);
      ADesc.RedShift := Shift;
      Dec(Shift, Prec);
      ADesc.GreenShift := Shift;
      Dec(Shift, Prec);
      ADesc.BlueShift := Shift;
    end;
    cbtRGBA: begin
      Shift := 32 - Prec;
      ADesc.RedShift := Shift;
      Dec(Shift, Prec);
      ADesc.GreenShift := Shift;
      Dec(Shift, Prec);
      ADesc.BlueShift := Shift;
      Dec(Shift, Prec);
      ADesc.AlphaShift := Shift;
    end;
  end;

  Result := True;
end;

{------------------------------------------------------------------------------
  Method:  TCocoaWidgetSetGDIService.createLCLRawImage

  Creates a rawimage description for a cocoabitmap
 ------------------------------------------------------------------------------}
function TCocoaWidgetSetGDIService.createLCLRawImage(out
  ARawImage: TRawImage; ABitmap, AMask: TCocoaBitmap; ARect: PRect): Boolean;
var
  lBitmapData: PByte;
begin
  FillChar(ARawImage, SizeOf(ARawImage), 0);
  fillLCLImageDescription(ARawImage.Description, ABitmap);

  ARawImage.DataSize := ABitmap.DataSize;
  ReAllocMem(ARawImage.Data, ARawImage.DataSize);
  lBitmapData := ABitmap.GetNonPreMultipliedData();
  if ARawImage.DataSize > 0 then
    System.Move(lBitmapData^, ARawImage.Data^, ARawImage.DataSize);

  Result := True;

  if AMask = nil then
  begin
    ARawImage.Description.MaskBitsPerPixel := 0;
    Exit;
  end;

  if AMask.Depth > 1
  then begin
    DebugLn('[WARNING] RawImage_FromCocoaBitmap: AMask.Depth > 1');
    Exit;
  end;

  ARawImage.MaskSize := AMask.DataSize;
  ReAllocMem(ARawImage.Mask, ARawImage.MaskSize);
  if ARawImage.MaskSize > 0 then
    System.Move(AMask.Data^, ARawImage.Mask^, ARawImage.MaskSize);
end;

function TCocoaWidgetSetGDIService.getCocoaBitmapType(
  ADesc: TRawImageDescription; out bmpType: TCocoaBitmapType): Boolean;
begin
  Result := False;

  if ADesc.Format = ricfGray
  then
  begin
    if ADesc.Depth = 1 then bmpType := cbtMono
    else bmpType := cbtGray;
  end
  else if ADesc.Depth = 1
  then bmpType := cbtMono
  else if ADesc.AlphaPrec <> 0
  then begin
    if ADesc.ByteOrder = riboMSBFirst
    then begin
      if  (ADesc.AlphaShift = 24)
      and (ADesc.RedShift   = 16)
      and (ADesc.GreenShift = 8 )
      and (ADesc.BlueShift  = 0 )
      then bmpType := cbtARGB
      else
      if  (ADesc.AlphaShift = 24)
      and (ADesc.RedShift   = 0 )
      and (ADesc.GreenShift = 8 )
      and (ADesc.BlueShift  = 16)
      then bmpType := cbtABGR
      else
      if  (ADesc.AlphaShift = 0 )
      and (ADesc.RedShift   = 24)
      and (ADesc.GreenShift = 16)
      and (ADesc.BlueShift  = 8 )
      then bmpType := cbtRGBA
      else
      if  (ADesc.AlphaShift = 0 )
      and (ADesc.RedShift   = 8 )
      and (ADesc.GreenShift = 16)
      and (ADesc.BlueShift  = 24)
      then bmpType := cbtBGRA
      else Exit;
    end
    else begin
      if  (ADesc.AlphaShift = 0 )
      and (ADesc.RedShift   = 8 )
      and (ADesc.GreenShift = 16)
      and (ADesc.BlueShift  = 24)
      then bmpType := cbtARGB
      else
      if  (ADesc.AlphaShift = 0 )
      and (ADesc.RedShift   = 24)
      and (ADesc.GreenShift = 16)
      and (ADesc.BlueShift  = 8 )
      then bmpType := cbtABGR
      else
      if  (ADesc.AlphaShift = 24)
      and (ADesc.RedShift   = 0 )
      and (ADesc.GreenShift = 8 )
      and (ADesc.BlueShift  = 16)
      then bmpType := cbtRGBA
      else
      if  (ADesc.AlphaShift = 24)
      and (ADesc.RedShift   = 16)
      and (ADesc.GreenShift = 8 )
      and (ADesc.BlueShift  = 0 )
      then bmpType := cbtBGRA
      else Exit;
    end;
  end
  else begin
    bmpType := cbtRGB;
  end;

  Result := True;
end;

{------------------------------------------------------------------------------
  Method:  TCocoaWidgetSetGDIService.getPixelData

  Used by RawImage_FromDevice. Copies the data from a CGImageRef into a local
  buffer.

  The buffer is created using GetMem, and the caller is responsible for using
  FreeMem to free the returned pointer.

  This function throws exceptions in case of errors and may return a nil pointer.
 ------------------------------------------------------------------------------}
function TCocoaWidgetSetGDIService.getPixelData(AImage: CGImageRef; out
  bitmapByteCount: PtrUInt): Pointer;
var
  bitmapData: Pointer;
  context: CGContextRef = nil;
  colorSpace: CGColorSpaceRef;
  bitmapBytesPerRow, pixelsWide, pixelsHigh: PtrUInt;
  imageRect: CGRect;
begin
  Result := nil;

   // Get image width, height. The entire image is used.
  pixelsWide := CGImageGetWidth(AImage);
  pixelsHigh := CGImageGetHeight(AImage);
  imageRect.origin.x := 0.0;
  imageRect.origin.y := 0.0;
  imageRect.size.width := pixelsWide;
  imageRect.size.height := pixelsHigh;

  // The target format is fixed in ARGB, DQWord alignment, with 32-bits depth and
  // 8-bits per channel, the default image format on the LCL
  bitmapBytesPerRow   := ((pixelsWide * 4) + $F) and not PtrUInt($F);
  bitmapByteCount     := (bitmapBytesPerRow * pixelsHigh);

  // Use the generic RGB color space.
  colorSpace := CGColorSpaceCreateWithName(kCGColorSpaceGenericRGB);
  if (colorSpace = nil) then
    raise Exception.Create('Unable to create CGColorSpaceRef');

  // Allocate memory for image data. This is the destination in memory
  // where any drawing to the bitmap context will be rendered.
  bitmapData := System.GetMem( bitmapByteCount );
  if (bitmapData = nil) then
    raise Exception.Create('Unable to allocate memory');

  { Creates the bitmap context.

    Regardless of what the source image format is, it will be converted
    over to the format specified here by CGBitmapContextCreate. }
  context := CGBitmapContextCreate(bitmapData,
                                   pixelsWide,
                                   pixelsHigh,
                                   8,      // bits per component
                                   bitmapBytesPerRow,
                                   colorSpace,
                                   kCGImageAlphaNoneSkipFirst); // The function fails with kCGImageAlphaFirst
  if (context = nil) then
  begin
    System.FreeMem(bitmapData);
    raise Exception.Create('Unable to create CGContextRef');
  end;

  // Draw the image to the bitmap context. Once we draw, the memory
  // allocated for the context for rendering will then contain the
  // raw image data in the specified color space.
  CGContextDrawImage(context, imageRect, AImage);

  // Now we can get a pointer to the image data associated with the context.
  // ToDo: Verify if we should copy this data to a new buffer
  Result := CGBitmapContextGetData(context);

  { Clean-up }
  CGColorSpaceRelease(colorSpace);
  CGContextRelease(context);
end;

function TCocoaWidgetSetGDIService.create32BitAlphaCocoaBitmap(ABitmap,
  AMask: TCocoaBitmap): TCocoaBitmap;
var
  ARawImage: TRawImage;
  Desc: TRawImageDescription absolute ARawimage.Description;

  ImgHandle, ImgMaskHandle: HBitmap;
  ImagePtr: PRawImage;
  DevImage: TRawImage;
  DevDesc: TRawImageDescription;
  SrcImage, DstImage: TLazIntfImage;
  W, H: Integer;
begin
  Result := nil;

  if not LCLIntf.RawImage_FromBitmap(ARawImage, HBITMAP(ABitmap), HBITMAP(AMask)) then
    Exit;

  ImgMaskHandle := 0;

  W := Desc.Width;
  if W < 1 then W := 1;
  H := Desc.Height;
  if H < 1 then H := 1;

  QueryDescription(DevDesc, [riqfRGB, riqfAlpha], W, H);

  if DevDesc.IsEqual(Desc)
  then begin
    // image is compatible, so use it
    DstImage := nil;
    ImagePtr := @ARawImage;
  end
  else begin
    // create compatible copy
    SrcImage := TLazIntfImage.Create(ARawImage, False);
    DstImage := TLazIntfImage.Create(0,0,[]);
    DstImage.DataDescription := DevDesc;
    DstImage.CopyPixels(SrcImage);
    SrcImage.Free;
    DstImage.GetRawImage(DevImage);
    ImagePtr := @DevImage;
  end;

  try
    if not LCLIntf.RawImage_CreateBitmaps(ImagePtr^, ImgHandle, ImgMaskHandle, True) then Exit;

    Result := TCocoaBitmap(ImgHandle);
  finally
    ARawImage.FreeData;
    DstImage.Free;
  end;
end;

{ TCocoaWidgetSetMenuService }

procedure TCocoaWidgetSetMenuService.DoSetMainMenu(AMenu: NSMenu; ALCLMenu: TMenu);
var
  i: Integer;
  lCurItem: TMenuItem;
  lMenuObj: NSObject;
  lCocoaMenu: TCocoaMenu absolute AMenu;
  appleMenuFound: Boolean = false;
begin
  if Assigned(prevMenu) then prevMenu.release;
  prevMenu := NSApplication(NSApp).mainMenu;
  prevMenu.retain;

  prevLCLMenu := currentLCLMenu;
  currentLCLMenu := ALCLMenu;

  if NOT Assigned(_mainFormMenu) or (_mainFormMenu.numberOfItems=0) then begin
    _mainFormMenu.release;
    _mainFormMenu:= AMenu;
    _mainFormMenu.retain;
  end;

  if (ALCLMenu = nil) or not ALCLMenu.HandleAllocated then begin
    lCocoaMenu:= TCocoaMenu( _mainFormMenu );
    if NOT Assigned(lCocoaMenu) then
      lCocoaMenu:= TCocoaMenu.new.autorelease;
    NSApp.setMainMenu( lCocoaMenu );
    Exit;
  end;

  // Find the Apple menu, if the user provided any by setting the Caption to 
  // Some older docs say we should use setAppleMenu to obtain the Services/Hide/Quit items,
  // but its now private and in 10.10 it doesn't seam to do anything
  // NSApp.setAppleMenu(NSMenu(lMenuObj));
  for i := 0 to ALCLMenu.Items.Count-1 do
  begin
    lCurItem := ALCLMenu.Items.Items[i];
    if not AMenu.isKindOfClass_(TCocoaMenu) then Break;
    if not lCurItem.HandleAllocated then Continue;

    lMenuObj := NSObject(lCurItem.Handle);
    if not lMenuObj.isKindOfClass_(TCocoaMenuItem) then Continue;
    if TCocoaMenuItem(lMenuObj).isValidAppleMenu() then
    begin
      lCocoaMenu.overrideAppleMenu(TCocoaMenuItem(lMenuObj));
      appleMenuFound:= true;
      Break;
    end;
  end;

  if AMenu.isKindOfClass(TCocoaMenu) then begin
    if NOT appleMenuFound then
      lCocoaMenu.createAppleMenu();
    lCocoaMenu.attachAppleMenu();
  end;

  NSApp.setMainMenu( AMenu );
end;

procedure TCocoaWidgetSetMenuService.SetMainMenu(const AMenu: HMENU; const ALCLMenu: TMenu);
begin
  DoSetMainMenu(NSMenu(AMenu), ALCLMenu);

  prevMenuEnabled := mainMenuEnabled;
  mainMenuEnabled := true;
  TCocoaMenuUtil.toggleAppMenu(true);
  //if not Assigned(ACustomForm.Menu) then ToggleAppMenu(false);

  // for modal windows work around bug, but doesn't work :(
  {$ifdef COCOA_USE_NATIVE_MODAL}
  {if CurModalForm <> nil then
  for i := 0 to lNSMenu.numberOfItems()-1 do
  begin
    lNSMenu.itemAtIndex(i).setTarget(TCocoaWSCustomForm.GetWindowFromHandle(CurModalForm));
  end;}
  {$endif}
end;

destructor TCocoaWidgetSetMenuService.Destroy;
begin
  _mainFormMenu.release;
end;

initialization
  CocoaWidgetSetState:= TCocoaWidgetSetState.Create;
  CocoaWidgetSetGDIService:= TCocoaWidgetSetGDIService.Create;
  CocoaWidgetSetService:= TCocoaWidgetSetService.Create;
  CocoaWidgetSetMenuService:= TCocoaWidgetSetMenuService.Create;

finalization
  FreeAndNil( CocoaWidgetSetMenuService );
  FreeAndNil( CocoaWidgetSetService );
  FreeAndNil( CocoaWidgetSetGDIService );
  FreeAndNil( CocoaWidgetSetState );

end.

