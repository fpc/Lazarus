{%MainUnit fpguiint.pp}
{
 *****************************************************************************
  This file is part of the Lazarus Component Library (LCL)

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************
}
//---------------------------------------------------------------

type

    { TFPGUITimer }

    TFPGUITimer = class
  private
    //FLCLTimer: TTimer;
    FTimer: TfpgTimer;
    FCallback: TWSTimerProc;
  protected
    procedure FPGTimer(Sender: TObject);
  public
    constructor Create(AInterval: Integer; ACallbackFunc: TWSTimerProc);
    destructor  Destroy; override;

    property Timer : TfpgTimer read FTimer;
  end;

{ TFPGUITimer }

procedure TFPGUITimer.FPGTimer(Sender: TObject);
begin
  if Assigned(FCallback) then
    FCallback;
end;

constructor TFPGUITimer.Create(AInterval: Integer; ACallbackFunc: TWSTimerProc);
begin
  FTimer := TfpgTimer.Create(AInterval);
  FTimer.OnTimer:=@FPGTimer;
  FCallback := ACallbackFunc;
  FTimer.Enabled:= True;
end;

destructor TFPGUITimer.Destroy;
begin
  FTimer.Free;
  inherited Destroy;
end;


{------------------------------------------------------------------------------
  Method: TFpGuiWidgetSet.Create
  Params:  None
  Returns: Nothing

  Constructor for the class.
 ------------------------------------------------------------------------------}
constructor TFpGuiWidgetSet.Create;
begin
  inherited Create;

  FpGuiWidgetSet := Self;
end;

{------------------------------------------------------------------------------
  Method: TFpGuiWidgetSet.Destroy
  Params:  None
  Returns: Nothing

  Destructor for the class.
 ------------------------------------------------------------------------------}
destructor TFpGuiWidgetSet.Destroy;
begin
  FpGuiWidgetSet := nil;

  inherited Destroy;
end;

function TFpGuiWidgetSet.DrawFrameControl(DC: HDC; const Rect: TRect; uType,
  uState: Cardinal): Boolean;
var
  ADC: TFPGUIDeviceContext;
  ControlType: Cardinal;
  ControlStyle: Cardinal;
  fpgRect: TfpgRect;
  Style: TfpgButtonFlags;
  (*
  DFC_CAPTION = $01;
  DFC_MENU = $02;
  DFC_SCROLL = $03;
  DFC_BUTTON =  $04;
  DFCS_BUTTONCHECK = 0;
  DFCS_BUTTONRADIOIMAGE = 1;
  DFCS_BUTTONRADIOMASK = 2;
  DFCS_BUTTONRADIO = 4;
  DFCS_BUTTON3STATE = 8;
  DFCS_BUTTONPUSH = 16;
  *)
const
  DFCS_ALLSTATES=DFCS_BUTTONCHECK or DFCS_BUTTONRADIOIMAGE or DFCS_BUTTONRADIOMASK
                or DFCS_BUTTONRADIO or DFCS_BUTTON3STATE or DFCS_BUTTONPUSH;
begin
  Result:=false;
  ADC:=TFPGUIDeviceContext(DC);
  if Assigned(ADC.fpgCanvas) then begin
    ControlType:=uType;
    ControlStyle:=uState and DFCS_ALLSTATES;
    fpgRect:=ADC.PrepareRectOffsets(Rect);
    Case ControlType of
      DFC_BUTTON:
        begin
          if (ControlStyle and DFCS_BUTTONPUSH)=DFCS_BUTTONPUSH then begin
            Style:=[];
            if (uState and DFCS_INACTIVE) <> 0 then
              Style:=Style+[btfIsEmbedded] //Disabled ?
            else
            if (uState and DFCS_PUSHED) <> 0 then
              Style:=Style+[btfIsPressed]
            else
            if (uState and DFCS_HOT) <> 0 then
              Style:=Style+[btfHover];
            ADC.fpgCanvas.DrawButtonFace(fpgRect,Style);
            Result:=true;
          end;
        end;
      else
        Result:=false;
    end;
  end;
end;

{------------------------------------------------------------------------------
  Method: TFpGuiWidgetSet.CreateTimer
  Params:  None
  Returns: Nothing

  Creates a new timer and sets the callback event.
 ------------------------------------------------------------------------------}
function TFpGuiWidgetSet.CreateTimer(Interval: integer; TimerFunc: TWSTimerProc): THandle;
var
  Timer: TFPGUITimer;
begin
  Timer := TFPGUITimer.Create(Interval, TimerFunc);

  Result := PtrInt(Timer);
end;

{------------------------------------------------------------------------------
  Method: TFpGuiWidgetSet.DestroyTimer
  Params:  None
  Returns: Nothing

  Destroys a timer.
 ------------------------------------------------------------------------------}
function TFpGuiWidgetSet.DestroyTimer(TimerHandle: THandle): boolean;
var
  Timer: TFPGUITimer absolute TimerHandle;
begin
  if Timer <> nil then
    Timer.Free;

  Result := True;
end;

function TFpGuiWidgetSet.CreateThemeServices: TThemeServices;
begin
  Result:=TFPGUIThemeServices.Create;
end;

{------------------------------------------------------------------------------
  Method: TFpGuiWidgetSet.AppInit
  Params:  None
  Returns: Nothing

  Initializes the application
 ------------------------------------------------------------------------------}
procedure TFpGuiWidgetSet.AppInit(var ScreenInfo: TScreenInfo);
begin
  fpgApplication.Initialize;

  ScreenInfo.PixelsPerInchX := fpgApplication.Screen_dpi_x;
  ScreenInfo.PixelsPerInchY := fpgApplication.Screen_dpi_y;
end;

{------------------------------------------------------------------------------
  Method: TFpGuiWidgetSet.AppRun
  Params:  None
  Returns: Nothing

  Enter the main message loop
 ------------------------------------------------------------------------------}
procedure TFpGuiWidgetSet.AppRun(const ALoop: TApplicationMainLoop);
var
  vMainForm: TfpgForm;
begin
  { Shows the main form }
  if Assigned(Application.MainForm) then
  begin
    vMainForm := TFPGUIPrivateWindow(Application.MainForm.Handle).Form;
    if Application.MainForm.Visible then
      vMainForm.Show;
  end;
  // GFApplication.EventFilter can maybe be used on X11 for aloop but it is X only
//  fpgApplication.Run;
  if Assigned(ALoop) then begin
    ALoop
  end else begin
    repeat
      try
        fpgApplication.ProcessMessages;
        if not fpgApplication.Terminated then Application.Idle(true);
      except
        Application.HandleException(Self);
      end;
    until fpgApplication.Terminated;
  end;

end;

{------------------------------------------------------------------------------
  Method: TFpGuiWidgetSet.AppWaitMessage
  Params:  None
  Returns: Nothing

  Wait till an OS application message is received
 ------------------------------------------------------------------------------}
procedure TFpGuiWidgetSet.AppWaitMessage;
begin
  fpgWaitWindowMessage;
end;

{------------------------------------------------------------------------------
  Method: TFpGuiWidgetSet.AppProcessMessage
  Params:  None
  Returns: Nothing

  Handle the messages in the queue
 ------------------------------------------------------------------------------}
procedure TFpGuiWidgetSet.AppProcessMessages;
begin
  fpgApplication.ProcessMessages;
end;

{------------------------------------------------------------------------------
  Method: TFpGuiWidgetSet.AppTerminate
  Params:  None
  Returns: Nothing

  Implements Application.Terminate and MainForm.Close.
 ------------------------------------------------------------------------------}
procedure TFpGuiWidgetSet.AppTerminate;
begin
  fpgApplication.Terminated := True;
end;

{------------------------------------------------------------------------------
  Method: TFpGuiWidgetSet.AppMinimize
  Params:  None
  Returns: Nothing

  Minimizes the application window.
 ------------------------------------------------------------------------------}
procedure TFpGuiWidgetSet.AppMinimize;
begin
end;

procedure TFpGuiWidgetSet.AppRestore;
begin

end;

{------------------------------------------------------------------------------
  Method: TFpGuiWidgetSet.AppBringToFront
  Params:  None
  Returns: Nothing

  Brings the application window to the front
 ------------------------------------------------------------------------------}
procedure TFpGuiWidgetSet.AppBringToFront;
begin

end;

function TFpGuiWidgetSet.LCLPlatform: TLCLPlatform;
begin
  Result:= lpfpGUI;
end;

function TFpGuiWidgetSet.DCGetPixel(CanvasHandle: HDC; X, Y: integer): TGraphicsColor;
begin
  Result:=clNone;
end;

procedure TFpGuiWidgetSet.DCSetPixel(CanvasHandle: HDC; X, Y: integer; AColor: TGraphicsColor);
var
  DC: TFPGUIDeviceContext;
  p: TPoint;
begin
  DC:=TFPGUIDeviceContext(CanvasHandle);
  p:=Point(X,Y);
  p:=DC.PreparePointOffsets(p);
  DC.FPrivateWidget.Widget.Canvas.Pixels[p.x,p.y]:=TColorToTfpgColor(AColor);
end;

procedure TFpGuiWidgetSet.DCRedraw(CanvasHandle: HDC);
begin

end;

procedure TFpGuiWidgetSet.SetDesigning(AComponent: TComponent);
begin
//  Include(AComponent.ComponentState, csDesigning);
end;

function TFpGuiWidgetSet.RawImage_CreateBitmaps(const ARawImage: TRawImage; out
  ABitmap, AMask: HBitmap; ASkipMask: Boolean): Boolean;
var
  OutBitmap: TFPGUIWinAPIBitmap;
  fpgBitmap: TfpgImage;
  ImgData: Pointer absolute ARawImage.Data;
  ImgMask: Pointer absolute ARawImage.Mask;
  ImgWidth: Cardinal absolute ARawImage.Description.Width;
  ImgHeight: Cardinal absolute ARawImage.Description.Height;
  ImgDepth: Byte absolute ARawImage.Description.Depth;
  ImgDataSize: PtrUInt absolute ARawImage.DataSize;
  function min(const a,b: SizeInt): SizeInt;
  begin
    if a>b then Result:=b else Result:=a;
  end;
begin
  ABitmap:=0;
  AMask:=0;
  Result:=false;
  OutBitmap:=TFPGUIWinAPIBitmap.Create(ARawImage.Description.BitsPerPixel,ARawImage.Description.Width,ARawImage.Description.Height);
  fpgBitmap:=OutBitmap.Image;
  ABitmap:=HBITMAP(OutBitmap);
  move(ARawImage.Data^,pbyte(fpgBitmap.ImageData)^,min(ARawImage.DataSize,fpgBitmap.ImageDataSize));
  fpgBitmap.UpdateImage;
  Result:=true;
end;

procedure FillRawImageDescriptionColors(var ADesc: TRawImageDescription);
begin
  case ADesc.BitsPerPixel of
    1,4,8:
      begin
        // palette mode, no offsets
        ADesc.Format := ricfGray;
        ADesc.RedPrec := ADesc.BitsPerPixel;
        ADesc.GreenPrec := 0;
        ADesc.BluePrec := 0;
        ADesc.RedShift := 0;
        ADesc.GreenShift := 0;
        ADesc.BlueShift := 0;
      end;
    16:
      begin
        // 5-5-5 mode
        ADesc.RedPrec := 5;
        ADesc.GreenPrec := 5;
        ADesc.BluePrec := 5;
        ADesc.RedShift := 10;
        ADesc.GreenShift := 5;
        ADesc.BlueShift := 0;
        ADesc.Depth := 15;
      end;
    24:
      begin
        // 8-8-8 mode
        ADesc.RedPrec := 8;
        ADesc.GreenPrec := 8;
        ADesc.BluePrec := 8;
        ADesc.RedShift := 16;
        ADesc.GreenShift := 8;
        ADesc.BlueShift := 0;
      end;
  else    //  32:
    // 8-8-8-8 mode, high byte can be native alpha or custom 1bit maskalpha
    ADesc.AlphaPrec := 8;
    ADesc.RedPrec := 8;
    ADesc.GreenPrec := 8;
    ADesc.BluePrec := 8;
    ADesc.AlphaShift := 24;
    ADesc.RedShift := 16;
    ADesc.GreenShift := 8;
    ADesc.BlueShift := 0;
    ADesc.Depth := 32;
  end;
end;

procedure FillRawImageDescription(const ABitmapInfo: TfpgImage; out ADesc: TRawImageDescription);
begin
  ADesc.Format := ricfRGBA;

  ADesc.Depth := 32;             // used bits per pixel
  ADesc.Width := ABitmapInfo.Width;
  ADesc.Height := ABitmapInfo.Height;
  ADesc.BitOrder := riboReversedBits;
  ADesc.ByteOrder := riboLSBFirst;
  ADesc.LineOrder := riloTopToBottom;
  ADesc.BitsPerPixel := 32;      // bits per pixel. can be greater than Depth.
  ADesc.LineEnd := rileDWordBoundary;

  if ADesc.BitsPerPixel <= 8
  then begin
    // each pixel is an index in the palette
    // TODO, ColorCount
    ADesc.PaletteColorCount := 0;
  end
  else ADesc.PaletteColorCount := 0;


  FillRawImageDescriptionColors(ADesc);

  ADesc.MaskBitsPerPixel := 8;
  ADesc.MaskShift := 0;
  ADesc.MaskLineEnd := rileWordBoundary; // CreateBitmap requires word boundary
  ADesc.MaskBitOrder := riboReversedBits;
end;

function TFpGuiWidgetSet.RawImage_DescriptionFromBitmap(ABitmap: HBITMAP; out ADesc: TRawImageDescription): Boolean;
var
  o: TFPGUIWinAPIBitmap;
begin
  o:=TFPGUIWinAPIBitmap(ABitmap);
  FillRawImageDescription(o.Image,ADesc);
  Result:=true;
end;

function TFpGuiWidgetSet.RawImage_DescriptionFromDevice(ADC: HDC; out ADesc: TRawImageDescription): Boolean;
var
  DC: TFPGUIDeviceContext;
  r: TfpgRect;
begin
  DC:=TFPGUIDeviceContext(ADC);
  ADesc.Init;
  with ADesc do begin
    Format:=      ricfRGBA;
    if Assigned(DC) and Assigned(DC.fpgCanvas) then begin
      dc.fpgCanvas.GetWinRect(r);
      Width:=     r.Width;
      Height:=    r.Height;
    end else begin
      Width:=     0;
      Height:=    0;
    end;
    Depth:=       32; // used bits per pixel
    BitOrder:=    riboBitsInOrder;
    ByteOrder:=   riboMSBFirst;
    LineOrder:=   riloTopToBottom;
    LineEnd:=     rileByteBoundary;
    BitsPerPixel:=32; // bits per pixel. can be greater than Depth.
    RedPrec:=     8;      // red or gray precision. bits for red
    RedShift:=    8;     // bitshift. Direction: from least to most significant
    GreenPrec:=   8;
    GreenShift:=  16;
    BluePrec:=    8;
    BlueShift:=   24;
    AlphaPrec:=   8;
    AlphaShift:=  0;
    // Test

    // The next values are only valid, if there is a mask (MaskBitsPerPixel > 0)
    // Masks are always separate with a depth of 1 bpp. One pixel can occupy
    // one byte at most
    // a value of 1 means that pixel is masked
    // a value of 0 means the pixel value is shown
    MaskBitsPerPixel:= 8;
    MaskShift:=        0;
    MaskLineEnd:=      rileWordBoundary;
    MaskBitOrder:=     riboReversedBits;
  end;
  Result:=true;
end;

procedure TFpGuiWidgetSet.InitializeCriticalSection(
  var CritSection: TCriticalSection);
var
  ACritSec: System.PRTLCriticalSection;
begin
  New(ACritSec);
  System.InitCriticalSection(ACritSec^);
  CritSection:=TCriticalSection(ACritSec);
end;

procedure TFpGuiWidgetSet.DeleteCriticalSection(
  var CritSection: TCriticalSection);
var
  ACritSec: System.PRTLCriticalSection;
begin
  ACritSec:=System.PRTLCriticalSection(CritSection);
  System.DoneCriticalsection(ACritSec^);
  Dispose(ACritSec);
  CritSection:=0;
end;

procedure TFpGuiWidgetSet.EnterCriticalSection(var CritSection: TCriticalSection);
var
  ACritSec: System.PRTLCriticalSection;
begin
  ACritSec:=System.PRTLCriticalSection(CritSection);
  System.EnterCriticalsection(ACritSec^);
end;

procedure TFpGuiWidgetSet.LeaveCriticalSection(var CritSection: TCriticalSection);
var
  ACritSec: System.PRTLCriticalSection;
begin
  ACritSec:=System.PRTLCriticalSection(CritSection);
  System.LeaveCriticalsection(ACritSec^);
end;

{------------------------------------------------------------------------------
  Function: TFpGuiWidgetSet.IsValidDC
  Params:   DC     -  handle to a device context (TFpGuiDeviceContext)
  Returns:  True   -  if the DC is valid
 ------------------------------------------------------------------------------}
function TFpGuiWidgetSet.IsValidDC(const DC: HDC): Boolean;
begin
  Result := (DC <> 0);
end;

{------------------------------------------------------------------------------
  Function: TFpGuiWidgetSet.IsValidGDIObject
  Params:   GDIObject  -  handle to a GDI Object (TFpGuiFont, TFpGuiBrush, etc)
  Returns:  True       -  if the DC is valid
  
  Remark: All handles for GDI objects must be pascal objects so we can
 distinguish between them
 ------------------------------------------------------------------------------}
function TFpGuiWidgetSet.IsValidGDIObject(const GDIObject: HGDIOBJ): Boolean;
var
  aObject: TObject;
begin
  Result := False;
  
  if GDIObject = 0 then Exit;
  
  aObject := TObject(GDIObject);

  try
    if aObject is TObject then
    begin
      Result:= (aObject is TFPGUIWinAPIObject);
    end;
  except
    //Eat exceptions. If Exception happends it is not a TObject after all and
    //of course it is not a fpgui GDI object.
  end;
end;

{ Unable to make it work properly
function TFpGuiWidgetSet.CreateRubberBand(const ARect: TRect;
  const ABrush: HBrush): HWND;
var
  FakeParams: TCreateParams;
  fpgForm: TfpgForm;
begin
  FillByte(FakeParams,sizeof(FakeParams),0);
  FakeParams.Style:=FakeParams.Style or WS_VISIBLE;
  FakeParams.X:=ARect.Left;
  FakeParams.Y:=ARect.Top;
  FakeParams.Width:=aRect.Width;
  FakeParams.Height:=aRect.Height;
  Result:=HWND(TFPGUIPrivateWindow.Create(nil,FakeParams));
  fpgForm:=TFPGUIPrivateWindow(Result).Form;
  TFPGUIPrivateWindow(Result).SetFormBorderStyle(TFormBorderStyle.bsNone);
  fpgForm.Show;
end;

procedure TFpGuiWidgetSet.DestroyRubberBand(ARubberBand: HWND);
begin
  TFPGUIPrivateWindow(ARubberBand).Free;
end;
}

function TFpGuiWidgetSet.RawImage_FromBitmap(out ARawImage: TRawImage; ABitmap,
  AMask: HBITMAP; ARect: PRect): Boolean;
var
  img: TFPGUIWinAPIBitmap;
begin
  ARawImage.Init;
  img:=TFPGUIWinAPIBitmap(ABitmap);
  FillRawImageDescription(img.Image, ARawImage.Description);
  ARawImage.DataSize:=ARawImage.Description.Width*ARawImage.Description.Height*4;
  ARawImage.Data:=GetMem(ARawImage.DataSize);
  move(img.Image.ImageData^,ARawImage.Data^,img.Image.ImageDataSize);
end;

//------------------------------------------------------------------------
