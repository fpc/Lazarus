{%MainUnit fpguiint.pp}
{
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
  ADC: TFPGUIDeviceContext absolute DC;
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
  if Assigned(ADC.fpgCanvas) then begin
    ControlType:=uType;
    ControlStyle:=uState and DFCS_ALLSTATES;
    TRectTofpgRect(Rect,fpgRect);
    AdjustRectToOrg(fpgRect,ADC.FOrg);
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

{------------------------------------------------------------------------------
  Method: TFpGuiWidgetSet.AppInit
  Params:  None
  Returns: Nothing

  Initializes the application
 ------------------------------------------------------------------------------}
procedure TFpGuiWidgetSet.AppInit(var ScreenInfo: TScreenInfo);
//var
//  Display: String;
begin
  // This doesn't hurt. on other playforms than X it just will do nothing
//  Display := GetEnvironmentVariableUTF8('DISPLAY');
  fpgApplication.Initialize;
  //GFApplication.QuitWhenLastWindowCloses := False;
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
  fpgApplication.Run;
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
begin

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

function TFpGuiWidgetSet.RawImage_DescriptionFromDevice(ADC: HDC; out
  ADesc: TRawImageDescription): Boolean;
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
    Depth:=       24; // used bits per pixel
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
    AlphaPrec:=   0;
    AlphaShift:=  0;
  end;
  Result:=true;
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

//------------------------------------------------------------------------
