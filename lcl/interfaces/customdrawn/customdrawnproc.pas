unit customdrawnproc;

{$mode objfpc}{$H+}
{$include customdrawndefines.inc}

interface

uses
  // rtl+ftl
  Types, Classes, SysUtils,
  fpimage, fpcanvas, Math,
  // LazUtils
  LazFileUtils,
  {$ifndef CD_UseNativeText}
  // LazFreeType
  TTTypes, LazFreeTypeIntfDrawer, LazFreeType, EasyLazFreeType, IniFiles,
  {$endif}
  // Custom Drawn Canvas
  IntfGraphics, lazcanvas, lazregions, customdrawndrawers, customdrawncontrols,
  // LCL
  GraphType, Controls, LCLMessageGlue, WSControls, LCLType, LCLProc,
  StdCtrls, ExtCtrls, Forms, Graphics, ComCtrls,
  InterfaceBase, LCLIntf;

type
  { TCDBaseControl }

  TCDBaseControl = class
  private
    FProps: TStringList;
    function GetProps(AnIndex: String): pointer;
    procedure SetProps(AnIndex: String; AValue: pointer);
  protected
    FWinControl: TWinControl;
  public
    Children: TFPList; // of TCDWinControl;
    // For scrolling a control
    // The initial values are x=0, y=0 After scrolling downwards (by dragging upwards)
    // it will be for example x=0, y=+27
    ScrollX, ScrollY: Integer;
    LastMousePos: TPoint;
    IsScrolling: Boolean;
    // Counter to keep track of when we requested Invalidate
    // Some systems like X11 and Win32 will keep sending unnecessary paint messages
    // so for them we just throw the previously painted image
    InvalidateCount: Integer;
    // painting objects
    ControlImage: TLazIntfImage;
    ControlCanvas: TLazCanvas;
    constructor Create; virtual;
    destructor Destroy; override;
    procedure IncInvalidateCount;
    function AdjustCoordinatesForScrolling(AX, AY: Integer): TPoint;
    procedure UpdateImageAndCanvas; virtual;
    function IsControlBackgroundVisible: Boolean; virtual;
    function GetWinControl: TWinControl; virtual;
    property Props[AnIndex:String]:pointer read GetProps write SetProps;
  end;

  { TCDWinControl }

  TCDWinControl = class(TCDBaseControl)
  public
    Region: TLazRegionWithChilds;
    WinControl: TWinControl;
    CDControl: TCDControl;
    CDControlInjected: Boolean;
    procedure UpdateImageAndCanvas; override;
    function IsControlBackgroundVisible: Boolean; override;
    function GetWinControl: TWinControl; override;
  end;

  { TCDForm }

  TCDForm = class(TCDBaseControl)
  public
    LCLForm: TCustomForm;
    NativeHandle: HWND;
    //
    LastMouseDownControl: TWinControl; // Stores the control which should receive the next MouseUp
    FocusedControl: TWinControl; // The control focused in the form
    FocusedIntfControl: TWinControl; // The intf control focused in the form
    LayoutAutoAdjusted: Boolean; // Indicates if the form layout was already auto-adjusted once
    // For merging invalidate requests, currently utilized in X11
    InvalidateRequestedInAnyControl: Boolean;
    // painting objects which represent the composed form image, don't confuse with ControlImage/ControlCanvas
    Image: TLazIntfImage;
    Canvas: TLazCanvas;
    constructor Create; virtual;
    function GetFocusedControl: TWinControl;
    function GetFormVirtualHeight(AScreenHeight: Integer): Integer;
    procedure SanityCheckScrollPos();
    procedure UpdateImageAndCanvas; override;
    function IsControlBackgroundVisible: Boolean; override;
    function GetWinControl: TWinControl; override;
  end;

  TCDNonNativeForm = class(TCDForm)
  public
    Visible: Boolean;
  end;

  { TCDBitmap }

  TCDBitmap = class
  public
    Image: TLazIntfImage;
    destructor Destroy; override;
  end;

  TCDTimer = class
  public
    NativeHandle: PtrInt; // The X11 timer uses this to store the current time which is summed up to the next interval
    NativeGlobalReference: PtrInt; // Utilized in Android to store the global JNI reference
    Interval: integer;
    TimerFunc: TWSTimerProc;
  end;

// Routines for form managing (both native and non-native)

procedure AddCDWinControlToForm(const AForm: TCustomForm; ACDWinControl: TCDWinControl);
function GetCDWinControlList(const AForm: TCustomForm): TFPList;

// Routines for non-native form managing
procedure InitNonNativeForms();
function GetCurrentForm(): TCDNonNativeForm;
function GetForm(AIndex: Integer): TCDNonNativeForm;
function GetFormCount(): Integer;
function AddNewForm(AForm: TCustomForm): TCDNonNativeForm;
procedure AddFormWithCDHandle(AHandle: TCDForm);
function FindFormWithNativeHandle(AHandle: HWND): TCDForm;
procedure ShowForm(ACDForm: TCDNonNativeForm);
procedure HideForm(ACDForm: TCDNonNativeForm);
procedure BringFormToFront(ACDForm: TCDNonNativeForm);
procedure SendFormToBack(ACDForm: TCDNonNativeForm);
function FindTopMostVisibleForm: TCDNonNativeForm;

// Routines for non-native wincontrol

procedure UpdateControlLazImageAndCanvas(var AImage: TLazIntfImage;
  var ACanvas: TLazCanvas; AWidth, AHeight: Integer; AFormat: TLazCanvasImageFormat;
  AData: Pointer = nil; AForceUpdate: Boolean = False;
  AFreeImageOnUpdate: Boolean = True; ADataOwner: Boolean = True);
procedure DrawFormBackground(var AImage: TLazIntfImage; var ACanvas: TLazCanvas; AForm: TCustomForm);
procedure RenderChildWinControls(var AImage: TLazIntfImage;
  var ACanvas: TLazCanvas; ACDControlsList: TFPList; ACDForm: TCDForm);
function RenderWinControl(var AImage: TLazIntfImage;
  var ACanvas: TLazCanvas; ACDWinControl: TCDWinControl; ACDForm: TCDForm): Boolean;
procedure RenderWinControlAndChildren(var AImage: TLazIntfImage;
  var ACanvas: TLazCanvas; ACDWinControl: TCDWinControl; ACDForm: TCDForm);
procedure RenderForm(var AImage: TLazIntfImage;
  var ACanvas: TLazCanvas; AForm: TCustomForm);
function FindControlWhichReceivedEvent(AForm: TCustomForm;
  AControlsList: TFPList; AX, AY: Integer): TWinControl;
function FindControlPositionRelativeToTheForm(ALCLControl: TWinControl; AConsiderScrolling: Boolean = False): TPoint;
function FormPosToControlPos(ALCLControl: TWinControl; AX, AY: Integer): TPoint;

// Other routines

function DateTimeToMilliseconds(aDateTime: TDateTime): Int64;
function IsValidDC(ADC: HDC): Boolean;
function IsValidGDIObject(AGDIObj: HGDIOBJ): Boolean;
function IsValidBitmap(ABitmap: HBITMAP): Boolean;
function RemoveAccelChars(AStr: string): string;

// Timers list management (for platforms that need it)

procedure InitTimersList();
procedure AddTimer(ATimer: TCDTimer);
function GetTimer(AIndex: Integer): TCDTimer;
function GetTimerCount(): Integer;
function GetSmallestTimerInterval(): Integer;
procedure RemoveTimer(ATimer: TCDTimer);
function FindTimerWithNativeHandle(ANativeHandle: PtrInt): TCDTimer;

// Font choosing routines

{$ifndef CD_UseNativeText}
procedure VerifyAndCleanUpFontDirectories(AFontDirectories: TStringList);
procedure FontsScanForTTF(APath: string; var AFontTable: THashedStringList);
procedure FontsScanDir(APath: string; var AFontPaths: TStringList; var AFontList: THashedStringList);
{$endif}

implementation

uses customdrawnint;

var
  // List with the Z-order of non-native forms, index=0 is the bottom-most form
  NonNativeForms: TFPList = nil;
  lCurrentForm: TCDNonNativeForm = nil;

  // List of timers
  TimersList: TFPList = nil;

procedure AddCDWinControlToForm(const AForm: TCustomForm; ACDWinControl: TCDWinControl);
var
  lWindowInfo: TCDForm;
begin
  lWindowInfo := TCDForm(AForm.Handle);
  if lWindowInfo.Children = nil then lWindowInfo.Children := TFPList.Create;
  lWindowInfo.Children.Add(ACDWinControl);
end;

function GetCDWinControlList(const AForm: TCustomForm): TFPList;
var
  lWindowInfo: TCDForm;
begin
  lWindowInfo := TCDForm(AForm.Handle);
  if lWindowInfo.Children = nil then lWindowInfo.Children := TFPList.Create;
  Result := lWindowInfo.Children;
end;

procedure InitNonNativeForms();
begin
  if NonNativeForms <> nil then Exit;
  NonNativeForms := TFPList.Create;
end;

function GetCurrentForm(): TCDNonNativeForm;
begin
  {$IFDEF VerboseCDForms}
    DebugLn('GetCurrentForm');
  {$ENDIF}
  Result := lCurrentForm;
end;

function GetForm(AIndex: Integer): TCDNonNativeForm;
begin
  InitNonNativeForms();
  Result := TCDNonNativeForm(NonNativeForms.Items[AIndex]);
end;

function GetFormCount: Integer;
begin
  InitNonNativeForms();
  Result := NonNativeForms.Count;
end;

function AddNewForm(AForm: TCustomForm): TCDNonNativeForm;
var
  lFormInfo: TCDNonNativeForm;
begin
  {$IFDEF VerboseCDForms}
    DebugLn('AddNewForm');
  {$ENDIF}
  lFormInfo := TCDNonNativeForm.Create;
  lFormInfo.LCLForm := AForm;
  AddFormWithCDHandle(lFormInfo);
  Result := lFormInfo;
end;

procedure AddFormWithCDHandle(AHandle: TCDForm);
begin
  InitNonNativeForms();
  NonNativeForms.Insert(0, AHandle);
end;

function FindFormWithNativeHandle(AHandle: HWND): TCDForm;
var
  lCDForm: TCDForm;
  i: Integer;
begin
  Result := nil;
  InitNonNativeForms();
  for i := 0 to NonNativeForms.Count - 1 do
  begin
    lCDForm := TCDForm(NonNativeForms.Items[i]);
    if lCDForm.NativeHandle = AHandle then
    begin
      Result := lCDForm;
      Exit;
    end;
  end;
end;

procedure ShowForm(ACDForm: TCDNonNativeForm);
begin
  {$IFDEF VerboseCDForms}
    DebugLn(Format('ShowForm LCLForm=%s:%s', [ACDForm.LCLForm.Name, ACDForm.LCLForm.ClassName]));
  {$ENDIF}
  ACDForm.Visible := True;
  BringFormToFront(ACDForm);
  lCurrentForm := ACDForm;
end;

procedure HideForm(ACDForm: TCDNonNativeForm);
begin
  ACDForm.Visible := False;
  // update the Current Form if required, and invalidate too
  if lCurrentForm = ACDForm then
  begin
    lCurrentForm := FindTopMostVisibleForm();
    LCLIntf.InvalidateRect(HWND(lCurrentForm), nil, True);
  end;
  // Warn the LCL that the form was hidden
  LCLSendCloseQueryMsg(ACDForm.LCLForm);
end;

procedure BringFormToFront(ACDForm: TCDNonNativeForm);
var
  lCount, lCurIndex: Integer;
begin
  InitNonNativeForms();
  lCount := NonNativeForms.Count;
  lCurIndex := NonNativeForms.IndexOf(ACDForm);
  {$IFDEF VerboseCDForms}
    DebugLn(Format('BringFormToFront lOldIndex=%d lNewIndex=%d', [lCurIndex, lCount-1]));
  {$ENDIF}
  NonNativeForms.Move(lCurIndex, lCount-1);
end;

procedure SendFormToBack(ACDForm: TCDNonNativeForm);
var
  lCount, lCurIndex: Integer;
begin
  // Hide the form
  ACDForm.Visible := False;

  InitNonNativeForms();
  lCount := NonNativeForms.Count;
  lCurIndex := NonNativeForms.IndexOf(ACDForm);
  {$IFDEF VerboseCDForms}
    DebugLn(Format('SendFormToBack lOldIndex=%d lNewIndex=0', [lCurIndex]));
  {$ENDIF}
  NonNativeForms.Move(lCurIndex, 0);
end;

function FindTopMostVisibleForm: TCDNonNativeForm;
var
  lCount: Integer;
  lForm: TCDNonNativeForm;
  i: Integer;
begin
  Result := nil;
  InitNonNativeForms();
  // Iterate starting from Count to zero until we find a visible form
  lCount := NonNativeForms.Count;

  for i := lCount-1 downto 0 do
  begin
    lForm := TCDNonNativeForm(NonNativeForms.Items[i]);
    if lForm.Visible then
    begin
      Result := lForm;
      Break;
    end;
  end;
  {$IFDEF VerboseCDForms}
    DebugLn(Format('FindTopMostVisibleForm FoundIndex=%d FoundForm=%s:%s',
      [i, Result.LCLForm.Name, Result.LCLForm.ClassName]));
  {$ENDIF}
end;

// If AForceUpdate=True then it will update even if the width and height remain the same
procedure UpdateControlLazImageAndCanvas(var AImage: TLazIntfImage;
  var ACanvas: TLazCanvas; AWidth, AHeight: Integer; AFormat: TLazCanvasImageFormat;
  AData: Pointer = nil; AForceUpdate: Boolean = False;
  AFreeImageOnUpdate: Boolean = True; ADataOwner: Boolean = True);
var
  lRawImage: TRawImage;
  lPixelSize: Byte;
begin
  {$IFDEF VerboseCDLazCanvas}
    DebugLn(Format(':>[UpdateControlLazImageAndCanvas] Input Image: %x Canvas: %x',
      [PtrInt(AImage), PtrInt(ACanvas)]));
  {$ENDIF}
  // Check if the image needs update
  if (AImage = nil) or (AWidth <> AImage.Width) or (AHeight <> AImage.Height)
    or AForceUpdate then
  begin
    if (AImage <> nil) and AFreeImageOnUpdate then AImage.Free;
    // Free the canvas and create a new one if it is a dummy Canvas created for text metrics reading by GetDC(control)
    if (ACanvas <> nil) and ACanvas.HasNoImage then
    begin
      ACanvas.Free;
      ACanvas := nil;
    end;

    lRawImage.Init;
    case AFormat of
    clfRGB16_R5G6B5:  lRawImage.Description.Init_BPP16_R5G6B5(AWidth, AHeight);
    clfRGB24:  lRawImage.Description.Init_BPP24_R8G8B8_BIO_TTB(AWidth, AHeight);
    clfRGB24UpsideDown: lRawImage.Description.Init_BPP24_R8G8B8_BIO_TTB_UpsideDown(AWidth, AHeight);
    clfBGR24:  lRawImage.Description.Init_BPP24_B8G8R8_BIO_TTB(AWidth, AHeight);
    clfBGRA32: lRawImage.Description.Init_BPP32_B8G8R8A8_BIO_TTB(AWidth, AHeight);
    clfRGBA32: lRawImage.Description.Init_BPP32_R8G8B8A8_BIO_TTB(AWidth, AHeight);
    clfARGB32: lRawImage.Description.Init_BPP32_A8R8G8B8_BIO_TTB(AWidth, AHeight);
    end;

    // Now connect the pixel buffer or create one
    if AData = nil then lRawImage.CreateData(True)
    else
    begin
      case AFormat of
      clfRGB16_R5G6B5:
        lPixelSize := 2;
      clfRGB24, clfRGB24UpsideDown, clfBGR24:
        lPixelSize := 3;
      clfBGRA32, clfRGBA32, clfARGB32:
        lPixelSize := 4;
      end;

      lRawImage.Data := AData;
      lRawImage.DataSize := AWidth * lPixelSize * AHeight;
    end;

    AImage := TLazIntfImage.Create(AWidth, AHeight);
    AImage.SetRawImage(lRawImage, ADataOwner);

    if (ACanvas <> nil) then ACanvas.Free;
    ACanvas := TLazCanvas.Create(AImage);
    ACanvas.ImageFormat := AFormat;
  end;
  {$IFDEF VerboseCDLazCanvas}
    DebugLn(Format(':<[UpdateControlLazImageAndCanvas] Output Image: %x Canvas: %x',
      [PtrInt(AImage), PtrInt(ACanvas)]));
  {$ENDIF}
end;

procedure DrawFormBackground(var AImage: TLazIntfImage; var ACanvas: TLazCanvas; AForm: TCustomForm);
var
  lColor: TColor;
begin
  ACanvas.SaveState;
  ACanvas.ResetCanvasState;
  lColor := AForm.Color;
  if (lColor <> clForm) and (lColor <> clDefault) then
    lColor := ColorToRGB(lColor)
  else
    lColor := ColorToRGB(clForm);
  ACanvas.Brush.FPColor := TColorToFPColor(lColor);
  ACanvas.Pen.FPColor := TColorToFPColor(lColor);
  ACanvas.Rectangle(0, 0, AImage.Width, AImage.Height);
  ACanvas.RestoreState(-1);
end;

// This does not render the win control itself, only it's children
// The WinControls themselves will render child TControls not descending from TWinControl
procedure RenderChildWinControls(var AImage: TLazIntfImage;
  var ACanvas: TLazCanvas; ACDControlsList: TFPList; ACDForm: TCDForm);
var
  i, lChildrenCount: Integer;
  lCDWinControl: TCDWinControl;
begin
  lChildrenCount := ACDControlsList.Count;
  {$ifdef VerboseCDWinControl}
  DebugLn(Format('[RenderChildWinControls] ACanvas=%x ACDControlsList=%x lChildrenCount=%d',
    [PtrInt(ACanvas), PtrInt(ACDControlsList), lChildrenCount]));
  {$endif}

  for i := 0 to lChildrenCount-1 do
  begin
    {$ifdef VerboseCDWinControl}
    DebugLn(Format('[RenderChildWinControls] i=%d', [i]));
    {$endif}

    lCDWinControl := TCDWinControl(ACDControlsList.Items[i]);

    RenderWinControlAndChildren(AImage, ACanvas, lCDWinControl, ACDForm);
  end;
end;

// Renders a WinControl, but not it's children
// Returns if the control is visible and therefore if its children should be rendered
function RenderWinControl(var AImage: TLazIntfImage; var ACanvas: TLazCanvas;
  ACDWinControl: TCDWinControl; ACDForm: TCDForm): Boolean;
var
  lWinControl, lParentControl: TWinControl;
  struct : TPaintStruct;
  lCanvas: TCanvas;
  lControlCanvas: TLazCanvas;
  lBaseWindowOrg: TPoint;
  lControlStateEx: TCDControlStateEx;
  lDrawControl: Boolean;
  lRegion:TLazRegionWithChilds;
begin
  Result := False;

  lWinControl := ACDWinControl.WinControl;

  {$ifdef VerboseCDWinControl}
  DebugLn(Format('[RenderWinControl] lWinControl=%x Name=%s:%s Left=%d'
    + ' Top=%d Width=%d Height=%d', [PtrInt(lWinControl), lWinControl.Name, lWinControl.ClassName,
    lWinControl.Left, lWinControl.Top, lWinControl.Width, lWinControl.Height]));
  {$endif}

  if lWinControl.Visible = False then Exit;

  // Disable the drawing itself, but keep the window org and region operations
  // or else clicking and other things are broken
  lDrawControl := ACDWinControl.IsControlBackgroundVisible();

  // Save the Canvas state
  ACanvas.SaveState;
  ACanvas.ResetCanvasState;

  // lBaseWindowOrg makes debugging easier
  // Iterate to find the appropriate BaseWindowOrg relative to the parent control
  lBaseWindowOrg := FindControlPositionRelativeToTheForm(lWinControl);
  ACanvas.BaseWindowOrg := Point(lBaseWindowOrg.X, lBaseWindowOrg.Y - ACDForm.ScrollY);
  ACanvas.WindowOrg := Point(0, 0);

  // Prepare the clippping relative to the form
  ACanvas.Clipping := True;
  ACDWinControl.Region.Rect := Bounds(lBaseWindowOrg.X, lBaseWindowOrg.Y - ACDForm.ScrollY,
    lWinControl.Width, lWinControl.Height);
  lRegion := TLazRegionWithChilds.Create;
  lRegion.Assign(ACDWinControl.Region);
  ACanvas.ClipRegion := lRegion;

  lControlCanvas := ACanvas;

  if (ACDWinControl.InvalidateCount > 0) and lDrawControl then
  begin
    ACDWinControl.UpdateImageAndCanvas();
    lControlCanvas := ACDWinControl.ControlCanvas;
    ACDWinControl.InvalidateCount := 0;

    // Special drawing for some native controls
    if (lWinControl is TCustomPanel) or (lWinControl is TTabSheet)
     or (lWinControl is TCustomPage) or (lWinControl is TNotebook)  then
    begin
      // Erase the background of TPanel controls, since it can draw it's own border, but fails to draw it's own background
      // and also erase the background for TTabSheet (children of TPageControl) and TCustomPage (children of TNotebook)
      lControlCanvas.SaveState;
      lControlCanvas.Brush.FPColor := TColorToFPColor(lWinControl.GetRGBColorResolvingParent());
      lControlCanvas.Pen.FPColor := lControlCanvas.Brush.FPColor;
      lControlCanvas.Rectangle(Bounds(0, 0, lWinControl.Width, lWinControl.Height));
      lControlCanvas.RestoreState(-1);
    end
    else if lWinControl is TCustomGroupBox then
    begin
      lControlCanvas.SaveState;
      lControlStateEx := TCDControlStateEx.Create;
      try
        lControlStateEx.Font := lWinControl.Font;
        lControlStateEx.Caption := lWinControl.Caption;
        lControlStateEx.ParentRGBColor := lWinControl.GetRGBColorResolvingParent();
        GetDefaultDrawer().DrawGroupBox(lControlCanvas, Point(0,0),
          Size(lWinControl.Width, lWinControl.Height), [], lControlStateEx);
      finally
        lControlStateEx.Free;
        lControlCanvas.RestoreState(-1);
      end;
    end;

    // Send the drawing message
    {$ifdef VerboseCDWinControl}
    DebugLn('[RenderWinControl] before LCLSendPaintMsg');
    {$endif}
    FillChar(struct, SizeOf(TPaintStruct), 0);
    struct.hdc := HDC(lControlCanvas);
    LCLSendEraseBackgroundMsg(lWinControl, struct.hdc);
    LCLSendPaintMsg(lWinControl, struct.hdc, @struct);
    {$ifdef VerboseCDWinControl}
    DebugLn('[RenderWinControl] after LCLSendPaintMsg');
    {$endif}
  end;

  // Here we actually blit the control to the form canvas
  if lDrawControl then
  ACanvas.CanvasCopyRect(ACDWinControl.ControlCanvas, 0, 0, 0, 0,
    lWinControl.Width, lWinControl.Height);

  // Now restore it
  ACanvas.RestoreState(-1);

  Result := True;
end;

// Render a WinControl and all it's children
procedure RenderWinControlAndChildren(var AImage: TLazIntfImage;
  var ACanvas: TLazCanvas; ACDWinControl: TCDWinControl; ACDForm: TCDForm);
begin
  if not RenderWinControl(AImage, ACanvas, ACDWinControl, ACDForm) then Exit;

  // Now Draw all sub-controls
  if ACDWinControl.Children <> nil then
    RenderChildWinControls(AImage, ACanvas, ACDWinControl.Children, ACDForm);
end;

// Draws a form and all of its child controls
procedure RenderForm(var AImage: TLazIntfImage; var ACanvas: TLazCanvas;
  AForm: TCustomForm);
var
  struct : TPaintStruct;
  lWindowHandle: TCDForm;
  lFormCanvas: TLazCanvas;
  lDrawControl: Boolean;
begin
  lWindowHandle := TCDForm(AForm.Handle);

  // Disable the drawing itself, but keep the window org and region operations
  // or else clicking and other things are broken, specially in Android
  //
  // If the form is smaller then the buffer (this might happen in Android)
  // then we need to force drawing the background to erase old contents of the buffer
  //
  // Consider also if the user wants to manually disable the background drawing
  lDrawControl := lWindowHandle.IsControlBackgroundVisible() or (AForm.Height < AImage.Height);
  if Assigned(CDWidgetset.DisableFormBackgroundDrawingProc) then
  begin
    if not CDWidgetset.DisableFormBackgroundDrawingProc(AForm) then
      if lDrawControl then
        DrawFormBackground(AImage, ACanvas, AForm);
  end
  else if lDrawControl then
    DrawFormBackground(AImage, ACanvas, AForm);

  // Consider the form scrolling
  // ToDo: Figure out why this "div 2" factor is necessary for drawing non-windows controls and remove this factor
  // If you remove this factor then the wincontrols are fine,
  // but graphiccontrols scroll with a different speed which is a huge problem
  ACanvas.BaseWindowOrg := Point(0, - lWindowHandle.ScrollY div 2);
  ACanvas.WindowOrg := Point(0, 0);

  lFormCanvas := ACanvas;

  if lDrawControl then
  begin
    // Send the paint message to the LCL
    {$IFDEF VerboseCDForms}
      DebugLn(Format('[RenderForm] OnPaint event started context: %x', [struct.hdc]));
    {$ENDIF}
    FillChar(struct, SizeOf(TPaintStruct), 0);
    struct.hdc := HDC(lFormCanvas);
    LCLSendPaintMsg(AForm, struct.hdc, @struct);
    {$IFDEF VerboseCDForms}
      DebugLn('[RenderForm] OnPaint event ended');
    {$ENDIF}
  end;

  // Now paint all child win controls
  RenderChildWinControls(AImage, ACanvas, GetCDWinControlList(AForm), lWindowHandle);
end;

function FindControlWhichReceivedEvent(AForm: TCustomForm;
  AControlsList: TFPList; AX, AY: Integer): TWinControl;
var
  i: Integer;
  lRegionOfEvent: TLazRegionWithChilds;
  lCurCDControl: TCDWinControl;
  lEventPos: TPoint; // local, already adjusted for the scrolling
begin
  Result := AForm;
  lEventPos := Point(AX, AY); // Don't adjust for the scrolling because the regions are scrolled too

  // The order of this loop is important to respect the Z-order of controls
  for i := AControlsList.Count-1 downto 0 do
  begin
    lCurCDControl := TCDWinControl(AControlsList.Items[i]);
    if lCurCDControl.Region = nil then Continue;
    if not lCurCDControl.WinControl.HandleObjectShouldBeVisible then Continue;
    lRegionOfEvent := lCurCDControl.Region.IsPointInRegion(lEventPos.X, lEventPos.Y);
    if lRegionOfEvent <> nil then
    begin
      if lRegionOfEvent.UserData = nil then
        raise Exception.Create('[FindControlWhichReceivedEvent] Malformed tree of regions');
      Result := TWinControl(lRegionOfEvent.UserData);

      // If it is a native LCL control, redirect to the CDControl
      if lCurCDControl.CDControl <> nil then
        Result := lCurCDControl.CDControl;

      Exit;
    end;
  end;
end;

function FindControlPositionRelativeToTheForm(ALCLControl: TWinControl; AConsiderScrolling: Boolean = False): TPoint;
var
  lParentControl: TWinControl;
  lParentHandle: TCDBaseControl;
  lScroll, lParentPos: TPoint;
begin
  // Iterate to find the appropriate BaseWindowOrg relative to the parent control
  Result := Point(ALCLControl.Left, ALCLControl.Top);
  lParentControl := ALCLControl.Parent;
  while (lParentControl <> nil) do
  begin
    if AConsiderScrolling and lParentControl.HandleAllocated then
    begin
      lParentHandle := TCDBaseControl(lParentControl.Handle);
      lScroll := Point(lParentHandle.ScrollX, lParentHandle.ScrollY);
    end
    else lScroll := Point(0, 0);

    if (lParentControl is TCustomForm) then lParentPos := Point(0, 0)
    else lParentPos := Point(lParentControl.Left, lParentControl.Top);

    Result.X := Result.X + lParentPos.X - lScroll.X;
    Result.Y := Result.Y + lParentPos.Y - lScroll.Y;
    lParentControl := lParentControl.Parent;
  end;
end;

function FormPosToControlPos(ALCLControl: TWinControl; AX, AY: Integer): TPoint;
var
  lControlPos: TPoint;
begin
  lControlPos := FindControlPositionRelativeToTheForm(ALCLControl, True);
  Result.X := AX - lControlPos.X;
  Result.Y := AY - lControlPos.Y;
end;

function DateTimeToMilliseconds(aDateTime: TDateTime): Int64;
var
  TimeStamp: TTimeStamp;
begin
  {Call DateTimeToTimeStamp to convert DateTime to TimeStamp:}
  TimeStamp:= DateTimeToTimeStamp (aDateTime);
  {Multiply and add to complete the conversion:}
  Result:= TimeStamp.Time;
end;

function IsValidDC(ADC: HDC): Boolean;
begin
  Result := ADC <> 0;
end;

function IsValidGDIObject(AGDIObj: HGDIOBJ): Boolean;
begin
  Result := AGDIObj <> 0;
end;

function IsValidBitmap(ABitmap: HBITMAP): Boolean;
begin
  Result := ABitmap <> 0;
end;

function RemoveAccelChars(AStr: string): string;
begin
  // ToDo convert && to & and keep it
  Result := StringReplace(AStr, '&', '', [rfReplaceAll]);
end;

procedure InitTimersList;
begin
  if TimersList = nil then TimersList := TFPList.Create;
end;

procedure AddTimer(ATimer: TCDTimer);
begin
  InitTimersList();
  TimersList.Add(ATimer);
end;

function GetTimer(AIndex: Integer): TCDTimer;
begin
  InitTimersList();
  Result := TCDTimer(TimersList.Items[AIndex]);
end;

function GetTimerCount: Integer;
begin
  InitTimersList();
  Result := TimersList.Count;
end;

function GetSmallestTimerInterval: Integer;
var
  i: Integer;
  lTimer: TCDTimer;
begin
  Result := High(Integer);
  for i := 0 to GetTimerCount()-1 do
  begin
    lTimer := GetTimer(i);
    Result := Min(Result, lTimer.Interval);
  end;
  if Result = High(Integer) then Result := -1;
end;

procedure RemoveTimer(ATimer: TCDTimer);
begin
  InitTimersList();
  TimersList.Remove(ATimer);
end;

function FindTimerWithNativeHandle(ANativeHandle: PtrInt): TCDTimer;
var
  lTimer: TCDTimer;
  i: Integer;
begin
  Result := nil;
  InitTimersList();
  for i := 0 to TimersList.Count - 1 do
  begin
    lTimer := TCDTimer(TimersList.Items[i]);
    if lTimer.NativeHandle = ANativeHandle then
      Exit(lTimer);
  end;
end;

{$ifndef CD_UseNativeText}
procedure VerifyAndCleanUpFontDirectories(AFontDirectories: TStringList);
var
  i, j: Integer;
begin
  // Add path delimitiers to the end of all paths
  for i := 0 to AFontDirectories.Count -1 do
  begin
    AFontDirectories.Strings[i] := IncludeTrailingPathDelimiter(AFontDirectories.Strings[i]);
  end;

  // remove all duplicates
  i := 0;
  while i < AFontDirectories.Count do
  begin
    j := i+1;
    while j < AFontDirectories.Count do
    begin
      if AFontDirectories.Strings[i] = AFontDirectories.Strings[j] then
        AFontDirectories.Delete(j);
      Inc(j);
    end;
    Inc(i);
  end;

  // Now remove all directories which don't exist
  i := 0;
  while i < AFontDirectories.Count do
  begin
    if not DirectoryExistsUTF8(AFontDirectories.Strings[i]) then
      AFontDirectories.Delete(i);
    Inc(i);
  end;

  // Raise an exception if there are no font directories
  if AFontDirectories.Count = 0 then
    raise Exception.Create('[VerifyAndCleanUpFontDirectories] After cleaning up no font directories were found.');
end;

{------------------------------------------------------------------------------
 Procedure: BackendScanForTTF - Scope=local
 Params: APath - path for a font directory
         AFontTable - Font name to Font path Hashed List

 Scan a directory for ttf fonts and updates the FontTable
------------------------------------------------------------------------------}
procedure FontsScanForTTF(APath: string; var AFontTable: THashedStringList);
var
  Rslt: TSearchRec;
  AFace: TT_Face;
  ErrNum: TT_Error;
  SearchResult, J: Integer;
  FontPath: String;
  NameCount: Integer;
  NameString: Pchar;
  NameLen: Integer;
  Platform,Encoding,Language: Integer;
  NameID: Integer;
  AName: String;
{$ifdef CD_Debug_TTF}
  DebugList: TstringList;
{$endif}
begin
  SearchResult := FindFirstUTF8(APath+'*.ttf', faAnyFile, Rslt);
{$ifdef CD_Debug_TTF}
  DebugList:= TStringList.Create;
{$endif}
  while SearchResult = 0 do
  begin
    FontPath:= APath+Rslt.Name;

    {$ifdef CD_Debug_TTF}
      DebugLn(Format('[FontsScanForTTF] font=%s', [FontPath]));
    {$endif}

    // Work around for fonts which cause errors. See bug 21456
    if Rslt.Name = 'tunga.ttf' then
    begin
      SearchResult := FindNextUTF8(Rslt);
      Continue;
    end;

    ErrNum:= TT_Open_Face(FontPath, AFace);
    if ErrNum = TT_Err_Ok then
    begin
      NameCount:= TT_Get_Name_Count(AFace);
      for J:= 0 to NameCount-1 do
      begin
        ErrNum:= TT_Get_Name_ID(AFace, J, Platform, Encoding, Language, NameID);
        { -------------------------------------------------------------------
            NameID: 0= Copyright
                    1= Font Family (e.g. Arial, Times, Liberation )
                    2= Font Subfamily (e.g. Bold, Italic, Condensed)
                    3= Unique Font Identifier
                    4= Full Name - Human readable - the one used by the IDE
        -----------------------------------------------------------------------}
        {$ifdef CD_Debug_TTF}
        if ErrNum = TT_Err_Ok then
        begin
          ErrNum:= TT_Get_Name_String(AFace,J,NameString,NameLen);
          AName:= NameString;
          if NameString <> '' then //DBG
          begin
            SetLength(AName,NameLen);
            DebugList.Add('ID='+IntToStr(NameID)+' Path='+FontPath+' Name='+AName);
          end
          else DebugList.Add('ID='+IntToStr(NameID)+' Path='+FontPath+' Name=<Empty String>');
        end;
        {$endif}
        if (ErrNum = TT_Err_Ok) and (NameID = 4) then begin
          ErrNum:= TT_Get_Name_String(AFace,J,NameString,NameLen);
          AName:= NameString;
          // Skip empty entries
          if NameString <> '' then begin
            SetLength(AName,NameLen);
            AFontTable.Add(AName+'='+FontPath);
          end;
        end;
      end;
    end;
    {$ifdef CD_Debug_TTF}
    DebugList.Add('------');
    {$endif}
    ErrNum:= TT_Close_Face(AFace);
    SearchResult := FindNextUTF8(Rslt);
  end;
  FindCloseUTF8(Rslt);
{$ifdef CD_Debug_TTF}
  AName:= ExtractFileDir(Apath);
  AName:= ExtractFileName(AName) + '.txt';
  DebugList.SaveToFile({$ifdef UNIX}'/tmp/'+{$endif}{$ifdef Windows}'C:\'+{$endif}AName);
  DebugList.Free;
{$endif}
end;

{------------------------------------------------------------------------------
 Procedure: BackendScanDir - Scope=Local
 Params: APath - path for a font directory
         AFontPaths - Font path List

 Recursively scans font directories to find the ones populated only
by fonts
------------------------------------------------------------------------------}
procedure FontsScanDir(APath: string; var AFontPaths: TStringList; var AFontList: THashedStringList);
var
  NextPath: string;
  Rslt: TSearchRec;
  I: Integer;
  DirFound,DirEmpty: Boolean;
  TmpList: THashedStringList;
begin
  DirFound:= False;
  DirEmpty:= True;
  I:= FindFirstUTF8(APath+'*',faAnyFile,Rslt);
  while I >= 0 do begin
    if (Rslt.Name <> '.') and (Rslt.Name <> '..') then
    begin
      DirEmpty:= False;
      if (Rslt.Attr and faDirectory) <> 0 then
      begin
        NextPath:= APath + Rslt.Name + PathDelim;
        DirFound:= true;
        FontsScanDir(NextPath,AFontPaths,AFontList);
      end;
    end;
    I:= FindNextUTF8(Rslt);
  end;
  FindCloseUTF8(Rslt);
  if (not DirFound) and (not DirEmpty) then
    AFontPaths.Add(APath);
end;

{$endif}

{ TCDWinControl }

procedure TCDWinControl.UpdateImageAndCanvas;
begin
  UpdateControlLazImageAndCanvas(ControlImage, ControlCanvas,
    WinControl.Width, WinControl.Height, {$ifdef CD_Support_Alpha_Controls}clfARGB32{$else}CDWidgetset.ScreenFormat{$endif});
end;

function TCDWinControl.IsControlBackgroundVisible: Boolean;
begin
  FWinControl := WinControl;
  Result:=inherited IsControlBackgroundVisible;
end;

function TCDWinControl.GetWinControl: TWinControl;
begin
  Result := WinControl;
end;

{ TCDBitmap }

destructor TCDBitmap.Destroy;
begin
  if Image <> nil then Image.Free;
  inherited Destroy;
end;

{ TCDBaseControl }

function TCDBaseControl.GetProps(AnIndex: String): pointer;
var
  i: Integer;
begin
  i:=Fprops.IndexOf(AnIndex);
  if i>=0 then
  begin
    result:=Fprops.Objects[i];
    exit;
  end;
  result := nil;
end;

function TCDBaseControl.GetWinControl: TWinControl;
begin
  Result := FWinControl;
end;

procedure TCDBaseControl.SetProps(AnIndex: String; AValue: pointer);
var
  i: Integer;
begin
  i := Fprops.IndexOf(AnIndex);
  if i < 0 then
    i := FProps.Add(AnIndex);
  Fprops.Objects[i] := TObject(AValue);
end;

constructor TCDBaseControl.Create;
begin
  inherited Create;
  FProps := TStringList.Create;
  //FProps.CaseSensitive:=false; commented as in the qt widgetset
  FProps.Sorted:=true;
  IncInvalidateCount(); // Always starts needing an invalidate

  Children := TFPList.Create;
end;

destructor TCDBaseControl.Destroy;
begin
  FProps.Free;
  Children.Free;

  // Free the Canvas and Image if required
  // Dont free for the Form because elsewhere this is taken care of
  if ControlCanvas <> nil then ControlCanvas.Free;
  if ControlImage <> nil then ControlImage.Free;

  inherited Destroy;
end;

procedure TCDBaseControl.IncInvalidateCount;
begin
  Inc(InvalidateCount);
end;

function TCDBaseControl.AdjustCoordinatesForScrolling(AX, AY: Integer): TPoint;
begin
  DebugLn(Format('AX=%d AY=%d ScrollX=%d ScrollY=%d', [AX, AY, ScrollX, ScrollY]));
  Result := Point(AX + ScrollX, AY + ScrollY);
end;

procedure TCDBaseControl.UpdateImageAndCanvas;
begin

end;

// This is utilized for optimizing the painting. If we figure out that there is
// nothing visible from a control, just give up drawing it completely
//
// What usually happens is that child controls might completely cover their
// parent controls
//
// We should watch out for alpha-blending, however
function TCDBaseControl.IsControlBackgroundVisible: Boolean;
var
  i: Integer;
  lChild: TControl;
  lWinChild: TWinControl;
begin
  Result := True;
  if FWinControl = nil then Exit;
  for i := 0 to FWinControl.ControlCount-1 do
  begin
    lChild := FWinControl.Controls[i];
    if not (lChild is TWinControl) then Continue;
    lWinChild := TWinControl(lChild);

    // Ignore invisible controls
    if not lWinChild.Visible then Continue;

    // ToDo: Ignore alpha blended controls

    // Basic case: alClient, but watch out for borders!!!
    if (lWinChild.Align = alClient) and
      (lWinChild.BorderSpacing.Around = 0) and
      (lWinChild.BorderSpacing.Bottom = 0) and
      (lWinChild.BorderSpacing.Left = 0) and
      (lWinChild.BorderSpacing.Right = 0) and
      (lWinChild.BorderSpacing.Top = 0) then Exit(False);

    // Another case: coordinates match
    if (lWinChild.Left = 0) and (lWinChild.Top = 0) and
       (lWinChild.Width = FWinControl.Width) and (lWinChild.Height = FWinControl.Height) then
       Exit(False);
  end;
end;

{ TCDForm }

constructor TCDForm.Create;
begin
  inherited Create;
  InvalidateCount := 1;
end;

function TCDForm.GetFocusedControl: TWinControl;
begin
  if FocusedIntfControl <> nil then Result := FocusedIntfControl
  else if FocusedControl <> nil then Result := FocusedControl
  else Result := LCLForm;
end;

function TCDForm.GetFormVirtualHeight(AScreenHeight: Integer): Integer;
var
  i, lControlRequiredHeight: Integer;
  lControl: TControl;
begin
  Result := AScreenHeight;
  for i := 0 to LCLForm.ControlCount-1 do
  begin
    lControl := LCLForm.Controls[i];
    lControlRequiredHeight := lControl.Top + lControl.Height;
    Result := Max(lControlRequiredHeight, Result);
  end;
end;

procedure TCDForm.SanityCheckScrollPos;
begin
  ScrollY := Max(ScrollY, 0);
  ScrollY := Min(ScrollY, GetFormVirtualHeight(Image.Height) - Image.Height);
end;

procedure TCDForm.UpdateImageAndCanvas;
begin
  UpdateControlLazImageAndCanvas(ControlImage, ControlCanvas,
    LCLForm.ClientWIdth, LCLForm.ClientHeight, clfARGB32);
end;

function TCDForm.IsControlBackgroundVisible: Boolean;
begin
  FWinControl := LCLForm;
  Result:=inherited IsControlBackgroundVisible;
end;

function TCDForm.GetWinControl: TWinControl;
begin
  Result := LCLForm;
end;

end.

