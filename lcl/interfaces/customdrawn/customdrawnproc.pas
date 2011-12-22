unit customdrawnproc;

{$mode objfpc}{$H+}

interface

uses
  // rtl+ftl
  Types, Classes, SysUtils,
  fpimage, fpcanvas, Math,
  // Custom Drawn Canvas
  IntfGraphics, lazcanvas, lazregions,
  // LCL
  GraphType, Controls, LCLMessageGlue, WSControls, LCLType, LCLProc,
  StdCtrls, ExtCtrls, Forms, Graphics, customdrawncontrols,
  InterfaceBase, LCLIntf;

type
  TUpdateLazImageFormat = (
    clfRGB16_R5G6B5,
    clfRGB24, clfRGB24UpsideDown, clfBGR24,
    clfBGRA32, clfRGBA32, clfARGB32);

  TCDBaseControl = class
  private
    FProps: TStringList;
    function GetProps(AnIndex: String): pointer;
    procedure SetProps(AnIndex: String; AValue: pointer);
  public
    Children: TFPList; // of TCDWinControl;
    constructor Create; virtual;
    destructor Destroy; override;
    property Props[AnIndex:String]:pointer read GetProps write SetProps;
  end;

  { TCDWinControl }

  TCDWinControl = class(TCDBaseControl)
  public
    Region: TLazRegionWithChilds;
    WinControl: TWinControl;
    CDControl: TCDControl;
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
    // Counter to keep track of when we requested Invalidate
    // Some systems like X11 and Win32 will keep sending unnecessary paint messages
    // so for them we just throw the previously painted image
    InvalidateCount: Integer;
    // painting objects
    Image: TLazIntfImage;
    Canvas: TLazCanvas;
    constructor Create; virtual;
    procedure IncInvalidateCount;
    function GetFocusedControl: TWinControl;
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
  var ACanvas: TLazCanvas; AWidth, AHeight: Integer; AFormat: TUpdateLazImageFormat;
  AData: Pointer = nil; AForceUpdate: Boolean = False;
  AFreeImageOnUpdate: Boolean = True; ADataOwner: Boolean = True);
procedure DrawFormBackground(var AImage: TLazIntfImage; var ACanvas: TLazCanvas);
procedure RenderChildWinControls(var AImage: TLazIntfImage;
  var ACanvas: TLazCanvas; ACDControlsList: TFPList);
function RenderWinControl(var AImage: TLazIntfImage;
  var ACanvas: TLazCanvas; ACDWinControl: TCDWinControl): Boolean;
procedure RenderWinControlAndChildren(var AImage: TLazIntfImage;
  var ACanvas: TLazCanvas; ACDWinControl: TCDWinControl);
procedure RenderForm(var AImage: TLazIntfImage;
  var ACanvas: TLazCanvas; AForm: TCustomForm);
function FindControlWhichReceivedEvent(AForm: TCustomForm;
  AControlsList: TFPList; AX, AY: Integer): TWinControl;
function FindControlPositionRelativeToTheForm(ALCLControl: TWinControl): TPoint;
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

implementation

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
  lFormInfo.Children := TFPList.Create;
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
  var ACanvas: TLazCanvas; AWidth, AHeight: Integer; AFormat: TUpdateLazImageFormat;
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
  end;
  {$IFDEF VerboseCDLazCanvas}
    DebugLn(Format(':<[UpdateControlLazImageAndCanvas] Output Image: %x Canvas: %x',
      [PtrInt(AImage), PtrInt(ACanvas)]));
  {$ENDIF}
end;

procedure DrawFormBackground(var AImage: TLazIntfImage; var ACanvas: TLazCanvas);
begin
  ACanvas.SaveState;
  ACanvas.ResetCanvasState;
  ACanvas.Brush.FPColor := TColorToFPColor(ColorToRGB(clForm));
  ACanvas.Pen.FPColor := TColorToFPColor(ColorToRGB(clForm));
  ACanvas.Rectangle(0, 0, AImage.Width, AImage.Height);
  ACanvas.RestoreState;
end;

// This does not render the win control itself, only it's children
// The WinControls themselves will render child TControls not descending from TWinControl
procedure RenderChildWinControls(var AImage: TLazIntfImage;
  var ACanvas: TLazCanvas; ACDControlsList: TFPList);
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

    RenderWinControlAndChildren(AImage, ACanvas, lCDWinControl);
  end;
end;

// Renders a WinControl, but not it's children
// Returns if the control is visible and therefore if its children should be rendered
function RenderWinControl(var AImage: TLazIntfImage; var ACanvas: TLazCanvas;
  ACDWinControl: TCDWinControl): Boolean;
var
  lWinControl, lParentControl: TWinControl;
  struct : TPaintStruct;
  lCanvas: TCanvas;
  lBaseWindowOrg: TPoint;
begin
  Result := False;

  FillChar(struct, SizeOf(TPaintStruct), 0);
  struct.hdc := HDC(ACanvas);

  lWinControl := ACDWinControl.WinControl;

  {$ifdef VerboseCDWinControl}
  DebugLn(Format('[RenderWinControl] lWinControl=%x Name=%s:%s Left=%d'
    + ' Top=%d Width=%d Height=%d', [PtrInt(lWinControl), lWinControl.Name, lWinControl.ClassName,
    lWinControl.Left, lWinControl.Top, lWinControl.Width, lWinControl.Height]));
  {$endif}

  if lWinControl.Visible = False then Exit;

  // Save the Canvas state
  ACanvas.SaveState;
  ACanvas.ResetCanvasState;

  // lBaseWindowOrg makes debugging easier
  // Iterate to find the appropriate BaseWindowOrg relative to the parent control
  lBaseWindowOrg := FindControlPositionRelativeToTheForm(lWinControl);
  ACanvas.BaseWindowOrg := lBaseWindowOrg;
  ACanvas.WindowOrg := Point(0, 0);

  // Prepare the clippping relative to the form
  ACanvas.Clipping := True;
  ACDWinControl.Region.Rect := Bounds(lBaseWindowOrg.X, lBaseWindowOrg.Y, lWinControl.Width, lWinControl.Height);
  ACanvas.ClipRegion := ACDWinControl.Region;

  // Special drawing for some native controls
  if lWinControl is TCustomPanel then
  begin
    // Erase the background of TPanel controls, since it can draw it's own border, but fails to draw it's own background
    ACanvas.SaveState;
    ACanvas.Brush.FPColor := TColorToFPColor((lWinControl as TCustomPanel).GetRGBColorResolvingParent());
    ACanvas.Pen.FPColor := ACanvas.Brush.FPColor;
    ACanvas.Rectangle(Bounds(0, 0, lWinControl.Width, lWinControl.Height));
    ACanvas.RestoreState;
  end;

  // Send the drawing message
  {$ifdef VerboseCDWinControl}
  DebugLn('[RenderWinControl] before LCLSendPaintMsg');
  {$endif}
  LCLSendPaintMsg(lWinControl, struct.hdc, @struct);
  {$ifdef VerboseCDWinControl}
  DebugLn('[RenderWinControl] after LCLSendPaintMsg');
  {$endif}

  // Now restore it
  ACanvas.RestoreState;

  Result := True;
end;

// Render a WinControl and all it's children
procedure RenderWinControlAndChildren(var AImage: TLazIntfImage;
  var ACanvas: TLazCanvas; ACDWinControl: TCDWinControl);
begin
  // Draw the control
  if not RenderWinControl(AImage, ACanvas, ACDWinControl) then Exit;

  // Now Draw all sub-controls
  if ACDWinControl.Children <> nil then
    RenderChildWinControls(AImage, ACanvas, ACDWinControl.Children);
end;

// Draws a form and all of its child controls
procedure RenderForm(var AImage: TLazIntfImage; var ACanvas: TLazCanvas;
  AForm: TCustomForm);
var
  struct : TPaintStruct;
begin
  DrawFormBackground(AImage, ACanvas);

  FillChar(struct, SizeOf(TPaintStruct), 0);
  struct.hdc := HDC(ACanvas);

  // Send the paint message to the LCL
  {$IFDEF VerboseCDForms}
    DebugLn(Format('[RenderForm] OnPaint event started context: %x', [struct.hdc]));
  {$ENDIF}
  LCLSendPaintMsg(AForm, struct.hdc, @struct);
  {$IFDEF VerboseCDForms}
    DebugLn('[RenderForm] OnPaint event ended');
  {$ENDIF}

  // Now paint all child win controls
  RenderChildWinControls(AImage, ACanvas, GetCDWinControlList(AForm));
end;

function FindControlWhichReceivedEvent(AForm: TCustomForm;
  AControlsList: TFPList; AX, AY: Integer): TWinControl;
var
  i: Integer;
  lRegionOfEvent: TLazRegionWithChilds;
  lCurCDControl: TCDWinControl;
begin
  Result := AForm;
  for i := 0 to AControlsList.Count-1 do
  begin
    lCurCDControl := TCDWinControl(AControlsList.Items[i]);
    if lCurCDControl.Region = nil then Continue;
    lRegionOfEvent := lCurCDControl.Region.IsPointInRegion(AX, AY);
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

function FindControlPositionRelativeToTheForm(ALCLControl: TWinControl): TPoint;
var
  lParentControl: TWinControl;
begin
  // Iterate to find the appropriate BaseWindowOrg relative to the parent control
  Result := Point(ALCLControl.Left, ALCLControl.Top);
  lParentControl := ALCLControl.Parent;
  while (lParentControl <> nil) and not (lParentControl is TCustomForm) do
  begin
    Result.X := Result.X + lParentControl.Left;
    Result.Y := Result.Y + lParentControl.Top;
    lParentControl := lParentControl.Parent;
  end;
end;

function FormPosToControlPos(ALCLControl: TWinControl; AX, AY: Integer): TPoint;
var
  lControlPos: TPoint;
begin
  lControlPos := FindControlPositionRelativeToTheForm(ALCLControl);
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
end;

destructor TCDBaseControl.Destroy;
begin
  FProps.Free;
  inherited Destroy;
end;

{ TCDForm }

constructor TCDForm.Create;
begin
  inherited Create;
  InvalidateCount := 1;
end;

procedure TCDForm.IncInvalidateCount;
begin
  Inc(InvalidateCount);
end;

function TCDForm.GetFocusedControl: TWinControl;
begin
  if FocusedIntfControl <> nil then Result := FocusedIntfControl
  else if FocusedControl <> nil then Result := FocusedControl
  else Result := LCLForm;
end;

end.

