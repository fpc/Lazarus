unit customdrawnproc;

{$mode objfpc}{$H+}

interface

uses
  // rtl+ftl
  Types, Classes, SysUtils,
  fpimage, fpcanvas,
  // Custom Drawn Canvas
  IntfGraphics, lazcanvas, lazregions,
  // LCL
  GraphType, Controls, LCLMessageGlue, WSControls, LCLType, LCLProc,
  Forms, Graphics, customdrawncontrols;

type
  TUpdateLazImageFormat = (
    clfRGB16_R5G6B5,
    clfRGB24, clfRGB24UpsideDown, clfBGR24,
    clfBGRA32, clfRGBA32, clfARGB32);

  TCDWinControl = class
  public
    Region: TLazRegionWithChilds;
    WinControl: TWinControl;
    CDControl: TCDControl;
    Children: TFPList;
  end;

  TCDForm = class
  public
    LCLForm: TCustomForm;
    Children: TFPList; // of TCDWinControl;
    NativeHandle: HWND;
    //
    LastMouseDownControl: TWinControl; // Stores the control which should receive the next MouseUp
    // painting objects
    Image: TLazIntfImage;
    Canvas: TLazCanvas;
  end;

  TCDNonNativeForm = class(TCDForm)
  public
  end;

  TCDBitmap = class
  public
    Image: TLazIntfImage;
  end;

// Routines for form managing (both native and non-native)

procedure AddCDWinControlToForm(const AForm: TCustomForm; ACDWinControl: TCDWinControl);
function GetCDWinControlList(const AForm: TCustomForm): TFPList;

procedure InitNonNativeForms();
function GetCurrentForm(): TCDNonNativeForm;
function AddNewForm(AForm: TCustomForm): TCDNonNativeForm;
procedure AddFormWithCDHandle(AHandle: TCDForm);
function FindFormWithNativeHandle(AHandle: HWND): TCDForm;
procedure ShowForm(ACDForm: TCDNonNativeForm);
procedure HideForm(ACDForm: TCDNonNativeForm);

// Routines for non-native wincontrol

procedure UpdateControlLazImageAndCanvas(var AImage: TLazIntfImage;
  var ACanvas: TLazCanvas; AWidth, AHeight: Integer; AFormat: TUpdateLazImageFormat;
  AData: Pointer = nil; AForceUpdate: Boolean = False; AFreeImageOnUpdate: Boolean = True);
procedure DrawFormBackground(var AImage: TLazIntfImage; var ACanvas: TLazCanvas);
procedure RenderChildWinControls(var AImage: TLazIntfImage;
  var ACanvas: TLazCanvas; ACDControlsList: TFPList);
//procedure RenderWinControl(var AImage: TLazIntfImage;
//  var ACanvas: TLazCanvas; ACDControlsList: TFPList);
function FindControlWhichReceivedEvent(AForm: TCustomForm;
  AControlsList: TFPList; AX, AY: Integer): TWinControl;
function FindControlPositionRelativeToTheForm(ALCLControl: TWinControl): TPoint;
function FormPosToControlPos(ALCLControl: TWinControl; AX, AY: Integer): TPoint;

// Other routines

function DateTimeToMilliseconds(aDateTime: TDateTime): Int64;
function IsValidDC(ADC: HDC): Boolean;
function IsValidGDIObject(AGDIObj: HGDIOBJ): Boolean;
function IsValidBitmap(ABitmap: HBITMAP): Boolean;

implementation

// List with the Z-order of non-native forms, index=0 is the bottom-most form
var
  NonNativeForms: TFPList = nil;

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
var
  lCount: Integer;
begin
  {$IFDEF VerboseCDForms}
    DebugLn('GetCurrentForm');
  {$ENDIF}
  InitNonNativeForms();
  lCount := NonNativeForms.Count;
  if lCount = 0 then Result := nil
  else Result := TCDNonNativeForm(NonNativeForms.Items[lCount-1]);
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
var
  lCount, lCurIndex: Integer;
begin
  InitNonNativeForms();
  lCount := NonNativeForms.Count;
  lCurIndex := NonNativeForms.IndexOf(ACDForm);
  {$IFDEF VerboseCDForms}
    DebugLn(Format('ShowForm lOldIndex=%d lNewIndex=%d', [lCurIndex, lCount-1]));
  {$ENDIF}
  NonNativeForms.Move(lCurIndex, lCount-1);
end;

procedure HideForm(ACDForm: TCDNonNativeForm);
var
  lCount, lCurIndex: Integer;
begin
  InitNonNativeForms();
  lCount := NonNativeForms.Count;
  lCurIndex := NonNativeForms.IndexOf(ACDForm);
  {$IFDEF VerboseCDForms}
    DebugLn(Format('HideForm lOldIndex=%d lNewIndex=0', [lCurIndex]));
  {$ENDIF}
  NonNativeForms.Move(lCurIndex, 0);
end;

// If AForceUpdate=True then it will update even if the width and height remain the same
procedure UpdateControlLazImageAndCanvas(var AImage: TLazIntfImage;
  var ACanvas: TLazCanvas; AWidth, AHeight: Integer; AFormat: TUpdateLazImageFormat;
  AData: Pointer = nil; AForceUpdate: Boolean = False; AFreeImageOnUpdate: Boolean = True);
var
  lRawImage: TRawImage;
  lPixelSize: Byte;
begin
  {$IFDEF VerboseCDForms}
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
    AImage.SetRawImage(lRawImage);

    if (ACanvas <> nil) then ACanvas.Free;
    ACanvas := TLazCanvas.Create(AImage);
  end;
  {$IFDEF VerboseCDForms}
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

procedure RenderChildWinControls(var AImage: TLazIntfImage;
  var ACanvas: TLazCanvas; ACDControlsList: TFPList);
var
  i, lChildrenCount: Integer;
  lCDWinControl: TCDWinControl;
  lWinControl, lParentControl: TWinControl;
  struct : TPaintStruct;
  lCanvas: TCanvas;
  lBaseWindowOrg: TPoint;
begin
  lChildrenCount := ACDControlsList.Count;
  {$ifdef VerboseCDWinControl}
  DebugLn(Format('[RenderChildWinControls] ACanvas=%x ACDControlsList=%x lChildrenCount=%d',
    [PtrInt(ACanvas), PtrInt(ACDControlsList), lChildrenCount]));
  {$endif}
  FillChar(struct, SizeOf(TPaintStruct), 0);
  struct.hdc := HDC(ACanvas);

  for i := 0 to lChildrenCount-1 do
  begin
    lCDWinControl := TCDWinControl(ACDControlsList.Items[i]);
    lWinControl := lCDWinControl.WinControl;
    {$ifdef VerboseCDWinControl}
    DebugLn(Format('[RenderChildWinControls] i=%d lWinControl=%x Name=%s:%s Left=%d'
      + ' Top=%d Width=%d Height=%d', [i, PtrInt(lWinControl), lWinControl.Name, lWinControl.ClassName,
      lWinControl.Left, lWinControl.Top, lWinControl.Width, lWinControl.Height]));
    {$endif}
    if lWinControl.Visible = False then Continue;

    // lBaseWindowOrg makes debugging easier
    // Iterate to find the appropriate BaseWindowOrg relative to the parent control
    lBaseWindowOrg := FindControlPositionRelativeToTheForm(lWinControl);
    ACanvas.BaseWindowOrg := lBaseWindowOrg;
    ACanvas.WindowOrg := Point(0, 0);

    // Prepare the clippping relative to the form
    ACanvas.Clipping := True;
    lCDWinControl.Region.Rect := Bounds(lBaseWindowOrg.X, lBaseWindowOrg.Y, lWinControl.Width, lWinControl.Height);
    ACanvas.ClipRegion := lCDWinControl.Region;

    // Save the Canvas state
    ACanvas.SaveState;
    ACanvas.ResetCanvasState;

    {$ifdef VerboseCDWinControl}
    DebugLn(Format('[RenderChildWinControls] i=%d before LCLSendPaintMsg', [i]));
    {$endif}
    LCLSendPaintMsg(lCDWinControl.WinControl, struct.hdc, @struct);

    // Now Draw all sub-controls
    if lCDWinControl.Children <> nil then
      RenderChildWinControls(AImage, ACanvas, lCDWinControl.Children);

    {$ifdef VerboseCDWinControl}
    DebugLn(Format('[RenderChildWinControls] i=%d Finished child controls', [i]));
    {$endif}

    // Now restore it
    ACanvas.RestoreState;
  end;

  ACanvas.Clipping := False;
  ACanvas.BaseWindowOrg := Point(0, 0);
  ACanvas.WindowOrg := Point(0, 0);
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

end.

