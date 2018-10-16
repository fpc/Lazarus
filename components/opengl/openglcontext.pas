{
 *****************************************************************************
  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************

  Author: Mattias Gaertner

  Abstract:
    TOpenGLControl is a LCL control with an opengl context.
    It works under the following platforms:
      - gtk with glx    : full
      - gtk2 with glx   : full
      - carbon with agl : full
      - cocoa           : no
      - windows with wgl: full
      - wince           : no
      - qt with glx     : no (started)
      - fpgui with glx  : no
      - nogui           : no
}
unit OpenGLContext;

{$mode objfpc}{$H+}

// choose the right backend depending on used LCL widgetset
{$IFDEF LCLGTK}
  {$IFDEF Linux}
    {$DEFINE UseGtkGLX}
    {$DEFINE HasRGBA}
    {$DEFINE HasRGBBits}
    {$DEFINE OpenGLTargetDefined}
  {$ENDIF}
{$ENDIF}
{$IFDEF LCLGTK2}
  {$IF defined(Linux) or defined(FreeBSD)}
    {$DEFINE UseGtk2GLX}
    {$DEFINE UsesModernGL}
    {$DEFINE HasRGBA}
    {$DEFINE HasRGBBits}
    {$DEFINE HasDebugContext}
    {$DEFINE OpenGLTargetDefined}
  {$ENDIF}
{$ENDIF}
{$IFDEF LCLCarbon}
  {$DEFINE UseCarbonAGL}
  {$DEFINE HasRGBA}
  {$DEFINE HasRGBBits}
  {$DEFINE OpenGLTargetDefined}
{$ENDIF}
{$IFDEF LCLCocoa}
  {$DEFINE UseCocoaNS}
  {$DEFINE UsesModernGL}
  {$DEFINE OpenGLTargetDefined}
  {$DEFINE HasMacRetinaMode}
{$ENDIF}
{$IFDEF LCLWin32}
  {$DEFINE UseWin32WGL}
  {$DEFINE HasRGBA}
  {$DEFINE HasRGBBits}
  {$DEFINE HasDebugContext}
  {$DEFINE OpenGLTargetDefined}
{$ENDIF}
{$IFDEF LCLQT}
  {$DEFINE UseQTGLX}
  {$DEFINE UsesModernGL}
  {$DEFINE HasRGBA}
  {$DEFINE HasRGBBits}
  {$DEFINE OpenGLTargetDefined}
{$ENDIF}
{$IFDEF LCLQT5}
  {$DEFINE UseQTGLX}
  {$DEFINE UsesModernGL}
  {$DEFINE HasRGBA}
  {$DEFINE HasRGBBits}
  {$DEFINE OpenGLTargetDefined}
{$ENDIF}
{$IFNDEF OpenGLTargetDefined}
  {$ERROR this LCL widgetset/OS is not yet supported}
{$ENDIF}

interface

uses
  Classes, SysUtils, LCLProc, Forms, Controls, LCLType, LCLIntf, LResources,
  Graphics, LMessages, WSLCLClasses, WSControls,
{$IFDEF UseGtkGLX}
  GLGtkGlxContext;
{$ENDIF}
{$IFDEF UseGtk2GLX}
  GLGtkGlxContext;
{$ENDIF}
{$IFDEF UseCarbonAGL}
  GLCarbonAGLContext;
{$ENDIF}
{$IFDEF UseCocoaNS}
  GLCocoaNSContext;
{$ENDIF}
{$IFDEF UseWin32WGL}
  GLWin32WGLContext;
{$ENDIF}
{$IFDEF UseQTGLX}
  GLQTContext;
{$ENDIF}

const
  DefaultDepthBits = 24;

type
  TOpenGlCtrlMakeCurrentEvent = procedure(Sender: TObject;
                                          var Allow: boolean) of object;

  TOpenGLControlOption = (ocoMacRetinaMode, ocoRenderAtDesignTime);
  TOpenGLControlOptions = set of TOpenGLControlOption;

  { TCustomOpenGLControl }
  { Sharing:
    You can share opengl contexts. For example:
    Assume OpenGLControl2 and OpenGLControl3 should share the same as
    OpenGLControl1. Then set

        OpenGLControl2.SharedControl:=OpenGLControl1;
        OpenGLControl3.SharedControl:=OpenGLControl1;

     After this OpenGLControl1.SharingControlCount will be two and
     OpenGLControl1.SharingControls will contain OpenGLControl2 and
     OpenGLControl3.
    }

  TCustomOpenGLControl = class(TWinControl)
  private
    FAutoResizeViewport: boolean;
    FCanvas: TCanvas; // only valid at designtime
    FDebugContext: boolean;
    FFrameDiffTime: integer;
    FOnMakeCurrent: TOpenGlCtrlMakeCurrentEvent;
    FOnPaint: TNotifyEvent;
    FCurrentFrameTime: integer; // in msec
    FLastFrameTime: integer; // in msec
    fOpenGLMajorVersion: Cardinal;
    fOpenGLMinorVersion: Cardinal;
    FRGBA: boolean;
    {$IFDEF HasRGBBits}
    FRedBits, FGreenBits, FBlueBits,
    {$ENDIF}
    FMultiSampling, FAlphaBits, FDepthBits, FStencilBits, FAUXBuffers: Cardinal;
    FSharedOpenGLControl: TCustomOpenGLControl;
    FSharingOpenGlControls: TList;
    FOptions: TOpenGLControlOptions;
    function GetSharingControls(Index: integer): TCustomOpenGLControl;
    procedure SetAutoResizeViewport(const AValue: boolean);
    procedure SetDebugContext(AValue: boolean);
    procedure SetOpenGLMajorVersion(AValue: Cardinal);
    procedure SetOpenGLMinorVersion(AValue: Cardinal);
    procedure SetOptions(AValue: TOpenGLControlOptions);
    procedure SetRGBA(const AValue: boolean);
    {$IFDEF HasRGBBits}
    procedure SetRedBits(const AValue: Cardinal);
    procedure SetGreenBits(const AValue: Cardinal);
    procedure SetBlueBits(const AValue: Cardinal);
    {$ENDIF}
    procedure SetMultiSampling(const AMultiSampling: Cardinal);
    procedure SetAlphaBits(const AValue: Cardinal);
    procedure SetDepthBits(const AValue: Cardinal);
    procedure SetStencilBits(const AValue: Cardinal);
    procedure SetAUXBuffers(const AValue: Cardinal);
    procedure SetSharedControl(const AValue: TCustomOpenGLControl);
    function IsOpenGLRenderAllowed: boolean;
  protected
    procedure WMPaint(var Message: TLMPaint); message LM_PAINT;
    procedure WMSize(var Message: TLMSize); message LM_SIZE;
    procedure UpdateFrameTimeDiff;
    procedure OpenGLAttributesChanged;
    procedure CMDoubleBufferedChanged(var Message: TLMessage); message CM_DOUBLEBUFFEREDCHANGED;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    Procedure Paint; virtual;
    procedure RealizeBounds; override;
    procedure DoOnPaint; virtual;
    procedure SwapBuffers; virtual;
    function MakeCurrent(SaveOldToStack: boolean = false): boolean; virtual;
    function ReleaseContext: boolean; virtual;
    function RestoreOldOpenGLControl: boolean;
    function SharingControlCount: integer;
    property SharingControls[Index: integer]: TCustomOpenGLControl read GetSharingControls;
    procedure Invalidate; override;
    procedure EraseBackground(DC: HDC); override;
  public
    property FrameDiffTimeInMSecs: integer read FFrameDiffTime;
    property OnMakeCurrent: TOpenGlCtrlMakeCurrentEvent read FOnMakeCurrent
                                                       write FOnMakeCurrent;
    property OnPaint: TNotifyEvent read FOnPaint write FOnPaint;
    property SharedControl: TCustomOpenGLControl read FSharedOpenGLControl
                                                 write SetSharedControl;
    property AutoResizeViewport: boolean read FAutoResizeViewport
                                         write SetAutoResizeViewport default false;
    property DoubleBuffered stored True default True;
    property ParentDoubleBuffered default False;
    property DebugContext: boolean read FDebugContext write SetDebugContext default false; // create context with debugging enabled. Requires OpenGLMajorVersion!
    property RGBA: boolean read FRGBA write SetRGBA default true;
    {$IFDEF HasRGBBits}
    property RedBits: Cardinal read FRedBits write SetRedBits default 8;
    property GreenBits: Cardinal read FGreenBits write SetGreenBits default 8;
    property BlueBits: Cardinal read FBlueBits write SetBlueBits default 8;
    {$ENDIF}
    property OpenGLMajorVersion: Cardinal read fOpenGLMajorVersion write SetOpenGLMajorVersion default 0;
    property OpenGLMinorVersion: Cardinal read fOpenGLMinorVersion write SetOpenGLMinorVersion default 0;
    { Number of samples per pixel, for OpenGL multi-sampling (anti-aliasing).

      Value <= 1 means that we use 1 sample per pixel, which means no anti-aliasing.
      Higher values mean anti-aliasing. Exactly which values are supported
      depends on GPU, common modern GPUs support values like 2 and 4.

      If this is > 1, and we will not be able to create OpenGL
      with multi-sampling, we will fallback to normal non-multi-sampled context.
      You can query OpenGL values GL_SAMPLE_BUFFERS_ARB and GL_SAMPLES_ARB
      (see ARB_multisample extension) to see how many samples have been
      actually allocated for your context. }
    property MultiSampling: Cardinal read FMultiSampling write SetMultiSampling default 1;

    property AlphaBits: Cardinal read FAlphaBits write SetAlphaBits default 0;
    property DepthBits: Cardinal read FDepthBits write SetDepthBits default DefaultDepthBits;
    property StencilBits: Cardinal read FStencilBits write SetStencilBits default 0;
    property AUXBuffers: Cardinal read FAUXBuffers write SetAUXBuffers default 0;
    property Options: TOpenGLControlOptions read FOptions write SetOptions;
  end;

  { TOpenGLControl }

  TOpenGLControl = class(TCustomOpenGLControl)
  published
    property Align;
    property Anchors;
    property AutoResizeViewport;
    property BorderSpacing;
    property Enabled;
    {$IFDEF HasRGBBits}
    property RedBits;
    property GreenBits;
    property BlueBits;
    {$ENDIF}
    property OpenGLMajorVersion;
    property OpenGLMinorVersion;
    property MultiSampling;
    property AlphaBits;
    property DepthBits;
    property StencilBits;
    property AUXBuffers;
    property OnChangeBounds;
    property OnClick;
    property OnConstrainedResize;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMakeCurrent;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnPaint;
    property OnResize;
    property OnShowHint;
    property PopupMenu;
    property ShowHint;
    property Visible;
  end;

  { TWSOpenGLControl }

  TWSOpenGLControl = class(TWSWinControl)
  published
    class function CreateHandle(const AWinControl: TWinControl;
                                const AParams: TCreateParams): HWND; override;
    class procedure DestroyHandle(const AWinControl: TWinControl); override;
    class function GetDoubleBuffered(const AWinControl: TWinControl): Boolean; override;
  end;



procedure Register;


implementation

{$R openglcontext.res}

var
  OpenGLControlStack: TList = nil;

procedure Register;
begin
  RegisterComponents('OpenGL',[TOpenGLControl]);
end;

{ TCustomOpenGLControl }

function TCustomOpenGLControl.GetSharingControls(Index: integer
  ): TCustomOpenGLControl;
begin
  Result:=TCustomOpenGLControl(FSharingOpenGlControls[Index]);
end;

procedure TCustomOpenGLControl.SetAutoResizeViewport(const AValue: boolean);
begin
  if FAutoResizeViewport=AValue then exit;
  FAutoResizeViewport:=AValue;
  if AutoResizeViewport
  and ([csLoading,csDestroying]*ComponentState=[])
  and IsVisible and HandleAllocated
  and MakeCurrent then
    LOpenGLViewport(Handle,0,0,Width,Height);
end;

procedure TCustomOpenGLControl.SetDebugContext(AValue: boolean);
begin
  if FDebugContext=AValue then Exit;
  FDebugContext:=AValue;
  OpenGLAttributesChanged;
end;

procedure TCustomOpenGLControl.CMDoubleBufferedChanged(var Message: TLMessage);
begin
  inherited;
  OpenGLAttributesChanged;
end;

procedure TCustomOpenGLControl.SetOpenGLMajorVersion(AValue: Cardinal);
begin
  if fOpenGLMajorVersion=AValue then Exit;
  fOpenGLMajorVersion:=AValue;
end;

procedure TCustomOpenGLControl.SetOpenGLMinorVersion(AValue: Cardinal);
begin
  if fOpenGLMinorVersion=AValue then Exit;
  fOpenGLMinorVersion:=AValue;
end;

procedure TCustomOpenGLControl.SetOptions(AValue: TOpenGLControlOptions);
var
  RemovedRenderAtDesignTime: boolean;
begin
  if FOptions=AValue then Exit;

  RemovedRenderAtDesignTime:=
         (ocoRenderAtDesignTime in FOptions) and
    (not (ocoRenderAtDesignTime in AValue));

  FOptions:=AValue;

  { if you remove the flag ocoRenderAtDesignTime at design-time,
    we need to destroy the handle. The call to OpenGLAttributesChanged
    would not do this, so do it explicitly by calling ReCreateWnd
    (ReCreateWnd will destroy handle, and not create new one,
    since IsOpenGLRenderAllowed = false). }
  if (csDesigning in ComponentState) and
     RemovedRenderAtDesignTime and
     HandleAllocated then
    ReCreateWnd(Self);

  OpenGLAttributesChanged();
end;

procedure TCustomOpenGLControl.SetRGBA(const AValue: boolean);
begin
  if FRGBA=AValue then exit;
  FRGBA:=AValue;
  OpenGLAttributesChanged;
end;

{$IFDEF HasRGBBits}
procedure TCustomOpenGLControl.SetRedBits(const AValue: Cardinal);
begin
  if FRedBits=AValue then exit;
  FRedBits:=AValue;
  OpenGLAttributesChanged;
end;

procedure TCustomOpenGLControl.SetGreenBits(const AValue: Cardinal);
begin
  if FGreenBits=AValue then exit;
  FGreenBits:=AValue;
  OpenGLAttributesChanged;
end;

procedure TCustomOpenGLControl.SetBlueBits(const AValue: Cardinal);
begin
  if FBlueBits=AValue then exit;
  FBlueBits:=AValue;
  OpenGLAttributesChanged;
end;
{$ENDIF}

procedure TCustomOpenGLControl.SetMultiSampling(const AMultiSampling: Cardinal);
begin
  if FMultiSampling=AMultiSampling then exit;
  FMultiSampling:=AMultiSampling;
  OpenGLAttributesChanged;
end;

procedure TCustomOpenGLControl.SetAlphaBits(const AValue: Cardinal);
begin
  if FAlphaBits=AValue then exit;
  FAlphaBits:=AValue;
  OpenGLAttributesChanged;
end;

procedure TCustomOpenGLControl.SetDepthBits(const AValue: Cardinal);
begin
  if FDepthBits=AValue then exit;
  FDepthBits:=AValue;
  OpenGLAttributesChanged;
end;

procedure TCustomOpenGLControl.SetStencilBits(const AValue: Cardinal);
begin
  if FStencilBits=AValue then exit;
  FStencilBits:=AValue;
  OpenGLAttributesChanged;
end;

procedure TCustomOpenGLControl.SetAUXBuffers(const AValue: Cardinal);
begin
  if FAUXBuffers=AValue then exit;
  FAUXBuffers:=AValue;
  OpenGLAttributesChanged;
end;

procedure TCustomOpenGLControl.SetSharedControl(
  const AValue: TCustomOpenGLControl);
begin
  if FSharedOpenGLControl=AValue then exit;
  if AValue=Self then
    Raise Exception.Create('A control can not be shared by itself.');
  // unshare old
  if (AValue<>nil) and (AValue.SharedControl<>nil) then
    Raise Exception.Create('Target control is sharing too. A sharing control can not be shared.');
  if FSharedOpenGLControl<>nil then
    FSharedOpenGLControl.FSharingOpenGlControls.Remove(Self);
  // share new
  if (AValue<>nil) and (csDestroying in AValue.ComponentState) then
    FSharedOpenGLControl:=nil
  else begin
    FSharedOpenGLControl:=AValue;
    if (FSharedOpenGLControl<>nil) then begin
      if FSharedOpenGLControl.FSharingOpenGlControls=nil then
        FSharedOpenGLControl.FSharingOpenGlControls:=TList.Create;
      FSharedOpenGLControl.FSharingOpenGlControls.Add(Self);
    end;
  end;
  // recreate handle if needed
  if HandleAllocated and IsOpenGLRenderAllowed then
    ReCreateWnd(Self);
end;

{ OpenGL rendering allowed, because not in design-mode or because we
  should render even in design-mode. }
function TCustomOpenGLControl.IsOpenGLRenderAllowed: boolean;
begin
  Result := (not (csDesigning in ComponentState)) or
    (ocoRenderAtDesignTime in Options);
end;

procedure TCustomOpenGLControl.WMPaint(var Message: TLMPaint);
begin
  Include(FControlState, csCustomPaint);
  inherited WMPaint(Message);
  //debugln('TCustomGTKGLAreaControl.WMPaint A ',dbgsName(Self),' ',dbgsName(FCanvas));
  if (not IsOpenGLRenderAllowed) and (FCanvas<>nil) then begin
    with FCanvas do begin
      if Message.DC <> 0 then
        Handle := Message.DC;
      Brush.Color:=clLtGray;
      Pen.Color:=clRed;
      Rectangle(0,0,Self.Width,Self.Height);
      MoveTo(0,0);
      LineTo(Self.Width,Self.Height);
      MoveTo(0,Self.Height);
      LineTo(Self.Width,0);
      if Message.DC <> 0 then
        Handle := 0;
    end;
  end else begin
    Paint;
  end;
  Exclude(FControlState, csCustomPaint);
end;

procedure TCustomOpenGLControl.WMSize(var Message: TLMSize);
begin
  if (Message.SizeType and Size_SourceIsInterface)>0 then
    DoOnResize;
end;

procedure TCustomOpenGLControl.UpdateFrameTimeDiff;
begin
  FCurrentFrameTime:=integer(GetTickCount);
  if FLastFrameTime=0 then
    FLastFrameTime:=FCurrentFrameTime;
  // calculate time since last call:
  FFrameDiffTime:=FCurrentFrameTime-FLastFrameTime;
  // if the counter is reset restart:
  if (FFrameDiffTime<0) then FFrameDiffTime:=1;
  FLastFrameTime:=FCurrentFrameTime;
end;

procedure TCustomOpenGLControl.OpenGLAttributesChanged;
begin
  if HandleAllocated and
    ( ([csLoading,csDestroying]*ComponentState=[]) and IsOpenGLRenderAllowed ) then
    RecreateWnd(Self);
end;

procedure TCustomOpenGLControl.EraseBackground(DC: HDC);
begin
  if DC=0 then ;
  // everything is painted, so erasing the background is not needed
end;

constructor TCustomOpenGLControl.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  ParentDoubleBuffered:=False;
  FDoubleBuffered:=true;
  FRGBA:=true;
  {$IFDEF HasRGBBits}
  FRedBits:=8;
  FGreenBits:=8;
  FBlueBits:=8;
  {$ENDIF}
  fOpenGLMajorVersion:=0;
  fOpenGLMinorVersion:=0;
  FMultiSampling:=1;
  FDepthBits:=DefaultDepthBits;
  ControlStyle:=ControlStyle-[csSetCaption];
  if not IsOpenGLRenderAllowed then begin
    FCanvas := TControlCanvas.Create;
    TControlCanvas(FCanvas).Control := Self;
  end else
    FCompStyle:=csNonLCL;
  SetInitialBounds(0, 0, 160, 90);
end;

destructor TCustomOpenGLControl.Destroy;
begin
  if FSharingOpenGlControls<>nil then begin
    while SharingControlCount>0 do
      SharingControls[SharingControlCount-1].SharedControl:=nil;
    FreeAndNil(FSharingOpenGlControls);
  end;
  SharedControl:=nil;
  if OpenGLControlStack<>nil then begin
    OpenGLControlStack.Remove(Self);
    if OpenGLControlStack.Count=0 then
      FreeAndNil(OpenGLControlStack);
  end;
  FCanvas.Free;
  FCanvas:=nil;
  inherited Destroy;
end;

procedure TCustomOpenGLControl.Paint;
begin
  if IsVisible and HandleAllocated then begin
    UpdateFrameTimeDiff;
    if IsOpenGLRenderAllowed and ([csDestroying]*ComponentState=[]) then begin
      if AutoResizeViewport then begin
        if not MakeCurrent then exit;
        LOpenGLViewport(Handle,0,0,Width,Height);
      end;
    end;
    //LOpenGLClip(Handle);
    DoOnPaint;
  end;
end;

procedure TCustomOpenGLControl.RealizeBounds;
begin
  if IsVisible and HandleAllocated
  and IsOpenGLRenderAllowed
  and ([csDestroying]*ComponentState=[])
  and AutoResizeViewport then begin
    if MakeCurrent then
      LOpenGLViewport(Handle,0,0,Width,Height);
  end;
  inherited RealizeBounds;
end;

procedure TCustomOpenGLControl.DoOnPaint;
begin
  if Assigned(OnPaint) then begin
    if not MakeCurrent then exit;
    OnPaint(Self);
  end;
end;

procedure TCustomOpenGLControl.SwapBuffers;
begin
  LOpenGLSwapBuffers(Handle);
end;

function TCustomOpenGLControl.MakeCurrent(SaveOldToStack: boolean): boolean;
var
  Allowed: Boolean;
begin
  if not IsOpenGLRenderAllowed then exit(false);
  if Assigned(FOnMakeCurrent) then begin
    Allowed:=true;
    OnMakeCurrent(Self,Allowed);
    if not Allowed then begin
      Result:=False;
      exit;
    end;
  end;
  // make current
  Result:=LOpenGLMakeCurrent(Handle);
  if Result and SaveOldToStack then begin
    // on success push on stack
    if OpenGLControlStack=nil then
      OpenGLControlStack:=TList.Create;
    OpenGLControlStack.Add(Self);
  end;
end;

function TCustomOpenGLControl.ReleaseContext: boolean;
begin
  Result:=false;
  if not HandleAllocated then exit;
  Result:=LOpenGLReleaseContext(Handle);
end;

function TCustomOpenGLControl.RestoreOldOpenGLControl: boolean;
var
  RestoredControl: TCustomOpenGLControl;
begin
  Result:=false;
  // check if the current context is on stack
  if (OpenGLControlStack=nil) or (OpenGLControlStack.Count=0) then exit;
  // pop
  OpenGLControlStack.Delete(OpenGLControlStack.Count-1);
  // make old control the current control
  if OpenGLControlStack.Count>0 then begin
    RestoredControl:=
      TCustomOpenGLControl(OpenGLControlStack[OpenGLControlStack.Count-1]);
    if (not LOpenGLMakeCurrent(RestoredControl.Handle)) then
      exit;
  end else begin
    FreeAndNil(OpenGLControlStack);
  end;
  Result:=true;
end;

function TCustomOpenGLControl.SharingControlCount: integer;
begin
  if FSharingOpenGlControls=nil then
    Result:=0
  else
    Result:=FSharingOpenGlControls.Count;
end;

procedure TCustomOpenGLControl.Invalidate;
begin
  if csCustomPaint in FControlState then exit;
  inherited Invalidate;
end;

{ TWSOpenGLControl }

class function TWSOpenGLControl.CreateHandle(const AWinControl: TWinControl;
  const AParams: TCreateParams): HWND;
var
  OpenGlControl: TCustomOpenGLControl;
  AttrControl: TCustomOpenGLControl;
begin
  OpenGlControl:=AWinControl as TCustomOpenGLControl;
  if not OpenGlControl.IsOpenGLRenderAllowed then
  begin
    // do not use "inherited CreateHandle", because the LCL changes the hierarchy at run time
    Result:=TWSWinControlClass(ClassParent).CreateHandle(AWinControl,AParams);
  end
  else
  begin
    if OpenGlControl.SharedControl<>nil then
      AttrControl:=OpenGlControl.SharedControl
    else
      AttrControl:=OpenGlControl;
    Result:=LOpenGLCreateContext(OpenGlControl,WSPrivate,
                                 OpenGlControl.SharedControl,
                                 AttrControl.DoubleBuffered,
                                 {$IFDEF HasMacRetinaMode}
                                 ocoMacRetinaMode in OpenGlControl.Options,
                                 {$ENDIF}
                                 {$IFDEF HasRGBA}
                                 AttrControl.RGBA,
                                 {$ENDIF}
                                 {$IFDEF HasDebugContext}
                                 AttrControl.DebugContext,
                                 {$ENDIF}
                                 {$IFDEF HasRGBBits}
                                 AttrControl.RedBits,
                                 AttrControl.GreenBits,
                                 AttrControl.BlueBits,
                                 {$ENDIF}
                                 {$IFDEF UsesModernGL}
                                 AttrControl.OpenGLMajorVersion,
                                 AttrControl.OpenGLMinorVersion,
                                 {$ENDIF}
                                 AttrControl.MultiSampling,
                                 AttrControl.AlphaBits,
                                 AttrControl.DepthBits,
                                 AttrControl.StencilBits,
                                 AttrControl.AUXBuffers,
                                 AParams);
  end;
end;

class procedure TWSOpenGLControl.DestroyHandle(const AWinControl: TWinControl);
begin
  LOpenGLDestroyContextInfo(AWinControl);
  // do not use "inherited DestroyHandle", because the LCL changes the hierarchy at run time
  TWSWinControlClass(ClassParent).DestroyHandle(AWinControl);
end;

class function TWSOpenGLControl.GetDoubleBuffered(const AWinControl: TWinControl): Boolean;
begin
  Result := False;
end;

initialization
  RegisterWSComponent(TCustomOpenGLControl,TWSOpenGLControl);

end.
