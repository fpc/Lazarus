{
 *****************************************************************************
 *                                                                           *
 *  See the file COPYING.modifiedLGPL, included in this distribution,        *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************

  Author: Mattias Gaertner

  Abstract:
    TOpenGLControl is a LCL control with an opengl context.
    It works under the following platforms:
      - gtk with glx    : full
      - gtk2 with glx   : started, not yet working
      - carbon with agl : full
      - windows with wgl: full
}
unit OpenGLContext;

{$mode objfpc}{$H+}

{$IFDEF LCLGTK}
  {$DEFINE UseGtkGLX}
  {$DEFINE OpenGLTargetDefined}
{$ENDIF}
{$IFDEF LCLGTK2}
  {$DEFINE UseGtkGLX}
  {$DEFINE OpenGLTargetDefined}
{$ENDIF}
{$IFDEF LCLCarbon}
  {$DEFINE UseCarbonAGL}
  {$DEFINE OpenGLTargetDefined}
{$ENDIF}
{$IFDEF LCLWin32}
  {$DEFINE UseWin32WGL}
  {$DEFINE OpenGLTargetDefined}
{$ENDIF}
{$IFNDEF OpenGLTargetDefined}
  {$ERROR this target is not yet supported}
{$ENDIF}

interface

uses
  Classes, SysUtils, LCLProc, Forms, Controls, LCLType, LCLIntf, LResources,
  Graphics, LMessages, WSLCLClasses, WSControls,
{$IFDEF UseGtkGLX}
  GLGtkGlxContext;
{$ENDIF}
{$IFDEF UseCarbonAGL}
  GLCarbonAGLContext;
{$ENDIF}
{$IFDEF UseWin32WGL}
  GLWin32WGLContext;
{$ENDIF}


type
  TOpenGlCtrlMakeCurrentEvent = procedure(Sender: TObject;
                                          var Allow: boolean) of object;

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
    FDoubleBuffered: boolean;
    FFrameDiffTime: integer;
    FOnMakeCurrent: TOpenGlCtrlMakeCurrentEvent;
    FOnPaint: TNotifyEvent;
    FCurrentFrameTime: integer; // in msec
    FLastFrameTime: integer; // in msec
    FRGBA: boolean;
    FSharedOpenGLControl: TCustomOpenGLControl;
    FSharingOpenGlControls: TList;
    function GetSharingControls(Index: integer): TCustomOpenGLControl;
    procedure SetAutoResizeViewport(const AValue: boolean);
    procedure SetDoubleBuffered(const AValue: boolean);
    procedure SetRGBA(const AValue: boolean);
    procedure SetSharedControl(const AValue: TCustomOpenGLControl);
  protected
    procedure WMPaint(var Message: TLMPaint); message LM_PAINT;
    procedure UpdateFrameTimeDiff;
    procedure OpenGLAttributesChanged;
    procedure EraseBackground(DC: HDC); override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    Procedure Paint; virtual;
    procedure RealizeBounds; override;
    procedure DoOnPaint; virtual;
    procedure SwapBuffers; virtual;
    function MakeCurrent(SaveOldToStack: boolean = false): boolean; virtual;
    function RestoreOldOpenGLControl: boolean;
    function SharingControlCount: integer;
    property SharingControls[Index: integer]: TCustomOpenGLControl read GetSharingControls;
  public
    property FrameDiffTimeInMSecs: integer read FFrameDiffTime;
    property OnMakeCurrent: TOpenGlCtrlMakeCurrentEvent read FOnMakeCurrent
                                                       write FOnMakeCurrent;
    property OnPaint: TNotifyEvent read FOnPaint write FOnPaint;
    property SharedControl: TCustomOpenGLControl read FSharedOpenGLControl
                                                 write SetSharedControl;
    property AutoResizeViewport: boolean read FAutoResizeViewport
                                         write SetAutoResizeViewport;
    property DoubleBuffered: boolean read FDoubleBuffered write SetDoubleBuffered default true;
    property RGBA: boolean read FRGBA write SetRGBA default true;
  end;

  { TOpenGLControl }

  TOpenGLControl = class(TCustomOpenGLControl)
  published
    property Align;
    property Anchors;
    property AutoResizeViewport;
    property BorderSpacing;
    property Enabled;
    property OnChangeBounds;
    property OnClick;
    property OnConstrainedResize;
    property OnDblClick;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
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

  TWSOpenGLControl = class(TWidgetSetWSWinControl)
  public
    class function CreateHandle(const AWinControl: TWinControl;
                                const AParams: TCreateParams): HWND; override;
    class procedure DestroyHandle(const AWinControl: TWinControl); override;
  end;



procedure Register;


implementation

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
    LOpenGLViewport(0,0,Width,Height);
end;

procedure TCustomOpenGLControl.SetDoubleBuffered(const AValue: boolean);
begin
  if FDoubleBuffered=AValue then exit;
  FDoubleBuffered:=AValue;
  OpenGLAttributesChanged;
end;

procedure TCustomOpenGLControl.SetRGBA(const AValue: boolean);
begin
  if FRGBA=AValue then exit;
  FRGBA:=AValue;
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
  if HandleAllocated and (not (csDesigning in ComponentState)) then
    ReCreateWnd(Self);
end;

procedure TCustomOpenGLControl.WMPaint(var Message: TLMPaint);
begin
  Include(FControlState, csCustomPaint);
  inherited WMPaint(Message);
  //debugln('TCustomGTKGLAreaControl.WMPaint A ',dbgsName(Self),' ',dbgsName(FCanvas));
  if (csDesigning in ComponentState) and (FCanvas<>nil) then begin
    with FCanvas do begin
      Brush.Color:=clLtGray;
      Pen.Color:=clRed;
      Rectangle(0,0,Self.Width-1,Self.Height-1);
      MoveTo(0,0);
      LineTo(Self.Width,Self.Height);
      MoveTo(0,Self.Height);
      LineTo(Self.Width,0);
    end;
  end else begin
    Paint;
  end;
  Exclude(FControlState, csCustomPaint);
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
  if HandleAllocated
  and ([csLoading,csDesigning,csDestroying]*ComponentState=[]) then
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
  FDoubleBuffered:=true;
  FRGBA:=true;
  ControlStyle:=ControlStyle-[csSetCaption];
  if (csDesigning in ComponentState) then begin
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
    if ([csDesigning,csDestroying]*ComponentState=[]) then begin
      if not MakeCurrent then exit;
      if AutoResizeViewport then
        LOpenGLViewport(0,0,Width,Height);
    end;
    DoOnPaint;
  end;
end;

procedure TCustomOpenGLControl.RealizeBounds;
begin
  if IsVisible and HandleAllocated
  and ([csDesigning,csDestroying]*ComponentState=[]) and MakeCurrent then begin
    if AutoResizeViewport then
      LOpenGLViewport(0,0,Width,Height);
  end;
  inherited RealizeBounds;
end;

procedure TCustomOpenGLControl.DoOnPaint;
begin
  if Assigned(OnPaint) then OnPaint(Self);
end;

procedure TCustomOpenGLControl.SwapBuffers;
begin
  LOpenGLSwapBuffers(Handle);
end;

function TCustomOpenGLControl.MakeCurrent(SaveOldToStack: boolean): boolean;
var
  Allowed: Boolean;
begin
  if csDesigning in ComponentState then exit(false);
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

{ TWSOpenGLControl }

class function TWSOpenGLControl.CreateHandle(const AWinControl: TWinControl;
  const AParams: TCreateParams): HWND;
var
  OpenGlControl: TCustomOpenGLControl;
  AttrControl: TCustomOpenGLControl;
begin
  if csDesigning in AWinControl.ComponentState then
    Result:=inherited CreateHandle(AWinControl,AParams)
  else begin
    OpenGlControl:=AWinControl as TCustomOpenGLControl;
    if OpenGlControl.SharedControl<>nil then
      AttrControl:=OpenGlControl.SharedControl
    else
      AttrControl:=OpenGlControl;
    Result:=LOpenGLCreateContext(OpenGlControl,WSPrivate,
                                 OpenGlControl.SharedControl,
                                 AttrControl.DoubleBuffered,AttrControl.RGBA);
  end;
end;

class procedure TWSOpenGLControl.DestroyHandle(const AWinControl: TWinControl);
begin
  LOpenGLDestroyContextInfo(AWinControl);
  inherited DestroyHandle(AWinControl);
end;

initialization
  RegisterWSComponent(TCustomOpenGLControl,TWSOpenGLControl);
  {$I openglcontext.lrs}

end.

