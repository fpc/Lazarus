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

}
unit GTKGLAreaControl;

{$MODE objfpc}{$H+}

interface

uses
  Classes, SysUtils, {$IFDEF VER1_0}Linux{$ELSE}Unix{$ENDIF}, Forms,
  Controls, Graphics, LMessages, VCLGlobals, InterfaceBase, GTKInt, LResources,
  GLib, NVGL, GTKGLArea_Int;
  
type
  TGtkGlAreaMakeCurrentEvent = procedure(Sender: TObject;
    var Allow: boolean) of object;

  { TCustomGTKGLAreaControl }

  TCustomGTKGLAreaControl = class(TWinControl)
  private
    FCanvas: TCanvas; // only valid at designtime
    FOnMakeCurrent: TGtkGlAreaMakeCurrentEvent;
    FOnPaint: TNotifyEvent;
    FCurrentFrameTime: integer; // in msec
    FLastFrameTime: integer; // in msec
    FFrameDiffTime: integer; // in msec
  protected
    procedure WMPaint(var Message: TLMPaint); message LM_PAINT;
    function GetWidget: PGtkGLArea;
    function CreateWindowHandle(const AParams: TCreateParams): THandle; override;
    procedure UpdateFrameTimeDiff;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    Procedure Paint; virtual;
    procedure DoOnResize; override;
    procedure DoOnPaint; virtual;
    procedure SwapBuffers; virtual;
    function MakeCurrent: integer; virtual;
    function RestoreOldGtkGLAreaControl: boolean;
  public
    property Widget: PGtkGLArea read GetWidget;
    property OnPaint: TNotifyEvent read FOnPaint write FOnPaint;
    property FrameDiffTimeInMSecs: integer read FFrameDiffTime;
    property OnMakeCurrent: TGtkGlAreaMakeCurrentEvent read FOnMakeCurrent
                                                       write FOnMakeCurrent;
  end;
  
  
  { TGTKGLAreaControl }
  
  TGTKGLAreaControl = class(TCustomGTKGLAreaControl)
  published
    property Align;
    property Anchors;
    property Enabled;
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
  

function GetCurrentGtkGLAreaControl: TGTKGLAreaControl;

procedure Register;

implementation

const
  InitAttrList: array [1..11] of LongInt = (
    GDK_GL_RGBA,
    GDK_GL_RED_SIZE, 1,
    GDK_GL_GREEN_SIZE, 1,
    GDK_GL_BLUE_SIZE, 1,
    GDK_GL_DEPTH_SIZE,1,
    GDK_GL_DOUBLEBUFFER,
    GDK_GL_None
    );

var
  GtkGLAreaControlStack: TList;

function GetCurrentGtkGLAreaControl: TGTKGLAreaControl;
begin
  if (GtkGLAreaControlStack<>nil)
  and (GtkGLAreaControlStack.Count>0) then
    Result:=TGTKGLAreaControl(GtkGLAreaControlStack[GtkGLAreaControlStack.Count-1])
  else
    Result:=nil;
end;

procedure Register;
begin
  RegisterComponents('OpenGL',[TGTKGLAreaControl]);
end;

{ TCustomGTKGLAreaControl }

constructor TCustomGTKGLAreaControl.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  ControlStyle:=ControlStyle-[csSetCaption];
  if (csDesigning in ComponentState) then begin
    FCanvas := TControlCanvas.Create;
    TControlCanvas(FCanvas).Control := Self;
  end else
    FCompStyle:=csNonLCL;
  SetInitialBounds(0, 0, 160, 90);
end;

destructor TCustomGTKGLAreaControl.Destroy;
begin
  FCanvas.Free;
  FCanvas:=nil;
  inherited Destroy;
end;

procedure TCustomGTKGLAreaControl.Paint;
begin
  if (not (csDesigning in ComponentState))
  and Enabled and Visible and HandleAllocated
  and (gint(True) = MakeCurrent) then begin
    try
      UpdateFrameTimeDiff;
      DoOnPaint;
    finally
      RestoreOldGtkGLAreaControl;
    end;
  end;
end;

procedure TCustomGTKGLAreaControl.DoOnResize;
var
  RestoreNeeded: Boolean;
begin
  RestoreNeeded:=false;
  if (not (csDesigning in ComponentState))
  and Enabled and Visible and HandleAllocated
  and (gint(True) = MakeCurrent) then begin
    RestoreNeeded:=true;
    glViewport (0, 0, Width, Height);
  end;
  try
    inherited DoOnResize;
  finally
    if RestoreNeeded then
      RestoreOldGtkGLAreaControl;
  end;
end;

procedure TCustomGTKGLAreaControl.DoOnPaint;
begin
  if Assigned(OnPaint) then OnPaint(Self);
end;

procedure TCustomGTKGLAreaControl.SwapBuffers;
begin
  gtk_gl_area_swap_buffers(Widget);
end;

function TCustomGTKGLAreaControl.MakeCurrent: integer;
var
  Allowed: Boolean;
begin
  if Assigned(FOnMakeCurrent) then begin
    Allowed:=true;
    OnMakeCurrent(Self,Allowed);
    if not Allowed then begin
      Result:=gint(False);
      exit;
    end;
  end;
  Result:=gtk_gl_area_make_current(Widget);
  if Result=gint(True) then begin
    // on success push on stack
    if GtkGLAreaControlStack=nil then
      GtkGLAreaControlStack:=TList.Create;
    GtkGLAreaControlStack.Add(Self);
  end;
end;

function TCustomGTKGLAreaControl.RestoreOldGtkGLAreaControl: boolean;
var
  RestoredControl: TGTKGLAreaControl;
begin
  Result:=false;
  // check if the current context is on stack
  if (GtkGLAreaControlStack=nil) or (GtkGLAreaControlStack.Count=0) then exit;
  // pop
  GtkGLAreaControlStack.Delete(GtkGLAreaControlStack.Count-1);
  // make old control the current control
  if GtkGLAreaControlStack.Count>0 then begin
    RestoredControl:=
      TGTKGLAreaControl(GtkGLAreaControlStack[GtkGLAreaControlStack.Count-1]);
    if gtk_gl_area_make_current(RestoredControl.Widget)<>gint(true) then
      exit;
  end;
  Result:=true;
end;

procedure TCustomGTKGLAreaControl.WMPaint(var Message: TLMPaint);
begin
  Include(FControlState, csCustomPaint);
  inherited WMPaint(Message);
  if (csDesigning in ComponentState) and (FCanvas<>nil) then begin
    with FCanvas do begin
      Brush.Color:=clLtGray;
      Pen.Color:=clRed;
      Rectangle(0,0,Width-1,Height-1);
      MoveTo(0,0);
      LineTo(Width,Height);
      MoveTo(0,Height);
      LineTo(Width,0);
    end;
  end else begin
    Paint;
  end;
  Exclude(FControlState, csCustomPaint);
end;

function TCustomGTKGLAreaControl.GetWidget: PGtkGLArea;
begin
  if HandleAllocated then
    Result:=PGtkGLArea(Handle)
  else
    Result:=nil;
end;

function TCustomGTKGLAreaControl.CreateWindowHandle(const AParams: TCreateParams
  ): THandle;
var
  NewWidget: Pointer;
begin
  if csDesigning in ComponentState then
    Result:=inherited CreateWindowHandle(AParams)
  else begin
    NewWidget:=gtk_gl_area_new(Plongint(@InitAttrList));
    Result:=longint(NewWidget);
    TGTKWidgetSet(InterfaceObject).FinishComponentCreate(Self,NewWidget,true);
  end;
end;

procedure TCustomGTKGLAreaControl.UpdateFrameTimeDiff;
var
  hour, minutes, secs, msecs, usecs: word;
begin
  GetTime(hour, minutes, secs, msecs, usecs);
  FCurrentFrameTime:=(((minutes*60)+secs) * 1000)+msecs;
  if FLastFrameTime=0 then
    FLastFrameTime:=FCurrentFrameTime;
  // calculate time since last call:
  FFrameDiffTime:=FCurrentFrameTime-FLastFrameTime;
  // if the hour changed, the minutes restarts:
  if (FFrameDiffTime<0) then inc(FFrameDiffTime,60*60*1000);
  FLastFrameTime:=FCurrentFrameTime;
end;

initialization
  {$i gtkglarea.lrs}
  GtkGLAreaControlStack:=nil;
  
finalization
  FreeAndNil(GtkGLAreaControlStack);

end.
