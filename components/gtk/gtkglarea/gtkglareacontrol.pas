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
  { TCustomGTKGLAreaControl }

  TCustomGTKGLAreaControl = class(TWinControl)
  private
    FCanvas: TCanvas; // only valid at designtime
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
    procedure MakeCurrent; virtual;
  public
    property Widget: PGtkGLArea read GetWidget;
    property OnPaint: TNotifyEvent read FOnPaint write FOnPaint;
    property FrameDiffTimeInMSecs: integer read FFrameDiffTime;
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
  SetBounds(1, 1, 160, 90);
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
  and (gint(True) = gtk_gl_area_make_current(Widget)) then begin
    UpdateFrameTimeDiff;
    DoOnPaint;
  end;
end;

procedure TCustomGTKGLAreaControl.DoOnResize;
begin
  if (not (csDesigning in ComponentState))
  and Enabled and Visible and HandleAllocated
  and (gint(True) = gtk_gl_area_make_current(Widget)) then
    glViewport (0, 0, Width, Height);
  inherited DoOnResize;
end;

procedure TCustomGTKGLAreaControl.DoOnPaint;
begin
  if Assigned(OnPaint) then OnPaint(Self);
end;

procedure TCustomGTKGLAreaControl.SwapBuffers;
begin
  gtk_gl_area_swap_buffers(Widget);
end;

procedure TCustomGTKGLAreaControl.MakeCurrent;
begin
  gtk_gl_area_make_current(Widget);
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

end.
