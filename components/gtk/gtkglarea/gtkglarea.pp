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
unit GTKGLArea;

{$MODE objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, Graphics, LMessages, VCLGlobals,
  InterfaceBase, GTKInt, LResources, GLib, NVGL, NVGLX, GTKGLArea_Int;
  
type
  TCustomGTKGLAreaControl = class(TWinControl)
  private
    FCanvas: TCanvas; // only valid at designtime
    FOnPaint: TNotifyEvent;
  protected
    procedure WMPaint(var Message: TLMPaint); message LM_PAINT;
    function GetWidget: PGtkGLArea;
    procedure CreateComponent(TheOwner: TComponent); override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    Procedure Paint; virtual;
    procedure DoOnResize; override;
  public
    property Widget: PGtkGLArea read GetWidget;
    property OnPaint: TNotifyEvent read FOnPaint write FOnPaint;
  end;
  
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
  if Assigned(OnPaint) then OnPaint(Self);
end;

procedure TCustomGTKGLAreaControl.DoOnResize;
begin
  if (gint(True) = gtk_gl_area_make_current(Widget)) then
    glViewport (0, 0, Width, Height);
  inherited DoOnResize;
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
    if (Widget<>nil)
    and (gint(True) = gtk_gl_area_make_current(Widget)) then
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

procedure TCustomGTKGLAreaControl.CreateComponent(TheOwner: TComponent);
var
  NewWidget: Pointer;
begin
  if csDesigning in ComponentState then
    inherited CreateComponent(TheOwner)
  else begin
    NewWidget:=gtk_gl_area_new(Plongint(@InitAttrList));
    Handle := longint(NewWidget);
    TGtkObject(InterfaceObject).FinishComponentCreate(Self,NewWidget,true);
  end;
end;

initialization
  {$i gtkglarea.lrs}

end.
