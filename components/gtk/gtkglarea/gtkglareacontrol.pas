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
  Classes, SysUtils, LCLType, {$IFDEF VER1_0}Linux{$ELSE}Unix{$ENDIF}, Forms,
  Controls, Graphics, LMessages, InterfaceBase, WSLCLClasses, WSControls,
  LResources, GTKInt, GLib, Gtk, NVGL, GTKGLArea_Int;
  
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
    FSharedArea: TCustomGTKGLAreaControl;
    FSharingAreas: TList;
    function GetSharingAreas(Index: integer): TCustomGTKGLAreaControl;
    procedure SetSharedArea(const AValue: TCustomGTKGLAreaControl);
  protected
    procedure WMPaint(var Message: TLMPaint); message LM_PAINT;
    function GetWidget: PGtkGLArea;
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
    function SharingAreasCount: integer;
    property SharingAreas[Index: integer]: TCustomGTKGLAreaControl read GetSharingAreas;
  public
    property FrameDiffTimeInMSecs: integer read FFrameDiffTime;
    property OnMakeCurrent: TGtkGlAreaMakeCurrentEvent read FOnMakeCurrent
                                                       write FOnMakeCurrent;
    property OnPaint: TNotifyEvent read FOnPaint write FOnPaint;
    property SharedArea: TCustomGTKGLAreaControl read FSharedArea write SetSharedArea;
    property Widget: PGtkGLArea read GetWidget;
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
    property SharedArea;
    property ShowHint;
    property Visible;
  end;
  
  
  { TWSGTKGLAreaControl }
  
  TWSGTKGLAreaControl = class(TWSWinControl)
  public
    class function CreateHandle(const AComponent: TComponent;
                                const AParams: TCreateParams): THandle; override;
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
  if FSharingAreas<>nil then begin
    while SharingAreasCount>0 do
      SharingAreas[SharingAreasCount-1].SharedArea:=nil;
    FreeAndNil(FSharingAreas);
  end;
  SharedArea:=nil;
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
  // make sure the widget is realized
  gtk_widget_realize(PGtkWidget(Widget));
  // make current
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

function TCustomGTKGLAreaControl.SharingAreasCount: integer;
begin
  if FSharingAreas=nil then
    Result:=0
  else
    Result:=FSharingAreas.Count;
end;

procedure TCustomGTKGLAreaControl.SetSharedArea(
  const AValue: TCustomGTKGLAreaControl);
begin
  if FSharedArea=AValue then exit;
  if AValue=Self then
    Raise Exception.Create('An area can not be shared by itself.');
  // unshare old
  if (AValue<>nil) and (AValue.SharedArea<>nil) then
    Raise Exception.Create('Target area is sharing too. A sharing area can not be shared.');
  if FSharedArea<>nil then FSharedArea.FSharingAreas.Remove(Self);
  // share new
  if (AValue<>nil) and (csDestroying in AValue.ComponentState) then
    FSharedArea:=nil
  else begin
    FSharedArea:=AValue;
    if (FSharedArea<>nil) then begin
      if FSharedArea.FSharingAreas=nil then
        FSharedArea.FSharingAreas:=TList.Create;
      FSharedArea.FSharingAreas.Add(Self);
    end;
  end;
  // recreate handle if needed
  if HandleAllocated and (not (csDesigning in ComponentState)) then
    ReCreateWnd;
end;

function TCustomGTKGLAreaControl.GetSharingAreas(Index: integer
  ): TCustomGTKGLAreaControl;
begin
  Result:=TCustomGTKGLAreaControl(FSharingAreas[Index]);
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

{ TWSGTKGLAreaControl }

function TWSGTKGLAreaControl.CreateHandle(const AComponent: TComponent;
  const AParams: TCreateParams): THandle;
var
  NewWidget: Pointer;
  Area: TCustomGTKGLAreaControl;
begin
  if csDesigning in AComponent.ComponentState then
    Result:=inherited CreateHandle(AComponent,AParams)
  else begin
    Area:=AComponent as TCustomGTKGLAreaControl;
    if (Area.SharedArea<>nil) and (not (csDestroying in Area.ComponentState))
    then
      NewWidget:=gtk_gl_area_share_new(Plongint(@InitAttrList),
                                       Area.SharedArea.Widget)
    else
      NewWidget:=gtk_gl_area_new(Plongint(@InitAttrList));
    Result:=longint(NewWidget);
    TGTKWidgetSet(InterfaceObject).FinishComponentCreate(AComponent,NewWidget,true);
  end;
end;

initialization
  {$i gtkglarea.lrs}
  GtkGLAreaControlStack:=nil;
  RegisterWSComponent(TCustomGTKGLAreaControl,TWSGTKGLAreaControl);

finalization
  FreeAndNil(GtkGLAreaControlStack);

end.
