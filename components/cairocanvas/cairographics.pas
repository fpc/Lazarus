unit CairoGraphics;

{$mode objfpc}{$H+}

interface

uses
  Types, CairoCanvas, Cairo, LCLType, LCLIntf, LMessages,
  Classes, Controls, ExtCtrls, Graphics;

type
  { TCairoGraphicControl }

  TCairoGraphicControl = class(TControl)
  private
    FCanvas: TCanvas;
    FOnPaint: TNotifyEvent;
    procedure WMPaint(var Message: TLMPaint); message LM_PAINT;
  protected
    //class procedure WSRegisterClass; override;
    procedure FontChanged(Sender: TObject); override;
    procedure Paint; virtual;
    procedure DoOnChangeBounds; override;
    procedure DoOnParentHandleDestruction; override;

    property OnPaint: TNotifyEvent read FOnPaint write FOnPaint;
    function GetDCHandle: HDC;
    procedure ReleaseDCHandle(DC: HDC);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Canvas: TCanvas read FCanvas;
  end;

  { TCairoPaintBox }

  TCairoPaintBox = class(TCairoGraphicControl)
  protected
    //class procedure WSRegisterClass; override;
    procedure Paint; override;
    class function GetControlClassDefaultSize: TSize; override;
  public
    constructor Create(AOwner: TComponent); override;
    property Canvas;
  published
    property Align;
    property Anchors;
    property BorderSpacing;
    property Color;
    property Constraints;
    property DragCursor;
    property DragMode;
    property Enabled;
    property Font;
    property Hint;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property Visible;
    property OnChangeBounds;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnPaint;
    property OnResize;
    property OnStartDrag;
  end;

  { TCairoControlCanvas }

  TCairoControlCanvas = class(TCairoPrinterCanvas)
  private
    procedure SetControl(const AValue: TCairoGraphicControl);
  protected
    sf: Pcairo_surface_t;
    FControl: TCairoGraphicControl;
    FDeviceContext: HDC;
    procedure DestroyCairoHandle; override;
  public
    procedure CreateHandle; override;
    procedure FreeHandle; override;
    property Control: TCairoGraphicControl read FControl write SetControl;
    destructor Destroy; override;
  end;

  TCairoControlCanvasClass = class of TCairoControlCanvas;

var
  CairoGraphicControlCanvasClass: TCairoControlCanvasClass = nil;

implementation

uses
    SysUtils;

{ TCairoGraphicControl }

procedure TCairoGraphicControl.WMPaint(var Message: TLMPaint);
begin
  if Message.DC <> 0 then
  begin
    Canvas.Lock;
    try
      Canvas.Handle := Message.DC;
      try
        Paint;
      finally
        Canvas.Handle := 0;
      end;
    finally
      Canvas.Unlock;
    end;
  end;
end;

{class procedure TCairoGraphicControl.WSRegisterClass;
begin
  inherited WSRegisterClass;
end;}

procedure TCairoGraphicControl.FontChanged(Sender: TObject);
begin
  Canvas.Font:=Font;
  inherited FontChanged(Sender);
end;

procedure TCairoGraphicControl.Paint;
begin
  if Assigned(FOnPaint) then FOnPaint(Self);
end;

procedure TCairoGraphicControl.DoOnChangeBounds;
begin
  inherited DoOnChangeBounds;
  // reset canvas handle in next access
  if Canvas.HandleAllocated then
    TCairoControlCanvas(Canvas).FreeHandle;
end;

procedure TCairoGraphicControl.DoOnParentHandleDestruction;
begin
  inherited DoOnParentHandleDestruction;
  if Canvas.HandleAllocated then
    TCairoControlCanvas(Canvas).FreeHandle;
end;

function TCairoGraphicControl.GetDCHandle: HDC;
begin
  Result := GetDC(Parent.Handle);
  MoveWindowOrgEx(Result, Left, Top);
  IntersectClipRect(Result, 0, 0, Width, Height);
end;

procedure TCairoGraphicControl.ReleaseDCHandle(DC: HDC);
begin
  ReleaseDC(Parent.Handle, DC);
end;

constructor TCairoGraphicControl.Create(AOwner: TComponent);
begin
  FCanvas := CairoGraphicControlCanvasClass.Create;
  TCairoControlCanvas(FCanvas).FControl := Self;
  inherited Create(AOwner);
end;

destructor TCairoGraphicControl.Destroy;
begin
  FreeAndNil(FCanvas);
  inherited Destroy;
end;

{ TCairoControlCanvas }

procedure TCairoControlCanvas.SetControl(const AValue: TCairoGraphicControl);
begin
  if FControl <> AValue then begin
    FreeHandle;
    FControl := AValue;
  end;
end;

procedure TCairoControlCanvas.DestroyCairoHandle;
begin
  cairo_surface_destroy(sf);
  sf := nil;
  inherited DestroyCairoHandle;
end;

procedure TCairoControlCanvas.CreateHandle;
begin
  inherited CreateHandle;
  if FDeviceContext = 0 then //Store it locally, what was Geted must be Released
    FDeviceContext := FControl.GetDCHandle;
  SetHandle(FDeviceContext);
  SurfaceXDPI := GetDeviceCaps(FDeviceContext, LOGPIXELSX);
  SurfaceYDPI := GetDeviceCaps(FDeviceContext, LOGPIXELSY);
  XDPI := SurfaceXDPI;
  YDPI := SurfaceXDPI;
end;

procedure TCairoControlCanvas.FreeHandle;
begin
  if FDeviceContext <> 0 then begin
    FControl.ReleaseDCHandle(FDeviceContext);
    FDeviceContext := 0;
  end;
  inherited FreeHandle;
end;

destructor TCairoControlCanvas.Destroy;
begin
  FreeHandle;
  inherited Destroy;
end;

{ TCairoPaintBox }

procedure TCairoPaintBox.Paint;
begin
  if csDesigning in ComponentState then begin
    Canvas.Brush.Color := Color;
    with Canvas do
    begin
      Pen.Style := psDash;
      Pen.Color:=clBlack;
      Brush.Style := bsClear;
      Rectangle(0, 0, Self.Width - 1, Self.Height - 1);
      Line(0,0,Self.Width-1,Self.Height-1);
      Line(Self.Width-1,0,0,Self.Height-1);
    end;
    exit;
  end;
  if Assigned(OnPaint) then begin
    Canvas.Font := Font;
    Canvas.Brush.Color := Color;
    inherited Paint;
  end;
end;

class function TCairoPaintBox.GetControlClassDefaultSize: TSize;
begin
  Result.Cx:=105;
  Result.Cy:=105;
end;

constructor TCairoPaintBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csReplicatable];
  SetInitialBounds(0,0,GetControlClassDefaultSize.Cx,GetControlClassDefaultSize.Cx);
end;

end.
