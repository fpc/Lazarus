unit CocoaSlider;

{$mode objfpc}{$H+}
{$modeswitch objectivec2}
{$include cocoadefines.inc}

interface

uses
  Classes, SysUtils,
  MacOSAll, CocoaAll,
  CocoaPrivate;

type

  { TManualTicks }

  TManualTicks = class(TObject)
  private
    count : integer;
    //todo: keep sorted and do binary search
    ticks : array of Integer;
    draw  : Boolean;
    function AddTick(atick: integer): Boolean;
  end;

  { TCocoaSlider }

  TCocoaSlider = objcclass(NSSlider)
    callback  : ICommonCallback;
    intval    : Integer;
    man       : TManualTicks;

    procedure dealloc; override;
    procedure drawRect(dirtyRect: NSRect); override;

    function acceptsFirstResponder: LCLObjCBoolean; override;
    function lclGetCallback: ICommonCallback; override;
    procedure lclClearCallback; override;
    //
    procedure SnapToInteger(AExtraFactor: Integer = 0); message 'SnapToInteger:';
    procedure sliderAction(sender: id); message 'sliderAction:';
    // mouse
    function acceptsFirstMouse(event: NSEvent): LCLObjCBoolean; override;
    procedure mouseDown(event: NSEvent); override;
    procedure mouseUp(event: NSEvent); override;
    procedure rightMouseDown(event: NSEvent); override;
    procedure rightMouseUp(event: NSEvent); override;
    procedure rightMouseDragged(event: NSEvent); override;
    procedure otherMouseDown(event: NSEvent); override;
    procedure otherMouseUp(event: NSEvent); override;
    procedure otherMouseDragged(event: NSEvent); override;
    procedure mouseDragged(event: NSEvent); override;
    procedure mouseMoved(event: NSEvent); override;
    procedure scrollWheel(event: NSEvent); override;

    procedure lclAddManTick(atick : integer); message 'lclAddManTick:';
    procedure lclSetManTickDraw(adraw: Boolean); message 'lclSetManTickDraw:';
    procedure lclExpectedKeys(var wantTabs, wantArrows, wantReturn, wantAll: Boolean); override;
  end;

  TCocoaSliderCell = objcclass(NSSliderCell)
  end;

implementation

{ TManualTicks }

function TManualTicks.AddTick(atick: integer): Boolean;
var
  i : integer;
begin
  //todo: must be a binary search
  for i:=0 to length(ticks)-1 do
    if ticks[i]=atick then begin
      Result:=false;
      Exit;
    end;

  // adding new tick
  if length(ticks)=count then begin
    if count=0 then SetLength(ticks, 8)
    else SetLength(ticks, count * 2);
  end;
  ticks[count]:=atick;
  inc(count);
  Result := true;
end;

{ TCocoaSlider }

function GetManTicks(slider: TCocoaSlider): TManualTicks;
begin
  if not Assigned(slider.man) then
    slider.man := TManualTicks.Create;
  Result := slider.man;
end;

procedure TCocoaSlider.dealloc;
begin
  man.Free;
  inherited dealloc;
end;

procedure TCocoaSlider.drawRect(dirtyRect: NSRect);
var
  i  : integer;
  nr : NSRect;
  xr : NSRect;
  dr : NSRect;
  nm : integer;
  ctx : NSGraphicsContext;
  pth : NSBezierPath;
begin
  if not Assigned(man) or (not man.draw) then begin
    inherited drawRect(dirtyRect);
    Exit;
  end;

  nm := round(maxValue - minValue);
  if nm = 0 then Exit;
  if numberOfTickMarks < 2 then Exit;

  nr := rectOfTickMarkAtIndex(0);
  xr := rectOfTickMarkAtIndex(1);

  ctx := NSGraphicsContext.currentContext;
  pth := NSBezierPath.bezierPath;
  NSColor.controlShadowColor.setFill;
  dr:=nr;
  dr.origin.y := dr.origin.y + 1;
  dr.size.height := dr.size.height - 1;
  for i := 0 to man.count - 1 do begin
    dr.origin.x := round(nr.origin.x + (xr.origin.x - nr.origin.x) * (man.ticks[i] - minValue) / nm);
    pth.fillRect(dr);
  end;
  inherited drawRect(dirtyRect);
end;

function TCocoaSlider.acceptsFirstResponder: LCLObjCBoolean;
begin
  Result := True;
end;

function TCocoaSlider.lclGetCallback: ICommonCallback;
begin
  Result:=callback;
end;

procedure TCocoaSlider.lclClearCallback;
begin
  callback := nil;
end;

procedure TCocoaSlider.SnapToInteger(AExtraFactor: Integer);
begin
  setIntValue(Round(doubleValue() + AExtraFactor));
end;

procedure TCocoaSlider.sliderAction(sender: id);
var
  newval: Integer;
begin
  SnapToInteger();
  newval := intValue;
  if newval <> intval then begin
    intval := newval;
    // OnChange event
    if callback <> nil then
      callback.SendOnChange();
  end;
end;

function TCocoaSlider.acceptsFirstMouse(event: NSEvent): LCLObjCBoolean;
begin
  Result:=true;
end;

procedure TCocoaSlider.mouseDown(event: NSEvent);
begin
  if not Assigned(callback) or not callback.MouseUpDownEvent(event) then
  begin
    inherited mouseDown(event);

    callback.MouseUpDownEvent(event, true);
  end;
end;

procedure TCocoaSlider.mouseUp(event: NSEvent);
begin
  if not Assigned(callback) or not callback.MouseUpDownEvent(event) then
    inherited mouseUp(event);
end;

procedure TCocoaSlider.rightMouseDown(event: NSEvent);
begin
  if not Assigned(callback) or not callback.MouseUpDownEvent(event) then
    inherited rightMouseDown(event);
end;

procedure TCocoaSlider.rightMouseUp(event: NSEvent);
begin
  if not Assigned(callback) or not callback.MouseUpDownEvent(event) then
    inherited rightMouseUp(event);
end;

procedure TCocoaSlider.rightMouseDragged(event: NSEvent);
begin
  if not Assigned(callback) or not callback.MouseUpDownEvent(event) then
    inherited rightMouseDragged(event);
end;

procedure TCocoaSlider.otherMouseDown(event: NSEvent);
begin
  if not Assigned(callback) or not callback.MouseUpDownEvent(event) then
    inherited otherMouseDown(event);
end;

procedure TCocoaSlider.otherMouseUp(event: NSEvent);
begin
  if not Assigned(callback) or not callback.MouseUpDownEvent(event) then
    inherited otherMouseUp(event);
end;

procedure TCocoaSlider.otherMouseDragged(event: NSEvent);
begin
  if not Assigned(callback) or not callback.MouseMove(event) then
    inherited otherMouseDragged(event);
end;

procedure TCocoaSlider.mouseDragged(event: NSEvent);
begin
  if not Assigned(callback) or not callback.MouseMove(event) then
    inherited mouseDragged(event);
end;

procedure TCocoaSlider.mouseMoved(event: NSEvent);
begin
  if not Assigned(callback) or not callback.MouseMove(event) then
    inherited mouseMoved(event);
end;

procedure TCocoaSlider.scrollWheel(event: NSEvent);
begin
  if not Assigned(callback) or not callback.scrollWheel(event) then
    inherited scrollWheel(event);
end;

procedure TCocoaSlider.lclAddManTick(atick: integer);
var
  mn : TManualTicks;
begin
  mn := GetManTicks(self);
  if mn.AddTick(atick) then
  begin
    if mn.draw then self.setNeedsDisplay_(true);
  end;
end;

procedure TCocoaSlider.lclSetManTickDraw(adraw: Boolean);
var
  mn : TManualTicks;
begin
  mn := GetManTicks(self);
  if mn.draw=adraw then Exit;
  mn.draw:=adraw;
  self.setNeedsDisplay_(true);
end;

procedure TCocoaSlider.lclExpectedKeys(var wantTabs, wantArrows, wantReturn,
  wantAll: Boolean);
begin
  wantTabs := false;
  wantArrows := true;
  wantReturn := false;
  wantAll := false;
end;

end.

