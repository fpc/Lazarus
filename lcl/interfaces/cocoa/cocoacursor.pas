unit CocoaCursor;

{$mode ObjFPC}{$H+}
{$modeswitch objectivec1}

interface

uses
  Classes, SysUtils,
  Controls, Forms,
  CocoaAll, CocoaUtils, CocoaGDIObjects;

type
  { TCocoaCursor }

  TCocoaCursor = class(TObject)
  strict private
    FStandard: Boolean;
    FBitmap: TCocoaBitmap;
    FCursor: NSCursor;
  public
    constructor CreateStandard(const ACursor: NSCursor);
    constructor CreateFromBitmap(const ABitmap: TCocoaBitmap; const hotSpot: NSPoint);
    constructor CreateFromCustomCursor(const ACursor: NSCursor);
    destructor Destroy; override;
    procedure SetCursor;
    property Cursor: NSCursor read FCursor;
    property Standard: Boolean read FStandard;
  end;

  { TCursorHelper }

  TCursorHelper = class
  private
    _lastCursor: NSCursor;
  public
    procedure SetNewCursor( newCursor:TCocoaCursor );
    procedure ForceSetDefaultCursor;
    procedure SetCursorOnActive;
    procedure SetScreenCursor;
    procedure SetScreenCursorWhenNotDefault;
  public
    class procedure SetCursorAtMousePos;
  end;

function CocoaGetCursorPos(var lpPoint: TPoint ): Boolean;

var
  CursorHelper: TCursorHelper;

implementation

function CocoaGetCursorPos(var lpPoint: TPoint ): Boolean;
begin
  lpPoint:= ScreenPointFromNSToLCL( NSEvent.mouseLocation );
  Result := True;
end;


{ TCocoaCursor }
constructor TCocoaCursor.CreateStandard(const ACursor: NSCursor);
begin
  FBitmap := nil;
  FCursor := ACursor;
  FStandard := True;
end;

constructor TCocoaCursor.CreateFromBitmap(const ABitmap: TCocoaBitmap; const hotSpot: NSPoint);
begin
  FBitmap := ABitmap;            // takes ownership, no ref count change required
  FCursor := NSCursor.alloc.initWithImage_hotSpot(ABitmap.Image, hotSpot);
  FStandard := False;
end;

constructor TCocoaCursor.CreateFromCustomCursor(const ACursor: NSCursor);
begin
  FCursor := ACursor;
  FStandard := False;
end;

destructor TCocoaCursor.Destroy;
begin
  FreeAndNil(FBitmap);
  if not Standard then
    FCursor.release;
  inherited;
end;

procedure TCocoaCursor.SetCursor;
begin
  FCursor.set_;
end;

{ TCursorHelper }

procedure TCursorHelper.SetNewCursor( newCursor:TCocoaCursor );
var
  currentCursor: NSCursor;
begin
  currentCursor:= NSCursor.currentCursor;
  if (_lastCursor=nil) or (currentCursor=NSCursor.arrowCursor) or (currentCursor=_lastCursor) then
  begin
    newCursor.SetCursor;
    _lastCursor:= newCursor.Cursor;
  end;
end;

procedure TCursorHelper.ForceSetDefaultCursor;
var
  newCursor: TCocoaCursor;
begin
  newCursor:= TCocoaCursor(Screen.Cursors[crDefault]);
  newCursor.SetCursor;
  _lastCursor:= newCursor.Cursor;
end;

procedure TCursorHelper.SetCursorOnActive;
begin
  _lastCursor:= NSCursor.arrowCursor;
  if Screen.Cursor<>crDefault then
    SetScreenCursor
  else
    SetCursorAtMousePos;
end;

class procedure TCursorHelper.SetCursorAtMousePos;
var
  P: TPoint;
  rect: NSRect;
  window: NSWindow;
  event: NSEvent;
begin
  window:= NSAPP.keyWindow;
  if not Assigned(window) then
    exit;

  CocoaGetCursorPos(P);

  rect:= NSZeroRect;
  rect.origin:= LCLToNSPoint(P, window.screen.frame.size.height);
  rect:= window.convertRectFromScreen(rect);

  event:= NSEvent.mouseEventWithType_location_modifierFlags_timestamp_windowNumber_context_eventNumber_clickCount_pressure(
            NSMouseMoved,
            rect.origin, 0, 0,
            window.windowNumber, nil, 0, 0, 0);

  NSApp.postEvent_atStart(event, true);
end;

procedure TCursorHelper.SetScreenCursor;
begin
  _lastCursor:= nil;
  TCocoaCursor(Screen.Cursors[Screen.Cursor]).SetCursor;
end;

procedure TCursorHelper.SetScreenCursorWhenNotDefault;
begin
  if Screen.Cursor<>crDefault then
    SetScreenCursor;
end;

initialization
  CursorHelper:= TCursorHelper.Create;

finalization
  FreeAndNil(CursorHelper);

end.

