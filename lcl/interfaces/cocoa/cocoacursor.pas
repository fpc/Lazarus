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
    _windowActivating: Boolean;
    _lastCursor: NSCursor;
  public
    procedure setWindowActivating;
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
  lpPoint:= TCocoaScreenUtil.toLCL( NSEvent.mouseLocation );
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
  // the cursor may be changed by the macOS, for example, it may be
  // automatically changed to IBeam when moved into an Edit
  notChangedBySystem: Boolean;
begin
  currentCursor:= NSCursor.currentCursor;

  // 1. when the Form initially becomes the keyWindow, it will
  //    set _windowActivating to True via TCursorHelper.setWindowActivating().
  // 2. the purpose of checking _windowActivating here is to prevent the APP
  //    from setting the Cursor again after TCursorHelper.setWindowActivating().
  // 3. one example is Lazaurs IDE, which checks for resource updates and
  //    reloads when it gains focus, at which point it calls
  //    Screen.BeginWaitCursor() to set the cursor.
  // 4. if it happens between TCursorHelper.setWindowActivating() and
  //    here (by TCursorHelper.SetCursorOnActive()), and this check is missing,
  //    Screen.EndWaitCursor() will fail to restore the cursor
  //    due to the incorrect notChangedBySystem.
  if _windowActivating then begin
    notChangedBySystem:= (currentCursor=NSCursor.arrowCursor);
  end else begin
    notChangedBySystem:= (_lastCursor=nil) or (currentCursor=NSCursor.arrowCursor) or (currentCursor=_lastCursor);
  end;

  if notChangedBySystem then begin
    newCursor.SetCursor;
    _lastCursor:= newCursor.Cursor;
  end;
  _windowActivating:= False;
end;

procedure TCursorHelper.ForceSetDefaultCursor;
var
  newCursor: TCocoaCursor;
begin
  newCursor:= TCocoaCursor(Screen.Cursors[crDefault]);
  newCursor.SetCursor;
  _lastCursor:= newCursor.Cursor;
  _windowActivating:= False;
end;

procedure TCursorHelper.SetCursorOnActive;
begin
  if Screen.Cursor<>crDefault then begin
    SetScreenCursor;
  end else begin
    SetCursorAtMousePos;
  end;
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
  _windowActivating:= False;
  TCocoaCursor(Screen.Cursors[Screen.Cursor]).SetCursor;
end;

procedure TCursorHelper.SetScreenCursorWhenNotDefault;
begin
  if Screen.Cursor<>crDefault then
    SetScreenCursor;
end;

procedure TCursorHelper.setWindowActivating;
begin
  _windowActivating:= True;
end;

initialization
  CursorHelper:= TCursorHelper.Create;

finalization
  FreeAndNil(CursorHelper);

end.

