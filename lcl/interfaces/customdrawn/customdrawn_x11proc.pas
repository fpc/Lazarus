unit customdrawn_x11proc;

{$mode objfpc}{$H+}

{$I customdrawndefines.inc}

interface

uses
  // rtl+ftl
  Types, Classes, SysUtils,
  fpimage, fpcanvas, ctypes,
  X, XLib,
  // Custom Drawn Canvas
  IntfGraphics, lazcanvas,
  //
  GraphType, Controls, LCLMessageGlue, WSControls, LCLType, LCLProc,
  customdrawnproc;

type
  TX11WindowInfo = class(TCDForm)
  public
    Window: X.TWindow;
    // Used and valid only during event processing
    XEvent: PXEvent;
    // X11 extra objects
    Attr: XLib.TXWindowAttributes;
    Colormap: TColormap;
    GC: TGC;
    ColorDepth: Byte;
  end;

{$ifdef CD_X11_UseNewTimer}
  TWSTimerProc = procedure of object;

    { TCDX11Timer }
  TCDX11Timer = class (TObject)
    Next: TCDX11Timer;
    Previous: TCDX11Timer;
    Interval: Integer;
    Expires: TDateTime;
    func: TWSTimerProc;
    constructor create (WSInterval: Integer; WSfunc: TWSTimerProc);
    procedure Insert;
    procedure Remove;
    procedure Expired;
    destructor destroy;
    end;

const
  KMsToDateTime = 86400000; // # of milliseconds in a day
{$endif}

function RectToXRect(const ARect: TRect): TXRectangle;
function XRectToRect(const ARect: TXRectangle): TRect;
function XButtonToMouseButton(const XButton: cint; var MouseButton: TMouseButton): Boolean;
function GetXEventName(Event: LongInt): String;

implementation
{$ifdef CD_X11_UseNewTimer}
uses CustomDrawnInt;
{$endif}

function RectToXRect(const ARect: TRect): TXRectangle;
begin
  Result.x      := ARect.Left;
  Result.y      := ARect.Top;
  Result.width  := ARect.Right - ARect.Left;
  Result.height := ARect.Bottom - ARect.Top;
end;

function XRectToRect(const ARect: TXRectangle): TRect;
begin
  Result.Left   := ARect.x;
  Result.Top    := ARect.y;
  Result.Right  := ARect.x + ARect.width;
  Result.Bottom := ARect.y + ARect.height;
end;

{ Returns True if the button is indeed a mouse button
  and False if it's the mouse wheel }
function XButtonToMouseButton(const XButton: cint; var MouseButton: TMouseButton): Boolean;
const
  ButtonTable: array[1..3] of TMouseButton = (mbLeft, mbMiddle, mbRight);
begin
  Result := False;

  if (XButton > 3) or (XButton < 1) then Exit;

  MouseButton := ButtonTable[XButton];

  Result := True;
end;

function GetXEventName(Event: LongInt): String;
const
  EventNames: array[2..34] of String = (
    'KeyPress', 'KeyRelease', 'ButtonPress', 'ButtonRelease', 'MotionNotify',
    'EnterNotify', 'LeaveNotify', 'FocusIn', 'FocusOut', 'KeymapNotify',
    'Expose', 'GraphicsExpose', 'NoExpose', 'VisibilityNotify', 'CreateNotify',
    'DestroyNotify', 'UnmapNotify', 'MapNotify', 'MapRequest', 'ReparentNotify',
    'ConfigureNotify', 'ConfigureRequest', 'GravityNotify', 'ResizeRequest',
    'CirculateNotify', 'CirculateRequest', 'PropertyNotify', 'SelectionClear',
    'SelectionRequest', 'SelectionNotify', 'ColormapNotify', 'ClientMessage',
    'MappingNotify');
begin
  if (Event >= Low(EventNames)) and (Event <= High(EventNames)) then
    Result := EventNames[Event]
  else
    Result := '#' + IntToStr(Event);
end;

{$ifdef CD_X11_UseNewTimer}
{ TCDX11Timer }

constructor TCDX11Timer.create(WSInterval: Integer; WSfunc: TWSTimerProc);
{$ifdef Verbose_CD_X11_Timer}
var
  lTInterval: Integer;
  TDiff,TNow: TDateTime;
{$endif}
begin
  Interval:= WSInterval; // Interval in ms
  Func:= WSfunc; // OnTimeEvent
  Expires:= Now + Interval/KMsToDateTime; //
  {$ifdef Verbose_CD_X11_Timer}
  TNow:= Now;
  TDiff:= Expires - TNow;
  lTInterval:=DateTimeToMilliseconds(Tdiff);
  DebugLn(Format('X11_Timer create: Interval= %d, Calculated=%d',[Interval,lTInterval]));
  {$endif}
  Previous:= Nil;
  Next:= Nil;
end;

procedure TCDX11Timer.Insert;
var
  lTimer,PTimer,NTimer: TCDX11Timer;
begin
  {$ifdef Verbose_CD_X11_Timer}
  DebugLn(Format('TCDX11Timer Insert: Interval := %d',[Interval]));
  {$endif}
  if CDWidgetSet.XTimerListHead = nil then begin// The list is empty
    CDWidgetSet.XTimerListHead:= self;
    Previous:=Nil; // This is the first and only timer
    Next:=Nil;
  end
  else begin
    PTimer:=nil; // previous in list
    NTimer:=nil; // Next in list
    lTimer := CDWidgetSet.XTimerListHead;
    while lTimer.Expires <= Expires do begin
      PTimer := ltimer;
      if not assigned(lTimer.Next) then Break
      else lTimer:= lTimer.Next;
      end;
    if PTimer<>nil then begin //We're not the first one
      Previous := PTimer;
      NTimer := PTimer.Next;
      if Assigned(NTimer) then begin
        Next := NTimer;
        NTimer.Previous := self;
        end
      else Next := Nil;
      PTimer.Next := self;
      end
    else begin // we're in first place. previous first becomes Next
      NTimer := CDWidgetSet.XTimerListHead;
      CDWidgetSet.XTimerListHead := Self;
      NTimer.Previous := Self;
      Next:= NTimer;
      Previous := nil;
      end;
  end;
  {$ifdef Verbose_CD_X11_Timer}
  lTimer := CDWidgetSet.XTimerListHead;
  while lTimer <> Nil do begin
    DebugLn(Format('TCDX11Timer Insert results: Interval := %d',[lTimer.Interval]));
    lTimer:= lTimer.Next;
    end;
  {$endif}
end;

procedure TCDX11Timer.remove;
begin
  {$ifdef Verbose_CD_X11_Timer}
  DebugLn(Format('TCDX11Timer Remove: Interval := %d',[Interval]));
  {$endif}
  if Previous <> Nil then begin
    if Next <> Nil then begin
      Previous.Next := Next;
      Next.Previous := Previous;
    end
    else Previous.Next:= Nil;
  end
  else begin
    CDWidgetSet.XTimerListHead := Next;
    if Next <> nil then begin
      Next.Previous:= Nil;
    end;
  end;
  Previous:= Nil;
  Next := Nil;
end;

procedure TCDX11Timer.Expired;
{$ifdef Verbose_CD_X11_Timer}
var
  lInterval,lTInterval: Integer;
  TDiff,TNow: TDateTime;
{$endif}
begin
  Expires:= Expires+Interval/KMsToDateTime; // don't leak
  {$ifdef Verbose_CD_X11_Timer}
  TNow:= Now;
  TDiff:= Expires - TNow;
  lTInterval:=DateTimeToMilliseconds(Tdiff);
  DebugLn(Format('X11_Timer Expired: Interval= %d, Calculated=%d',[Interval,lTInterval]));
  {$endif}
  Remove; // Remove from list Head
  if func <> nil then
    func(); // Execute OnTimer
  Insert; // And insert again in right place
end;

destructor TCDX11Timer.destroy;
begin
  {$ifdef Verbose_CD_X11_Timer}
  DebugLn(Format('TCDX11Timer Destroy: Interval := %d',[Interval]));
  {$endif}
  remove;
  //Free;
end;

{$endif}

end.

