unit customdrawn_x11proc;

{$mode objfpc}{$H+}

{$I customdrawndefines.inc}

interface

uses
  // rtl+ftl
  Types, Classes, SysUtils,
  fpimage, fpcanvas, ctypes,
  X, XLib,
  BaseUnix,Unix,
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
    {$ifdef CD_X11_SmartPaint}
    Valid: Boolean;
    Moved: Boolean;
    {$endif}
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

  { TCDX11TimerThread }

  TCDX11TimerThread = class(TThread)
  private
    rfds: TFDset;
    Timeout: cint;
    retval,ByteRec: integer;
  protected
    procedure Execute; override;
  public
    X11TimerPipeIn,X11TimerPipeOut: Integer; // Pipe to Timer
    MainLoopPipeIn,MainLoopPipeOut: Integer; // Pipe to Main Loop
    constructor Create(CreateSuspended: Boolean; const StackSize: SizeUInt=DefaultStackSize
       );
   end;

var
  X11TimerThread: TCDX11TimerThread;

{$endif}

const
  fpFD_SETSIZE = 1024; // As defined in typesizes.h
  KMsToDateTime = 86400000; // # of milliseconds in a day

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

{ TCDX11TimerThread }

procedure TCDX11TimerThread.Execute;
var
  Answ: array [0..80] of byte;
  Answlen: Integer;
  ANextTime: TDateTime absolute answ;
  NextToExpire,TNow,TDiff: TDateTime;
  HeadTimer: TCDX11Timer;
begin
    retval:= AssignPipe(X11TimerPipeIn,X11TimerPipeOut);
    retval:= AssignPipe(MainLoopPipeIn,MainLoopPipeOut);
    WriteLn('TimerThread: Started!');
    NextToExpire:= Now+10; // Ten days in future - high enough
    Repeat
      TNow := Now;
      if NextToExpire > TNow+7 then // no timers until next week,
        //or List Head just processed
        Timeout:= -1 // wait until the first timer is activated
      else if CDWidgetSet.XTimerListHead = nil then
        Timeout:= -1
      else begin
        // Pick up timer which will expire first
        // We must recalculate each time, because a message in between
        // may have interrupted our timeout.
        HeadTimer := CDWidgetSet.XTimerListHead;
        NextToExpire:= HeadTimer.Expires;
        // Compute how many ms from now
        TDiff:= NextToExpire-Now;
        Timeout:= DateTimeToMilliseconds(Tdiff);
        // if already expired (we're late) handle right now
        if Timeout <=0 then Timeout:= 0;
        end;
      // Wait for a message telling that the timer list has changed,
      // until our current timer (if any) expires
      fpFD_ZERO(rfds);
      fpFD_SET(X11TimerPipeIn,rfds);
      retval:= fpSelect(fpFD_SETSIZE,@rfds,nil,nil,Timeout);
      if (retval <> 0) then begin // We've received a message
         ByteRec := FileRead(X11TimerPipeIn,Answ,sizeof(Answ));
         // Debugln doesn't like to be executed in a thread which isn't the MT
         // and after a number of writes crashes with a DISK FULL error!
         //WriteLn('TimerThread: Got message!');
         if ByteRec >=SizeOf(ANextTime) then begin
           if ANextTime < NextToExpire then NextToExpire:=ANextTime;
           end;
         //WriteLn(Format('TimerThread: Message received - NextTime= %s',[DateTimeToStr(ANextTime)]));
         end
      else begin  // A Timer has expired - Send a message to Main Loop
        // message content is irrelevant. We put Timeout for debug
        ANextTime:= Timeout;
        FileWrite(MainLoopPipeOut,Answ,sizeOf(Timeout));
        // we don't want to send twice a messages for the same timer
        // When timer is processed, the list is updateded and we will receive
        // a new message. So we set NextToExpire to a value larger than any
        // expectable value
        NextToExpire:= TNow+10;
        end;
      until Terminated;
end;

constructor TCDX11TimerThread.Create(CreateSuspended: Boolean;
  const StackSize: SizeUInt);
var
  thisTM: TThreadManager;
begin
  GetThreadManager(thisTM);
  if not Assigned(thisTM.InitManager) then begin
    Raise Exception.Create
    ('You must define UseCThread (-dUseCThreads in Project Options-> Compiler Options) in order to run this program!');
    end;
  inherited Create (CreateSuspended);
  {Priority := 99; // it would be nice to assign priority and policy
  Policy := SCHED_RR;} // but it depends on application rights to do so
  FreeOnTerminate := True;
  // Pipes do not yet exist. Better make it clear
  MainLoopPipeIn:= -1;
  MainLoopPipeOut:= -1;
  X11TimerPipeIn:= -1;
  X11TimerPipeOut:= -1;
end;

{ TCDX11Timer }

constructor TCDX11Timer.create(WSInterval: Integer; WSfunc: TWSTimerProc);
{$ifdef Verbose_CD_X11_Timer}
var
  lTInterval: Integer;
  TDiff,TNow: TDateTime;
{$endif}
begin
{$ifdef TimerUseCThreads}
  if X11TimerThread.Suspended then begin
     X11TimerThread.Suspended:= False; // Activate Timer Thread
     end;
{$endif}
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
  ABuffer: array[0..15] of byte;
  ExpireTime: TDateTime absolute ABuffer;
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
  {$ifdef TimerUseCThreads}
  ExpireTime := Expires; // Copy Expire time to Buffer and send to TimerThread
  FileWrite(X11TimerThread.X11TimerPipeOut,ABuffer,SizeOf(ExpireTime));
  {$endif}
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
var
  TNow: TDateTime;
{$ifdef Verbose_CD_X11_Timer}
  lInterval,lTInterval: Integer;
  TDiff: TDateTime;
{$endif}
begin
  TNow:= Now;
  Expires:= Expires+Interval/KMsToDateTime; // don't leak
  while Expires <= TNow do begin // but if we're late, let's skip some! Bad kludge
    Expires:= Expires+Interval/KMsToDateTime;
    end;
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

