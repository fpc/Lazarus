{
 /***************************************************************************
                    CocoaInt.pas  -  CocoaInterface Object
                    ----------------------------------------

                 Initial Revision  : Mon August 6th CST 2004


 ***************************************************************************/

 *****************************************************************************
 *                                                                           *
 *  This file is part of the Lazarus Component Library (LCL)                 *
 *                                                                           *
 *  See the file COPYING.modifiedLGPL.txt, included in this distribution,    *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************
 }

unit CocoaInt;

{$mode objfpc}{$H+}
{$modeswitch objectivec1}

interface

uses
  // rtl+ftl
  Types, Classes, SysUtils, Math,
  // carbon bindings
  MacOSAll,
  // interfacebase
  InterfaceBase, GraphType,
  // private
  CocoaAll, CocoaPrivate, CocoaUtils, CocoaGDIObjects, CocoaTextLayout,
  // LCL
  LCLStrConsts, LMessages, LCLMessageGlue, LCLProc, LCLIntf, LCLType,
  CocoaWSFactory;

type

  { TCocoaTimerObject }

  TCocoaTimerObject=objcclass(NSObject)
    func : TWSTimerProc;
    procedure timerEvent; message 'timerEvent';
    class function initWithFunc(afunc: TWSTimerProc): TCocoaTimerObject; message 'initWithFunc:';
  end;

  { TCocoaAppDelegate }

  TCocoaAppDelegate = objcclass(NSObject)
    function applicationShouldTerminate(sender: NSApplication): NSApplicationTerminateReply; message 'applicationShouldTerminate:';
  end;

  { TCocoaWidgetSet }

  TCocoaWidgetSet = class(TWidgetSet)
  private
    FTerminating: Boolean;

    pool      : NSAutoreleasePool;
    NSApp     : NSApplication;
    delegate  : TCocoaAppDelegate;

  public
    constructor Create; override;
    destructor Destroy; override;

    function LCLPlatform: TLCLPlatform; override;

    procedure AppInit(var ScreenInfo: TScreenInfo); override;
    procedure AppRun(const ALoop: TApplicationMainLoop); override;
    procedure AppWaitMessage; override;
    procedure AppProcessMessages; override;
    procedure AppTerminate; override;
    procedure AppMinimize; override;
    procedure AppRestore; override;
    procedure AppBringToFront; override;
    procedure AppSetTitle(const ATitle: string); override;

    function CreateTimer(Interval: integer; TimerFunc: TWSTimerProc): THandle; override;
    function DestroyTimer(TimerHandle: THandle): boolean; override;
    function AppHandle: THandle; override;

    {todo:}
    function  DCGetPixel(CanvasHandle: HDC; X, Y: integer): TGraphicsColor; override;
    procedure DCSetPixel(CanvasHandle: HDC; X, Y: integer; AColor: TGraphicsColor); override;
    procedure DCRedraw(CanvasHandle: HDC); override;
    procedure DCSetAntialiasing(CanvasHandle: HDC; AEnabled: Boolean); override;
    procedure SetDesigning(AComponent: TComponent); override;

    // the winapi compatibility methods
    {$I cocoawinapih.inc}
    // the extra LCL interface methods
//    {$I carbonlclintfh.inc}
  end;
  
var
  CocoaWidgetSet: TCocoaWidgetSet;

implementation

var
  ScreenContext : TCocoaContext = nil;

//todo: a better check!

function CheckDC(dc: HDC): TCocoaContext;
begin
  Result:=TCocoaContext(dc);
end;

function CheckGDIOBJ(obj: HGDIOBJ): TCocoaGDIObject;
begin
  Result:=TCocoaGDIObject(obj);
end;


// the implementation of the winapi compatibility methods
{$I cocoawinapi.inc}
// the implementation of the extra LCL interface methods
//{$I Cocoalclintf.inc}


{ TCocoaWidgetSet }

{------------------------------------------------------------------------------
  Method:  TCocoaWidgetSet.AppInit
  Params:  ScreenInfo

  Initialize Carbon Widget Set
 ------------------------------------------------------------------------------}
procedure TCocoaWidgetSet.AppInit(var ScreenInfo: TScreenInfo);
begin
  {$IFDEF VerboseObject}
    DebugLn('TCocoaWidgetSet.AppInit');
  {$ENDIF}

  delegate:=TCocoaAppDelegate.alloc;

  { Creates the application NSApp object }
  NsApp := NSApplication.sharedApplication;
  NSApp.setDelegate(delegate);
end;

{------------------------------------------------------------------------------
  Method:  TCocoaWidgetSet.AppRun
  Params:  ALoop
 ------------------------------------------------------------------------------}
procedure TCocoaWidgetSet.AppRun(const ALoop: TApplicationMainLoop);
begin
  {$IFDEF VerboseObject}
    DebugLn('TCocoaWidgetSet.AppRun');
  {$ENDIF}

  { Enters main message loop }
  NSApp.run;
end;

{------------------------------------------------------------------------------
  Method:  TCocoaWidgetSet.AppProcessMessages

  Handle all pending messages
 ------------------------------------------------------------------------------}
procedure TCocoaWidgetSet.AppProcessMessages;
var
  event : NSEvent;
begin
  {$IFDEF VerboseObject}
    DebugLn('TCocoaWidgetSet.AppProcessMessages');
  {$ENDIF}

  event:=NSApp.nextEventMatchingMask_untilDate_inMode_dequeue(NSAnyEventMask, nil, NSDefaultRunLoopMode, true);
  NSApp.sendEvent(event);

  {$IFDEF VerboseObject}
    DebugLn('TCocoaWidgetSet.AppProcessMessages END');
  {$ENDIF}
end;

{------------------------------------------------------------------------------
  Method:  TCocoaWidgetSet.AppWaitMessage

  Passes execution control to Cocoa
 ------------------------------------------------------------------------------}
procedure TCocoaWidgetSet.AppWaitMessage;
var
  event : NSEvent;
begin
  {$IFDEF VerboseObject}
    DebugLn('TCocoaWidgetSet.AppWaitMessage');
  {$ENDIF}
  event:=NSApp.nextEventMatchingMask_untilDate_inMode_dequeue(NSAnyEventMask, NSDate.distantFuture, NSDefaultRunLoopMode, true);
  NSApp.sendEvent(event);
end;

{------------------------------------------------------------------------------
  Method:  TCocoaWidgetSet.Create

  Constructor for the class
 ------------------------------------------------------------------------------}
constructor TCocoaWidgetSet.Create;
begin
  CocoaWidgetSet := Self;
  inherited Create;
  FTerminating := False;

  {  Creates the AutoreleasePool }
  pool := NSAutoreleasePool(NSAutoreleasePool.alloc).init;
end;

{------------------------------------------------------------------------------
  Method:  TCocoaWidgetSet.Destroy

  Destructor for the class
 ------------------------------------------------------------------------------}
destructor TCocoaWidgetSet.Destroy;
begin
  inherited Destroy;
  CocoaWidgetSet := nil;

  {  Releases the AutoreleasePool }
  pool.release;
end;

{------------------------------------------------------------------------------
  Method:  TCocoaWidgetSet.AppTerminate

  Tells Carbon to halt the application
 ------------------------------------------------------------------------------}
procedure TCocoaWidgetSet.AppTerminate;
begin
  if FTerminating then Exit;
  {$IFDEF VerboseObject}
    DebugLn('TCocoaWidgetSet.AppTerminate');
  {$ENDIF}

  NSApp.terminate(nil);
end;

{------------------------------------------------------------------------------
  Method:  TCocoaWidgetSet.AppMinimize

  Minimizes the whole application to the taskbar
 ------------------------------------------------------------------------------}
procedure TCocoaWidgetSet.AppMinimize;
begin
  {$IFDEF VerboseObject}
    DebugLn('TCocoaWidgetSet.AppMinimize');
  {$ENDIF}
  NSApp.miniaturizeAll(nil);
end;

{------------------------------------------------------------------------------
  Method:  TCocoaWidgetSet.AppRestore

  Restores the whole minimized application from the taskbar
 ------------------------------------------------------------------------------}
procedure TCocoaWidgetSet.AppRestore;
begin
  {$IFDEF VerboseObject}
    DebugLn('TCocoaWidgetSet.AppRestore');
  {$ENDIF}
  NSApp.activateIgnoringOtherApps(False);
end;

{------------------------------------------------------------------------------
  Method:  TCocoaWidgetSet.AppBringToFront

  Brings the entire application on top of all other non-topmost programs
 ------------------------------------------------------------------------------}
procedure TCocoaWidgetSet.AppBringToFront;
begin
  {$IFDEF VerboseObject}
    DebugLn('TCocoaWidgetSet.AppBringToFront');
  {$ENDIF}
  NSApp.activateIgnoringOtherApps(True);
end;

{------------------------------------------------------------------------------
  Method:  TCocoaWidgetSet.AppSetTitle
  Params:  ATitle - New application title

  Changes the application title
 ------------------------------------------------------------------------------}
procedure TCocoaWidgetSet.AppSetTitle(const ATitle: string);
begin
  if not Assigned(NSApp.dockTile) then Exit;
  //todo: setBadgeLabel is for 10.5 only, should be removed
  if NSApp.dockTile.respondsToSelector_(objcselector('setBadgeLabel:')) then
    NSApp.dockTile.setBadgeLabel(NSStringUtf8(ATitle));
end;

function TCocoaWidgetSet.CreateTimer(Interval: integer; TimerFunc: TWSTimerProc): THandle;
var
  timer : NSTimer;
  user  : TCocoaTimerObject;
begin
  {$IFDEF VerboseObject}
    DebugLn('TCocoaWidgetSet.CreateTimer');
  {$ENDIF}
  user:=TCocoaTimerObject.initWithFunc(TimerFunc);

  timer:=NSTimer.timerWithTimeInterval_target_selector_userInfo_repeats(
    Interval/1000, user, objcselector(user.timerEvent), user, True);

  NSRunLoop.currentRunLoop.addTimer_forMode(timer, NSDefaultRunLoopMode);

  {user is retained (twice, because it's target), by the timer and }
  {released (twice) on timer invalidation}
  user.release;

  Result:=THandle(timer);
end;

function TCocoaWidgetSet.DestroyTimer(TimerHandle: THandle): boolean;
var
  obj : NSObject;
begin
  {$IFDEF VerboseObject}
    DebugLn('TCocoaWidgetSet.DestroyTimer');
  {$ENDIF}
  obj:=NSObject(TimerHandle);
  try
    Result:= Assigned(obj) and obj.isKindOfClass_(NSTimer);
  except
    Result:=false;
  end;
  if not Result then Exit;
  NSTimer(obj).invalidate;
end;

{------------------------------------------------------------------------------
  Method:  TCocoaWidgetSet.AppHandle
  Returns: Returns NSApp object, created via NSApplication.sharedApplication
 ------------------------------------------------------------------------------}
function TCocoaWidgetSet.AppHandle: THandle;
begin
  Result:=THandle(NSApp);
end;

function TCocoaWidgetSet.DCGetPixel(CanvasHandle: HDC; X, Y: integer): TGraphicsColor;
begin
  Result:=0;
end;

procedure TCocoaWidgetSet.DCSetPixel(CanvasHandle: HDC; X, Y: integer; AColor: TGraphicsColor);
begin

end;

procedure TCocoaWidgetSet.DCRedraw(CanvasHandle: HDC);
begin

end;

procedure TCocoaWidgetSet.DCSetAntialiasing(CanvasHandle: HDC; AEnabled: Boolean);
begin
  inherited DCSetAntialiasing(CanvasHandle, AEnabled);
end;

procedure TCocoaWidgetSet.SetDesigning(AComponent: TComponent);
begin

end;

{------------------------------------------------------------------------------
  Method:  TCocoaWidgetSet.LCLPlatform
  Returns: lpCocoa - enum value for Cocoa widgetset
 ------------------------------------------------------------------------------}
function TCocoaWidgetSet.LCLPlatform: TLCLPlatform;
begin
  Result:= lpCocoa;
end;

procedure InternalInit;
begin
end;

procedure InternalFinal;
begin
  if Assigned(ScreenContext) then ScreenContext.Free;
end;


{ TCocoaAppDelegate }

function TCocoaAppDelegate.applicationShouldTerminate(sender: NSApplication): NSApplicationTerminateReply;
begin
  Result := NSTerminateNow;
end;

{ TCocoaTimerObject }

procedure TCocoaTimerObject.timerEvent;
begin
  if Assigned(@func) then func;
end;

class function TCocoaTimerObject.initWithFunc(afunc: TWSTimerProc): TCocoaTimerObject;
begin
  Result:=alloc;
  Result.func:=afunc;
end;

initialization
//  {$I Cocoaimages.lrs}
  InternalInit;

finalization
  InternalFinal;

end.
