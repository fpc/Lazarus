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
  InterfaceBase,
  // private
  CocoaAll, CocoaPrivate,
  // LCL
  LCLStrConsts, LMessages, LCLMessageGlue, LCLProc, LCLIntf, LCLType,
  CocoaWSFactory;

type
  { TAppDelegate }

  TNSAppDelegate = objcclass(NSObject)
    function applicationShouldTerminate(sender: NSApplication): NSApplicationTerminateReply; message 'applicationShouldTerminate:';
  end;

  { TCocoaWidgetSet }

  TCocoaWidgetSet = class(TWidgetSet)
  private
    // Set when the QuitEventHandler terminates
    FTerminating: Boolean;
{    FMainEventQueue: EventQueueRef;
    FTimerMap: TMap; // the map contains all installed timers
    FCurrentCursor: HCURSOR;
    FMainMenu: TMainMenu; // Main menu attached to menu bar
    FCaptureWidget: HWND; // Captured widget (TCarbonWidget descendant)
    FOpenEventHandlerUPP: AEEventHandlerUPP;
    FQuitEventHandlerUPP: AEEventHandlerUPP;}

    pool      : NSAutoreleasePool;
    NSApp     : NSApplication;
    delegate  : TNSAppDelegate;

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

    // the winapi compatibility methods
    {$I cocoawinapih.inc}
    // the extra LCL interface methods
//    {$I carbonlclintfh.inc}
  end;
  
var
  CocoaWidgetSet: TCocoaWidgetSet;

implementation

// the implementation of the winapi compatibility methods
{$I cocoawinapi.inc}
// the implementation of the extra LCL interface methods
//{$I Cocoalclintf.inc}


{ TNSAppDelegate }

function TNSAppDelegate.applicationShouldTerminate(sender: NSApplication): NSApplicationTerminateReply;
begin
  Result := NSTerminateNow;
end;


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

  delegate:=TNSAppDelegate.alloc;

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
begin
  {$IFDEF VerboseObject}
    DebugLn('TCocoaWidgetSet.AppProcessMessages');
  {$ENDIF}


  {$IFDEF VerboseObject}
    DebugLn('TCocoaWidgetSet.AppProcessMessages END');
  {$ENDIF}
end;

{------------------------------------------------------------------------------
  Method:  TCocoaWidgetSet.AppWaitMessage

  Passes execution control to Carbon
 ------------------------------------------------------------------------------}
procedure TCocoaWidgetSet.AppWaitMessage;
begin
  {$IFDEF VerboseObject}
    DebugLn('TCocoaWidgetSet.AppWaitMessage');
  {$ENDIF}

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

end;

{------------------------------------------------------------------------------
  Method:  TCocoaWidgetSet.AppSetTitle
  Params:  ATitle - New application title

  Changes the application title
 ------------------------------------------------------------------------------}
procedure TCocoaWidgetSet.AppSetTitle(const ATitle: string);
begin
  // TODO
end;

{------------------------------------------------------------------------------
  Method:  TCocoaWidgetSet.LCLPlatform
  Returns: lpCarbon - enum value for Carbon widgetset
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

end;


initialization
//  {$I Cocoaimages.lrs}
  InternalInit;

finalization
  InternalFinal;

end.
