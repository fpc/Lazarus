{%MainUnit cocoaint.pas}

{ $Id: carbonobject.inc 15152 2008-05-15 10:15:06Z sekelsenmat $ }
{******************************************************************************
  All utility method implementations of the TCocoaWidgetSet class are here.


 ******************************************************************************
 Implementation
 ******************************************************************************

 *****************************************************************************
 *                                                                           *
 *  This file is part of the Lazarus Component Library (LCL)                 *
 *                                                                           *
 *  See the file COPYING.modifiedLGPL.txt, included in this distribution,        *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************
}

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

  { Creates the application NSApp object }
  NSApp := NSApplication.sharedApplication;
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

//  NSApp.setDelegate(myController);

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
  pool := NSAutoreleasePool.Create;
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
  pool.Free;
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


