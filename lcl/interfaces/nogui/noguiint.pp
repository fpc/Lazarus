{
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

unit NoGUIInt;

{$mode objfpc}{$H+}

interface

{$ifdef Trace}
{$ASSERTIONS ON}
{$endif}

uses
  {$IFDEF TraceGdiCalls}
  LineInfo,
  {$ENDIF}
  // rtl+fcl
  Types, Classes, SysUtils, FPCAdds,
  // interfacebase
  InterfaceBase,
  // LCL
  Dialogs, Controls, Forms, LCLStrConsts, LMessages,
  LCLProc, LCLIntf, LCLType, GraphType, Graphics, Menus, Themes,
  // widgetset
  WSLCLClasses;

type

  { TNoGUIWidgetSet }

  TNoGUIWidgetSet = class(TWidgetSet)
  protected
  public
    procedure PassCmdLineOptions; override;
  public
    function LCLPlatform: TLCLPlatform; override;
    // Application
    procedure AppInit(var ScreenInfo: TScreenInfo); override;
    procedure AppProcessMessages; override;
    procedure AppWaitMessage; override;
    procedure AppTerminate; override;
    procedure AppMinimize; override;
    procedure AppRestore; override;
    procedure AppBringToFront; override;
    procedure AppSetTitle(const ATitle: string); override;
    function EnumFontFamiliesEx(DC: HDC; lpLogFont: PLogFont; Callback: FontEnumExProc; Lparam: LParam; Flags: dword): longint; override;
  public
    constructor Create; override;
    destructor Destroy; override;

    // create and destroy
    function CreateTimer(Interval: integer; TimerFunc: TWSTimerProc) : THandle; override;
    function DestroyTimer(TimerHandle: THandle) : boolean; override;
    procedure DestroyLCLComponent(Sender: TObject);virtual;
  public
  end;

var
  NoGUIWidgetSet: TNoGUIWidgetSet;

implementation

uses
  NoGUIWSFactory;

{ TNoGUIWidgetSet }

procedure TNoGUIWidgetSet.PassCmdLineOptions;
begin
  inherited PassCmdLineOptions;
end;

function TNoGUIWidgetSet.LCLPlatform: TLCLPlatform;
begin
  Result:=lpNoGUI;
end;

procedure TNoGUIWidgetSet.AppInit(var ScreenInfo: TScreenInfo);
begin

end;

procedure TNoGUIWidgetSet.AppProcessMessages;
begin

end;

procedure TNoGUIWidgetSet.AppWaitMessage;
begin

end;

procedure TNoGUIWidgetSet.AppTerminate;
begin

end;

procedure TNoGUIWidgetSet.AppMinimize;
begin

end;

procedure TNoGUIWidgetSet.AppRestore;
begin

end;

procedure TNoGUIWidgetSet.AppBringToFront;
begin

end;

procedure TNoGUIWidgetSet.AppSetTitle(const ATitle: string);
begin

end;

function TNoGUIWidgetSet.EnumFontFamiliesEx(DC: HDC; lpLogFont: PLogFont;
  Callback: FontEnumExProc; Lparam: LParam; Flags: dword): longint;
begin
  Result:=0;
end;

constructor TNoGUIWidgetSet.Create;
begin
  inherited Create;
end;

destructor TNoGUIWidgetSet.Destroy;
begin
  inherited Destroy;
end;

function TNoGUIWidgetSet.CreateTimer(Interval: integer; TimerFunc: TWSTimerProc
  ): THandle;
begin
  Result:=0;
end;

function TNoGUIWidgetSet.DestroyTimer(TimerHandle: THandle): boolean;
begin
  Result:=false;
end;

procedure TNoGUIWidgetSet.DestroyLCLComponent(Sender: TObject);
begin

end;

end.
