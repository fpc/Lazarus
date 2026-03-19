{ $Id$ }
{
 /***************************************************************************
                         TestMockINT.pp  -  Test Mock Interface Object
                             -------------------



 ***************************************************************************/

 *****************************************************************************
  This file is part of the Lazarus Component Library (LCL)

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************
}

unit TestMockInt;

{$mode objfpc}{$H+}{$T-}
{$OPTIMIZATION NOREMOVEEMPTYPROCS}

(* MOCKINT_TEST_COMPILE_WITH_BASE
   Compile against the list of functions in the includes used by the parent TWidgetSet.
   Checks if all methods are mocked
*)
{ $define MOCKINT_TEST_COMPILE_WITH_BASE}

(* MOCKINT_TEST_ASSERT_NO_BYPASS_TO_BASE
   assert false for all methods that have not been implemented in the mock WS
   Implement can either be:
   - having dedicated mock code
   - deliberately forwarded to the base class
*)
{ $define MOCKINT_TEST_ASSERT_NO_BYPASS_TO_BASE}

interface

uses
  Classes, SysUtils, Types, Math, fgl,
  InterfaceBase, LCLType, Controls, Forms, GraphType,
  LazUTF8, LazLoggerBase,
  TestMockMiscClasses, TestMockMessages;

type

  { TTestMockWidgetSet }

  TTestMockWidgetSet = class(TWidgetSet)
  private
  public
    procedure AppInit(var ScreenInfo: TScreenInfo); override;
    procedure AppProcessMessages; override;

    function CreateTimer(Interval: integer; TimerProc: TWSTimerProc): TLCLHandle; override;
    function DestroyTimer(TimerHandle: TLCLHandle): boolean; override;

//    function EnumDisplayMonitors(hdc: HDC; lprcClip: PRect; lpfnEnum: MonitorEnumProc; dwData: LPARAM): LongBool; override;
//    //function MonitorFromPoint(ptScreenCoords: TPoint; dwFlags: DWord): HMONITOR; override;
//    function MonitorFromRect(lprcScreenCoords: PRect; dwFlags: DWord): HMONITOR; override;
//    //function MonitorFromWindow(hWnd: HWND; dwFlags: DWord): HMONITOR; override;
//
//    function GetTextMetrics(DC: HDC; var TM: TTextMetric): Boolean; override;

    {$IFDEF MOCKINT_TEST_COMPILE_WITH_BASE}
      {$DEFINE IF_BASE_MEMBER}
      {$Macro on}
      {$define virtual := override}
      {$I ../../include/winapih.inc}
      {$I ../../include/lclintfh.inc}
      {$Macro off}
      {$UNDEF IF_BASE_MEMBER}
    {$ELSE}
      {$I mock_winapih.inc}
      {$I mock_lclintfh.inc}
    {$ENDIF}

  end;


var
  TestMockWidgetSet: TTestMockWidgetSet absolute WidgetSet;

implementation

uses
  TestMockWSFactory;

type
  TMockCreatedTimer = class
    Interval: Integer;
    Proc: TWSTimerProc;
  end;
  TMockCreatedTimerList = specialize TFPGList<TMockCreatedTimer>;
var
  MockCreatedTimerList: TMockCreatedTimerList = nil;

{ TTestMockWidgetSet }

{$I mock_winapi.inc}
{$I mock_lclintf.inc}

procedure TTestMockWidgetSet.AppInit(var ScreenInfo: TScreenInfo);
begin
  ScreenInfo.PixelsPerInchX := 96;
  ScreenInfo.PixelsPerInchY := 96;
  ScreenInfo.ColorDepth := 24;
end;

procedure TTestMockWidgetSet.AppProcessMessages;
var
  i: Integer;
begin
  if MockCreatedTimerList <> nil then begin
    // TODO: sort by interval
    i := MockCreatedTimerList.Count - 1;
    while i >= 0 do begin
      MockCreatedTimerList[i].Proc();
      dec(i);
      if i >= MockCreatedTimerList.Count then
        i := MockCreatedTimerList.Count - 1;
    end;
  end;
end;

function TTestMockWidgetSet.CreateTimer(Interval: integer; TimerProc: TWSTimerProc): TLCLHandle;
var
  t: TMockCreatedTimer;
begin
  if MockCreatedTimerList = nil then
    MockCreatedTimerList := TMockCreatedTimerList.Create;
  t := TMockCreatedTimer.Create;
  t.Interval := Interval;
  t.Proc := TimerProc;
  MockCreatedTimerList.Add(t);
  Result := TLCLHandle(pointer(t));
end;

function TTestMockWidgetSet.DestroyTimer(TimerHandle: TLCLHandle): boolean;
begin
  if MockCreatedTimerList.IndexOf(TMockCreatedTimer(pointer(TimerHandle))) < 0 then
    raise Exception.Create('invalid timer handle');

  MockCreatedTimerList.Remove(TMockCreatedTimer(pointer(TimerHandle)));
  TMockCreatedTimer(pointer(TimerHandle)).Destroy;
end;


finalization
  MockCreatedTimerList.Free;
end.
