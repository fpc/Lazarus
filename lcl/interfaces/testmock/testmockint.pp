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
  Classes, SysUtils, Types, Math,
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
begin
  //
end;


end.
