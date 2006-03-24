{ $Id: winceint.pp 8004 2005-10-30 15:33:20Z micha $ }
{
 /***************************************************************************
                         WINCEINT.pp  -  WinCEInterface Object
                             -------------------



 ***************************************************************************/

 *****************************************************************************
 *                                                                           *
 *  This file is part of the Lazarus Component Library (LCL)                 *
 *                                                                           *
 *  See the file COPYING.LCL, included in this distribution,                 *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************
}

unit WinCEInt;

{$mode objfpc}{$H+}

interface

{$IFDEF Trace}
{$ASSERTIONS ON}
{$ENDIF}

// defining the following will print all messages as they are being handled
// valuable for investigation of message trees / interrelations
{ $define MSG_DEBUG}

{
  When editing this unit list, be sure to keep Windows listed first to ensure
  successful compilation.
}
uses
  Types, Classes, ComCtrls, Controls, Buttons, Dialogs, ExtCtrls, Forms,
  GraphMath, GraphType, InterfaceBase, LCLIntf, LCLType,
  LMessages, StdCtrls, SysUtils, Graphics, Menus, Windows;

type
  { WinCE interface-object class }

  { TWinCEWidgetSet }

  TWinCEWidgetSet = class(TWidgetSet)
  public
    { Constructor of the class }
    constructor Create;
    { Destructor of the class }
    destructor Destroy; override;
    { Initialize the API }
    procedure AppInit(var ScreenInfo: TScreenInfo); override;
    procedure AppMinimize; override;
    procedure AppBringToFront; override;
    procedure DCSetPixel(CanvasHandle: HDC; X, Y: integer; AColor: TGraphicsColor); override;
    function  DCGetPixel(CanvasHandle: HDC; X, Y: integer): TGraphicsColor; override;
    procedure DCRedraw(CanvasHandle: HDC); override;
    procedure SetDesigning(AComponent: TComponent); override;
    procedure AppProcessMessages; override;
    procedure AppWaitMessage; override;
    Procedure AppTerminate; override;
    Function  InitHintFont(HintFont: TObject): Boolean; override;
    Procedure AttachMenuToWindow(AMenuObject: TComponent); override;
    procedure AppRun(const ALoop: TApplicationMainLoop); override;

    // create and destroy
    function CreateComponent(Sender : TObject): THandle; override;
    function CreateTimer(Interval: integer; TimerFunc: TFNTimerProc) : integer; override;
    function DestroyTimer(TimerHandle: Integer) : boolean; override;

    {$I wincewinapih.inc}
    {$I wincelclintfh.inc}
  end;


const
  BOOL_RESULT: Array[Boolean] Of String = ('False', 'True');
  ClsName: array[0..6] of WideChar = ('W','i','n','d','o','w',#0);
  EditClsName: array[0..4] of WideChar = ('E','D','I','T',#0);
  ButtonClsName: array[0..6] of WideChar = ('B','U','T','T','O','N',#0);
//  ComboboxClsName: array[0..8] of WideChar = 'ComboBox'#0;
//  TabControlClsName: array[0..15] of WideChar = 'SysTabControl32'#0;

  CP_UTF7                  = 65000;         { UTF-7 translation }
  CP_UTF8                  = 65001;         { UTF-8 translation }

{ export for widgetset implementation }

function WindowProc(Window: HWnd; Msg: UInt; WParam: Windows.WParam;
    LParam: Windows.LParam): LResult; stdcall;

var
  WinCEWidgetSet: TWinCEWidgetSet;

implementation

uses
////////////////////////////////////////////////////
// I M P O R T A N T
////////////////////////////////////////////////////
// To get as little as possible circles,
// uncomment only those units with implementation
////////////////////////////////////////////////////
// WinCEWSActnList,
// WinCEWSArrow,
 WinCEWSButtons,
// WinCEWSCalendar,
// WinCEWSCheckLst,
// WinCEWSCListBox,
// WinCEWSComCtrls,
// WinCEWSControls,
// WinCEWSDbCtrls,
// WinCEWSDBGrids,
// WinCEWSDialogs,
// WinCEWSDirSel,
// WinCEWSEditBtn,
// WinCEWSExtCtrls,
// WinCEWSExtDlgs,
// WinCEWSFileCtrl,
 WinCEWSForms,
// WinCEWSGrids,
// WinCEWSImgList,
// WinCEWSMaskEdit,
// WinCEWSMenus,
// WinCEWSPairSplitter,
// WinCEWSSpin,
 WinCEWSStdCtrls,
// WinCEWSToolwin,
////////////////////////////////////////////////////
  LCLProc;

{$I wincecallback.inc}
{$I winceobject.inc}
{$I wincewinapi.inc}
{$I wincelclintf.inc}

initialization
  Assert(False, 'Trace:WinCEint.pp - Initialization');

finalization
  Assert(False, 'Trace:WinCEint.pp - Finalization');

end.
