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

Unit WinCEInt;

{$mode objfpc}{$H+}

Interface

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
Uses
  Types, Classes, ComCtrls, Controls, Buttons, Dialogs, ExtCtrls, Forms,
  GraphMath, GraphType, InterfaceBase, LCLIntf, LCLType,
  LMessages, StdCtrls, SysUtils, Graphics, Menus, Windows;

Type
  { WinCE interface-object class }

  { TWinCEWidgetSet }

  TWinCEWidgetSet = Class(TWidgetSet)
  Public
    { Constructor of the class }
    Constructor Create;
    { Destructor of the class }
    Destructor Destroy; Override;
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
    Procedure AppTerminate; Override;
    Function  InitHintFont(HintFont: TObject): Boolean; Override;
    Procedure AttachMenuToWindow(AMenuObject: TComponent); Override;

    // create and destroy
    function CreateComponent(Sender : TObject): THandle; override;
    function CreateTimer(Interval: integer; TimerFunc: TFNTimerProc) : integer; override;
    function DestroyTimer(TimerHandle: Integer) : boolean; override;

    {$I wincewinapih.inc}
    {$I wincelclintfh.inc}
  End;


Implementation

Uses
////////////////////////////////////////////////////
// I M P O R T A N T
////////////////////////////////////////////////////
// To get as little as possible circles,
// uncomment only those units with implementation
////////////////////////////////////////////////////
// WinCEWSActnList,
// WinCEWSArrow,
// WinCEWSButtons,
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
// WinCEWSForms,
// WinCEWSGrids,
// WinCEWSImgList,
// WinCEWSMaskEdit,
// WinCEWSMenus,
// WinCEWSPairSplitter,
// WinCEWSSpin,
// WinCEWSStdCtrls,
// WinCEWSToolwin,
////////////////////////////////////////////////////
  LCLProc;

{$I winceobject.inc}
{$I wincewinapi.inc}
{$I wincelclintf.inc}

initialization
  Assert(False, 'Trace:WinCEint.pp - Initialization');

finalization
  Assert(False, 'Trace:WinCEint.pp - Finalization');

end.
