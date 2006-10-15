{ $Id$ }
{ 
 /*************************************************************************** 
                         QTINT.pp  -  QTInterface Object
                             ------------------- 
 
                   Initial Revision  : Thu July 1st CST 1999 
 
 
 ***************************************************************************/ 
 
 *****************************************************************************
 *                                                                           *
 *  This file is part of the Lazarus Component Library (LCL)                 *
 *                                                                           *
 *  See the file COPYING.modifiedLGPL, included in this distribution,        *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************
}
 
unit qtint;
 
{$mode objfpc}{$H+}

interface

{$ifdef Trace}
{$ASSERTIONS ON}
{$endif}
 
uses 
  // Bindings - qt4 must come first to avoid type redefinition problems on Windows
  qt4,
  // LCL
  Types, InterfaceBase, SysUtils, LCLProc, LCLType, LMessages, Classes,
  Controls, ExtCtrls, Forms, Dialogs, StdCtrls, Comctrls, LCLIntf,
  GraphType, Math;

type

  { TQtWidgetSet }

  TQtWidgetSet = Class(TWidgetSet)
  private
    App: QApplicationH;
  public
    // Application
    procedure AppInit(var ScreenInfo: TScreenInfo); override;
    procedure AppRun(const ALoop: TApplicationMainLoop); override;
    procedure AppWaitMessage; override;
    procedure AppProcessMessages; override;
    procedure AppTerminate; override;
    procedure AppMinimize; override;
    procedure AppBringToFront; override;
//    procedure AppSetTitle(const ATitle: string); override;
  public
    constructor Create;
    destructor Destroy; override;

    function  DCGetPixel(CanvasHandle: HDC; X, Y: integer): TGraphicsColor; override;
    procedure DCSetPixel(CanvasHandle: HDC; X, Y: integer; AColor: TGraphicsColor); override;
    procedure DCRedraw(CanvasHandle: HDC); override;
    procedure SetDesigning(AComponent: TComponent); override;

    function  InitHintFont(HintFont: TObject): Boolean; override;

    // create and destroy
    function CreateComponent(Sender : TObject): LCLType.THandle; override; // deprecated
    function CreateTimer(Interval: integer; TimerFunc: TFNTimerProc): LCLType.THandle; override;
    function DestroyTimer(TimerHandle: LCLType.THandle): boolean; override;

    // device contexts
    function IsValidDC(const DC: HDC): Boolean; virtual;
    function IsValidGDIObject(const GDIObject: HGDIOBJ): Boolean; virtual;
  public
    {$I qtwinapih.inc}
    {$I qtlclintfh.inc}
  end;


type
  TEventProc = record
    Name : String[25];
    CallBack : Procedure(Data : TObject);
    Data : Pointer;
  End;

  CallbackProcedure = Procedure (Data : Pointer);

  pTRect = ^TRect;

  procedure EventTrace(message : string; data : pointer);


const
   TargetEntrys = 3;

var
  QtWidgetSet: TQtWidgetSet;

implementation

uses 
////////////////////////////////////////////////////
// I M P O R T A N T
////////////////////////////////////////////////////
// To get as little as possible circles,
// uncomment only those units with implementation
////////////////////////////////////////////////////
// QtWSActnList,
// QtWSArrow,
 QtWSButtons,
// QtWSCalendar,
// QtWSCheckLst,
// QtWSCListBox,
 QtWSComCtrls,
 QtWSControls,
// QtWSDbCtrls,
// QtWSDBGrids,
 QtWSDialogs,
// QtWSDirSel,
// QtWSEditBtn,
 QtWSExtCtrls,
// QtWSExtDlgs,
// QtWSFileCtrl,
 QtWSForms,
// QtWSGrids,
// QtWSImgList,
// QtWSMaskEdit,
 QtWSMenus,
// QtWSPairSplitter,
 QtWSSpin,
 QtWSStdCtrls,
// QtWSToolwin,
////////////////////////////////////////////////////
  Graphics, buttons, Menus,
  // Bindings
  qtprivate, qtwidgets, qtobjects;


const

  KEYMAP_VKUNKNOWN = $10000;
  KEYMAP_TOGGLE    = $20000;
  KEYMAP_EXTENDED  = $40000;

procedure EventTrace(message: string; data: pointer);
begin

end;

{$I qtobject.inc}
{$I qtwinapi.inc}
{$I qtlclintf.inc}
{.$I qtcallback.inc}


initialization

finalization

end.
