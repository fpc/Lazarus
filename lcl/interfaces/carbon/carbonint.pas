{
 /***************************************************************************
                    CarbonInt.pas  -  CarbonInterface Object
                    ----------------------------------------

                 Initial Revision  : Mon August 6th CST 2004


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

unit CarbonInt;

{$mode objfpc}{$H+}

interface

{$ifdef Trace}
{$ASSERTIONS ON}
{$endif}

uses
  // rtl+ftl
  Types, Classes, SysUtils, Math, FPCAdds,
  // interfacebase
  InterfaceBase,
  // carbon bindings

  FPCMacOSAll, CarbonUtils, CarbonExtra,

  // LCL
  Controls, Forms, Dialogs, LCLStrConsts, LMessages, LCLProc, LCLIntf, LCLType,
  GraphType, GraphMath, Graphics, Menus, Maps;


type

  { TCarbonWidgetSet }

  TCarbonWidgetSet = class(TWidgetSet)
  private
    // Set when the QuitEventHandler terminates
    FTerminating: Boolean;
    FMainEventQueue: EventQueueRef;
    FTimerMap: TMap; // the map contains all installed timers
  protected
    procedure PassCmdLineOptions; override;
    procedure SendCheckSynchronizeMessage;
    procedure OnWakeMainThread(Sender: TObject);
  public
    constructor Create;
    destructor Destroy; override;

    procedure AppInit(var ScreenInfo: TScreenInfo); override;
    procedure AppRun(const ALoop: TApplicationMainLoop); override;
    procedure AppWaitMessage; override;
    procedure AppProcessMessages; override;
    procedure AppTerminate; override;
    procedure AppMinimize; override;
    procedure AppBringToFront; override;
    function  WidgetSetName: string; override;
    
    function  DCGetPixel(CanvasHandle: HDC; X, Y: integer): TGraphicsColor; override;
    procedure DCSetPixel(CanvasHandle: HDC; X, Y: integer; AColor: TGraphicsColor); override;
    procedure DCRedraw(CanvasHandle: HDC); override;
    procedure SetDesigning(AComponent: TComponent); override;

    // create and destroy
    function CreateComponent(Sender : TObject): THandle; override;
    function CreateTimer(Interval: integer; TimerFunc: TFNTimerProc) : THandle; override;
    function DestroyTimer(TimerHandle: THandle) : boolean; override;

    // the winapi compatibility methods
    {$I carbonwinapih.inc}
    // the extra LCL interface methods
    {$I carbonlclintfh.inc}

  public
  end;

var
  CarbonWidgetSet: TCarbonWidgetSet;

implementation

uses
////////////////////////////////////////////////////
// I M P O R T A N T
////////////////////////////////////////////////////
// To get as little as possible circles,
// uncomment only those units with implementation
////////////////////////////////////////////////////
// CarbonWSActnList,
// CarbonWSArrow,
  CarbonWSButtons,
// CarbonWSCalendar,
// CarbonWSCheckLst,
// CarbonWSCListBox,
// CarbonWSComCtrls,
  CarbonWSControls,
// CarbonWSDbCtrls,
// CarbonWSDBGrids,
// CarbonWSDialogs,
// CarbonWSDirSel,
// CarbonWSEditBtn,
// CarbonWSExtCtrls,
// CarbonWSExtDlgs,
// CarbonWSFileCtrl,
  CarbonWSForms,
// CarbonWSGrids,
// CarbonWSImgList,
// CarbonWSMaskEdit,
// CarbonWSMenus,
// CarbonWSPairSplitter,
// CarbonWSSpin,
  CarbonWSStdCtrls,
// CarbonWSToolwin,
////////////////////////////////////////////////////
  CarbonDef, CarbonProc,
  Buttons, StdCtrls, PairSplitter, ComCtrls, CListBox, Calendar, Arrow,
  Spin, CommCtrl, ExtCtrls, FileCtrl, LResources;

// the implementation of the utility methods
{$I carbonobject.inc}
// the implementation of the winapi compatibility methods
{$I carbonwinapi.inc}
// the implementation of the extra LCL interface methods
{$I carbonlclintf.inc}


procedure InternalInit;
begin
end;

procedure InternalFinal;
begin
end;


initialization
  {$I carbonimages.lrs}
  InternalInit;

finalization
  InternalFinal;

end.
