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

// debugging defines
{$I carbondebug.inc}

uses
 // rtl+ftl
  Types, Classes, SysUtils, Math, FPCAdds,
 // carbon bindings
  FPCMacOSAll,
 // interfacebase
  InterfaceBase,
 // widgetset
  CarbonGDIObjects,
 {$ifdef DebugBitmaps}
  CarbonDebug,
 {$endif}
   glgrab,
 // LCL
  LCLStrConsts, LMessages, LCLMessageGlue, LCLProc, LCLIntf, LCLType,
  GraphType, GraphMath, Graphics, Controls, Forms, Dialogs, Menus, Maps, Themes;

type

  { TCarbonWidgetSet }

  TCarbonWidgetSet = class(TWidgetSet)
  private
    // Set when the QuitEventHandler terminates
    FTerminating: Boolean;
    FMainEventQueue: EventQueueRef;
    FTimerMap: TMap; // the map contains all installed timers
    FCurrentCursor: HCURSOR;
    FMainMenu: TMainMenu; // Main menu attached to menu bar
    FCaptureWidget: HWND; // Captured widget (TCarbonWidget descendant)
    FOpenEventHandlerUPP: AEEventHandlerUPP;
    FQuitEventHandlerUPP: AEEventHandlerUPP;

    function RawImage_DescriptionFromCarbonBitmap(out ADesc: TRawImageDescription; ABitmap: TCarbonBitmap): Boolean;
    function RawImage_FromCarbonBitmap(out ARawImage: TRawImage; ABitmap, AMask: TCarbonBitmap; const ARect: TRect): Boolean;
    function GetImagePixelData(AImage: CGImageRef; var bitmapByteCount: PtrUInt): Pointer;
  protected
    function CreateThemeServices: TThemeServices; override;
    procedure PassCmdLineOptions; override;
    procedure SendCheckSynchronizeMessage;
    procedure OnWakeMainThread(Sender: TObject);

    procedure RegisterEvents;
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

    procedure AttachMenuToWindow(AMenuObject: TComponent); override;
    
    function  DCGetPixel(CanvasHandle: HDC; X, Y: integer): TGraphicsColor; override;
    procedure DCSetPixel(CanvasHandle: HDC; X, Y: integer; AColor: TGraphicsColor); override;
    procedure DCRedraw(CanvasHandle: HDC); override;
    procedure SetDesigning(AComponent: TComponent); override;

    function  IsHelpKey(Key: Word; Shift: TShiftState): Boolean; override;

    // create and destroy
    function CreateTimer(Interval: integer; TimerFunc: TFNTimerProc) : THandle; override;
    function DestroyTimer(TimerHandle: THandle) : boolean; override;
    function PrepareUserEvent(Handle: HWND; Msg: Cardinal; wParam: WParam;
      lParam: LParam; out Target: EventTargetRef): EventRef;

    // the winapi compatibility methods
    {$I carbonwinapih.inc}
    // the extra LCL interface methods
    {$I carbonlclintfh.inc}

  public
    procedure SetMainMenuEnabled(AEnabled: Boolean);
    procedure SetRootMenu(const AMenu: TMainMenu);
    property MainMenu: TMainMenu read FMainMenu;
  public
    procedure SetCaptureWidget(const AWidget: HWND);
    procedure SetTextFractional(ACanvas: TCanvas; AEnabled: Boolean);
  end;
  
const
  // missing constant
  kAEOpenContents = $6F636F6E (* 'ocon' *);

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
  CarbonWSArrow,
  CarbonWSButtons,
// CarbonWSCalendar,
 CarbonWSCheckLst,
  CarbonWSComCtrls,
  CarbonWSControls,
// CarbonWSDbCtrls,
// CarbonWSDBGrids,
  CarbonWSDialogs,
// CarbonWSDirSel,
// CarbonWSEditBtn,
  CarbonWSExtCtrls,
// CarbonWSExtDlgs,
// CarbonWSFileCtrl,
  CarbonWSForms,
// CarbonWSGrids,
// CarbonWSImgList,
// CarbonWSMaskEdit,
  CarbonWSMenus,
  CarbonWSPairSplitter,
  CarbonWSSpin,
  CarbonWSStdCtrls,
// CarbonWSToolwin,
////////////////////////////////////////////////////
  { these can/should go up }
  CarbonDef, CarbonPrivate, CarbonMenus, CarbonButtons, CarbonBars, CarbonEdits,
  CarbonListViews, CarbonTabs,
  CarbonThemes, CarbonCanvas, CarbonStrings, CarbonClipboard, CarbonCaret,
  CarbonProc, CarbonDbgConsts, CarbonUtils,
  
  Buttons, StdCtrls, PairSplitter, ComCtrls, Calendar, Arrow,
  Spin, ExtCtrls, FileCtrl, LResources;

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
