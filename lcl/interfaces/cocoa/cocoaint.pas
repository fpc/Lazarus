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
 *  See the file COPYING.modifiedLGPL, included in this distribution,        *
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

interface

uses
  // rtl+ftl
  Types, Classes, SysUtils, Math,
  // carbon bindings
{$ifdef ver2_2_0}
  FPCMacOSAll,
{$else}
  MacOSAll,
{$endif}
  // Cocoa bindings
  ctypes, objc, foundation, appkit,
  // interfacebase
  InterfaceBase,
  // private
  CocoaPrivate,
  // LCL
  LCLStrConsts, LMessages, LCLMessageGlue, LCLProc, LCLIntf, LCLType;
//  GraphType, GraphMath, Graphics, Controls, Forms, Dialogs, Menus, Maps, Themes;

type

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

    pool: NSAutoreleasePool;

{    function RawImage_DescriptionFromCarbonBitmap(out ADesc: TRawImageDescription; ABitmap: TCarbonBitmap): Boolean;
    function RawImage_FromCarbonBitmap(out ARawImage: TRawImage; ABitmap, AMask: TCarbonBitmap; const ARect: TRect): Boolean;
    function GetImagePixelData(AImage: CGImageRef; var bitmapByteCount: PtrUInt): Pointer;}
  protected
{    function CreateThemeServices: TThemeServices; override;
    procedure PassCmdLineOptions; override;
    procedure SendCheckSynchronizeMessage;
    procedure OnWakeMainThread(Sender: TObject);

    procedure RegisterEvents;}
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

//    procedure AttachMenuToWindow(AMenuObject: TComponent); override;
    
//    function  DCGetPixel(CanvasHandle: HDC; X, Y: integer): TGraphicsColor; override;
//    procedure DCSetPixel(CanvasHandle: HDC; X, Y: integer; AColor: TGraphicsColor); override;
//    procedure DCRedraw(CanvasHandle: HDC); override;
//    procedure SetDesigning(AComponent: TComponent); override;

//    function  IsHelpKey(Key: Word; Shift: TShiftState): Boolean; override;

    // create and destroy
//    function CreateTimer(Interval: integer; TimerFunc: TFNTimerProc) : THandle; override;
 //   function DestroyTimer(TimerHandle: THandle) : boolean; override;
 //   function PrepareUserEvent(Handle: HWND; Msg: Cardinal; wParam: WParam;
 //     lParam: LParam; out Target: EventTargetRef): EventRef;

    // the winapi compatibility methods
    {$I cocoawinapih.inc}
    // the extra LCL interface methods
//    {$I carbonlclintfh.inc}

  end;
  
var
  CocoaWidgetSet: TCocoaWidgetSet;

implementation

uses
////////////////////////////////////////////////////
// I M P O R T A N T
////////////////////////////////////////////////////
// To get as little as possible circles,
// uncomment only those units with implementation
////////////////////////////////////////////////////
// CocoaWSActnList,
//  CocoaWSArrow,
//  CocoaWSButtons,
// CocoaWSCalendar,
// CocoaWSCheckLst,
//  CocoaWSComCtrls,
//  CocoaWSControls,
// CocoaWSDbCtrls,
// CocoaWSDBGrids,
//  CocoaWSDialogs,
// CocoaWSDirSel,
// CocoaWSEditBtn,
//  CocoaWSExtCtrls,
// CocoaWSExtDlgs,
// CocoaWSFileCtrl,
  CocoaWSForms,
// CocoaWSGrids,
// CocoaWSImgList,
// CocoaWSMaskEdit,
//  CocoaWSMenus,
//  CocoaWSPairSplitter,
//  CocoaWSSpin,
  CocoaWSStdCtrls;
// CocoaWSToolwin,
////////////////////////////////////////////////////

// the implementation of the utility methods
{$I cocoaobject.inc}
// the implementation of the winapi compatibility methods
{$I cocoawinapi.inc}
// the implementation of the extra LCL interface methods
//{$I Cocoalclintf.inc}


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
