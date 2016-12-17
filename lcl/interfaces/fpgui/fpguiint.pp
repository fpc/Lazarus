{
 /*************************************************************************** 
                         FpGui.pp  -  FpGuiInterface Object

 ***************************************************************************/
 
 *****************************************************************************
  This file is part of the Lazarus Component Library (LCL)

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************
}
 
unit fpguiint;
 
{$mode objfpc}{$H+}

interface

{$ifdef Trace}
{$ASSERTIONS ON}
{$endif}
 
uses 
  // FCL
  Classes, Types, SysUtils, Math,
  // LCL
  LCLPlatformDef, InterfaceBase, LCLProc, LCLType, LMessages,
  Controls, ExtCtrls, Forms, Dialogs, StdCtrls, Comctrls, LCLIntf, GraphType,
  // Bindings
  fpg_main, fpg_form, fpguiproc, fpg_base, fpg_dialogs,
  // Widgetset
  FPGUIWSPrivate, fpguiobjects;

type

  { TFpGuiWidgetSet }

  TFpGuiWidgetSet = Class(TWidgetSet)
  private
  public
    // Application
    procedure AppInit(var ScreenInfo: TScreenInfo); override;
    procedure AppRun(const ALoop: TApplicationMainLoop); override;
    procedure AppWaitMessage; override;
    procedure AppProcessMessages; override;
    procedure AppTerminate; override;
    procedure AppMinimize; override;
    procedure AppRestore; override;
    procedure AppBringToFront; override;
//    procedure AppSetTitle(const ATitle: string); override;
    function LCLPlatform: TLCLPlatform; override;
  public
    constructor Create; override;
    destructor Destroy; override;

    //Graphic procedures
    function DrawFrameControl(DC: HDC; const Rect: TRect; uType, uState: Cardinal
      ): Boolean; override;
    function  DCGetPixel(CanvasHandle: HDC; X, Y: integer): TGraphicsColor; override;
    procedure DCSetPixel(CanvasHandle: HDC; X, Y: integer; AColor: TGraphicsColor); override;
    procedure DCRedraw(CanvasHandle: HDC); override;

    procedure SetDesigning(AComponent: TComponent); override;

    //Raw image support
    function  RawImage_CreateBitmaps(const ARawImage: TRawImage; out ABitmap,
      AMask: HBitmap; ASkipMask: Boolean=False): Boolean; override;
    function RawImage_DescriptionFromDevice(ADC: HDC; out ADesc: TRawImageDescription
      ): Boolean; override;

    //Critical sections... Are really needed ?
    {*
    procedure InitializeCriticalSection(var CritSection: TCriticalSection); override;
    procedure DeleteCriticalSection(var CritSection: TCriticalSection); override;
    procedure EnterCriticalSection(var CritSection: TCriticalSection); override;
    procedure LeaveCriticalSection(var CritSection: TCriticalSection); override;
    *}

    // create and destroy
    function CreateTimer(Interval: integer; TimerFunc: TWSTimerProc): THandle; override;
    function DestroyTimer(TimerHandle: THandle): boolean; override;

    // device contexts
    function IsValidDC(const DC: HDC): Boolean; virtual;
    function IsValidGDIObject(const GDIObject: HGDIOBJ): Boolean; virtual;
  public
    {$I fpguiwinapih.inc}
    {$I fpguilclintfh.inc}
  end;


var
  FpGuiWidgetSet: TFpGuiWidgetSet;

implementation

uses 
  FpGuiWSFactory,
  // todo: remove unneeded here and move RegisterWSXXX to FpGuiWSFactory unit
  FpGuiWSButtons,
  FpGuiWSControls,
  FpGuiWSExtCtrls,
  FpGuiWSForms,
  FpGuiWSMenus,
  FpGuiWSStdCtrls,
  FpGuiWSComCtrls,
////////////////////////////////////////////////////
  Graphics, buttons, Menus;

{$I fpguiobject.inc}
{$I fpguiwinapi.inc}
{$I fpguilclintf.inc}
{.$I fpguicallback.inc}


initialization

finalization

end.
