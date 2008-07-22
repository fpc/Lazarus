{
 /***************************************************************************
                               InterfaceBase.pp
                               ----------------
                   Initial Revision  : Fri Jul 23 20:00:00 PDT 1999


 ***************************************************************************/

 *****************************************************************************
 *                                                                           *
 *  This file is part of the Lazarus Component Library (LCL)                 *
 *                                                                           *
 *  See the file COPYING.modifiedLGPL.txt, included in this distribution,        *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************
}

unit InterfaceBase;

{$mode objfpc}
{$LONGSTRINGS ON}

interface

{$ifdef Trace}
  {$ASSERTIONS ON}
{$endif}

uses
  Types, Classes, SysUtils, Math, LCLStrConsts, LCLType, LCLProc, LMessages,
  GraphType, GraphMath, Themes;

type
  PEventHandler = type Pointer;
  PProcessEventHandler = type Pointer;
  PPipeEventHandler = type Pointer;
  PSocketEventHandler = type Pointer;

  TChildExitReason = (cerExit, cerSignal);
  TPipeReason = (prDataAvailable, prBroken, prCanWrite);
  TPipeReasons = set of TPipeReason;

  TApplicationMainLoop = procedure of object;
  TWaitHandleEvent = procedure(AData: PtrInt; AFlags: dword) of object;
  TChildExitEvent = procedure(AData: PtrInt; AReason: TChildExitReason; AInfo: dword) of object;
  TPipeEvent = procedure(AData: PtrInt; AReasons: TPipeReasons) of object;
  TSocketEvent = procedure(AData: PtrInt; AFlags: dword) of object;

  TLCLWndMethod = procedure(var TheMessage: TLMessage) of Object;

  TLCLPlatform = (
    lpGtk,
    lpGtk2,
    lpWin32,
    lpWinCE,
    lpCarbon,
    lpQT,
    lpfpGUI,
    lpNoGUI,
    lpCocoa
    );
    
  TLCLPlatforms = set of TLCLPlatform;

  TLCLCapability = (
    lcAsyncProcess       // support for async process
  );

  { TWidgetSet }

  TWidgetSet = class(TObject)
  protected
    FThemeServices: TThemeServices;
    procedure PassCmdLineOptions; virtual;
    function CreateThemeServices: TThemeServices; virtual;
  public
    constructor Create; virtual;
    destructor Destroy; override;

    procedure AppInit(var ScreenInfo: TScreenInfo); virtual; abstract;
    procedure AppRun(const ALoop: TApplicationMainLoop); virtual;
    procedure AppWaitMessage; virtual; abstract;
    procedure AppProcessMessages; virtual; abstract;
    procedure AppTerminate; virtual; abstract;
    procedure AppMinimize; virtual; abstract;
    procedure AppRestore; virtual; abstract;
    procedure AppBringToFront; virtual; abstract;
    procedure AppSetIcon(const AIcon: HICON); virtual;
    procedure AppSetTitle(const ATitle: string); virtual;
    
    function  LCLPlatform: TLCLPlatform; virtual; abstract;
    function  LCLCapability(ACapability: TLCLCapability): PtrUInt; virtual;

    function  DCGetPixel(CanvasHandle: HDC; X, Y: integer): TGraphicsColor; virtual; abstract;
    procedure DCSetPixel(CanvasHandle: HDC; X, Y: integer; AColor: TGraphicsColor); virtual; abstract;
    procedure DCRedraw(CanvasHandle: HDC); virtual; abstract;
    procedure SetDesigning(AComponent: TComponent); virtual; abstract;

    function  InitHintFont(HintFont: TObject): Boolean; virtual;
    function  IsHelpKey(Key: Word; Shift: TShiftState): Boolean; virtual;

    // create and destroy
    function CreateTimer(Interval: integer; TimerFunc: TFNTimerProc): THandle; virtual; abstract;
    function DestroyTimer(TimerHandle: THandle): boolean; virtual; abstract;
    function AppHandle: Thandle; virtual;

    {$DEFINE IF_BASE_MEMBER}
    {$I winapih.inc}
    {$I lclintfh.inc}
    {$UNDEF IF_BASE_MEMBER}
    
    property ThemeServices: TThemeServices read FThemeServices;
  end;
  TWidgetSetClass = class of TWidgetSet;

const
  LCLPlatformDirNames: array[TLCLPlatform] of string = (
      'gtk',
      'gtk2',
      'win32',
      'wince',
      'carbon',
      'qt',
      'fpgui',
      'nogui',
      'cocoa'
    );
    
type
  EInterfaceException = class(Exception);
  EInterfaceError = class(EInterfaceException);
  EInterfaceCritical = class(EInterfaceException);
  EInterfaceWarning = class(EInterfaceException);

type
  TInputDialogFunction = function (const InputCaption, InputPrompt : String;
                             MaskInput : Boolean; var Value : String) : Boolean;
var
  InputDialogFunction: TInputDialogFunction = nil;

type
  TPromptDialogFunction = Function(const DialogCaption, DialogMessage : String;
    DialogType : longint; Buttons : PLongint;
    ButtonCount, DefaultIndex, EscapeResult : Longint;
    UseDefaultPos: boolean;
    X, Y : Longint) : Longint;
var
  PromptDialogFunction: TPromptDialogFunction = nil;

var
  WidgetSet: TWidgetSet=nil;

implementation

const
  UNKNOWN_VK_PREFIX = 'Word(''';
  UNKNOWN_VK_POSTFIX = ''')';

{$I interfacebase.inc}
{$I intfbasewinapi.inc}
{$I intfbaselcl.inc}

end.
