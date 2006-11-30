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
 *  See the file COPYING.modifiedLGPL, included in this distribution,        *
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
  GraphType, GraphMath;

type
  PEventHandler = type Pointer;
  PProcessEventHandler = type Pointer;
  PPipeEventHandler = type Pointer;

  TChildExitReason = (cerExit, cerSignal);
  TPipeReason = (prDataAvailable, prBroken, prCanWrite);
  TPipeReasons = set of TPipeReason;

  TApplicationMainLoop = procedure of object;
  TWaitHandleEvent = procedure(AData: PtrInt; AFlags: dword) of object;
  TChildExitEvent = procedure(AData: PtrInt; AReason: TChildExitReason; AInfo: dword) of object;
  TPipeEvent = procedure(AData: PtrInt; AReasons: TPipeReasons) of object;

  { TWidgetSet }

  TWidgetSet = class(TObject)
  protected
    procedure PassCmdLineOptions; virtual;
  public
    constructor Create;
    destructor Destroy; override;

    procedure AppInit(var ScreenInfo: TScreenInfo); virtual; abstract;
    procedure AppRun(const ALoop: TApplicationMainLoop); virtual;
    procedure AppWaitMessage; virtual; abstract;
    procedure AppProcessMessages; virtual; abstract;
    procedure AppTerminate; virtual; abstract;
    procedure AppMinimize; virtual; abstract;
    procedure AppRestore; virtual; abstract;
    procedure AppBringToFront; virtual; abstract;
    procedure AppSetTitle(const ATitle: string); virtual;
    
    function  WidgetSetName: string; virtual;

    function  DCGetPixel(CanvasHandle: HDC; X, Y: integer): TGraphicsColor; virtual; abstract;
    procedure DCSetPixel(CanvasHandle: HDC; X, Y: integer; AColor: TGraphicsColor); virtual; abstract;
    procedure DCRedraw(CanvasHandle: HDC); virtual; abstract;
    procedure SetDesigning(AComponent: TComponent); virtual; abstract;

    function  InitHintFont(HintFont: TObject): Boolean; virtual;

    // create and destroy
    function CreateComponent(Sender : TObject): THandle; virtual; abstract;
    function CreateTimer(Interval: integer; TimerFunc: TFNTimerProc): THandle; virtual; abstract;
    function DestroyTimer(TimerHandle: THandle): boolean; virtual; abstract;

    {$DEFINE IF_BASE_MEMBER}
    {$I winapih.inc}
    {$I lclintfh.inc}
    {$UNDEF IF_BASE_MEMBER}
  end;

type
  EInterfaceException = class(Exception);
  EInterfaceError = class(EInterfaceException);
  EInterfaceCritical = class(EInterfaceException);
  EInterfaceWarning = class(EInterfaceException);


{$I defaultbitbtnimages.inc}
{$I messagedialogpixmaps.inc}

type
  TInputDialogFunction = Function (const InputCaption, InputPrompt : String;
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
