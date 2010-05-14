{ $Id: qtint.pp 10067 2006-10-15 13:27:27Z andrew $ }
{ 
 /*************************************************************************** 
                         FpGui.pp  -  FpGuiInterface Object
                             ------------------- 
 
                   Initial Revision  : Thu July 1st CST 1999 
 
 
 ***************************************************************************/ 
 
 *****************************************************************************
 *                                                                           *
 *  This file is part of the Lazarus Component Library (LCL)                 *
 *                                                                           *
 *  See the file COPYING.modifiedLGPL.txt, included in this distribution,    *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
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
  InterfaceBase, LCLProc, LCLType, LMessages,
  Controls, ExtCtrls, Forms, Dialogs, StdCtrls, Comctrls, LCLIntf,
  GraphType,
  // Bindings
  fpg_main, fpg_form, fpguiproc, fpg_base,
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

    function  DCGetPixel(CanvasHandle: HDC; X, Y: integer): TGraphicsColor; override;
    procedure DCSetPixel(CanvasHandle: HDC; X, Y: integer; AColor: TGraphicsColor); override;
    procedure DCRedraw(CanvasHandle: HDC); override;
    procedure SetDesigning(AComponent: TComponent); override;  

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
////////////////////////////////////////////////////
  Graphics, buttons, Menus;

{$I fpguiobject.inc}
{$I fpguiwinapi.inc}
{$I fpguilclintf.inc}
{.$I fpguicallback.inc}


initialization

finalization

end.
