{ 
 /*************************************************************************** 
                         WIN32INT.pp  -  Win32Interface Object
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
 
Unit Win32Int;

{$H+}

Interface

{$IFDEF Trace}
{$ASSERTIONS ON}
{$ENDIF}
 
{ 
  When editing this unit list, be sure to keep Windows listed first to ensure
  successful compilation.
}
Uses
  Windows, Classes, ComCtrls, Controls, Dialogs, DynHashArray, ExtCtrls, Forms,
  GraphMath, GraphType, InterfaceBase, LCLLinux, LCLType, LMessages,
  StdCtrls, SysUtils, VCLGlobals, Win32Def;

Type
  { Virtual alignment-control record }
  TAlignment = Record
    Parent: HWnd;     // Parent Control
    Self: HWnd;       // Virtual control handle of alignment
    XAlign: Integer;  // Horizontal alignment
    YAlign: Integer;  // Vertical alignment
    XScale: Real;     // Horizontal scaling
    YScale: Real;     // Vertical scaling
  End;

  { Win32 interface-object class }
  TWin32Object = Class(TInterfaceBase)
  Private
    FKeyStateList: TList; // Keeps track of which keys are pressed
    FDeviceContexts: TDynHashArray;
    FGDIObjects: TDynHashArray;
    FMessageQueue: TList;
    FToolTipWindow: HWND;
    FTimerWindow: HWND;
    FAccelGroup: HACCEL;
    FControlIndex: Cardinal; // Win32-API control index
    FMainForm: TForm;
    FMenu: HMENU; // Main menu/menu bar
    FParentWindow: HWND; // The parent window
    FSubMenu: HMENU; // current sub menu

    FStockNullBrush: HBRUSH;
    FStockBlackBrush: HBRUSH;
    FStockLtGrayBrush: HBRUSH;
    FStockGrayBrush: HBRUSH;
    FStockDkGrayBrush: HBRUSH;
    FStockWhiteBrush: HBRUSH;

    Procedure CreateComponent(Sender: TObject);
    Function RecreateWnd(Sender: TObject): Integer; virtual;
    Function  GetText(Sender: TComponent; var Data: String): Boolean; virtual;
    Procedure SetLabel(Sender: TObject; Data: Pointer);
    Procedure AddChild(Parent, Child: HWND);
    Procedure ResizeChild(Sender: TObject; Left, Top, Width, Height: Integer);
    Procedure AssignSelf(Window: HWnd; Data: Pointer);
    Procedure ReDraw(Child: TObject);
    Procedure SetCursor(Sender: TObject);
    Procedure SetLimitText(Window: HWND; Limit: Word);

    Procedure ShowHide(Sender: TObject);
    Procedure AddNBPage(Notebook: TCustomNotebook; NewPage: TPage; Index: Integer);
    Procedure RemoveNBPage(Parent: TObject; Index: Integer);
    Procedure SetText(Window: HWND; Data: Pointer);
    Procedure SetColor(Sender : TObject);
    Procedure SetPixel(Sender: TObject; Data: Pointer);
    Procedure GetPixel(Sender: TObject; Data: Pointer);
    Function GetValue (Sender: TObject; Data: Pointer): Integer;
    Function SetValue (Sender: TObject; Data: Pointer): Integer;
    Function SetProperties (Sender: TObject): Integer;
    Procedure AttachMenu(Sender: TObject);

    Function WinRegister: Boolean;
    Function TimerWinRegister: Boolean;
    Function ToolBtnWinRegister: Boolean;
    Procedure SetName(Window: HWND; Value: PChar);
    Procedure SetOwner(Window: HWND; Owner: TObject);
    Procedure PaintPixmap(Surface: TObject; PixmapData: Pointer);
    Procedure NormalizeIconName(Var IconName: String);
    Procedure NormalizeIconName(Var IconName: PChar);
    Procedure CreateCommonDialog(Sender: TCommonDialog);

    { Creates a callback of Lazarus message Msg for Sender }
    Procedure SetCallback(Msg: LongInt; Sender: TObject); virtual;
    { Removes all callbacks for Sender }
    Procedure RemoveCallbacks(Sender: TObject); virtual;

  Public
    { Constructor of the class }
    Constructor Create;
    { Destructor of the class }
    Destructor Destroy; Override;
    { Initialize the API }
    Procedure AppInit; Override;
    Function IntSendMessage3(LM_Message: Integer; Sender: TObject; Data: Pointer) : Integer; Override;
    Procedure HandleEvents; Override;
    Procedure WaitMessage; Override;
    Procedure AppTerminate; Override;

    function CreateTimer(Interval: integer; TimerFunc: TFNTimerProc) : integer; override;
    function DestroyTimer(TimerHandle: integer) : boolean; override;

    {$I win32winapih.inc}
  End;
   
  {$I win32listslh.inc}
  
  { Asserts a trace for event named Message in the object Data }
  Procedure EventTrace(Message: String; Data: TObject);

Implementation

Uses
  Arrow, Buttons, Calendar, CListBox, Graphics, Menus, Process, Spin, WinExt;

{$I win32listsl.inc}

Type
  TEventType = (etNotify, etKey, etKeyPress, etMouseWheeel, etMouseUpDown);

  { Linked list of objects for events }
  PLazObject = ^TLazObject;
  TLazObject = Record
    Parent: TObject;
    Messages: TList;
    Next: PLazObject;
  End;

  {$IFDEF VER1_1}
    TMsgArray = Array Of Integer;
  {$ELSE}
    TMsgArray = Array[0..1] Of Integer;
  {$ENDIF}

Const
  BOOL_RESULT: Array[Boolean] Of String = ('False', 'True');
  ClsName : array[0..20] of char = 'LazarusForm'#0;
  TimerClsName : array[0..20] of char = 'LCLTimerWindow'#0;
  ToolBtnClsName : array[0..20] of char = 'ToolbarButton'#0;
  
Var
  WndList: TList;

{$I win32proc.inc}
{$I win32callback.inc}
{$I win32object.inc}
{$I win32winapi.inc}

Initialization

Assert(False, 'Trace:win32int.pp - Initialization');
WndList := TList.Create;

Finalization

Assert(False, 'Trace:win32int.pp - Finalization');
WndList.Free;
WndList := Nil;

End.

{ =============================================================================

  $Log$
  Revision 1.26  2002/12/28 09:42:12  mattias
  toolbutton patch from Martin Smat

  Revision 1.25  2002/12/16 09:02:27  mattias
  applied win32 notebook patch from Vincent

  Revision 1.24  2002/02/09 01:48:23  mattias
  renamed TinterfaceObject.Init to AppInit and TWinControls can now contain childs in gtk

  Revision 1.23  2002/12/04 20:39:16  mattias
  patch from Vincent: clean ups and fixed crash on destroying window

  Revision 1.22  2002/12/03 09:15:15  mattias
  cleaned up

  Revision 1.21  2002/11/26 20:51:05  mattias
  applied clipbrd patch from Vincent

  Revision 1.20  2002/11/23 13:48:48  mattias
  added Timer patch from Vincent Snijders

  Revision 1.19  2002/11/15 23:43:54  mattias
  applied patch from Karl Brandt

  Revision 1.18  2002/10/27 19:59:03  lazarus
  AJ: fixed compiling

  Revision 1.17  2002/10/26 15:15:55  lazarus
  MG: broke LCL<->interface circles

  Revision 1.16  2002/08/08 18:05:48  lazarus
  MG: added graphics extensions from Andrew Johnson

  Revision 1.15  2002/05/31 13:10:49  lazarus
  Keith: Code cleanup.

  Revision 1.14  2002/05/10 07:43:48  lazarus
  MG: updated licenses

  Revision 1.13  2002/04/03 03:41:29  lazarus
  Keith:
    * Removed more obsolete code
    * Compiles again!

  Revision 1.12  2002/04/03 01:52:42  lazarus
  Keith: Removed obsolete code, in preperation of a pending TWin32Object cleanup

  Revision 1.11  2002/02/07 08:35:12  lazarus
  Keith: Fixed persistent label captions and a few less noticable things

  Revision 1.10  2002/02/03 06:06:25  lazarus
  Keith: Fixed Win32 compilation problems

  Revision 1.9  2002/02/01 10:13:09  lazarus
  Keith: Fixes for Win32

  Revision 1.8  2002/01/31 09:32:07  lazarus
  Keith:
    * Open and save dialogs can now coexist in apps (however, only one of each type of common dialog can be used per app :( )
    * Fixed make all
    * Fixed crash in Windows 98/ME

  Revision 1.7  2002/01/25 19:42:56  lazarus
  Keith: Improved events and common dialogs on Win32

  Revision 1.6  2002/01/17 03:17:44  lazarus
  Keith: Fixed TPage creation

  Revision 1.5  2002/01/05 13:16:09  lazarus
  MG: win32 interface update from Keith Bowes

  Revision 1.4  2001/11/01 22:40:13  lazarus
  MG: applied Keith Bowes win32 interface updates

  Revision 1.3  2001/08/02 12:58:35  lazarus
  MG: win32 interface patch from Keith Bowes

  Revision 1.2  2000/12/12 14:16:43  lazarus
  Updated OI from Mattias
  Shane

  Revision 1.1  2000/07/13 10:28:29  michael
  + Initial import

}
