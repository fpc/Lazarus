{ 
 /*************************************************************************** 
                         WIN32INT.pp  -  Win32Interface Object
                             ------------------- 
 
 
 
 ***************************************************************************/ 
 
/*************************************************************************** 
 *                                                                         * 
 *   This program is free software; you can redistribute it and/or modify  * 
 *   it under the terms of the GNU General Public License as published by  * 
 *   the Free Software Foundation; either version 2 of the License, or     * 
 *   (at your option) any later version.                                   * 
 *                                                                         * 
 ***************************************************************************/ 
} 
 
Unit Win32Int;

{ $I checkcompiler.inc}

{$MODE OBJFPC} 
{$LONGSTRINGS ON}

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
  GraphType, InterfaceBase, LCLLinux, LCLType, LMessages, StdCtrls, SysUtils,
  VCLGlobals, Win32Def;

Var
  AppName: PChar;
  FormClassName: PChar;

Const
  ClsName = 'LazarusForm';

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
    FAccelGroup: HACCEL;
    FTimerData: TList;       // Keeps track of timer event structures

    FAlignment: TAlignment; // Tracks alignment
    FControlIndex: Cardinal; // Win32-API control index
    FHkProc: HHOOK; // Hooking procedure
    FMainForm: TForm;
    FMenu: HMENU; // Main menu/menu bar
    FMessage: TMSG; // The Windows message
    FParentWindow: HWND; // The parent window
    FSender: TObject; // The sender
    FSubMenu: HMENU; // current sub menu
    //FWndList: TList; // Collection of windows with properties
    FWndProc: WNDPROC;

    FStockNullBrush: HBRUSH;
    FStockBlackBrush: HBRUSH;
    FStockLtGrayBrush: HBRUSH;
    FStockGrayBrush: HBRUSH;
    FStockDkGrayBrush: HBRUSH;
    FStockWhiteBrush: HBRUSH;

    Procedure CreateComponent(Sender: TObject);
    Procedure AddChild(Parent, Child: HWND; Left, Top: Integer);
    Procedure ResizeChild(Sender: TObject; Left, Top, Width, Height: Integer);
    Function GetLabel(CompStyle: Integer; Window: HWnd): String;
    Procedure AssignSelf(Window: HWnd; Data: Pointer);
    Procedure ReDraw(Child: TObject);
    Procedure SetCursor(Sender: TObject);
    Procedure SetLimitText(Window: HWND; Limit: Word);
 
    Function IsValidDC(const DC: HDC): Boolean;
    Function IsValidGDIObject(const GDIObject: HGDIOBJ): Boolean;
    Function IsValidGDIObjectType(const GDIObject: HGDIOBJ; const GDIType: TGDIType): Boolean;
    Function NewGDIObject(const GDIType: TGDIType): PGdiObject;
    Function NewDC: PDeviceContext;
    Function CreateDefaultBrush: PGdiObject;
    Function CreateDefaultFont: PGdiObject;
    Function CreateDefaultPen: PGdiObject;

    Procedure ShowHide(Sender: TObject);
    Procedure AddNBPage(Parent, Child: TObject; Index: Integer);
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
    Procedure SetName(Window: HWND; Value: PChar);
    Procedure SetOwner(Window: HWND; Owner: TObject);
    Procedure ShowHide(CompStyle: Integer; P: Pointer; Visible: Boolean);
    Procedure AddNBPage(Parent, Child: Pointer; Index: Integer);
    Procedure RemoveNBPage(Parent, Child: Pointer; Index: Integer);
    Procedure GetFontInfo(Sender: TObject; Data: Pointer);
    Procedure DrawFillRect(Child: TObject; Data: Pointer);
    Procedure DrawRect(Child: TObject; Data: PRect);
    Procedure DrawLine(Child: TObject; Data: Pointer);
    Procedure DrawText(Child: TObject; Data: Pointer);
    Procedure PaintPixmap(Surface: TObject; PixmapData: Pointer);
    Procedure NormalizeIconName(Var IconName: String);
    Procedure NormalizeIconName(Var IconName: PChar);
    Procedure CreateCommonDialog(Sender: TCommonDialog);
  Public
    { Constructor of the class }
    Constructor Create;
    { Destructor of the class }
    Destructor Destroy; Override;
    { Initialize the API }
    Procedure Init; Override;
    { Get the text from control Sender and store it in variable Data }
    Function GetText(Sender: TControl; Var Data: String): Boolean; Override;
    { Set Label of control Sender to Data }
    Procedure SetLabel(Sender: TObject; Data: Pointer);
    { Process Lazarus message LM_Message }
    Procedure IntSendMessage(LM_Message: Integer; CompStyle: Integer; Var P: Pointer; Val1: Integer; Var Str1: String);
    { Process Lazarus message LM_Message, version 2 }
    Function IntSendMessage2(LM_Message: Integer; Parent, Child, Data: Pointer): Integer;
    { Process Lazarus message LM_Message and return an integer result }
    Function IntSendMessage3(LM_Message: Integer; Sender: TObject; Data: Pointer) : Integer; Override;
    { Creates a callback of Lazarus message Msg for Sender }
    Procedure SetCallback(Msg: LongInt; Sender: TObject); Override;
    { Removes all callbacks for Sender }
    Procedure RemoveCallbacks(Sender: TObject); Override;
    { Processes all events (Window messages) in the queue }
    Procedure DoEvents; Override;
    { Handle all events (Window messages) }
    Procedure HandleEvents; Override;
    { Wait until a message is received }
    Procedure WaitMessage; Override;
    { Abruptly halt execution of the program }
    Procedure AppTerminate; Override;
    { Update a hint (tool tip) }
    Function UpdateHint(Sender: TObject): Integer; Override;
    { Create a window again }
    Function RecreateWnd(Sender: TObject): Integer; Override;

    {$I win32winapih.inc}
  End;
   
  {$I win32listslh.inc}

  WPointer = Pointer;
  PWin32Control = ^TWin32Control;
  PPWin32Control = ^PWin32Control;
  { Properies of the Windows control }
  TWin32Control = Record
    TheObject: TWin32Object;
    Private_Flags: Word;
    State: Byte;
    Saved_State: Byte;
    Name: PChar;
    TheStyle: Pointer;
    Window: HWnd;
    Parent: PWin32Control;
  End;

  PTabInfo = ^TTabInfo;
  { Tab information }
  TTabInfo = Record
    Caption: PChar;
    Index: Cardinal;
  End;

  { Enumerated type of key states }
  TWin32KeyType = (WIN32_KEY_PRESS, WIN32_KEY_RELEASE);
  PWin32KeyEvent = ^TWin32KeyEvent;
  { Key event record }
  TWin32KeyEvent = Record
    KeyVal: Word;
    Length: Integer;
    Send_Event: Integer;
    State: Integer;
    TheString: String;
    TheType: TWin32KeyType;
    Window: HWND;
  End;

  { Record of key data for events }
  TEventProc = record
    Name : String[25];
    CallBack : Procedure(Data : TObject);
    Data : Pointer;
  End;

  { Procedural type for casting as a callback procedure }
  CallbackProcedure = Function: Boolean;
  { Procedural type for casting as a callback function }
  TCbFunc = Function(Win32Control: HWND; Event: Pointer; Data: Pointer): Boolean;
  PCbFunc = ^TCbFunc;

  pTRect = ^TRect;
  
  { Asserts a trace for event named Message in the object Data }
  Procedure EventTrace(Message: String; Data: TObject);

Implementation

Uses
  Arrow, Buttons, Calendar, CListBox, Graphics, Menus, Process, Spin, WinExt;

{$I win32listsl.inc}

Type
  PList = ^TList;
  PLMNotebookEvent = ^TLMNotebookEvent;

  { Lazarus Message structure for call backs }
  TLazMsg = Record
    Window: HWND;
    WinMsg: UINT;
    LParam: LPARAM;
    WParam: WPARAM;
    Win32Control: PWin32Control;
    Event: Pointer;
    Draw: TPoint;
    ExtData: Pointer;
    Reserved: Pointer;
  End;

  { Linked list of objects for events }
  PLazObject = ^TLazObject;
  TLazObject = Record
    Parent: TObject;
    Messages: TList;
    Next: PLazObject;
  End;

  PLazProp = ^TLazProp;
  TLazProp = Record
    Window: HWND;
    Key: PChar;
    Val: Pointer;
  End;

  {$IFDEF VER1_1}
    TMsgArray = Array Of Integer;
  {$ELSE}
    TMsgArray = Array[0..1] Of Integer;
  {$ENDIF}

  TPrivateControl = Class(TControl)
  Public
    Procedure WndProc(Var LMsg: TLMessage); Override;
  End;

  Procedure TPrivateControl.WndProc(Var LMsg: TLMessage);
  Begin
    Inherited WndProc(LMsg);
  End;

Const
  IcoExt: String = '.ico';
  BOOL_RESULT: Array[Boolean] Of String = ('False', 'True');

Var
  FromCBProc: Boolean;
  LazMsg: TLazMsg;
  LazObject: PLazObject;
  LMessage: Integer;
  OldClipboardViewer: HWND;
  OrigWndProc: WNDPROC;
  SignalFunc: Pointer;
  WndList: TList;

Const

  KEYMAP_VKUNKNOWN = $10000;
  KEYMAP_TOGGLE    = $20000;
  KEYMAP_EXTENDED  = $40000;

Type
  { record of data for timers }
  PWin32ITimerInfo = ^TWin32ITimerinfo;
  TWin32ITimerInfo = Record
    Handle   : HWND;
    IDEvent  : Integer;
    TimerFunc: TFNTimerProc;
  End;

{$I win32proc.inc}
{$I win32callback.inc}
{$I win32object.inc}
{$I win32winapi.inc}

Var
  N: Integer;

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
