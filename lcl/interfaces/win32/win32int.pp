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
 
unit Win32Int;
 
{$mode objfpc} 
{$LONGSTRINGS ON}

interface

{$ifdef Trace}
{$ASSERTIONS ON}
{$endif}
 
uses
  Windows, Strings, WinExt, InterfaceBase, sysutils, lmessages,
  Classes, Controls, extctrls, forms, dialogs, VclGlobals,
  stdctrls, comctrls, LCLLinux, win32def, DynHashArray;
 
Var AppName : PChar;
    FormClassName : PChar;

Const
  ClsName = 'MainWinClass';

Type
   TAlignment = Record // New record to create a virtual alignment control
     Parent: HWnd;     // Parent Control
     Self: HWnd;       // Virtual control handle of alignment
     XAlign: Integer;  // Horizontal alignment
     YAlign: Integer;  // Vertical alignment
     XScale: Real;     // Horizontal scaling
     YScale: Real;     // Vertical scaling
   End;

   TWin32Object = Class(TInterfaceBase)
   private
      FKeyStateList: TList; // Keeps track of which keys are pressed
      FDeviceContexts: TDynHashArray;
      FGDIObjects: TDynHashArray;
      FMessageQueue: TList;
      FToolTipWindow: HWND;
      FAccelGroup: HACCEL;
      FTimerData : TList;       // keeps track of timer evenet structures

      { New fields for the Win32 target }
      FMenu: HMENU; // Main menu/menu bar
      FSubMenu: HMENU; // current sub menu
      FControlIndex: Cardinal; // Win32-API control index.
      FParentWindow: HWND; // The parent window
      FSender: TObject; // The sender
      FMessage: MSG; // The Windows message
      FHkProc: HHOOK; // Hooking procedure
      FAlignment: TAlignment; // Tracks alignment
      { End of new fields }

      FStockNullBrush: HBRUSH;
      FStockBlackBrush: HBRUSH;
      FStockLtGrayBrush: HBRUSH;
      FStockGrayBrush: HBRUSH;
      FStockDkGrayBrush: HBRUSH;
      FStockWhiteBrush: HBRUSH;
      
      procedure CreateComponent(Sender : TObject);
      procedure AddChild(Parent,Child : Pointer; Left,Top: Integer);
      procedure ResizeChild(Sender : TObject; Left,Top,Width,Height : Integer);
      function GetLabel(CompStyle: Integer; P : Pointer) : String;
      procedure AssignSelf(Child ,Data : Pointer);
      procedure ReDraw(Child : Pointer);
      Procedure SetCursor(Sender : TObject);
      
      function IsValidDC(const DC: HDC): Boolean;
      function IsValidGDIObject(const GDIObject: HGDIOBJ): Boolean;
      function IsValidGDIObjectType(const GDIObject: HGDIOBJ; const GDIType: TGDIType): Boolean;
      function NewGDIObject(const GDIType: TGDIType): PGdiObject;
      function NewDC: PDeviceContext;
      function CreateDefaultBrush: PGdiObject;
      function CreateDefaultFont: PGdiObject;
      function CreateDefaultPen: PGdiObject;

      procedure ShowHide(Sender : TObject);
      procedure AddNBPage(Parent,Child: TObject; Index: Integer);
      procedure RemoveNBPage(Parent: TObject; Index: Integer);
      procedure SetText(Child,Data : Pointer);
      procedure SetColor(Sender : TObject);
      Procedure SetPixel(Sender : TObject; Data : Pointer);
      Procedure GetPixel(Sender : TObject; Data : Pointer);
      function GetValue (Sender : TObject; Data : pointer) : integer;
      function SetValue (Sender : TObject; Data : pointer) : integer;
      function SetProperties (Sender: TObject) : integer;
      procedure AttachMenu(Sender: TObject);

      Function WinRegister: Boolean;
      procedure SetName(Child ,Data : Pointer);
      procedure ShowHide(CompStyle : Integer; P : Pointer ; visible : boolean);
      procedure AddNBPage(Parent,Child: Pointer; Index: Integer);
      procedure RemoveNBPage(Parent,Child: Pointer; Index: Integer);
      procedure GetFontinfo(Sender : TObject; Data : Pointer);
      procedure DrawFillRect(Child,Data : Pointer);
      procedure DrawRect(Child,Data : Pointer);
      procedure DrawLine(Child,Data : Pointer);
      procedure DrawText(Child,Data : Pointer);
   public
      constructor Create;
      destructor Destroy; override;
      procedure SetLabel(Sender : TObject; Data : Pointer);
      Function  GetText(Sender: TControl; Var Data: String): Boolean; Override;
      function  IntSendMessage3(LM_Message : Integer; Sender : TObject; data : pointer) : integer; override;
      procedure SetCallback(Msg : LongInt; Sender : TObject); override;
      procedure DoEvents; override;
      procedure HandleEvents; override;
      procedure AppTerminate; override;
      procedure Init; override;
      function  UpdateHint(Sender: TObject): Integer; override;
      function  RecreateWnd(Sender: TObject): Integer; override;
      Procedure MessageBox(Message, Title: String; Flags: Cardinal);
      procedure IntSendMessage(LM_Message : Integer; CompStyle : Integer; Var P : Pointer; Val1 : Integer; Var Str1 : String);
      function  IntSendMessage2( LM_Message : Integer; Parent,Child, Data : Pointer) : Integer;
      procedure RemoveCallbacks(Sender : TControl);
   end;

       wPointer = Pointer;
       PWin32Control = ^TWin32Control;
       PPWin32Control = ^PWin32Control;
       TWin32Control = record
            theobject :TWin32Object;
            private_flags :Word;
            state : Byte;
            saved_state : Byte;
            name : Pchar;
            thestyle : pointer;
            window : HWnd;
            parent : PWin32Control;
         end;

   PTabInfo = ^TTabInfo;
   TTabInfo = Record
     Caption: PChar;
     Index: Cardinal;
   End;

   TWin32KeyType = (WIN32_KEY_PRESS, WIN32_KEY_RELEASE);
   PWin32KeyEvent = ^TWin32KeyEvent;
   TWin32KeyEvent = Record
     KeyVal: Word;
     Length: Integer;
     Send_Event: Integer;
     State: Integer;
     TheString: String;
     TheType: TWin32KeyType;
     Window: HWND;
   End;

   TWin32ListStringList = Class(TList)
     Constructor Create(Wnd: TObject);
     Sorted: Boolean;
   End;

   TWin32CListStringList = Class(TWin32ListStringList)
   End;
   
   TEventProc = record
      Name : String[25];
      CallBack : Procedure(Data : TObject);
      Data : Pointer;
   End;

   CallbackProcedure = Procedure (Data : Pointer);
   TCbFunc = Function(Win32Control: PWin32Control; Event: Pointer; Data: Pointer): Boolean;
   PCbFunc = ^TCbFunc;

   pTRect = ^TRect;


  procedure EventTrace(message : string; data : pointer);

Implementation

uses Graphics, buttons, Menus, CListBox;


Const
  IcoExt: String = '.ico';

Var
  FromCBProc: Boolean;
  LMessage: Integer;
  
const

  KEYMAP_VKUNKNOWN = $10000;
  KEYMAP_TOGGLE    = $20000;
  KEYMAP_EXTENDED  = $40000;

type
  { lazarus GtkInterface definition for additional timer data, not in gtk }
  PWin32ITimerInfo = ^TWin32ITimerinfo;
  TWin32ITimerInfo = record
    Handle   : hWND;
    IDEvent  : Integer;
    TimerFunc: TFNTimerProc;
  end;
  
   Constructor TWin32ListStringList.Create(Wnd: TObject);
   Begin
     Inherited Create;
   End;


{$I win32proc.inc}
{$I win32callback.inc}
{$I win32object.inc}



var
  n: Integer;


initialization
  

  {gtk_handler_quark := g_quark_from_static_string('gtk-signal-handlers');

  Target_Table[0].Target := 'STRING';
  Target_Table[0].Flags := 0;
  Target_Table[0].Info := TARGET_STRING;
  Target_Table[1].Target := 'text/plain';
  Target_Table[1].Flags := 0;
  Target_Table[1].Info := TARGET_STRING;
  Target_Table[2].Target := 'application/x-rootwin-drop';
  Target_Table[2].Flags := 0;
  Target_Table[2].Info := TARGET_ROOTWIN;

  MCaptureHandle := 0;}

end.