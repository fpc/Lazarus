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

interface

{$ifdef Trace}
{$ASSERTIONS ON}
{$endif}
 
uses Windows, Strings, sysutils, lmessages, Classes, Controls, dialogs, vclGlobals, forms,
     extctrls;
 
const
   csAlignment = 1;
   csBox = 2;
   csButton = 3;
   csComboBox = 4;
   csCheckbox = 5;
   csEdit = 6;
   csForm= 7;
   csgLabel = 8;
   csgtkTable = 9;
   csHScrollBar = 10;
   csListView = 11;
   csMainForm = 12;
   csMemo = 13;
   csMenu = 14;
   csMenuBar = 15;
   csMenuItem = 16;
   csNotebook = 17;
   csFileDialog = 18;
   csRadioButton = 19;
   csScrolledWindow= 20;
   csSpinedit = 21;
   csStatusBar = 22; 
   csTable = 23;
   csToggleBox = 24;
   csVScrollBar = 25;
   csFrame = 26; 
   csButtonBox = 27;   //Not yet used
   csCanvas = 28; 
   csGroupBox = 29;

   csFont = 30;
   csPen = 31;
   csBrush = 32;
   csTimer = 33;
   csPage = 34;

   csColorDialog = 35;

Var AppName : PChar;
    FormClassName : PChar;

Type

   TWin32Object = Class(TInterfaceBase)
   private
//      function GetFixed(Widget : Pointer) : PgtkFixed;
      Function WinRegister: Boolean;
      procedure CreateComponent(Sender : TObject);
      procedure AddChild(Parent,Child : Pointer; Left,Top: Integer);
      procedure ResizeChild(Parent,Child : Pointer; Left,Top,Width,Height : Integer);
      function GetLabel(CompStyle: Integer; P : Pointer) : String;
      procedure AssignSelf(Child ,Data : Pointer);
      procedure ReDraw(Child : Pointer);
      procedure SetName(Child ,Data : Pointer);
      procedure ShowHide(CompStyle : Integer; P : Pointer ; visible : boolean);
      procedure AddNBPage(Parent,Child: Pointer; Index: Integer);
      procedure RemoveNBPage(Parent,Child: Pointer; Index: Integer);
      procedure SetText(Child,Data : Pointer);
      procedure GetFontinfo(Sender : TObject; Data : Pointer);
      procedure DrawFillRect(Child,Data : Pointer);
      procedure DrawRect(Child,Data : Pointer);
      procedure DrawLine(Child,Data : Pointer);
      procedure DrawText(Child,Data : Pointer);
      procedure SetColor(Sender : TObject);

   protected

   public
      procedure SetLabel(CompStyle : Integer; Var P : Pointer; Str1 : String);
      procedure IntSendMessage(LM_Message : Integer; CompStyle : Integer; Var P : Pointer; Val1 : Integer; Var Str1 : String);
      function  IntSendMessage2( LM_Message : Integer; Parent,Child, Data : Pointer) : Integer;
      function  IntSendMessage3(LM_Message : Integer; Sender : TObject; data : pointer) : integer;
      procedure SetCallback(Msg : LongInt; Sender : TObject);
      procedure RemoveCallbacks(Sender : TControl);
      procedure DoEvents;
      procedure HandleEvents;
      procedure AppTerminate;
      procedure Init; override;


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

   TEventProc = record
      Name : String[25];
      CallBack : Procedure(Data : TObject);
      Data : Pointer;
   End;

   CallbackProcedure = Procedure (Data : Pointer);

   pTRect = ^TRect;


   procedure EventTrace(message : string; data : pointer);

Implementation

uses Graphics;

{$I win32callback.inc}
{$I win32object.inc}

end.

