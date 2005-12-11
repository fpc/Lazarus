{ $Id$ }
{ 
 /*************************************************************************** 
                         QTINT.pp  -  QTInterface Object
                             ------------------- 
 
                   Initial Revision  : Thu July 1st CST 1999 
 
 
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
 
unit qtint;
 
{$mode objfpc} 

interface

{$ifdef Trace}
{$ASSERTIONS ON}
{$endif}
 
uses 
  InterfaceBase, sysutils, LCLType, LMessages, Classes, Controls,
  ExtCtrls, Forms, Dialogs, StdCtrls, Comctrls, LCLIntf, qt;

type

  TQtWidgetSet = Class(TWidgetSet)
  private
    procedure CreateComponent(Sender : TObject);
    procedure ShowHide(Sender : TObject);

  public
    {$I qtwinapih.inc}
    {$I qtlclintfh.inc}
  public
    function GetText(Sender: TControl; var Text: String): Boolean; override;
    procedure SetLabel(Sender : TObject; Data : Pointer);
    function  IntSendMessage3(LM_Message : Integer; Sender : TObject; data : pointer) : integer; override;
    procedure SetCallback(Msg : LongInt; Sender : TObject); override;
    procedure RemoveCallbacks(Sender : TObject); override;
    procedure DoEvents; override;
    procedure AppProcessMessages; override;
    procedure AppTerminate; override;
    procedure Init; override;
  end;


type

   TEventProc = record
      Name : String[25];
      CallBack : Procedure(Data : TObject);
      Data : Pointer;
   End;

   CallbackProcedure = Procedure (Data : Pointer);

   pTRect = ^TRect;


   procedure EventTrace(message : string; data : pointer);



const
   TargetEntrys = 3;

implementation

uses 
////////////////////////////////////////////////////
// I M P O R T A N T
////////////////////////////////////////////////////
// To get as little as possible circles,
// uncomment only those units with implementation
////////////////////////////////////////////////////
// QtWSActnList,
// QtWSArrow,
// QtWSButtons,
// QtWSCalendar,
// QtWSCheckLst,
// QtWSCListBox,
// QtWSComCtrls,
// QtWSControls,
// QtWSDbCtrls,
// QtWSDBGrids,
// QtWSDialogs,
// QtWSDirSel,
// QtWSEditBtn,
// QtWSExtCtrls,
// QtWSExtDlgs,
// QtWSFileCtrl,
// QtWSForms,
// QtWSGrids,
// QtWSImgList,
// QtWSMaskEdit,
// QtWSMenus,
// QtWSPairSplitter,
// QtWSSpin,
// QtWSStdCtrls,
// QtWSToolwin,
////////////////////////////////////////////////////
  Graphics, buttons, Menus, CListBox;


const

  KEYMAP_VKUNKNOWN = $10000;
  KEYMAP_TOGGLE    = $20000;
  KEYMAP_EXTENDED  = $40000;

// PDB: note this is a hack. Windows maintains a system wide
//      system color table we will have to have our own
//      to be able to do the translations required from
//      window manager to window manager this means every
//      application will carry its own color table
//      we set the defaults here to reduce the initial
//      processing of creating a default table
// MWE: Naaaaah, not a hack, just something temporary
const
  SysColorMap: array [0..MAX_SYS_COLORS] of DWORD = (
    $C0C0C0,     {COLOR_SCROLLBAR}
    $808000,     {COLOR_BACKGROUND}
    $800000,     {COLOR_ACTIVECAPTION}
    $808080,     {COLOR_INACTIVECAPTION}
    $C0C0C0,     {COLOR_MENU}
    $FFFFFF,     {COLOR_WINDOW}
    $000000,     {COLOR_WINDOWFRAME}
    $000000,     {COLOR_MENUTEXT}
    $000000,     {COLOR_WINDOWTEXT}
    $FFFFFF,     {COLOR_CAPTIONTEXT}
    $C0C0C0,     {COLOR_ACTIVEBORDER}
    $C0C0C0,     {COLOR_INACTIVEBORDER}
    $808080,     {COLOR_APPWORKSPACE}
    $800000,     {COLOR_HIGHLIGHT}
    $FFFFFF,     {COLOR_HIGHLIGHTTEXT}
    $C0C0C0,     {COLOR_BTNFACE}
    $808080,     {COLOR_BTNSHADOW}
    $808080,     {COLOR_GRAYTEXT}
    $000000,     {COLOR_BTNTEXT}
    $C0C0C0,     {COLOR_INACTIVECAPTIONTEXT}
    $FFFFFF,     {COLOR_BTNHIGHLIGHT}
    $000000,     {COLOR_3DDKSHADOW}
    $C0C0C0,     {COLOR_3DLIGHT}
    $000000,     {COLOR_INFOTEXT}
    $E1FFFF,     {COLOR_INFOBK}
    $000000,     {unasigned}
    $000000,     {COLOR_HOTLIGHT}
    $000000,     {COLOR_GRADIENTACTIVECAPTION}
    $000000      {COLOR_GRADIENTINACTIVECAPTION}
  ); {end _SysColors}

type
  TGDIType = (gdiBitmap, gdiBrush, gdiFont, gdiPen, gdiRegion);
  TGDIBitmapType = (gbBitmap, gbPixmap, gbImage);
  
  PGDIRGB = ^TGDIRGB;
  TGDIRGB = record
    Red,
    Green,
    Blue: Byte;
  end;

  PGDIRawImage = ^TGDIRawImage;
  TGDIRawImage = record
    Height,
    Width: Integer;
    Depth: Byte;
    Data: array[0..0] of TGDIRGB;
  end;
  

var
  DeviceContexts: TList;
  GDIObjects: TList;


  KeyStateList: TList; // Keeps track of which keys are pressed

const
  TARGET_STRING = 1;
  TARGET_ROOTWIN = 2;


{$I qtobject.inc}
{$I qtwinapi.inc}
{$I qtcallback.inc}
{$I qtobject.inc}


var
  n: Integer;


initialization
  
  DeviceContexts := TList.Create;
  GDIObjects := TList.Create;
  KeyStateList := TList.Create;


finalization
  if (DeviceContexts.Count > 0) or (GDIObjects.Count > 0)
  then begin
    DebugLn(Format('[QTInt] WARNING: There are %d unreleased DCs and %d unreleased GDIObjects' ,[DeviceContexts.Count, GDIObjects.Count]));
  end;
  DeviceContexts.Free;
  GDIObjects.Free;
  KeyStateList.Free;

end.
