{
 /***************************************************************************
                                     menus.pp
                                     --------
                Component Library TMenu, TMenuItem, TMenuBar Controls
                   Initial Revision  : Mon Jul 26 0:10:12 1999


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

{
TMenu, TMenuItem, TMenuBar
@author(TMenu - Shane Miller <smiller@lakefield.net>)
@author(TMenuItem - Shane Miller <smiller@lakefield.net>)
@author(TMenuBar - Shane Miller <smiller@lakefield.net>)
@author(TMainMenu - Marc Weustink <weus@quicknet.nl>)
@author(TPopupMenu - Marc Weustink <weus@quicknet.nl>
@created(26-Jul-1999)
@lastmod(27-Oct-1999)

Detailed description of the Unit.
}
unit Menus;

{$mode objfpc}{$H+}

interface

{$ifdef Trace}
{$ASSERTIONS ON}
{$endif}

uses
  VCLGlobals, Classes, SysUtils, LMessages;


type
  TShortCut = Low(Word)..High(Word);   {should be moved to classes}

  TMenu = class;
  EMenuError = class(Exception);

  TMenuItem = class;
  
  TMenuChangeEvent = procedure (Sender: TObject; Source: TMenuItem;
                                Rebuild: Boolean) of object;

  TMenuItem = class(TComponent)//TWinControl)
  private
    FCaption: string;
    FChecked: Boolean;
    FCommand: integer;
    FDefault: Boolean;
    FEnabled: Boolean;
    FHandle: HMenu;
    FHint : String;
    FImageIndex : Integer;
    FItems: TList;
    FMenu: TMenu;
    FParent: TMenuItem;
    FShortCut: TShortCut;	
    FVisible: Boolean;
    FOnChange: TMenuChangeEvent;
    FOnClick: TNotifyEvent;
    function GetCount: Integer;
    function GetItem(Index: Integer): TMenuItem;
    function GetParent: TMenuItem;
    procedure SetCaption(const Value: string);
    procedure SetChecked(Value: Boolean);
    procedure SetDefault(Value: Boolean);
    procedure SetEnabled(Value: Boolean);
    procedure ShortcutChanged(const OldValue, Value : TShortcut);
  protected
    procedure CreateHandle; virtual;
    procedure DoClicked(var msg); message LM_ACTIVATE;               //'activate';
    function GetHandle: HMenu;
    Procedure SetImageIndex(value : Integer);
    procedure SetShortCut(Value : TShortCut);
    procedure SetVisible(Value: Boolean);
    procedure MenuChanged(Rebuild : Boolean);
    procedure SetParentComponent(Value : TComponent); override;
    procedure GetChildren(Proc: TGetChildProc; Root: TComponent); override;
  public
    FCompStyle : LongInt;
    procedure Add(Item: TMenuItem);
    constructor Create(AOwner: TComponent); override;
    procedure Delete(Index: Integer);
    destructor Destroy; override;
    function GetParentMenu: TMenu;
    function HandleAllocated : Boolean;
    procedure HandleNeeded;
    function HasParent : Boolean; override;
    function IndexOf(Item: TMenuItem): Integer;
    procedure Insert(Index: Integer; Item: TMenuItem);
    procedure Remove(Item: TMenuItem);
    property Count: Integer read GetCount;
    property Handle: HMenu read GetHandle write FHandle;
    property Items[Index: Integer]: TMenuItem read GetItem; default;
    property Parent: TMenuItem read GetParent;
  published
    property Caption: String read FCaption write SetCaption {stored IsCaptionStored};
    property Checked: Boolean read FChecked write SetChecked {stored IsCheckedStored} default False;
    property Default: Boolean read FDefault write SetDefault default False;
    property Enabled: Boolean read FEnabled write SetEnabled {stored IsEnabledStored} default True;
    property Hint : String read FHint write FHint;
    property ImageIndex : Integer read FImageIndex write SetImageIndex;
    property ShortCut: TShortCut read FShortCut write SetShortCut {stored IsShortCutStored} default 0;
    property Visible: Boolean read FVisible write SetVisible {stored IsVisibleStored} default True;
    property OnClick: TNotifyEvent read FOnClick write FOnclick; 
  end;

  TFindItemKind = (fkCommand, fkHandle, fkShortCut);

  TMenu = class(TComponent) //TWinControl)
  private
    FItems: TMenuItem;
    FParent: TComponent;
    procedure SetParent(const AValue: TComponent);
  protected
    procedure CreateHandle; virtual;
    function GetHandle: HMENU; virtual;
    procedure GetChildren(Proc: TGetChildProc; Root: TComponent); override;
  public
    FCompStyle: LongInt;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function FindItem(Value: Integer; Kind: TFindItemKind) : TMenuItem;
    function HandleAllocated: Boolean;
    Function IsRightToLeft: Boolean;
    procedure HandleNeeded;
    property Handle: HMenu read GetHandle;
    property Parent: TComponent read FParent write SetParent;
  published
    property Items: TMenuItem read FItems;
  end;

  TMainMenu = class(TMenu)
  public
    constructor Create(AOwner: TComponent); override;
  published
    property Items;
  end;

  TPopupMenu = class(TMenu)
  private
    FAutoPopup : Boolean;
    FPopupComponent : TComponent;
  public
    constructor Create(AOwner: TComponent); override;
    procedure PopUp(X, Y : Integer);
    property PopupComponent : TComponent read FPopupComponent write FPopupComponent;
  published
    property AutoPopup : Boolean read FAutoPopup write FAutoPopup default True;
  end;


  // will be removed
  TMenuBar = class(TComponent) //TWinControl)
  private
    //fMenu: TMenuItem;
    //fOwner : TControl;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Show; {override;}
    procedure AddMenu(Title: TCaption; Menu: TMenu);
  end;

function ShortCut(const Key: Word; const Shift : TShiftState) : TShortCut;
procedure ShortCuttoKey(const ShortCut : TShortCut; var Key: Word; var Shift : TShiftState);



implementation

uses
  Interfaces, LCLLinux;

{ Menu command managment }

var
  CommandPool: TBits;

function UniqueCommand: Word;
begin
  Result := CommandPool.OpenBit;
  CommandPool[Result] := True;
end;


{$I menubar.inc}
{$I menu.inc}
{$I menuitem.inc}
{$I mainmenu.inc}
{$I popupmenu.inc}

Function ShortCut(const Key: Word; const Shift : TShiftState) : TShortCut;
Begin
  Result := 0;
  if WordRec(Key).Hi <> 0 then exit;

  Result := Key;
  if ssShift in Shift then Inc(Result,scShift);
  if ssCtrl in Shift then Inc(Result,scCtrl);
  if ssAlt in Shift then Inc(Result,scAlt);
end;

Procedure ShortCuttoKey(const ShortCut : TShortCut; var Key: Word; var Shift : TShiftState);
begin
  key := ShortCut and not(scShift+scAlt+scCtrl);
  Shift := [];
  if ShortCut and scShift <> 0 then Include(shift,ssShift);
  if ShortCut and scAlt <> 0 then Include(shift,ssAlt);
  if ShortCut and scCtrl <> 0 then Include(shift,ssCtrl);

end;


end.

{
  $Log$
  Revision 1.14  2002/08/05 08:56:56  lazarus
  MG: TMenuItems can now be enabled and disabled

  Revision 1.13  2002/05/30 21:33:10  lazarus
  + added / fixed streaming functions for TMenu & TMenuItem, stoppok

  Revision 1.12  2002/05/19 08:28:50  lazarus
  + added helper functions to enable streaming of TMenu / TMenuItem
    stoppok

  Revision 1.11  2002/05/15 05:58:17  lazarus
  MG: added TMainMenu.Parent

  Revision 1.10  2002/05/10 06:05:50  lazarus
  MG: changed license to LGPL

  Revision 1.9  2002/05/09 12:41:28  lazarus
  MG: further clientrect bugfixes

  Revision 1.8  2002/03/25 17:59:19  lazarus
  GTK Cleanup
  Shane

  Revision 1.7  2002/02/20 23:33:24  lazarus
  MWE:
    + Published OnClick for TMenuItem
    + Published PopupMenu property for TEdit and TMemo (Doesn't work yet)
    * Fixed debugger running twice
    + Added Debugger output form
    * Enabled breakpoints

  Revision 1.6  2002/02/18 22:46:11  lazarus
  Implented TMenuItem.ShortCut (not much tested).

  Revision 1.5  2001/06/14 14:57:58  lazarus
  MG: small bugfixes and less notes

  Revision 1.4  2000/12/29 17:50:53  lazarus
  Added a dropdown image to the resource and a downarrow button by the OPEN button.
  Shane

  Revision 1.3  2000/12/22 19:55:37  lazarus
  Added the Popupmenu code to the LCL.
  Now you can right click on the editor and a PopupMenu appears.
  Shane

  Revision 1.2  2000/09/10 23:08:30  lazarus
  MWE:
    + Added CreateCompatibeleBitamp function
    + Updated TWinControl.WMPaint
    + Added some checks to avoid gtk/gdk errors
    - Removed no fixed warning from GetDC
    - Removed some output

  Revision 1.1  2000/07/13 10:28:24  michael
  + Initial import

  Revision 1.14  2000/02/22 21:51:40  lazarus
  MWE: Removed some double (or triple) event declarations.
       The latest compiler doesn't like it

  Revision 1.13  1999/12/20 21:37:12  lazarus
  Added ISRIGHTTOLEFT in menus file.
  Added ISACCEL in forms.pp
  Shane

  Revision 1.12  1999/12/14 22:21:11  lazarus
  *** empty log message ***

  Revision 1.11  1999/12/08 00:56:07  lazarus
  MWE:
    Fixed menus. Events aren't enabled yet (dumps --> invalid typecast ??)

  Revision 1.10  1999/11/05 00:34:11  lazarus
  MWE: Menu structure updated, events and visible code not added yet

  Revision 1.9  1999/11/02 16:02:34  lazarus
  Added a bunch of wndproc stuff and a lot of functions that really don't do a thing at this point.
  Shane

  Revision 1.8  1999/10/28 23:48:57  lazarus
  MWE: Added new menu classes and started to use handleneeded

}

