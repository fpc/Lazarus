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
  Classes, SysUtils, LCLLinux, LCLType, VCLGlobals, LMessages,
  ActnList, Graphics, ImgList;


type
  TShortCut = Low(Word)..High(Word);   {should be moved to classes}

  TMenu = class;
  EMenuError = class(Exception);

  TMenuItem = class;
  
  TMenuChangeEvent = procedure (Sender: TObject; Source: TMenuItem;
                                Rebuild: Boolean) of object;

  { TMenuActionLink }

  TMenuActionLink = class(TActionLink)
  protected
    FClient: TMenuItem;
    procedure AssignClient(AClient: TObject); override;
    function IsAutoCheckLinked: Boolean; virtual;
    function IsCaptionLinked: Boolean; override;
    function IsCheckedLinked: Boolean; override;
    function IsEnabledLinked: Boolean; override;
    function IsHelpContextLinked: Boolean; override;
    function IsHintLinked: Boolean; override;
    function IsGroupIndexLinked: Boolean; override;
    function IsImageIndexLinked: Boolean; override;
    function IsShortCutLinked: Boolean; override;
    function IsVisibleLinked: Boolean; override;
    function IsOnExecuteLinked: Boolean; override;
    procedure SetAutoCheck(Value: Boolean); override;
    procedure SetCaption(const Value: string); override;
    procedure SetChecked(Value: Boolean); override;
    procedure SetEnabled(Value: Boolean); override;
    procedure SetHelpContext(Value: THelpContext); override;
    procedure SetHint(const Value: string); override;
    procedure SetImageIndex(Value: Integer); override;
    procedure SetShortCut(Value: TShortCut); override;
    procedure SetVisible(Value: Boolean); override;
    procedure SetOnExecute(Value: TNotifyEvent); override;
  end;

  TMenuActionLinkClass = class of TMenuActionLink;
  
  
  { TMenuItem }

  TMenuItem = class(TComponent)//TWinControl)
  private
    FActionLink: TMenuActionLink;
    FAutoCheck: boolean;
    FCaption: string;
    FChecked: Boolean;
    FCommand: integer;
    FDefault: Boolean;
    FEnabled: Boolean;
    FGraphic: TGraphic;
    FGroupIndex: Byte;
    FHandle: HMenu;
    FHint : String;
    FImageIndex : Integer;
    FItems: TList; // list of TMenuItem
    FMenu: TMenu;
    FParent: TMenuItem;
    FRadioItem: Boolean;
    FRightJustify: boolean;
    FShortCut: TShortCut;
    FShowAlwaysCheckable: boolean;
    FSubMenuImages: TCustomImageList;
    FVisible: Boolean;
    FOnChange: TMenuChangeEvent;
    FOnClick: TNotifyEvent;
    function GetCount: Integer;
    function GetItem(Index: Integer): TMenuItem;
    function GetMenuIndex: Integer;
    function GetParent: TMenuItem;
    function IsCaptionStored: boolean;
    function IsCheckedStored: boolean;
    function IsEnabledStored: boolean;
    function IsShortCutStored: boolean;
    function IsVisibleStored: boolean;
    procedure SetAutoCheck(const AValue: boolean);
    procedure SetCaption(const AValue: string);
    procedure SetChecked(AValue: Boolean);
    procedure SetDefault(AValue: Boolean);
    procedure SetEnabled(AValue: Boolean);
    procedure SetGraphic(const AValue: TGraphic);
    procedure SetMenuIndex(AValue: Integer);
    procedure SetRadioItem(const AValue: Boolean);
    procedure SetRightJustify(const AValue: boolean);
    procedure SetShowAlwaysCheckable(const AValue: boolean);
    procedure SetSubMenuImages(const AValue: TCustomImageList);
    procedure ShortcutChanged(const OldValue, Value : TShortcut);
    procedure SubItemChanged(Sender: TObject; Source: TMenuItem;
                             Rebuild: Boolean);
    procedure TurnSiblingsOff;
    procedure VerifyGroupIndex(Position: Integer; Value: Byte);
  protected
    property ActionLink: TMenuActionLink read FActionLink write FActionLink;
    procedure CreateHandle; virtual;
    procedure DestroyHandle; virtual;
    procedure DoClicked(var msg); message LM_ACTIVATE;  //'activate';
    function GetHandle: HMenu;
    Procedure SetImageIndex(value : Integer);
    procedure SetGroupIndex(AValue: Byte);
    procedure SetShortCut(AValue : TShortCut);
    procedure SetVisible(AValue: Boolean);
    procedure MenuChanged(Rebuild : Boolean);
    procedure SetChildOrder(Child: TComponent; Order: Integer); override;
    procedure SetParentComponent(AValue : TComponent); override;
    procedure GetChildren(Proc: TGetChildProc; Root: TComponent); override;
  public
    FCompStyle : LongInt;
    procedure Add(Item: TMenuItem);
    constructor Create(AOwner: TComponent); override;
    procedure Delete(Index: Integer);
    destructor Destroy; override;
    function GetImageList: TCustomImageList; virtual;
    function GetParentComponent: TComponent; override;
    function GetParentMenu: TMenu; virtual;
    function HandleAllocated : Boolean;
    procedure HandleNeeded; virtual;
    function HasIcon: boolean; virtual;
    function HasParent : Boolean; override;
    function IndexOf(Item: TMenuItem): Integer;
    function IndexOfCaption(const ACaption: string): Integer; virtual;
    procedure Insert(Index: Integer; Item: TMenuItem);
    procedure RecreateHandle; virtual;
    procedure Remove(Item: TMenuItem);
    property Count: Integer read GetCount;
    property Handle: HMenu read GetHandle write FHandle;
    property Items[Index: Integer]: TMenuItem read GetItem; default;
    property MenuIndex: Integer read GetMenuIndex write SetMenuIndex;
    property Parent: TMenuItem read GetParent;
    function IsCheckItem: boolean; virtual;
  published
    property AutoCheck: boolean read FAutoCheck write SetAutoCheck default False;
    property Caption: String
      read FCaption write SetCaption stored IsCaptionStored;
    property Checked: Boolean
      read FChecked write SetChecked stored IsCheckedStored default False;
    property Default: Boolean read FDefault write SetDefault default False;
    property Enabled: Boolean
      read FEnabled write SetEnabled stored IsEnabledStored default True;
    property Graphic: TGraphic read FGraphic write SetGraphic;
    property GroupIndex: Byte read FGroupIndex write SetGroupIndex default 0;
    property Hint : String read FHint write FHint;
    property ImageIndex : Integer read FImageIndex write SetImageIndex;
    property RadioItem: Boolean
      read FRadioItem write SetRadioItem default False;
    property RightJustify: boolean read FRightJustify write SetRightJustify;
    property ShortCut: TShortCut
      read FShortCut write SetShortCut stored IsShortCutStored default 0;
    property ShowAlwaysCheckable: boolean
      read FShowAlwaysCheckable write SetShowAlwaysCheckable;
    property SubMenuImages: TCustomImageList
      read FSubMenuImages write SetSubMenuImages;
    property Visible: Boolean
      read FVisible write SetVisible stored IsVisibleStored default True;
    property OnClick: TNotifyEvent read FOnClick write FOnclick; 
  end;

  TFindItemKind = (fkCommand, fkHandle, fkShortCut);


  { TMenu }

  TMenu = class(TComponent) //TWinControl)
  private
    FImages: TCustomImageList;
    FItems: TMenuItem;
    FParent: TComponent;
    procedure SetImages(const AValue: TCustomImageList);
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
    property Images: TCustomImageList read FImages write SetImages;
  end;


  { TMainMenu }

  TMainMenu = class(TMenu)
  protected
    procedure ItemChanged;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property Items;
  end;
  
  
  { TPopupMenu }

  TPopupMenu = class(TMenu)
  private
    FAutoPopup : Boolean;
    FOnPopup: TNotifyEvent;
    FPopupComponent : TComponent;
    FPopupPoint: TPoint;
  protected
    procedure DoPopup(Sender: TObject); virtual;
  public
    constructor Create(AOwner: TComponent); override;
    procedure PopUp(X, Y : Integer);
    property PopupComponent : TComponent
      read FPopupComponent write FPopupComponent;
    property PopupPoint: TPoint read FPopupPoint;
  published
    property AutoPopup : Boolean read FAutoPopup write FAutoPopup default True;
    property OnPopup: TNotifyEvent read FOnPopup write FOnPopup;
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
procedure ShortCutToKey(const ShortCut : TShortCut; var Key: Word;
                        var Shift : TShiftState);

function TextToShortCut(Text: string): TShortCut;
function ShortCutToText(ShortCut: TShortCut): string;


implementation

uses
  Interfaces, Controls, Forms;

{ Menu command managment }

var
  CommandPool: TBits;

function UniqueCommand: Word;
begin
  Result := CommandPool.OpenBit;
  CommandPool[Result] := True;
end;

type
  TMenuKeyCap = (mkcBkSp, mkcTab, mkcEsc, mkcEnter, mkcSpace, mkcPgUp,
    mkcPgDn, mkcEnd, mkcHome, mkcLeft, mkcUp, mkcRight, mkcDown, mkcIns,
    mkcDel, mkcShift, mkcCtrl, mkcAlt);

const
  SmkcBkSp = 'BkSp';
  SmkcTab = 'Tab';
  SmkcEsc = 'Esc';
  SmkcEnter = 'Enter';
  SmkcSpace = 'Space';
  SmkcPgUp = 'PgUp';
  SmkcPgDn = 'PgDn';
  SmkcEnd = 'End';
  SmkcHome = 'Home';
  SmkcLeft = 'Left';
  SmkcUp = 'Up';
  SmkcRight = 'Right';
  SmkcDown = 'Down';
  SmkcIns = 'Ins';
  SmkcDel = 'Del';
  SmkcShift = 'Shift+';
  SmkcCtrl = 'Ctrl+';
  SmkcAlt = 'Alt+';

  MenuKeyCaps: array[TMenuKeyCap] of string = (
    SmkcBkSp, SmkcTab, SmkcEsc, SmkcEnter, SmkcSpace, SmkcPgUp,
    SmkcPgDn, SmkcEnd, SmkcHome, SmkcLeft, SmkcUp, SmkcRight,
    SmkcDown, SmkcIns, SmkcDel, SmkcShift, SmkcCtrl, SmkcAlt);

function GetSpecialName(ShortCut: TShortCut): string;
{var
  ScanCode: Integer;
  KeyName: array[0..255] of Char;}
begin
  Result := '';
  // ToDo:
  {
  ScanCode := MapVirtualKey(WordRec(ShortCut).Lo, 0) shl 16;
  if ScanCode <> 0 then
  begin
    GetKeyNameText(ScanCode, KeyName, SizeOf(KeyName));
    GetSpecialName := KeyName;
  end;
  }
end;

function ShortCutToText(ShortCut: TShortCut): string;
var
  Name: string;
begin
  case WordRec(ShortCut).Lo of
    $08, $09:
      Name := MenuKeyCaps[TMenuKeyCap(Ord(mkcBkSp) + WordRec(ShortCut).Lo - $08)];
    $0D: Name := MenuKeyCaps[mkcEnter];
    $1B: Name := MenuKeyCaps[mkcEsc];
    $20..$28:
      Name := MenuKeyCaps[TMenuKeyCap(Ord(mkcSpace) + WordRec(ShortCut).Lo - $20)];
    $2D..$2E:
      Name := MenuKeyCaps[TMenuKeyCap(Ord(mkcIns) + WordRec(ShortCut).Lo - $2D)];
    $30..$39: Name := Chr(WordRec(ShortCut).Lo - $30 + Ord('0'));
    $41..$5A: Name := Chr(WordRec(ShortCut).Lo - $41 + Ord('A'));
    $60..$69: Name := Chr(WordRec(ShortCut).Lo - $60 + Ord('0'));
    $70..$87: Name := 'F' + IntToStr(WordRec(ShortCut).Lo - $6F);
  else
    Name := GetSpecialName(ShortCut);
  end;
  if Name <> '' then
  begin
    Result := '';
    if ShortCut and scShift <> 0 then Result := Result + MenuKeyCaps[mkcShift];
    if ShortCut and scCtrl <> 0 then Result := Result + MenuKeyCaps[mkcCtrl];
    if ShortCut and scAlt <> 0 then Result := Result + MenuKeyCaps[mkcAlt];
    Result := Result + Name;
  end
  else Result := '';
end;

function TextToShortCut(Text: string): TShortCut;

  { If the front of Text is equal to Front then remove the matching piece
    from Text and return True, otherwise return False }

  function CompareFront(var Text: string; const Front: string): Boolean;
  begin
    Result := False;
    if (Length(Text) >= Length(Front)) and
      (AnsiStrLIComp(PChar(Text), PChar(Front), Length(Front)) = 0) then
    begin
      Result := True;
      Delete(Text, 1, Length(Front));
    end;
  end;

var
  Key: TShortCut;
  Shift: TShortCut;
begin
  Result := 0;
  Shift := 0;
  while True do
  begin
    if CompareFront(Text, MenuKeyCaps[mkcShift]) then Shift := Shift or scShift
    else if CompareFront(Text, '^') then Shift := Shift or scCtrl
    else if CompareFront(Text, MenuKeyCaps[mkcCtrl]) then Shift := Shift or scCtrl
    else if CompareFront(Text, MenuKeyCaps[mkcAlt]) then Shift := Shift or scAlt
    else Break;
  end;
  if Text = '' then Exit;
  for Key := $08 to $255 do { Copy range from table in ShortCutToText }
    if AnsiCompareText(Text, ShortCutToText(Key)) = 0 then
    begin
      Result := Key or Shift;
      Exit;
    end;
end;

{$I menubar.inc}
{$I menu.inc}
{$I menuitem.inc}
{$I mainmenu.inc}
{$I popupmenu.inc}
{$I menuactionlink.inc}

Function ShortCut(const Key: Word; const Shift : TShiftState) : TShortCut;
Begin
  Result := MapIrregularVirtualKey(Key);
  if WordRec(Result).Hi <> 0 then begin
    Result:=0;
    exit;
  end;

  if ssShift in Shift then Inc(Result,scShift);
  if ssCtrl in Shift then Inc(Result,scCtrl);
  if ssAlt in Shift then Inc(Result,scAlt);
end;

Procedure ShortCutToKey(const ShortCut : TShortCut; var Key: Word;
  var Shift : TShiftState);
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
  Revision 1.27  2002/09/19 16:45:54  lazarus
  MG: fixed Menu.Free and gdkwindow=nil bug

  Revision 1.26  2002/08/22 13:45:57  lazarus
  MG: fixed non AutoCheck menuitems and editor bookmark popupmenu

  Revision 1.25  2002/08/17 07:57:05  lazarus
  MG: added TPopupMenu.OnPopup and SourceEditor PopupMenu checks

  Revision 1.24  2002/08/16 20:13:09  lazarus
  MG: custom external tools are now shown in the menu

  Revision 1.23  2002/08/15 13:37:57  lazarus
  MG: started menuitem icon, checked, radio and groupindex

  Revision 1.22  2002/08/12 15:32:28  lazarus
  MG: started enhanced menuitem

  Revision 1.21  2002/08/08 17:26:37  lazarus
  MG: added property TMenuItems.RightJustify

  Revision 1.20  2002/08/08 10:33:49  lazarus
  MG: main bar speedbar open arrow now shows recent projects and files

  Revision 1.19  2002/08/08 09:07:06  lazarus
  MG: TMenuItem can now be created/destroyed/moved at any time

  Revision 1.18  2002/08/07 09:55:30  lazarus
  MG: codecompletion now checks for filebreaks, savefile now checks for filedate

  Revision 1.17  2002/08/06 20:05:38  lazarus
  MG: added stored funcitons

  Revision 1.16  2002/08/06 19:57:39  lazarus
  MG: added actnlist.pp

  Revision 1.15  2002/08/05 10:45:02  lazarus
  MG: TMenuItem.Caption can now be set after creation

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

