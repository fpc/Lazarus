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
  Classes, SysUtils, LCLStrConsts, LCLType, LCLProc, LCLIntf, InterfaceBase,
  LMessages, ActnList, Graphics, ImgList, LCLClasses;


type
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

  TMenuItem = class(TLCLComponent)
  private
    FActionLink: TMenuActionLink;
    FAutoCheck: boolean;
    FCaption: string;
    FChecked: Boolean;
    FCommand: integer;
    FDefault: Boolean;
    FEnabled: Boolean;
    FBitmap: TBitmap;
    FGroupIndex: Byte;
    FHandle: HMenu;
    FHelpContext: THelpContext;
    FHint: String;
    FImageChangeLink: TChangeLink;
    FImageIndex : Integer;
    FItems: TList; // list of TMenuItem
    FMenu: TMenu;
    FOnChange: TMenuChangeEvent;
    FOnClick: TNotifyEvent;
    FParent: TMenuItem;
    FRadioItem: Boolean;
    FRightJustify: boolean;
    FShortCut: TShortCut;
    FShowAlwaysCheckable: boolean;
    FSubMenuImages: TCustomImageList;
    FVisible: Boolean;
    function GetBitmap: TBitmap;
    function GetCount: Integer;
    function GetItem(Index: Integer): TMenuItem;
    function GetMenuIndex: Integer;
    function GetParent: TMenuItem;
    function IsBitmapStored: boolean;
    function IsCaptionStored: boolean;
    function IsCheckedStored: boolean;
    function IsEnabledStored: boolean;
    function IsHelpContextStored: boolean;
    function IsHintStored: Boolean;
    function IsImageIndexStored: Boolean;
    function IsOnClickStored: Boolean;
    function IsShortCutStored: boolean;
    function IsVisibleStored: boolean;
    procedure SetAutoCheck(const AValue: boolean);
    procedure SetCaption(const AValue: string);
    procedure SetChecked(AValue: Boolean);
    procedure SetDefault(AValue: Boolean);
    procedure SetEnabled(AValue: Boolean);
    procedure SetBitmap(const AValue: TBitmap);
    procedure SetMenuIndex(AValue: Integer);
    procedure SetRadioItem(const AValue: Boolean);
    procedure SetRightJustify(const AValue: boolean);
    procedure SetShowAlwaysCheckable(const AValue: boolean);
    procedure SetSubMenuImages(const AValue: TCustomImageList);
    procedure ShortcutChanged(const OldValue, Value : TShortcut);
    procedure SubItemChanged(Sender: TObject; Source: TMenuItem;
                             Rebuild: Boolean);
    procedure TurnSiblingsOff;
    procedure DoActionChange(Sender: TObject);
  protected
    procedure ActionChange(Sender: TObject; CheckDefaults: Boolean); dynamic;
    procedure AssignTo(Dest: TPersistent); override;
    function GetAction: TBasicAction;
    function GetActionLinkClass: TMenuActionLinkClass; dynamic;
    function GetHandle: HMenu;
    procedure DoClicked(var msg); message LM_ACTIVATE;
    procedure CreateHandle; virtual;
    procedure DestroyHandle; virtual;
    procedure Loaded; override;
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
    procedure GetChildren(Proc: TGetChildProc; Root: TComponent); override;
    procedure InitiateActions;
    procedure MenuChanged(Rebuild : Boolean);
    procedure SetAction(NewAction: TBasicAction);
    procedure SetChildOrder(Child: TComponent; Order: Integer); override;
    procedure SetGroupIndex(AValue: Byte);
    Procedure SetImageIndex(value : Integer);
    procedure SetParentComponent(AValue : TComponent); override;
    procedure SetShortCut(const AValue : TShortCut);
    procedure SetVisible(AValue: Boolean);
    procedure UpdateImages;
    procedure ImageListChange(Sender: TObject);
  protected
    property ActionLink: TMenuActionLink read FActionLink write FActionLink;
  public
    FCompStyle: LongInt;
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    function GetImageList: TCustomImageList; virtual;
    function GetParentComponent: TComponent; override;
    function GetParentMenu: TMenu; virtual;
    function HandleAllocated : Boolean;
    function HasIcon: boolean; virtual;
    function HasParent: Boolean; override;
    procedure InitiateAction; virtual;
    function IndexOf(Item: TMenuItem): Integer;
    function IndexOfCaption(const ACaption: string): Integer; virtual;
    function VisibleIndexOf(Item: TMenuItem): Integer;
    function IsCheckItem: boolean; virtual;
    procedure Add(Item: TMenuItem);
    procedure AddSeparator;
    procedure Click; virtual;
    procedure Delete(Index: Integer);
    procedure HandleNeeded; virtual;
    procedure Insert(Index: Integer; Item: TMenuItem);
    procedure RecreateHandle; virtual;
    procedure Remove(Item: TMenuItem);
    function IsInMenuBar: boolean; virtual;
    procedure Clear;
    function HasBitmap: boolean;
    function GetIconSize: TPoint; virtual;
  public
    property Count: Integer read GetCount;
    property Handle: HMenu read GetHandle write FHandle;
    property Items[Index: Integer]: TMenuItem read GetItem; default;
    property MenuIndex: Integer read GetMenuIndex write SetMenuIndex;
    property Parent: TMenuItem read GetParent;
    property Command: integer read FCommand;
    function MenuVisibleIndex: integer;
  published
    property Action: TBasicAction read GetAction write SetAction;
    property AutoCheck: boolean read FAutoCheck write SetAutoCheck default False;
    property Caption: String read FCaption write SetCaption
                             stored IsCaptionStored;
    property Checked: Boolean read FChecked write SetChecked
                              stored IsCheckedStored default False;
    property Default: Boolean read FDefault write SetDefault default False;
    property Enabled: Boolean read FEnabled write SetEnabled
                              stored IsEnabledStored default True;
    property Bitmap: TBitmap read GetBitmap write SetBitmap stored IsBitmapStored;
    property GroupIndex: Byte read FGroupIndex write SetGroupIndex default 0;
    property HelpContext: THelpContext read FHelpContext write FHelpContext
                                           stored IsHelpContextStored default 0;
    property Hint: String read FHint write FHint stored IsHintStored;
    property ImageIndex: Integer read FImageIndex write SetImageIndex
                                           stored IsImageIndexStored default -1;
    property RadioItem: Boolean read FRadioItem write SetRadioItem
                                default False;
    property RightJustify: boolean read FRightJustify write SetRightJustify;
    property ShortCut: TShortCut read FShortCut write SetShortCut
                                 stored IsShortCutStored default 0;
    property ShowAlwaysCheckable: boolean read FShowAlwaysCheckable
                                          write SetShowAlwaysCheckable;
    property SubMenuImages: TCustomImageList read FSubMenuImages
                                             write SetSubMenuImages;
    property Visible: Boolean read FVisible write SetVisible
                              stored IsVisibleStored default True;
    property OnClick: TNotifyEvent read FOnClick write FOnClick
                                                         stored IsOnClickStored;
  end;


  { TMenu }

  TFindItemKind = (fkCommand, fkHandle, fkShortCut);

  TMenu = class(TLCLComponent)
  private
    FImageChangeLink: TChangeLink;
    FImages: TCustomImageList;
    FItems: TMenuItem;
    FOnChange: TMenuChangeEvent;
    FParent: TComponent;
    procedure SetImages(const AValue: TCustomImageList);
    procedure SetParent(const AValue: TComponent);
    procedure ImageListChange(Sender: TObject);
  protected
    procedure CreateHandle; virtual;
    procedure DoChange(Source: TMenuItem; Rebuild: Boolean); virtual;
    function GetHandle: HMENU; virtual;
    procedure GetChildren(Proc: TGetChildProc; Root: TComponent); override;
    procedure MenuChanged(Sender: TObject; Source: TMenuItem;
                          Rebuild: Boolean); virtual;
    property OnChange: TMenuChangeEvent read FOnChange write FOnChange;
    procedure SetChildOrder(Child: TComponent; Order: Integer); override;
    procedure UpdateItems;
  public
    FCompStyle: LongInt;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure DestroyHandle; virtual;
    function FindItem(AValue: Integer; Kind: TFindItemKind) : TMenuItem;
    function IsShortcut(var Message: TLMKey): boolean;
    function HandleAllocated: Boolean;
    Function IsRightToLeft: Boolean;
    procedure HandleNeeded;
    function DispatchCommand(ACommand: Word): Boolean;
  public
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
    property OnChange;
  end;


  { TPopupMenu }

  TPopupMenu = class(TMenu)
  private
    FAutoPopup : Boolean;
    FOnClose: TNotifyEvent;
    FOnPopup: TNotifyEvent;
    FPopupComponent : TComponent;
    FPopupPoint: TPoint;
  protected
    procedure DoPopup(Sender: TObject); virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure PopUp(X, Y: Integer);
    property PopupComponent: TComponent read FPopupComponent
                                        write FPopupComponent;
    property PopupPoint: TPoint read FPopupPoint;
    procedure Close;
    procedure DoClose;
  published
    property AutoPopup: Boolean read FAutoPopup write FAutoPopup default True;
    property OnPopup: TNotifyEvent read FOnPopup write FOnPopup;
    property OnClose: TNotifyEvent read FOnClose write FOnClose;
  end;

function ShortCut(const Key: Word; const Shift : TShiftState) : TShortCut;
procedure ShortCutToKey(const ShortCut : TShortCut; var Key: Word;
                        var Shift : TShiftState);

var
  DesignerMenuItemClick: TNotifyEvent;
  ActivePopupMenu: TPopupMenu;
  OnMenuPopupHandler: TNotifyEvent;

procedure Register;

implementation

uses
  WSMenus,
  Forms {KeyDataToShiftState};

{ Menu command managment }

var
  CommandPool: TBits;

function UniqueCommand: LongInt;
begin
  if CommandPool=nil then
    CommandPool:=TBits.Create(32);
  Result := CommandPool.OpenBit;
  CommandPool[Result] := True;
end;

procedure Register;
begin
  RegisterComponents('Standard',[TMainMenu,TPopupMenu]);
  RegisterNoIcon([TMenuItem]);
end;

{$I menu.inc}
{$I menuitem.inc}
{$I mainmenu.inc}
{$I popupmenu.inc}
{$I menuactionlink.inc}

function ShortCut(const Key: Word; const Shift : TShiftState) : TShortCut;
begin
  Result := Key;
  if WordRec(Result).Hi <> 0 then begin
    Result:=0;
    exit;
  end;

  if ssShift in Shift then Inc(Result,scShift);
  if ssCtrl in Shift then Inc(Result,scCtrl);
  if ssAlt in Shift then Inc(Result,scAlt);
end;

procedure ShortCutToKey(const ShortCut: TShortCut; var Key: Word;
  var Shift : TShiftState);
begin
  Key := ShortCut and $FF;
  Shift := [];
  if ShortCut and scShift <> 0 then Include(Shift,ssShift);
  if ShortCut and scAlt <> 0 then Include(Shift,ssAlt);
  if ShortCut and scCtrl <> 0 then Include(Shift,ssCtrl);
end;


initialization
  DesignerMenuItemClick:=nil;
  ActivePopupMenu:=nil;
  CommandPool := nil;
  OnMenuPopupHandler := nil;

finalization
  FreeThenNil(CommandPool);

end.

{
  $Log$
  Revision 1.78  2005/03/07 00:52:51  mattias
  various Delphi compatibilities  from C Western

  Revision 1.77  2005/02/03 15:10:23  micha
  implement shortcut handling, tcustomlabel accelerator focuscontrol functionality

  Revision 1.76  2004/12/12 03:54:09  mattias
  implemented open project after open standard windows

  Revision 1.75  2004/12/10 21:36:27  mattias
  implemented TMenuItem.SetVisible

  Revision 1.74  2004/11/10 20:53:18  vincents
  Destroy menu handle, when destroying form handle.

  Revision 1.73  2004/10/24 14:50:31  micha
  fix menuitem caption issue (partly by martin smat)
  remove ShortCutToText and TextToShortCut wrapper functions

  Revision 1.72  2004/09/17 10:56:24  micha
  convert LM_SHORTCUT message to interface methods

  Revision 1.71  2004/09/10 09:43:12  micha
  convert LM_SETLABEL message to interface methods

  Revision 1.70  2004/08/18 09:31:21  mattias
  removed obsolete unit vclglobals

  Revision 1.69  2004/08/13 10:20:19  mattias
  fixed codetools ConstSet, implemented notifying TApplication whenmenu popups

  Revision 1.68  2004/06/17 21:24:19  mattias
  implemented painting menuitem icons from ImageList

  Revision 1.67  2004/06/17 20:52:18  mattias
  fixed setting ImageIndex when TMenuItem.ActionChange

  Revision 1.66  2004/03/17 00:34:37  marc
  * Interface reconstruction. Created skeleton units, classes and wscontrols

  Revision 1.65  2004/02/23 08:19:04  micha
  revert intf split

  Revision 1.63  2004/02/08 11:31:32  mattias
  TMenuItem.Bitmap is now auto created on read. Added TMenuItem.HasBitmap

  Revision 1.62  2004/02/05 09:45:33  mattias
  implemented Actions for TSpeedButton, TMenuItem, TCheckBox

  Revision 1.61  2004/02/04 17:06:26  mattias
  fixed updating menu designer caption when editing in OI

  Revision 1.60  2004/02/04 13:40:19  mattias
  ShortCutToText now deletes any modifier

  Revision 1.59  2004/02/02 18:09:41  mattias
  added TMenuItem.Action

  Revision 1.58  2004/01/10 18:09:38  mattias
  implemented TMenuItem.Clear

  Revision 1.57  2003/11/27 23:02:30  mattias
  removed menutype.pas

  Revision 1.56  2003/11/26 21:30:19  mattias
  reduced unit circles, fixed fpImage streaming

  Revision 1.55  2003/11/16 01:56:15  mattias
  changed TMenuItem.Graphic to TMenuItem.Bitmap

  Revision 1.54  2003/11/10 16:15:31  micha
  cleanups; win32 fpimage support

  Revision 1.53  2003/10/26 17:34:41  micha
  new interface method to attach a menu to window

  Revision 1.52  2003/10/16 23:54:27  marc
  Implemented new gtk keyevent handling

  Revision 1.51  2003/09/18 09:21:03  mattias
  renamed LCLLinux to LCLIntf

  Revision 1.50  2003/07/21 23:43:32  marc
  * Fixed radiogroup menuitems

  Revision 1.49  2003/06/26 17:00:00  mattias
  fixed result on searching proc in interface

  Revision 1.48  2003/06/24 15:57:55  mattias
  applied win32 menu patch from Micha Nelissen

  Revision 1.47  2003/06/24 15:23:10  mattias
  deleted unused code

  Revision 1.46  2003/06/23 12:33:55  mattias
  implemented TPairSplitter streaming

  Revision 1.45  2003/06/23 09:42:09  mattias
  fixes for debugging lazarus

  Revision 1.44  2003/06/09 09:20:27  mattias
  removed menubar.inc

  Revision 1.43  2003/05/12 13:40:50  mattias
  fixed clsing popupmenu on showmodal

  Revision 1.42  2003/05/02 22:22:15  mattias
  localization, added context policy to make resource string dialog

  Revision 1.41  2003/04/19 18:37:58  mattias
  implemented adding OnClick, when clicking on designer menuitem

  Revision 1.40  2003/04/04 16:35:24  mattias
  started package registration

  Revision 1.39  2003/03/16 09:41:05  mattias
  fixed checking menuitems

  Revision 1.38  2003/03/11 07:46:43  mattias
  more localization for gtk- and win32-interface and lcl

  Revision 1.37  2003/02/23 10:42:06  mattias
  implemented changing TMenuItem.GroupIndex at runtime

  Revision 1.36  2003/01/04 11:58:32  mattias
  added Windows menu to IDE

  Revision 1.35  2002/12/02 16:38:13  mattias
  started position highlighter

  Revision 1.34  2002/11/12 10:16:14  lazarus
  MG: fixed TMainMenu creation

  Revision 1.33  2002/10/26 15:15:47  lazarus
  MG: broke LCL<->interface circles

  Revision 1.32  2002/10/26 11:20:30  lazarus
  MG: broke some interfaces.pp circles

  Revision 1.31  2002/10/26 10:21:01  lazarus
  MG: broke actnlist <-> menus circle

  Revision 1.30  2002/10/24 09:37:39  lazarus
  MG: broke menus.pp <-> controls.pp circle

  Revision 1.29  2002/10/20 21:49:09  lazarus
  MG: fixes for fpc1.1

  Revision 1.28  2002/10/08 22:32:26  lazarus
  MG: fixed cool little bug (menu double attaching bug)

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

