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
 *  See the file COPYING.modifiedLGPL, included in this distribution,        *
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
  
  TMenuItemHandlerType = (
    mihtDestroy
    );

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
    FBitmapIsValid: Boolean;
    FMenuItemHandlers: array[TMenuItemHandlerType] of TMethodList;
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
    procedure SetCaption(const AValue: TTranslateString);
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
    procedure SetImageIndex(value : Integer);
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
    function Find(const ACaption: string): TMenuItem;
    function GetImageList: TCustomImageList; virtual;
    function GetParentComponent: TComponent; override;
    function GetParentMenu: TMenu; virtual;
    function GetIsRightToLeft:Boolean; virtual;
    function HandleAllocated : Boolean;
    function HasIcon: boolean; virtual;
    function HasParent: Boolean; override;
    procedure InitiateAction; virtual;
    function IndexOf(Item: TMenuItem): Integer;
    function IndexOfCaption(const ACaption: string): Integer; virtual;
    function VisibleIndexOf(Item: TMenuItem): Integer;
    function IsCheckItem: boolean; virtual;
    function IsLine: Boolean;
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
    // Event lists
    procedure RemoveAllHandlersOfObject(AnObject: TObject); override;
    procedure AddHandlerOnDestroy(const OnDestroyEvent: TNotifyEvent;
                                  AsLast: boolean = false);
    procedure RemoveHandlerOnDestroy(const OnDestroyEvent: TNotifyEvent);
    procedure AddHandler(HandlerType: TMenuItemHandlerType;
                         const AMethod: TMethod; AsLast: boolean);
    procedure RemoveHandler(HandlerType: TMenuItemHandlerType;
                            const AMethod: TMethod);
  public
    property Count: Integer read GetCount;
    property Handle: HMenu read GetHandle write FHandle;
    property Items[Index: Integer]: TMenuItem read GetItem; default;
    property MenuIndex: Integer read GetMenuIndex write SetMenuIndex;
    property Parent: TMenuItem read GetParent;
    property Command: integer read FCommand;
    function MenuVisibleIndex: integer;
    procedure WriteDebugReport(const Prefix: string);
  published
    property Action: TBasicAction read GetAction write SetAction;
    property AutoCheck: boolean read FAutoCheck write SetAutoCheck default False;
    property Caption: TTranslateString read FCaption write SetCaption
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
    property Hint: TTranslateString read FHint write FHint stored IsHintStored;
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
  TMenuItemClass = class of TMenuItem;


  { TMenu }

  TFindItemKind = (fkCommand, fkHandle, fkShortCut);

  TMenu = class(TLCLComponent)
  private
    FBiDiMode: TBiDiMode;
    FImageChangeLink: TChangeLink;
    FImages: TCustomImageList;
    FItems: TMenuItem;
    FOnChange: TMenuChangeEvent;
    FParent: TComponent;
    FParentBiDiMode: Boolean;
    FShortcutHandled: boolean;
//See TCustomForm.CMBiDiModeChanged
    procedure CMParentBiDiModeChanged(var Message: TLMessage); message CM_PARENTBIDIMODECHANGED;
    function IsBiDiModeStored: Boolean;
    procedure ImageListChange(Sender: TObject);
    procedure SetBiDiMode(const AValue: TBiDiMode);
    procedure SetImages(const AValue: TCustomImageList);
    procedure SetParent(const AValue: TComponent);
    procedure SetParentBiDiMode(const AValue: Boolean);
  protected
    procedure BidiModeChanged; virtual;
    procedure CreateHandle; virtual;
    procedure DoChange(Source: TMenuItem; Rebuild: Boolean); virtual;
    function GetHandle: HMENU; virtual;
    procedure GetChildren(Proc: TGetChildProc; Root: TComponent); override;
    procedure MenuChanged(Sender: TObject; Source: TMenuItem;
                          Rebuild: Boolean); virtual;
    procedure ParentBidiModeChanged;
    procedure ParentBidiModeChanged(AOwner:TComponent);//used in Create constructor
    procedure SetChildOrder(Child: TComponent; Order: Integer); override;
    procedure UpdateItems;

    property OnChange: TMenuChangeEvent read FOnChange write FOnChange;
  public
    FCompStyle: LongInt;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure DestroyHandle; virtual;
    function FindItem(AValue: PtrInt; Kind: TFindItemKind) : TMenuItem;
    function IsShortcut(var Message: TLMKey): boolean;
    function HandleAllocated: Boolean;
    function IsRightToLeft: Boolean; virtual;
    procedure HandleNeeded;
    function DispatchCommand(ACommand: Word): Boolean;
  public
    property Handle: HMenu read GetHandle;
    property Parent: TComponent read FParent write SetParent;
    property ShortcutHandled: boolean read FShortcutHandled write FShortcutHandled;
  published
    property BidiMode:TBidiMode read FBidiMode write SetBidiMode stored IsBiDiModeStored default bdLeftToRight;
    property ParentBidiMode:Boolean read FParentBidiMode write SetParentBidiMode default True;
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
    property OnChange;
  end;


  { TPopupMenu }

  TPopupAlignment = (paLeft, paRight, paCenter);

  TPopupMenu = class(TMenu)
  private
    FAutoPopup : Boolean;
    FOnClose: TNotifyEvent;
    FOnPopup: TNotifyEvent;
    FPopupComponent : TComponent;
    FPopupPoint: TPoint;
  protected
    procedure DoPopup(Sender: TObject); virtual;
    procedure DoClose; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure PopUp(X, Y: Integer); virtual;
    property PopupComponent: TComponent read FPopupComponent
                                        write FPopupComponent;
    property PopupPoint: TPoint read FPopupPoint;
    procedure Close;
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

function NewMenu(Owner: TComponent; const AName: string;
                 const Items: array of TMenuItem): TMainMenu;
function NewPopupMenu(Owner: TComponent; const AName: string;
                      Alignment: TPopupAlignment; AutoPopup: Boolean;
                      const Items: array of TMenuItem): TPopupMenu;
function NewSubMenu(const ACaption: string; hCtx: THelpContext;
                    const AName: string; const Items: array of TMenuItem;
                    TheEnabled: Boolean = True): TMenuItem;
function NewItem(const ACaption: string; AShortCut: TShortCut;
                 AChecked, TheEnabled: Boolean; TheOnClick: TNotifyEvent;
                 hCtx: THelpContext; const AName: string): TMenuItem;
function NewLine: TMenuItem;


procedure Register;


const
  cHotkeyPrefix   = '&';
  cLineCaption    = '-';
  cDialogSuffix   = '...';


implementation

uses
  WSMenus,
  Forms {KeyDataToShiftState};

{ Menu command management }

var
  CommandPool: TBits;

function UniqueCommand: LongInt;
begin
  if CommandPool=nil then
    CommandPool:=TBits.Create(32);
  Result := CommandPool.OpenBit;
  CommandPool[Result] := True;
end;

{ Easy Menu building }

procedure AddMenuItems(AMenu: TMenu; const Items: array of TMenuItem);

  procedure SetOwner(Item: TMenuItem);
  var
    i: Integer;
  begin
    if Item.Owner=nil then
      AMenu.Owner.InsertComponent(Item);
    for i:=0 to Item.Count-1 do
      SetOwner(Item[i]);
  end;

var
  i: Integer;
begin
  for i:=Low(Items) to High(Items) do begin
    SetOwner(Items[i]);
    AMenu.FItems.Add(Items[i]);
  end;
end;

function NewMenu(Owner: TComponent; const AName: string;
  const Items: array of TMenuItem): TMainMenu;
begin
  Result:=TMainMenu.Create(Owner);
  Result.Name:=AName;
  AddMenuItems(Result,Items);
end;

function NewPopupMenu(Owner: TComponent; const AName: string;
  Alignment: TPopupAlignment; AutoPopup: Boolean;
  const Items: array of TMenuItem): TPopupMenu;
begin
  Result:=TPopupMenu.Create(Owner);
  Result.Name:=AName;
  Result.AutoPopup:=AutoPopup;
  if Alignment=paLeft then ;
  // TODO Result.Alignment:=Alignment;
  AddMenuItems(Result,Items);
end;

function NewSubMenu(const ACaption: string; hCtx: THelpContext;
  const AName: string; const Items: array of TMenuItem; TheEnabled: Boolean
  ): TMenuItem;
var
  i: Integer;
begin
  Result:=TMenuItem.Create(nil);
  for i:=Low(Items) to High(Items) do
    Result.Add(Items[i]);
  Result.Caption:=ACaption;
  Result.HelpContext:=hCtx;
  Result.Name:=AName;
  Result.Enabled:=TheEnabled;
end;

function NewItem(const ACaption: string; AShortCut: TShortCut; AChecked,
  TheEnabled: Boolean; TheOnClick: TNotifyEvent; hCtx: THelpContext;
  const AName: string): TMenuItem;
begin
  Result:=TMenuItem.Create(nil);
  with Result do begin
    Caption:=ACaption;
    ShortCut:=AShortCut;
    OnClick:=TheOnClick;
    HelpContext:=hCtx;
    Checked:=AChecked;
    Enabled:=TheEnabled;
    Name:=AName;
  end;
end;

function NewLine: TMenuItem;
begin
  Result := TMenuItem.Create(nil);
  Result.Caption := '-';
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
  Result := LCLType.KeyToShortCut(Key,Shift);
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
