{
 /***************************************************************************
                                     menus.pp
                                     --------
                   Component Library TMenu, TMenuItem Controls
                   Initial Revision  : Mon Jul 26 0:10:12 1999


 ***************************************************************************/

 *****************************************************************************
  This file is part of the Lazarus Component Library (LCL)

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************
}

{
TMenu, TMenuItem
@author(TMenu - Shane Miller <smiller@lakefield.net>)
@author(TMenuItem - Shane Miller <smiller@lakefield.net>)
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
  Types, Classes, SysUtils,
  // LCL
  LCLStrConsts, LCLType, LCLProc, LCLIntf, LCLClasses, LResources, LMessages,
  ActnList, Graphics, ImgList, Themes,
  // LazUtils
  LazMethodList, LazLoggerBase;

type
  TMenu = class;
  TMenuItem = class;
  EMenuError = class(Exception);

  TGlyphShowMode = (
    gsmAlways,       // always show
    gsmNever,        // never show
    gsmApplication,  // depends on application settings
    gsmSystem        // depends on system settings
  );

  TMenuChangeEvent = procedure (Sender: TObject; Source: TMenuItem;
                                Rebuild: Boolean) of object;

  { TMenuActionLink }

  TMenuActionLink = class(TActionLink)
  protected
    FClient: TMenuItem;
    procedure AssignClient(AClient: TObject); override;
    function IsAutoCheckLinked: Boolean; virtual;
  protected
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
  public
    function IsCaptionLinked: Boolean; override;
    function IsCheckedLinked: Boolean; override;
    function IsEnabledLinked: Boolean; override;
    function IsHelpContextLinked: Boolean; override;
    function IsHintLinked: Boolean; override;
    function IsGroupIndexLinked: Boolean; override;
    function IsImageIndexLinked: Boolean; override;
    function IsShortCutLinked: Boolean; override;
    function IsVisibleLinked: Boolean; override;
  end;

  TMenuActionLinkClass = class of TMenuActionLink;

  { TMenuItemEnumerator }

  TMenuItemEnumerator = class
  private
    FMenuItem: TMenuItem;
    FPosition: Integer;
    function GetCurrent: TMenuItem;
  public
    constructor Create(AMenuItem: TMenuItem);
    function MoveNext: Boolean;
    property Current: TMenuItem read GetCurrent;
  end;

  { TMenuItem }
  
  TMenuItemHandlerType = (
    mihtDestroy
    );

  TMenuDrawItemEvent = procedure(Sender: TObject; ACanvas: TCanvas;
    ARect: TRect; AState: TOwnerDrawState) of object;
  TMenuMeasureItemEvent = procedure(Sender: TObject; ACanvas: TCanvas;
    var AWidth, AHeight: Integer) of object;

  TMergedMenuItems = class
  private
    fList: array[Boolean] of TList; // visible

    function GetInvisibleCount: Integer;
    function GetInvisibleItem(Index: Integer): TMenuItem;
    function GetVisibleCount: Integer;
    function GetVisibleItem(Index: Integer): TMenuItem;
  public
    constructor Create(const aParent: TMenuItem);
    destructor Destroy; override;
    class function DefaultSort(aItem1, aItem2, aParentItem: Pointer): Integer; static;
    property VisibleCount: Integer read GetVisibleCount;
    property VisibleItems[Index: Integer]: TMenuItem read GetVisibleItem;
    property InvisibleCount: Integer read GetInvisibleCount;
    property InvisibleItems[Index: Integer]: TMenuItem read GetInvisibleItem;
  end;

  TMenuItems = class(TList)
  private
    FMenuItem: TMenuItem;
  protected
    procedure Notify(Ptr: Pointer; Action: TListNotification); override;
  public
    constructor Create(const AMenuItem: TMenuItem);
  end;

  TMenuItem = class(TLCLComponent)
  private
    FActionLink: TMenuActionLink;
    FCaption: TTranslateString;
    FBitmap: TBitmap;
    FGlyphShowMode: TGlyphShowMode;
    FHandle: HMenu;
    FHelpContext: THelpContext;
    FHint: String;
    FImageChangeLink: TChangeLink;
    FImageIndex: TImageIndex;
    FItems: TList; // list of TMenuItem
    FMenu: TMenu;
    FOnChange: TMenuChangeEvent;
    FOnClick: TNotifyEvent;
    FOnDrawItem: TMenuDrawItemEvent;
    FOnMeasureItem: TMenuMeasureItemEvent;
    FParent: TMenuItem;
    FMerged: TMenuItem;
    FMergedWith: TMenuItem;
    FMergedItems: TMergedMenuItems;
    FMenuItemHandlers: array[TMenuItemHandlerType] of TMethodList;
    FSubMenuImages: TCustomImageList;
    FSubMenuImagesWidth: Integer;
    FShortCut: TShortCut;
    FShortCutKey2: TShortCut;
    FGroupIndex: Byte;
    FRadioItem: Boolean;
    FRightJustify: boolean;
    FShowAlwaysCheckable: boolean;
    FVisible: Boolean;
    // True => Bitmap property indicates assigned Bitmap.
    // False => Bitmap property is not assigned but can represent imagelist bitmap
    FBitmapIsValid: Boolean;
    FAutoCheck: Boolean;
    FChecked: Boolean;
    FDefault: Boolean;
    FEnabled: Boolean;
    function GetBitmap: TBitmap;
    function GetCount: Integer;
    function GetItem(Index: Integer): TMenuItem;
    function GetMenuIndex: Integer;
    function GetMergedItems: TMergedMenuItems;
    function GetMergedParent: TMenuItem;
    function GetParent: TMenuItem;
    function IsBitmapStored: boolean;
    function IsCaptionStored: boolean;
    function IsCheckedStored: boolean;
    function IsEnabledStored: boolean;
    function IsHelpContextStored: boolean;
    function IsHintStored: Boolean;
    function IsImageIndexStored: Boolean;
    function IsShortCutStored: boolean;
    function IsVisibleStored: boolean;
    procedure MergeWith(const aMenu: TMenuItem);
    procedure SetAutoCheck(const AValue: boolean);
    procedure SetCaption(const AValue: TTranslateString);
    procedure SetChecked(AValue: Boolean);
    procedure SetDefault(AValue: Boolean);
    procedure SetEnabled(AValue: Boolean);
    procedure SetBitmap(const AValue: TBitmap);
    procedure SetGlyphShowMode(const AValue: TGlyphShowMode);
    procedure SetMenuIndex(AValue: Integer);
    procedure SetName(const Value: TComponentName); override;
    procedure SetRadioItem(const AValue: Boolean);
    procedure SetRightJustify(const AValue: boolean);
    procedure SetShowAlwaysCheckable(const AValue: boolean);
    procedure SetSubMenuImages(const AValue: TCustomImageList);
    procedure SetSubMenuImagesWidth(const aSubMenuImagesWidth: Integer);
    procedure ShortcutChanged;
    procedure SubItemChanged(Sender: TObject; Source: TMenuItem; Rebuild: Boolean);
    procedure TurnSiblingsOff;
    procedure DoActionChange(Sender: TObject);
  protected
    FCommand: Word;
    class procedure WSRegisterClass; override;
    procedure ActionChange(Sender: TObject; CheckDefaults: Boolean); virtual;
    procedure AssignTo(Dest: TPersistent); override;
    procedure BitmapChange(Sender: TObject);
    function DoDrawItem(ACanvas: TCanvas; ARect: TRect; AState: TOwnerDrawState): Boolean; virtual;
    function DoMeasureItem(ACanvas: TCanvas; var AWidth, AHeight: Integer): Boolean; virtual;
    function GetAction: TBasicAction;
    function GetActionLinkClass: TMenuActionLinkClass; virtual;
    function GetHandle: HMenu;
    procedure DoClicked(var msg); message LM_ACTIVATE;
    procedure CheckChildrenHandles;
    procedure CreateHandle; virtual;
    procedure DestroyHandle; virtual;
    procedure Loaded; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure GetChildren(Proc: TGetChildProc; Root: TComponent); override;
    procedure InitiateActions;
    procedure MenuChanged(Rebuild : Boolean);
    procedure SetAction(NewAction: TBasicAction);
    procedure SetChildOrder(Child: TComponent; Order: Integer); override;
    procedure SetGroupIndex(AValue: Byte);
    procedure SetImageIndex(AValue : TImageIndex);
    procedure SetParentComponent(AValue : TComponent); override;
    procedure SetShortCut(const AValue : TShortCut);
    procedure SetShortCutKey2(const AValue : TShortCut);
    procedure SetVisible(AValue: Boolean);
    procedure UpdateWSIcon;
    procedure ImageListChange(Sender: TObject);
  protected
    property ActionLink: TMenuActionLink read FActionLink write FActionLink;
  public
    FCompStyle: LongInt;
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    function Find(const ACaption: string): TMenuItem;
    function GetEnumerator: TMenuItemEnumerator;
    procedure GetImageList(out aImages: TCustomImageList; out aImagesWidth: Integer); virtual;
    function GetImageList: TCustomImageList;
    function GetParentComponent: TComponent; override;
    function GetParentMenu: TMenu; virtual;
    function GetMergedParentMenu: TMenu; virtual;
    function GetIsRightToLeft:Boolean; virtual;
    function HandleAllocated : Boolean;
    function HasIcon: boolean; virtual;
    function HasParent: Boolean; override;
    procedure InitiateAction; virtual;
    procedure IntfDoSelect; virtual;
    function IndexOf(Item: TMenuItem): Integer;
    function IndexOfCaption(const ACaption: string): Integer; virtual;
    procedure InvalidateMergedItems;
    function VisibleIndexOf(Item: TMenuItem): Integer;
    procedure Add(Item: TMenuItem);
    procedure Add(const AItems: array of TMenuItem);
    procedure AddSeparator;
    procedure Click; virtual;
    procedure Delete(Index: Integer);
    procedure HandleNeeded; virtual;
    procedure Insert(Index: Integer; Item: TMenuItem);
    procedure RecreateHandle; virtual;
    procedure Remove(Item: TMenuItem);
    procedure UpdateImage(forced: Boolean = false);
    procedure UpdateImages(forced: Boolean = false);
    function IsCheckItem: boolean; virtual;
    function IsLine: Boolean;
    function IsInMenuBar: boolean; virtual;
    procedure Clear;
    function HasBitmap: boolean;
    function GetIconSize(ADC: HDC): TPoint; virtual;
    // Event lists
    procedure RemoveAllHandlersOfObject(AnObject: TObject); override;
    procedure AddHandlerOnDestroy(const OnDestroyEvent: TNotifyEvent;
                                  AsFirst: boolean = false);
    procedure RemoveHandlerOnDestroy(const OnDestroyEvent: TNotifyEvent);
    procedure AddHandler(HandlerType: TMenuItemHandlerType;
                         const AMethod: TMethod; AsFirst: boolean = false);
    procedure RemoveHandler(HandlerType: TMenuItemHandlerType;
                            const AMethod: TMethod);
    property Merged: TMenuItem read FMerged;
    property MergedWith: TMenuItem read FMergedWith;
  public
    property Count: Integer read GetCount;
    property Handle: HMenu read GetHandle write FHandle;
    property Items[Index: Integer]: TMenuItem read GetItem; default;
    property MergedItems: TMergedMenuItems read GetMergedItems;
    property MenuIndex: Integer read GetMenuIndex write SetMenuIndex;
    property Menu: TMenu read FMenu;
    property Parent: TMenuItem read GetParent;
    property MergedParent: TMenuItem read GetMergedParent;
    property Command: Word read FCommand;
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
    property GlyphShowMode: TGlyphShowMode read FGlyphShowMode write SetGlyphShowMode default gsmApplication;
    property HelpContext: THelpContext read FHelpContext write FHelpContext
                                           stored IsHelpContextStored default 0;
    property Hint: TTranslateString read FHint write FHint stored IsHintStored;
    property ImageIndex: TImageIndex read FImageIndex write SetImageIndex
                                           stored IsImageIndexStored default -1;
    property RadioItem: Boolean read FRadioItem write SetRadioItem default False;
    property RightJustify: boolean read FRightJustify write SetRightJustify default False;
    property ShortCut: TShortCut read FShortCut write SetShortCut
                                 stored IsShortCutStored default 0;
    property ShortCutKey2: TShortCut read FShortCutKey2 write SetShortCutKey2 default 0;
    property ShowAlwaysCheckable: boolean read FShowAlwaysCheckable
                                 write SetShowAlwaysCheckable default False;
    property SubMenuImages: TCustomImageList read FSubMenuImages write SetSubMenuImages;
    property SubMenuImagesWidth: Integer read FSubMenuImagesWidth write SetSubMenuImagesWidth default 0;
    property Visible: Boolean read FVisible write SetVisible
                              stored IsVisibleStored default True;
    property OnClick: TNotifyEvent read FOnClick write FOnClick;
    property OnDrawItem: TMenuDrawItemEvent read FOnDrawItem write FOnDrawItem;
    property OnMeasureItem: TMenuMeasureItemEvent read FOnMeasureItem write FOnMeasureItem;
  end;
  TMenuItemClass = class of TMenuItem;


  { TMenu }

  TFindItemKind = (fkCommand, fkHandle, fkShortCut);

  TMenu = class(TLCLComponent)
  private
    FBiDiMode: TBiDiMode;
    FImageChangeLink: TChangeLink;
    FImages: TCustomImageList;
    FImagesWidth: Integer;
    FItems: TMenuItem;
    FOnDrawItem: TMenuDrawItemEvent;
    FOnChange: TMenuChangeEvent;
    FOnMeasureItem: TMenuMeasureItemEvent;
    FOwnerDraw: Boolean;
    FParent: TComponent;
    FParentBiDiMode: Boolean;
    FShortcutHandled: boolean;
//See TCustomForm.CMBiDiModeChanged
    procedure CMParentBiDiModeChanged(var Message: TLMessage); message CM_PARENTBIDIMODECHANGED;
    procedure CMAppShowMenuGlyphChanged(var Message: TLMessage); message CM_APPSHOWMENUGLYPHCHANGED;
    function IsBiDiModeStored: Boolean;
    procedure ImageListChange(Sender: TObject);
    procedure SetBiDiMode(const AValue: TBiDiMode);
    procedure SetImages(const AValue: TCustomImageList);
    procedure SetImagesWidth(const aImagesWidth: Integer);
    procedure SetParent(const AValue: TComponent);
    procedure SetParentBiDiMode(const AValue: Boolean);
  protected
    class procedure WSRegisterClass; override;
    procedure BidiModeChanged; virtual;
    procedure CreateHandle; virtual;
    procedure DoChange(Source: TMenuItem; Rebuild: Boolean); virtual;
    function GetHandle: HMENU; virtual;
    procedure GetChildren(Proc: TGetChildProc; Root: TComponent); override;
    procedure MenuChanged(Sender: TObject; Source: TMenuItem;
                          Rebuild: Boolean); virtual;
    procedure AssignTo(Dest: TPersistent); override;
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
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
    function FindItem(AValue: PtrInt; Kind: TFindItemKind): TMenuItem;
    function GetHelpContext(AValue: PtrInt; ByCommand: Boolean): THelpContext;
    function IsShortcut(var Message: TLMKey): boolean;
    function HandleAllocated: Boolean;
    function IsRightToLeft: Boolean; virtual;
    function UseRightToLeftAlignment: Boolean; virtual;
    function UseRightToLeftReading: Boolean; virtual;
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
    property ImagesWidth: Integer read FImagesWidth write SetImagesWidth default 0;
    property OwnerDraw: Boolean read FOwnerDraw write FOwnerDraw default False;
    property OnDrawItem: TMenuDrawItemEvent read FOnDrawItem write FOnDrawItem;
    property OnMeasureItem: TMenuMeasureItemEvent read FOnMeasureItem write FOnMeasureItem;
  end;


  { TMainMenu }

  TMainMenu = class(TMenu)
  private
    FWindowHandle: HWND;
    function GetHeight: Integer;
    procedure SetWindowHandle(const AValue: HWND);
  protected
    procedure ItemChanged;
    class procedure WSRegisterClass; override;
    procedure MenuChanged(Sender: TObject; Source: TMenuItem; Rebuild: Boolean); override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Merge(Menu: TMainMenu);
    procedure Unmerge(Menu: TMainMenu);
    property Height: Integer read GetHeight;
    property WindowHandle: HWND read FWindowHandle write SetWindowHandle;
  published
    property OnChange;
  end;


  { TPopupMenu }

  TPopupAlignment = (paLeft, paRight, paCenter);
  TTrackButton = (tbRightButton, tbLeftButton);

  TPopupMenu = class(TMenu)
  private
    FAlignment: TPopupAlignment;
    FAutoPopup: Boolean;
    FOnClose: TNotifyEvent;
    FOnPopup: TNotifyEvent;
    FPopupComponent: TComponent;
    FPopupPoint: TPoint;
    FTrackButton: TTrackButton;
    function GetHelpContext: THelpContext;
    procedure SetHelpContext(const AValue: THelpContext);
  protected
    class procedure WSRegisterClass; override;
    procedure DoPopup(Sender: TObject); virtual;
    procedure DoClose; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure PopUp;
    procedure PopUp(X, Y: Integer); virtual;
    property PopupComponent: TComponent read FPopupComponent write FPopupComponent;
    property PopupPoint: TPoint read FPopupPoint;
    procedure Close;
  published
    property Alignment: TPopupAlignment read FAlignment write FAlignment default paLeft;
    property AutoPopup: Boolean read FAutoPopup write FAutoPopup default True;
    property HelpContext: THelpContext read GetHelpContext write SetHelpContext default 0;
    property TrackButton: TTrackButton read FTrackButton write FTrackButton default tbRightButton;
    property OnPopup: TNotifyEvent read FOnPopup write FOnPopup;
    property OnClose: TNotifyEvent read FOnClose write FOnClose;
  end;

function ShortCut(const Key: Word; const Shift : TShiftState) : TShortCut;
procedure ShortCutToKey(const ShortCut : TShortCut; out Key: Word;
                        out Shift : TShiftState);

var
  DesignerMenuItemClick: TNotifyEvent = nil;
  ActivePopupMenu: TPopupMenu = nil;
  OnMenuPopupHandler: TNotifyEvent = nil;

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

function StripHotkey(const Text: string): string;

procedure Register;


const
  cHotkeyPrefix   = '&';
  cLineCaption    = '-';
  cDialogSuffix   = '...';

  ValidMenuHotkeys: string = '1234567890ABCDEFGHIJKLMNOPQRSTUVWXYZ';



implementation

uses
  WSMenus,
  Forms {KeyDataToShiftState};

{ Helpers for Assign() }

procedure MenuItem_Copy(ASrc, ADest: TMenuItem);
var
  mi: TMenuItem;
  i: integer;
begin
  ADest.Clear;
  ADest.Action:= ASrc.Action;
  ADest.AutoCheck:= ASrc.AutoCheck;
  ADest.Caption:= ASrc.Caption;
  ADest.Checked:= ASrc.Checked;
  ADest.Default:= ASrc.Default;
  ADest.Enabled:= ASrc.Enabled;
  ADest.Bitmap:= ASrc.Bitmap;
  ADest.GroupIndex:= ASrc.GroupIndex;
  ADest.GlyphShowMode:= ASrc.GlyphShowMode;
  ADest.HelpContext:= ASrc.HelpContext;
  ADest.Hint:= ASrc.Hint;
  ADest.ImageIndex:= ASrc.ImageIndex;
  ADest.RadioItem:= ASrc.RadioItem;
  ADest.RightJustify:= ASrc.RightJustify;
  ADest.ShortCut:= ASrc.ShortCut;
  ADest.ShortCutKey2:= ASrc.ShortCutKey2;
  ADest.ShowAlwaysCheckable:= ASrc.ShowAlwaysCheckable;
  ADest.SubMenuImages:= ASrc.SubMenuImages;
  ADest.SubMenuImagesWidth:= ASrc.SubMenuImagesWidth;
  ADest.Visible:= ASrc.Visible;
  ADest.OnClick:= ASrc.OnClick;
  ADest.OnDrawItem:= ASrc.OnDrawItem;
  ADest.OnMeasureItem:= ASrc.OnMeasureItem;
  ADest.Tag:= ASrc.Tag;

  for i:= 0 to ASrc.Count-1 do
  begin
    mi:= TMenuItem.Create(ASrc.Owner);
    MenuItem_Copy(ASrc.Items[i], mi);
    ADest.Add(mi);
  end;
end;

procedure Menu_Copy(ASrc, ADest: TMenu);
begin
  ADest.BidiMode:= ASrc.BidiMode;
  ADest.ParentBidiMode:= ASrc.ParentBidiMode;
  ADest.Images:= ASrc.Images;
  ADest.ImagesWidth:= ASrc.ImagesWidth;
  ADest.OwnerDraw:= ASrc.OwnerDraw;
  ADest.OnDrawItem:= ASrc.OnDrawItem;
  ADest.OnMeasureItem:= ASrc.OnMeasureItem;

  MenuItem_Copy(ASrc.Items, ADest.Items);
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
  Result.Alignment:=Alignment;
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
  Result.Caption := cLineCaption;
end;

function StripHotkey(const Text: string): string;
var
  I, R: Integer;
begin
  SetLength(Result, Length(Text));
  I := 1;
  R := 1;
  while I <= Length(Text) do
  begin
    if Text[I] = cHotkeyPrefix then
    begin
      if (I < Length(Text)) and (Text[I+1] = cHotkeyPrefix) then
      begin
        Result[R] := Text[I];
        Inc(R);
        Inc(I, 2);
      end else
        Inc(I);
    end else
    begin
      Result[R] := Text[I];
      Inc(R);
      Inc(I);
    end;
  end;
  SetLength(Result, R-1);
end;

procedure Register;
begin
  RegisterComponents('Standard',[TMainMenu,TPopupMenu]);
  RegisterNoIcon([TMenuItem]);
end;

{ TMenuItems }

constructor TMenuItems.Create(const AMenuItem: TMenuItem);
begin
  inherited Create;

  FMenuItem := AMenuItem;
end;

procedure TMenuItems.Notify(Ptr: Pointer; Action: TListNotification);
begin
  FMenuItem.InvalidateMergedItems;
  if Assigned(FMenuItem.MergedWith) then
  begin
    FMenuItem.MergedWith.InvalidateMergedItems;
    FMenuItem.MergedWith.CheckChildrenHandles;
  end;
end;

{ TMergedMenuItems }

constructor TMergedMenuItems.Create(const aParent: TMenuItem);
  procedure SearchVis(const aGroupIndex: Integer; out outIndex: Integer; out outReplace: Boolean);
  var
    AItem: TMenuItem;
    I: Integer;
  begin
    outReplace := False;
    for I := 0 to VisibleCount-1 do
    begin
      AItem := VisibleItems[I];
      if AItem.GroupIndex=aGroupIndex then
      begin
        outIndex := I;
        outReplace := True;
        Exit;
      end else
      if AItem.GroupIndex>aGroupIndex then
      begin
        outIndex := I;
        Exit;
      end;
    end;
    outIndex := -1;
  end;
var
  B, AReplace: Boolean;
  I, AItemIndex: Integer;
  AItem: TMenuItem;
begin
  inherited Create;

  for B := Low(fList) to High(fList) do
    fList[B] := TList.Create;

  for I := 0 to aParent.Count-1 do
    fList[aParent.Items[I].Visible].Add(aParent.Items[I]);
  if Assigned(aParent.FMerged) then
  begin
    for I := 0 to aParent.FMerged.Count-1 do
    begin
      AItem := aParent.FMerged.Items[I];
      if AItem.Visible then
      begin
        SearchVis(AItem.GroupIndex, AItemIndex, AReplace);
        if AItemIndex>=0 then
        begin
          if AReplace then
          begin
            fList[False].Add(VisibleItems[AItemIndex]); // copy to invisible list
            fList[True].Items[AItemIndex] := AItem // replace
          end else
            fList[True].Insert(AItemIndex, AItem); // insert
        end else
          fList[True].Add(AItem); // add
      end else
        fList[False].Add(AItem); // add to invisible
    end;
  end;
end;

class function TMergedMenuItems.DefaultSort(aItem1, aItem2,
  aParentItem: Pointer): Integer;
var
  Item1: TMenuItem absolute aItem1;
  Item2: TMenuItem absolute aItem2;
begin
  Result := Item1.GroupIndex-Item2.GroupIndex;
  if Result=0 then
  begin
    if Pointer(Item1.Parent)=aParentItem then
      Result := 1
    else
      Result := -1;
  end;
end;

destructor TMergedMenuItems.Destroy;
var
  B: Boolean;
begin
  for B := Low(fList) to High(fList) do
    fList[B].Destroy;

  inherited Destroy;
end;

function TMergedMenuItems.GetInvisibleCount: Integer;
begin
  Result := fList[False].Count;
end;

function TMergedMenuItems.GetInvisibleItem(Index: Integer): TMenuItem;
begin
  Result := TMenuItem(fList[False].Items[Index]);
end;

function TMergedMenuItems.GetVisibleCount: Integer;
begin
  Result := fList[True].Count;
end;

function TMergedMenuItems.GetVisibleItem(Index: Integer): TMenuItem;
begin
  Result := TMenuItem(fList[True].Items[Index]);
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

procedure ShortCutToKey(const ShortCut: TShortCut; out Key: Word;
  out Shift : TShiftState);
begin
  Key := ShortCut and $FF;
  Shift := [];
  if ShortCut and scShift <> 0 then Include(Shift,ssShift);
  if ShortCut and scAlt <> 0 then Include(Shift,ssAlt);
  if ShortCut and scCtrl <> 0 then Include(Shift,ssCtrl);
  if ShortCut and scMeta <> 0 then Include(Shift,ssMeta);
end;

{ TMenuItemEnumerator }

function TMenuItemEnumerator.GetCurrent: TMenuItem;
begin
  Result := FMenuItem.Items[FPosition];
end;

constructor TMenuItemEnumerator.Create(AMenuItem: TMenuItem);
begin
  FMenuItem := AMenuItem;
  FPosition := -1;
end;

function TMenuItemEnumerator.MoveNext: Boolean;
begin
  inc(FPosition);
  Result := FPosition < FMenuItem.Count;
end;

end.
