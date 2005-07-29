{
 *****************************************************************************
 *                                                                           *
 *  See the file COPYING.modifiedLGPL, included in this distribution,        *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************

  Author: Mattias Gaertner

  Abstract:
    Interface to the IDE menus.
}
unit MenuIntf;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Menus, ImgList, Graphics, IDECommands;
  
type
  TIDEMenuSection = class;

  { TIDEMenuItem
    A menu item in one of the IDE's menus.
    This is only the base class for TIDEMenuSection and TIDEMenuCommand }
    
  TIDEMenuItem = class(TPersistent)
  private
    FBitmap: TBitmap;
    FCaption: string;
    FEnabled: Boolean;
    FImageIndex: Integer;
    FMenuItem: TMenuItem;
    FMenuItemClass: TMenuItemClass;
    FName: string;
    FOnClick: TNotifyEvent;
    FSection: TIDEMenuSection;
    FVisible: Boolean;
    FHint: string;
    procedure SetEnabled(const AValue: Boolean);
  protected
    function GetBitmap: TBitmap; virtual;
    function GetCaption: string; virtual;
    function GetHint: String; virtual;
    procedure SetBitmap(const AValue: TBitmap); virtual;
    procedure SetCaption(const AValue: string); virtual;
    procedure SetHint(const AValue: String); virtual;
    procedure SetImageIndex(const AValue: Integer); virtual;
    procedure SetMenuItem(const AValue: TMenuItem); virtual;
    procedure SetName(const AValue: string); virtual;
    procedure SetSection(const AValue: TIDEMenuSection); virtual;
    procedure SetVisible(const AValue: Boolean); virtual;
  public
    constructor Create(const TheName: string); virtual;
    destructor Destroy; override;
    function HasBitmap: Boolean;
    procedure CreateMenuItem; virtual;
    function Size: Integer; virtual;
  public
    property Name: string read FName write SetName;
    property Bitmap: TBitmap read GetBitmap write SetBitmap;
    property Hint: String read GetHint write SetHint;
    property ImageIndex: Integer read FImageIndex write SetImageIndex;
    property Visible: Boolean read FVisible write SetVisible;
    property OnClick: TNotifyEvent read FOnClick write FOnClick;
    property Caption: string read GetCaption write SetCaption;
    property Section: TIDEMenuSection read FSection write SetSection;
    property Enabled: Boolean read FEnabled write SetEnabled;
    property MenuItem: TMenuItem read FMenuItem write SetMenuItem;
    property MenuItemClass: TMenuItemClass read FMenuItemClass write FMenuItemClass;
  end;
  TIDEMenuItemClass = class of TIDEMenuItem;
  
  { TIDEMenuSection
    An TIDEMenuItem with childs, either in a sub menu or separated with
    separators. }
  
  TIDEMenuSection = class(TIDEMenuItem)
  private
    FBottomSeparator: TMenuItem;
    FChildsAsSubMenu: boolean;
    FSubMenuImages: TCustomImageList;
    FItems: TFPList;
    FTopSeparator: TMenuItem;
    function GetItems(Index: Integer): TIDEMenuItem;
  protected
    procedure SetChildsAsSubMenu(const AValue: boolean); virtual;
    procedure SetSubMenuImages(const AValue: TCustomImageList); virtual;
  public
    constructor Create(const TheName: string); override;
    destructor Destroy; override;
    procedure Clear;
    function Count: Integer;
    procedure AddFirst(AnItem: TIDEMenuItem);
    procedure AddLast(AnItem: TIDEMenuItem);
    procedure Insert(Index: Integer; AnItem: TIDEMenuItem);
    procedure CreateChildMenuItem(Index: Integer); virtual;
    function GetChildsStartIndex: Integer;
    function Size: Integer; override;
    function IndexOf(AnItem: TIDEMenuItem): Integer;
  public
    property ChildsAsSubMenu: boolean read FChildsAsSubMenu
                                          write SetChildsAsSubMenu default true;
    property SubMenuImages: TCustomImageList read FSubMenuImages
                                             write SetSubMenuImages;
    property Items[Index: Integer]: TIDEMenuItem read GetItems; default;
    property TopSeparator: TMenuItem read FTopSeparator;
    property BottomSeparator: TMenuItem read FBottomSeparator;
  end;
  TIDEMenuSectionClass = class of TIDEMenuSection;

  { TIDEMenuCommand
    A leaf menu item. No childs.
    An IDE command can be assigned, which defines the shortcut.
  }

  TIDEMenuCommand = class(TIDEMenuItem)
  private
    FAutoCheck: boolean;
    FChecked: Boolean;
    FCommand: TIDECommandKeys;
    FDefault: Boolean;
    FGroupIndex: Byte;
    FRadioItem: Boolean;
    FRightJustify: boolean;
    FShowAlwaysCheckable: boolean;
  protected
    procedure SetAutoCheck(const AValue: boolean); virtual;
    procedure SetChecked(const AValue: Boolean); virtual;
    procedure SetDefault(const AValue: Boolean); virtual;
    procedure SetGroupIndex(const AValue: Byte); virtual;
    procedure SetRadioItem(const AValue: Boolean); virtual;
    procedure SetRightJustify(const AValue: boolean); virtual;
    procedure SetShowAlwaysCheckable(const AValue: boolean); virtual;
    procedure SetCommand(const AValue: TIDECommandKeys); virtual;
    procedure SetMenuItem(const AValue: TMenuItem); override;
  public
    property Command: TIDECommandKeys read FCommand write SetCommand;
    property AutoCheck: boolean read FAutoCheck write SetAutoCheck default False;
    property Checked: Boolean read FChecked write SetChecked default False;
    property Default: Boolean read FDefault write SetDefault default False;
    property GroupIndex: Byte read FGroupIndex write SetGroupIndex default 0;
    property RadioItem: Boolean read FRadioItem write SetRadioItem;
    property RightJustify: boolean read FRightJustify write SetRightJustify;
    property ShowAlwaysCheckable: boolean read FShowAlwaysCheckable
                                          write SetShowAlwaysCheckable;
  end;
  TIDEMenuCommandClass = class of TIDEMenuCommand;
  
  { TIDEMenuRoots }

  TIDEMenuRoots = class(TPersistent)
  private
    FItems: TFPList;
    function GetItems(Index: integer): TIDEMenuItem;
  public
    constructor Create;
    destructor Destroy; override;
    procedure RegisterMenuRoot(Section: TIDEMenuItem);
    procedure UnregisterMenuRoot(Section: TIDEMenuItem);
    function Count: Integer;
    procedure Clear;
    procedure Delete(Index: Integer);
  public
    property Items[Index: integer]: TIDEMenuItem read GetItems; default;
  end;

implementation

{ TIDEMenuItem }

procedure TIDEMenuItem.SetEnabled(const AValue: Boolean);
begin
  if FEnabled=AValue then exit;
  FEnabled:=AValue;
end;

function TIDEMenuItem.GetBitmap: TBitmap;
begin
  if FBitmap=nil then
    FBitmap:=TBitmap.Create;
  FBitmap.Transparent:=True;
  Result:=FBitmap;
end;

function TIDEMenuItem.GetCaption: string;
begin
  if FCaption<>'' then
    Result:=FCaption
  else
    Result:=FName;
end;

function TIDEMenuItem.GetHint: String;
begin
  Result:=FHint;
end;

procedure TIDEMenuItem.SetBitmap(const AValue: TBitmap);
begin
  if FBitmap=AValue then exit;
  if AValue<>nil then
    Bitmap.Assign(AValue)
  else
    FBitmap.Free;
  if MenuItem<>nil then
    MenuItem.Bitmap:=FBitmap;
end;

procedure TIDEMenuItem.SetCaption(const AValue: string);
begin
  FCaption:=AValue;
  if MenuItem<>nil then
    MenuItem.Caption:=Caption;
end;

procedure TIDEMenuItem.SetHint(const AValue: String);
begin
  FHint:=AValue;
  if MenuItem<>nil then
    MenuItem.Hint:=Hint;
end;

procedure TIDEMenuItem.SetImageIndex(const AValue: Integer);
begin
  if FImageIndex=AValue then exit;
  FImageIndex:=AValue;
  if MenuItem<>nil then
    MenuItem.ImageIndex:=ImageIndex;
end;

procedure TIDEMenuItem.SetMenuItem(const AValue: TMenuItem);
begin
  if FMenuItem=AValue then exit;
  FMenuItem:=AValue;
  if MenuItem<>nil then begin
    MenuItem.Caption:=Caption;
    MenuItem.Bitmap:=FBitmap;
    MenuItem.Hint:=Hint;
    MenuItem.ImageIndex:=ImageIndex;
    MenuItem.Visible:=Visible;
  end;
end;

procedure TIDEMenuItem.SetName(const AValue: string);
begin
  if FName=AValue then exit;
  FName:=AValue;
end;

procedure TIDEMenuItem.SetSection(const AValue: TIDEMenuSection);
begin
  if FSection=AValue then exit;
  FSection:=AValue;
end;

procedure TIDEMenuItem.SetVisible(const AValue: Boolean);
begin
  if FVisible=AValue then exit;
  FVisible:=AValue;
  if MenuItem<>nil then
    MenuItem.Visible:=Visible;
end;

constructor TIDEMenuItem.Create(const TheName: string);
begin
  inherited Create;
  FName:=TheName;
  FEnabled:=true;
  FMenuItemClass:=TMenuItem;
end;

destructor TIDEMenuItem.Destroy;
begin
  FreeAndNil(FBitmap);
  FreeAndNil(FMenuItem);
  inherited Destroy;
end;

function TIDEMenuItem.HasBitmap: Boolean;
begin
  Result:=FBitmap<>nil;
end;

procedure TIDEMenuItem.CreateMenuItem;
begin
  if FMenuItem<>nil then exit;
  MenuItem:=MenuItemClass.Create(nil);
end;

function TIDEMenuItem.Size: Integer;
begin
  Result:=1;
end;

{ TIDEMenuSection }

procedure TIDEMenuSection.SetSubMenuImages(const AValue: TCustomImageList);
begin
  if FSubMenuImages=AValue then exit;
  FSubMenuImages:=AValue;
end;

constructor TIDEMenuSection.Create(const TheName: string);
begin
  inherited Create(TheName);
  FChildsAsSubMenu:=true;
  FItems:=TFPList.Create;
end;

destructor TIDEMenuSection.Destroy;
begin
  Clear;
  FItems.Free;
  inherited Destroy;
end;

procedure TIDEMenuSection.Clear;
var
  i: Integer;
begin
  for i:=FItems.Count-1 downto 0 do TObject(FItems[i]).Free;
  FItems.Clear;
end;

function TIDEMenuSection.Count: Integer;
begin
  Result:=FItems.Count;
end;

procedure TIDEMenuSection.AddFirst(AnItem: TIDEMenuItem);
begin
  Insert(0,AnItem);
end;

procedure TIDEMenuSection.AddLast(AnItem: TIDEMenuItem);
begin
  Insert(Count,AnItem);
end;

procedure TIDEMenuSection.Insert(Index: Integer; AnItem: TIDEMenuItem);
begin
  FItems.Insert(Index,AnItem);
  AnItem.FSection:=Self;
  CreateChildMenuItem(Index);
end;

procedure TIDEMenuSection.CreateChildMenuItem(Index: Integer);
var
  Item: TIDEMenuItem;
  SubSection: TIDEMenuSection;
  i: Integer;
begin
  if MenuItem=nil then exit;
  Item:=Items[Index];
  // create the child TMenuItem
  Item.CreateMenuItem;
  MenuItem.Insert(Index+GetChildsStartIndex,Item.MenuItem);
  // create the subsections
  if Item is TIDEMenuSection then begin
    SubSection:=TIDEMenuSection(Item);
    for i:=0 to SubSection.Count-1 do
      SubSection.CreateChildMenuItem(i);
  end;
end;

function TIDEMenuSection.GetChildsStartIndex: Integer;
var
  SiblingIndex: Integer;
begin
  Result:=0;
  if ChildsAsSubMenu or (Section=nil) then exit;
  SiblingIndex:=0;
  while (Section[SiblingIndex]<>Self) do begin
    inc(Result,Section[SiblingIndex].Size);
    inc(SiblingIndex);
  end;
end;

function TIDEMenuSection.Size: Integer;
var
  SelfIndex: LongInt;
  NextSibling: TIDEMenuItem;
begin
  Result:=1;
  if (Section<>nil) and (not ChildsAsSubMenu) then begin
    // childs are not in a submenu but directly added to parents menuitem
    Result:=Count;
    SelfIndex:=Section.IndexOf(Self);
    if (SelfIndex>0) then begin
      // a top separator is needed
      inc(Result);
    end;
    if (SelfIndex<Section.Count-1) then begin
      NextSibling:=Section[SelfIndex-1];
      if (not (NextSibling is TIDEMenuSection))
      or (not TIDEMenuSection(NextSibling).ChildsAsSubMenu) then begin
        // a bottom separator is needed
        inc(Result);
      end;
    end;
  end;
end;

function TIDEMenuSection.IndexOf(AnItem: TIDEMenuItem): Integer;
begin
  Result:=FItems.IndexOf(AnItem);
end;

function TIDEMenuSection.GetItems(Index: Integer): TIDEMenuItem;
begin
  Result:=TIDEMenuItem(FItems[Index]);
end;

procedure TIDEMenuSection.SetChildsAsSubMenu(const AValue: boolean);
begin
  if FChildsAsSubMenu=AValue then exit;
  FChildsAsSubMenu:=AValue;
end;

{ TIDEMenuCommand }

procedure TIDEMenuCommand.SetAutoCheck(const AValue: boolean);
begin
  if FAutoCheck=AValue then exit;
  FAutoCheck:=AValue;
  if MenuItem<>nil then MenuItem.AutoCheck:=AutoCheck;
end;

procedure TIDEMenuCommand.SetChecked(const AValue: Boolean);
begin
  if FChecked=AValue then exit;
  FChecked:=AValue;
  if MenuItem<>nil then MenuItem.Checked:=Checked;
end;

procedure TIDEMenuCommand.SetDefault(const AValue: Boolean);
begin
  if FDefault=AValue then exit;
  FDefault:=AValue;
  if MenuItem<>nil then MenuItem.Default:=Default;
end;

procedure TIDEMenuCommand.SetGroupIndex(const AValue: Byte);
begin
  if FGroupIndex=AValue then exit;
  FGroupIndex:=AValue;
  if MenuItem<>nil then
    MenuItem.GroupIndex:=GroupIndex;
end;

procedure TIDEMenuCommand.SetRadioItem(const AValue: Boolean);
begin
  if FRadioItem=AValue then exit;
  FRadioItem:=AValue;
  if MenuItem<>nil then
    MenuItem.RadioItem:=RadioItem;
end;

procedure TIDEMenuCommand.SetRightJustify(const AValue: boolean);
begin
  if FRightJustify=AValue then exit;
  FRightJustify:=AValue;
  if MenuItem<>nil then
    MenuItem.RightJustify:=RightJustify;
end;

procedure TIDEMenuCommand.SetShowAlwaysCheckable(const AValue: boolean);
begin
  if FShowAlwaysCheckable=AValue then exit;
  FShowAlwaysCheckable:=AValue;
  if MenuItem<>nil then
    MenuItem.ShowAlwaysCheckable:=ShowAlwaysCheckable;
end;

procedure TIDEMenuCommand.SetCommand(const AValue: TIDECommandKeys);
begin
  if FCommand=AValue then exit;
  FCommand:=AValue;
  if MenuItem<>nil then
    MenuItem.ShortCut:=IDEShortCutToMenuShortCut(AValue.KeyA);
end;

procedure TIDEMenuCommand.SetMenuItem(const AValue: TMenuItem);
begin
  inherited SetMenuItem(AValue);
  if MenuItem<>nil then begin
    MenuItem.AutoCheck:=AutoCheck;
    MenuItem.Checked:=Checked;
    MenuItem.Default:=Default;
    MenuItem.RadioItem:=RadioItem;
    MenuItem.RightJustify:=RightJustify;
    MenuItem.ShowAlwaysCheckable:=ShowAlwaysCheckable;
    if Command<>nil then
      MenuItem.ShortCut:=IDEShortCutToMenuShortCut(Command.KeyA)
    else
      MenuItem.ShortCut:=ShortCut(0,[]);
    MenuItem.GroupIndex:=GroupIndex;
  end;
end;

{ TIDEMenuRoots }

function TIDEMenuRoots.GetItems(Index: integer): TIDEMenuItem;
begin
  Result:=TIDEMenuSection(FItems[Index]);
end;

constructor TIDEMenuRoots.Create;
begin
  FItems:=TFPList.Create;
end;

destructor TIDEMenuRoots.Destroy;
begin
  Clear;
  inherited Destroy;
end;

procedure TIDEMenuRoots.RegisterMenuRoot(Section: TIDEMenuItem);
begin
  FItems.Add(Section);
end;

procedure TIDEMenuRoots.UnregisterMenuRoot(Section: TIDEMenuItem);
begin
  FItems.Remove(Section);
end;

function TIDEMenuRoots.Count: Integer;
begin
  Result:=FItems.Count;
end;

procedure TIDEMenuRoots.Clear;
var
  i: Integer;
begin
  for i:=FItems.Count-1 downto 0 do TObject(FItems[i]).Free;
  FItems.Clear;
end;

procedure TIDEMenuRoots.Delete(Index: Integer);
var
  OldItem: TIDEMenuItem;
begin
  OldItem:=Items[Index];
  UnregisterMenuRoot(OldItem);
  OldItem.Free;
end;

end.

