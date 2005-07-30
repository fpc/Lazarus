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
    
  ToDo:
    - Create MainBar menu with the menu interface
    - Create Source Editor Popupmenu with the menu interface
    - Create CodeExplorer Popupmenu with the menu interface
    - Create Project Inspector Popupmenu with the menu interface
    - Create Messages Popupmenu with the menu interface
}
unit MenuIntf;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LCLProc, Menus, ImgList, Graphics, TextTools, HelpIntf,
  IDECommands;
  
type
  TAddMenuItemProc =
    function (const NewCaption: string; const NewEnabled: boolean;
              const NewOnClick: TNotifyEvent): TMenuItem of object;

  TIDEMenuSection = class;
  TNotifyProcedure = procedure(Sender: TObject);

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
    FOnClickMethod: TNotifyEvent;
    FOnClickProc: TNotifyProcedure;
    FSection: TIDEMenuSection;
    FVisible: Boolean;
    FHint: string;
    procedure MenuItemClick(Sender: TObject);
  protected
    function GetBitmap: TBitmap; virtual;
    function GetCaption: string; virtual;
    function GetHint: String; virtual;
    procedure SetBitmap(const AValue: TBitmap); virtual;
    procedure SetCaption(const AValue: string); virtual;
    procedure SetEnabled(const AValue: Boolean); virtual;
    procedure SetHint(const AValue: String); virtual;
    procedure SetImageIndex(const AValue: Integer); virtual;
    procedure SetMenuItem(const AValue: TMenuItem); virtual;
    procedure SetName(const AValue: string); virtual;
    procedure SetSection(const AValue: TIDEMenuSection); virtual;
    procedure SetVisible(const AValue: Boolean); virtual;
    procedure ClearMenuItems; virtual;
  public
    constructor Create(const TheName: string); virtual;
    destructor Destroy; override;
    function HasBitmap: Boolean;
    procedure CreateMenuItem; virtual;
    function Size: Integer; virtual;
    function GetPath: string;
  public
    property Name: string read FName write SetName;
    property Bitmap: TBitmap read GetBitmap write SetBitmap;
    property Hint: String read GetHint write SetHint;
    property ImageIndex: Integer read FImageIndex write SetImageIndex;
    property Visible: Boolean read FVisible write SetVisible;
    property OnClickMethod: TNotifyEvent read FOnClickMethod write FOnClickMethod;
    property OnClickProc: TNotifyProcedure read FOnClickProc write FOnClickProc;
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
    procedure SetMenuItem(const AValue: TMenuItem); override;
    procedure SetChildsAsSubMenu(const AValue: boolean); virtual;
    procedure SetSubMenuImages(const AValue: TCustomImageList); virtual;
    procedure ClearMenuItems; override;
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
    function NeedTopSeparator: Boolean;
    function NeedBottomSeparator: Boolean;
    function GetContainerMenuItem: TMenuItem;
    function IndexByName(const AName: string): Integer;
    function FindByName(const AName: string): TIDEMenuItem;
    function CreateUniqueName(const AName: string): string;
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
    Hint: The shortcut is defined via the Command property.
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
  
  
  { TIDEMenuRoots
    These are the top level menu items of the IDE. }

  TIDEMenuRoots = class(TPersistent)
  private
    FItems: TFPList;// list of TIDEMenuSection
    function GetItems(Index: integer): TIDEMenuSection;
  public
    constructor Create;
    destructor Destroy; override;
    procedure RegisterMenuRoot(Section: TIDEMenuSection);
    procedure UnregisterMenuRoot(Section: TIDEMenuSection);
    function Count: Integer;
    procedure Clear;
    procedure Delete(Index: Integer);
    function IndexByName(const Name: string): Integer;
    function FindByName(const Name: string): TIDEMenuSection;
    function CreateUniqueName(const Name: string): string;
    function FindByPath(const Path: string;
                        ErrorOnNotFound: boolean): TIDEMenuItem;
  public
    property Items[Index: integer]: TIDEMenuSection read GetItems; default;
  end;

var
  IDEMenuRoots: TIDEMenuRoots = nil;// created by the IDE
  // Popupmenu of the SourceEditor:
  SourceEditorMenuRoot: TIDEMenuSection;
  // Source Editor: First dynamic section for often used context sensitive stuff
  SrcEditMenuSectionFirstDynamic: TIDEMenuSection;
  // Source Editor: First static section (e.g. Find Declaration)
  SrcEditMenuSectionFirstStatic: TIDEMenuSection;
  // Source Editor: Clipboard section (e.g. cut, copy, paste)
  SrcEditMenuSectionClipboard: TIDEMenuSection;
  // Source Editor: register the File Specific dynamic section
  SrcEditMenuSectionFileDynamic: TIDEMenuSection;
  // Source Editor: register the marks section
  SrcEditMenuSectionMarks: TIDEMenuSection;
  // Source Editor: register the Goto Bookmarks Submenu
  SrcEditSubMenuGotoBookmarks: TIDEMenuSection;
  // Source Editor: register the Set Bookmarks Submenu
  SrcEditSubMenuSetBookmarks: TIDEMenuSection;

  MessagesMenuRoot: TIDEMenuSection;
  CodeExplorerMenuRoot: TIDEMenuSection;

function RegisterIDEMenuRoot(const Name: string; MenuItem: TMenuItem = nil
                             ): TIDEMenuSection;
function RegisterIDEMenuSection(const Path, Name: string): TIDEMenuSection;
function RegisterIDESubMenu(const Path, Name, Caption: string;
                            const OnClickMethod: TNotifyEvent = nil;
                            const OnClickProc: TNotifyProcedure = nil
                            ): TIDEMenuSection;
function RegisterIDEMenuCommand(const Path, Name, Caption: string;
                                const OnClickMethod: TNotifyEvent = nil;
                                const OnClickProc: TNotifyProcedure = nil;
                                const Command: TIDECommandKeys = nil
                                ): TIDEMenuCommand;

implementation

function RegisterIDEMenuRoot(const Name: string; MenuItem: TMenuItem
  ): TIDEMenuSection;
begin
  {$IFDEF VerboseMenuIntf}
  debugln('RegisterIDEMenuRoot Name="',Name,'"');
  {$ENDIF}
  Result:=TIDEMenuSection.Create(Name);
  IDEMenuRoots.RegisterMenuRoot(Result);
  Result.MenuItem:=MenuItem;
end;

function RegisterIDEMenuSection(const Path, Name: string): TIDEMenuSection;
var
  Parent: TIDEMenuSection;
begin
  {$IFDEF VerboseMenuIntf}
  debugln('RegisterIDEMenuSection Path="',Path,'" Name="',Name,'"');
  {$ENDIF}
  Parent:=IDEMenuRoots.FindByPath(Path,true) as TIDEMenuSection;
  Result:=TIDEMenuSection.Create(Name);
  Result.ChildsAsSubMenu:=false;
  Parent.AddLast(Result);
end;

function RegisterIDESubMenu(const Path, Name, Caption: string;
  const OnClickMethod: TNotifyEvent; const OnClickProc: TNotifyProcedure
  ): TIDEMenuSection;
var
  Parent: TIDEMenuSection;
begin
  {$IFDEF VerboseMenuIntf}
  debugln('RegisterIDESubMenu Path="',Path,'" Name="',Name,'"');
  {$ENDIF}
  Parent:=IDEMenuRoots.FindByPath(Path,true) as TIDEMenuSection;
  Result:=TIDEMenuSection.Create(Name);
  Result.ChildsAsSubMenu:=true;
  Result.Caption:=Caption;
  Result.OnClickMethod:=OnClickMethod;
  Result.OnClickProc:=OnClickProc;
  Parent.AddLast(Result);
end;

function RegisterIDEMenuCommand(const Path, Name, Caption: string;
  const OnClickMethod: TNotifyEvent; const OnClickProc: TNotifyProcedure;
  const Command: TIDECommandKeys): TIDEMenuCommand;
var
  Parent: TIDEMenuSection;
begin
  {$IFDEF VerboseMenuIntf}
  debugln('RegisterIDEMenuCommand Path="',Path,'" Name="',Name,'"');
  {$ENDIF}
  Parent:=IDEMenuRoots.FindByPath(Path,true) as TIDEMenuSection;
  Result:=TIDEMenuCommand.Create(Name);
  Result.Caption:=Caption;
  Result.OnClickMethod:=OnClickMethod;
  Result.OnClickProc:=OnClickProc;
  Result.Command:=Command;
  Parent.AddLast(Result);
end;

{ TIDEMenuItem }

procedure TIDEMenuItem.MenuItemClick(Sender: TObject);
begin
  if Assigned(OnClickMethod) then OnClickMethod(Sender);
  if Assigned(OnClickProc) then OnClickProc(Sender);
end;

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
  if FMenuItem<>nil then ClearMenuItems;
  FMenuItem:=AValue;
  if MenuItem<>nil then begin
    MenuItem.Caption:=Caption;
    MenuItem.Bitmap:=FBitmap;
    MenuItem.Hint:=Hint;
    MenuItem.ImageIndex:=ImageIndex;
    MenuItem.Visible:=Visible;
    MenuItem.OnClick:=@MenuItemClick;
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
  ClearMenuItems;
  FSection:=AValue;
end;

procedure TIDEMenuItem.SetVisible(const AValue: Boolean);
begin
  if FVisible=AValue then exit;
  FVisible:=AValue;
  if MenuItem<>nil then
    MenuItem.Visible:=Visible;
end;

procedure TIDEMenuItem.ClearMenuItems;
begin
  FMenuItem:=nil;
end;

constructor TIDEMenuItem.Create(const TheName: string);
begin
  inherited Create;
  FName:=TheName;
  FEnabled:=true;
  FVisible:=true;
  FMenuItemClass:=TMenuItem;
  {$IFDEF VerboseMenuIntf}
  debugln('TIDEMenuItem.Create ',dbgsName(Self),' Name="',Name,'"');
  {$ENDIF}
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
  if Visible then
    Result:=1
  else
    Result:=0;
end;

function TIDEMenuItem.GetPath: string;
var
  Item: TIDEMenuItem;
begin
  Result:=Name;
  Item:=Section;
  while Item<>nil do begin
    Result:=Item.Name+'/'+Result;
    Item:=Item.Section;
  end;
end;

{ TIDEMenuSection }

procedure TIDEMenuSection.SetSubMenuImages(const AValue: TCustomImageList);
begin
  if FSubMenuImages=AValue then exit;
  FSubMenuImages:=AValue;
end;

procedure TIDEMenuSection.ClearMenuItems;
var
  i: Integer;
begin
  inherited ClearMenuItems;
  for i:=0 to Count-1 do Items[i].MenuItem:=nil;
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
  AnItem.fName:=CreateUniqueName(AnItem.Name);
  FItems.Insert(Index,AnItem);
  AnItem.FSection:=Self;
  CreateChildMenuItem(Index);
end;

procedure TIDEMenuSection.CreateChildMenuItem(Index: Integer);
var
  Item: TIDEMenuItem;
  SubSection: TIDEMenuSection;
  i: Integer;
  ContainerMenuItem: TMenuItem;
  MenuIndex: Integer;
begin
  ContainerMenuItem:=GetContainerMenuItem;
  if (ContainerMenuItem=nil) then exit;

  Item:=Items[Index];
  {$IFDEF VerboseMenuIntf}
  debugln('TIDEMenuSection.CreateChildMenuItem ',dbgsName(Self),' Name=',Name,' Index=',dbgs(Index),' Item=',Item.Name,' Container=',ContainerMenuItem.Name);
  {$ENDIF}

  MenuIndex:=GetChildsStartIndex+Index;

  if NeedTopSeparator then begin
    if (TopSeparator=nil) then begin
      // create TopSeparator
      FTopSeparator:=MenuItemClass.Create(nil);
      FTopSeparator.Caption:='-';
      MenuItem.Insert(GetChildsStartIndex,FTopSeparator);
    end;
    inc(MenuIndex);
  end;
  
  // create the child TMenuItem
  Item.CreateMenuItem;
  MenuItem.Insert(MenuIndex,Item.MenuItem);
  // create the subsections
  if Item is TIDEMenuSection then begin
    SubSection:=TIDEMenuSection(Item);
    for i:=0 to SubSection.Count-1 do
      SubSection.CreateChildMenuItem(i);
  end;
  
  if (Index=Count-1) and NeedBottomSeparator and (BottomSeparator=nil) then
  begin
    // create bottom separator
    FBottomSeparator:=MenuItemClass.Create(nil);
    FBottomSeparator.Caption:='-';
    MenuItem.Insert(MenuIndex+1,FBottomSeparator);
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
  if not Section.ChildsAsSubMenu then
    inc(Result,Section.GetChildsStartIndex);
end;

function TIDEMenuSection.Size: Integer;
begin
  if Visible then begin
    Result:=1;
    if (Section<>nil) and (not ChildsAsSubMenu) then begin
      // childs are not in a submenu but directly added to parents menuitem
      Result:=Count;
      if NeedTopSeparator then inc(Result);
      if NeedBottomSeparator then inc(Result);
    end;
  end else begin
    Result:=0;
  end;
end;

function TIDEMenuSection.IndexOf(AnItem: TIDEMenuItem): Integer;
begin
  Result:=FItems.IndexOf(AnItem);
end;

function TIDEMenuSection.NeedTopSeparator: Boolean;
begin
  if (not ChildsAsSubMenu) and (Section<>nil) and (Section.Items[0]<>Self) then
    Result:=true
  else
    Result:=false;
end;

function TIDEMenuSection.NeedBottomSeparator: Boolean;
var
  SelfIndex: LongInt;
  NextSibling: TIDEMenuItem;
begin
  Result:=false;
  if (not ChildsAsSubMenu) and (Section<>nil)
  and (Section.Items[Section.Count-1]<>Self) then begin
    SelfIndex:=Section.IndexOf(Self);
    NextSibling:=Section[SelfIndex-1];
    if (not (NextSibling is TIDEMenuSection))
    or (not TIDEMenuSection(NextSibling).ChildsAsSubMenu) then begin
      // a bottom separator is needed
      Result:=true;
    end;
  end;
end;

function TIDEMenuSection.GetContainerMenuItem: TMenuItem;
var
  IDEMenuItem: TIDEMenuSection;
begin
  IDEMenuItem:=Self;
  while (IDEMenuItem<>nil) and (not IDEMenuItem.ChildsAsSubMenu) do
    IDEMenuItem:=IDEMenuItem.Section;
  if (IDEMenuItem<>nil) then
    Result:=IDEMenuItem.MenuItem;
end;

function TIDEMenuSection.IndexByName(const AName: string): Integer;
begin
  Result:=Count-1;
  while (Result>=0) and (CompareText(AName,Items[Result].Name)<>0) do
    dec(Result);
end;

function TIDEMenuSection.FindByName(const AName: string): TIDEMenuItem;
var
  i: LongInt;
begin
  i:=IndexByName(AName);
  if i>=0 then
    Result:=Items[i]
  else
    Result:=nil;
end;

function TIDEMenuSection.CreateUniqueName(const AName: string): string;
begin
  Result:=AName;
  if IndexByName(Result)<0 then exit;
  Result:=CreateFirstIdentifier(Result);
  while IndexByName(Result)>=0 do
    Result:=CreateNextIdentifier(Result);
end;

function TIDEMenuSection.GetItems(Index: Integer): TIDEMenuItem;
begin
  Result:=TIDEMenuItem(FItems[Index]);
end;

procedure TIDEMenuSection.SetMenuItem(const AValue: TMenuItem);
var
  i: Integer;
begin
  if MenuItem=AValue then exit;
  inherited SetMenuItem(AValue);
  if MenuItem<>nil then begin
    for i:=0 to Count-1 do CreateChildMenuItem(i);
  end;
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

function TIDEMenuRoots.GetItems(Index: integer): TIDEMenuSection;
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
  FItems.Free;
  inherited Destroy;
end;

procedure TIDEMenuRoots.RegisterMenuRoot(Section: TIDEMenuSection);
begin
  Section.FName:=CreateUniqueName(Section.Name);
  FItems.Add(Section);
end;

procedure TIDEMenuRoots.UnregisterMenuRoot(Section: TIDEMenuSection);
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
  for i:=FItems.Count-1 downto 0 do begin
    Items[i].ClearMenuItems;
    Items[i].Free;
  end;
  FItems.Clear;
end;

procedure TIDEMenuRoots.Delete(Index: Integer);
var
  OldItem: TIDEMenuSection;
begin
  OldItem:=Items[Index];
  UnregisterMenuRoot(OldItem);
  OldItem.Free;
end;

function TIDEMenuRoots.IndexByName(const Name: string): Integer;
begin
  Result:=Count-1;
  while (Result>=0) and (CompareText(Name,Items[Result].Name)<>0) do
    dec(Result);
end;

function TIDEMenuRoots.FindByName(const Name: string): TIDEMenuSection;
var
  i: LongInt;
begin
  i:=IndexByName(Name);
  if i>=0 then
    Result:=Items[i]
  else
    Result:=nil;
end;

function TIDEMenuRoots.CreateUniqueName(const Name: string): string;
begin
  Result:=Name;
  if IndexByName(Result)<0 then exit;
  Result:=CreateFirstIdentifier(Result);
  while IndexByName(Result)>=0 do
    Result:=CreateNextIdentifier(Result);
end;

function TIDEMenuRoots.FindByPath(const Path: string;
  ErrorOnNotFound: boolean): TIDEMenuItem;
var
  StartPos: Integer;
  EndPos: LongInt;
  Name: String;
begin
  Result:=nil;
  StartPos:=1;
  while StartPos<=length(Path) do begin
    EndPos:=StartPos;
    while (EndPos<=length(Path)) and (Path[EndPos]<>'/') do inc(EndPos);
    if EndPos>StartPos then begin
      Name:=copy(Path,StartPos,EndPos-StartPos);
      if Result=nil then
        // search root
        Result:=FindByName(Name)
      else if Result is TIDEMenuSection then
        // search child
        Result:=TIDEMenuSection(Result).FindByName(Name)
      else
        // path too long -> we are already at a leaf
        Result:=nil;
      if Result=nil then break;
    end;
    StartPos:=EndPos+1;
  end;
  if Result=nil then begin
    if ErrorOnNotFound then begin
      raise Exception.Create('IDE Menu path not found: '+Path);
    end;
  end;
end;

end.

