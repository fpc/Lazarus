{
 *****************************************************************************
  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************

  Author: Mattias Gaertner

  Abstract:
    Interface to the IDE menus.
}
unit MenuIntf;

{$mode objfpc}{$H+}

{off $DEFINE VerboseMenuIntf}

interface

uses
  Classes, SysUtils,
  // LCL
  LCLType, LCLProc, Menus, ImgList, Graphics,
  // LazUtils
  LazMethodList, LazLoggerBase,
  // IdeIntf
  IDECommands, IDEImagesIntf;
  
type
  TIDEMenuItem = class;
  TIDEMenuCommand = class;
  TIDEMenuSection = class;

  TAddMenuItemProc =
    function (const NewCaption: string; const NewEnabled: boolean;
              const NewOnClick: TNotifyEvent): TIDEMenuItem of object;

  { TIDEMenuItem
    A menu item in one of the IDE's menus.
    This is only the base class for TIDEMenuSection and TIDEMenuCommand }
    
  TIDEMenuItem = class(TIDESpecialCommand)
  private
    FBitmap: TBitmap;
    FMenuItem: TMenuItem;
    FMenuItemClass: TMenuItemClass;
    FSection: TIDEMenuSection; // parent section
    FSectionIndex: Integer;// index in parent section
    FTag: Integer;
    FUserTag: PtrUInt;
    FVisible: Boolean;
    FVisibleCommandCount: integer;
    procedure MenuItemDestroy(Sender: TObject);
    procedure BitmapChange(Sender: TObject);
  protected
    procedure RealizeVisible;
    procedure SetCommand(const AValue: TIDECommand); override;
    procedure MenuItemClick(Sender: TObject); virtual;
    function GetBitmap: TBitmap; virtual;
    procedure SetBitmap(const AValue: TBitmap); virtual;
    procedure SetCaption(AValue: string); override;
    procedure SetEnabled(const AValue: Boolean); override;
    procedure SetChecked(const AValue: Boolean); override;
    procedure SetHint(const AValue: String); override;
    procedure SetImageIndex(const AValue: Integer); override;
    procedure SetMenuItem(const AValue: TMenuItem); virtual;
    procedure SetSection(const AValue: TIDEMenuSection); virtual;
    procedure SetVisible(const AValue: Boolean); virtual;
    procedure ClearMenuItems; virtual;
    procedure ShortCutsUpdated(const aShortCut, aShortCutKey2: TShortCut); override;
  public
    constructor Create(const TheName: string); override;
    destructor Destroy; override;
    function GetImageList: TCustomImageList; virtual;
    function HasBitmap: Boolean;
    procedure CreateMenuItem; virtual; // only create and set properties, does not add to Section.MenuItem
    function GetPath: string;
    function GetRoot: TIDEMenuItem;
    function VisibleActive: boolean; virtual;
    function RealVisible: boolean; // this one and all parent sections are visible
    function GetContainerSection: TIDEMenuSection; // returns nearest sub menu section
    function GetContainerMenuItem: TMenuItem; // returns nearest sub menu
    function GetNextSameContainer: TIDEMenuItem;
    function GetPrevSameContainer: TIDEMenuItem;
    function HasAsParent(Item: TIDEMenuItem): boolean;
    procedure WriteDebugReport(const Prefix: string;
                               MenuItemDebugReport: boolean); virtual;
    procedure ConsistencyCheck; virtual;
  public
    property Bitmap: TBitmap read GetBitmap write SetBitmap;
    property Section: TIDEMenuSection read FSection write SetSection;
    property MenuItem: TMenuItem read FMenuItem write SetMenuItem; // Note: root section MenuItem = TMenu.Items, setting a non root does not add to Section.MenuItem
    property MenuItemClass: TMenuItemClass read FMenuItemClass write FMenuItemClass;
    property SectionIndex: Integer read FSectionIndex;
    property Tag: Integer read FTag write FTag;
    property UserTag: PtrUInt read FUserTag write FUserTag;
    property Visible: Boolean read FVisible write SetVisible;
    property VisibleCommandCount: integer read FVisibleCommandCount; // with grandchildren
  end;
  TIDEMenuItemClass = class of TIDEMenuItem;

  { TIDEMenuSection
    A TIDEMenuItem with children, either in a sub menu or separated with
    separators.
    If no children are visible, the section will not be visible.
    }

  TIDEMenuSectionState = (
    imssClearing
    );
  TIDEMenuSectionStates = set of TIDEMenuSectionState;

  TIDEMenuSectionHandlerType = (
    imshtOnShow  // called before showing. Use this to enable/disable context sensitive items.
    );

  TIDEMenuSection = class(TIDEMenuItem)
  private
    FBottomSeparator: TMenuItem;
    FChildrenAsSubMenu: boolean;
    FItems: TFPList;
    FSectionHandlers: array[TIDEMenuSectionHandlerType] of TMethodList;
    FStates: TIDEMenuSectionStates;
    FSubMenuImages: TCustomImageList;
    FTopSeparator: TMenuItem;
    procedure OnSeparatorDestroy(Sender: TObject);
    function GetItems(Index: Integer): TIDEMenuItem;
    procedure AddHandler(HandlerType: TIDEMenuSectionHandlerType;
                         const AMethod: TMethod; AsLast: boolean = false);
    procedure RemoveHandler(HandlerType: TIDEMenuSectionHandlerType;
                            const AMethod: TMethod);
  protected
    procedure MenuItemClick(Sender: TObject); override;
    procedure SetChildrenAsSubMenu(const AValue: boolean); virtual;
    procedure SetVisible(const AValue: Boolean); override;
    procedure SetSubMenuImages(const AValue: TCustomImageList); virtual;
    procedure SetMenuItem(const AValue: TMenuItem); override;
    procedure ClearMenuItems; override;
    procedure FreeTopSeparator;
    procedure FreeBottomSeparator;
    procedure UpdateAllChildrenIndex(StartIndex: Integer);
    procedure UpdateTopSeparator(ParentMenuItem: TMenuItem; var aMenuIndex: integer);
    procedure UpdateBottomSeparator(ParentMenuItem: TMenuItem; var aMenuIndex: integer);
    procedure UpdateContainer;
    procedure UpdateSubMenus;
    procedure UpdateVisibleCommandCount(Add: integer);
  public
    constructor Create(const TheName: string); override;
    destructor Destroy; override;
    procedure Clear;
    function Count: Integer;
    procedure AddFirst(AnItem: TIDEMenuItem);
    procedure AddLast(AnItem: TIDEMenuItem);
    procedure Insert(Index: Integer; AnItem: TIDEMenuItem);
    procedure Remove(AnItem: TIDEMenuItem);
    procedure CreateMenuItem; override;
    function IndexOf(AnItem: TIDEMenuItem): Integer;
    function IndexByName(const AName: string): Integer;
    function FindByName(const AName: string): TIDEMenuItem;
    function CreateUniqueName(const AName: string): string;
    function VisibleActive: boolean; override;
    function NeedTopSeparator: boolean;
    function NeedBottomSeparator: boolean;
    function GetFirstChildSameContainer: TIDEMenuItem;
    function GetLastChildSameContainer: TIDEMenuItem;
    procedure BeginUpdate; deprecated;
    procedure EndUpdate; deprecated;

    procedure NotifySubSectionOnShow(Sender: TObject;
                                     WithChildren: Boolean = true); virtual;
    procedure RemoveAllHandlersOfObject(AnObject: TObject);
    procedure AddHandlerOnShow(const OnShowEvent: TNotifyEvent;
                               AsLast: boolean = false);
    procedure RemoveHandlerOnShow(const OnShowEvent: TNotifyEvent);
    procedure WriteDebugReport(const Prefix: string;
                               MenuItemDebugReport: boolean); override;
    procedure ConsistencyCheck; override;
  public
    property ChildrenAsSubMenu: boolean read FChildrenAsSubMenu
                                          write SetChildrenAsSubMenu default true;
    property ChildsAsSubMenu: boolean read FChildrenAsSubMenu
                                          write SetChildrenAsSubMenu default true; deprecated;// use ChildrenAsSubMenu instead
    property SubMenuImages: TCustomImageList read FSubMenuImages
                                             write SetSubMenuImages;
    property Items[Index: Integer]: TIDEMenuItem read GetItems; default;
    property TopSeparator: TMenuItem read FTopSeparator;
    property BottomSeparator: TMenuItem read FBottomSeparator;
    property States: TIDEMenuSectionStates read FStates;
  end;
  TIDEMenuSectionClass = class of TIDEMenuSection;

  { TIDEMenuCommand
    A leaf menu item. No children.
    Hint: The shortcut is defined via the Command property.
  }
  TIDEMenuCommand = class(TIDEMenuItem)
  private
    FAutoCheck: boolean;
    FDefault: Boolean;
    FGroupIndex: Byte;
    FRadioItem: Boolean;
    FRightJustify: boolean;
    FShowAlwaysCheckable: boolean;
  protected
    procedure MenuItemClick(Sender: TObject); override;
    procedure SetAutoCheck(const AValue: boolean); virtual;
    procedure SetDefault(const AValue: Boolean); virtual;
    procedure SetGroupIndex(const AValue: Byte); virtual;
    procedure SetRadioItem(const AValue: Boolean); virtual;
    procedure SetRightJustify(const AValue: boolean); virtual;
    procedure SetShowAlwaysCheckable(const AValue: boolean); virtual;
    procedure SetMenuItem(const AValue: TMenuItem); override;
  public
    constructor Create(const TheName: string); override;
    procedure ConsistencyCheck; override;
    property AutoCheck: boolean read FAutoCheck write SetAutoCheck default False;
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

  // IDE MainMenu
  mnuMain: TIDEMenuSection = nil;

    // file menu
    mnuFile: TIDEMenuSection;
      itmFileNew: TIDEMenuSection;
      itmFileOpenSave: TIDEMenuSection;
        itmFileRecentOpen: TIDEMenuSection;
      itmFileDirectories: TIDEMenuSection;
      itmFileIDEStart: TIDEMenuSection;

    // edit menu
    mnuEdit: TIDEMenuSection;
      itmEditReUndo: TIDEMenuSection;
      itmEditClipboard: TIDEMenuSection;
      itmEditSelect: TIDEMenuSection;
      itmEditBlockActions: TIDEMenuSection;
      itmEditInsertions: TIDEMenuSection;

    // search menu
    mnuSearch: TIDEMenuSection;
      itmSearchFindReplace: TIDEMenuSection;
      itmJumpings: TIDEMenuSection;
        itmJumpToSection: TIDEMenuSection;
      itmBookmarks: TIDEMenuSection;
      itmCodeToolSearches: TIDEMenuSection;

    // view menu
    mnuView: TIDEMenuSection;
      itmViewMainWindows: TIDEMenuSection;
      itmViewDesignerWindows: TIDEMenuSection;
      itmViewSecondaryWindows: TIDEMenuSection;
        itmViewDebugWindows: TIDEMenuSection;
        itmViewIDEInternalsWindows: TIDEMenuSection;

    // source menu
    mnuSource: TIDEMenuSection;
      itmSourceBlockActions: TIDEMenuSection;
      itmSourceCodeToolChecks: TIDEMenuSection;
      itmSourceRefactor: TIDEMenuSection;
        itmRefactorCodeTools: TIDEMenuSection;
        itmRefactorAdvanced: TIDEMenuSection;
        itmRefactorTools: TIDEMenuSection;
      itmSourceInsertions: TIDEMenuSection;
        itmSourceInsertCVSKeyWord: TIDEMenuSection;
        itmSourceInsertGeneral: TIDEMenuSection;
      itmSourceTools: TIDEMenuSection;

    // project menu
    mnuProject: TIDEMenuSection;
      itmProjectNewSection: TIDEMenuSection;
      itmProjectOpenSection: TIDEMenuSection;
        itmProjectRecentOpen: TIDEMenuSection;
      itmProjectSaveSection: TIDEMenuSection;
      itmProjectWindowSection: TIDEMenuSection;
      itmProjectAddRemoveSection: TIDEMenuSection;

    // run menu
    mnuRun: TIDEMenuSection;
      itmRunBuilding: TIDEMenuSection;
      itmRunnning: TIDEMenuSection;
      itmRunBuildingFile: TIDEMenuSection;
      itmRunDebugging: TIDEMenuSection;
        itmRunMenuAddBreakpoint: TIDEMenuSection;

    // package menu
    mnuPackage: TIDEMenuSection;
    mnuComponent: TIDEMenuSection; // = mnuPackage, for compatibility with older lazarus versions
      itmPkgOpening: TIDEMenuSection;
        itmPkgOpenRecent: TIDEMenuSection;
      itmPkgUnits: TIDEMenuSection;
      itmPkgGraphSection: TIDEMenuSection;

    // tools menu
    mnuTools: TIDEMenuSection;
      itmOptionsDialogs: TIDEMenuSection;
      itmCustomTools: TIDEMenuSection;
      itmSecondaryTools: TIDEMenuSection;
      itmConversion: TIDEMenuSection;
      itmDelphiConversion: TIDEMenuSection;
      itmBuildingLazarus: TIDEMenuSection;

    // windows menu
    mnuWindow: TIDEMenuSection;
      itmWindowManagers: TIDEMenuSection;
      itmWindowLists: TIDEMenuSection;
      itmCenterWindowLists: TIDEMenuSection;
      itmTabLists: TIDEMenuSection;
        itmTabListProject: TIDEMenuSection;
        itmTabListOther: TIDEMenuSection;
        itmTabListPackage: TIDEMenuSection;

    // help menu
    mnuHelp: TIDEMenuSection;
      itmOnlineHelps: TIDEMenuSection;
      itmInfoHelps: TIDEMenuSection;
      itmHelpTools: TIDEMenuSection;

  // Source Editor's tab: Popupmenu
  SourceTabMenuRoot: TIDEMenuSection = nil;
    SrcEditMenuSectionPages: TIDEMenuSection;
      SrcEditSubMenuMovePage: TIDEMenuSection;
    SrcEditMenuSectionEditors: TIDEMenuSection;


  // Source Editor(s): Popupmenu
  SourceEditorMenuRoot: TIDEMenuSection = nil;
    // Source Editor: First dynamic section for often used context sensitive stuff
    //                The items are cleared automatically after each popup.
    SrcEditMenuSectionFirstDynamic: TIDEMenuSection;
    SrcEditMenuSectionClipboard: TIDEMenuSection;
    SrcEditMenuSectionFirstStatic: TIDEMenuSection;
      SrcEditSubMenuFind: TIDEMenuSection;
    SrcEditMenuSectionFiles: TIDEMenuSection;
      SrcEditSubMenuOpenFile: TIDEMenuSection;
        // Source Editor: File Specific dynamic section
        //                The items are cleared automatically after each popup.
        SrcEditMenuSectionFileDynamic: TIDEMenuSection;
    SrcEditMenuSectionMarks: TIDEMenuSection;
      SrcEditSubMenuGotoBookmarks: TIDEMenuSection;
      SrcEditSubMenuToggleBookmarks: TIDEMenuSection;
    SrcEditMenuSectionDebug: TIDEMenuSection;
      SrcEditSubMenuDebug: TIDEMenuSection;
    SrcEditSubMenuSource: TIDEMenuSection;
    SrcEditSubMenuRefactor: TIDEMenuSection;
    SrcEditSubMenuFlags: TIDEMenuSection;
      SrcEditSubMenuHighlighter: TIDEMenuSection;
      SrcEditSubMenuLineEnding: TIDEMenuSection;
      SrcEditSubMenuEncoding: TIDEMenuSection;

  // Messages window popupmenu
  MessagesMenuRoot: TIDEMenuSection = nil;

  // CodeExplorer window popupmenu
  CodeExplorerMenuRoot: TIDEMenuSection = nil;

  // Code templates popupmenu
  CodeTemplatesMenuRoot: TIDEMenuSection = nil;

  // Designer: Popupmenu
  DesignerMenuRoot: TIDEMenuSection = nil;
    // Designer: Dynamic section for component editor
    DesignerMenuSectionComponentEditor: TIDEMenuSection;
    // Designer: custom dynamic section
    DesignerMenuSectionCustomDynamic: TIDEMenuSection;
    DesignerMenuSectionAlign: TIDEMenuSection;
    DesignerMenuSectionOrder: TIDEMenuSection;
      DesignerMenuSectionZOrder: TIDEMenuSection;
    DesignerMenuSectionClipboard: TIDEMenuSection;
    DesignerMenuSectionMisc: TIDEMenuSection;
    DesignerMenuSectionOptions: TIDEMenuSection;

  // Package editor(s)
  PackageEditorMenuRoot: TIDEMenuSection = nil;
    PkgEditMenuSectionFiles: TIDEMenuSection; // e.g. sort files, clean up files
    PkgEditMenuSectionUse: TIDEMenuSection; // e.g. install, add to project
    PkgEditMenuSectionSave: TIDEMenuSection; // e.g. save as, revert, publish
    PkgEditMenuSectionCompile: TIDEMenuSection; // e.g. build clean, create Makefile
    PkgEditMenuSectionAddRemove: TIDEMenuSection; // e.g. add unit, add dependency
    PkgEditMenuSectionMisc: TIDEMenuSection; // e.g. options
  PackageEditorMenuFilesRoot: TIDEMenuSection = nil;
    PkgEditMenuSectionFile: TIDEMenuSection; // e.g. open file, remove file, move file up/down
    PkgEditMenuSectionDirectory: TIDEMenuSection; // e.g. change all properties of all files in a directory and ub directories moved ..
    PkgEditMenuSectionDependency: TIDEMenuSection; // e.g. open package, remove dependency

function RegisterIDEMenuRoot(const Name: string; MenuItem: TMenuItem = nil
                             ): TIDEMenuSection;
function RegisterIDEMenuSection(Parent: TIDEMenuSection;
                                const Name: string): TIDEMenuSection; overload;
function RegisterIDEMenuSection(const Path, Name: string): TIDEMenuSection; overload;
function RegisterIDESubMenu(Parent: TIDEMenuSection;
                            const Name, Caption: string;
                            const OnClickMethod: TNotifyEvent = nil;
                            const OnClickProc: TNotifyProcedure = nil;
                            const ResourceName: String = ''
                            ): TIDEMenuSection; overload;
function RegisterIDESubMenu(const Path, Name, Caption: string;
                            const OnClickMethod: TNotifyEvent = nil;
                            const OnClickProc: TNotifyProcedure = nil;
                            const ResourceName: String = ''
                            ): TIDEMenuSection; overload;
function RegisterIDEMenuCommand(Parent: TIDEMenuSection;
                                const Name, Caption: string;
                                const OnClickMethod: TNotifyEvent = nil;
                                const OnClickProc: TNotifyProcedure = nil;
                                const Command: TIDECommand = nil;
                                const ResourceName: String = '';
                                const UserTag: PtrUint = 0
                                ): TIDEMenuCommand; overload;
function RegisterIDEMenuCommand(const Path, Name, Caption: string;
                                const OnClickMethod: TNotifyEvent = nil;
                                const OnClickProc: TNotifyProcedure = nil;
                                const Command: TIDECommand = nil;
                                const ResourceName: String = '';
                                const UserTag: PtrUInt = 0
                                ): TIDEMenuCommand; overload;

implementation

function RegisterIDEMenuRoot(const Name: string; MenuItem: TMenuItem
  ): TIDEMenuSection;
begin
  //debugln('RegisterIDEMenuRoot Name="',Name,'"');
  Result:=TIDEMenuSection.Create(Name);
  IDEMenuRoots.RegisterMenuRoot(Result);
  Result.MenuItem:=MenuItem;
end;

function RegisterIDEMenuSection(Parent: TIDEMenuSection; const Name: string
  ): TIDEMenuSection;
begin
  Result:=TIDEMenuSection.Create(Name);
  Result.ChildrenAsSubMenu:=false;
  Parent.AddLast(Result);
end;

function RegisterIDEMenuSection(const Path, Name: string): TIDEMenuSection;
var
  Parent: TIDEMenuSection;
begin
  //debugln('RegisterIDEMenuSection Path="',Path,'" Name="',Name,'"');
  Parent:=IDEMenuRoots.FindByPath(Path,true) as TIDEMenuSection;
  Result:=RegisterIDEMenuSection(Parent,Name);
end;

function RegisterIDESubMenu(Parent: TIDEMenuSection; const Name,
  Caption: string; const OnClickMethod: TNotifyEvent;
  const OnClickProc: TNotifyProcedure;
  const ResourceName: String): TIDEMenuSection;
begin
  Result := TIDEMenuSection.Create(Name);
  Result.ChildrenAsSubMenu := True;
  Result.Caption := Caption;
  Result.OnClick := OnClickMethod;
  Result.OnClickProc := OnClickProc;
  Result.ResourceName := ResourceName;
  Parent.AddLast(Result);
end;

function RegisterIDESubMenu(const Path, Name, Caption: string;
  const OnClickMethod: TNotifyEvent; const OnClickProc: TNotifyProcedure;
  const ResourceName: String): TIDEMenuSection;
var
  Parent: TIDEMenuSection;
begin
  //debugln('RegisterIDESubMenu Path="',Path,'" Name="',Name,'"');
  Parent := IDEMenuRoots.FindByPath(Path,true) as TIDEMenuSection;
  Result := RegisterIDESubMenu(Parent, Name, Caption, OnClickMethod, OnClickProc,
    ResourceName);
end;

function RegisterIDEMenuCommand(Parent: TIDEMenuSection;
                                const Name, Caption: string;
                                const OnClickMethod: TNotifyEvent = nil;
                                const OnClickProc: TNotifyProcedure = nil;
                                const Command: TIDECommand = nil;
                                const ResourceName: String = '';
                                const UserTag: PtrUInt = 0
                                ): TIDEMenuCommand;
var
  s: String;
begin
  Result := TIDEMenuCommand.Create(Name);
  s:=Caption;
  if (s='') and (Command<>nil) then s:=Command.LocalizedName;
  Result.Caption := s;
  Result.OnClick := OnClickMethod;
  Result.OnClickProc := OnClickProc;
  Result.Command := Command;
  Result.ResourceName := ResourceName;
  Result.UserTag := UserTag;
  Parent.AddLast(Result);
end;

function RegisterIDEMenuCommand(const Path, Name, Caption: string;
                                const OnClickMethod: TNotifyEvent = nil;
                                const OnClickProc: TNotifyProcedure = nil;
                                const Command: TIDECommand = nil;
                                const ResourceName: String = '';
                                const UserTag: PtrUInt = 0
                                ): TIDEMenuCommand;
var
  Parent: TIDEMenuSection;
begin
  //debugln('RegisterIDEMenuCommand Path="',Path,'" Name="',Name,'"');
  Parent := IDEMenuRoots.FindByPath(Path,true) as TIDEMenuSection;
  Result := RegisterIDEMenuCommand(Parent, Name, Caption,
    OnClickMethod, OnClickProc, Command, ResourceName, UserTag);
end;

{ TIDEMenuItem }

procedure TIDEMenuItem.MenuItemClick(Sender: TObject);
begin
  if Assigned(OnClick) then
    OnClick(Self)
  else
  if Assigned(OnClickProc) then
    OnClickProc(Self);
end;

procedure TIDEMenuItem.MenuItemDestroy(Sender: TObject);
begin
  FMenuItem:=nil;
end;

function TIDEMenuItem.RealVisible: boolean;
begin
  Result := VisibleActive;
  if Result and (Section<>nil) then
    Result:=Section.RealVisible;
end;

procedure TIDEMenuItem.BitmapChange(Sender: TObject);
begin
  if MenuItem<>nil then MenuItem.Bitmap:=Bitmap;
end;

procedure TIDEMenuItem.RealizeVisible;
begin
  if MenuItem=nil then exit;
  MenuItem.Visible:=RealVisible
    or (Section=nil); // keep the root menuitem always visible
end;

procedure TIDEMenuItem.SetEnabled(const AValue: Boolean);
begin
  if Enabled=AValue then exit;
  inherited SetEnabled(AValue);
  if MenuItem<>nil then
    MenuItem.Enabled:=Enabled;
end;

function TIDEMenuItem.GetBitmap: TBitmap;
begin
  if FBitmap=nil then begin
    FBitmap:=TBitmap.Create;
    FBitmap.OnChange:=@BitmapChange;
  end;
  FBitmap.Transparent:=True;
  Result:=FBitmap;
end;

procedure TIDEMenuItem.SetBitmap(const AValue: TBitmap);
begin
  if FBitmap=AValue then exit;
  if AValue<>nil then
    Bitmap.Assign(AValue)
  else
    FreeAndNil(FBitmap);
  if MenuItem<>nil then
    MenuItem.Bitmap:=FBitmap;
end;

procedure TIDEMenuItem.SetCaption(AValue: string);
begin
  if Caption=AValue then Exit;
  inherited SetCaption(AValue);
  if MenuItem<>nil then
    MenuItem.Caption:=Caption;
end;

procedure TIDEMenuItem.SetChecked(const AValue: Boolean);
begin
  if Checked=AValue then exit;
  inherited SetChecked(AValue);
  if MenuItem<>nil then
    MenuItem.Checked:=Checked;
end;

procedure TIDEMenuItem.SetCommand(const AValue: TIDECommand);
var
  I: Integer;
  xUser: TIDESpecialCommand;
begin
  inherited SetCommand(AValue);
  //copy properties to other command users to support legacy code
  if (AValue<>nil) and SyncAvailable then
    for I := 0 to AValue.UserCount-1 do
      if AValue.Users[I] <> Self then
      begin
        xUser:=AValue.Users[I];
        xUser.BlockSync;
        try
          xUser.Caption:=Caption;
          xUser.Hint:=Hint;
          xUser.ImageIndex:=ImageIndex;
          xUser.Enabled:=Enabled;
        finally
          xUser.UnblockSync;
        end;
      end;
end;

procedure TIDEMenuItem.SetHint(const AValue: String);
begin
  if Hint=AValue then Exit;
  inherited SetHint(AValue);
  if MenuItem<>nil then
    MenuItem.Hint:=Hint;
end;

procedure TIDEMenuItem.SetImageIndex(const AValue: Integer);
begin
  if ImageIndex=AValue then exit;
  inherited SetImageIndex(AValue);
  if MenuItem<>nil then
    MenuItem.ImageIndex:=ImageIndex;
end;

procedure TIDEMenuItem.SetMenuItem(const AValue: TMenuItem);
// only set the properties, do not update container
begin
  if FMenuItem = AValue then exit;
  if FMenuItem <> nil then ClearMenuItems;
  FMenuItem := AValue;
  if MenuItem <> nil then
  begin
    MenuItem.AddHandlerOnDestroy(@MenuItemDestroy);
    MenuItem.Caption := Caption;
    MenuItem.Bitmap := FBitmap;
    MenuItem.Hint := Hint;
    MenuItem.ImageIndex := ImageIndex;
    MenuItem.Enabled := Enabled;
    MenuItem.OnClick := @MenuItemClick;
    MenuItem.ImageIndex := ImageIndex;
    RealizeVisible;
  end;
end;

procedure TIDEMenuItem.SetSection(const AValue: TIDEMenuSection);
begin
  if FSection=AValue then exit;
  if Section<>nil then
    Section.Remove(Self)
  else begin
    ClearMenuItems;
    FSection:=nil;
    FSectionIndex:=-1;
  end;
  if FSection<>nil then
    FSection.AddLast(Self);
end;

procedure TIDEMenuItem.SetVisible(const AValue: Boolean);
var
  OldVisibleActive: Boolean;
begin
  if Visible=AValue then exit;
  OldVisibleActive:=VisibleActive;
  FVisible:=AValue;
  if MenuItem<>nil then
    RealizeVisible;
  if (VisibleActive<>OldVisibleActive) and (Section<>nil)
  and (VisibleCommandCount>0) then begin
    if Visible then
      Section.UpdateVisibleCommandCount(VisibleCommandCount)
    else
      Section.UpdateVisibleCommandCount(-VisibleCommandCount);
  end;
end;

procedure TIDEMenuItem.ShortCutsUpdated(const aShortCut,
  aShortCutKey2: TShortCut);
begin
  inherited ShortCutsUpdated(aShortCut, aShortCutKey2);
  if MenuItem<>nil then
  begin
    MenuItem.ShortCut:=aShortCut;
    MenuItem.ShortCutKey2:=aShortCutKey2;
  end;
end;

procedure TIDEMenuItem.ClearMenuItems;
begin
  if FMenuItem <> nil then begin
    FMenuItem.OnClick := nil;
    FMenuItem.RemoveHandlerOnDestroy(@MenuItemDestroy);
    if (FMenuItem.Menu=nil) and ((Section<>nil) or (FMenuItem.Parent<>nil)) then
      FMenuItem.Free;
    FMenuItem:=nil;
  end;
end;

constructor TIDEMenuItem.Create(const TheName: string);
begin
  inherited Create(TheName);
  FVisible:=true;
  FMenuItemClass:=TMenuItem;
  FSectionIndex:=-1;
  {$IFDEF VerboseMenuIntf}
  debugln('TIDEMenuItem.Create ',dbgsName(Self),' Name="',Name,'"');
  {$ENDIF}
end;

destructor TIDEMenuItem.Destroy;
begin
  {$IFDEF VerboseMenuIntf}
  debugln('TIDEMenuItem.Destroy ',dbgsName(Self),' Name="',Name,'"');
  {$ENDIF}
  Section:=nil;
  ClearMenuItems;
  FreeAndNil(FBitmap);
  inherited Destroy;
end;

function TIDEMenuItem.GetImageList: TCustomImageList;
var
  CurSection: TIDEMenuSection;
  AMenu: TMenu;
begin
  Result:=nil;
  CurSection:=Section;
  while CurSection<>nil do begin
    Result:=CurSection.SubMenuImages;
    if Result<>nil then exit;
    if (CurSection.Section=nil) then begin
      if CurSection.MenuItem<>nil then begin
        AMenu:=CurSection.MenuItem.GetParentMenu;
        if AMenu<>nil then
          Result:=AMenu.Images;
      end;
      exit;
    end;
    CurSection:=CurSection.Section;
  end;
end;

function TIDEMenuItem.HasBitmap: Boolean;
begin
  Result:=(FBitmap<>nil) or ((ImageIndex>=0) and (GetImageList<>nil));
end;

procedure TIDEMenuItem.CreateMenuItem;
begin
  if FMenuItem<>nil then exit;
  {$IFDEF VerboseMenuIntf}
  //debugln('TIDEMenuItem.CreateMenuItem ',dbgsName(Self),' Name="',Name,'"');
  {$ENDIF}
  MenuItem:=MenuItemClass.Create(nil);
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

function TIDEMenuItem.GetRoot: TIDEMenuItem;
begin
  Result:=Self;
  while Result.Section<>nil do Result:=Result.Section;
end;

function TIDEMenuItem.VisibleActive: boolean;
// true if has visible content
begin
  Result:=Visible;
end;

function TIDEMenuItem.GetContainerSection: TIDEMenuSection;
begin
  if Self is TIDEMenuSection then
    Result:=TIDEMenuSection(Self)
  else
    Result:=Section;
  while (Result<>nil) and (not Result.ChildrenAsSubMenu) do
    Result:=Result.Section;
end;

function TIDEMenuItem.GetContainerMenuItem: TMenuItem;
var
  ASection: TIDEMenuSection;
begin
  ASection:=GetContainerSection;
  if (ASection<>nil) then
    Result:=ASection.MenuItem
  else
    Result:=nil;
end;

function TIDEMenuItem.GetNextSameContainer: TIDEMenuItem;
// find the next visible TIDEMenuItem in the container (i.e. same MenuItem.Parent)
// The result can be:
//  - a TIDEMenuCommand
//  - a TIDEMenuSection with ChildrenAsSubMenu=true
//  - a TIDEMenuSection with TopSeparator<>nil
var
  i: Integer;
  Sibling: TIDEMenuItem;
  SiblingSection: TIDEMenuSection;
begin
  Result:=nil;
  if Section=nil then exit;
  if Section.ChildrenAsSubMenu then
    exit; // Self is the last item -> there is no next
  for i:=SectionIndex+1 to Section.Count-1 do begin
    Sibling:=Section[i];
    if not Sibling.VisibleActive then continue;
    if Sibling is TIDEMenuSection then begin
      SiblingSection:=TIDEMenuSection(Sibling);
      if SiblingSection.ChildrenAsSubMenu
      or (SiblingSection.TopSeparator<>nil) then
        exit(SiblingSection);
      Result:=SiblingSection.GetFirstChildSameContainer;
    end else begin
      exit(Sibling as TIDEMenuCommand);
    end;
  end;
  // search behind parent Section
  Result:=Section.GetNextSameContainer;
end;

function TIDEMenuItem.GetPrevSameContainer: TIDEMenuItem;
// find the previous visible TIDEMenuItem in the container (i.e. same MenuItem.Parent)
// The result can be:
//  - a TIDEMenuCommand
//  - a TIDEMenuSection with ChildrenAsSubMenu=true
//  - a TIDEMenuSection with BottomSeparator<>nil
var
  i: Integer;
  Sibling: TIDEMenuItem;
  SiblingSection: TIDEMenuSection;
begin
  Result:=nil;
  if Section=nil then exit;
  if Section.ChildrenAsSubMenu then
    exit; // Self is the first item -> there is no previous
  for i:=SectionIndex-1 downto 0 do begin
    Sibling:=Section[i];
    if not Sibling.VisibleActive then continue;
    if Sibling is TIDEMenuSection then begin
      SiblingSection:=TIDEMenuSection(Sibling);
      if SiblingSection.ChildrenAsSubMenu
      or (SiblingSection.BottomSeparator<>nil) then
        exit(SiblingSection);
      Result:=SiblingSection.GetLastChildSameContainer;
    end else begin
      exit(Sibling as TIDEMenuCommand);
    end;
  end;
  // search in front of parent Section
  Result:=Section.GetNextSameContainer;
end;

function TIDEMenuItem.HasAsParent(Item: TIDEMenuItem): boolean;
var
  CurItem: TIDEMenuSection;
begin
  CurItem:=Section;
  while CurItem<>nil do begin
    if CurItem=Item then exit(true);
    CurItem:=CurItem.Section;
  end;
  Result:=false;
end;

procedure TIDEMenuItem.WriteDebugReport(const Prefix: string;
  MenuItemDebugReport: boolean);
begin
  debugln([Prefix,'SectionIndex=',dbgs(SectionIndex),' Name="',DbgStr(Name),'"',
    ' VisibleActive=',dbgs(VisibleActive),' Handle=',((MenuItem<>nil) and (MenuItem.HandleAllocated))]);
  if MenuItemDebugReport and (MenuItem<>nil) then
    MenuItem.WriteDebugReport(Prefix);
end;

procedure TIDEMenuItem.ConsistencyCheck;

  procedure RaiseError(const Msg: string = '');
  var
    s: String;
  begin
    s:='TIDEMenuItem.ConsistencyCheck Name="'+Name+'" Caption="'+DbgStr(Caption)+'"';
    if Msg<>'' then
      s+='. '+Msg;
    debugln(s);
    RaiseGDBException(s);
  end;

begin
  if MenuItem<>nil then begin
    //debugln(['TIDEMenuItem.ConsistencyCheck: Bitmap=', FBitmap,
    //         ', ImageIndex=', ImageIndex, ', ImageList=', GetImageList]);
    if MenuItem.Enabled<>Enabled then
      RaiseError('MenuItem.Enabled='+dbgs(MenuItem.Enabled)+' Enabled='+dbgs(Enabled));
    if MenuItem.Visible<>(RealVisible or (Section=nil)) then
      RaiseError('MenuItem.Visible='+dbgs(MenuItem.Visible)+' VisibleActive='+dbgs(VisibleActive)+' Visible='+dbgs(Visible)+' RealVisible='+dbgs(RealVisible));
    if MenuItem.Caption<>Caption then
      RaiseError;
    if MenuItem.ImageIndex<>ImageIndex then
      RaiseError;
    if MenuItem.Hint<>Hint then
      RaiseError;
  end;
  if (Section=nil) then begin
    if SectionIndex<>-1 then
      RaiseError;
  end else begin
    if SectionIndex<0 then
      RaiseError;
    if Section[SectionIndex]<>Self then
      RaiseError;
  end;
end;

{ TIDEMenuSection }

procedure TIDEMenuSection.SetSubMenuImages(const AValue: TCustomImageList);
begin
  if FSubMenuImages=AValue then exit;
  FSubMenuImages:=AValue;
  if MenuItem<>nil then
    MenuItem.SubMenuImages:=SubMenuImages;
end;

procedure TIDEMenuSection.SetMenuItem(const AValue: TMenuItem);
begin
  inherited SetMenuItem(AValue);
  if (Section=nil) and (MenuItem<>nil) then begin
    // root section -> create menu items
    UpdateContainer;
  end;
end;

procedure TIDEMenuSection.ClearMenuItems;
var
  i: Integer;
begin
  if FItems<>nil then
    for i:=Count-1 downto 0 do
      Items[i].ClearMenuItems;
  FreeTopSeparator;
  FreeBottomSeparator;
  inherited ClearMenuItems;
end;

procedure TIDEMenuSection.FreeTopSeparator;
begin
  if TopSeparator=nil then exit;
  FreeAndNil(FTopSeparator);
end;

procedure TIDEMenuSection.FreeBottomSeparator;
begin
  if BottomSeparator=nil then exit;
  FreeAndNil(FBottomSeparator);
end;

procedure TIDEMenuSection.UpdateAllChildrenIndex(StartIndex: Integer);
var
  i: LongInt;
begin
  for i:=StartIndex to FItems.Count-1 do
    Items[i].FSectionIndex:=i;
end;

procedure TIDEMenuSection.UpdateTopSeparator(ParentMenuItem: TMenuItem;
  var aMenuIndex: integer);
begin
  if NeedTopSeparator then begin
    if (TopSeparator<>nil)
    and (aMenuIndex<ParentMenuItem.Count)
    and (TopSeparator=ParentMenuItem[aMenuIndex]) then begin
      // already in place
    end else begin
      if TopSeparator<>nil then
        FreeTopSeparator;
      FTopSeparator:=MenuItemClass.Create(nil);
      TopSeparator.Caption:='-';
      TopSeparator.AddHandlerOnDestroy(@OnSeparatorDestroy);
      ParentMenuItem.Insert(aMenuIndex,TopSeparator);
    end;
    inc(aMenuIndex);
  end else begin
    if TopSeparator=nil then exit;
    FreeTopSeparator;
  end;
end;

procedure TIDEMenuSection.UpdateBottomSeparator(ParentMenuItem: TMenuItem;
  var aMenuIndex: integer);
begin
  if NeedBottomSeparator then begin
    if (BottomSeparator<>nil)
    and (aMenuIndex<ParentMenuItem.Count)
    and (BottomSeparator=ParentMenuItem[aMenuIndex]) then begin
      // already in place
    end else begin
      if BottomSeparator<>nil then
        FreeBottomSeparator;
      FBottomSeparator:=MenuItemClass.Create(nil);
      BottomSeparator.Caption:='-';
      BottomSeparator.AddHandlerOnDestroy(@OnSeparatorDestroy);
      ParentMenuItem.Insert(aMenuIndex,BottomSeparator);
    end;
    inc(aMenuIndex);
  end else begin
    if BottomSeparator=nil then exit;
    FreeBottomSeparator;
  end;
end;

procedure TIDEMenuSection.UpdateContainer;
var
  ParentMenuItem: TMenuItem;
  aMenuIndex: integer;

  procedure UpdateSection(aSection: TIDEMenuSection);
  var
    i: Integer;
    Item: TIDEMenuItem;
    SubSection: TIDEMenuSection;
    aVisible: Boolean;
  begin
    if imssClearing in aSection.FStates then exit;
    aVisible:=aSection.RealVisible;
    for i:=0 to aSection.Count-1 do begin
      Item:=aSection[i];
      if (Item is TIDEMenuSection)
      and (not TIDEMenuSection(Item).ChildrenAsSubMenu) then begin
        SubSection:=TIDEMenuSection(Item);
        SubSection.UpdateTopSeparator(ParentMenuItem,aMenuIndex);
        UpdateSection(SubSection);
        SubSection.UpdateBottomSeparator(ParentMenuItem,aMenuIndex);
      end else begin
        // append MenuItem
        if (Item.MenuItem<>nil)
        and (aMenuIndex<ParentMenuItem.Count)
        and (Item.MenuItem=ParentMenuItem[aMenuIndex])
        then begin
          // already in place -> ok
          inc(aMenuIndex);
          // update MenuItem.Visible here for less overhead
          Item.MenuItem.Visible:=Item.RealVisible;
        end else begin
          // structure has changed
          if Item.MenuItem<>nil then
            Item.ClearMenuItems;
          //debugln(['  UpdateSection Item=',Item.Name,' RealVisible=',Item.RealVisible,' Item.VisibleActive=',Item.VisibleActive]);
          if (Item.MenuItem=nil) and aVisible and Item.VisibleActive then begin
            Item.CreateMenuItem;
            if Item is TIDEMenuSection then
              TIDEMenuSection(Item).UpdateContainer;
          end;
          if Item.MenuItem<>nil then begin
            if Item.MenuItem.Parent=nil then
              ParentMenuItem.Insert(aMenuIndex,Item.MenuItem);
            inc(aMenuIndex);
          end;
        end;
      end;
    end;
  end;

begin
  if not ChildrenAsSubMenu then begin
    if Section<>nil then Section.UpdateContainer;
    exit;
  end;
  if imssClearing in FStates then exit;
  if MenuItem=nil then exit;
  {$IFDEF VerboseMenuIntf}
  debugln(['TIDEMenuSection.UpdateContainer "',Name,'" Count=',Count]);
  {$ENDIF}
  ParentMenuItem:=MenuItem;
  aMenuIndex:=0;
  UpdateSection(Self);
end;

procedure TIDEMenuSection.UpdateSubMenus;

  procedure UpdateSection(aSection: TIDEMenuSection);
  var
    i: Integer;
    Item: TIDEMenuItem;
    SubSection: TIDEMenuSection;
  begin
    for i:=0 to aSection.Count-1 do begin
      Item:=aSection[i];
      if not (Item is TIDEMenuSection) then continue;
      SubSection:=TIDEMenuSection(Item);
      if SubSection.ChildrenAsSubMenu then
        SubSection.UpdateContainer;
      UpdateSection(SubSection);
    end;
  end;

begin
  UpdateSection(Self);
end;

procedure TIDEMenuSection.UpdateVisibleCommandCount(Add: integer);
var
  PendingContainer: TIDEMenuSection;

  procedure Update(aSection: TIDEMenuSection);
  begin
    aSection:=aSection.GetContainerSection;
    if PendingContainer=aSection then exit;
    if PendingContainer<>nil then
      PendingContainer.UpdateContainer;
    PendingContainer:=aSection;
  end;

var
  aSection: TIDEMenuSection;
  WasVisibleActive: Boolean;
begin
  aSection:=Self;
  PendingContainer:=GetContainerSection; // always update the current container
  while aSection<>nil do begin
    WasVisibleActive:=aSection.VisibleActive;
    inc(aSection.FVisibleCommandCount,Add);
    if aSection.FVisibleCommandCount<0 then
      RaiseGDBException('');
    if (WasVisibleActive<>aSection.VisibleActive) then begin
      {$IFDEF VerboseMenuIntf}
      debugln(['TIDEMenuSection.UpdateVisibleCommandCount "',Name,'" Section="',aSection.Name,'" WasVis=',WasVisibleActive,' NowVis=',aSection.VisibleActive,' MI.Vis=',(aSection.MenuItem<>nil) and aSection.MenuItem.Visible]);
      {$ENDIF}
      if aSection.MenuItem<>nil then
        aSection.RealizeVisible;
      Update(aSection);
      if aSection.ChildrenAsSubMenu and (aSection.Section<>nil) then
        Update(aSection.Section);
    end;
    if not aSection.Visible then break;
    aSection:=aSection.Section;
  end;
  if PendingContainer<>nil then
    PendingContainer.UpdateContainer;
end;

procedure TIDEMenuSection.NotifySubSectionOnShow(Sender: TObject;
  WithChildren: Boolean);
var
  i: Integer;
  Child: TIDEMenuItem;
begin
  //DebugLn(['TIDEMenuSection.NotifySubSectionOnShow ',Name,' ChildrenAsSubMenu=',ChildrenAsSubMenu,' Count=',Count]);
  FSectionHandlers[imshtOnShow].CallNotifyEvents(Sender);
  if WithChildren or (not ChildrenAsSubMenu) then begin
    i:=0;
    while i<Count do begin
      Child:=Items[i];
      Child.DoOnRequestCaption(Child);
      if Child is TIDEMenuSection then
        TIDEMenuSection(Child).NotifySubSectionOnShow(Sender,false);
      inc(i);
    end;
  end;
end;

constructor TIDEMenuSection.Create(const TheName: string);
begin
  inherited Create(TheName);
  FChildrenAsSubMenu := True;
  FItems := TFPList.Create;
end;

destructor TIDEMenuSection.Destroy;
var
  AHandlerType: TIDEMenuSectionHandlerType;
begin
  Clear;
  FreeAndNil(FItems);
  for AHandlerType := Low(TIDEMenuSectionHandlerType) to High(TIDEMenuSectionHandlerType) do
    FreeAndNil(FSectionHandlers[AHandlerType]);
  inherited Destroy;
end;

procedure TIDEMenuSection.Clear;
var
  i: Integer;
begin
  if imssClearing in FStates then
    raise Exception.Create('TIDEMenuSection.Clear imssClearing is set');
  Include(FStates,imssClearing);

  ClearMenuItems;

  for i:=FItems.Count-1 downto 0 do begin
    TObject(FItems[i]).Free;
    FItems[i] := nil;
  end;
  FItems.Clear;

  if FVisibleCommandCount<>0 then
    RaiseGDBException('');
  Exclude(FStates,imssClearing);
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
var
  AddedVisibleCommands: Integer;
begin
  AnItem.Section:=nil;
  AnItem.Name:=CreateUniqueName(AnItem.Name);
  {$IFDEF VerboseMenuIntf}
  debugln(['TIDEMenuSection.Insert Self="',Name,'" Item="',AnItem.Name,'" AnItem.VisibleActive=',AnItem.VisibleActive]);
  {$ENDIF}
  FItems.Insert(Index,AnItem);
  UpdateAllChildrenIndex(Index);
  AnItem.FSection:=Self;

  AddedVisibleCommands:=0;
  if AnItem.Visible then
    AddedVisibleCommands:=AnItem.VisibleCommandCount;
  // update this and parents TMenuItems
  UpdateVisibleCommandCount(AddedVisibleCommands);

  {$IFDEF VerboseMenuIntf}
  debugln(['TIDEMenuSection.Insert END Self="',Name,'" Item="',AnItem.Name,'" VisibleActive=',VisibleActive,' VisibleCommandCount=',VisibleCommandCount,' MenuItem=',DbgSName(MenuItem)]);
  ConsistencyCheck;
  {$ENDIF}
end;

procedure TIDEMenuSection.Remove(AnItem: TIDEMenuItem);
var
  RemovedVisibleCommands: Integer;
begin
  // consistency checks
  if AnItem=nil then
    RaiseGDBException('');
  if AnItem.Section<>Self then
    RaiseGDBException('');

  if not (imssClearing in FStates) then begin
    // remove from FItems
    FItems.Delete(AnItem.SectionIndex);
    UpdateAllChildrenIndex(AnItem.SectionIndex);
  end;

  RemovedVisibleCommands:=0;
  if AnItem.Visible then
    RemovedVisibleCommands:=AnItem.VisibleCommandCount;

  AnItem.FSection:=nil;
  AnItem.FSectionIndex:=-1;

  // free TMenuItems
  if not (imssClearing in FStates) then
    AnItem.ClearMenuItems;

  // update this and parents TMenuItems
  UpdateVisibleCommandCount(-RemovedVisibleCommands);

  if not (imssClearing in FStates) then begin
    {$IFDEF VerboseMenuIntf}
    ConsistencyCheck;
    {$ENDIF}
  end;
end;

procedure TIDEMenuSection.CreateMenuItem;
begin
  if ChildrenAsSubMenu then
    inherited CreateMenuItem
  else
    ; // this section has no menuitem for its own
end;

function TIDEMenuSection.IndexOf(AnItem: TIDEMenuItem): Integer;
begin
  Result:=FItems.IndexOf(AnItem);
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

function TIDEMenuSection.VisibleActive: boolean;
begin
  Result:=Visible and (VisibleCommandCount>0);
end;

function TIDEMenuSection.NeedTopSeparator: boolean;
var
  i: Integer;
  Sibling: TIDEMenuItem;
begin
  Result:=false;
  if ChildrenAsSubMenu then exit;
  if Section=nil then exit;
  if not VisibleActive then exit;
  // this is a logical section with visible MenuItems
  // search for a MenuItem in front
  for i:=SectionIndex-1 downto 0 do begin
    Sibling:=Section[i];
    if Sibling.VisibleActive then
      exit(true); // there is a visible sibling above -> yes, need TopSeparator
  end;
end;

function TIDEMenuSection.NeedBottomSeparator: boolean;
var
  i: Integer;
  Sibling: TIDEMenuItem;
begin
  Result:=false;
  if ChildrenAsSubMenu then exit;
  if Section=nil then exit;
  if not VisibleActive then exit;
  // this is a logical section with visible MenuItems
  for i:=SectionIndex+1 to Section.Count-1 do begin
    Sibling:=Section[i];
    if Sibling.VisibleActive then begin
      // there is a visible sibling below
      if Sibling is TIDEMenuSection then begin
        if not TIDEMenuSection(Sibling).ChildrenAsSubMenu then
          exit(false); // the below sibling is a logical section with a TopSeparator -> no need for BottomSeparator
      end;
      // -> yes, need BottomSeparator
      exit(true);
    end;
  end;
end;

function TIDEMenuSection.GetFirstChildSameContainer: TIDEMenuItem;
// find the first visible TIDEMenuItem in the same container (i.e. same MenuItem.Parent)
// The result can be:
//  - a TIDEMenuCommand
//  - a TIDEMenuSection with ChildrenAsSubMenu=true
//  - a TIDEMenuSection with TopSeparator<>nil
var
  i: Integer;
  Item: TIDEMenuItem;
  ChildSection: TIDEMenuSection;
begin
  Result:=nil;
  if ChildrenAsSubMenu then exit;
  if not VisibleActive then exit;
  for i:=0 to Count-1 do begin
    Item:=Items[i];
    if not Item.VisibleActive then continue;
    if Item is TIDEMenuCommand then
      exit(Item);
    ChildSection:=Item as TIDEMenuSection;
    if ChildSection.ChildrenAsSubMenu
    or (ChildSection.TopSeparator<>nil) then
      exit(ChildSection);
    Result:=ChildSection.GetFirstChildSameContainer;
  end;
end;

function TIDEMenuSection.GetLastChildSameContainer: TIDEMenuItem;
// find the last visible TIDEMenuItem in the same container (i.e. same MenuItem.Parent)
// The result can be:
//  - a TIDEMenuCommand
//  - a TIDEMenuSection with ChildrenAsSubMenu=true
//  - a TIDEMenuSection with BottomSeparator<>nil
var
  i: Integer;
  Item: TIDEMenuItem;
  ChildSection: TIDEMenuSection;
begin
  Result:=nil;
  if ChildrenAsSubMenu then exit;
  if not VisibleActive then exit;
  for i:=Count-1 downto 0 do begin
    Item:=Items[i];
    if not Item.VisibleActive then continue;
    if Item is TIDEMenuCommand then
      exit(Item);
    ChildSection:=Item as TIDEMenuSection;
    if ChildSection.ChildrenAsSubMenu
    or (ChildSection.BottomSeparator<>nil) then
      exit(ChildSection);
    Result:=ChildSection.GetLastChildSameContainer;
  end;
end;

procedure TIDEMenuSection.BeginUpdate;
begin

end;

procedure TIDEMenuSection.EndUpdate;
begin

end;

procedure TIDEMenuSection.RemoveAllHandlersOfObject(AnObject: TObject);
var
  HandlerType: TIDEMenuSectionHandlerType;
begin
  for HandlerType:=Low(TIDEMenuSectionHandlerType)
  to High(TIDEMenuSectionHandlerType) do
    FSectionHandlers[HandlerType].RemoveAllMethodsOfObject(AnObject);
end;

procedure TIDEMenuSection.AddHandlerOnShow(const OnShowEvent: TNotifyEvent;
  AsLast: boolean);
begin
  AddHandler(imshtOnShow,TMethod(OnShowEvent),AsLast);
end;

procedure TIDEMenuSection.RemoveHandlerOnShow(const OnShowEvent: TNotifyEvent);
begin
  RemoveHandler(imshtOnShow,TMethod(OnShowEvent));
end;

procedure TIDEMenuSection.WriteDebugReport(const Prefix: string;
  MenuItemDebugReport: boolean);
var
  i: Integer;
begin
  debugln([Prefix,'SectionIndex=',SectionIndex,' Name="',DbgStr(Name),'"',
    ' Visible=',Visible,
    ' VisCmdCnt=',VisibleCommandCount,
    ' VisActive=',VisibleActive,
    ' ChildrenAsSubMenu=',ChildrenAsSubMenu,
    '']);
  for i:=0 to Count-1 do
    if Items[i]<>nil then Items[i].WriteDebugReport(Prefix+'  ',false);
  if MenuItemDebugReport and (MenuItem<>nil) then
    MenuItem.WriteDebugReport(Prefix);
end;

procedure TIDEMenuSection.ConsistencyCheck;

  procedure RaiseError(const Msg: string = '');
  var
    s: String;
  begin
    s:='TIDEMenuSection.ConsistencyCheck Name="'+Name+'"';
    if Msg<>'' then
      s+='. '+Msg;
    debugln(s);
    RaiseGDBException(s);
  end;

  procedure CheckMenuItemIndex(aMenuItem: TMenuItem; var Index: integer);
  begin
    if aMenuItem.Parent<>MenuItem then
      RaiseError('');
    if Index>=MenuItem.Count then
      RaiseError('');
    if MenuItem[Index]<>aMenuItem then
      RaiseError('');
    inc(Index);
  end;

  procedure CheckContainerMenuItems(aSection: TIDEMenuSection; var Index: integer);
  var
    i: Integer;
    Item: TIDEMenuItem;
    SubSection: TIDEMenuSection;
    aVisible: Boolean;
  begin
    aVisible:=aSection.RealVisible;
    for i:=0 to aSection.Count-1 do begin
      Item:=aSection[i];
      if (Item is TIDEMenuSection) then begin
        SubSection:=TIDEMenuSection(Item);
        if SubSection.NeedTopSeparator then begin
          if SubSection.TopSeparator=nil then
            RaiseError('missing TopSeparator');
          CheckMenuItemIndex(SubSection.TopSeparator,Index);
        end else begin
          if SubSection.TopSeparator<>nil then
            RaiseError('dangling TopSeparator');
        end;
        if SubSection.ChildrenAsSubMenu then begin
          if aVisible and SubSection.VisibleActive then begin
            if SubSection.MenuItem=nil then
              RaiseError('missing SubMenu');
            CheckMenuItemIndex(SubSection.MenuItem,Index);
          end else begin
            // a hidden item can have a MenuItem
            if (SubSection.MenuItem<>nil) and (SubSection.MenuItem.Parent<>nil) then
              CheckMenuItemIndex(SubSection.MenuItem,Index);
          end;
        end else
          CheckContainerMenuItems(SubSection,Index);
        if SubSection.NeedBottomSeparator then begin
          if SubSection.BottomSeparator=nil then
            RaiseError('missing BottomSeparator');
          CheckMenuItemIndex(SubSection.BottomSeparator,Index);
        end else begin
          if SubSection.BottomSeparator<>nil then
            RaiseError('dangling BottomSeparator');
        end;
      end else begin
        // TIDEMenuCommand
        if aVisible and Item.VisibleActive then begin
          if Item.MenuItem=nil then
            RaiseError('missing MenuItem');
          CheckMenuItemIndex(Item.MenuItem,Index);
        end else begin
          // a hidden item can have a MenuItem
          if (Item.MenuItem<>nil) and (Item.MenuItem.Parent<>nil) then
            CheckMenuItemIndex(Item.MenuItem,Index);
        end;
      end;
    end;
  end;

var
  i: Integer;
  Item: TIDEMenuItem;
  RealVisibleCommandCount: Integer;
  CanHaveMenuItem: Boolean;
begin
  inherited ConsistencyCheck;

  CanHaveMenuItem:=RealVisible and (GetRoot.MenuItem<>nil);

  RealVisibleCommandCount:=0;
  for i:=0 to Count-1 do begin
    Item:=Items[i];
    if Item.SectionIndex<>i then
      RaiseError('');
    Item.ConsistencyCheck;
    if Item.Visible then begin
      if Item is TIDEMenuCommand then
        inc(RealVisibleCommandCount)
      else if Item is TIDEMenuSection then
        inc(RealVisibleCommandCount,TIDEMenuSection(Item).VisibleCommandCount);
    end;
  end;
  if RealVisibleCommandCount<>VisibleCommandCount then
    RaiseError('VisibleCommandCount='+dbgs(VisibleCommandCount)+' Real='+dbgs(RealVisibleCommandCount));

  if NeedTopSeparator then begin
    if (TopSeparator=nil) and CanHaveMenuItem then
      RaiseError('');
  end else begin
    if TopSeparator<>nil then
      RaiseError('');
  end;

  if NeedBottomSeparator then begin
    if (BottomSeparator=nil) and CanHaveMenuItem then
      RaiseError('');
  end else begin
    if BottomSeparator<>nil then
      RaiseError('');
  end;

  if ChildrenAsSubMenu then begin
    if MenuItem<>nil then begin
      i:=0;
      CheckContainerMenuItems(Self,i);
    end else begin
      if VisibleActive and CanHaveMenuItem then
        RaiseError('');
    end;
  end;
end;

function TIDEMenuSection.GetItems(Index: Integer): TIDEMenuItem;
begin
  Result:=TIDEMenuItem(FItems[Index]);
end;

procedure TIDEMenuSection.OnSeparatorDestroy(Sender: TObject);
begin
  if Sender=FTopSeparator then
    FTopSeparator:=nil
  else if Sender=FBottomSeparator then
    FBottomSeparator:=nil;
end;

procedure TIDEMenuSection.AddHandler(HandlerType: TIDEMenuSectionHandlerType;
  const AMethod: TMethod; AsLast: boolean);
begin
  if FSectionHandlers[HandlerType]=nil then
    FSectionHandlers[HandlerType]:=TMethodList.Create;
  FSectionHandlers[HandlerType].Add(AMethod,AsLast);
end;

procedure TIDEMenuSection.RemoveHandler(
  HandlerType: TIDEMenuSectionHandlerType; const AMethod: TMethod);
begin
  FSectionHandlers[HandlerType].Remove(AMethod);
end;

procedure TIDEMenuSection.MenuItemClick(Sender: TObject);
begin
  inherited MenuItemClick(Sender);
  NotifySubSectionOnShow(Sender);
end;

procedure TIDEMenuSection.SetChildrenAsSubMenu(const AValue: boolean);
begin
  if FChildrenAsSubMenu=AValue then exit;
  FChildrenAsSubMenu:=AValue;
  ClearMenuItems;
  {$IFDEF VerboseMenuIntf}
  debugln(['TIDEMenuSection.SetChildrenAsSubMenu Name="',Name,'" ChildrenAsSubMenu=',ChildrenAsSubMenu]);
  {$ENDIF}
  if Section<>nil then
    Section.UpdateContainer;
  if ChildrenAsSubMenu then
    UpdateContainer;
end;

procedure TIDEMenuSection.SetVisible(const AValue: Boolean);
begin
  if AValue=Visible then exit;
  inherited SetVisible(AValue);
  if VisibleActive then begin
    if ChildrenAsSubMenu then
      UpdateContainer;
    UpdateSubMenus;
  end;
end;

{ TIDEMenuCommand }

procedure TIDEMenuCommand.MenuItemClick(Sender: TObject);
begin
  inherited MenuItemClick(Sender);
  // do not execute if something is already executed
  //debugln(['TIDEMenuCommand.MenuItemClick ',Assigned(OnClick),' ',Assigned(OnClickProc),' ',Assigned(Command),' ',(Command<>nil) and (Command.OnExecuteProc<>nil)]);
  if (not Assigned(OnClick)) and (not Assigned(OnClickProc))
  and Assigned(Command) then
    Command.Execute(Sender);
end;

procedure TIDEMenuCommand.SetAutoCheck(const AValue: boolean);
begin
  if FAutoCheck=AValue then exit;
  FAutoCheck:=AValue;
  if MenuItem<>nil then MenuItem.AutoCheck:=AutoCheck;
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
    if Command<>nil then begin
      MenuItem.ShortCut:=KeyToShortCut(Command.ShortcutA.Key1,Command.ShortcutA.Shift1);
      MenuItem.ShortCutKey2:=KeyToShortCut(Command.ShortcutA.Key2,Command.ShortcutA.Shift2);
    end
    else begin
      MenuItem.ShortCut:=0;
      MenuItem.ShortCutKey2:=0;
    end;
    MenuItem.GroupIndex:=GroupIndex;
  end;
end;

constructor TIDEMenuCommand.Create(const TheName: string);
begin
  inherited Create(TheName);
  FVisibleCommandCount:=1;
end;

procedure TIDEMenuCommand.ConsistencyCheck;

  procedure RaiseError;
  var
    s: String;
  begin
    s:='TIDEMenuItem.ConsistencyCheck Name="'+Name+'" Caption="'+DbgStr(Caption)+'"';
    debugln(s);
    RaiseGDBException(s);
  end;

begin
  inherited ConsistencyCheck;
  if MenuItem<>nil then begin
    if MenuItem.AutoCheck<>AutoCheck then
      RaiseError;
    if MenuItem.Checked<>Checked then
      RaiseError;
    if MenuItem.Default<>Default then
      RaiseError;
    if MenuItem.RadioItem<>RadioItem then
      RaiseError;
    if MenuItem.RightJustify<>RightJustify then
      RaiseError;
    if MenuItem.ShowAlwaysCheckable<>ShowAlwaysCheckable then
      RaiseError;
    if MenuItem.GroupIndex<>GroupIndex then
      RaiseError;
    if Command<>nil then begin
      if MenuItem.ShortCut<>KeyToShortCut(Command.ShortcutA.Key1,Command.ShortcutA.Shift1) then
        RaiseError;
      if MenuItem.ShortCutKey2<>KeyToShortCut(Command.ShortcutA.Key2,Command.ShortcutA.Shift2) then
        RaiseError;
    end
    else begin
      if MenuItem.ShortCut<>0 then
        RaiseError;
      if MenuItem.ShortCutKey2<>0 then
        RaiseError;
    end;
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
  Section.Name:=CreateUniqueName(Section.Name);
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

