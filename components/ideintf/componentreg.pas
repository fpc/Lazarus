{  $Id$  }
{
 /***************************************************************************
                            componentreg.pas
                            ----------------

 ***************************************************************************/

 *****************************************************************************
  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************

  Author: Mattias Gaertner, Juha Manninen

  Abstract:
    Interface to the component palette and the registered component classes.
    Supports reordering of pages and components by user settings in environment options.
}
unit ComponentReg;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, typinfo, Laz_AVL_Tree, fgl,
  // LCL
  Controls, LCLProc,
  // LazUtils
  Laz2_XMLCfg, LazMethodList;

type
  TComponentPriorityCategory = (
    cpBase,
    cpUser,            // User has changed the order using options GUI.
    cpRecommended,
    cpNormal,
    cpOptional
    );
    
  TComponentPriority = record
    Category: TComponentPriorityCategory;
    Level: integer; // higher level means higher priority (range: -1000 to 1000)
  end;
    
const
  ComponentPriorityNormal: TComponentPriority = (Category: cpNormal; Level: 0);

  LCLCompPriority: TComponentPriority = (Category: cpBase; Level: 10);
  FCLCompPriority: TComponentPriority = (Category: cpBase; Level: 9);
  IDEIntfCompPriority: TComponentPriority = (Category: cpBase; Level: 8);

type
  TBaseComponentPage = class;
  TBaseComponentPalette = class;
  TOnGetCreationClass = procedure(Sender: TObject;
                              var NewComponentClass: TComponentClass) of object;

  { TBaseCompPaletteOptions }

  TBaseCompPaletteOptions = class
  protected
    // Pages reordered by user.
    FPageNames: TStringList;
    // List of page names with component contents.
    // Object holds another StringList for the component names.
    FComponentPages: TStringList;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    procedure Assign(Source: TBaseCompPaletteOptions);
    procedure AssignComponentPage(aPageName: string; aList: TStringList);
    function Equals(Obj: TObject): boolean; override;
  public
    property PageNames: TStringList read FPageNames;
    property ComponentPages: TStringList read FComponentPages;
  end;

  { TCompPaletteOptions }

  TCompPaletteOptions = class(TBaseCompPaletteOptions)
  private
    // Pages removed or renamed. They must be hidden in the palette.
    FHiddenPageNames: TStringList;
    FName: string;
    FVisible: boolean;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    procedure Assign(Source: TCompPaletteOptions);
    function IsDefault: Boolean;
    procedure Load(XMLConfig: TXMLConfig; Path: String);
    procedure Save(XMLConfig: TXMLConfig; Path: String);
    function Equals(Obj: TObject): boolean; override;
  public
    property Name: string read FName write FName;
    property HiddenPageNames: TStringList read FHiddenPageNames;
    property Visible: boolean read FVisible write FVisible;
  end;

  { TCompPaletteUserOrder }

  // Only used by the component palette options to show all available pages.
  // It's like TCompPaletteOptions but collects all pages and components,
  //  including the original ones and the newly installed ones.
  //  The active palette is later synchronized with this.
  TCompPaletteUserOrder = class(TBaseCompPaletteOptions)
  private
    fPalette: TBaseComponentPalette;
    // Reference to either EnvironmentOptions.ComponentPaletteOptions or a copy of it.
    fOptions: TCompPaletteOptions;
  public
    constructor Create(aPalette: TBaseComponentPalette);
    destructor Destroy; override;
    procedure Clear;
    function SortPagesAndCompsUserOrder: Boolean;
  public
    property ComponentPages; // all pages, ordered first by Options, then by default priority
    property Options: TCompPaletteOptions read fOptions write fOptions;
  end;

  { TRegisteredComponent }

  TRegisteredComponent = class
  private
    FComponentClass: TComponentClass;
    FOnGetCreationClass: TOnGetCreationClass;
    FOrigPageName: string;
    FRealPage: TBaseComponentPage;
    FVisible: boolean;
  protected
    procedure SetVisible(const AValue: boolean); virtual;
  public
    constructor Create(TheComponentClass: TComponentClass; const ThePageName: string);
    destructor Destroy; override;
    procedure ConsistencyCheck; virtual;
    function GetUnitName: string; virtual; abstract;
    function GetPriority: TComponentPriority; virtual;
    procedure AddToPalette; virtual;
    function CanBeCreatedInDesigner: boolean; virtual;
    function GetCreationClass: TComponentClass; virtual;
  public
    property ComponentClass: TComponentClass read FComponentClass;
    property OnGetCreationClass: TOnGetCreationClass read FOnGetCreationClass
                                                     write FOnGetCreationClass;
    property OrigPageName: string read FOrigPageName; // case sensitive
    property RealPage: TBaseComponentPage read FRealPage write FRealPage;
    property Visible: boolean read FVisible write SetVisible;
  end;

  TRegisteredComponentList = specialize TFPGList<TRegisteredComponent>;


  { TBaseComponentPage }

  TBaseComponentPage = class
  private
    FPageName: string;
    FPalette: TBaseComponentPalette;
    FVisible: boolean;
  protected
    FIndex: Integer;           // Index in the Pages container.
    procedure SetVisible(const AValue: boolean); virtual;
    procedure OnComponentVisibleChanged({%H-}AComponent: TRegisteredComponent); virtual;
  public
    constructor Create(const ThePageName: string);
    destructor Destroy; override;
  public
    property PageName: string read FPageName;
    property Palette: TBaseComponentPalette read FPalette write FPalette;
    property Visible: boolean read FVisible write SetVisible;
  end;

  TBaseComponentPageClass = class of TBaseComponentPage;

  { TBaseComponentPalette }
  
  TComponentPaletteHandlerType = (
    cphtUpdateVisible,   // Visibility of component palette icons is recomputed
    cphtComponentAdded,  // Typically selection is changed after component was added.
    cphtSelectionChanged
    );

  TComponentSelectionMode = (
    csmSingle, // reset selection on component add
    csmMulty   // don't reset selection on component add
  );

  TEndUpdatePaletteEvent = procedure(Sender: TObject; PaletteChanged: boolean) of object;
  TGetComponentClassEvent = procedure(const AClass: TComponentClass) of object;
  TUpdateCompVisibleEvent = procedure(AComponent: TRegisteredComponent;
                      var VoteVisible: integer { Visible>0 }  ) of object;
  TPaletteHandlerEvent = procedure of object;
  TComponentAddedEvent = procedure(ALookupRoot, AComponent: TComponent; ARegisteredComponent: TRegisteredComponent) of object;
  RegisterUnitComponentProc = procedure(const Page, UnitName: ShortString;
                                        ComponentClass: TComponentClass);
  TBaseComponentPageList = specialize TFPGList<TBaseComponentPage>;
  TPagePriorityList = specialize TFPGMap<String, TComponentPriority>;

  TBaseComponentPalette = class
  private
    // List of pages, created based on user ordered and original pages.
    fPages: TBaseComponentPageList;
    // List of all components in all pages.
    fComps: TRegisteredComponentList;
    // New pages added and their priorities, ordered by priority.
    fOrigPagePriorities: TPagePriorityList;
    // User ordered + original pages and components
    fUserOrder: TCompPaletteUserOrder;
    // Component cache, a tree of TRegisteredComponent sorted for componentclass
    fComponentCache: TAVLTree;
    // Two page caches, one for original pages, one for user ordered pages.
    // Lists have page names. Object holds another StringList for component names.
    fOrigComponentPageCache: TStringList;  // Original
    fUserComponentPageCache: TStringList;  // User ordered
    // Used to find names that differ in character case only.
    fOrigPageHelper: TStringList;
    fHandlers: array[TComponentPaletteHandlerType] of TMethodList;
    fComponentPageClass: TBaseComponentPageClass;
    fSelected: TRegisteredComponent;
    fSelectionMode: TComponentSelectionMode;
    fHideControls: boolean;
    fUpdateLock: integer;
    fChanged: boolean;
    fChangeStamp: integer;
    fOnClassSelected: TNotifyEvent;
    procedure AddHandler(HandlerType: TComponentPaletteHandlerType;
                         const AMethod: TMethod; AsLast: boolean = false);
    procedure RemoveHandler(HandlerType: TComponentPaletteHandlerType;
                            const AMethod: TMethod);
    procedure CacheOrigComponentPages;
    function CreatePagesFromUserOrder: Boolean;
    procedure DoChange;
    procedure DoPageAddedComponent(Component: TRegisteredComponent);
    procedure DoPageRemovedComponent(Component: TRegisteredComponent);
    function VoteCompVisibility(AComponent: TRegisteredComponent): Boolean;
    function GetSelected: TRegisteredComponent;
    function GetMultiSelect: boolean;
    procedure SetSelected(const AValue: TRegisteredComponent);
    procedure SetMultiSelect(AValue: boolean);
  public
    constructor Create(EnvPaletteOptions: TCompPaletteOptions);
    destructor Destroy; override;
    procedure Clear;
    function AssignOrigCompsForPage(PageName: string; DestComps: TStringList): Boolean;
    function AssignOrigVisibleCompsForPage(PageName: string; DestComps: TStringList): Boolean;
    function RefUserCompsForPage(PageName: string): TStringList;
    procedure BeginUpdate(Change: boolean);
    procedure EndUpdate;
    function IsUpdateLocked: boolean;
    procedure IncChangeStamp;
    function IndexOfPageName(const APageName: string; ACaseSensitive: Boolean): integer;
    function GetPage(const APageName: string; ACaseSensitive: Boolean=False): TBaseComponentPage;
    procedure AddComponent(NewComponent: TRegisteredComponent);
    procedure RemoveComponent(AComponent: TRegisteredComponent);
    function FindComponent(const CompClassName: string): TRegisteredComponent;
    function CreateNewClassName(const Prefix: string): string;
    procedure Update({%H-}ForceUpdateAll: Boolean); virtual;
    procedure IterateRegisteredClasses(Proc: TGetComponentClassEvent);
    procedure SetSelectedComp(AComponent: TRegisteredComponent; AMulti: Boolean);
    // Registered handlers
    procedure DoAfterComponentAdded(ALookupRoot, AComponent: TComponent;
                            ARegisteredComponent: TRegisteredComponent); virtual;
    procedure DoAfterSelectionChanged;
    procedure RemoveAllHandlersOfObject(AnObject: TObject);
    procedure AddHandlerUpdateVisible(const OnUpdateCompVisibleEvent: TUpdateCompVisibleEvent;
                                      AsLast: boolean = false);
    procedure RemoveHandlerUpdateVisible(OnUpdateCompVisibleEvent: TUpdateCompVisibleEvent);
    procedure AddHandlerComponentAdded(OnComponentAddedEvent: TComponentAddedEvent);
    procedure RemoveHandlerComponentAdded(OnComponentAddedEvent: TComponentAddedEvent);
    procedure AddHandlerSelectionChanged(OnSelectionChangedEvent: TPaletteHandlerEvent);
    procedure RemoveHandlerSelectionChanged(OnSelectionChangedEvent: TPaletteHandlerEvent);
  public
    property Pages: TBaseComponentPageList read fPages;
    property Comps: TRegisteredComponentList read fComps;
    property OrigPagePriorities: TPagePriorityList read fOrigPagePriorities;
    property ComponentPageClass: TBaseComponentPageClass read FComponentPageClass
                                                        write FComponentPageClass;
    property ChangeStamp: integer read fChangeStamp;
    property HideControls: boolean read FHideControls write FHideControls;
    property Selected: TRegisteredComponent read GetSelected write SetSelected;
    property MultiSelect: boolean read GetMultiSelect write SetMultiSelect;
    property SelectionMode: TComponentSelectionMode read FSelectionMode write FSelectionMode;
    // User ordered + original pages and components.
    property UserOrder: TCompPaletteUserOrder read fUserOrder;
    property OnClassSelected: TNotifyEvent read fOnClassSelected write fOnClassSelected;
  end;
  

  {$IFDEF VerboseComponentPalette}
const
  CompPalVerbPgName = 'Dialogs'; //'Standard';
  {$ENDIF}
var
  IDEComponentPalette: TBaseComponentPalette = nil;

function ComponentPriority(Category: TComponentPriorityCategory; Level: integer): TComponentPriority;
function ComparePriority(const p1,p2: TComponentPriority): integer;
function CompareIDEComponentByClassName(Data1, Data2: pointer): integer;
function dbgs(const c: TComponentPriorityCategory): string; overload;
function dbgs(const p: TComponentPriority): string; overload;

implementation

const
  BasePath = 'ComponentPaletteOptions/';

procedure RaiseException(const Msg: string);
begin
  raise Exception.Create(Msg);
end;

function ComponentPriority(Category: TComponentPriorityCategory; Level: integer
  ): TComponentPriority;
begin
  Result.Category:=Category;
  Result.Level:=Level;
end;

function ComparePriority(const p1, p2: TComponentPriority): integer;
begin
  // lower category is better
  Result:=ord(p2.Category)-ord(p1.Category);
  if Result<>0 then exit;
  // higher level is better
  Result:=p1.Level-p2.Level;
end;

function CompareIDEComponentByClassName(Data1, Data2: Pointer): integer;
var
  Comp1: TRegisteredComponent absolute Data1;
  Comp2: TRegisteredComponent absolute Data2;
begin
  // The same case-insensitive compare function should be used in this function
  //  and in CompareClassNameWithRegisteredComponent.
  Result:=ShortCompareText(Comp1.ComponentClass.Classname,
                           Comp2.ComponentClass.Classname);
end;

function CompareClassNameWithRegisteredComponent(Key, Data: Pointer): integer;
var
  AClassName: String;
  RegComp: TRegisteredComponent;
begin
  AClassName:=String(Key);
  RegComp:=TRegisteredComponent(Data);
  Result:=ShortCompareText(AClassName, RegComp.ComponentClass.ClassName);
end;

function dbgs(const c: TComponentPriorityCategory): string;
begin
  Result:=GetEnumName(TypeInfo(TComponentPriorityCategory),ord(c));
end;

function dbgs(const p: TComponentPriority): string;
begin
  Result:='Cat='+dbgs(p.Category)+',Lvl='+IntToStr(p.Level);
end;

{ TBaseCompPaletteOptions }

constructor TBaseCompPaletteOptions.Create;
begin
  inherited Create;
  FPageNames := TStringList.Create;
  FComponentPages := TStringList.Create;
  FComponentPages.OwnsObjects := True;
end;

destructor TBaseCompPaletteOptions.Destroy;
begin
  FreeAndNil(FComponentPages);
  FreeAndNil(FPageNames);
  inherited Destroy;
end;

procedure TBaseCompPaletteOptions.Clear;
begin
  FPageNames.Clear;
  FComponentPages.Clear;
end;

procedure TBaseCompPaletteOptions.Assign(Source: TBaseCompPaletteOptions);
var
  i: Integer;
begin
  FPageNames.Assign(Source.FPageNames);
  FComponentPages.Clear;
  for i:=0 to Source.FComponentPages.Count-1 do
    AssignComponentPage(Source.FComponentPages[i],
            TStringList(Source.FComponentPages.Objects[i]));
end;

procedure TBaseCompPaletteOptions.AssignComponentPage(aPageName: string; aList: TStringList);
var
  sl: TStringList;
begin
  sl := TStringList.Create;
  sl.Assign(aList);
  FComponentPages.AddObject(aPageName, sl);
end;

function TBaseCompPaletteOptions.Equals(Obj: TObject): boolean;
var
  Source: TBaseCompPaletteOptions;
  i, j: Integer;
  MyList, SrcList: TStringList;
begin
  if Obj is TBaseCompPaletteOptions then
  begin
    Source:=TBaseCompPaletteOptions(Obj);
    if (not FPageNames.Equals(Source.FPageNames))
    or (FComponentPages.Count<>Source.FComponentPages.Count) then exit(false);
    for i:=0 to Source.FComponentPages.Count-1 do
    begin
      MyList:=TStringList(FComponentPages[i]);
      SrcList:=TStringList(Source.FComponentPages[i]);
      if not MyList.Equals(SrcList) then exit(false);
      for j:=0 to MyList.Count-1 do
        if MyList.Objects[j]<>SrcList.Objects[j] then exit(false);
    end;
    Result:=true;
  end else
    Result:=inherited Equals(Obj);
end;

{ TCompPaletteOptions }

constructor TCompPaletteOptions.Create;
begin
  inherited Create;
  FHiddenPageNames := TStringList.Create;
  FVisible := True;
end;

destructor TCompPaletteOptions.Destroy;
begin
  FHiddenPageNames.Free;
  inherited Destroy;
end;

procedure TCompPaletteOptions.Clear;
begin
  inherited Clear;
  FHiddenPageNames.Clear;
end;

procedure TCompPaletteOptions.Assign(Source: TCompPaletteOptions);
begin
  inherited Assign(Source);
  FHiddenPageNames.Assign(Source.FHiddenPageNames);
  FVisible := Source.FVisible;
  // Name: do not assign name
end;

function TCompPaletteOptions.IsDefault: Boolean;
begin
  Result := (PageNames.Count = 0)
    and (ComponentPages.Count = 0)
    and (HiddenPageNames.Count = 0);
end;

procedure TCompPaletteOptions.Load(XMLConfig: TXMLConfig; Path: String);
var
  CompList: TStringList;
  SubPath, CompPath: String;
  PageName, CompName: String;
  PageCount, CompCount: Integer;
  i, j: Integer;
begin
  Path := Path + BasePath;
  try
    FName:=XMLConfig.GetValue(Path+'Name/Value','');
    FVisible:=XMLConfig.GetValue(Path+'Visible/Value',true);

    // Pages
    FPageNames.Clear;
    SubPath:=Path+'Pages/';
    PageCount:=XMLConfig.GetValue(SubPath+'Count', 0);
    for i:=1 to PageCount do begin
      PageName:=XMLConfig.GetValue(SubPath+'Item'+IntToStr(i)+'/Value', '');
      if PageName <> '' then
        FPageNames.Add(PageName);
    end;

    // HiddenPages
    FHiddenPageNames.Clear;
    SubPath:=Path+'HiddenPages/';
    PageCount:=XMLConfig.GetValue(SubPath+'Count', 0);
    for i:=1 to PageCount do begin
      PageName:=XMLConfig.GetValue(SubPath+'Item'+IntToStr(i)+'/Value', '');
      if PageName <> '' then
        FHiddenPageNames.Add(PageName);
    end;

    // ComponentPages
    FComponentPages.Clear;
    SubPath:=Path+'ComponentPages/';
    PageCount:=XMLConfig.GetValue(SubPath+'Count', 0);
    for i:=1 to PageCount do begin
      CompPath:=SubPath+'Page'+IntToStr(i)+'/';
      PageName:=XMLConfig.GetValue(CompPath+'Value', '');
      CompList:=TStringList.Create;
      CompCount:=XMLConfig.GetValue(CompPath+'Components/Count', 0);
      for j:=1 to CompCount do begin
        CompName:=XMLConfig.GetValue(CompPath+'Components/Item'+IntToStr(j)+'/Value', '');
        CompList.Add(CompName);
      end;
      FComponentPages.AddObject(PageName, CompList); // CompList is owned by FComponentPages
    end;
  except
    on E: Exception do begin
      DebugLn('ERROR: TCompPaletteOptions.Load: ',E.Message);
      exit;
    end;
  end;
end;

procedure TCompPaletteOptions.Save(XMLConfig: TXMLConfig; Path: String);
var
  CompList: TStringList;
  SubPath, CompPath: String;
  i, j: Integer;
begin
  try
    Path := Path + BasePath;
    XMLConfig.SetDeleteValue(Path+'Name/Value', FName,'');
    XMLConfig.SetDeleteValue(Path+'Visible/Value', FVisible,true);

    SubPath:=Path+'Pages/';
    XMLConfig.DeletePath(SubPath);
    XMLConfig.SetDeleteValue(SubPath+'Count', FPageNames.Count, 0);
    for i:=0 to FPageNames.Count-1 do
      XMLConfig.SetDeleteValue(SubPath+'Item'+IntToStr(i+1)+'/Value', FPageNames[i], '');

    SubPath:=Path+'HiddenPages/';
    XMLConfig.DeletePath(SubPath);
    XMLConfig.SetDeleteValue(SubPath+'Count', FHiddenPageNames.Count, 0);
    for i:=0 to FHiddenPageNames.Count-1 do
      XMLConfig.SetDeleteValue(SubPath+'Item'+IntToStr(i+1)+'/Value', FHiddenPageNames[i], '');

    SubPath:=Path+'ComponentPages/';
    XMLConfig.DeletePath(SubPath);
    XMLConfig.SetDeleteValue(SubPath+'Count', FComponentPages.Count, 0);
    for i:=0 to FComponentPages.Count-1 do begin
      CompList:=FComponentPages.Objects[i] as TStringList;
      CompPath:=SubPath+'Page'+IntToStr(i+1)+'/';
      XMLConfig.SetDeleteValue(CompPath+'Value', FComponentPages[i], '');
      XMLConfig.SetDeleteValue(CompPath+'Components/Count', CompList.Count, 0);
      for j:=0 to CompList.Count-1 do
        XMLConfig.SetDeleteValue(CompPath+'Components/Item'+IntToStr(j+1)+'/Value', CompList[j], '');
    end;
  except
    on E: Exception do begin
      DebugLn('ERROR: TCompPaletteOptions.Save: ',E.Message);
      exit;
    end;
  end;
end;

function TCompPaletteOptions.Equals(Obj: TObject): boolean;
var
  Source: TCompPaletteOptions;
begin
  Result:=inherited Equals(Obj);
  if not Result then exit;
  if Obj is TCompPaletteOptions then
  begin
    Source:=TCompPaletteOptions(Obj);
    // Name: do not check Name
    if Visible<>Source.Visible then exit(false);
    if not FHiddenPageNames.Equals(Source.FHiddenPageNames) then exit(false);
  end;
end;

{ TCompPaletteUserOrder }

constructor TCompPaletteUserOrder.Create(aPalette: TBaseComponentPalette);
begin
  inherited Create;
  fPalette:=aPalette;
end;

destructor TCompPaletteUserOrder.Destroy;
begin
  Clear;
  inherited Destroy;
end;

procedure TCompPaletteUserOrder.Clear;
begin
  inherited Clear;
end;

function TCompPaletteUserOrder.SortPagesAndCompsUserOrder: Boolean;
// Calculate page order using user config and default order. User config takes priority.
// This order will finally be shown in the palette.
var
  DstComps: TStringList;
  PageI, i: Integer;
  PgName: String;
begin
  Result:=True;
  Clear;
  fPalette.CacheOrigComponentPages;
  // First add user defined page order from EnvironmentOptions,
  FComponentPages.Assign(fOptions.PageNames);
  // then add other pages which don't have user configuration
  for PageI := 0 to fPalette.OrigPagePriorities.Count-1 do
  begin
    PgName:=fPalette.OrigPagePriorities.Keys[PageI];
    if (FComponentPages.IndexOf(PgName) = -1)
    and (fOptions.HiddenPageNames.IndexOf(PgName) = -1) then
      FComponentPages.Add(PgName);
  end;
  // Map components with their pages
  for PageI := 0 to FComponentPages.Count-1 do
  begin
    PgName := FComponentPages[PageI];
    DstComps := TStringList.Create;
    DstComps.CaseSensitive := True;
    FComponentPages.Objects[PageI] := DstComps;
    i := fOptions.ComponentPages.IndexOf(PgName);
    if i >= 0 then                      // Add components reordered by user.
      DstComps.Assign(fOptions.ComponentPages.Objects[i] as TStringList)
    else                                // Add components that were not reordered.
      fPalette.AssignOrigCompsForPage(PgName, DstComps);
  end;
end;

{ TRegisteredComponent }

procedure TRegisteredComponent.SetVisible(const AValue: boolean);
begin
  if FVisible=AValue then exit;
  FVisible:=AValue;
  if (FRealPage<>nil) then
    FRealPage.OnComponentVisibleChanged(Self);
end;

constructor TRegisteredComponent.Create(TheComponentClass: TComponentClass;
  const ThePageName: string);
begin
  FComponentClass:=TheComponentClass;
  FOrigPageName:=ThePageName;
  FVisible:=true;
end;

destructor TRegisteredComponent.Destroy;
begin
  if Assigned(FRealPage) and Assigned(FRealPage.Palette) then
    FRealPage.Palette.RemoveComponent(Self);
  inherited Destroy;
end;

procedure TRegisteredComponent.ConsistencyCheck;
begin
  if (FComponentClass=nil) then
    RaiseException('TRegisteredComponent.ConsistencyCheck FComponentClass=nil');
  if not IsValidIdent(FComponentClass.ClassName) then
    RaiseException('TRegisteredComponent.ConsistencyCheck not IsValidIdent(FComponentClass.ClassName)');
end;

function TRegisteredComponent.GetPriority: TComponentPriority;
begin
  Result:=ComponentPriorityNormal;
end;

procedure TRegisteredComponent.AddToPalette;
begin
  IDEComponentPalette.AddComponent(Self);
end;

function TRegisteredComponent.CanBeCreatedInDesigner: boolean;
begin
  Result:=true;
end;

function TRegisteredComponent.GetCreationClass: TComponentClass;
begin
  Result:=FComponentClass;
  if Assigned(OnGetCreationClass) then
    OnGetCreationClass(Self,Result);
end;

{ TBaseComponentPage }

constructor TBaseComponentPage.Create(const ThePageName: string);
begin
  FPageName:=ThePageName;
  FVisible:=FPageName<>'';
end;

destructor TBaseComponentPage.Destroy;
begin
  inherited Destroy;
end;

procedure TBaseComponentPage.SetVisible(const AValue: boolean);
begin
  if FVisible=AValue then exit;
  FVisible:=AValue;
  //if (FPalette<>nil) then
  //  FPalette.OnPageVisibleChanged(Self);
end;

procedure TBaseComponentPage.OnComponentVisibleChanged(AComponent: TRegisteredComponent);
begin

end;

{ TBaseComponentPalette }

constructor TBaseComponentPalette.Create(EnvPaletteOptions: TCompPaletteOptions);
begin
  fSelectionMode:=csmSingle;
  fPages:=TBaseComponentPageList.Create;
  fComps:=TRegisteredComponentList.Create;
  fOrigPagePriorities:=TPagePriorityList.Create;
  fUserOrder:=TCompPaletteUserOrder.Create(Self);
  fUserOrder.Options:=EnvPaletteOptions; // EnvironmentOptions.ComponentPaletteOptions;
  fComponentCache:=TAVLTree.Create(@CompareIDEComponentByClassName);
  fOrigComponentPageCache:=TStringList.Create;
  fOrigComponentPageCache.OwnsObjects:=True;
  fOrigComponentPageCache.CaseSensitive:=True;
  fOrigComponentPageCache.Sorted:=True;
  fUserComponentPageCache:=TStringList.Create;
  fUserComponentPageCache.OwnsObjects:=True;
  fUserComponentPageCache.CaseSensitive:=True;
  fUserComponentPageCache.Sorted:=True;
  fOrigPageHelper:=TStringList.Create; // Note: CaseSensitive = False
  fOrigPageHelper.Sorted:=True;
end;

destructor TBaseComponentPalette.Destroy;
var
  HandlerType: TComponentPaletteHandlerType;
begin
  Clear;
  FreeAndNil(fOrigPageHelper);
  FreeAndNil(fUserComponentPageCache);
  FreeAndNil(fOrigComponentPageCache);
  FreeAndNil(fComponentCache);
  FreeAndNil(fUserOrder);
  FreeAndNil(fOrigPagePriorities);
  FreeAndNil(fComps);
  FreeAndNil(fPages);
  for HandlerType:=Low(HandlerType) to High(HandlerType) do
    FHandlers[HandlerType].Free;
  inherited Destroy;
end;

procedure TBaseComponentPalette.Clear;
var
  i: Integer;
begin
  for i:=0 to fPages.Count-1 do
    fPages[i].Free;
  fPages.Clear;
  for i:=0 to fComps.Count-1 do
    fComps[i].RealPage:=nil;
  fComps.Clear;
  fOrigPagePriorities.Clear;
  fOrigPageHelper.Clear;
end;

procedure TBaseComponentPalette.CacheOrigComponentPages;
var
  sl: TStringList;
  PageI, CompI: Integer;
  PgName: string;
  Comp: TRegisteredComponent;
begin
  if fOrigComponentPageCache.Count > 0 then Exit;  // Fill cache only once.
  for PageI := 0 to fOrigPagePriorities.Count-1 do
  begin
    PgName:=fOrigPagePriorities.Keys[PageI];
    Assert((PgName <> '') and not fOrigComponentPageCache.Find(PgName, CompI),
                  Format('CacheComponentPages: %s already cached.', [PgName]));
    // Add a cache StringList for this page name.
    sl := TStringList.Create;
    sl.CaseSensitive := True;
    fOrigComponentPageCache.AddObject(PgName, sl);
    // Find all components for this page and add them to cache.
    for CompI := 0 to fComps.Count-1 do begin
      Comp := fComps[CompI];
      if Comp.OrigPageName = PgName then // case sensitive!
        sl.AddObject(Comp.ComponentClass.ClassName, Comp);
    end;
  end;
end;

function TBaseComponentPalette.CreatePagesFromUserOrder: Boolean;
var
  UserPageI, CurPgInd, CompI: Integer;
  aVisibleCompCnt: integer;
  PgName: String;
  Pg: TBaseComponentPage;
  CompNames, UserComps: TStringList;
  Comp: TRegisteredComponent;
begin
  Result := True;
  fUserComponentPageCache.Clear;
  for UserPageI := 0 to fUserOrder.ComponentPages.Count-1 do
  begin
    PgName := fUserOrder.ComponentPages[UserPageI];
    CurPgInd := IndexOfPageName(PgName, True);
    if CurPgInd = -1 then begin
      // Create a new page
      {$IFDEF VerboseComponentPalette}
      DebugLn(['TComponentPalette.CreatePagesFromUserOrder, page ', PgName, ' index ',UserPageI]);
      {$ENDIF}
      Pg := ComponentPageClass.Create(PgName);
      fPages.Insert(UserPageI, Pg);
      Pg.Palette := Self;
    end
    else if CurPgInd <> UserPageI then begin
      {$IFDEF VerboseComponentPalette}
      DebugLn(['TComponentPalette.CreatePagesFromUserOrder, move ', PgName, ' from ',CurPgInd, ' to ',UserPageI]);
      {$ENDIF}
      fPages.Move(CurPgInd, UserPageI); // Move page to right place.
    end;
    Pg := Pages[UserPageI];
    Pg.FIndex := UserPageI;
    Assert(PgName = Pg.PageName,
      Format('TComponentPalette.CreatePagesFromUserOrder: Page names differ, "%s" and "%s".',
             [PgName, Pg.PageName]));
    // New cache page
    UserComps := TStringList.Create;
    UserComps.CaseSensitive := True;
    fUserComponentPageCache.AddObject(PgName, UserComps);
    // Associate components belonging to this page
    aVisibleCompCnt := 0;
    CompNames := TStringList(fUserOrder.ComponentPages.Objects[UserPageI]);
    for CompI := 0 to CompNames.Count-1 do
    begin
      Comp := FindComponent(CompNames[CompI]);
      if not Assigned(Comp) then Continue;
      Comp.RealPage := Pg;
      UserComps.AddObject(CompNames[CompI], Comp);
      if VoteCompVisibility(Comp) then
        inc(aVisibleCompCnt);
    end;
    {$IFDEF VerboseComponentPalette}
    if PgName=CompPalVerbPgName then
      debugln(['TComponentPalette.CreatePagesFromUserOrder HideControls=',HideControls,' aVisibleCompCnt=',aVisibleCompCnt]);
    {$ENDIF}
    Pg.Visible := (CompareText(PgName,'Hidden')<>0) and (aVisibleCompCnt>0);
  end;
  // Remove left-over pages.
  while fPages.Count > fUserOrder.ComponentPages.Count do begin
    Pg := fPages[fPages.Count-1];
    {$IFDEF VerboseComponentPalette}
    DebugLn(['TComponentPalette.CreatePagesFromUserOrder: Deleting left-over page=',
             Pg.PageName, ', Index=', fPages.Count-1]);
    {$ENDIF}
    fPages.Delete(fPages.Count-1);
    Pg.Free;
  end;
end;

function TBaseComponentPalette.AssignOrigCompsForPage(PageName: string;
  DestComps: TStringList): Boolean;
// Returns True if the page was found.
var
  sl: TStringList;
  i: Integer;
begin
  Result := fOrigComponentPageCache.Find(PageName, i);
  if Result then begin
    sl := fOrigComponentPageCache.Objects[i] as TStringList;
    DestComps.Assign(sl);
  end
  else
    DestComps.Clear;
    //raise Exception.Create(Format('AssignOrigCompsForPage: %s not found in cache.', [PageName]));
end;

function TBaseComponentPalette.AssignOrigVisibleCompsForPage(PageName: string;
  DestComps: TStringList): Boolean;
// Returns True if the page was found.
var
  sl: TStringList;
  i: Integer;
begin
  DestComps.Clear;
  Result := fOrigComponentPageCache.Find(PageName, i);
  if not Result then Exit;
  sl := fOrigComponentPageCache.Objects[i] as TStringList;
  for i := 0 to sl.Count-1 do
    if FindComponent(sl[i]).Visible then
      DestComps.Add(sl[i]);
end;

function TBaseComponentPalette.RefUserCompsForPage(PageName: string): TStringList;
var
  i: Integer;
begin
  if fUserComponentPageCache.Find(PageName, i) then
    Result := fUserComponentPageCache.Objects[i] as TStringList
  else
    Result := Nil;
end;

function TBaseComponentPalette.GetSelected: TRegisteredComponent;
begin
  Result := fSelected;
end;

function TBaseComponentPalette.GetMultiSelect: boolean;
begin
  Result := FSelectionMode = csmMulty;
end;

procedure TBaseComponentPalette.SetSelected(const AValue: TRegisteredComponent);
begin
  if fSelected=AValue then exit;
  fSelected:=AValue;
  if fSelected<>nil then begin
    if (fSelected.RealPage=nil) or (fSelected.RealPage.Palette<>Self)
    or (not fSelected.Visible)
    or (not fSelected.CanBeCreatedInDesigner) then
      fSelected:=nil;
  end;
  DoAfterSelectionChanged;
end;

procedure TBaseComponentPalette.SetMultiSelect(AValue: boolean);
begin
  if AValue then
    FSelectionMode := csmMulty
  else
    FSelectionMode := csmSingle;
end;

procedure TBaseComponentPalette.AddHandler(HandlerType: TComponentPaletteHandlerType;
  const AMethod: TMethod; AsLast: boolean);
begin
  if FHandlers[HandlerType]=nil then
    FHandlers[HandlerType]:=TMethodList.Create;
  FHandlers[HandlerType].Add(AMethod,AsLast);
end;

procedure TBaseComponentPalette.RemoveHandler(HandlerType: TComponentPaletteHandlerType;
  const AMethod: TMethod);
begin
  FHandlers[HandlerType].Remove(AMethod);
end;

procedure TBaseComponentPalette.DoChange;
begin
  if FUpdateLock>0 then
    fChanged:=true
  else
    Update(False);
end;

procedure TBaseComponentPalette.DoPageAddedComponent(Component: TRegisteredComponent);
begin
  fComponentCache.Add(Component);
  DoChange;
end;

procedure TBaseComponentPalette.DoPageRemovedComponent(Component: TRegisteredComponent);
begin
  fComponentCache.Remove(Component);
  DoChange;
end;

procedure TBaseComponentPalette.BeginUpdate(Change: boolean);
begin
  inc(FUpdateLock);
  if FUpdateLock=1 then
    fChanged:=Change
  else
    fChanged:=fChanged or Change;
end;

procedure TBaseComponentPalette.EndUpdate;
begin
  if FUpdateLock<=0 then
    RaiseException('TBaseComponentPalette.EndUpdate');
  dec(FUpdateLock);
  if (FUpdateLock=0) and fChanged then
    Update(False);
end;

function TBaseComponentPalette.IsUpdateLocked: boolean;
begin
  Result:=FUpdateLock>0;
end;

procedure TBaseComponentPalette.IncChangeStamp;
begin
  Inc(fChangeStamp);
end;

function TBaseComponentPalette.IndexOfPageName(const APageName: string;
  ACaseSensitive: Boolean): integer;
begin
  Result:=Pages.Count-1;
  if ACaseSensitive then
  begin                          // Case sensitive search
    while (Result>=0) and (Pages[Result].PageName <> APageName) do
      dec(Result);
  end
  else begin                     // Case in-sensitive search
    while (Result>=0) and (AnsiCompareText(Pages[Result].PageName,APageName)<>0) do
      dec(Result);
  end;
end;

function TBaseComponentPalette.GetPage(const APageName: string;
  ACaseSensitive: Boolean=False): TBaseComponentPage;
var
  i: Integer;
begin
  i:=IndexOfPageName(APageName, ACaseSensitive);
  if i>=0 then
    Result:=Pages[i]
  else
    Result:=nil;
end;

procedure TBaseComponentPalette.AddComponent(NewComponent: TRegisteredComponent);
var
  NewPriority: TComponentPriority;
  InsertIndex: Integer;
begin
  // Store components to fComps, sorting them by priority.
  NewPriority:=NewComponent.GetPriority;
  InsertIndex:=0;
  while (InsertIndex<fComps.Count)
  and (ComparePriority(NewPriority,Comps[InsertIndex].GetPriority)<=0) do
    inc(InsertIndex);
  fComps.Insert(InsertIndex,NewComponent);
  DoPageAddedComponent(NewComponent);

  if NewComponent.FOrigPageName = '' then Exit;

  // See if page was added with different char case. Use the first version always.
  if fOrigPageHelper.Find(NewComponent.FOrigPageName, InsertIndex) then begin
    NewComponent.FOrigPageName := fOrigPageHelper[InsertIndex]; // Possibly different case
    Assert(fOrigPagePriorities.IndexOf(NewComponent.FOrigPageName) >= 0,
           'TBaseComponentPalette.AddComponent: FOrigPageName not found!');
  end
  else begin
    fOrigPageHelper.Add(NewComponent.FOrigPageName);
    Assert(fOrigPagePriorities.IndexOf(NewComponent.FOrigPageName) = -1,
           'TBaseComponentPalette.AddComponent: FOrigPageName exists but it should not!');
    // Store a list of page names and their priorities.
    InsertIndex:=0;
    while (InsertIndex<fOrigPagePriorities.Count)
    and (ComparePriority(NewPriority, fOrigPagePriorities.Data[InsertIndex])<=0) do
      inc(InsertIndex);
    fOrigPagePriorities.InsertKeyData(InsertIndex, NewComponent.FOrigPageName, NewPriority);
  end;
end;

procedure TBaseComponentPalette.RemoveComponent(AComponent: TRegisteredComponent);
begin
  fComps.Remove(AComponent);
  AComponent.RealPage:=nil;
  //ToDo: fix DoPageRemovedComponent(AComponent);
end;

function TBaseComponentPalette.FindComponent(const CompClassName: string): TRegisteredComponent;
var
  ANode: TAVLTreeNode;
begin
  ANode:=fComponentCache.FindKey(Pointer(CompClassName),
                                 @CompareClassNameWithRegisteredComponent);
  if ANode<>nil then
    Result:=TRegisteredComponent(ANode.Data)
  else
    Result:=nil;
end;

function TBaseComponentPalette.CreateNewClassName(const Prefix: string): string;
var
  i: Integer;
begin
  if FindComponent(Prefix)=nil then begin
    Result:=Prefix+'1';
  end else begin
    i:=1;
    repeat
      Result:=Prefix+IntToStr(i);
      inc(i);
    until FindComponent(Result)=nil;
  end;
end;

procedure TBaseComponentPalette.Update(ForceUpdateAll: Boolean);
begin
  fUserOrder.SortPagesAndCompsUserOrder;
  CreatePagesFromUserOrder;
end;

procedure TBaseComponentPalette.IterateRegisteredClasses(Proc: TGetComponentClassEvent);
var
  i: Integer;
begin
  for i:=0 to Comps.Count-1 do
    Proc(Comps[i].ComponentClass);
end;

procedure TBaseComponentPalette.SetSelectedComp(AComponent: TRegisteredComponent; AMulti: Boolean);
begin
  MultiSelect := AMulti;
  Selected := AComponent;
end;

// Execute handlers

function TBaseComponentPalette.VoteCompVisibility(AComponent: TRegisteredComponent): Boolean;
var
  i, Vote: Integer;
begin
  Vote:=1;
  if HideControls and AComponent.ComponentClass.InheritsFrom(TControl) then
    Dec(Vote);
  i:=FHandlers[cphtUpdateVisible].Count;
  while FHandlers[cphtUpdateVisible].NextDownIndex(i) do
    TUpdateCompVisibleEvent(FHandlers[cphtUpdateVisible][i])(AComponent,Vote);
  Result:=Vote>0;
  AComponent.Visible:=Result;
end;

procedure TBaseComponentPalette.DoAfterComponentAdded(ALookupRoot,
  AComponent: TComponent; ARegisteredComponent: TRegisteredComponent);
var
  i: Integer;
begin
  i:=FHandlers[cphtComponentAdded].Count;
  while FHandlers[cphtComponentAdded].NextDownIndex(i) do
    TComponentAddedEvent(FHandlers[cphtComponentAdded][i])(ALookupRoot, AComponent, ARegisteredComponent);
end;

procedure TBaseComponentPalette.DoAfterSelectionChanged;
var
  i: Integer;
begin
  i:=FHandlers[cphtSelectionChanged].Count;
  while FHandlers[cphtSelectionChanged].NextDownIndex(i) do
    TPaletteHandlerEvent(FHandlers[cphtSelectionChanged][i])();
end;

procedure TBaseComponentPalette.RemoveAllHandlersOfObject(AnObject: TObject);
var
  HandlerType: TComponentPaletteHandlerType;
begin
  for HandlerType:=Low(HandlerType) to High(HandlerType) do
    FHandlers[HandlerType].RemoveAllMethodsOfObject(AnObject);
end;

// Add / Remove handlers

// UpdateVisible
procedure TBaseComponentPalette.AddHandlerUpdateVisible(
  const OnUpdateCompVisibleEvent: TUpdateCompVisibleEvent; AsLast: boolean);
begin
  AddHandler(cphtUpdateVisible,TMethod(OnUpdateCompVisibleEvent),AsLast);
end;

procedure TBaseComponentPalette.RemoveHandlerUpdateVisible(
  OnUpdateCompVisibleEvent: TUpdateCompVisibleEvent);
begin
  RemoveHandler(cphtUpdateVisible,TMethod(OnUpdateCompVisibleEvent));
end;

// ComponentAdded
procedure TBaseComponentPalette.AddHandlerComponentAdded(
  OnComponentAddedEvent: TComponentAddedEvent);
begin
  AddHandler(cphtComponentAdded,TMethod(OnComponentAddedEvent));
end;

procedure TBaseComponentPalette.RemoveHandlerComponentAdded(
  OnComponentAddedEvent: TComponentAddedEvent);
begin
  RemoveHandler(cphtComponentAdded,TMethod(OnComponentAddedEvent));
end;

// SelectionChanged
procedure TBaseComponentPalette.AddHandlerSelectionChanged(
  OnSelectionChangedEvent: TPaletteHandlerEvent);
begin
  AddHandler(cphtSelectionChanged,TMethod(OnSelectionChangedEvent));
end;

procedure TBaseComponentPalette.RemoveHandlerSelectionChanged(
  OnSelectionChangedEvent: TPaletteHandlerEvent);
begin
  RemoveHandler(cphtSelectionChanged,TMethod(OnSelectionChangedEvent));
end;

end.

