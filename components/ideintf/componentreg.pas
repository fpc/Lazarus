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
  Classes, SysUtils, typinfo, Controls,
  Laz2_XMLCfg, LCLProc, fgl;

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
  public
    property PageNames: TStringList read FPageNames;
    property ComponentPages: TStringList read FComponentPages;
  end;

  { TCompPaletteOptions }

  TCompPaletteOptions = class(TBaseCompPaletteOptions)
  private
    // Pages removed or renamed. They must be hidden in the palette.
    FHiddenPageNames: TStringList;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    procedure Assign(Source: TCompPaletteOptions);
    function IsDefault: Boolean;
    function Load(XMLConfig: TXMLConfig): boolean;
    function Save(XMLConfig: TXMLConfig): boolean;
  public
    property HiddenPageNames: TStringList read FHiddenPageNames;
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
    function IsTControl: boolean;
  public
    property ComponentClass: TComponentClass read FComponentClass;
    property OnGetCreationClass: TOnGetCreationClass read FOnGetCreationClass
                                                     write FOnGetCreationClass;
    property OrigPageName: string read FOrigPageName;
    property RealPage: TBaseComponentPage read FRealPage write FRealPage;
    property Visible: boolean read FVisible write SetVisible;
  end;

  TRegisteredComponentClass = class of TRegisteredComponent;
  TRegisteredComponentList = specialize TFPGList<TRegisteredComponent>;


  { TBaseComponentPage }

  TBaseComponentPage = class
  private
    FPageName: string;
    FPalette: TBaseComponentPalette;
    FVisible: boolean;
  protected
    procedure SetVisible(const AValue: boolean); virtual;
    procedure OnComponentVisibleChanged(AComponent: TRegisteredComponent); virtual;
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
    cphtUpdateVisible, // visibility of component palette icons is recomputed
    cphtComponentAdded // Typically selection is changed after component was added.
    );

  TEndUpdatePaletteEvent = procedure(Sender: TObject; PaletteChanged: boolean) of object;
  TGetComponentClassEvent = procedure(const AClass: TComponentClass) of object;
  TUpdateCompVisibleEvent = procedure(AComponent: TRegisteredComponent;
                      var VoteVisible: integer { Visible>0 }  ) of object;
  TComponentAddedEvent = procedure of object;
  RegisterUnitComponentProc = procedure(const Page, UnitName: ShortString;
                                        ComponentClass: TComponentClass);
  TBaseComponentPageList = specialize TFPGList<TBaseComponentPage>;
  TPagePriorityList = specialize TFPGMap<String, TComponentPriority>;

  TBaseComponentPalette = class
  private
    FHandlers: array[TComponentPaletteHandlerType] of TMethodList;
    FBaseComponentPageClass: TBaseComponentPageClass;
    FRegisteredComponentClass: TRegisteredComponentClass;
    FOnBeginUpdate: TNotifyEvent;
    FOnEndUpdate: TEndUpdatePaletteEvent;
    FHideControls: boolean;
    FUpdateLock: integer;
    fChanged: boolean;
    fChangeStamp: integer;
    // Used to find names that differ in character case only.
    fOrigPageHelper: TStringList;
    procedure AddHandler(HandlerType: TComponentPaletteHandlerType;
                         const AMethod: TMethod; AsLast: boolean = false);
    procedure RemoveHandler(HandlerType: TComponentPaletteHandlerType;
                            const AMethod: TMethod);
  protected
    // List of pages, created based on user ordered and original pages.
    fPages: TBaseComponentPageList;
    // List of all components in all pages.
    fComps: TRegisteredComponentList;
    // New pages added and their priorities, ordered by priority.
    fOrigPagePriorities: TPagePriorityList;
    procedure DoChange; virtual;
    procedure DoBeginUpdate; virtual;
    procedure DoEndUpdate(Changed: boolean); virtual;
    procedure OnPageAddedComponent({%H-}Component: TRegisteredComponent); virtual;
    procedure OnPageRemovedComponent({%H-}Page: TBaseComponentPage;
                                {%H-}Component: TRegisteredComponent); virtual;
    procedure OnComponentVisibleChanged({%H-}AComponent: TRegisteredComponent); virtual;
    procedure OnPageVisibleChanged({%H-}APage: TBaseComponentPage); virtual;
    function VoteCompVisibility(AComponent: TRegisteredComponent): Boolean; virtual;
    function GetSelected: TRegisteredComponent; virtual;
    procedure SetBaseComponentPageClass(const AValue: TBaseComponentPageClass); virtual;
    procedure SetRegisteredComponentClass(const AValue: TRegisteredComponentClass); virtual;
    procedure SetSelected(const AValue: TRegisteredComponent); virtual; abstract;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    function AssignOrigCompsForPage(PageName: string;
                                    DestComps: TStringList): Boolean; virtual; abstract;
    function AssignOrigVisibleCompsForPage(PageName: string;
                                    DestComps: TStringList): Boolean; virtual; abstract;
    function RefUserCompsForPage(PageName: string): TStringList; virtual; abstract;
    procedure BeginUpdate(Change: boolean);
    procedure EndUpdate;
    function IsUpdateLocked: boolean;
    procedure IncChangeStamp;
    procedure DoAfterComponentAdded; virtual;
    function IndexOfPageName(const APageName: string): integer;
    function IndexOfPageWithName(const APageName: string): integer;
    function GetPage(const APageName: string; aCaseSens: Boolean = False): TBaseComponentPage;
    procedure AddComponent(NewComponent: TRegisteredComponent);
    procedure RemoveComponent(AComponent: TRegisteredComponent);
    function FindComponent(const CompClassName: string): TRegisteredComponent; virtual;
    function CreateNewClassName(const Prefix: string): string;
    procedure Update(ForceUpdateAll: Boolean); virtual; abstract;
    procedure IterateRegisteredClasses(Proc: TGetComponentClassEvent);
    {$IFDEF CustomIDEComps}
    procedure RegisterCustomIDEComponents(
                        const RegisterProc: RegisterUnitComponentProc); virtual; abstract;
    {$ENDIF}
    procedure RemoveAllHandlersOfObject(AnObject: TObject);
    procedure AddHandlerUpdateVisible(
                        const OnUpdateCompVisibleEvent: TUpdateCompVisibleEvent;
                        AsLast: boolean = false);
    procedure RemoveHandlerUpdateVisible(
                        const OnUpdateCompVisibleEvent: TUpdateCompVisibleEvent);
    procedure AddHandlerComponentAdded(
                        const OnComponentAddedEvent: TComponentAddedEvent);
    procedure RemoveHandlerComponentAdded(
                        const OnComponentAddedEvent: TComponentAddedEvent);
  public
    property Pages: TBaseComponentPageList read fPages;
    property Comps: TRegisteredComponentList read fComps;
    property OrigPagePriorities: TPagePriorityList read fOrigPagePriorities;
    property BaseComponentPageClass: TBaseComponentPageClass read FBaseComponentPageClass;
    property RegisteredComponentClass: TRegisteredComponentClass
                                                 read FRegisteredComponentClass;
    property UpdateLock: integer read FUpdateLock;
    property ChangeStamp: integer read fChangeStamp;
    property OnBeginUpdate: TNotifyEvent read FOnBeginUpdate write FOnBeginUpdate;
    property OnEndUpdate: TEndUpdatePaletteEvent read FOnEndUpdate write FOnEndUpdate;
    property HideControls: boolean read FHideControls write FHideControls;
    property Selected: TRegisteredComponent read GetSelected write SetSelected;
  end;
  

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

function CompareIDEComponentByClassName(Data1, Data2: pointer): integer;
var
  Comp1: TRegisteredComponent;
  Comp2: TRegisteredComponent;
begin
  Comp1:=TRegisteredComponent(Data1);
  Comp2:=TRegisteredComponent(Data2);
  Result:=AnsiCompareText(Comp1.ComponentClass.Classname,
                          Comp2.ComponentClass.Classname);
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
  FComponentPages.Free;
  FPageNames.Free;
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

{ TCompPaletteOptions }

constructor TCompPaletteOptions.Create;
begin
  inherited Create;
  FHiddenPageNames := TStringList.Create;
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
end;

function TCompPaletteOptions.IsDefault: Boolean;
begin
  Result := (PageNames.Count = 0)
    and (ComponentPages.Count = 0)
    and (HiddenPageNames.Count = 0);
end;

function TCompPaletteOptions.Load(XMLConfig: TXMLConfig): boolean;
var
  CompList: TStringList;
  SubPath, CompPath: String;
  PageName, CompName: String;
  PageCount, CompCount: Integer;
  i, j: Integer;
begin
  Result:=False;
  try
    FPageNames.Clear;
    SubPath:=BasePath+'Pages/';
    PageCount:=XMLConfig.GetValue(SubPath+'Count', 0);
    for i:=1 to PageCount do begin
      PageName:=XMLConfig.GetValue(SubPath+'Item'+IntToStr(i)+'/Value', '');
      if PageName <> '' then
        FPageNames.Add(PageName);
    end;

    FHiddenPageNames.Clear;
    SubPath:=BasePath+'HiddenPages/';
    PageCount:=XMLConfig.GetValue(SubPath+'Count', 0);
    for i:=1 to PageCount do begin
      PageName:=XMLConfig.GetValue(SubPath+'Item'+IntToStr(i)+'/Value', '');
      if PageName <> '' then
        FHiddenPageNames.Add(PageName);
    end;

    FComponentPages.Clear;
    SubPath:=BasePath+'ComponentPages/';
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
  Result:=True;
end;

function TCompPaletteOptions.Save(XMLConfig: TXMLConfig): boolean;
var
  CompList: TStringList;
  SubPath, CompPath: String;
  i, j: Integer;
begin
  Result:=False;
  try
    SubPath:=BasePath+'Pages/';
    XMLConfig.DeletePath(SubPath);
    XMLConfig.SetDeleteValue(SubPath+'Count', FPageNames.Count, 0);
    for i:=0 to FPageNames.Count-1 do
      XMLConfig.SetDeleteValue(SubPath+'Item'+IntToStr(i+1)+'/Value', FPageNames[i], '');

    SubPath:=BasePath+'HiddenPages/';
    XMLConfig.DeletePath(SubPath);
    XMLConfig.SetDeleteValue(SubPath+'Count', FHiddenPageNames.Count, 0);
    for i:=0 to FHiddenPageNames.Count-1 do
      XMLConfig.SetDeleteValue(SubPath+'Item'+IntToStr(i+1)+'/Value', FHiddenPageNames[i], '');

    SubPath:=BasePath+'ComponentPages/';
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
  Result:=true;
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

function TRegisteredComponent.IsTControl: boolean;
begin
  Result:=ComponentClass.InheritsFrom(TControl);
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
  if (FPalette<>nil) then
    FPalette.OnPageVisibleChanged(Self);
end;

procedure TBaseComponentPage.OnComponentVisibleChanged(AComponent: TRegisteredComponent);
begin
  if FPalette<>nil then
    FPalette.OnComponentVisibleChanged(AComponent);
end;

{ TBaseComponentPalette }

constructor TBaseComponentPalette.Create;
begin
  fPages:=TBaseComponentPageList.Create;
  fComps:=TRegisteredComponentList.Create;
  fOrigPagePriorities:=TPagePriorityList.Create;
  fOrigPageHelper:=TStringList.Create; // Note: CaseSensitive = False
  fOrigPageHelper.Sorted:=True;
end;

destructor TBaseComponentPalette.Destroy;
var
  HandlerType: TComponentPaletteHandlerType;
begin
  Clear;
  FreeAndNil(fOrigPageHelper);
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

procedure TBaseComponentPalette.AddHandler(HandlerType: TComponentPaletteHandlerType;
  const AMethod: TMethod; AsLast: boolean);
begin
  if FHandlers[HandlerType]=nil then
    FHandlers[HandlerType]:=TMethodList.Create;
  FHandlers[HandlerType].Add(AMethod,AsLast);
end;

function TBaseComponentPalette.GetSelected: TRegisteredComponent;
begin
  result := nil;
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

procedure TBaseComponentPalette.DoBeginUpdate;
begin

end;

procedure TBaseComponentPalette.DoEndUpdate(Changed: boolean);
begin
  if Assigned(OnEndUpdate) then OnEndUpdate(Self,Changed);
end;

procedure TBaseComponentPalette.OnPageAddedComponent(Component: TRegisteredComponent);
begin
  DoChange;
end;

procedure TBaseComponentPalette.OnPageRemovedComponent(Page: TBaseComponentPage;
  Component: TRegisteredComponent);
begin
  DoChange;
end;

procedure TBaseComponentPalette.OnComponentVisibleChanged(AComponent: TRegisteredComponent);
begin
  ;
end;

procedure TBaseComponentPalette.OnPageVisibleChanged(APage: TBaseComponentPage);
begin
  ;
end;

function TBaseComponentPalette.VoteCompVisibility(AComponent: TRegisteredComponent): Boolean;
var
  i, Vote: Integer;
begin
  Vote:=1;
  if HideControls and AComponent.IsTControl then
    Dec(Vote);
  i:=FHandlers[cphtUpdateVisible].Count;
  while FHandlers[cphtUpdateVisible].NextDownIndex(i) do
    TUpdateCompVisibleEvent(FHandlers[cphtUpdateVisible][i])(AComponent,Vote);
  Result:=Vote>0;
  AComponent.Visible:=Result;
end;

procedure TBaseComponentPalette.SetBaseComponentPageClass(
  const AValue: TBaseComponentPageClass);
begin
  FBaseComponentPageClass:=AValue;
end;

procedure TBaseComponentPalette.SetRegisteredComponentClass(
  const AValue: TRegisteredComponentClass);
begin
  FRegisteredComponentClass:=AValue;
end;

procedure TBaseComponentPalette.BeginUpdate(Change: boolean);
begin
  inc(FUpdateLock);
  if FUpdateLock=1 then begin
    fChanged:=Change;
    DoBeginUpdate;
    if Assigned(OnBeginUpdate) then OnBeginUpdate(Self);
  end else
    fChanged:=fChanged or Change;
end;

procedure TBaseComponentPalette.EndUpdate;
begin
  if FUpdateLock<=0 then RaiseException('TBaseComponentPalette.EndUpdate');
  dec(FUpdateLock);
  if FUpdateLock=0 then DoEndUpdate(fChanged);
end;

function TBaseComponentPalette.IsUpdateLocked: boolean;
begin
  Result:=FUpdateLock>0;
end;

procedure TBaseComponentPalette.IncChangeStamp;
begin
  Inc(fChangeStamp);
end;

procedure TBaseComponentPalette.DoAfterComponentAdded;
var
  i: Integer;
begin
  i:=FHandlers[cphtComponentAdded].Count;
  while FHandlers[cphtComponentAdded].NextDownIndex(i) do
    TComponentAddedEvent(FHandlers[cphtComponentAdded][i])();
end;

function TBaseComponentPalette.IndexOfPageName(const APageName: string): integer;
begin
  Result:=Pages.Count-1;         // Case sensitive search
  while (Result>=0) and (Pages[Result].PageName <> APageName) do
    dec(Result);
end;

function TBaseComponentPalette.IndexOfPageWithName(const APageName: string): integer;
begin
  Result:=Pages.Count-1;         // Case in-sensitive search
  while (Result>=0) and (AnsiCompareText(Pages[Result].PageName,APageName)<>0) do
    dec(Result);
end;

function TBaseComponentPalette.GetPage(const APageName: string;
  aCaseSens: Boolean = False): TBaseComponentPage;
var
  i: Integer;
begin
  if aCaseSens then
    i:=IndexOfPageName(APageName)
  else
    i:=IndexOfPageWithName(APageName);
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
  OnPageAddedComponent(NewComponent);

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
  //ToDo: fix OnPageRemovedComponent(AComponent.RealPage,AComponent);
end;

function TBaseComponentPalette.FindComponent(const CompClassName: string): TRegisteredComponent;
var
  i: Integer;
begin
  for i:=0 to Comps.Count-1 do begin
    Result:=Comps[i];
    if CompareText(Result.ComponentClass.ClassName,CompClassName) = 0 then
      exit;
  end;
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

procedure TBaseComponentPalette.IterateRegisteredClasses(Proc: TGetComponentClassEvent);
var
  i: Integer;
begin
  for i:=0 to Comps.Count-1 do
    Proc(Comps[i].ComponentClass);
end;

procedure TBaseComponentPalette.RemoveAllHandlersOfObject(AnObject: TObject);
var
  HandlerType: TComponentPaletteHandlerType;
begin
  for HandlerType:=Low(HandlerType) to High(HandlerType) do
    FHandlers[HandlerType].RemoveAllMethodsOfObject(AnObject);
end;

procedure TBaseComponentPalette.AddHandlerUpdateVisible(
  const OnUpdateCompVisibleEvent: TUpdateCompVisibleEvent; AsLast: boolean);
begin
  AddHandler(cphtUpdateVisible,TMethod(OnUpdateCompVisibleEvent),AsLast);
end;

procedure TBaseComponentPalette.RemoveHandlerUpdateVisible(
  const OnUpdateCompVisibleEvent: TUpdateCompVisibleEvent);
begin
  RemoveHandler(cphtUpdateVisible,TMethod(OnUpdateCompVisibleEvent));
end;

procedure TBaseComponentPalette.AddHandlerComponentAdded(
  const OnComponentAddedEvent: TComponentAddedEvent);
begin
  AddHandler(cphtComponentAdded,TMethod(OnComponentAddedEvent));
end;

procedure TBaseComponentPalette.RemoveHandlerComponentAdded(
  const OnComponentAddedEvent: TComponentAddedEvent);
begin
  RemoveHandler(cphtComponentAdded,TMethod(OnComponentAddedEvent));
end;

end.

