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

  Author: Mattias Gaertner

  Abstract:
    Interface to the component palette and the registered component classes.
}
unit ComponentReg;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, typinfo, Controls, ComCtrls, Forms,
  LazarusPackageIntf, LazConfigStorage, LCLProc, fgl;

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

  { TCompPaletteOptions }

  TCompPaletteOptions = class
  private
    FConfigStore: TConfigStorage;
    // Pages reordered by user.
    FPageNames: TStringList;
    // Pages removed or renamed. They must be hidden in the palette.
    FHiddenPageNames: TStringList;
    // List of page names with changed component contents.
    // Object holds another StringList for the component names.
    FComponentPages: TStringList;
  public
    constructor Create;
    destructor Destroy; override;
    procedure ClearComponentPages;
    procedure AssignComponentPages(aPageName: string; aList: TStringList);
    function Load: boolean;
    function Save: boolean;
  public
    property ConfigStore: TConfigStorage read FConfigStore write FConfigStore;
    property PageNames: TStringList read FPageNames;
    property HiddenPageNames: TStringList read FHiddenPageNames;
    property ComponentPages: TStringList read FComponentPages;
  end;


  { TRegisteredComponent }

  TRegisteredComponent = class
  private
    FButton: TComponent;
    FComponentClass: TComponentClass;
    FOnGetCreationClass: TOnGetCreationClass;
    FRealPage: TBaseComponentPage;
    FOrigPageName: string;
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
    property Button: TComponent read FButton write FButton;
    property Visible: boolean read FVisible write SetVisible;
  end;
  TRegisteredComponentClass = class of TRegisteredComponent;


  { TBaseComponentPage }

  TBaseComponentPage = class
  private
    FPageComponent: TCustomPage;
    FPageName: string;
    FPalette: TBaseComponentPalette;
    FPriority: TComponentPriority;
    FSelectButton: TComponent;
    FVisible: boolean;
  protected
    procedure SetVisible(const AValue: boolean); virtual;
    procedure OnComponentVisibleChanged(AComponent: TRegisteredComponent); virtual;
  public
    constructor Create(const ThePageName: string);
    destructor Destroy; override;
    function FindComponent(const CompClassName: string): TRegisteredComponent;
    function FindButton(Button: TComponent): TRegisteredComponent;
    procedure UpdateVisible;
    function GetScrollBox: TScrollBox;
  public
    property PageName: string read FPageName;
    property Palette: TBaseComponentPalette read FPalette;
    property Priority: TComponentPriority read FPriority write FPriority;
    property PageComponent: TCustomPage read FPageComponent write FPageComponent;
    property SelectButton: TComponent read FSelectButton write FSelectButton;
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
  TPagePriorityList = specialize TFPGMap<String, TComponentPriority>;

  TBaseComponentPalette = class
  private
    FPages: TList;  // list of TBaseComponentPage
    FComps: TList;  // list of all TRegisteredComponent in all pages
    // New pages added and their priorities, ordered by priority.
    fOrigPagePriorities: TPagePriorityList;
    // Pages ordered by user. Contains page name + another StringList
    //  for component names, just like TCompPaletteOptions.ComponentPages.
    fPagesUserOrder: TStringList;
    FHandlers: array[TComponentPaletteHandlerType] of TMethodList;
    FBaseComponentPageClass: TBaseComponentPageClass;
    FRegisteredComponentClass: TRegisteredComponentClass;
    FOnBeginUpdate: TNotifyEvent;
    FOnEndUpdate: TEndUpdatePaletteEvent;
    FHideControls: boolean;
    FUpdateLock: integer;
    fChanged: boolean;
    function GetPages(Index: integer): TBaseComponentPage;
    function GetComps(Index: integer): TRegisteredComponent;
    procedure AddHandler(HandlerType: TComponentPaletteHandlerType;
                         const AMethod: TMethod; AsLast: boolean = false);
    procedure RemoveHandler(HandlerType: TComponentPaletteHandlerType;
                            const AMethod: TMethod);
    procedure SetHideControls(const AValue: boolean);
  protected
    procedure DoChange; virtual;
    procedure DoBeginUpdate; virtual;
    procedure DoEndUpdate(Changed: boolean); virtual;
    procedure OnPageAddedComponent({%H-}Component: TRegisteredComponent); virtual;
    procedure OnPageRemovedComponent({%H-}Page: TBaseComponentPage;
                                {%H-}Component: TRegisteredComponent); virtual;
    procedure OnComponentVisibleChanged({%H-}AComponent: TRegisteredComponent); virtual;
    procedure OnPageVisibleChanged({%H-}APage: TBaseComponentPage); virtual;
    procedure Update; virtual;
    function UpdateVisible(AComponent: TRegisteredComponent): Boolean; virtual;
    function GetSelected: TRegisteredComponent; virtual;
    procedure SetBaseComponentPageClass(const AValue: TBaseComponentPageClass); virtual;
    procedure SetRegisteredComponentClass(const AValue: TRegisteredComponentClass); virtual;
    procedure SetSelected(const AValue: TRegisteredComponent); virtual; abstract;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    procedure ClearButtons; virtual; abstract;
    procedure BeginUpdate(Change: boolean);
    procedure EndUpdate;
    function IsUpdateLocked: boolean;
    procedure DoAfterComponentAdded; virtual;
    function PageCount: integer;
    function GetPage(const APageName: string; aCaseSens: Boolean = False): TBaseComponentPage;
    function IndexOfPageName(const APageName: string): integer;
    function IndexOfPageWithName(const APageName: string): integer;
    function CompCount: integer;
    procedure AddComponent(NewComponent: TRegisteredComponent);
    function CreateNewPage(const NewPageName: string;
                        const Priority: TComponentPriority): TBaseComponentPage;
    procedure RemoveComponent(AComponent: TRegisteredComponent);
    function FindComponent(const CompClassName: string): TRegisteredComponent; virtual;
    function FindButton(Button: TComponent): TRegisteredComponent;
    function CreateNewClassName(const Prefix: string): string;
    function IndexOfPageComponent(AComponent: TComponent): integer;
    procedure UpdateVisible; virtual;
    procedure IterateRegisteredClasses(Proc: TGetComponentClassEvent);
    procedure RegisterCustomIDEComponents(
                        const RegisterProc: RegisterUnitComponentProc); virtual; abstract;
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
    property Pages[Index: integer]: TBaseComponentPage read GetPages; default;
    property Comps[Index: integer]: TRegisteredComponent read GetComps;
    property BaseComponentPageClass: TBaseComponentPageClass read FBaseComponentPageClass;
    property RegisteredComponentClass: TRegisteredComponentClass
                                                 read FRegisteredComponentClass;
    property UpdateLock: integer read FUpdateLock;
    property OnBeginUpdate: TNotifyEvent read FOnBeginUpdate write FOnBeginUpdate;
    property OnEndUpdate: TEndUpdatePaletteEvent read FOnEndUpdate write FOnEndUpdate;
    property HideControls: boolean read FHideControls write SetHideControls;
    property Selected: TRegisteredComponent read GetSelected write SetSelected;
    property OrigPagePriorities: TPagePriorityList read fOrigPagePriorities;
    property PagesUserOrder: TStringList read fPagesUserOrder;
  end;
  

var
  IDEComponentPalette: TBaseComponentPalette = nil;

function ComponentPriority(Category: TComponentPriorityCategory; Level: integer): TComponentPriority;
function ComparePriority(const p1,p2: TComponentPriority): integer;
function CompareIDEComponentByClassName(Data1, Data2: pointer): integer;
function dbgs(const c: TComponentPriorityCategory): string; overload;
function dbgs(const p: TComponentPriority): string; overload;

implementation

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

{ TCompPaletteOptions }

constructor TCompPaletteOptions.Create;
begin
  inherited Create;
  FPageNames := TStringList.Create;
  FHiddenPageNames := TStringList.Create;
  FComponentPages := TStringList.Create;
end;

destructor TCompPaletteOptions.Destroy;
begin
  ClearComponentPages;
  FComponentPages.Free;
  FHiddenPageNames.Free;
  FPageNames.Free;
  inherited Destroy;
end;

procedure TCompPaletteOptions.ClearComponentPages;
var
  i: Integer;
begin
  for i:=0 to FComponentPages.Count-1 do
    FComponentPages.Objects[i].Free;   // Free also the contained StringList.
  FComponentPages.Clear;
end;

procedure TCompPaletteOptions.AssignComponentPages(aPageName: string; aList: TStringList);
var
  sl: TStringList;
begin
  sl := TStringList.Create;
  sl.Assign(aList);
  FComponentPages.AddObject(aPageName, sl);
end;

function TCompPaletteOptions.Load: boolean;
var
  CompList: TStringList;
  Path, SubPath, CompPath: String;
  PageName, CompName: String;
  PageCount, CompCount: Integer;
  i, j: Integer;
begin
  Result:=False;
  if ConfigStore=nil then exit;
  try
    Path:='ComponentPaletteOptions/';
    //FileVersion := ConfigStore.GetValue(Path+'Version/Value',0);

    FPageNames.Clear;
    SubPath:=Path+'Pages/';
    PageCount:=ConfigStore.GetValue(SubPath+'Count', 0);
    for i:=1 to PageCount do begin
      PageName:=ConfigStore.GetValue(SubPath+'Item'+IntToStr(i)+'/Value', '');
      FPageNames.Add(PageName);
    end;

    FHiddenPageNames.Clear;
    SubPath:=Path+'HiddenPages/';
    PageCount:=ConfigStore.GetValue(SubPath+'Count', 0);
    for i:=1 to PageCount do begin
      PageName:=ConfigStore.GetValue(SubPath+'Item'+IntToStr(i)+'/Value', '');
      FHiddenPageNames.Add(PageName);
    end;

    ClearComponentPages;
    SubPath:=Path+'ComponentPages/';
    PageCount:=ConfigStore.GetValue(SubPath+'Count', 0);
    for i:=1 to PageCount do begin
      CompPath:=SubPath+'Page'+IntToStr(i)+'/';
      PageName:=ConfigStore.GetValue(CompPath+'Value', '');
      CompList:=TStringList.Create;
      CompCount:=ConfigStore.GetValue(CompPath+'Components/Count', 0);
      for j:=1 to CompCount do begin
        CompName:=ConfigStore.GetValue(CompPath+'Components/Item'+IntToStr(j)+'/Value', '');
        CompList.Add(CompName);
      end;
      FComponentPages.AddObject(PageName, CompList); // CompList is owned by FComponentPages
    end;
  except
    on E: Exception do begin
      DebugLn('ERROR: TOIOptions.Load: ',E.Message);
      exit;
    end;
  end;
  Result:=True;
end;

function TCompPaletteOptions.Save: boolean;
var
  CompList: TStringList;
  Path, SubPath, CompPath: String;
  i, j: Integer;
begin
  Result:=False;
  if ConfigStore=nil then exit;
  try
    Path:='ComponentPaletteOptions/';

    SubPath:=Path+'Pages/';
    ConfigStore.SetDeleteValue(SubPath+'Count', FPageNames.Count, 0);
    for i:=0 to FPageNames.Count-1 do
      ConfigStore.SetDeleteValue(SubPath+'Item'+IntToStr(i+1)+'/Value', FPageNames[i], '');

    SubPath:=Path+'HiddenPages/';
    ConfigStore.SetDeleteValue(SubPath+'Count', FHiddenPageNames.Count, 0);
    for i:=0 to FHiddenPageNames.Count-1 do
      ConfigStore.SetDeleteValue(SubPath+'Item'+IntToStr(i+1)+'/Value', FHiddenPageNames[i], '');

    SubPath:=Path+'ComponentPages/';
    ConfigStore.SetDeleteValue(SubPath+'Count', FComponentPages.Count, 0);
    for i:=0 to FComponentPages.Count-1 do begin
      CompList:=FComponentPages.Objects[i] as TStringList;
      CompPath:=SubPath+'Page'+IntToStr(i+1)+'/';
      ConfigStore.SetDeleteValue(CompPath+'Value', FComponentPages[i], '');
      ConfigStore.SetDeleteValue(CompPath+'Components/Count', CompList.Count, 0);
      for j:=0 to CompList.Count-1 do
        ConfigStore.SetDeleteValue(CompPath+'Components/Item'+IntToStr(j+1)+'/Value', CompList[j], '');
    end;
  except
    on E: Exception do begin
      DebugLn('ERROR: TOIOptions.Save: ',E.Message);
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
  FreeAndNil(FButton);
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

constructor TBaseComponentPage.Create(const ThePageName: string);
begin
  FPageName:=ThePageName;
  FVisible:=FPageName<>'';
end;

destructor TBaseComponentPage.Destroy;
begin
  FreeAndNil(FPageComponent);
  FreeAndNil(FSelectButton);
  inherited Destroy;
end;

function TBaseComponentPage.FindComponent(const CompClassName: string): TRegisteredComponent;
var
  i: Integer;
begin
  for i:=0 to Palette.CompCount-1 do begin
    Result:=Palette.Comps[i];
    if (Result.RealPage = Self)
    and (CompareText(Result.ComponentClass.ClassName,CompClassName) = 0) then
      exit;
  end;
  Result:=nil;
end;

function TBaseComponentPage.FindButton(Button: TComponent): TRegisteredComponent;
var
  i: Integer;
begin
  for i:=0 to Palette.CompCount-1 do begin
    Result:=Palette.Comps[i];
    if Result.Button=Button then exit;
  end;
  Result:=nil;
end;

procedure TBaseComponentPage.UpdateVisible;
var
  i: Integer;
  HasVisibleComponents: Boolean;
begin
  if Palette = nil then Exit;
  HasVisibleComponents:=false;
  for i:=0 to Palette.CompCount-1 do
    if (Palette.Comps[i].RealPage = Self) then
      if Palette.UpdateVisible(Palette.Comps[i]) then
        HasVisibleComponents:=true;
  Visible:=HasVisibleComponents and (PageName<>'');
end;

function TBaseComponentPage.GetScrollBox: TScrollBox;
begin
  if Assigned(PageComponent) and (PageComponent.ComponentCount > 0)
  and (PageComponent.Components[0] is TScrollBox) then
    Result := TScrollBox(PageComponent.Components[0])
  else
    Result := Nil;
end;

{ TBaseComponentPalette }

function TBaseComponentPalette.GetPages(Index: integer): TBaseComponentPage;
begin
  Result:=TBaseComponentPage(FPages[Index]);
end;

function TBaseComponentPalette.GetComps(Index: integer): TRegisteredComponent;
begin
  Result:=TRegisteredComponent(FComps[Index])
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

procedure TBaseComponentPalette.SetHideControls(const AValue: boolean);
begin
  if FHideControls=AValue then exit;
  FHideControls:=AValue;
  UpdateVisible;
end;

procedure TBaseComponentPalette.DoChange;
begin
  if FUpdateLock>0 then
    fChanged:=true
  else
    Update;
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
  DoChange;
end;

procedure TBaseComponentPalette.OnPageVisibleChanged(APage: TBaseComponentPage);
begin
  DoChange;
end;

procedure TBaseComponentPalette.Update;
begin

end;

function TBaseComponentPalette.UpdateVisible(AComponent: TRegisteredComponent): Boolean;
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

constructor TBaseComponentPalette.Create;
begin
  FPages:=TList.Create;
  FComps:=TList.Create;
  fOrigPagePriorities:=TPagePriorityList.Create;
  fPagesUserOrder:=TStringList.Create;
end;

destructor TBaseComponentPalette.Destroy;
var
  HandlerType: TComponentPaletteHandlerType;
  i: Integer;
begin
  Clear;
  for i := 0 to fPagesUserOrder.Count-1 do
    fPagesUserOrder.Objects[i].Free;     // Free also contained StringLists.
  FreeAndNil(fPagesUserOrder);
  FreeAndNil(fOrigPagePriorities);
  FreeAndNil(FComps);
  FreeAndNil(FPages);
  for HandlerType:=Low(HandlerType) to High(HandlerType) do
    FHandlers[HandlerType].Free;
  inherited Destroy;
end;

procedure TBaseComponentPalette.Clear;
var
  i: Integer;
begin
  ClearButtons;
  for i:=0 to FComps.Count-1 do
    Comps[i].RealPage:=nil;
  FComps.Clear;
  for i:=0 to FPages.Count-1 do
    Pages[i].Free;
  FPages.Clear;
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

procedure TBaseComponentPalette.DoAfterComponentAdded;
var
  i: Integer;
begin
  i:=FHandlers[cphtComponentAdded].Count;
  while FHandlers[cphtComponentAdded].NextDownIndex(i) do
    TComponentAddedEvent(FHandlers[cphtComponentAdded][i])();
end;

function TBaseComponentPalette.PageCount: integer;
begin
  Result:=FPages.Count;
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

function TBaseComponentPalette.IndexOfPageName(const APageName: string): integer;
begin
  Result:=PageCount-1;         // Case sensitive search
  while (Result>=0) and (Pages[Result].PageName <> APageName) do
    dec(Result);
end;

function TBaseComponentPalette.IndexOfPageWithName(const APageName: string): integer;
begin
  Result:=PageCount-1;         // Case in-sensitive search
  while (Result>=0) and (AnsiCompareText(Pages[Result].PageName,APageName)<>0) do
    dec(Result);
end;

function TBaseComponentPalette.CompCount: integer;
begin
  Result:=FComps.Count;
end;

procedure TBaseComponentPalette.AddComponent(NewComponent: TRegisteredComponent);
var
  NewPriority: TComponentPriority;
  InsertIndex: Integer;
begin
  // Store components to FComps, sorting them by priority.
  NewPriority:=NewComponent.GetPriority;
  InsertIndex:=0;
  while (InsertIndex<FComps.Count)
  and (ComparePriority(NewPriority,Comps[InsertIndex].GetPriority)<=0) do
    inc(InsertIndex);
  FComps.Insert(InsertIndex,NewComponent);
  OnPageAddedComponent(NewComponent);

  // Store a list of page names and their priorities.
  if (NewComponent.OrigPageName <> '')
  and (fOrigPagePriorities.IndexOf(NewComponent.OrigPageName) = -1) then begin
    InsertIndex:=0;
    while (InsertIndex<fOrigPagePriorities.Count)
    and (ComparePriority(NewPriority, fOrigPagePriorities.Data[InsertIndex])<=0) do
      inc(InsertIndex);
    fOrigPagePriorities.InsertKeyData(InsertIndex, NewComponent.OrigPageName, NewPriority);
  end;
end;

function TBaseComponentPalette.CreateNewPage(const NewPageName: string;
  const Priority: TComponentPriority): TBaseComponentPage;
var
  InsertIndex: Integer;
begin
  Result:=TBaseComponentPage.Create(NewPageName);
  Result.Priority:=Priority;
  InsertIndex:=0;
  while (InsertIndex<PageCount)
  and (ComparePriority(Priority,Pages[InsertIndex].Priority)<=0) do
    inc(InsertIndex);
  FPages.Insert(InsertIndex,Result);
  Result.FPalette:=Self;
  if CompareText(NewPageName,'Hidden')=0 then
    Result.Visible:=false;
end;

procedure TBaseComponentPalette.RemoveComponent(AComponent: TRegisteredComponent);
begin
  FComps.Remove(AComponent);
  AComponent.RealPage:=nil;
  //ToDo: fix OnPageRemovedComponent(AComponent.RealPage,AComponent);
end;

function TBaseComponentPalette.FindComponent(const CompClassName: string): TRegisteredComponent;
var
  i: Integer;
begin
  for i:=0 to PageCount-1 do begin
    Result:=Pages[i].FindComponent(CompClassName);
    if Result<>nil then exit;
  end;
  Result:=nil;
end;

function TBaseComponentPalette.FindButton(Button: TComponent): TRegisteredComponent;
var
  i: Integer;
begin
  for i:=0 to PageCount-1 do begin
    Result:=Pages[i].FindButton(Button);
    if Result<>nil then exit;
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

function TBaseComponentPalette.IndexOfPageComponent(AComponent: TComponent): integer;
begin
  if AComponent<>nil then begin
    Result:=PageCount-1;
    while (Result>=0) and (Pages[Result].PageComponent<>AComponent) do
      dec(Result);
  end else
    Result:=-1;
end;

procedure TBaseComponentPalette.UpdateVisible;
var
  i: Integer;
begin
  BeginUpdate(false);
  for i:=0 to PageCount-1 do
    Pages[i].UpdateVisible;
  EndUpdate;
end;

procedure TBaseComponentPalette.IterateRegisteredClasses(Proc: TGetComponentClassEvent);
var
  i: Integer;
begin
  for i:=0 to CompCount-1 do
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

