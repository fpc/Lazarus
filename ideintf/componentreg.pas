{  $Id$  }
{
 /***************************************************************************
                            componentreg.pas
                            ----------------

 ***************************************************************************/

 *****************************************************************************
 *                                                                           *
 *  See the file COPYING.modifiedLGPL.txt, included in this distribution,    *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************

  Author: Mattias Gaertner

  Abstract:
    Interface to the component palette and the registered component classes.
}
unit ComponentReg;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, typinfo, Controls, LazarusPackageIntf, LCLProc;

type
  TComponentPriorityCategory = (
    cpBase,
    cpRecommended,
    cpNormal,
    cpOptional
    );
    
  TComponentPriority = record
    Category: TComponentPriorityCategory;
    Level: integer; // higher level means higher priority (range: -1000 to 1000)
  end;
    
const
  ComponentPriorityNormal: TComponentPriority = (Category: cpNormal; Level:0);

  LCLCompPriority: TComponentPriority = (Category: cpBase; Level: 10);
  FCLCompPriority: TComponentPriority = (Category: cpBase; Level: 9);
  IDEIntfCompPriority: TComponentPriority = (Category: cpBase; Level: 8);

type
  TBaseComponentPage = class;
  TBaseComponentPalette = class;
  TOnGetCreationClass = procedure(Sender: TObject;
                              var NewComponentClass: TComponentClass) of object;

  { TRegisteredComponent }

  TRegisteredComponent = class
  private
    FButton: TComponent;
    FComponentClass: TComponentClass;
    FOnGetCreationClass: TOnGetCreationClass;
    FPage: TBaseComponentPage;
    FPageName: string;
  protected
    FVisible: boolean;
    procedure SetVisible(const AValue: boolean); virtual;
    procedure FreeButton;
  public
    constructor Create(TheComponentClass: TComponentClass;
      const ThePageName: string);
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
    property PageName: string read FPageName;
    property Page: TBaseComponentPage read FPage write FPage;
    property Button: TComponent read FButton write FButton;
    property Visible: boolean read FVisible write SetVisible;
    property OnGetCreationClass: TOnGetCreationClass read FOnGetCreationClass
                                                     write FOnGetCreationClass;
  end;
  TRegisteredComponentClass = class of TRegisteredComponent;


  { TBaseComponentPage }

  TBaseComponentPage = class
  private
    FItems: TList; // list of TRegisteredComponent
    FPageComponent: TComponent;
    FPageName: string;
    FPalette: TBaseComponentPalette;
    FPriority: TComponentPriority;
    FSelectButton: TComponent;
    function GetItems(Index: integer): TRegisteredComponent;
  protected
    FVisible: boolean;
    procedure SetVisible(const AValue: boolean); virtual;
    procedure OnComponentVisibleChanged(AComponent: TRegisteredComponent); virtual;
  public
    constructor Create(const ThePageName: string);
    destructor Destroy; override;
    procedure Clear;
    procedure ClearButtons;
    procedure ConsistencyCheck;
    function Count: integer;
    procedure Add(NewComponent: TRegisteredComponent);
    procedure Remove(AComponent: TRegisteredComponent);
    function FindComponent(const CompClassName: string): TRegisteredComponent;
    function FindButton(Button: TComponent): TRegisteredComponent;
    procedure UpdateVisible;
    function GetMaxComponentPriority: TComponentPriority;
  public
    property Items[Index: integer]: TRegisteredComponent read GetItems; default;
    property PageName: string read FPageName;
    property Palette: TBaseComponentPalette read FPalette;
    property Priority: TComponentPriority read FPriority write FPriority;
    property PageComponent: TComponent read FPageComponent write FPageComponent;
    property SelectButton: TComponent read FSelectButton write FSelectButton;
    property Visible: boolean read FVisible write SetVisible;
  end;
  TBaseComponentPageClass = class of TBaseComponentPage;


  { TBaseComponentPalette }
  
  TComponentPaletteHandlerType = (
    cphtUpdateVisible // visibility of component palette icons is recomputed
    );

  TEndUpdatePaletteEvent =
    procedure(Sender: TObject; PaletteChanged: boolean) of object;
  TGetComponentClass = procedure(const AClass: TComponentClass) of object;
  TUpdateCompVisibleEvent = procedure(AComponent: TRegisteredComponent;
                      var VoteVisible: integer { Visible>0 }  ) of object;
  RegisterUnitComponentProc = procedure(const Page, UnitName: ShortString;
                                        ComponentClass: TComponentClass);

  TBaseComponentPalette = class
  private
    FHandlers: array[TComponentPaletteHandlerType] of TMethodList;
    FBaseComponentPageClass: TBaseComponentPageClass;
    FHideControls: boolean;
    FItems: TList; // list of TBaseComponentPage
    FOnBeginUpdate: TNotifyEvent;
    FOnEndUpdate: TEndUpdatePaletteEvent;
    FRegisteredComponentClass: TRegisteredComponentClass;
    FUpdateLock: integer;
    fChanged: boolean;
    function GetItems(Index: integer): TBaseComponentPage;
    procedure AddHandler(HandlerType: TComponentPaletteHandlerType;
                         const AMethod: TMethod; AsLast: boolean = false);
    procedure RemoveHandler(HandlerType: TComponentPaletteHandlerType;
                            const AMethod: TMethod);
    procedure SetHideControls(const AValue: boolean);
  protected
    procedure DoChange; virtual;
    procedure DoBeginUpdate; virtual;
    procedure DoEndUpdate(Changed: boolean); virtual;
    procedure OnPageAddedComponent(Component: TRegisteredComponent); virtual;
    procedure OnPageRemovedComponent(Page: TBaseComponentPage;
                                Component: TRegisteredComponent); virtual;
    procedure OnComponentVisibleChanged(
                                     AComponent: TRegisteredComponent); virtual;
    procedure OnPageVisibleChanged(APage: TBaseComponentPage); virtual;
    procedure Update; virtual;
    procedure UpdateVisible(AComponent: TRegisteredComponent); virtual;
    function GetSelected: TRegisteredComponent; virtual;
    procedure SetBaseComponentPageClass(
                                const AValue: TBaseComponentPageClass); virtual;
    procedure SetRegisteredComponentClass(
                              const AValue: TRegisteredComponentClass); virtual;
    procedure SetSelected(const AValue: TRegisteredComponent); virtual;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    procedure ClearButtons; virtual;
    procedure BeginUpdate(Change: boolean);
    procedure EndUpdate;
    function IsUpdateLocked: boolean;
    procedure DoAfterComponentAdded; virtual;
    procedure ConsistencyCheck;
    function Count: integer;
    function GetPage(const APageName: string;
                     CreateIfNotExists: boolean): TBaseComponentPage;
    function IndexOfPageWithName(const APageName: string): integer;
    procedure AddComponent(NewComponent: TRegisteredComponent);
    function CreateNewPage(const NewPageName: string;
                        const Priority: TComponentPriority): TBaseComponentPage;
    function FindComponent(const CompClassName: string
                           ): TRegisteredComponent; virtual;
    function FindButton(Button: TComponent): TRegisteredComponent;
    function CreateNewClassName(const Prefix: string): string;
    function IndexOfPageComponent(AComponent: TComponent): integer;
    procedure UpdateVisible; virtual;
    procedure IterateRegisteredClasses(Proc: TGetComponentClass);
    procedure RegisterCustomIDEComponents(
                        const RegisterProc: RegisterUnitComponentProc); virtual;
    procedure RemoveAllHandlersOfObject(AnObject: TObject);
    procedure AddHandlerUpdateVisible(
                        const OnUpdateCompVisibleEvent: TUpdateCompVisibleEvent;
                        AsLast: boolean = false);
    procedure RemoveHandlerUpdateVisible(
                       const OnUpdateCompVisibleEvent: TUpdateCompVisibleEvent);
  public
    property Pages[Index: integer]: TBaseComponentPage read GetItems; default;
    property UpdateLock: integer read FUpdateLock;
    property OnBeginUpdate: TNotifyEvent read FOnBeginUpdate
                                         write FOnBeginUpdate;
    property OnEndUpdate: TEndUpdatePaletteEvent read FOnEndUpdate
                                                 write FOnEndUpdate;
    property BaseComponentPageClass: TBaseComponentPageClass
                                                   read FBaseComponentPageClass;
    property RegisteredComponentClass: TRegisteredComponentClass
                                                 read FRegisteredComponentClass;
    property HideControls: boolean read FHideControls write SetHideControls;
    property Selected: TRegisteredComponent read GetSelected write SetSelected;
  end;
  

var
  IDEComponentPalette: TBaseComponentPalette;

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

{ TRegisteredComponent }

procedure TRegisteredComponent.SetVisible(const AValue: boolean);
begin
  if FVisible=AValue then exit;
  FVisible:=AValue;
  if (FPage<>nil) then FPage.OnComponentVisibleChanged(Self);
end;

procedure TRegisteredComponent.FreeButton;
begin
  FButton.Free;
  FButton:=nil;
end;

constructor TRegisteredComponent.Create(TheComponentClass: TComponentClass;
  const ThePageName: string);
begin
  FComponentClass:=TheComponentClass;
  FPageName:=ThePageName;
  FVisible:=true;
end;

destructor TRegisteredComponent.Destroy;
begin
  if FPage<>nil then FPage.Remove(Self);
  FreeButton;
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

function TBaseComponentPage.GetItems(Index: integer): TRegisteredComponent;
begin
  Result:=TRegisteredComponent(FItems[Index]);
end;

procedure TBaseComponentPage.SetVisible(const AValue: boolean);
begin
  if FVisible=AValue then exit;
  FVisible:=AValue;
  if (FPalette<>nil) then FPalette.OnPageVisibleChanged(Self);
end;

procedure TBaseComponentPage.OnComponentVisibleChanged(
  AComponent: TRegisteredComponent);
begin
  if FPalette<>nil then FPalette.OnComponentVisibleChanged(AComponent);
end;

constructor TBaseComponentPage.Create(const ThePageName: string);
begin
  FPageName:=ThePageName;
  FItems:=TList.Create;
  FVisible:=FPageName<>'';
end;

destructor TBaseComponentPage.Destroy;
begin
  Clear;
  FPageComponent.Free;
  FPageComponent:=nil;
  FSelectButton.Free;
  FSelectButton:=nil;
  FItems.Free;
  FItems:=nil;
  inherited Destroy;
end;

procedure TBaseComponentPage.Clear;
var
  i: Integer;
begin
  ClearButtons;
  for i:=0 to FItems.Count-1 do Items[i].Page:=nil;
  FItems.Clear;
end;

procedure TBaseComponentPage.ClearButtons;
var
  Cnt: Integer;
  i: Integer;
begin
  Cnt:=Count;
  for i:=0 to Cnt-1 do Items[i].FreeButton;
  FSelectButton.Free;
  FSelectButton:=nil;
end;

procedure TBaseComponentPage.ConsistencyCheck;
begin

end;

function TBaseComponentPage.Count: integer;
begin
  Result:=FItems.Count;
end;

procedure TBaseComponentPage.Add(NewComponent: TRegisteredComponent);
var
  InsertIndex: Integer;
  NewPriority: TComponentPriority;
begin
  NewPriority:=NewComponent.GetPriority;
  InsertIndex:=0;
  while (InsertIndex<Count)
  and (ComparePriority(NewPriority,Items[InsertIndex].GetPriority)<=0) do
    inc(InsertIndex);
  FItems.Insert(InsertIndex,NewComponent);
  NewComponent.Page:=Self;
  if FPalette<>nil then FPalette.OnPageAddedComponent(NewComponent);
end;

procedure TBaseComponentPage.Remove(AComponent: TRegisteredComponent);
begin
  FItems.Remove(AComponent);
  AComponent.Page:=nil;
  if FPalette<>nil then FPalette.OnPageRemovedComponent(Self,AComponent);
end;

function TBaseComponentPage.FindComponent(const CompClassName: string
  ): TRegisteredComponent;
var
  i: Integer;
begin
  for i:=0 to Count-1 do begin
    Result:=Items[i];
    if CompareText(Result.ComponentClass.ClassName,CompClassName)=0 then
      exit;
  end;
  Result:=nil;
end;

function TBaseComponentPage.FindButton(Button: TComponent
  ): TRegisteredComponent;
var
  i: Integer;
begin
  for i:=0 to Count-1 do begin
    Result:=Items[i];
    if Result.Button=Button then exit;
  end;
  Result:=nil;
end;

procedure TBaseComponentPage.UpdateVisible;
var
  i: Integer;
  HasVisibleComponents: Boolean;
begin
  if Palette<>nil then begin
    HasVisibleComponents:=false;
    for i:=0 to Count-1 do begin
      Palette.UpdateVisible(Items[i]);
      if Items[i].Visible then HasVisibleComponents:=true;
    end;
    Visible:=HasVisibleComponents and (PageName<>'');
  end;
end;

function TBaseComponentPage.GetMaxComponentPriority: TComponentPriority;
var
  i: Integer;
begin
  if Count=0 then
    Result:=ComponentPriorityNormal
  else begin
    Result:=Items[0].GetPriority;
    for i:=1 to Count-1 do
      if ComparePriority(Items[i].GetPriority,Result)>0 then
        Result:=Items[i].GetPriority;
  end;
end;

{ TBaseComponentPalette }

function TBaseComponentPalette.GetItems(Index: integer): TBaseComponentPage;
begin
  Result:=TBaseComponentPage(FItems[Index]);
end;

procedure TBaseComponentPalette.AddHandler(
  HandlerType: TComponentPaletteHandlerType; const AMethod: TMethod;
  AsLast: boolean);
begin
  if FHandlers[HandlerType]=nil then
    FHandlers[HandlerType]:=TMethodList.Create;
  FHandlers[HandlerType].Add(AMethod);
end;

function TBaseComponentPalette.GetSelected: TRegisteredComponent;
begin
  result := nil;
end;

procedure TBaseComponentPalette.RemoveHandler(
  HandlerType: TComponentPaletteHandlerType; const AMethod: TMethod);
begin
  FHandlers[HandlerType].Remove(AMethod);
end;

procedure TBaseComponentPalette.SetHideControls(const AValue: boolean);
begin
  if FHideControls=AValue then exit;
  FHideControls:=AValue;
  UpdateVisible;
end;

procedure TBaseComponentPalette.SetSelected(const AValue: TRegisteredComponent);
begin
  // ignore
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

procedure TBaseComponentPalette.OnPageAddedComponent(
  Component: TRegisteredComponent);
begin
  DoChange;
end;

procedure TBaseComponentPalette.OnPageRemovedComponent(
  Page: TBaseComponentPage; Component: TRegisteredComponent);
begin
  DoChange;
end;

procedure TBaseComponentPalette.OnComponentVisibleChanged(
  AComponent: TRegisteredComponent);
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

procedure TBaseComponentPalette.UpdateVisible(AComponent: TRegisteredComponent);
var
  Vote: Integer;
  i: LongInt;
begin
  Vote:=1;
  if HideControls and AComponent.IsTControl then dec(Vote);

  i:=FHandlers[cphtUpdateVisible].Count;
  while FHandlers[cphtUpdateVisible].NextDownIndex(i) do
    TUpdateCompVisibleEvent(FHandlers[cphtUpdateVisible][i])(AComponent,Vote);
  AComponent.Visible:=Vote>0;
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
  FItems:=TList.Create;
end;

destructor TBaseComponentPalette.Destroy;
begin
  Clear;
  FItems.Free;
  inherited Destroy;
end;

procedure TBaseComponentPalette.Clear;
var
  i: Integer;
begin
  for i:=0 to FItems.Count-1 do
    Pages[i].Free;
  FItems.Clear;
end;

procedure TBaseComponentPalette.ClearButtons;
var
  Cnt: Integer;
  i: Integer;
begin
  Cnt:=Count;
  for i:=0 to Cnt-1 do Pages[i].ClearButtons;
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
begin

end;

procedure TBaseComponentPalette.ConsistencyCheck;
begin

end;

function TBaseComponentPalette.Count: integer;
begin
  Result:=FItems.Count;
end;

function TBaseComponentPalette.GetPage(const APageName: string;
  CreateIfNotExists: boolean): TBaseComponentPage;
var
  i: Integer;
begin
  i:=IndexOfPageWithName(APageName);
  if i>=0 then begin
    Result:=Pages[i];
  end else begin
    if CreateIfNotExists then begin
      Result:=TBaseComponentPage.Create(APageName);
      Result.FPalette:=Self;
      FItems.Add(Result);
    end else
      Result:=nil;
  end;
end;

function TBaseComponentPalette.IndexOfPageWithName(const APageName: string
  ): integer;
begin
  Result:=Count-1;
  while (Result>=0) and (AnsiCompareText(Pages[Result].PageName,APageName)<>0)
  do
    dec(Result);
end;

procedure TBaseComponentPalette.AddComponent(NewComponent: TRegisteredComponent);
var
  CurPage: TBaseComponentPage;
begin
  CurPage:=GetPage(NewComponent.PageName,false);
  if CurPage=nil then
    CurPage:=CreateNewPage(NewComponent.PageName,NewComponent.GetPriority);
  CurPage.Add(NewComponent);
end;

function TBaseComponentPalette.CreateNewPage(const NewPageName: string;
  const Priority: TComponentPriority): TBaseComponentPage;
var
  InsertIndex: Integer;
begin
  Result:=TBaseComponentPage.Create(NewPageName);
  Result.Priority:=Priority;
  InsertIndex:=0;
  while (InsertIndex<Count)
  and (ComparePriority(Priority,Pages[InsertIndex].Priority)<=0) do
    inc(InsertIndex);
  FItems.Insert(InsertIndex,Result);
  Result.FPalette:=Self;
  if CompareText(NewPageName,'Hidden')=0 then
    Result.Visible:=false;
end;

function TBaseComponentPalette.FindComponent(const CompClassName: string
  ): TRegisteredComponent;
var
  i: Integer;
begin
  for i:=0 to Count-1 do begin
    Result:=Pages[i].FindComponent(CompClassName);
    if Result<>nil then exit;
  end;
  Result:=nil;
end;

function TBaseComponentPalette.FindButton(Button: TComponent
  ): TRegisteredComponent;
var
  i: Integer;
begin
  for i:=0 to Count-1 do begin
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

function TBaseComponentPalette.IndexOfPageComponent(AComponent: TComponent
  ): integer;
begin
  if AComponent<>nil then begin
    Result:=Count-1;
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
  for i:=0 to Count-1 do
    Pages[i].UpdateVisible;
  EndUpdate;
end;

procedure TBaseComponentPalette.IterateRegisteredClasses(
  Proc: TGetComponentClass);
var
  i: Integer;
  APage: TBaseComponentPage;
  j: Integer;
begin
  for i:=0 to Count-1 do begin
    APage:=Pages[i];
    for j:=0 to APage.Count-1 do
      Proc(APage[j].ComponentClass);
  end;
end;

procedure TBaseComponentPalette.RegisterCustomIDEComponents(
  const RegisterProc: RegisterUnitComponentProc);
begin

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
  AddHandler(cphtUpdateVisible,TMethod(OnUpdateCompVisibleEvent));
end;

procedure TBaseComponentPalette.RemoveHandlerUpdateVisible(
  const OnUpdateCompVisibleEvent: TUpdateCompVisibleEvent);
begin
  RemoveHandler(cphtUpdateVisible,TMethod(OnUpdateCompVisibleEvent));
end;


initialization
  IDEComponentPalette:=nil;
  
end.

