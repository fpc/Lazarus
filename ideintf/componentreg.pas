{  $Id$  }
{
 /***************************************************************************
                            componentreg.pas
                            ----------------

 ***************************************************************************/

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
    Interface to the component palette and the registered component classes.
}
unit ComponentReg;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, LazarusPackageIntf;

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

    
type
  TBaseComponentPage = class;
  TBaseComponentPalette = class;


  { TRegisteredComponent }

  TRegisteredComponent = class
  private
    FButton: TComponent;
    FComponentClass: TComponentClass;
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
    procedure ShowHideControl(Show: boolean);
    function IsTControl: boolean;
  public
    property ComponentClass: TComponentClass read FComponentClass;
    property PageName: string read FPageName;
    property Page: TBaseComponentPage read FPage write FPage;
    property Button: TComponent read FButton write FButton;
    property Visible: boolean read FVisible write SetVisible;
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
    procedure ShowHideControls(Show: boolean);
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
  
  TEndUpdatePaletteEvent =
    procedure(Sender: TObject; PaletteChanged: boolean) of object;
  TGetComponentClass = procedure(const AClass: TComponentClass) of object;
  RegisterUnitComponentProc = procedure(const Page, UnitName: ShortString;
                                       ComponentClass: TComponentClass);

  TBaseComponentPalette = class
  private
    FBaseComponentPageClass: TBaseComponentPageClass;
    FItems: TList; // list of TBaseComponentPage
    FOnBeginUpdate: TNotifyEvent;
    FOnEndUpdate: TEndUpdatePaletteEvent;
    FRegisteredComponentClass: TRegisteredComponentClass;
    FUpdateLock: integer;
    fChanged: boolean;
    function GetItems(Index: integer): TBaseComponentPage;
  protected
    procedure DoChange; virtual;
    procedure DoEndUpdate(Changed: boolean); virtual;
    procedure OnPageAddedComponent(Component: TRegisteredComponent); virtual;
    procedure OnPageRemovedComponent(Page: TBaseComponentPage;
                                Component: TRegisteredComponent); virtual;
    procedure OnComponentVisibleChanged(
                                     AComponent: TRegisteredComponent); virtual;
    procedure Update; virtual;
    procedure SetBaseComponentPageClass(
                                const AValue: TBaseComponentPageClass); virtual;
    procedure SetRegisteredComponentClass(
                              const AValue: TRegisteredComponentClass); virtual;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    procedure ClearButtons; virtual;
    procedure BeginUpdate(Change: boolean);
    procedure EndUpdate;
    function IsUpdateLocked: boolean;
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
    procedure ShowHideControls(Show: boolean);
    procedure IterateRegisteredClasses(Proc: TGetComponentClass);
    procedure RegisterCustomIDEComponents(
                        const RegisterProc: RegisterUnitComponentProc); virtual;
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
  end;
  

var
  IDEComponentPalette: TBaseComponentPalette;

function ComparePriority(const p1,p2: TComponentPriority): integer;
function CompareIDEComponentByClassName(Data1, Data2: pointer): integer;


implementation

procedure RaiseException(const Msg: string);
begin
  raise Exception.Create(Msg);
end;

function ComparePriority(const p1, p2: TComponentPriority): integer;
begin
  Result:=ord(p2.Category)-ord(p1.Category);
  if Result<>0 then exit;
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

procedure TRegisteredComponent.ShowHideControl(Show: boolean);
begin
  if IsTControl then
    Visible:=Show;
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

procedure TBaseComponentPage.ShowHideControls(Show: boolean);
var
  i: Integer;
begin
  for i:=0 to Count-1 do
    Items[i].ShowHideControl(Show);
end;

{ TBaseComponentPalette }

function TBaseComponentPalette.GetItems(Index: integer): TBaseComponentPage;
begin
  Result:=TBaseComponentPage(FItems[Index]);
end;

procedure TBaseComponentPalette.DoChange;
begin
  if FUpdateLock>0 then
    fChanged:=true
  else
    Update;
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

procedure TBaseComponentPalette.Update;
begin

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

procedure TBaseComponentPalette.ShowHideControls(Show: boolean);
var
  i: Integer;
begin
  BeginUpdate(false);
  for i:=0 to Count-1 do
    Pages[i].ShowHideControls(Show);
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


initialization
  IDEComponentPalette:=nil;
  
end.

