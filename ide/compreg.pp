unit CompReg;
{
  Author: Mattias Gaertner

  Abstract:
    All components that should be usable to the IDE must register themselves.
    There is a list of all components. And every component can also be found
    in the list of its component page.

  ToDo:
    see XXX
}
{$MODE OBJFPC}

interface

uses Classes, SysUtils;

type
  TRegisteredComponentPage = class;
  TRegisteredComponentList = class;

  TRegisteredComponent = class
  // describes a single component
  private
    FPage:TRegisteredComponentPage;
    FComponentClass:TComponentClass;
    FIndexInPage:integer;
    FUnitName:ShortString;
  public
    property Page:TRegisteredComponentPage read FPage;
    property ComponentClass:TComponentClass read FComponentClass;
    property IndexInPage:integer read FIndexInPage;
    property UnitName:ShortString;
    constructor Create(APage:TRegisteredComponentPage; TheIndexInPage:integer;
      AUnitName:ShortString;  AComponentClass:TComponentClass);
  end;

  TRegisteredComponentPage = class
  // describes the components in a single component page
  private
    FPageIndex:integer;
    FName:ShortString;
    FItems:TList;
    FCompList:TRegisteredComponentList;
    function GetItem(Index:integer):TRegisteredComponent;
    Function GetCount : Integer;
  public
    property Items[Index:integer]:TRegisteredComponent read GetItem;  default;
    property Count:integer read GetCount;
    property PageIndex:integer read FPageIndex;
    property Name:ShortString read FName;
    property CompList:TRegisteredComponentList read FCompList;
    constructor Create(ACompList:TRegisteredComponentList; APageIndex:integer;
      PageName:ShortString);
    destructor Destroy;  override;
  end;

  TRegisteredComponentList = class
  // a list of all registered components and all component pages
  private
    FItems:TList;
    FPages:TList;
    function GetItem(Index:integer):TRegisteredComponent;
    function GetPage(Index:integer):TRegisteredComponentPage;
  public
    procedure RegisterComponents(const Page:ShortString;  UnitName:ShortString;
      ComponentClasses: array of TComponentClass);
    property Items[Index:integer]:TRegisteredComponent read GetItem;  default;
    function Count:integer;
    function FindComponentClassByName(Name:ShortString):TRegisteredComponent;
    property Pages[Index:integer]:TRegisteredComponentPage read GetPage;
    function PageCount:integer;
    function FindPageByName(Name:ShortString):TRegisteredComponentPage;
    procedure Clear;
    constructor Create;
    destructor Destroy;  override;
  end;


const
  RegisterComponentsProc: procedure(const Page,UnitName:ShortString;
    ComponentClasses: array of TComponentClass) = nil;

procedure RegisterComponents(const Page,UnitName:ShortString;
  ComponentClasses: array of TComponentClass);


implementation


procedure RegisterComponents(const Page,UnitName:ShortString;
  ComponentClasses: array of TComponentClass);
begin
  if Assigned(RegisterComponentsProc) then
    RegisterComponentsProc(Page, UnitName, ComponentClasses)
  else begin
    // XXX ToDo:
    {raise EComponentError.CreateRes(@SRegisterError);}
    writeln('[RegisterComponents] Error: RegisterComponentsProc not assigned.');
    halt;
  end;
end;


{ TRegisteredComponent }

constructor TRegisteredComponent.Create(APage:TRegisteredComponentPage;
  TheIndexInPage:integer;  AUnitName:ShortString;
  AComponentClass:TComponentClass);
begin
  FPage:=APage;
  FIndexInPage:=TheIndexInPage;
  FComponentClass:=AComponentClass;
  FUnitName:=AUnitName;
end;

{ TRegisteredComponentPage }

constructor TRegisteredComponentPage.Create(ACompList:TRegisteredComponentList;
  APageIndex:integer;  PageName:ShortString);
begin
  FName:=PageName;
  FCompList:=ACompList;
  FPageIndex:=APageIndex;
  FItems:=TList.Create;
end;

destructor TRegisteredComponentPage.Destroy;
begin
  FItems.Free;
end;

function TRegisteredComponentPage.GetItem(Index:integer):TRegisteredComponent;
begin
  Result:=TRegisteredComponent(FItems[Index]);
end;

function TRegisteredComponentPage.GetCount:Integer;
begin
  Result:= FItems.Count;
end;


{ TRegisteredComponentList }

constructor TRegisteredComponentList.Create;
begin
  FItems:=TList.Create;
  FPages:=TList.Create;
end;

destructor TRegisteredComponentList.Destroy;
begin
  Clear;
  FPages.Free;
  FItems.Free;
end;

procedure TRegisteredComponentList.Clear;
var a:integer;
begin
  for a:=0 to FItems.Count-1 do
    Items[a].Free;
  FItems.Clear;
  for a:=0 to FPages.Count-1 do
    Pages[a].Free;
  FPages.Clear;
end;

function TRegisteredComponentList.GetItem(Index:integer):TRegisteredComponent;
begin
  Result:=TRegisteredComponent(FItems[Index]);
end;

function TRegisteredComponentList.Count:integer;
begin
  Result:=FItems.Count;
end;

function TRegisteredComponentList.FindComponentClassByName(
  Name:ShortString):TRegisteredComponent;
var a:integer;
begin
  Name:=uppercase(Name);
  for a:=0 to FItems.Count-1 do begin
    if uppercase(Items[a].FComponentClass.ClassName)=Name then begin
      Result:=Items[a];
      exit;
    end;
  end;
  Result:=nil;
end;

procedure TRegisteredComponentList.RegisterComponents(const Page:ShortString;
  UnitName:ShortString;  ComponentClasses: array of TComponentClass);
var NewPage:TRegisteredComponentPage;
  a:integer;
  NewComp:TRegisteredComponent;
begin

  NewPage:=FindPageByName(Page);
  if (NewPage=nil) then begin
    NewPage:=TRegisteredComponentPage.Create(Self,FPages.Count,Page);
    FPages.Add(NewPage);
  end;
  for a:=Low(ComponentClasses) to High(ComponentClasses) do begin
    NewComp:=TRegisteredComponent.Create(NewPage,NewPage.Count,UnitName,
      ComponentClasses[a]);
    FItems.Add(NewComp);
    NewPage.FItems.Add(NewComp);
  end;
end;

function TRegisteredComponentList.PageCount:integer;
begin
  Result:=FPages.Count;
end;

function TRegisteredComponentList.GetPage(Index:integer):TRegisteredComponentPage;
begin
  Result:=TRegisteredComponentPage(FPages[Index]);
end;

function TRegisteredComponentList.FindPageByName(
  Name:ShortString):TRegisteredComponentPage;
var a:integer;
begin
  Name:=uppercase(Name);
  for a:=0 to FPages.Count-1 do begin
    if uppercase(Pages[a].Name)=Name then begin
      Result:=Pages[a];
      exit;
    end;
  end;
  Result:=nil;
end;



end.

