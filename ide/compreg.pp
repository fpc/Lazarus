{
 ***************************************************************************
 *                                                                         *
 *   This source is free software; you can redistribute it and/or modify   *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 *   This code is distributed in the hope that it will be useful, but      *
 *   WITHOUT ANY WARRANTY; without even the implied warranty of            *
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU     *
 *   General Public License for more details.                              *
 *                                                                         *
 *   A copy of the GNU General Public License is available on the World    *
 *   Wide Web at <http://www.gnu.org/copyleft/gpl.html>. You can also      *
 *   obtain it by writing to the Free Software Foundation,                 *
 *   Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.        *
 *                                                                         *
 ***************************************************************************
 
  Author: Mattias Gaertner

  Abstract:
    All components that should be usable to the IDE must register themselves.
    There is a list of all components. And every component can also be found
    in the list of its component page.

  ToDo:
    see XXX
}
unit CompReg;

{$MODE OBJFPC}{$H+}

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
    property UnitName:ShortString read FUnitName;
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


type
  TRegisterComponentsProc = procedure(const Page,UnitName:ShortString;
    ComponentClasses: array of TComponentClass);

var
  RegisterComponentsProc: TRegisterComponentsProc;

procedure RegisterComponent(const Page,UnitName:ShortString;
  ComponentClass: TComponentClass);
procedure RegisterComponents(const Page,UnitName:ShortString;
  ComponentClasses: array of TComponentClass);
function FindRegsiteredComponentClass(
  const AClassName: string): TRegisteredComponent;

var
  RegCompList:TRegisteredComponentList;
  

implementation


procedure RegisterComponent(const Page, UnitName: ShortString;
  ComponentClass: TComponentClass);
begin
  if RegCompList<>nil then
    RegCompList.RegisterComponents(Page,UnitName,[ComponentClass])
  else
    raise EComponentError.Create(
      'RegisterComponentsInGlobalList RegCompList=nil');
end;

procedure RegisterComponents(const Page,UnitName:ShortString;
  ComponentClasses: array of TComponentClass);
begin
  if Assigned(RegisterComponentsProc) then
    RegisterComponentsProc(Page, UnitName, ComponentClasses)
  else begin
    raise EComponentError.Create(
      '[RegisterComponents] Error: RegisterComponentsProc not assigned.'
      {SRegisterError});
  end;
end;

function FindRegsiteredComponentClass(
  const AClassName: string): TRegisteredComponent;
begin
  Result:=nil;
  if RegCompList<>nil then begin
    Result:=RegCompList.FindComponentClassByName(AClassName);
  end;
end;

procedure RegisterComponentsInGlobalList(const Page,UnitName:ShortString;
  ComponentClasses: array of TComponentClass);
begin
  if RegCompList<>nil then
    RegCompList.RegisterComponents(Page,UnitName,ComponentClasses)
  else
    raise EComponentError.Create(
      'RegisterComponentsInGlobalList RegCompList=nil');
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
  // the hidden page is the empty ''
  if (High(ComponentClasses)-Low(ComponentClasses)<0)
  or (UnitName='') then exit;
  if not IsValidIdent(UnitName) then begin
    raise EComponentError.Create(
      'RegisterComponents: Invalid unitname "'+UnitName+'"');
  end;
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

initialization
  RegisterComponentsProc:=nil;
  RegCompList := TRegisteredComponentList.Create;
  RegisterComponentsProc := @RegisterComponentsInGlobalList;

finalization
  RegCompList.Free;
  RegCompList:=nil;

end.

