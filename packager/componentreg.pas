{  $Id$  }
{
 /***************************************************************************
                            componentreg.pas
                            ----------------


 ***************************************************************************/

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

}
unit ComponentReg;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, IDEProcs;

const
  CompPriorityLCL         =  0;
  CompPriorityBase        = 10;
  CompPriorityRecommended = 20;
  CompPriorityNormal      = 30;
  CompPriorityOptional    = 40;

type
  TIDEComponentPage = class;
  TIDEComponentPalette = class;


  { TIDEComponent }

  TIDEComponent = class
  private
    FComponentClass: TComponentClass;
    FPage: TIDEComponentPage;
    FPageName: string;
  public
    constructor Create(TheComponentClass: TComponentClass;
      const ThePageName: string);
    destructor Destroy; override;
    procedure ConsistencyCheck; virtual;
    function GetUnitName: string; virtual; abstract;
    function GetPriority: integer; virtual;
    procedure AddToPalette; virtual;
  public
    property ComponentClass: TComponentClass read FComponentClass;
    property PageName: string read FPageName;
    property Page: TIDEComponentPage read FPage write FPage;
  end;


  { TIDEComponentPage }

  TIDEComponentPage = class
  private
    FItems: TList; // list of TIDEComponent
    FPageName: string;
    FPalette: TIDEComponentPalette;
    FPriority: integer;
    function GetItems(Index: integer): TIDEComponent;
  public
    constructor Create(const ThePageName: string);
    destructor Destroy; override;
    procedure Clear;
    procedure ConsistencyCheck;
    function Count: integer;
    procedure Add(NewComponent: TIDEComponent);
    procedure Remove(AComponent: TIDEComponent);
    function FindComponent(const CompClassName: string): TIDEComponent;
  public
    property Items[Index: integer]: TIDEComponent read GetItems; default;
    property PageName: string read FPageName;
    property Palette: TIDEComponentPalette read FPalette;
    property Priority: integer read FPriority write FPriority;
  end;


  { TIDEComponentPalette }

  TIDEComponentPalette = class
  private
    FItems: TList; // list of TIDEComponentPage
    function GetItems(Index: integer): TIDEComponentPage;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    procedure ConsistencyCheck;
    function Count: integer;
    function GetPage(const APageName: string;
      CreateIfNotExists: boolean): TIDEComponentPage;
    function IndexOfPageWithName(const APageName: string): integer;
    procedure AddComponent(NewComponent: TIDEComponent);
    function CreateNewPage(const NewPageName: string;
      Priority: integer): TIDEComponentPage;
    function FindComponent(const CompClassName: string): TIDEComponent;
  public
    property Items[Index: integer]: TIDEComponentPage read GetItems; default;
  end;
  
var
  IDEComponentPalette: TIDEComponentPalette;


implementation



{ TIDEComponent }

constructor TIDEComponent.Create(TheComponentClass: TComponentClass;
  const ThePageName: string);
begin
  FComponentClass:=TheComponentClass;
  FPageName:=ThePageName;
end;

destructor TIDEComponent.Destroy;
begin
  if FPage<>nil then FPage.Remove(Self);
  inherited Destroy;
end;

procedure TIDEComponent.ConsistencyCheck;
begin
  if (FComponentClass=nil) then
    RaiseException('TIDEComponent.ConsistencyCheck FComponentClass=nil');
  if not IsValidIdent(FComponentClass.ClassName) then
    RaiseException('TIDEComponent.ConsistencyCheck not IsValidIdent(FComponentClass.ClassName)');
end;

function TIDEComponent.GetPriority: integer;
begin
  Result:=CompPriorityNormal;
end;

procedure TIDEComponent.AddToPalette;
begin
  IDEComponentPalette.AddComponent(Self);
end;

{ TIDEComponentPage }

function TIDEComponentPage.GetItems(Index: integer): TIDEComponent;
begin
  Result:=TIDEComponent(FItems[Index]);
end;

constructor TIDEComponentPage.Create(const ThePageName: string);
begin
  FPageName:=ThePageName;
  FItems:=TList.Create;
end;

destructor TIDEComponentPage.Destroy;
begin
  Clear;
  FItems.Free;
  inherited Destroy;
end;

procedure TIDEComponentPage.Clear;
var
  i: Integer;
begin
  for i:=0 to FItems.Count-1 do
    Items[i].Page:=nil;
  FItems.Clear;
end;

procedure TIDEComponentPage.ConsistencyCheck;
begin

end;

function TIDEComponentPage.Count: integer;
begin
  Result:=FItems.Count;
end;

procedure TIDEComponentPage.Add(NewComponent: TIDEComponent);
var
  InsertIndex: Integer;
  NewPriority: Integer;
begin
  NewPriority:=NewComponent.GetPriority;
  InsertIndex:=0;
  while (InsertIndex<Count) and (NewPriority<Items[InsertIndex].GetPriority) do
    inc(InsertIndex);
  FItems.Insert(InsertIndex,NewComponent);
  NewComponent.Page:=Self;
end;

procedure TIDEComponentPage.Remove(AComponent: TIDEComponent);
begin
  FItems.Remove(AComponent);
  AComponent.Page:=nil;
end;

function TIDEComponentPage.FindComponent(const CompClassName: string
  ): TIDEComponent;
var
  i: Integer;
begin
  for i:=0 to Count-1 do begin
    Result:=Items[i];
    if AnsiCompareText(Result.ComponentClass.ClassName,CompClassName)=0 then
      exit;
  end;
  Result:=nil;
end;

{ TIDEComponentPalette }

function TIDEComponentPalette.GetItems(Index: integer): TIDEComponentPage;
begin
  Result:=TIDEComponentPage(FItems[Index]);
end;

constructor TIDEComponentPalette.Create;
begin
  FItems:=TList.Create;
end;

destructor TIDEComponentPalette.Destroy;
begin
  Clear;
  FItems.Free;
  inherited Destroy;
end;

procedure TIDEComponentPalette.Clear;
var
  i: Integer;
begin
  for i:=0 to FItems.Count-1 do
    Items[i].Free;
  FItems.Clear;
end;

procedure TIDEComponentPalette.ConsistencyCheck;
begin

end;

function TIDEComponentPalette.Count: integer;
begin
  Result:=FItems.Count;
end;

function TIDEComponentPalette.GetPage(const APageName: string;
  CreateIfNotExists: boolean): TIDEComponentPage;
var
  i: Integer;
begin
  i:=IndexOfPageWithName(APageName);
  if i>=0 then begin
    Result:=Items[i];
  end else begin
    Result:=TIDEComponentPage.Create(APageName);
    Result.FPalette:=Self;
    FItems.Add(Result);
  end;
end;

function TIDEComponentPalette.IndexOfPageWithName(const APageName: string
  ): integer;
begin
  Result:=Count-1;
  while (Result>=0) and (AnsiCompareText(Items[Result].PageName,APageName)<>0)
  do
    dec(Result);
end;

procedure TIDEComponentPalette.AddComponent(NewComponent: TIDEComponent);
var
  CurPage: TIDEComponentPage;
begin
  CurPage:=GetPage(NewComponent.PageName,false);
  if CurPage=nil then
    CurPage:=CreateNewPage(NewComponent.PageName,NewComponent.GetPriority);
  CurPage.Add(NewComponent);
end;

function TIDEComponentPalette.CreateNewPage(const NewPageName: string;
  Priority: integer): TIDEComponentPage;
var
  InsertIndex: Integer;
begin
  Result:=TIDEComponentPage.Create(NewPageName);
  InsertIndex:=0;
  while (InsertIndex<Count) and (Items[InsertIndex].Priority<Priority) do
    inc(InsertIndex);
  FItems.Insert(InsertIndex,Result);
  Result.FPalette:=Self;
end;

function TIDEComponentPalette.FindComponent(const CompClassName: string
  ): TIDEComponent;
var
  i: Integer;
begin
  for i:=0 to Count-1 do begin
    Result:=Items[i].FindComponent(CompClassName);
    if Result<>nil then exit;
  end;
  Result:=nil;
end;


initialization
  IDEComponentPalette:=nil;

end.

