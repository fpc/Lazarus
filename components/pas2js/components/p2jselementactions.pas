
unit p2jselementactions;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, stub.htmlactions;

Type

  { TRegisteredHTMLAction }

  TRegisteredHTMLAction = class(TCollectionItem)
  private
    FActionClass: THTMLCustomElementActionClass;
    FDescription: String;
  public
    Property ActionClass: THTMLCustomElementActionClass read FActionClass;
    Property Description : String Read FDescription Write FDescription;
  end;

  { TRegisteredHTMLActionList }

  TRegisteredHTMLActionList = Class(TCollection)
  private
    function GetAction(aIndex : Integer): TRegisteredHTMLAction;
  Public
    Function IndexOfClass (aClass : THTMLCustomElementActionClass) : Integer;
    Function IndexOfClassName (const aClassName : String) : Integer;
    Function FindByClassName (const aClassName : String) : TRegisteredHTMLAction;
    Function AddAction (aClass : THTMLCustomElementActionClass; const aDescription : String) : TRegisteredHTMLAction;
    Property Actions[aIndex : Integer] : TRegisteredHTMLAction Read GetAction; default;
  end;

  { TPas2JSActionRegistry }

  TPas2JSActionRegistry = Class
  Private
    class var _Instance : TPas2JSActionRegistry;
    function GetAction(aIndex : Integer): TRegisteredHTMLAction;
    function GetActionCount: Integer;
  Private
    FList : TRegisteredHTMLActionList;
  public
    Class Constructor Init;
    Class destructor Done;
    Class property Instance : TPas2JSActionRegistry Read _Instance;
  Public
    Constructor Create; virtual;
    Destructor Destroy; override;
    Function RegisterAction(aClass : THTMLCustomElementActionClass; const aDescription : String) : TRegisteredHTMLAction;
    Procedure UnRegisterAction(aClass : THTMLCustomElementActionClass);
    Function IndexOfActionClassName(const aName : String) : Integer;
    Function FindActionByClassName(const aName : String) : TRegisteredHTMLAction;
    Property ActionCount : Integer Read GetActionCount;
    Property Actions[aIndex : Integer] : TRegisteredHTMLAction Read GetAction; default;
  end;

Function Pas2JSActionRegistry : TPas2JSActionRegistry;
Function RegisterPas2JSAction(aClass : THTMLCustomElementActionClass; const aDescription : String) : TRegisteredHTMLAction;

implementation

function Pas2JSActionRegistry: TPas2JSActionRegistry;
begin
  Result:=TPas2JSActionRegistry.Instance;
end;

function RegisterPas2JSAction(aClass: THTMLCustomElementActionClass;
  const aDescription: String): TRegisteredHTMLAction;
begin
  Result:=TPas2JSActionRegistry.Instance.RegisterAction(aClass,aDescription);
end;

{ TPas2JSActionRegistry }

function TPas2JSActionRegistry.GetAction(aIndex : Integer
  ): TRegisteredHTMLAction;
begin
  Result:=FList[aIndex];
end;

function TPas2JSActionRegistry.GetActionCount: Integer;
begin
  Result:=FList.Count;
end;

class constructor TPas2JSActionRegistry.Init;
begin
  _instance:=TPas2JSActionRegistry.Create;
end;

class destructor TPas2JSActionRegistry.Done;
begin
  FreeAndNil(_instance);
end;

constructor TPas2JSActionRegistry.Create;
begin
  FList:=TRegisteredHTMLActionList.Create(TRegisteredHTMLAction);
end;

destructor TPas2JSActionRegistry.Destroy;
begin
  FreeAndNil(FList);
  inherited Destroy;
end;

function TPas2JSActionRegistry.RegisterAction(
  aClass: THTMLCustomElementActionClass; const aDescription: String
  ): TRegisteredHTMLAction;
begin
  Result:=FList.AddAction(aClass,aDescription);
end;

procedure TPas2JSActionRegistry.UnRegisterAction(
  aClass: THTMLCustomElementActionClass);

Var
  Idx : Integer;

begin
  Idx:=FList.IndexOfClass(aClass);
  if Idx<>-1 then
    FList.Delete(Idx);
end;

function TPas2JSActionRegistry.IndexOfActionClassName(const aName: String
  ): Integer;
begin
  Result:=FList.IndexOfClassName(aName);
end;

function TPas2JSActionRegistry.FindActionByClassName(const aName: String
  ): TRegisteredHTMLAction;
begin
  Result:=FList.FindByClassName(aName);
end;

{ TRegisteredHTMLActionList }

function TRegisteredHTMLActionList.GetAction(aIndex : Integer
  ): TRegisteredHTMLAction;
begin
  Result:=TRegisteredHTMLAction(Items[aIndex]);
end;

function TRegisteredHTMLActionList.IndexOfClass(aClass: THTMLCustomElementActionClass): Integer;
begin
  Result:=Count-1;
  While (Result>=0) and (GetAction(Result).ActionClass<>aClass) do
    Dec(Result);
end;

function TRegisteredHTMLActionList.IndexOfClassName(const aClassName: String
  ): Integer;
begin
  Result:=Count-1;
  While (Result>=0) and Not SameText(GetAction(Result).ActionClass.ClassName,aClassName) do
    Dec(Result);
end;

function TRegisteredHTMLActionList.FindByClassName(const aClassName: String
  ): TRegisteredHTMLAction;

Var
  Idx : Integer;

begin
  Idx:=IndexOfClassName(aClassName);
  if Idx=-1 then
    Result:=Nil
  else
    Result:=GetAction(Idx);
end;

function TRegisteredHTMLActionList.AddAction(aClass: THTMLCustomElementActionClass;
  const aDescription: String): TRegisteredHTMLAction;
begin
  Result:=Add as TRegisteredHTMLAction;
  Result.FActionClass:=aClass;
  Result.Description:=aDescription;
end;

end.

