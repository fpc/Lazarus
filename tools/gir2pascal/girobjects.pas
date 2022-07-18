{
girobjects.pas
Copyright (C) 2011  Andrew Haines andrewd207@aol.com

This program is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License
as published by the Free Software Foundation; either version 2
of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.
}
unit girObjects;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DOM, girTokens;

type

  TGirModeState = (msNone, msWriting, msWritten);

  TGirObjectType = (otBaseType, otAlias, otArray, otBitfield, otCallback, otClass,
                    otClassStruct, otConstant, otConstructor, otEnumeration, otFunction,
                    otFunctionReturn, otFunctionParam, otFuzzyType, otGlibSignal,
                    otGType, otInterface, otMethod, otNativeType, otObject, otProperty,
                    otRecord, otTypeParam, otUnion, otVirtualMethod);


  { TGirBaseType }

  TGirBaseType = class
  private
    FBits: Integer;
    FCType: String;
    FDeprecated: Boolean;
    FDeprecatedMsg: String;
    FDeprecatedOverride: Boolean;
    FDeprecatedVersion: TGirVersion;
    FDoc: String;
    FForwardDefinitionWritten: Boolean;
    FGLibGetType: String;
    FHasFields: Boolean;
    FImpliedPointerLevel: Integer;
    FName: String;
    FObjectType: TGirObjectType;
    FDisguised: Boolean;
    FOwner: TObject;
    FTranslatedName: String;
    FVersion: TGirVersion;
    FWriting: TGirModeState;
    procedure SetImpliedPointerLevel(AValue: Integer);
    function MaybeResolvedType: TGirBaseType;
    function GetPointerLevelFromCType(ACType: String = ''): Integer;
  public
    constructor Create(AOwner: TObject; ANode: TDomNode); virtual;
    property CType: String read FCType write FCType;
    property GLibGetType: String read FGLibGetType;
    property Name: String read FName;
    property TranslatedName: String read FTranslatedName write FTranslatedName;
    property ImpliedPointerLevel: Integer read FImpliedPointerLevel write SetImpliedPointerLevel; // only grows
    property Owner: TObject Read FOwner; // TgirNameSpace
    property Doc: String read FDoc;
    property Bits: Integer read FBits;
    property Version: TGirVersion read FVersion;
    property ForwardDefinitionWritten: Boolean read FForwardDefinitionWritten write FForwardDefinitionWritten;
    property Writing: TGirModeState read FWriting write FWriting;
    property Disguised: Boolean read FDisguised; // only access this type with the functions given not fieids directly.
    property ObjectType: TGirObjectType read FObjectType;
    property Deprecated: Boolean read FDeprecated;
    property DeprecatedMsg: String read FDeprecatedMsg;
    property DeprecatedVersion: TGirVersion read FDeprecatedVersion;
    { if an object that is not deprecated depends on this deprecated item then override it. }
    property DeprecatedOverride: Boolean read FDeprecatedOverride write FDeprecatedOverride;
  end;

  { TgirNativeTypeDef }

  TgirNativeTypeDef = class(TGirBaseType)
  private
    FPascalName: String;
  public
    constructor Create(AOwner:TObject; AGType: String; APascalCTypeName: String);
    property PascalName: String read FPascalName write FPascalName;
  end;

  { TgirFuzzyType }

  TgirFuzzyType = class(TGirBaseType)
  private
    FResolvedType: TGirBaseType;
    function GetResolvedType: TGirBaseType;
    procedure SetResolvedType(const AValue: TGirBaseType);
  public
    constructor Create(AOwner: TObject; AName: String; ACtype: String);
    property ResolvedType: TGirBaseType read GetResolvedType write SetResolvedType;
  end;

  { TgirAlias }

  TgirAlias = class(TGirBaseType)
  private
    FIsOpaque: Boolean;
    FForType: TGirBaseType;
    function GetForType: TGirBaseType;
  public
    constructor Create(AOwner: TObject; ANode: TDomNode); override;
    constructor Create(AOwner: TObject; AName, ACType, ATranslatedName: String);
    property IsOpaque: Boolean read FIsOpaque;
    property ForType: TGirBaseType read GetForType write FForType;
  end;

  { TgirTypeParam }

  TgirTypeParam = class(TGirBaseType)
  private
    FIsConst: Boolean;
    FIsInstanceParam: Boolean;
    FVarType: TGirBaseType;
    FPointerLevel: Integer;
    function GetPointerLevel: Integer;
    function GetType: TGirBaseType;
  public
    constructor Create(AOwner: TObject; ANode: TDomNode); override;
    constructor Create(AOwner: TObject; ANode: TDomNode; AIsInstanceParam: Boolean); virtual;
    property VarType: TGirBaseType read GetType;
    property PointerLevel: Integer read GetPointerLevel;
    property IsInstanceParam: Boolean read FIsInstanceParam;
    property IsConst: Boolean read FIsConst;
  end;

  { TgirProperty }

  TgirProperty = class(TgirBaseType)
  private
    FIsArray: Boolean;
    FPropType: TgirBaseType;
    FWriteable: Boolean;
    function GetPropType: TgirBaseType;
  public
    constructor Create(AOwner: TObject; ANode: TDomNode); override;
    property PropType: TgirBaseType read GetPropType;
    property Writable: Boolean read FWriteable;
    property IsArray: Boolean read FIsArray;
  end;

  { TgirArray }

  TgirArray = class(TgirTypeParam)
  private
    FFixedSize: Integer;
    FParentFieldName: String;
    FNode: TDOMNode;
    function GetBestPointerLevel: Integer; // only works while the Constructor is active.
  public
    constructor Create(AOwner: TObject; ANode: TDomNode); override;
    property FixedSize: Integer read FFixedSize;
    property ParentFieldName: String read FParentFieldName;
  end;

  { TgirConstant }

  TgirConstant = class(TGirBaseType)
  private
    FCName: String;
    FIsString: Boolean;
    FTypeDecl: TGirBaseType;
    FValue: String;
  public
    constructor Create(AOwner: TObject; ANode: TDomNode); override;
    property TypeDecl: TGirBaseType read FTypeDecl;
    property Value: String read FValue;
    property IsString: Boolean read FIsString;
    property CName: String read FCName;
  end;

  { TgirEnumeration }
  PgirEnumMember = ^TgirEnumMember;
  TgirEnumMember = record
    Name: String;
    Value: String;
    CIdentifier: String;
  end;

  { TgirEnumList }

  TgirEnumList = class(TList)
  private
    function GetMember(AIndex: Integer): PgirEnumMember;
  public
    procedure Delete(Index: Integer);
    property Member[AIndex: Integer]: PgirEnumMember read GetMember;
  end;


  TgirEnumeration = class(TGirBaseType)
  private
    FMembers: TgirEnumList;
    FNeedsSignedType: Boolean;
    FNotIntTypeEnum: Boolean;
    procedure AddMember(AName, AValue, ACIdentifier: String; Node: TDomElement);
    procedure HandleFunction(ANode: TDomNode);
  public
    constructor Create(AOwner: TObject; ANode: TDomNode); override;
    destructor Destroy; override;
    property Members: TgirEnumList read FMembers;
    property NeedsSignedType: Boolean read FNeedsSignedType;
  end;

  { TgirBitField }

  TgirBitField = class(TgirEnumeration)
    constructor Create(AOwner: TObject; ANode: TDomNode); override;

  end;

  { TGirFunctionParam }

  TGirFunctionParam = class(TgirTypeParam)
  public
    constructor Create(AOwner: TObject; ANode: TDomNode); override;
    constructor Create(AOwner: TObject; ANode: TDomNode; AIsInstanceParam: Boolean); override;
  end;

  { TgirFunctionReturn }

  TgirFunctionReturn = class(TgirTypeParam)
    constructor Create(AOwner: TObject; ANode: TDomNode); override;
  end;

  { TgirParamList }

  TgirParamList= class (TList)
  private
    function GetParam(AIndex: Integer): TGirFunctionParam;
  public
     property Param[AIndex: Integer]: TGirFunctionParam read GetParam;
  end;

  { TgirFunction }

  TgirFunction = class(TGirBaseType)
  private
    FCIdentifier: String;
    FParams: TgirParamList;
    FReturns: TgirFunctionReturn;
    FThrowsGError: Boolean;
  publiC
    constructor Create(AOwner: TObject; ANode: TDomNode); override;
    destructor Destroy; override;
    property Params: TgirParamList read FParams;
    property Returns: TgirFunctionReturn read FReturns;
    property CIdentifier: String read FCIdentifier;
    property ThrowsGError: Boolean read FThrowsGError write FThrowsGError;
  end;

  { TgirMethod }

  TgirMethod = class(TgirFunction)
    constructor Create(AOwner: TObject; ANode:TDOMNode); override;
  end;

  { TgirGlibSignal }

  TgirGlibSignal = class(TgirFunction)
    constructor Create(AOwner: TObject; ANode:TDOMNode); override;

  end;

  { TgirVirtualMethod }

  TgirVirtualMethod = class(TgirMethod)
    constructor Create(AOwner: TObject; ANode:TDOMNode); override;
  end;

  { TgirCallback }

  TgirCallback = class(TgirFunction)
    constructor Create(AOwner: TObject; ANode: TDomNode); override;
    //property OwnsResult: Boolean;
  end;

  { TgirConstructor }

  TgirConstructor = class(TgirFunction)
    constructor Create(AOwner: TObject; ANode:TDOMNode); override;
  end;

  { TgirFieldsList }

  TgirFieldsList = class(TList)
  private
    FHasFields: Boolean;
    function GetField(AIndex: Integer): TGirBaseType;
  protected
    procedure Notify(Ptr: Pointer; Action: TListNotification); override;
  public
    property Field[AIndex: Integer]: TGirBaseType read GetField;
  end;

  { tgirRecord }

  TgirRecord = class(TGirBaseType)
  private
    FFields: TgirFieldsList;
    function GetHasFields: Boolean;
  protected
    procedure HandleUnion(ANode: TDomNode);
    procedure HandleField(ANode: TDomNode);
    procedure ParseNode(ANode: TDomNode; ANodeType: TGirToken); virtual;
  public
    constructor Create(AOwner: TObject; ANode: TDomNode); override;
    destructor Destroy; override;
    property Fields: TgirFieldsList read FFields;
    property HasFields: Boolean read GetHasFields;
  end;

  { TgirUnion }

  TgirUnion = class(TgirRecord)
    constructor Create(AOwner: TObject; ANode: TDomNode); override;
    procedure ParseNode(ANode: TDomNode; ANodeType: TGirToken); override;
  end;


  { TgirObject }

  TgirObject = class(TgirRecord)
  protected
    procedure ParseNode(ANode: TDomNode; ANodeType: TGirToken); override;
  public
    constructor Create(AOwner: TObject; ANode: TDomNode); override;
  end;

  { TgirGType }

  TgirGType = class(TgirObject)
  private
    FGetTypeFunction: String;
  protected
    procedure ParseNode(ANode: TDomNode; ANodeType: TGirToken); override;
  public
    constructor Create(AOwner: TObject; ANode: TDomNode); override;
    property GetTypeFunction: String read FGetTypeFunction;
  end;

  { TgirClass }

  { TgirClassStruct }
  TgirClass = class;

  TgirClassStruct = class(TgirGType)
  private
    FIsClassStructFor: TgirClass;
    function GetIsClassStructFor: TgirClass;
  public
    constructor Create(AOwner: TObject; ANode: TDomNode); override;
    property IsClassStructFor: TgirClass read GetIsClassStructFor;

  end;

  TgirClass = class(TgirGType)
  private
    FClassStruct: TgirClassStruct;
    FInterfaces: TList;
    FParentClass: TgirClass;
    function GetParentClass: TgirClass;
  protected
    procedure ParseNode(ANode: TDomNode; ANodeType: TGirToken); override;
    procedure AddInterface(ANode: TDomNode);
  public
    constructor Create(AOwner: TObject; ANode: TDomNode); override;
    destructor Destroy; override;
    property Interfaces: TList read FInterfaces;
    property ParentClass: TgirClass read GetParentClass;
    property ClassStruct: TgirClassStruct read FClassStruct;

  end;

  { TgirInterface }

  TgirInterface = class(TgirClass)
  public
    constructor Create(AOwner: TObject; ANode: TDomNode); override;
    procedure ParseNode(ANode: TDomNode; ANodeType: TGirToken); override;
  end;






implementation
uses girNameSpaces, girErrors, XMLRead;

{ TgirClassStruct }

function TgirClassStruct.GetIsClassStructFor: TgirClass;
begin
  Result := nil;
  if FIsClassStructFor.ObjectType = otFuzzyType then
  begin
    if TgirFuzzyType(FIsClassStructFor).ResolvedType <> nil then
      FIsClassStructFor := TgirClass(TgirFuzzyType(FIsClassStructFor).ResolvedType);
  end;
  Result := FIsClassStructFor;
end;

constructor TgirClassStruct.Create(AOwner: TObject; ANode: TDomNode);
var
  IsClassForName: String;
begin
  inherited Create(AOwner, ANode);
  FObjectType:=otClassStruct;
  IsClassForName:= (ANode as TDOMElement).GetAttribute('glib:is-gtype-struct-for');
  if IsClassForName <> '' then
  begin
    FIsClassStructFor := TgirClass((Owner as TgirNamespace).LookupTypeByName(IsClassForName, '', True));
    if (FIsClassStructFor <> nil) and (FIsClassStructFor.ObjectType = otClass) then
      FIsClassStructFor.FClassStruct := Self;
  end;
end;

{ TgirConstructor }

constructor TgirConstructor.Create(AOwner: TObject; ANode: TDOMNode);
begin
  inherited Create(AOwner, ANode);
  FObjectType:=otConstructor;
end;

{ TgirCallback }

constructor TgirCallback.Create(AOwner: TObject; ANode: TDomNode);
begin
  inherited Create(AOwner, ANode);
  FObjectType:=otCallback;
end;

{ TgirVirtualMethod }

constructor TgirVirtualMethod.Create(AOwner: TObject; ANode: TDOMNode);
begin
  inherited Create(AOwner, ANode);
  FObjectType:=otVirtualMethod;
end;

{ TgirGlibSignal }

constructor TgirGlibSignal.Create(AOwner: TObject; ANode: TDOMNode);
begin
  inherited Create(AOwner, ANode);
  FObjectType:=otGlibSignal;
end;

{ TgirMethod }

constructor TgirMethod.Create(AOwner: TObject; ANode: TDOMNode);
begin
  inherited Create(AOwner, ANode);
  FObjectType:=otMethod;
end;

{ TgirBitField }

constructor TgirBitField.Create(AOwner: TObject; ANode: TDomNode);
begin
  inherited Create(AOwner, ANode);
  FObjectType:=otBitfield;
  FHasFields:=True;
end;

{ TgirFieldsList }

function TgirFieldsList.GetField(AIndex: Integer): TGirBaseType;
begin
  Result := TGirBaseType(Items[AIndex]);
end;

procedure TgirFieldsList.Notify(Ptr: Pointer; Action: TListNotification);
var
  gir: TGirBaseType absolute Ptr;
begin
  if FHasFields then
    Exit;
  FHasFields:= gir.ObjectType in [otTypeParam, otCallback, otArray];
end;

{ TgirParamList }

function TgirParamList.GetParam(AIndex: Integer): TGirFunctionParam;
begin
  Result := TGirFunctionParam(Items[AIndex]);
end;

constructor TgirNativeTypeDef.Create(AOwner:TObject; AGType: String; APascalCTypeName: String);
begin
  FOwner := AOwner;
  FCType:=AGType;
  FName:=AGType; // used by LookupName in namespace
  FVersion := TgirNamespace(FOwner).Version.AsMajor;
  FDeprecatedVersion := girVersion(MaxInt, MaxInt);

  //now some fixups :(
  if FName = 'gchar' then
    FName := 'utf8';
  FTranslatedName:=AGType;
  FPascalName:=APascalCTypeName;
  //to create PPType
  FImpliedPointerLevel:=2;
  FObjectType:=otNativeType;
end;

{ TgirNativeTypeDef }


{ TgirInterface }

constructor TgirInterface.Create(AOwner: TObject; ANode: TDomNode);
begin
  inherited Create(AOwner, ANode);
  FObjectType:=otInterface;
end;

procedure TgirInterface.ParseNode(ANode: TDomNode; ANodeType: TGirToken);
begin
  case ANodeType of
    gtPrerequisite:; // ignore for now since these are bindings.
                     //The interface expects the implementing object to be a descendant of the prerequisite
  else
    inherited ParseNode(ANode, ANodeType);

  end;
end;

{ TgirProperty }

function TgirProperty.GetPropType: TgirBaseType;
begin
  Result := FPropType;
  if Assigned(Result) and Result.InheritsFrom(TgirFuzzyType) and (TgirFuzzyType(Result).ResolvedType <> nil) then
    Result := TgirFuzzyType(Result).ResolvedType;
end;

constructor TgirProperty.Create(AOwner: TObject; ANode: TDomNode);
var
  Node: TDOMElement;
begin
  inherited Create(AOwner, ANode);
  FWriteable := (TDOMElement(ANode).GetAttribute('writable') = '1');
  Node := TDomElement(ANode.FirstChild);
  while Node <> nil do
  begin
    case GirTokenNameToToken(Node.NodeName) of
      gtDoc,
      gtDocDeprecated:; // ignore
      gtType: FPropType := TgirNamespace(Owner).LookupTypeByName(Node.GetAttribute('name'), Node.GetAttribute('c:type'));
      gtArray:
      begin
        FIsArray:=True;
        FPropType := TgirNamespace(Owner).LookupTypeByName(TDomElement(Node.FindNode('type')).GetAttribute('name'), Node.GetAttribute('c:type'));
      end
      else
        WriteLn('Unknown Node Type for property : ', Node.NodeName);
        halt(1);
    end;
    Node := TDOMElement(Node.NextSibling);
  end;
  FObjectType:=otProperty;
end;

{ TgirClass }

function TgirClass.GetParentClass: TgirClass;
begin
  Result := FParentClass;
  if (FParentClass <> nil) and (FParentClass.ObjectType = otFuzzyType) and (TgirFuzzyType(FParentClass).ResolvedType <> nil) then
     FParentClass := TgirClass(TgirFuzzyType(FParentClass).ResolvedType);
  Result := FParentClass;
end;

procedure TgirClass.ParseNode(ANode: TDomNode; ANodeType: TGirToken);
begin
  case ANodeType of
    gtProperty: Fields.Add(TgirProperty.Create(Owner, ANode));
    gtVirtualMethod: Fields.Add(TgirVirtualMethod.Create(Owner, ANode));
    gtGlibSignal: Fields.Add(TgirGlibSignal.Create(Owner, ANode));
    gtImplements: AddInterface(ANode)
  else
    inherited ParseNode(ANode, ANodeType);
  end;
end;

procedure TgirClass.AddInterface(ANode: TDomNode);
var
  Intf: TGirBaseType;
  Node: TDOMElement absolute ANode;
begin
  Intf := TgirNamespace(Owner).LookupTypeByName(Node.GetAttribute('name'), '');
  FInterfaces.Add(Intf);
end;

constructor TgirClass.Create(AOwner: TObject; ANode: TDomNode);
var
  Parent: String;
begin
  FInterfaces := TList.Create; // must be before inherited else list does not exist when ParseNeode is called
  inherited Create(AOwner, ANode);
  Parent := TDOMElement(ANode).GetAttribute('parent');
  if Parent = '' then
    FParentClass := nil
  else
    FParentClass := TgirClass(TgirNamespace(Owner).LookupTypeByName(Parent, '', True));

  if CType = '' then
     CType := TDOMElement(ANode).GetAttribute('glib:type-name');
  FObjectType:=otClass;
end;

destructor TgirClass.Destroy;
begin
  FInterfaces.Free;
  inherited Destroy;
end;

{ TgirUnion }

constructor TgirUnion.Create(AOwner: TObject; ANode: TDomNode);
begin
  inherited Create(AOwner, ANode);
  FObjectType:=otUnion;
end;

procedure TgirUnion.ParseNode(ANode: TDomNode; ANodeType: TGirToken);
begin
  case ANodeType of
    gtRecord: Fields.Add(TgirRecord.Create(Owner, ANode));
    gtMethod: Fields.Add(TgirMethod.Create(Owner, ANode));
    gtConstructor: Fields.Add(TgirConstructor.Create(Owner, ANode));
  else
    inherited ParseNode(ANode, ANodeType);
  end;
end;

{ TgirGType }

procedure TgirGType.ParseNode(ANode: TDomNode; ANodeType: TGirToken);
begin
  case ANodeType of
    gtConstructor: Fields.Add(TgirConstructor.Create(Owner, ANode));
    //gtFunction: Fields.Add(TgirFunction.Create(Owner, ANode));
  else
    inherited ParseNode(ANode, ANodeType);
  end;
end;

constructor TgirGType.Create(AOwner: TObject; ANode: TDomNode);
begin
  inherited Create(AOwner, ANode);
  FGetTypeFunction := TDOMElement(ANode).GetAttribute('glib:get-type');
  FObjectType:=otGType;
end;

{ TgirArray }

function TgirArray.GetBestPointerLevel: Integer;
var
  ArrayCType: String;
  TypeCType: String;
  TypeNode: TDOMElement;
  ArrayCTypeLevel,
  TypeCTypeLevel: Integer;
begin
  ArrayCType:=TDOMElement(FNode).GetAttribute('c:type');
  TypeNode := TdomElement(FNode.FindNode('type'));
  TypeCType:=TDOMElement(TypeNode).GetAttribute('c:type');


  ArrayCTypeLevel := GetPointerLevelFromCType(ArrayCType);
  TypeCTypeLevel := GetPointerLevelFromCType(TypeCType);

  if ArrayCTypeLevel > TypeCTypeLevel then
  begin
    FCType:=ArrayCType;
    Exit(ArrayCTypeLevel)
  end;

  Result := TypeCTypeLevel;
  //FCType:=TypeCType; // already assigned in TgirTypeParam.Create
end;

constructor TgirArray.Create(AOwner: TObject; ANode: TDomNode);
var
  Node: TDomELement;
begin
  FObjectType:=otArray;
  FNode := ANode;
  inherited Create(AOwner, ANode);
  // ctype is in two places for arrays. one as an attribute of the array node.
  // and in the subnode 'type' assigned above in inherited Create.
  FPointerLevel:=GetBestPointerLevel;
  Node := TDomElement(ANode.FindNode('type'));
  if Node <> nil then
  begin
    FVarType := TgirNamespace(Owner).LookupTypeByName(Node.GetAttribute('name'), CType);
    FVarType.ImpliedPointerLevel:=FPointerLevel;
    TryStrToInt(TDomElement(ANode).GetAttribute('fixed-size'), FFixedSize);
  end;


  Node := TDOMElement(ANode.ParentNode);
  FParentFieldName := Node.GetAttribute('name');
  if FName = '' then
    FName := FParentFieldName;
end;

{ TgirObject }

procedure TgirObject.ParseNode(ANode: TDomNode; ANodeType: TGirToken);
begin
  case ANodeType of
    //gtMethod: (Owner as TgirNamespace).ParseSubNode(ANode);
    gtMethod: Fields.Add(TgirMethod.Create(Owner, ANode));
  else
    inherited ParseNode(ANode, ANodeType);
  end;
end;

constructor TgirObject.Create(AOwner: TObject; ANode: TDomNode);
begin
  inherited Create(AOwner, ANode);
  FObjectType:=otObject;
end;

{ tgirRecord }



function TgirRecord.GetHasFields: Boolean;
begin
  Result := Fields.FHasFields;
end;

procedure TgirRecord.HandleUnion(ANode: TDomNode);
var
  Item: TgirUnion;
begin
  Item := TgirUnion.Create(Owner, ANode);
  FFields.Add(Item);
end;

procedure TgirRecord.HandleField(ANode: TDomNode);
var
  Node: TDOMNode;
begin
  Node := ANode.FirstChild;
  while Node <> nil do
  begin
    case GirTokenNameToToken(Node.NodeName) of
      gtDoc,
      gtDocDeprecated:;
      gtType: FFields.Add(TgirTypeParam.Create(Owner, ANode));
      gtCallback: FFields.Add(TgirCallback.Create(Owner, Node));
      gtArray: Fields.Add(TgirArray.Create(Owner, Node));
    else
      girError(geError ,Format(geUnhandledNode,[ClassName, Node.NodeName]));
      halt;
    end;
    Node := Node.NextSibling;
  end;

end;

procedure tgirRecord.ParseNode(ANode: TDomNode; ANodeType: TGirToken);
var
  NameStr: String;
begin
  case ANodeType of
    gtDoc,
    gtDocDeprecated,
    gtSourcePosition:;
    gtField : HandleField(ANode);
    gtUnion: HandleUnion(ANode);
    gtFunction: begin
                  //(Owner as TgirNamespace).ParseSubNode(ANode);
                  //we'll add it for now since it may be interesting to make this into an object later
                  Fields.Add(TgirFunction.Create(Owner, ANode));
    end
  else
    NameStr := TDOMElement(ANode).GetAttribute('name');
    girError(geWarn,Format(geUnhandledNode,[ClassName, ANode.ParentNode.Attributes.Item[0].NodeValue +' : >> '+ ANode.NodeName+' << '+ NameStr]));
  end;
end;

constructor tgirRecord.Create(AOwner: TObject; ANode: TDomNode);
var
  Node: TDomNode;
begin
  inherited Create(AOwner, ANode);
  FFields := TgirFieldsList.Create;
  Node := ANode.FirstChild;
  while Node <> nil do
  begin
    ParseNode(Node, GirTokenNameToToken(Node.NodeName));
    Node := Node.NextSibling;
  end;
  FObjectType:=otRecord;
end;

destructor tgirRecord.Destroy;
begin
  FFields.Free;
  inherited Destroy;
end;

{ TgirFunctionReturn }

constructor TgirFunctionReturn.Create(AOwner: TObject; ANode: TDomNode);
begin
  inherited Create(AOwner, ANode);
  FObjectType:=otFunctionReturn;
end;

{ TGirFunctionParam }

constructor TGirFunctionParam.Create(AOwner: TObject; ANode: TDomNode);
begin
  inherited Create(AOwner, ANode);
end;

constructor TGirFunctionParam.Create(AOwner: TObject; ANode: TDomNode; AIsInstanceParam: Boolean);
begin
  inherited Create(AOwner, ANode, AIsInstanceParam);
  FObjectType:=otFunctionParam;
end;

{ TgirTypeParam }

function TgirTypeParam.GetType: TGirBaseType;
begin
  if (FVarType <> nil) and (FVarType.ClassType = TgirFuzzyType) and (TgirFuzzyType(FVarType).ResolvedType <> nil) then
  begin
    TgirFuzzyType(FVarType).ResolvedType.ImpliedPointerLevel:=FVarType.ImpliedPointerLevel;
    FVarType := TgirFuzzyType(FVarType).ResolvedType;
  end;
  Result := FVarType;
end;

function TgirTypeParam.GetPointerLevel: Integer;
begin
  Result := FPointerLevel;
end;

procedure NodeURL(ANode: TDomNode);
var
  URL: String = '';
begin
  while ANode <> nil do
  begin
    try
    Url := '/'+ANode.NodeName+':'+TDOMElement(ANode).GetAttribute('name')+Url;
    ANode := ANode.ParentNode;

    except
      ANode := nil
    end;
  end;
  WriteLn(URL);
end;

constructor TgirTypeParam.Create(AOwner: TObject; ANode: TDomNode);
  function PointerLevelFromVarName(AName: String): Integer;
  var
      C: Char;
  begin
    Result := 0;
    for C in AName do
      if C = '*' then
        Inc(Result);
  end;

  function AssignC_Type(C_Type: String): String;
  begin
    if Pos('const ', C_Type) > 0 then
    begin
      FIsConst:=True;
      Result := Copy(C_Type, 7, Length(C_Type));
    end
    else
      Result := C_Type;

    { Pascal doesn't support the concept of "pointer to const", so let's reduce
      some special cases by stripping the "const" keyword. }
    // used by GLib-2.0.gir and Gio-2.0.gir
    Result := StringReplace(Result, 'gchar* const*', 'gchar**', [rfReplaceAll]);
  end;
var
  Node: TDOMElement;
  C_Type: String;
  Tmp: String;
  Token: TGirToken;
  VarTypeName: String;
  SubTypeNode: TDomElement;
  SubTypeName: String;
  ParamDir: TGirToken;
  ParamPointerLevel: Integer;
begin
  inherited Create(AOwner, ANode);
  //NodeURL(ANode);
  //Node := TDomELement(ANode.FindNode('type'));
  Node := TDOMElement(ANode.FirstChild);
  if Node = nil then
     girError(geError, Format(geMissingNode,[ClassName, '', ANode.NodeName]));

  FPointerLevel:=-1;

  ParamDir:= GirTokenNameToToken(TDomElement(ANode).GetAttribute('direction'));
  case ParamDir of
    gtIn,
    gtEmpty: ParamPointerLevel := 0;
    gtOut,
    gtInOut: ParamPointerLevel := 1;
  end;


  while Node <> nil do
  begin
    // it's one or the other
    Token := GirTokenNameToToken(Node.NodeName);
    case Token of
      gtDoc,
      gtDocDeprecated:;
      gtType:    begin
                   C_Type := AssignC_Type(Node.GetAttribute('c:type'));

                   FCType:= C_Type;
                   VarTypeName:=Node.GetAttribute('name');
                   if VarTypeName = '' then
                     VarTypeName:= StringReplace(C_Type, '*', '', [rfReplaceAll]);
                   FVarType := TgirNamespace(Owner).LookupTypeByName(VarTypeName, C_Type);

                   SubTypeNode := TDomElement(Node.FindNode('type'));
                   if SubTypeNode <> nil then
                   begin
                     SubTypeName := SubTypeNode.GetAttribute('name');
                     if  (SubTypeName = 'any') or (SubTypeName = 'gpointer') then // 'any' means gpointer for some reason
                       Inc(ParamPointerLevel);
                   end;
                   if InheritsFrom(TgirArray) then
                     ParamPointerLevel:=TgirArray(Self).GetBestPointerLevel
                   else if GetPointerLevelFromCType > ParamPointerLevel then
                     ParamPointerLevel:=GetPointerLevelFromCType;
                 end;
      gtArray:   begin

                   C_Type := AssignC_Type(Node.GetAttribute('c:type'));
                   FVarType := TgirNamespace(Owner).LookupTypeByName(TDOMElement(Node.FirstChild).GetAttribute('name'), C_Type);
                   Tmp := Node.GetAttribute('length');
                   if Tmp <> '' then
                     FVarType.ImpliedPointerLevel:=StrToInt(Tmp);
                   if PointerLevelFromVarName(C_Type) > ParamPointerLevel then
                     ParamPointerLevel:=PointerLevelFromVarName(C_Type);
                   if (ParamPointerLevel = 0) and (ParamDir in [gtOut, gtInOut]) then
                     ParamPointerLevel:=1;
                 end;
      gtVarArgs: begin
                   FVarType := nil
                 end
    else
      girError(geError, Format(geUnexpectedNodeType,[ClassName, Node.NodeName, GirTokenName[gtParameter]]));
    end;
    Node := TDOMElement(Node.NextSibling);
  end;


  //if FPointerLevel = -1 then
  //  FPointerLevel := PointerLevelFromVarName(C_Type);
  if ParamPointerLevel > FPointerLevel then
    FPointerLevel:=ParamPointerLevel;

  if (FVarType <> nil) {and (GirTokenNameToToken(ANode.NodeName) = gtArray)} then
  begin
    FVarType.ImpliedPointerLevel:=FPointerLevel;
    //FVarType.ImpliedPointerLevel := PointerLevelFromVarName(CType);
    if FVarType.Deprecated and (TgirNamespace(Owner).DeprecatedVersion <= DeprecatedVersion) and not FVarType.DeprecatedOverride then
    begin
      girError(geWarn, Format('Type %s is deprecated but is used by %s. Including %s',[FVarType.CType, Name, FVarType.CType]));
      FVarType.DeprecatedOverride:=True;
    end;
  end;

  if (FVarType <> nil) and (Token <> gtVarArgs) then
      FVarType.ImpliedPointerLevel:=PointerLevel; //only will grow

  if (FVarType = nil) and (Token <> gtVarArgs) then
  begin
    WriteLn('Vartype name = ',VarTypeName);
    VarTypeName := ANode.NodeName;
    Node := TDOMElement(Anode.ParentNode);
    while Node <> nil do
    begin
      if node.InheritsFrom(TDOMElement) then
        VarTypeName := Node.NodeName + '('+Node.GetAttribute('name')+')/'+ VarTypeName
      else
        VarTypeName := Node.NodeName + '/'+ VarTypeName;
      Node := TDOMElement(Node.ParentNode);
    end;
    WriteLn('Vartype is nil when it shouldnt be! '+VarTypeName );
    raise Exception.Create('Vartype is nil when it shouldn''t be! ');
  end;
  FObjectType:=otTypeParam;
end;

constructor TgirTypeParam.Create(AOwner: TObject; ANode: TDomNode;
  AIsInstanceParam: Boolean);
begin
  FIsInstanceParam:=AIsInstanceParam;
  Create(AOwner, ANode);
end;

{ TgirFunction }

constructor TgirFunction.Create(AOwner: TObject; ANode: TDomNode);
var
  Node: TDOMNode;
  NodeToken: TGirToken;

  procedure CheckAddErrorParam(ParentNode: TDomNode);
  //const
  //  ErrorXML = '<parameter name="error"> <type name="Error" c:type="GError**"/> </parameter>';
  var
    Param: TGirFunctionParam;
    AddParam: Boolean;
    ErrorNode: TDOMElement;
    TypeNode: TDOMElement;
  begin
    // some functions have throws=1 as a property of the node. This indicates that
    // the last parameter is GError **error. The introspection file does not include
    // this as a parameter so we have to add it ourselves
    AddParam:=False;
    if FParams.Count > 0 then
    begin
      Param := FParams.Param[FParams.Count-1];
      //WriteLn(Param.CType);
      if Param.CType <> 'GError**' then
      begin
        AddParam:=True;

      end;
    end
    else
      AddParam:=True;

    if AddParam then
    begin
      //girError(geInfo, Format(geAddingErrorNode,[ClassName, CIdentifier]));
      ErrorNode := ParentNode.OwnerDocument.CreateElement('parameter');
      ErrorNode.SetAttribute('name','error');
      TypeNode := ParentNode.OwnerDocument.CreateElement('type');
      ErrorNode.AppendChild(TypeNode);
      TypeNode.SetAttribute('name','Error');
      TypeNode.SetAttribute('c:type','GError**');
      ParentNode.AppendChild(ErrorNode);
    end;
  end;


  procedure CreateParameters(ANode: TDomNode);
  var
    PNode: TDomNode;
    Param: TGirFunctionParam;
  begin
    PNode := ANode.FirstChild;
    while PNode <> nil do
    begin
      case GirTokenNameToToken(PNode.NodeName) of
        gtDoc,
      gtDocDeprecated:;
        gtParameter:
          begin
            Param := TGirFunctionParam.Create(AOwner, PNode, False);
            FParams.Add(Param);
          end;
        gtInstanceParameter:
          begin
            Param := TGirFunctionParam.Create(AOwner, PNode, True);
            FParams.Add(Param);
          end;
      else
        girError(geError, Format(geUnexpectedNodeType,[ClassName, PNode.NodeName, GirTokenName[gtParameter]]));
      end;
      if FThrowsGError and (PNode.NextSibling = nil) then
        CheckAddErrorParam(ANode); // may add a NextSibling
      PNode := PNode.NextSibling;
    end;
  end;

begin
  inherited Create(AOwner, ANode);
  FParams := TgirParamList.Create;
  FCIdentifier:=TDOMElement(ANode).GetAttribute('c:identifier');
  ThrowsGError:=TDOMElement(ANode).GetAttribute('throws') = '1';
  if FName = '' then FName:=FCIdentifier;
  if FName = '' then FName:=StringReplace(FCType, '*', '', [rfReplaceAll]);

  NodeToken := GirTokenNameToToken(ANode.NodeName);
  if not (NodeToken in [gtFunction, gtMethod, gtCallback, gtConstructor, gtVirtualMethod, gtGlibSignal]) then
  begin
    girError(geError, Format(geUnexpectedNodeType,[ClassName,ANode.NodeName, GirTokenName[gtFunction]+'", "'+      GirTokenName[gtMethod]+'", "'+
                                                                             GirTokenName[gtCallback]+'", "'+      GirTokenName[gtConstructor]+'", "'+
                                                                             GirTokenName[gtVirtualMethod]+'", "'+ GirTokenName[gtGlibSignal]    ]));
    Halt;
  end;

  Node := ANode.FirstChild;
  while Node <> nil do
  begin
    case GirTokenNameToToken(Node.NodeName) of
      gtDoc,
      gtDocDeprecated,
      gtSourcePosition:;
      gtReturnValue: FReturns := TgirFunctionReturn.Create(AOwner, Node);
      gtParameters: CreateParameters(Node);
      else
        girError(geWarn, Format(geUnhandledNode,[ClassName, Node.NodeName]));
    end;
    Node := Node.NextSibling;
  end;
  if FReturns = nil then
    begin
      WriteLn('Return value not defined for: ', Name);
      Halt
    end;
 FObjectType:=otFunction;
end;

destructor TgirFunction.Destroy;
begin
  FParams.Free;
  inherited Destroy;
end;

{ TgirEnumList }

function TgirEnumList.GetMember(AIndex: Integer): PgirEnumMember;
begin
  Result:= PgirEnumMember(Items[AIndex]);
end;

procedure TgirEnumList.Delete(Index: Integer);
begin
  Dispose(Member[Index]);
  TList(Self).Delete(Index);
end;

{ TgirEnumeration }

procedure TgirEnumeration.AddMember(AName, AValue, ACIdentifier: String;
  Node: TDomElement);
var
  Member: PgirEnumMember;
  IntValue: LongInt;
  FailPoint: Integer;
begin
  if (ACIdentifier = '') and Assigned(Node) then
  begin
    // sometimes there is an attribute child node
    Node := TDomElement(Node.FirstChild);
    while Node <> nil do
    begin
      if Node.NodeName = 'attribute' then
      begin
        if Node.GetAttribute('name') = 'c:identifier' then
          ACIdentifier:=Node.GetAttribute('value');
      end;
      Node := TDomElement(Node.NextSibling);
    end;
  end;
  if ACIdentifier = 'GDK_DRAG_MOTION' then ACIdentifier := 'GDK_DRAG_MOTION_';
  if ACIdentifier = 'GDK_DRAG_STATUS' then ACIdentifier := 'GDK_DRAG_STATUS_';
  if ACIdentifier = 'GDK_PROPERTY_DELETE' then ACIdentifier := 'GDK_PROPERTY_DELETE_';

  // See of the enum needs a signed type or not. Probably not.
  if not FNotIntTypeEnum and not FNeedsSignedType then
  begin
    Val(AValue, IntValue, FailPoint);
    if (FailPoint = 0) and (Length(AValue) > 0) and (AValue[1] = '-') then
      FNeedsSignedType:=True;
    if FailPoint <> 0 then
      FNotIntTypeEnum:=True; // so we won't continue trying to convert invalid values to ints.
  end;

  New(Member);
  Member^.Name:=AName;
  Member^.Value:=AValue;
  Member^.CIdentifier:=ACIdentifier;
  FMembers.Add(Member);
  //girError(geDebug, Format('Added Enum [%s] Member "%s" with value "%s"',[Name, AName, AValue]));
end;

procedure TgirEnumeration.HandleFunction(ANode: TDomNode);
var
  NS: TgirNamespace;
begin
  NS := TgirNamespace(FOwner);
  NS.ParseSubNode(ANode);

end;

constructor TgirEnumeration.Create(AOwner: TObject; ANode: TDomNode);
var
  Node: TDOMElement;
  NameStr: DOMString;
begin
  inherited Create(AOwner, ANode);
  FMembers := TgirEnumList.Create;
  Node := TDOMElement(ANode.FirstChild);
  while Node <> nil do
  begin
    case GirTokenNameToToken(Node.NodeName) of
      gtDoc,
      gtDocDeprecated,
      gtSourcePosition:;
      gtMember: AddMember(Node.GetAttribute('name'), Node.GetAttribute('value'),Node.GetAttribute('c:identifier'), Node);
      // some enumerations seem to have functions part of them. They are only functions directly related to the enumeration and cannot be part of the enum
      gtFunction: HandleFunction(Node);
    else
      NameStr := TDOMElement(Node).GetAttribute('name');
      girError(geWarn,Format(geUnhandledNode,[ClassName, Node.ParentNode.Attributes.Item[0].NodeValue +' : >> '+ Node.NodeName+' << '+ NameStr]));
      //girError(geWarn,Format(geUnhandledNode, [ClassName, Node.NodeName]));
    end;
    Node := TDOMElement(Node.NextSibling);
  end;
  FObjectType:=otEnumeration;
end;

destructor TgirEnumeration.Destroy;
begin
  FMembers.Free;
  inherited Destroy;
end;

{ TgirConstant }

constructor TgirConstant.Create(AOwner: TObject; ANode: TDomNode);
var
  Node: TDOMElement;
begin
  inherited Create(AOwner, ANode);
  FCName:=TDomELement(ANode).GetAttribute('c:type');
  if FCName = '' then
    FCName := FName;
  Node := TDomELement(ANode.FindNode('type'));
  FTypeDecl := TgirNamespace(Owner).LookupTypeByName(Node.GetAttribute('name'), Node.GetAttribute('c:type'));
  FValue:= TDOMElement(ANode).GetAttribute('value');
  FIsString:=(Node.GetAttribute('c:type') = 'gchar*') or (Node.GetAttribute('name') = 'utf8');
  //girError(geDebug, Format('Added constant "%s" with value "%s" of type "%s"',[Name, Value, FTypeDecl.Name]));
  FObjectType:=otConstant;
end;

{ TgirFuzzyType }

function TgirFuzzyType.GetResolvedType: TGirBaseType;
begin
  Result := FResolvedType;
end;

procedure TgirFuzzyType.SetResolvedType(const AValue: TGirBaseType);
begin
  if AValue = FResolvedType then
    Exit;
  FResolvedType := AValue;
  if Assigned(FResolvedType) then
    FResolvedType.ImpliedPointerLevel:=ImpliedPointerLevel;
  //girError(geDebug, 'Resolved FuzzyType '+AValue.Name);
end;

constructor TgirFuzzyType.Create(AOwner: TObject; AName: String; ACtype: String);
begin
  FName:=AName;
  FOwner := AOwner;
  FCType:=ACtype;
  FObjectType:=otFuzzyType;
  FVersion := TgirNamespace(FOwner).Version.AsMajor;
  FDeprecatedVersion := girVersion(MaxInt, MaxInt); // not deprecated
  //girError(geFuzzy, 'Creating Fuzzy Type "'+AName+'/'+ACtype+'"');
end;

{ TgirAlias }

function TgirAlias.GetForType: TGirBaseType;
begin
  if (FForType <> nil) and (FForType.ObjectType = otFuzzyType) and (TgirFuzzyType(FForType).ResolvedType <> nil) then
    FForType := TgirFuzzyType(FForType).ResolvedType;
  Result := FForType;
end;

constructor TgirAlias.Create(AOwner: TObject; ANode: TDomNode);
var
  TmpNode, Node: TDOMElement;
  NodePath, NodeName, NodeCType: String;
begin
  inherited Create(AOwner, ANode);
  NodePath := '';
  Node := TDomELement(ANode.FindNode('type'));
  TmpNode := Node;
    while TmpNode <> nil do
    begin
      if TmpNode.InheritsFrom(TDOMElement) then
        NodePath := TmpNode.NodeName + '('+TmpNode.GetAttribute('name')+')/'+ NodePath
      else
        NodePath := TmpNode.NodeName + '/'+ NodePath;
      TmpNode := TDOMElement(TmpNode.ParentNode);
    end;

  NodeName := Node.GetAttribute('name');
  NodeCType := Node.GetAttribute('c:type');

  { "alias" might implicitly declare an 'opaque' data type. In that case we want
    to substitute the type (so that a suitable fuzzy type gets added), and tag
    the current object; which allows TPascalUnit.HandleAlias() to take care of
    correctly emitting the opaque type (= Pascal record) later. }
  if (NodeName = 'none') and (NodeCType = 'void') then
  begin
    FIsOpaque := True;
    Node := ANode as TDOMElement;
    if Node.hasAttribute('name') then
      NodeName := Node.GetAttribute('name'); // "name" attribute of ANode
    if Node.hasAttribute('c:type') then
      NodeCType := Node.GetAttribute('c:type'); // "c:type" attribute of ANode
  end;

  //WriteLn('ALIAS: ', NodeName, ' ', NodePath);
  FForType := TgirNamespace(Owner).LookupTypeByName(NodeName, NodeCType);
  if FIsOpaque then assert(FForType.ObjectType = otFuzzyType); // sanity check
  FObjectType:=otAlias;
end;

constructor TgirAlias.Create(AOwner: TObject; AName, ACType,
  ATranslatedName: String);
begin
  FOwner := AOwner;
  FName:=AName;
  FCType:=ACType;
  FTranslatedName:=ATranslatedName;
  FObjectType:=otAlias;
  FVersion := TgirNamespace(FOwner).Version.AsMajor;
  FDeprecatedVersion := girVersion(MaxInt, MaxInt); // not deprecated
end;

{ TGirBaseType }

procedure TGirBaseType.SetImpliedPointerLevel(AValue: Integer);
begin
  if FImpliedPointerLevel<AValue then
  begin
      FImpliedPointerLevel:=AValue;
  end;
  if FImpliedPointerLevel > 3 then
    FImpliedPointerLevel:=3;
end;

function TGirBaseType.MaybeResolvedType: TGirBaseType;
begin
  if Self.InheritsFrom(TgirFuzzyType) and (TgirFuzzyType(Self).ResolvedType <> nil) then
    Result := TgirFuzzyType(Self).ResolvedType
  else
    Result := Self;
end;

function TGirBaseType.GetPointerLevelFromCType(ACType: String): Integer;
var
  C: Char;
begin
  if ACType = '' then
    ACType:=FCType;
  Result := 0;
  for C in ACType do
    if C = '*' then
      Inc(Result);
end;

constructor TGirBaseType.Create(AOwner: TObject; ANode: TDomNode);
var
  Element: TDOMElement absolute ANode;
  Node: TDomNode;
  AttrValue: String;
begin
  if ANode = nil then
    girError(geError, 'Creating '+ClassName+' with a nil node');
  FOwner := AOwner;
  FCType := Element.GetAttribute('c:type');
  if FCType = '' then
    FCType := Element.GetAttribute('glib:type-name');
  FName  := Element.GetAttribute('name');
  try
    FVersion:= girVersion(Element.GetAttribute('version'));
  except
    FVersion := TgirNamespace(FOwner).Version.AsMajor;
  end;

  FDisguised := Element.GetAttribute('disguised') = '1';
  FGLibGetType:= Element.GetAttribute('glib:get-type');
  AttrValue := Element.GetAttribute('bits');
  if AttrValue <> '' then
    FBits := StrToInt(AttrValue);
  Node := ANode.FindNode('doc');
  if Node <> nil then
    FDoc := Node.FirstChild.TextContent;
  ImpliedPointerLevel:=2;
  FDeprecated:=TDOMElement(ANode).GetAttribute('deprecated') = '1';
  if FDeprecated then
  begin
    Node := ANode.FindNode('doc-deprecated');
    if Node <> nil then
    begin
      FDeprecatedMsg:=StringReplace(Node.TextContent, #10, ' ', [rfReplaceAll]);
      FDeprecatedMsg := StringReplace(FDeprecatedMsg, '''', '''''', [rfReplaceAll]); // replace ' with ''
    end;
    try
      FDeprecatedVersion:=girVersion(TDOMElement(ANode).GetAttribute('deprecated-version'));
    except
      FDeprecatedVersion:=TgirNamespace(FOwner).Version.AsMajor;
    end;
  end
  else
    FDeprecatedVersion := girVersion(MaxInt, MaxInt); // not deprecated

  FObjectType:=otBaseType;
end;

end.

