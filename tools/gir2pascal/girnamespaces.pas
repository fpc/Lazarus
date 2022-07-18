{
girnamespaces.pas
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
unit girNameSpaces;

{$mode objfpc}{$H+}
{$INTERFACES CORBA}

interface

uses
  Classes, DOM, girParser, girTokens, girObjects, contnrs;

type

  TgirNeedGirFileEvent = function (AGirFile: TObject; BaseNamespaceName: String) : TXMLDocument of object;

  { TgirNamespace }

  TgirNamespace = class(IgirParser)
  private
    FCIncludeName: String;
    FConstants: TList;
    FCPackageName: String;
    FCPrefix: String;
    FDeprecatedVersion: TGirVersion;
    FFunctions: TList;
    FMaxSymbolVersion: TGirVersion;
    FNameSpace: String;
    FOnlyImplied: Boolean;
    FOnNeedGirFile: TgirNeedGirFileEvent;
    FOwner: TObject;
    FRequiredNameSpaces: TList;
    FSharedLibrary: String;
    FTypes: TFPHashObjectList;
    FUnresolvedTypes: TList;
    FVersion: TGirVersion;
    procedure SetOnNeedGirFile(AValue: TgirNeedGirFileEvent);
  protected
    function AddFuzzyType(AName: String; ACType: String): TGirBaseType;
    procedure HandleAlias(ANode: TDomNode);
    procedure HandleConstant(ANode: TDomNode);
    procedure HandleEnumeration(ANode: TDomNode);
    procedure HandleBitField(ANode: TDomNode);
    procedure HandleCallback(ANode: TDOMNode);
    procedure HandleFunction(ANode: TDOMNode);
    procedure HandleUnion(ANode: TDOMNode);
    {
      Some 'records' have methods these corelate to pascal 'object'
      GType extends this 'object' type to have a sort of vmt
      GObject and subclasses extend gtype and adds more vmt method entries and method entries to the instance itself.
    }
    procedure HandleRecord(ANode: TDomNode); //could be struct, object, gtype, gobject, or gobject descendant
    procedure HandlePlainObject(ANode: TDomNode); // is a record/object with methods but no gtype
    procedure HandleGType(ANode: TDomNode); // one step above plain object
    procedure HandleClassStruct(ANode: TDomNode); // one step above GType. Is the 'Virtual' part of an object (VMT)
    procedure HandleClass(ANode: TDomNode); // one step above GType. Is the object structure and it's methods. ClassStruct is like the VMT
    procedure HandleInterface(ANode: TDomNode);
    procedure AddGLibBaseTypes;
  public
    procedure AddType(AType: TGirBaseType);
    function LookupTypeByName(AName: String; const ACType: String; SearchOnly: Boolean = False): TGirBaseType;
    function ResolveFuzzyType(AFuzzyType: TgirFuzzyType): TGirBaseType;
    function UsesGLib: Boolean;
    procedure ResolveFuzzyTypes; // called after done
    procedure ParseNode(ANode: TDomNode);
    procedure ParseSubNode(ANode: TDomNode); // generally do not use outside of TgirNameSpace
    constructor Create(AOwner:TObject; AImpliedNamespace: String);
    constructor CreateFromRepositoryNode(AOwner:TObject; ANode: TDOMNode; AIncludes: TList);
    destructor Destroy; override;
    property NameSpace: String read FNameSpace;
    property CIncludeName: String read FCIncludeName;
    property CPackageName: String read FCPackageName;
    property CPrefix: String read FCPrefix;
    property RequiredNameSpaces: TList Read FRequiredNameSpaces;
    property SharedLibrary: String read FSharedLibrary;
    property Version: TGirVersion read FVersion;
    property OnlyImplied: Boolean read FOnlyImplied;
    property Owner: TObject Read FOwner;

    // has all types in it (records classes classstructs bitfields callbacks gtypes unions etc) does not contain consts or functions
    property Types: TFPHashObjectList read FTypes;

    property Functions: TList read FFunctions;
    property Constants: TList read FConstants;
    property UnresolvedTypes: TList read FUnresolvedTypes write FUnresolvedTypes;
    // exclude symbols newer than this version
    property MaxSymbolVersion: TGirVersion read FMaxSymbolVersion write FMaxSymbolVersion;
    // exclude symbols this version and older that are marked as deprecated
    property DeprecatedVersion: TGirVersion read FDeprecatedVersion write FDeprecatedVersion;
  end;

  { TgirNamespaces }

  TgirNamespaces = class(TList)
  private
    FOnNeedGirFile: TgirNeedGirFileEvent;
    FOwner: TObject;
    function GetNameSpace(AIndex: Integer): TgirNamespace;
    procedure SetNameSpace(AIndex: Integer; const AValue: TgirNamespace);
    procedure SetOnNeedGirFile(AValue: TgirNeedGirFileEvent);
  public
    constructor Create(AOwner: TObject);
    function FindNameSpace(AName: String; Version: String = ''): TgirNamespace;
    property NameSpace[AIndex: Integer]: TgirNamespace read GetNameSpace write SetNameSpace;
    property Owner: TObject read FOwner;
    property OnNeedGirFile: TgirNeedGirFileEvent read FOnNeedGirFile write SetOnNeedGirFile;
  end;

implementation
uses
  girErrors, SysUtils, girCTypesMapping;

{ TgirNamespaces }

function TgirNamespaces.GetNameSpace(AIndex: Integer): TgirNamespace;
begin
  Result := TgirNamespace(Items[AIndex]);
end;

procedure TgirNamespaces.SetNameSpace(AIndex: Integer;
  const AValue: TgirNamespace);
begin
  Items[AIndex] := AValue;
end;

procedure TgirNamespaces.SetOnNeedGirFile(AValue: TgirNeedGirFileEvent);
begin
  if FOnNeedGirFile=AValue then Exit;
  FOnNeedGirFile:=AValue;
end;

constructor TgirNamespaces.Create(AOwner: TObject);
begin
  FOwner := AOwner;
  inherited Create;
end;

function TgirNamespaces.FindNameSpace(AName: String; Version: String=''): TgirNamespace;
var
  i: Integer;
  NameSpaceSearchedFor: Boolean;
  Doc: TXMLDocument;
begin
  Result := nil;
  NameSpaceSearchedFor := False;
  while Result = nil do
  begin
    for i := 0 to Count-1 do
    begin
      if NameSpace[i].NameSpace = AName then
        Exit(NameSpace[i]);
    end;

    if NameSpaceSearchedFor then
      Exit;
    NameSpaceSearchedFor := True;
    if Assigned(FOnNeedGirFile) then
    begin
      Doc := FOnNeedGirFile(Owner, AName+'-'+Version);
      if Doc <> nil then
      begin
        (Owner as IgirParser).ParseNode(Doc.DocumentElement);
        Doc.Free;
      end;
    end;
  end;
end;

{ TgirNamespace }

procedure TgirNamespace.ParseNode(ANode: TDomNode);

begin
  ANode := ANode.FirstChild;
  while ANode <> nil do
  begin
    //girError(geDebug, 'Parsing Node "'+ANode.NodeName+'"');
    ParseSubNode(ANode);
    ANode := ANode.NextSibling;
  end;
  ResolveFuzzyTypes;
end;

procedure TgirNamespace.SetOnNeedGirFile(AValue: TgirNeedGirFileEvent);
begin
  if FOnNeedGirFile=AValue then Exit;
  FOnNeedGirFile:=AValue;
end;

function TgirNamespace.AddFuzzyType(AName: String; ACType: String
  ): TGirBaseType;
begin
  Result := TgirFuzzyType.Create(Self, AName, ACType);
  AddType(Result);
  FUnresolvedTypes.Add(Result);
end;

procedure TgirNamespace.HandleAlias(ANode: TDomNode);
var
  Item: TgirAlias;
begin
  Item := TgirAlias.Create(Self, ANode);
  AddType(Item);
end;

procedure TgirNamespace.HandleConstant(ANode: TDomNode);
var
  Item: TgirConstant;
begin
  Item := TgirConstant.Create(Self, ANode);
  FConstants.Add(Item);
end;

procedure TgirNamespace.HandleEnumeration(ANode: TDomNode);
var
  Item : TgirEnumeration;
begin
  Item := TgirEnumeration.Create(Self, ANode);
  AddType(Item);
end;

procedure TgirNamespace.HandleBitField(ANode: TDomNode);
var
  Item : TgirBitField;
begin
  Item := TgirBitField.Create(Self, ANode);
  AddType(Item);
end;

procedure TgirNamespace.HandleCallback(ANode: TDOMNode);
var
  Item: TgirCallback;
begin
  Item := TgirCallback.Create(Self, ANode);
  AddType(Item);
end;

procedure TgirNamespace.HandleFunction(ANode: TDOMNode);
var
  Item: TgirFunction;
begin
  Item := TgirFunction.Create(Self, ANode);
  Functions.Add(Item);
end;

procedure TgirNamespace.HandleUnion(ANode: TDOMNode);
var
  Item: TgirUnion;
begin
  Item := TgirUnion.Create(Self, ANode);
  AddType(Item);
end;

procedure TgirNamespace.HandleRecord(ANode: TDomNode);
var
  Item: tgirRecord;
begin
  if TDOMElement(ANode).GetAttribute('glib:is-gtype-struct-for') <> '' then // is gobject class
  begin
    HandleClassStruct(ANode);
  end
  else if TDOMElement(ANode).GetAttribute('glib:get-type') <> '' then // is GType
    HandleGType(ANode)
  else if (ANode.FindNode('method') <> nil) or (ANode.FindNode('constructor') <> nil) or (ANode.FindNode('function') <> nil) then // is Plain object that is not gtype
    HandlePlainObject(ANode)
  else
  begin
    Item := tgirRecord.Create(Self, ANode);
    AddType(Item);
  end;

end;

procedure TgirNamespace.HandlePlainObject(ANode: TDomNode);
var
  Item: TgirObject;
begin
  Item := TgirObject.Create(Self, ANode);
  AddType(Item);
end;

procedure TgirNamespace.HandleGType(ANode: TDomNode);
var
  Item: TgirGType;
begin
  Item := TgirGType.Create(Self, ANode);
  AddType(Item);
end;

procedure TgirNamespace.HandleClassStruct(ANode: TDomNode);
var
  Item: TgirClassStruct;
begin
  Item := TgirClassStruct.Create(Self, ANode);
  AddType(Item);
end;

procedure TgirNamespace.HandleClass(ANode: TDomNode);
var
  Item: TgirClass;
begin
  Item := TgirClass.Create(Self, ANode);
  AddType(Item);
end;

procedure TgirNamespace.HandleInterface(ANode: TDomNode);
var
  Item: TgirInterface;
begin
  Item := TgirInterface.Create(Self, ANode);
  AddType(Item);
end;

procedure TgirNamespace.AddGLibBaseTypes;
  function AddNativeTypeDef(GType: String; PascalCName: String; TranslatedName: String): TgirNativeTypeDef;
  var
    NativeType: TgirNativeTypeDef;
  begin
    NativeType:= TgirNativeTypeDef.Create(Self, GType, PascalCName);
    if TranslatedName <> '' then
      NativeType.TranslatedName:=TranslatedName;
    NativeType.ImpliedPointerLevel:=3;
    AddType(NativeType);
    Result := NativeType;

  end;
var
  i: Integer;
begin
  for i := 0 to CTypesMax-1 do
    AddNativeTypeDef(TypesGTypes[i], TypesPascalCTypes[i], '');
end;

procedure TgirNamespace.AddType(AType: TGirBaseType);
var
  PrevFound: TGirBaseType = nil;
begin
  PrevFound := TGirBaseType(FTypes.Find(AType.Name));
  if (PrevFound <> nil) and (PrevFound.ObjectType = otFuzzyType)  then
  begin
    (PrevFound as TgirFuzzyType).ResolvedType := AType;
    //WriteLn('Resolved FuzzyType: ', AType.Name);
    FUnresolvedTypes.Remove(PrevFound);
  end;
  //if PrevFound <> nil then WriteLn('Found Name Already Added: ', AType.Name, ' ', PrevFound.ObjectType, ' ', AType.ObjectType);
  if PrevFound = nil then
    FTypes.Add(AType.Name, AType);
end;

procedure TgirNamespace.ResolveFuzzyTypes;
var
  i: Integer;
  FuzzyI: Integer;
  Fuzzy: TgirFuzzyType;
  FuzzyP: Pointer absolute Fuzzy;
  Tmp: TGirBaseType;
  StillFuzzy: TList;
  Current: TGirBaseType;
  ReqNS: TgirNamespace;
begin
  i:= 0;
  FuzzyI := 0;
  Fuzzy := nil;
  StillFuzzy := TList.Create;
  while (i < FTypes.Count) or (Fuzzy <> nil) do
  begin
    // make our loop safe
    if i >= FTypes.Count then
    begin
      i := FuzzyI+1;
      StillFuzzy.Add(Fuzzy);
      Fuzzy := nil;
      continue;
    end;

    Tmp := TGirBaseType(FTypes.Items[i]);

    if Fuzzy <> nil then
    begin
      if {(Tmp.CType = Fuzzy.CType) or} (Tmp.Name = Fuzzy.Name) then
      begin
        Fuzzy.ResolvedType := Tmp;
        Tmp.ImpliedPointerLevel:=Fuzzy.ImpliedPointerLevel;
        Tmp.DeprecatedOverride:= Tmp.DeprecatedOverride or Fuzzy.DeprecatedOverride;
        i := FuzzyI+1;
        Fuzzy := nil;
        //WriteLn('Resolved Fuzzy Type: ', Tmp.CType);
        continue;
      end;
    end;

    if (Fuzzy = nil) and (Tmp.ObjectType = otFuzzyType) and (TgirFuzzyType(Tmp).ResolvedType = nil) then
    begin
      if i >= FTypes.Count then
        break;
      FuzzyI:=i;
      Fuzzy := TgirFuzzyType(Tmp);
      //WriteLn('Looking For: ',Fuzzy.CType);
    end;
    inc(i);
  end;

  // if the types are still fuzzy then we will search used namespaces for what we want
  for FuzzyP in StillFuzzy do //FuzzyP is Fuzzy absolute
  begin
    if Fuzzy.ResolvedType <> nil then
      continue;
    for i := 0 to RequiredNameSpaces.Count-1 do
    begin
      ReqNS := TgirNamespace(RequiredNameSpaces.Items[i]);
      Current := ReqNS.LookupTypeByName(Fuzzy.Name, '', True);
      if Current <> nil then
      begin
        if (Current.ObjectType = otFuzzyType) and (TgirFuzzyType(Current).ResolvedType <> nil) then
          Current := TgirFuzzyType(Current).ResolvedType;
        Fuzzy.ResolvedType := Current;
        Break;
      end;
    end;
  end;
  StillFuzzy.Free;
end;

procedure TgirNamespace.ParseSubNode(ANode: TDomNode);
begin
  case GirTokenNameToToken(ANode.NodeName) of
    gtAlias:       HandleAlias(ANode);
    gtConstant:    HandleConstant(ANode);
    gtRecord:      HandleRecord(ANode);
    gtBitField:    HandleBitField(ANode);
    gtEnumeration: HandleEnumeration(ANode);
    gtCallback:    HandleCallback(ANode);
    gtUnion:       HandleUnion(ANode);
    gtFunction:    HandleFunction(ANode);
    gtClass:       HandleClass(ANode);
    gtInterface:   HandleInterface(ANode);
    gtMethod:      HandleFunction(ANode);
    else
      girError(geError, 'Unknown NodeType: '+ANode.NodeName);
  end;
end;

function TgirNamespace.LookupTypeByName(AName: String; const ACType: String; SearchOnly: Boolean = False): TGirBaseType;
  function StripPointers(ACPointeredType: String; PtrLevel: PInteger = nil): String;
  var
    i: Integer;
  begin
    for i := Length(ACPointeredType) downto 1 do
    if ACPointeredType[i] = '*' then
    begin
      Delete(ACPointeredType, i, 1);
    end;
    if PtrLevel <> nil then
      Inc(PtrLevel^);
    Result := ACPointeredType;
  end;

var
  NS: TgirNamespace;
  NSString: String;
  FPos: Integer;
  PointerLevel: Integer = 0;
  PlainCType: String;
begin
  Result := nil;
  NS := Self;
  // some basic fixes
  PlainCType:=StringReplace(StripPointers(ACType, @PointerLevel), ' ', '_', [rfReplaceAll]);
  if (PlainCType = 'gchar') or {(PlainCType = 'guchar') or} (PlainCType = 'char') or (PlainCType = 'const_char') then
    AName := 'GLib.utf8';

  if (PlainCType = 'GType')  {or (AName = 'Type')} or (AName = 'GType')then
    AName := 'GLib.Type';

  if AName = 'any' then
    AName := 'gpointer';

  FPos := Pos('.', AName);

  if FPos > 0 then  // type includes namespace "NameSpace.Type"
  begin
    NSString:=Copy(AName,1,FPos-1);

    //NS := (Owner As TgirNamespaces).FindNameSpace(NSString);
    NS := TgirNamespaces(Owner).FindNameSpace(NSString);
    if NS = nil then
      girError(geError, 'Referenced Namespace "'+NSString+'" not found while looking for '+AName);
    AName := Copy(AName, FPos+1, Length(AName));
  end;

  if NS <> Self then SearchOnly:=True;

  //if NS <> Self then WriteLn('Self NS = ', NameSpace, ' Lookup NS = ', NS.NameSpace);
  Result := TGirBaseType(NS.Types.Find(AName));
  if (Result <> nil) and (Result.ObjectType = otFuzzyType) and (TgirFuzzyType(Result).ResolvedType <> nil) then
    Result := TgirFuzzyType(Result).ResolvedType;

  // if we find a result in another namespace then we need to depend on that namespace/unit
  if (NS <> nil) and (NS <> Self) and (Result <> nil) then
    if FRequiredNameSpaces.IndexOf(NS) = -1 then
      FRequiredNameSpaces.Add(NS);

  if (Result = nil) and Not SearchOnly then
      Result := NS.AddFuzzyType(AName, ACType);
  if Result <> nil then
    Result.ImpliedPointerLevel:=PointerLevel;

end;

function TgirNamespace.ResolveFuzzyType(AFuzzyType: TgirFuzzyType): TGirBaseType;
var
  i: Integer;
begin
  for i := 0 to FTypes.Count-1 do
  begin
    if (TGirBaseType(FTypes[i]) <> AFuzzyType) and (TGirBaseType(FTypes[i]).Name = AFuzzyType.Name) then
      Exit(TGirBaseType(FTypes[i]));
  end;
end;

function TgirNamespace.UsesGLib: Boolean;
var
  Tmp: Pointer;
  NS: TgirNamespace absolute Tmp;
begin
  Result := False;
  if Pos('glib', LowerCase(NameSpace)) = 1 then
    Exit(True);
  for Tmp in RequiredNameSpaces do
    if Pos('glib',LowerCase(NS.NameSpace)) = 1 then
      Exit(True);
end;

constructor TgirNamespace.Create(AOwner:TObject; AImpliedNamespace: String);
begin
  Fowner:=AOwner;
  FOnlyImplied:=True;
  FNameSpace:=AImpliedNamespace;
  girError(geDebug, 'Creating Stub for namespace: '+ AImpliedNamespace);
end;

constructor TgirNamespace.CreateFromRepositoryNode(AOwner:TObject; ANode: TDOMNode; AIncludes: TList);
  procedure SetCInclude;
  var
    Child: TDomElement;
  begin
    Child := TDOMElement(ANode.FindNode('c:include name'));
    if (Child <> nil) and Child.InheritsFrom(TDOMElement) then
      FCIncludeName:= Child.GetAttribute('name');
  end;
  procedure SetPackage;
  var
    Child: TDOMElement;
  begin
    Child := TDOMElement(ANode.FindNode('package'));
    if (Child <> nil) and Child.InheritsFrom(TDOMElement) then
      FCPackageName:=Child.GetAttribute('name');
  end;

var
  Node: TDOMElement;
begin
  FOwner := AOwner;
  if ANode = nil then
    girError(geError, 'expected namespace got nil');
  if ANode.NodeName <> 'repository' then
    girError(geError, 'expected "repository" got '+ANode.NodeName);
  Node := TDOMElement( ANode.FindNode('namespace') );
  FNameSpace:=Node.GetAttribute('name');
  FRequiredNameSpaces := AIncludes;
  FSharedLibrary:=Node.GetAttribute('shared-library');
  FVersion:=girVersion(Node.GetAttribute('version'));
  FCPrefix:=Node.GetAttribute('c:prefix');
  SetCInclude;
  SetPackage;
  girError(geDebug, Format('Creating namespace=%s Version=%s LibName=%s',[FNameSpace, FVersion.AsString, FSharedLibrary]));

  FConstants := TList.Create;
  FFunctions := TList.Create;
  FTypes := TFPHashObjectList.Create(True);
  FUnresolvedTypes := TList.Create;

  FMaxSymbolVersion.Major:=MaxInt;

  if FNameSpace = 'GLib' then
    AddGLibBaseTypes;
end;

destructor TgirNamespace.Destroy;
begin
  FConstants.Free;
  FFunctions.Free;
  FTypes.Free;
  FUnresolvedTypes.Free;
  if Assigned(FRequiredNameSpaces) then
    FRequiredNameSpaces.Free;

  inherited Destroy;
end;

end.

