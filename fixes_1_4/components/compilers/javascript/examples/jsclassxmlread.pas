unit JSClassXMLRead;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileProcs, OtherIdentifierTree, XMLRead, XMLCfg, DOM,
  avl_tree;

type
  TJSIdentifier = class(TOtherIdentifierTreeNode)
  public
    JSName: string;
    PascalName: string;
  end;

  TJSIUnresolvedIdentifier = class(TJSIdentifier)
  public
    Resolved: TJSIdentifier;
  end;

  TJSIAlias = class(TJSIdentifier)
  public
    PointsTo: TJSIdentifier;
  end;

  TJSIClass = class;

  TJSIParameter = class(TJSIdentifier)
  public
    Typ: TJSIdentifier;
    Optional: boolean;
  end;

  TJSIMethodFlag = (
    jsimStatic,
    jsimOverload
    );
  TJSIMethodFlags = set of TJSIMethodFlag;

  { TJSIMethod }

  TJSIMethod = class(TJSIdentifier)
  public
    Flags: TJSIMethodFlags;
    JSIClass: TJSIClass;
    Params: TFPList; // list of TJSIParameter
    ReturnType: TJSIdentifier;
    destructor Destroy; override;
    procedure ClearParams;
    procedure AddParameter(aParam: TJSIParameter);
  end;

  TJSIPropertyFlag = (
    jsipEnum,
    jsipConfig,
    jsipStatic,
    jsipDefault
    );
  TJSIPropertyFlags = set of TJSIPropertyFlag;

  TJSIProperty = class(TJSIdentifier)
  public
    Flags: TJSIPropertyFlags;
    Typ: TJSIdentifier;
  end;

  TJSIClassFlag = (
    jsicAutoCreated
    );
  TJSIClassFlags = set of TJSIClassFlag;

  { TJSIClass }

  TJSIClass = class(TJSIdentifier)
  public
    Flags: TJSIClassFlags;
    ParentClass: TJSIdentifier;
    Unitname: TJSIdentifier;
    Simplename: TJSIdentifier;
    Methods: TFPList; // list of TJSIMethod
    Properties: TFPList; // list TJSIProperty
    Classes: TFPList; // list of TJSIClass
    destructor Destroy; override;
    procedure ClearMethods;
    procedure ClearProperties;
    procedure ClearClasses;
    function FindIdentifier(const AJSName: string): TJSIdentifier;
    procedure AddClass(AClass: TJSIClass);
    procedure AddMethod(AMethod: TJSIMethod);
    procedure AddProperty(AProperty: TJSIProperty);
  end;

  { TJavascriptIdentifierTree }

  TJavascriptIdentifierTree = class(TOtherIdentifierTree)
  private
    function FindNode(Doc: TXMLDocument; const APath: String; PathHasValue: boolean): TDomNode;
    function Escape(const s: String): String;
    procedure ReadExtJSNodes(Node: TDOMNode);
    procedure ReadExtJSUnits(UnitsNode: TDOMNode);
    procedure ReadExtJSClasses(ClassesNode: TDOMNode);
    procedure ReadExtJSMethods(ClassNode: TDOMNode; JSIClass: TJSIClass);
    procedure ReadExtJSProperties(ClassNode: TDOMNode; JSIClass: TJSIClass);
    function CreateClass(const Path: string; CreateLast: boolean): TJSIClass;
    function CreateAlias(const Path: string): TJSIAlias;
    function FindGlobal(const aJSName: string): TAVLTreeNode;
    function CreateUnresolved(const aPath: string): TJSIUnresolvedIdentifier;
  public
    Globals: TAVLTree;
    constructor Create;
    destructor Destroy; override;
    procedure LoadFromFile(Filename: string);
    procedure ClearGlobals;
  end;

function CompareJSIdentifiers(Data1, Data2: Pointer): integer;
function CompareJSNameWithJSIdentifier(Key, Data: Pointer): integer;
function IsValidJSName(const Name: string): boolean;

implementation

function CompareJSIdentifiers(Data1, Data2: Pointer): integer;
var
  Ident1: TJSIdentifier absolute Data1;
  Ident2: TJSIdentifier absolute Data2;
begin
  Result:=CompareStr(Ident1.JSName,Ident2.JSName);
end;

function CompareJSNameWithJSIdentifier(Key, Data: Pointer): integer;
var
  Ident: TJSIdentifier absolute Data;
  s: String;
begin
  s:=AnsiString(Key);
  Result:=CompareStr(s,Ident.JSName);
end;

function IsValidJSName(const Name: string): boolean;
var
  i: Integer;
begin
  Result:=false;
  if Name='' then exit;
  if length(Name)>255 then exit;
  if not (Name[1] in ['a'..'z','A'..'Z','_']) then exit;
  for i:=1 to Length(Name) do
    if not (Name[i] in ['a'..'z','A'..'Z','_','0'..'9']) then exit;
  Result:=true;
end;

{ TJavascriptIdentifierTree }

function TJavascriptIdentifierTree.FindNode(Doc: TXMLDocument;
  const APath: String; PathHasValue: boolean): TDomNode;
var
  NodePath: String;
  StartPos, EndPos: integer;
  PathLen: integer;
begin
  Result := Doc.DocumentElement;
  debugln(['TJavascriptIdentifierTree.FindNode ',Result.NodeName]);
  PathLen := Length(APath);
  StartPos := 1;
  while Assigned(Result) do
  begin
    EndPos := StartPos;
    while (EndPos <= PathLen) and (APath[EndPos] <> '/') do
      Inc(EndPos);
    if (EndPos > PathLen) and PathHasValue then
      exit;
    if EndPos = StartPos then
      break;
    SetLength(NodePath, EndPos - StartPos);
    Move(APath[StartPos], NodePath[1], Length(NodePath));
    NodePath:=Escape(NodePath);
    debugln(['TJavascriptIdentifierTree.FindNode ',NodePath]);
    Result := Result.FindNode(NodePath);
    StartPos := EndPos + 1;
    if StartPos > PathLen then
      exit;
  end;
  Result := nil;
end;

function TJavascriptIdentifierTree.Escape(const s: String): String;
const
  AllowedChars = ['A'..'Z', 'a'..'z', '0'..'9', '.', '-', '_'];
var
  EscapingNecessary: Boolean;
  i: Integer;
begin
  if Length(s) < 1 then
    raise EXMLConfigError.Create(SMissingPathName);

  if not (s[1] in ['A'..'Z', 'a'..'z', '_']) then
    EscapingNecessary := True
  else
  begin
    EscapingNecessary := False;
    for i := 2 to Length(s) do
      if not (s[i] in AllowedChars) then
      begin
        EscapingNecessary := True;
	exit;
      end;
  end;

  if EscapingNecessary then
  begin
    Result := '_';
    for i := 1 to Length(s) do
      if s[i] in (AllowedChars - ['_']) then
	  Result := Result + s[i]
	else
	  Result := Result + '_' + IntToHex(Ord(s[i]), 2);
  end
  else	// No escaping necessary
    Result := s;
end;

procedure TJavascriptIdentifierTree.ReadExtJSNodes(Node: TDOMNode);
var
  UnitsNode: TDOMNode;
  ClassesNode: TDOMNode;
begin
  debugln(['TJavascriptIdentifierTree.ReadExtJSClasses ',DbgSName(Node)]);

  UnitsNode:=Node.FindNode('units');
  if UnitsNode=nil then begin
    debugln(['TJavascriptIdentifierTree.ReadExtJSClasses no units found']);
    exit;
  end;
  ReadExtJSUnits(UnitsNode);

  ClassesNode:=Node.FindNode('classes');
  if UnitsNode=nil then begin
    debugln(['TJavascriptIdentifierTree.ReadExtJSClasses no classes found']);
    exit;
  end;
  ReadExtJSClasses(ClassesNode);
end;

procedure TJavascriptIdentifierTree.ReadExtJSUnits(UnitsNode: TDOMNode);
var
  Node: TDOMNode;
  ClassesNode: TDOMNode;
  ClassNode: TDOMNode;
  CurUnitName, PascalClassname: widestring;
begin
  Node:=UnitsNode.FirstChild;
  while Node<>nil do begin
    if (Node.NodeName='unit') and (Node is TDOMElement) then begin
      CurUnitName:=TDOMElement(Node).GetAttribute('name');
      ClassesNode:=Node.FindNode('classes');
      if (ClassesNode<>nil) and (CurUnitName<>'') then begin
        ClassNode:=ClassesNode.FirstChild;
        while ClassNode<>nil do begin
          if ClassNode.NodeName='class' then begin
            if ClassNode is TDOMElement then begin
              PascalClassname:=TDOMElement(ClassNode).GetAttribute('name');
              if PascalClassname<>'' then ;
              //debugln(['TJavascriptIdentifierTree.ReadExtJSUnits ',CurUnitName,' ',PascalClassname]);
            end;
          end;
          ClassNode:=ClassNode.NextSibling;
        end;
      end;
    end;
    Node:=Node.NextSibling;
  end;
end;

procedure TJavascriptIdentifierTree.ReadExtJSClasses(ClassesNode: TDOMNode);
var
  Node: TDOMNode;
  ClassNode: TDOMElement;
  NewClass: TJSIClass;
  JSName: WideString;
  SimpleName: WideString;
  Alias: TJSIAlias;
begin
  Node:=ClassesNode.FirstChild;
  while Node<>nil do begin
    if (Node.NodeName='class') and (Node is TDOMElement) then begin
      ClassNode:=TDOMElement(Node);
      JSName:=ClassNode.GetAttribute('jsname');
      if JSName<>'' then begin
        if JSName='Object' then begin
          debugln(['TJavascriptIdentifierTree.ReadExtJSClasses SKIPPING jsname=Object pasname=',ClassNode.GetAttribute('name')]);
        end else begin
          debugln(['TJavascriptIdentifierTree.ReadExtJSClasses class=',JSName]);
          // create new class
          NewClass:=CreateClass(JSName,true);
          if not (jsicAutoCreated in NewClass.Flags) then
            raise Exception.Create('class redefined: '+JSName);
          Exclude(NewClass.Flags,jsicAutoCreated);
          // pascalname
          NewClass.PascalName:=ClassNode.GetAttribute('name');
          // simplename
          SimpleName:=ClassNode.GetAttribute('simplename');
          if (SimpleName<>'') and (SimpleName<>JSName) then begin
            Alias:=CreateAlias(SimpleName);
            if (Alias.PointsTo<>nil) and (Alias.PointsTo<>NewClass) then
              raise Exception.Create('class simplename redefined: '+JSName+' '+SimpleName);
            NewClass.Simplename:=Alias;
          end;
          // methods
          ReadExtJSMethods(ClassNode,NewClass);
          // properties
          ReadExtJSProperties(ClassNode,NewClass);
        end;
      end;
    end;
    Node:=Node.NextSibling;
  end;
end;

procedure TJavascriptIdentifierTree.ReadExtJSMethods(ClassNode: TDOMNode;
  JSIClass: TJSIClass);
var
  MethodsNode: TDOMNode;
  Node: TDOMNode;
  MethodNode: TDOMElement;
  JSName: WideString;
  NewMethod: TJSIMethod;
  ParamsNode: TDOMNode;
  SubNode: TDOMNode;
  NewParam: TJSIParameter;
  ParamNode: TDOMElement;
  ParamJSName: WideString;
  ReturnType: Widestring;
begin
  MethodsNode:=ClassNode.FindNode('methods');
  if MethodsNode=nil then exit;
  Node:=MethodsNode.FirstChild;
  while Node<>nil do begin
    if (Node.NodeName='method') and (Node is TDOMElement) then begin
      MethodNode:=TDOMElement(Node);
      JSName:=MethodNode.GetAttribute('jsname');
      if copy(JSName,1,length(JSIClass.JSName)+1)=JSIClass.JSName+'.' then
        JSName:=copy(JSName,length(JSIClass.JSName)+2,length(JSName));
      if not IsValidJSName(JSName) then
        raise Exception.Create('invalid method name '+JSName);
      NewMethod:=TJSIMethod.Create;
      NewMethod.JSName:=JSName;
      JSIClass.AddMethod(NewMethod);
      NewMethod.PascalName:=MethodNode.GetAttribute('name');
      if MethodNode.GetAttribute('static')='1' then
        Include(NewMethod.Flags,jsimStatic);
      if MethodNode.GetAttribute('overload')='1' then
        Include(NewMethod.Flags,jsimOverload);
      // return type
      ReturnType:=MethodNode.GetAttribute('return');
      if ReturnType<>'' then
        NewMethod.ReturnType:=CreateUnresolved(ReturnType);
      // parameters
      ParamsNode:=MethodNode.FindNode('params');
      if ParamsNode<>nil then begin
        SubNode:=ParamsNode.FirstChild;
        while SubNode<>nil do begin
          if (SubNode is TDOMElement) and (SubNode.NodeName='param') then begin
            ParamNode:=TDOMElement(SubNode);
            ParamJSName:=ParamNode.GetAttribute('name');
            if not IsValidJSName(ParamJSName) then
              raise Exception.Create('invalid param name '+ParamJSName);
            NewParam:=TJSIParameter.Create;
            NewParam.JSName:=ParamJSName;
            NewParam.PascalName:=ParamJSName;
            NewParam.Optional:=ParamNode.GetAttribute('optional')='1';
            NewParam.Typ:=CreateUnresolved(ParamNode.GetAttribute('type'));
            NewMethod.AddParameter(NewParam);
          end;
          SubNode:=SubNode.NextSibling;
        end;
      end;
    end;
    Node:=Node.NextSibling;
  end;
end;

procedure TJavascriptIdentifierTree.ReadExtJSProperties(ClassNode: TDOMNode;
  JSIClass: TJSIClass);
var
  PropertiesNode: TDOMNode;
  PropertyNode: TDOMElement;
  PropertyJSName: WideString;
  NewProperty: TJSIProperty;
  Node: TDOMNode;
  TypeName: WideString;
begin
  // properties
  PropertiesNode:=ClassNode.FindNode('properties');
  if PropertiesNode=nil then exit;
  Node:=PropertiesNode.FirstChild;
  while Node<>nil do begin
    if (Node is TDOMElement) and (Node.NodeName='property') then begin
      PropertyNode:=TDOMElement(Node);
      PropertyJSName:=PropertyNode.GetAttribute('jsname');
      if not IsValidJSName(PropertyJSName) then
        raise Exception.Create('invalid property name '+PropertyJSName);
      NewProperty:=TJSIProperty.Create;
      NewProperty.JSName:=PropertyJSName;
      NewProperty.PascalName:=PropertyNode.GetAttribute('name');
      if PropertyNode.GetAttribute('enum')='1' then
        Include(NewProperty.Flags,jsipEnum);
      if PropertyNode.GetAttribute('config')='1' then
        Include(NewProperty.Flags,jsipConfig);
      if PropertyNode.GetAttribute('static')='1' then
        Include(NewProperty.Flags,jsipStatic);
      if PropertyNode.GetAttribute('default')='1' then
        Include(NewProperty.Flags,jsipDefault);
      TypeName:=PropertyNode.GetAttribute('type');
      if (jsipEnum in NewProperty.Flags) and (TypeName[1]='(') then
        TypeName:='';
      if TypeName<>'' then
        NewProperty.Typ:=CreateUnresolved(TypeName);
      JSIClass.AddProperty(NewProperty);
    end;
    Node:=Node.NextSibling;
  end;
end;

function TJavascriptIdentifierTree.CreateClass(const Path: string; CreateLast: boolean
  ): TJSIClass;
var
  p: Integer;
  StartPos: Integer;
  AName: String;
  AVLNode: TAVLTreeNode;
  Identifier: TJSIdentifier;
  Parent: TJSIClass;
  IsLast: Boolean;
  PropType: TJSIdentifier;
begin
  Result:=nil;
  p:=1;
  repeat
    StartPos:=p;
    while (p<=length(Path))  and (Path[p]<>'.') do inc(p);
    AName:=copy(Path,StartPos,p-StartPos);
    if not IsValidJSName(AName) then
      raise Exception.Create('invalid javascript class path: '+Path);
    IsLast:=p>Length(Path);
    if IsLast and not CreateLast then exit;
    // search class
    if Result=nil then begin
      AVLNode:=FindGlobal(AName);
      if AVLNode=nil then begin
        // create new global class
        debugln(['TJavascriptIdentifierTree.CreateClass new global class: ',AName]);
        Result:=TJSIClass.Create;
        Include(Result.Flags,jsicAutoCreated);
        Result.JSName:=AName;
        Globals.Add(Result);
      end else begin
        // class already exists
        if not (TObject(AVLNode.Data) is TJSIClass) then
          raise Exception.Create('path is not class: '+AName);
        Result:=TJSIClass(AVLNode.Data);
      end;
    end else begin
      Identifier:=Result.FindIdentifier(AName);
      if Identifier=nil then begin
        // create new sub class
        debugln(['TJavascriptIdentifierTree.CreateClass new sub class: ',AName,' of ',Result.JSName]);
        Parent:=Result;
        Result:=TJSIClass.Create;
        Include(Result.Flags,jsicAutoCreated);
        Result.JSName:=AName;
        Result.ParentClass:=Parent;
        Parent.AddClass(Result);
      end else if Identifier is TJSIClass then begin
        // sub class already exists
        Result:=TJSIClass(Identifier);
      end else if Identifier is TJSIProperty then begin
        // resolve property
        PropType:=TJSIProperty(Identifier).Typ;
        if PropType is TJSIClass then
          Result:=TJSIClass(PropType)
        else
          raise Exception.Create('path is not class: '+AName+' is '+Identifier.ClassName);
      end else begin
        raise Exception.Create('path is not class: '+AName+' is '+Identifier.ClassName);
      end;
    end;
    // skip point
    inc(p);
  until p>Length(Path);
end;

function TJavascriptIdentifierTree.CreateAlias(const Path: string): TJSIAlias;
var
  Context: TJSIClass;
  AVLNode: TAVLTreeNode;
  JSName: String;
begin
  Context:=CreateClass(Path,false);
  if Context<>nil then
    raise Exception.Create('nested alias not spported yet: '+Path);
  JSName:=Path;
  AVLNode:=FindGlobal(JSName);
  if AVLNode=nil then begin
    // create new alias
    debugln(['TJavascriptIdentifierTree.CreateAlias new alias: ',JSName]);
    Result:=TJSIAlias.Create;
    Result.JSName:=JSName;
  end else begin
    // alias already exists
    if not (TObject(AVLNode.Data) is TJSIAlias) then
      raise Exception.Create('path not an alias: '+Path);
    Result:=TJSIAlias(AVLNode.Data);
  end;
end;

function TJavascriptIdentifierTree.FindGlobal(const aJSName: string
  ): TAVLTreeNode;
begin
  Result:=Globals.FindKey(Pointer(aJSName),@CompareJSNameWithJSIdentifier);
end;

function TJavascriptIdentifierTree.CreateUnresolved(const aPath: string
  ): TJSIUnresolvedIdentifier;
begin
  if not IsValidJSName(aPath) then
    raise Exception.Create('invalid type name '+aPath);
  Result:=TJSIUnresolvedIdentifier.Create;
  Result.JSName:=aPath;
end;

constructor TJavascriptIdentifierTree.Create;
begin
  inherited Create;
  Globals:=TAVLTree.Create(@CompareJSIdentifiers);
end;

destructor TJavascriptIdentifierTree.Destroy;
begin
  ClearGlobals;
  FreeAndNil(Globals);
  inherited Destroy;
end;

procedure TJavascriptIdentifierTree.LoadFromFile(Filename: string);
var
  Doc: TXMLDocument;
begin
  debugln(['TJavascriptIdentifierTree.LoadFromFile ',Filename]);
  ClearNodes;
  Doc:=nil;
  try
    ReadXMLFile(Doc,Filename);
    if (Doc.DocumentElement<>nil) and (Doc.DocumentElement.NodeName='ExtJSClasses') then
      ReadExtJSNodes(Doc.DocumentElement)
    else
      raise Exception.Create('ExtJSClasses not found in file '+Filename);
  finally
    Doc.Free;
  end;
end;

procedure TJavascriptIdentifierTree.ClearGlobals;
begin
  Globals.FreeAndClear;
end;

{ TJSIMethod }

destructor TJSIMethod.Destroy;
begin
  ClearParams;
  inherited Destroy;
end;

procedure TJSIMethod.ClearParams;
var
  i: Integer;
begin
  if Params<>nil then
    for i:=0 to Params.Count-1 do TObject(Params[i]).Free;
  FreeAndNil(Params);
end;

procedure TJSIMethod.AddParameter(aParam: TJSIParameter);
begin
  if Params=nil then
    Params:=TFPList.Create;
  Params.Add(aParam);
end;

{ TJSIClass }

destructor TJSIClass.Destroy;
begin
  ClearClasses;
  ClearMethods;
  ClearProperties;
  inherited Destroy;
end;

procedure TJSIClass.ClearMethods;
var
  i: Integer;
begin
  if Methods<>nil then
    for i:=0 to Methods.Count-1 do TObject(Methods[i]).Free;
  FreeAndNil(Methods);
end;

procedure TJSIClass.ClearProperties;
var
  i: Integer;
begin
  if Properties<>nil then
    for i:=0 to Properties.Count-1 do TObject(Properties[i]).Free;
  FreeAndNil(Properties);
end;

procedure TJSIClass.ClearClasses;
var
  i: Integer;
begin
  if Classes<>nil then
    for i:=0 to Classes.Count-1 do TObject(Classes[i]).Free;
  FreeAndNil(Classes);
end;

function TJSIClass.FindIdentifier(const AJSName: string): TJSIdentifier;

  function Find(List: TFPList): TJSIdentifier;
  var
    i: Integer;
  begin
    if List=nil then exit(nil);
    for i:=0 to List.Count-1 do begin
      Result:=TJSIdentifier(List[i]);
      if CompareStr(AJSName,Result.JSName)=0 then exit;
    end;
    Result:=nil;
  end;

begin
  Result:=Find(Classes);
  if Result<>nil then exit;
  Result:=Find(Properties);
  if Result<>nil then exit;
  Result:=Find(Methods);
  if Result<>nil then exit;
end;

procedure TJSIClass.AddClass(AClass: TJSIClass);
begin
  if Classes=nil then
    Classes:=TFPList.Create;
  Classes.Add(AClass);
end;

procedure TJSIClass.AddMethod(AMethod: TJSIMethod);
begin
  if Methods=nil then
    Methods:=TFPList.Create;
  Methods.Add(AMethod);
end;

procedure TJSIClass.AddProperty(AProperty: TJSIProperty);
begin
  if Properties=nil then
    Properties:=TFPList.Create;
  Properties.Add(AProperty);
end;

end.

