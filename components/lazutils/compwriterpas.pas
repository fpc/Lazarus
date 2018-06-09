{
 *****************************************************************************
  This file is part of the Lazarus Component Library (LCL)

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************

Component serialisation into Pascal.

Author: Mattias Gaertner

Working:
- signature begin, end, version
- boolean, set of boolean
- char, widechar, custom char, set of custom char
- integers, custom int, set of custom int
- strings, codepage system and UTF8
- float, currency
- enum, custom enum range
- set of enum, set of custom enum range
- variant: integers, boolean, string, floats, currency
- method
- persistent
- component children, use SetParentComponent or optional Parent:=
- collection
- IInterfaceComponentReference
- with ancestor
- ancestor: change ComponentIndex -> call SetChildPos
- reference foreign root, reference foreign component
- create components before setting properties to avoid having to set references
  later
- inline component, csInline, call SetInline, inherited inline, inline on inherited
- TComponent.Left/Right via DesignInfo
- DefineProperties
- RegisterDefinePropertiesPas
}

unit CompWriterPas;

{$mode objfpc}{$H+}

{off $DEFINE VerboseCompWriterPas}

interface

uses
  Classes, SysUtils, typinfo, RtlConsts, contnrs, LazLoggerBase, LazUTF8;

const
  // Component serialized as Pascal
  CSPVersion = 1;
  CSPDefaultSignature = '// Component serialized as Pascal';
  CSPDefaultSignatureBegin = CSPDefaultSignature+' - Begin';
  CSPDefaultSignatureEnd = CSPDefaultSignature+' - End';
  CSPDefaultAccessClass = 'TPasStreamAccess';
  CSPDefaultExecCustomProc = 'ExecCustomCSP';
  CSPDefaultExecCustomProcUnit = 'LazPasReadUtil';
  CSPDefaultMaxColumn = 80;
  CSPDefaultAssignOp = ':=';
  CWPSkipParentName = '-';
type
  TCompWriterPas = class;
  TCWPFindAncestorEvent = procedure(Writer: TCompWriterPas; Component: TComponent;
    const Name: string; var Ancestor, RootAncestor: TComponent) of object;
  TCWPGetMethodName = procedure(Writer: TCompWriterPas; Instance: TPersistent;
    PropInfo: PPropInfo; out Name: String) of object;
  TCWPGetParentPropertyEvent = procedure(Writer: TCompWriterPas;
    Component: TComponent; var PropName: string) of object;
  TCWPDefinePropertiesEvent = procedure(Writer: TCompWriterPas;
    Instance: TPersistent; const Identifier: string; var Handled: boolean) of object;

  TCWPOption = (
    cwpoNoSignature,     // do not write Begin, End signatures
    cwpoNoSelf,          // enclose in "with LookupRootname do begin"
    cwpoSetParentFirst,  // add "SetParentComponent" before setting properties, default: after
    cwpoSrcCodepageUTF8, // target unit uses $codepage utf-8, aka do not convert UTF-8 string literals
    cwpoNoWithBlocks     // do not use with-do
    );
  TCWPOptions = set of TCWPOption;

  TCWPChildrenStep = (
    cwpcsCreate,
    cwpcsProperties
  );

  { TCompWriterPas }

  TCompWriterPas = class
  private
    FAccessClass: string;
    FAncestor: TPersistent;
    FAncestorPos: Integer;
    FAncestors: TStringList;
    FAssignOp: String;
    FCurIndent: integer;
    FCurrentPos: Integer;
    FDefaultDefineProperties: CodePointer;
    FExecCustomProc: string;
    FExecCustomProcUnit: string;
    FIgnoreChildren: Boolean;
    FIndentStep: integer;
    FLineEnding: string;
    FLookupRoot: TComponent;
    FMaxColumn: integer;
    FNeedAccessClass: boolean;
    FNeededUnits: TStrings;
    FOnDefineProperties: TCWPDefinePropertiesEvent;
    FOnFindAncestor: TCWPFindAncestorEvent;
    FOnGetMethodName: TCWPGetMethodName;
    FOnGetParentProperty: TCWPGetParentPropertyEvent;
    FOnWriteMethodProperty: TWriteMethodPropertyEvent;
    FOnWriteStringProperty: TReadWriteStringPropertyEvent;
    FOptions: TCWPOptions;
    FParent: TComponent;
    FPropPath: string;
    FRoot: TComponent;
    FRootAncestor: TComponent;
    FSignatureBegin: String;
    FSignatureEnd: String;
    FStream: TStream;
    procedure AddToAncestorList(Component: TComponent);
    procedure DetermineAncestor(Component: TComponent);
    procedure SetNeededUnits(const AValue: TStrings);
    procedure SetRoot(const AValue: TComponent);
    procedure WriteComponentData(Instance: TComponent);
    procedure WriteChildren(Component: TComponent; Step: TCWPChildrenStep);
    procedure WriteProperty(Instance: TPersistent; PropInfo: PPropInfo);
    procedure WriteProperties(Instance: TPersistent);
    procedure WriteDefineProperties(Instance: TPersistent);
    procedure WriteCollection(PropName: string; Collection: TCollection);
    function ShortenFloat(s: string): string;
  public
    constructor Create(AStream: TStream);
    destructor Destroy; override;
    // stream a component:
    procedure WriteDescendant(ARoot: TComponent; AAncestor: TComponent = nil);
    // utility functions:
    procedure WriteComponentCreate(Component: TComponent);
    procedure WriteComponent(Component: TComponent);
    procedure WriteIndent;
    procedure Write(const s: string);
    procedure WriteLn;
    procedure WriteStatement(const s: string);
    procedure WriteAssign(const LHS, RHS: string);
    procedure WriteWithDo(const Expr: string);
    procedure WriteWithEnd;
    function GetComponentPath(Component: TComponent): string;
    function GetBoolLiteral(b: boolean): string;
    function GetCharLiteral(c: integer): string;
    function GetWideCharLiteral(c: integer): string;
    function GetStringLiteral(const s: string): string;
    function GetWStringLiteral(p: PWideChar; Count: integer): string;
    function GetFloatLiteral(const e: Extended): string;
    function GetCurrencyLiteral(const c: currency): string;
    function GetEnumExpr(TypeInfo: PTypeInfo; Value: integer;
      AllowOutOfRange: boolean): string;
    function GetVersionStatement: string;
    function CreatedByAncestor(Component: TComponent): boolean;
    procedure AddNeededUnit(const AnUnitName: string);
    procedure Indent;
    procedure Unindent;
    property Stream: TStream read FStream;
    property Root: TComponent read FRoot write SetRoot;
    property LookupRoot: TComponent read FLookupRoot;
    property Ancestor: TPersistent read FAncestor write FAncestor;
    property RootAncestor: TComponent read FRootAncestor write FRootAncestor;
    property Parent: TComponent read FParent;
    property OnFindAncestor: TCWPFindAncestorEvent read FOnFindAncestor write FOnFindAncestor;
    property OnGetMethodName: TCWPGetMethodName read FOnGetMethodName write FOnGetMethodName;
    property PropertyPath: string read FPropPath;
    property CurIndent: integer read FCurIndent write FCurIndent;
    property IndentStep: integer read FIndentStep write FIndentStep;
    property Options: TCWPOptions read FOptions write FOptions;
    property IgnoreChildren: Boolean read FIgnoreChildren write FIgnoreChildren;
    property OnGetParentProperty: TCWPGetParentPropertyEvent read FOnGetParentProperty write FOnGetParentProperty;
  public
    // for custom DefineProperties
    property OnWriteMethodProperty: TWriteMethodPropertyEvent read FOnWriteMethodProperty write FOnWriteMethodProperty;
    property OnWriteStringProperty: TReadWriteStringPropertyEvent read FOnWriteStringProperty write FOnWriteStringProperty;
    property OnDefineProperties: TCWPDefinePropertiesEvent read FOnDefineProperties write FOnDefineProperties;
  public
    // code snippets
    property LineEnding: string read FLineEnding write FLineEnding; // default: system.LineEnding
    property AssignOp: String read FAssignOp write FAssignOp; // default CSPDefaultAssignOp;
    property SignatureBegin: String read FSignatureBegin write FSignatureBegin; // default CSPDefaultSignatureBegin
    property SignatureEnd: String read FSignatureEnd write FSignatureEnd; // default CSPDefaultSignatureEnd
    property AccessClass: string read FAccessClass
      write FAccessClass; // classname used to access protected TComponent members like SetChildOrder
    property ExecCustomProc: string read FExecCustomProc write FExecCustomProc; // default CSPDefaultExecCustomProc
    property ExecCustomProcUnit: string read FExecCustomProcUnit write FExecCustomProcUnit; // default CSPDefaultExecCustomProcUnit
    property MaxColumn: integer read FMaxColumn write FMaxColumn default CSPDefaultMaxColumn;
  public
    // set automatically when writing
    property NeedAccessClass: boolean read FNeedAccessClass write FNeedAccessClass; // some property needed AccessClass
    property NeededUnits: TStrings read FNeededUnits write SetNeededUnits;
  end;

procedure WriteComponentToPasStream(AComponent: TComponent; AStream: TStream);

type
  TCWPDefinePropertiesProc = procedure(Sender: TCompWriterPas;
    Instance: TPersistent; const Identifier: string; var Handled: boolean);

procedure RegisterDefinePropertiesPas(aClass: TPersistentClass;
  const OnDefineProperties: TCWPDefinePropertiesProc);
procedure UnregisterDefinePropertiesPas(
  const OnDefineProperties: TCWPDefinePropertiesProc);
procedure CallDefinePropertiesPas(Writer: TCompWriterPas; Instance: TPersistent;
  const Identifier: string; var Handled: boolean);

implementation

type
  TDefinePropertiesPas = class
    BaseClass: TPersistentClass;
    Event: TCWPDefinePropertiesProc;
  end;

var
  DefinePropertiesEvents: TObjectList = nil;

procedure WriteComponentToPasStream(AComponent: TComponent; AStream: TStream);
var
  Writer: TCompWriterPas;
begin
  Writer:=TCompWriterPas.Create(AStream);
  try
    Writer.WriteDescendant(AComponent);
  finally
    Writer.Free;
  end;
end;

procedure RegisterDefinePropertiesPas(aClass: TPersistentClass;
  const OnDefineProperties: TCWPDefinePropertiesProc);
var
  i, Cnt: Integer;
  E: TDefinePropertiesPas;
begin
  if not Assigned(OnDefineProperties) then
    raise Exception.Create('');
  if not Assigned(aClass) then
    raise Exception.Create('');
  if DefinePropertiesEvents=nil then
    DefinePropertiesEvents:=TObjectList.Create(true);
  Cnt:=DefinePropertiesEvents.Count;
  i:=0;
  while i<Cnt do
  begin
    E:=TDefinePropertiesPas(DefinePropertiesEvents[i]);
    if E.BaseClass.InheritsFrom(aClass) then
      break;
    inc(Cnt);
  end;
  E:=TDefinePropertiesPas.Create;
  E.BaseClass:=aClass;
  E.Event:=OnDefineProperties;
  DefinePropertiesEvents.Insert(i,E);
end;

procedure UnregisterDefinePropertiesPas(
  const OnDefineProperties: TCWPDefinePropertiesProc);
var
  i: Integer;
  E: TDefinePropertiesPas;
begin
  for i:=DefinePropertiesEvents.Count-1 downto 0 do
  begin
    E:=TDefinePropertiesPas(DefinePropertiesEvents[i]);
    if E.Event=OnDefineProperties then
      DefinePropertiesEvents.Delete(i);
  end;
end;

procedure CallDefinePropertiesPas(Writer: TCompWriterPas;
  Instance: TPersistent; const Identifier: string; var Handled: boolean);
var
  i: Integer;
  E: TDefinePropertiesPas;
begin
  if DefinePropertiesEvents=nil then exit;
  for i:=0 to DefinePropertiesEvents.Count-1 do begin
    E:=TDefinePropertiesPas(DefinePropertiesEvents[i]);
    if not Instance.InheritsFrom(E.BaseClass) then
      continue;
    E.Event(Writer,Instance,Identifier,Handled);
    if Handled then exit;
  end;
end;

function IsValidUTF8(p: PChar): integer;
var
  c: Char;
begin
  c:=p^;
  if ord(c)<%10000000 then begin
    // regular single byte ASCII character (#0 is a character, this is Pascal ;)
    Result:=1;
  end else if ord(c)<=%11000001 then begin
    // single byte character, between valid UTF-8 encodings
    // %11000000 and %11000001 map 2 byte to #0..#128, which is invalid and used for XSS attacks
    Result:=0;
  end else if ord(c)<=%11011111 then begin
    // could be 2 byte character (%110xxxxx %10xxxxxx)
    if ((ord(p[1]) and %11000000) = %10000000) then
      Result:=2
    else
      Result:=0; // missing following bytes
  end
  else if ord(c)<=%11101111 then begin
    // could be 3 byte character (%1110xxxx %10xxxxxx %10xxxxxx)
    if ((ord(p[1]) and %11000000) = %10000000)
    and ((ord(p[2]) and %11000000) = %10000000) then begin
      if (ord(c)=%11100000) and (ord(p[1])<=%10011111) then
        Result:=0; // XSS attack: 3 bytes are mapped to the 1 or 2 byte codes
      Result:=3;
    end else
      Result:=0; // missing following bytes
  end
  else if ord(c)<=%11110111 then begin
    // could be 4 byte character (%11110xxx %10xxxxxx %10xxxxxx %10xxxxxx)
    if ((ord(p[1]) and %11000000) = %10000000)
    and ((ord(p[2]) and %11000000) = %10000000)
    and ((ord(p[3]) and %11000000) = %10000000) then begin
      if (ord(c)=%11110000) and (ord(p[1])<=%10001111) then
        Result:=0; // XSS attack: 4 bytes are mapped to the 1-3 byte codes
      Result:=4;
    end else
      Result:=0; // missing following bytes
  end
  else begin
    Result:=0;
  end;
end;

function IsValidUTF16(p: PWideChar): integer;
var
  c: WideChar;
begin
  c:=p^;
  if c<=#$DC7F then
    exit(1)
  else if c<=#$DBFF then begin
    c:=p[1];
    if (c>=#$DC00) and (c<=#$DFFF) then
      exit(2)
    else
      exit(0);
  end else if c<=#$Dfff then begin
    exit(0);
  end else
    exit(1);
end;


type
  TAccessComp = class(TComponent); // to access TComponent protected members

  { TPosComponent }

  TPosComponent = class(TObject)
    FPos: Integer;
    FComponent: TComponent;
    constructor Create(APos: Integer; AComponent: TComponent);
  end;

{ TPosComponent }

constructor TPosComponent.Create(APos: Integer; AComponent: TComponent);
begin
  FPos:=APos;
  FComponent:=AComponent;
end;

{ TCompWriterPas }

procedure TCompWriterPas.AddToAncestorList(Component: TComponent);
begin
  FAncestors.AddObject(Component.Name,TPosComponent.Create(FAncestors.Count,Component));
end;

procedure TCompWriterPas.DetermineAncestor(Component: TComponent);
var
  i : Integer;
  C: TComponent;
begin
  if Assigned(FAncestors) then
  begin
    i:=FAncestors.IndexOf(Component.Name);
    if i<0 then
    begin
      FAncestor:=nil;
      FAncestorPos:=-1;
    end
    else
      With TPosComponent(FAncestors.Objects[i]) do
      begin
        FAncestor:=FComponent;
        FAncestorPos:=FPos;
      end;
  end;
  if Assigned(FOnFindAncestor) then
    if (Ancestor=Nil) or (Ancestor is TComponent) then
    begin
      C:=TComponent(Ancestor);
      FOnFindAncestor(Self,Component,Component.Name,C,FRootAncestor);
      Ancestor:=C;
    end;
end;

procedure TCompWriterPas.SetNeededUnits(const AValue: TStrings);
begin
  if FNeededUnits=AValue then Exit;
  FNeededUnits.Assign(AValue);
end;

procedure TCompWriterPas.SetRoot(const AValue: TComponent);
begin
  FRoot:=AValue;
  FLookupRoot:=FRoot;
end;

procedure TCompWriterPas.WriteComponentData(Instance: TComponent);
var
  HasAncestor: Boolean;
  SavedPropPath: String;

  procedure WriteSetParent;
  var
    PropName: String;
  begin
    if Parent=nil then exit;
    if Instance.GetParentComponent=nil then exit;
    if CreatedByAncestor(Instance) then begin
      // ancestor creates the component
      // and descendants cannot change parent
      exit;
    end;
    PropName:='';
    if Assigned(OnGetParentProperty) then
      OnGetParentProperty(Self,Instance,PropName);
    if PropName=CWPSkipParentName then
    else if PropName<>'' then
      WriteAssign(PropertyPath+PropName,GetComponentPath(Parent))
    else begin
      NeedAccessClass:=true;
      WriteStatement(AccessClass+'(TComponent('+Instance.Name+')).SetParentComponent('+GetComponentPath(Parent)+');');
    end;
  end;

begin
  HasAncestor := Assigned(Ancestor) and ((Instance = Root) or
    (Instance.ClassType = Ancestor.ClassType));
  SavedPropPath:=FPropPath;
  try
    if Instance=LookupRoot then begin
      WriteAssign('Name',''''+Instance.Name+'''');
      WriteChildren(Instance,cwpcsCreate);
    end
    else begin
      WriteWithDo(Instance.Name);
      if cwpoNoWithBlocks in Options then
        FPropPath:=GetComponentPath(Instance)+'.';
      if not CreatedByAncestor(Instance) then
        WriteAssign(PropertyPath+'Name',''''+Instance.Name+'''');
      if cwpoSetParentFirst in Options then
        WriteSetParent;
    end;

    WriteProperties(Instance);

    if not (cwpoSetParentFirst in Options) then
      WriteSetParent;

    if not IgnoreChildren then
      WriteChildren(Instance,cwpcsProperties);
    if Instance<>LookupRoot then
      WriteWithEnd;
  finally
    FPropPath:=SavedPropPath;
  end;
  if HasAncestor and (Ancestor<>FRootAncestor)
      and (FCurrentPos<>FAncestorPos) then
  begin
    if (Parent=LookupRoot) and not (cwpoNoSelf in Options) then
      WriteStatement('SetChildOrder('+GetComponentPath(Instance)+','+IntToStr(FCurrentPos)+');')
    else begin
      NeedAccessClass:=true;
      WriteStatement(AccessClass+'(TComponent('+GetComponentPath(Parent)+')).SetChildOrder('+GetComponentPath(Instance)+','+IntToStr(FCurrentPos)+');');
    end;
  end;
  Inc(FCurrentPos);
end;

procedure TCompWriterPas.WriteChildren(Component: TComponent;
  Step: TCWPChildrenStep);
var
  SRoot, SRootA, SParent: TComponent;
  SList: TStringList;
  SPos, i, SAncestorPos: Integer;
begin
  // Write children list.
  // While writing children, the ancestor environment must be saved
  // This is recursive...
  SRoot:=FRoot;
  SRootA:=FRootAncestor;
  SList:=FAncestors;
  SPos:=FCurrentPos;
  SAncestorPos:=FAncestorPos;
  SParent:=Parent;
  try
    FAncestors:=Nil;
    FCurrentPos:=0;
    FAncestorPos:=-1;
    FParent:=Component;
    if csInline in Component.ComponentState then
      FRoot:=Component;
    if (FAncestor is TComponent) then
    begin
      FAncestors:=TStringList.Create;
      if csInline in TComponent(FAncestor).ComponentState then
        FRootAncestor := TComponent(FAncestor);
      TAccessComp(FAncestor).GetChildren(@AddToAncestorList,FRootAncestor);
      FAncestors.Sorted:=True;
    end;
    try
      case Step of
      cwpcsCreate:
        TAccessComp(Component).GetChildren(@WriteComponentCreate, FRoot);
      cwpcsProperties:
        TAccessComp(Component).GetChildren(@WriteComponent, FRoot);
      end;
    finally
      if Assigned(FAncestor) then
        for i:=0 to FAncestors.Count-1 do
          FAncestors.Objects[i].Free;
      FreeAndNil(FAncestors);
    end;
  finally
    FParent:=SParent;
    FAncestors:=SList;
    FRoot:=SRoot;
    FRootAncestor:=SRootA;
    FCurrentPos:=SPos;
    FAncestorPos:=SAncestorPos;
  end;
end;

procedure TCompWriterPas.WriteProperty(Instance: TPersistent;
  PropInfo: PPropInfo);
type
  TSet = set of 0..31;
var
  PropType, CompType: PTypeInfo;
  ObjValue, AncestorObj: TObject;
  HasAncestor, BoolValue, DefBoolValue: Boolean;
  Int32Value, DefValue: longint;
  PropName, Ident, s, StrValue, DefStrValue, Name, SavedPropPath: String;
  IntToIdentFn: TIntToIdent;
  i, j: Integer;
  Int64Value, DefInt64Value: Int64;
  FloatValue, DefFloatValue: Extended;
  MethodValue, DefMethodValue: TMethod;
  WStrValue, WDefStrValue: WideString;
  UStrValue, UDefStrValue: UnicodeString;
  VarValue, DefVarValue: tvardata;
  aTypeData: PTypeData;
  Component, AncestorComponent: TComponent;
  SavedAncestor: TPersistent;
  IntfValue, AncestorIntf: IInterface;
  CompRef: IInterfaceComponentReference;
begin
  // do not stream properties without getter
  if not Assigned(PropInfo^.GetProc) then
    exit;

  // properties without setter are only allowed, if they are csSubComponent
  PropType := PropInfo^.PropType;
  if not Assigned(PropInfo^.SetProc) then begin
    if PropType^.Kind<>tkClass then
      exit;
    ObjValue := TObject(GetObjectProp(Instance, PropInfo));
    if not (ObjValue is TComponent) or
       not (csSubComponent in TComponent(ObjValue).ComponentStyle) then
      exit;
  end;

  { Check if the ancestor can be used }
  HasAncestor := Assigned(Ancestor) and ((Instance = Root) or
    (Instance.ClassType = Ancestor.ClassType));
  PropName:=FPropPath + PropInfo^.Name;
  {$IFDEF VerboseCompWriterPas}
  debugln(['TWriter.WriteProperty PropName="',PropName,'" TypeName=',PropType^.Name,' Kind=',GetEnumName(TypeInfo(TTypeKind),ord(PropType^.Kind)),' HasAncestor=',HasAncestor]);
  {$ENDIF}

  case PropType^.Kind of
    tkInteger, tkChar, tkEnumeration, tkSet, tkWChar:
      begin
        Int32Value := GetOrdProp(Instance, PropInfo);
        if HasAncestor then
          DefValue := GetOrdProp(Ancestor, PropInfo)
        else
          DefValue := PPropInfo(PropInfo)^.Default;
        //debugln([PropInfo^.Name,', HasAncestor=',HasAncestor,', Value=',Int32Value,', Default=',DefValue]);
        if (Int32Value <> DefValue) or (DefValue=longint($80000000)) then
        begin
          case PropType^.Kind of
            tkInteger:
              begin
                // Check if this integer has a string identifier
                IntToIdentFn := FindIntToIdent(PropInfo^.PropType);
                Ident:='';
                if Assigned(IntToIdentFn) and IntToIdentFn(Int32Value, Ident) then
                  // Integer with a custom identifier
                  // ToDo: check if this is an actual Pascal constant and remember the unit
                  WriteAssign(PropName,Ident)
                else begin
                  // Integer has to be written just as number
                  case PropType^.Name of
                  'ByteBool': WriteAssign(PropName,GetBoolLiteral(ByteBool(Int32Value)));
                  'WordBool': WriteAssign(PropName,GetBoolLiteral(WordBool(Int32Value)));
                  'LongBool': WriteAssign(PropName,GetBoolLiteral(LongBool(Int32Value)));
                  else
                    aTypeData:=GetTypeData(PropInfo^.PropType);
                    if aTypeData^.MinValue>=0 then
                      WriteAssign(PropName,IntToStr(longword(Int32Value)))
                    else
                      WriteAssign(PropName,IntToStr(Int32Value));
                  end;
                end;
              end;
            tkChar:
              WriteAssign(PropName,GetCharLiteral(Int32Value));
            tkWChar:
              WriteAssign(PropName,GetWideCharLiteral(Int32Value));
            tkSet:
              begin
              s:='';
              CompType:=GetTypeData(PropType)^.CompType;
              i:=0;
              while i<32 do
              begin
                if i in TSet(Int32Value) then
                begin
                  if s<>'' then s:=s+',';
                  // ToDo: store needed unit
                  s:=s+GetEnumExpr(CompType, i,false);
                  j:=i;
                  while (i<31) and (byte(i+1) in TSet(Int32Value)) do
                    inc(i);
                  if i>j then
                    s:=s+'..'+GetEnumExpr(CompType, i,false);
                end;
                inc(i);
              end;
              WriteAssign(PropName,'['+s+']');
              end;
            tkEnumeration:
              // ToDo: store needed unit
              WriteAssign(PropName,GetEnumExpr(PropType, Int32Value,true));
          end;
        end;
      end;
    tkFloat:
      begin
        FloatValue := GetFloatProp(Instance, PropInfo);
        if HasAncestor then
          DefFloatValue := GetFloatProp(Ancestor, PropInfo)
        else
          begin
          DefValue :=PropInfo^.Default;
          DefFloatValue:=PSingle(@PropInfo^.Default)^;
          end;
        if (FloatValue<>DefFloatValue) or (DefValue=longint($80000000)) then
          WriteAssign(PropName,GetFloatLiteral(FloatValue));
      end;
    tkMethod:
      begin
        MethodValue := GetMethodProp(Instance, PropInfo);
        if HasAncestor then
          DefMethodValue := GetMethodProp(Ancestor, PropInfo)
        else begin
          DefMethodValue.Data := nil;
          DefMethodValue.Code := nil;
        end;

        //debugln(['TCompWriterPas.WriteProperty ',dbgs(MethodValue.Data),' ',dbgs(MethodValue.Code),' ',dbgs(DefMethodValue.Data),' ',dbgs(DefMethodValue.Code)]);
        if Assigned(OnGetMethodName) then
        begin
          if (MethodValue.Code <> DefMethodValue.Code) or
            (MethodValue.Data <> DefMethodValue.Data) then
          begin
            OnGetMethodName(Self,Instance,PropInfo,Ident);
            s:='';
            if HasAncestor then
              OnGetMethodName(Self,Ancestor,PropInfo,s);
            if Ident<>s then
            begin
              if Ident='' then
                WriteAssign(PropName,'nil')
              else
                // ToDo: check nameclash of Ident with current with-do block
                WriteAssign(PropName,'@'+Ident);
            end;
          end;
        end else begin
          if (MethodValue.Code <> DefMethodValue.Code) then
          begin
            if not Assigned(MethodValue.Code) then
              Ident:=''
            else
              Ident:=FLookupRoot.MethodName(MethodValue.Code);
            if Ident='' then
              WriteAssign(PropName,'nil')
            else
              // ToDo: check nameclash of Ident with current with-do block
              WriteAssign(PropName,'@'+Ident);
          end;
        end;
      end;
    tkSString, tkLString, tkAString:
      begin
        StrValue := GetStrProp(Instance, PropInfo);
        if HasAncestor then
          DefStrValue := GetStrProp(Ancestor, PropInfo)
        else
          SetLength(DefStrValue, 0);

        if StrValue <> DefStrValue then
          WriteAssign(PropName,GetStringLiteral(StrValue));
      end;
    tkWString:
      begin
        WStrValue := GetWideStrProp(Instance, PropInfo);
        if HasAncestor then
          WDefStrValue := GetWideStrProp(Ancestor, PropInfo)
        else
          WDefStrValue := '';

        if WStrValue <> WDefStrValue then
          WriteAssign(PropName,GetWStringLiteral(PWideChar(WStrValue),length(WStrValue)));
      end;
    tkUString:
      begin
        UStrValue := GetUnicodeStrProp(Instance, PropInfo);
        if HasAncestor then
          UDefStrValue := GetUnicodeStrProp(Ancestor, PropInfo)
        else
          SetLength(UDefStrValue, 0);

        if UStrValue <> UDefStrValue then
          WriteAssign(PropName,GetWStringLiteral(PWideChar(UStrValue),length(UStrValue)));
      end;
    tkVariant:
      begin
        // Ensure that a Variant manager is installed
        if not Assigned(VarClearProc) then
          raise EWriteError.Create(SErrNoVariantSupport);

        VarValue := tvardata(GetVariantProp(Instance, PropInfo));
        if HasAncestor then
          DefVarValue := tvardata(GetVariantProp(Ancestor, PropInfo))
        else
          FillChar(DefVarValue,sizeof(DefVarValue),0);

        if (CompareByte(VarValue,DefVarValue,sizeof(VarValue)) <> 0) then
          begin
            // can't use variant() typecast, pulls in variants unit
            case VarValue.vtype of
            varsmallint : WriteAssign(PropName,'SmallInt('+IntToStr(VarValue.vsmallint)+')');
            varinteger : WriteAssign(PropName,'LongInt('+IntToStr(VarValue.vinteger)+')');
            varsingle : WriteAssign(PropName,'Single('+GetFloatLiteral(VarValue.vsingle)+')');
            vardouble : WriteAssign(PropName,'Double('+GetFloatLiteral(VarValue.vdouble)+')');
            vardate : WriteAssign(PropName,'TDateTime('+GetFloatLiteral(VarValue.vdate)+')');
            varcurrency : WriteAssign(PropName,'Currency('+GetCurrencyLiteral(VarValue.vcurrency)+')');
            //varolestr : (volestr : pwidechar);
            //vardispatch : (vdispatch : pointer);
            //varerror : (verror : hresult);
            varboolean : WriteAssign(PropName,GetBoolLiteral(VarValue.vboolean));
            //varunknown : (vunknown : pointer);
            // vardecimal : ( : );
            varshortint : WriteAssign(PropName,'ShortInt('+IntToStr(VarValue.vshortint)+')');
            varbyte : WriteAssign(PropName,'Byte('+IntToStr(VarValue.vbyte)+')');
            varword : WriteAssign(PropName,'Word('+IntToStr(VarValue.vword)+')');
            varlongword : WriteAssign(PropName,'LongWord('+IntToStr(VarValue.vlongword)+')');
            varint64 : WriteAssign(PropName,'Int64('+IntToStr(VarValue.vint64)+')');
            varqword : WriteAssign(PropName,'QWord('+IntToStr(VarValue.vqword)+')');
            // duplicate: varword64
            varstring : WriteAssign(PropName,GetStringLiteral(AnsiString(VarValue.vstring)));
            //varany :  (vany : pointer);
            //vararray : (varray : pvararray);
            //varbyref : (vpointer : pointer);
            //varrecord : (vrecord : pointer;precinfo : pointer);
            else
              {$IFDEF VerboseCompWriterPas}
              debugln(['TCompWriterPas.WriteProperty Property="',PropName,'" Kind=',PropType^.Kind,' vtype=',VarValue.vtype]);
              raise EWriteError.Create('proptype not supported: '+GetEnumName(TypeInfo(PropType^.Kind),ord(PropType^.Kind))+' vtype='+dbgs(VarValue.vtype));
              {$ENDIF}
            end;
            //ToDo WriteVariant(pvariant(@VarValue)^);
          end;
      end;
    tkClass:
      begin
        ObjValue := TObject(GetObjectProp(Instance, PropInfo));
        if HasAncestor then
        begin
          AncestorObj := TObject(GetObjectProp(Ancestor, PropInfo));
          if (AncestorObj is TComponent) and
             (ObjValue is TComponent) then
          begin
            //debugln(['TWriter.WriteProperty AncestorObj=',TComponent(AncestorObj).Name,' OwnerFit=',TComponent(AncestorObj).Owner = FRootAncestor,' ',TComponent(ObjValue).Name,' OwnerFit=',TComponent(ObjValue).Owner = Root]);
            if (AncestorObj<>ObjValue) and
               (TComponent(AncestorObj).Owner = FRootAncestor) and
               (TComponent(ObjValue).Owner = Root) and
               SameText(TComponent(AncestorObj).Name,TComponent(ObjValue).Name) then
            begin
              // value is a component, and it is the same as in the ancestor
              // Note: a descendant has new instances with same names
              AncestorObj := ObjValue;
            end;
          end;
        end else
          AncestorObj := nil;

        if not Assigned(ObjValue) then
        begin
          if ObjValue <> AncestorObj then
            WriteAssign(PropName,'Nil');
        end
        else if ObjValue.InheritsFrom(TPersistent) then
        begin
          // Subcomponents are streamed the same way as persistents
          if ObjValue.InheritsFrom(TComponent)
            and ((not (csSubComponent in TComponent(ObjValue).ComponentStyle))
                 or ((TComponent(ObjValue).Owner<>Instance) and (TComponent(ObjValue).Owner<>Nil))) then
          begin
            Component := TComponent(ObjValue);
            if (ObjValue <> AncestorObj)
                and not (csTransient in Component.ComponentStyle) then
            begin
              // set property value
              Name:=GetComponentPath(Component);
              if Name='' then
                raise EWriteError.Create('cannot write property "'+DbgSName(Instance)+'.'+PropName+'"');
              WriteAssign(PropName,Name);
            end; //(ObjValue <> AncestorObj)
          end // ObjValue.InheritsFrom(TComponent)
          else
          begin
            // keep property value, set sub properties recursively with full path
            // e.g. Font.Size:=5;
            SavedAncestor := Ancestor;
            SavedPropPath := FPropPath;
            try
              FPropPath := FPropPath + PPropInfo(PropInfo)^.Name + '.';
              if HasAncestor then
                Ancestor := TPersistent(GetObjectProp(Ancestor, PropInfo));
              WriteProperties(TPersistent(ObjValue));
            finally
              Ancestor := SavedAncestor;
              FPropPath := SavedPropPath;
            end;
            if ObjValue.InheritsFrom(TCollection) then
            begin
              if (not HasAncestor) or (not CollectionsEqual(TCollection(ObjValue),
                TCollection(GetObjectProp(Ancestor, PropInfo)),Root,RootAncestor)) then
              begin
                // create collection items
                SavedPropPath := FPropPath;
                try
                  if cwpoNoWithBlocks in Options then
                    FPropPath:=PropName+'.'
                  else
                    FPropPath:='';
                  WriteCollection(PropName,TCollection(ObjValue));
                finally
                  FPropPath := SavedPropPath;
                end;
              end;
            end // TCollection
          end;
        end; // Inheritsfrom(TPersistent)
      end;
    tkInt64, tkQWord:
      begin
        Int64Value := GetInt64Prop(Instance, PropInfo);
        if HasAncestor then
          DefInt64Value := GetInt64Prop(Ancestor, PropInfo)
        else
          DefInt64Value := 0;
        if Int64Value <> DefInt64Value then
          if PropType^.Kind=tkInt64 then
            WriteAssign(PropName,IntToStr(Int64Value))
          else
            WriteAssign(PropName,IntToStr(QWord(Int64Value)));
      end;
    tkBool:
      begin
        BoolValue := GetOrdProp(Instance, PropInfo)<>0;
        if HasAncestor then
          DefBoolValue := GetOrdProp(Ancestor, PropInfo)<>0
        else
          DefBoolValue := PropInfo^.Default<>0;
        DefValue:=PropInfo^.Default;
        //debugln([PropInfo^.Name,', HasAncestor=',HasAncestor,', BoolValue=',BoolValue,', DefBoolValue=',DefBoolValue,' Default=',DefValue]);
        if (BoolValue<>DefBoolValue) or (DefValue=longint($80000000)) then
          WriteAssign(PropName,GetBoolLiteral(BoolValue));
      end;
    tkInterface:
      begin
        IntfValue := GetInterfaceProp(Instance, PropInfo);
        if not Assigned(IntfValue) then
          WriteAssign(PropName,'Nil')
        else if Supports(IntfValue, IInterfaceComponentReference, CompRef) then
        begin
          Component := CompRef.GetComponent;
          AncestorComponent := nil;
          if HasAncestor then
          begin
            AncestorIntf := GetInterfaceProp(Instance, PropInfo);
            if Supports(AncestorIntf, IInterfaceComponentReference, CompRef) then
            begin
              AncestorComponent := CompRef.GetComponent;
              if (AncestorComponent<>Component) and
                 (AncestorComponent.Owner = FRootAncestor) and
                 (Component.Owner = Root) and
                 SameText(AncestorComponent.Name,Component.Name) then
              begin
                // value is a component, and it is the same as in the ancestor
                // Note: a descendant has new instances with same names
                AncestorComponent := Component;
              end;
            end;
          end;

          if Component<>AncestorComponent then
          begin
            Name:=GetComponentPath(Component);
            if Name='' then
              raise EWriteError.Create('cannot write property "'+DbgSName(Instance)+'.'+PropName+'"');
            WriteAssign(PropName,Name);
          end;
        end else
          raise EWriteError.Create('interface property "'+PropName+'" does not support IInterfaceComponentReference');
      end;
  else
    {$IFDEF VerboseCompWriterPas}
    debugln(['TCompWriterPas.WriteProperty Property="',PropName,'" Kind=',PropType^.Kind]);
    raise EWriteError.Create('proptype not supported: '+GetEnumName(TypeInfo(PropType^.Kind),ord(PropType^.Kind)));
    {$ENDIF}
  end;
end;

procedure TCompWriterPas.WriteProperties(Instance: TPersistent);
var
  PropCount, i: integer;
  PropList: PPropList;
begin
  PropCount:=GetPropList(Instance,PropList);
  if PropCount>0 then
    try
      for i := 0 to PropCount-1 do
        if IsStoredProp(Instance,PropList^[i]) then
          WriteProperty(Instance,PropList^[i]);
    finally
      Freemem(PropList);
    end;
  WriteDefineProperties(Instance);
end;

procedure TCompWriterPas.WriteDefineProperties(Instance: TPersistent);
var
  Col: Integer;
  InLit, NeedComma: boolean;
  InstancePath: String;

  function CheckCol(aCol: integer): boolean;
  begin
    if (Col<=CurIndent+1) or (aCol<=MaxColumn) then exit(true);
    Result:=false;
    if NeedComma then
      Write(',');
    WriteLn;
    WriteIndent;
    Col:=CurIndent+1;
    NeedComma:=false;
  end;

  function GetPath: string;
  begin
    if InstancePath='' then
    begin
      if PropertyPath<>'' then
      begin
        InstancePath:=PropertyPath;
        Delete(InstancePath,length(InstancePath),1); // chomp '.'
      end
      else if Instance is TComponent then
        InstancePath:=GetComponentPath(TComponent(Instance))
      else
        InstancePath:='';
      if InstancePath='' then
        raise EWriteError.Create('cannot write DefineProperties of "'+DbgSName(Instance)+'"');
    end;
    Result:=InstancePath;
  end;

var
  HasAncestor, Handled: Boolean;
  DefValue, Value: LongInt;
  aStream: TMemoryStream;
  BinWriter: TWriter;
  s: String;
  p: PChar;
  c: Char;
  i: Integer;
begin
  InstancePath:='';

  Handled:=false;
  if Assigned(OnDefineProperties) then
  begin
    s:=GetPath;
    OnDefineProperties(Self,Instance,s,Handled);
    if Handled then exit;
  end;
  if DefinePropertiesEvents<>nil then
  begin
    s:=GetPath;
    CallDefinePropertiesPas(Self,Instance,s,Handled);
    if Handled then exit;
  end;

  if Instance is TComponent then
  begin
    HasAncestor := Assigned(Ancestor) and ((Instance = Root) or
      (Instance.ClassType = Ancestor.ClassType));
    if HasAncestor then
      DefValue := TComponent(Ancestor).DesignInfo
    else
      DefValue := 0;
    Value:=TComponent(Instance).DesignInfo;
    if Value<>DefValue then
    begin
      // Note: DesignInfo contains Left/Top. On BigEndian systems the order
      // is reversed, which is already handled in TComponent.DefineProperties
      // -> it is the same longint value on Little and BigEndian system
      s:=GetPath;
      if s<>'' then
      begin
        if SameText(s,'Self') then
          s:=''
        else
          s:=s+'.';
      end;
      WriteAssign(s + 'DesignInfo',IntToStr(Value));
    end;
  end;

  if TMethod(@TAccessComp(Instance).DefineProperties).Code<>FDefaultDefineProperties
  then begin
    // this class has overriden DefineProperties
    aStream:=TMemoryStream.Create;
    BinWriter:=TWriter.Create(aStream,1024);
    try
      BinWriter.Root:=Root;
      BinWriter.RootAncestor:=RootAncestor;
      BinWriter.Ancestor:=Ancestor;
      BinWriter.IgnoreChildren:=IgnoreChildren;
      BinWriter.OnWriteMethodProperty:=OnWriteMethodProperty;
      BinWriter.OnWriteStringProperty:=OnWriteStringProperty;
      TAccessComp(Instance).DefineProperties(BinWriter);
      BinWriter.WriteListEnd;
      FreeAndNil(BinWriter); // flush buffer to stream
      if aStream.Size>1 then
      begin
        WriteIndent;
        s:=GetPath;
        s:=ExecCustomProc+'('+s+',[';
        Write(s);
        AddNeededUnit(ExecCustomProcUnit);
        Col:=CurIndent+length(s)+1;
        Indent;
        NeedComma:=false;
        CheckCol(Col);
        InLit:=false;
        p:=PChar(aStream.Memory);
        for i:=0 to aStream.Size-1 do
        begin
          c:=p^;
          if c in [#32..#126] then
          begin
            if (not InLit) or (Col+2>MaxColumn) then
            begin
              if InLit then
                Write('''');
              CheckCol(Col+3);
              InLit:=true;
              Write('''');
              inc(Col);
            end;
            Write(c);
            inc(Col);
            NeedComma:=true;
          end else begin
            if InLit then
            begin
              Write('''');
              inc(Col);
              InLit:=false;
            end;
            s:='#'+IntToStr(ord(c));
            CheckCol(Col+length(s));
            Write(s);
            inc(Col,length(s));
            NeedComma:=true;
          end;
          inc(p);
        end;
        if InLit then
          Write('''');
        Write(']);');
        WriteLn;
        Unindent;
      end;
    finally
      BinWriter.Free;
      aStream.Free;
    end;
  end;
end;

procedure TCompWriterPas.WriteCollection(PropName: string;
  Collection: TCollection);
var
  i: Integer;
  Item: TCollectionItem;
begin
  WriteStatement(PropName+'.Clear;');
  for i:=0 to Collection.Count-1 do
  begin
    Item:=Collection.Items[i];
    WriteWithDo(Item.ClassName+'('+PropName+'.Add)');
    WriteProperties(Item);
    WriteWithEnd;
  end;
end;

function TCompWriterPas.GetComponentPath(Component: TComponent): string;
var
  Name: String;
  C: TComponent;
begin
  if Component=nil then
    Result:='Nil'
  else if Component=LookupRoot then
  begin
    if cwpoNoSelf in Options then
      Result:=LookupRoot.Name
    else
      Result:='Self';
  end
  else begin
    Name:= '';
    C:=Component;
    While (C<>Nil) do
    begin
      if (Name<>'') Then
        Name:='.'+Name;
      if C.Owner = LookupRoot then
      begin
        Name := C.Name+Name;
        if (cwpoNoWithBlocks in Options) then
        begin
          if cwpoNoSelf in Options then
            Name := C.Owner.Name+'.'+Name;
        end;
        break;
      end
      else if C = LookupRoot then
      begin
        if cwpoNoSelf in Options then
          Name := C.Name+Name
        else
          Name := 'Self'+Name;
        break;
      end else if C.Name='' then
        exit('');
      Name:=C.Name+Name;
      // ToDo: store used unit
      C:=C.Owner;
    end;
    Result:=Name;
  end;
end;

function TCompWriterPas.GetBoolLiteral(b: boolean): string;
begin
  if b then
    Result:='True'
  else
    Result:='False';
end;

function TCompWriterPas.GetCharLiteral(c: integer): string;
begin
  case c of
  32..126: Result:=''''+chr(c)+'''';
  else     Result:='#'+IntToStr(c);
  end;
end;

function TCompWriterPas.GetWideCharLiteral(c: integer): string;
begin
  case c of
  32..126:
    Result:=''''+Chr(c)+'''';
  0..31,127..255,$D800..$DFFF:
    Result:='#'+IntToStr(c);
  else
    if cwpoSrcCodepageUTF8 in Options then
      Result:=''''+UTF16ToUTF8(WideChar(c))+''''
    else
      Result:='#'+IntToStr(c);
  end;
end;

function TCompWriterPas.GetStringLiteral(const s: string): string;

  function IsSpecialChar(p: PChar): boolean;
  const
    SpecialChars = [#0..#31,#127,#255];
  begin
    Result:=(p^ in SpecialChars) or (IsValidUTF8(p)=0);
  end;

var
  InLit: Boolean;
  p, StartP: PChar;
  c: Char;
begin
  Result:='';
  if s='' then exit;
  InLit:=false;
  p:=PChar(s);
  repeat
    c:=p^;
    if (c=#0) and (p-PChar(s)=length(s)) then
      break
    else if IsSpecialChar(p) then
    begin
      if InLit then begin
        InLit:=false;
        Result:=Result+'''';
      end;
      Result:=Result+'#'+IntToStr(ord(c));
      inc(p);
    end else begin
      if not InLit then begin
        InLit:=true;
        Result:=Result+'''';
      end;
      if c='''' then begin
        Result:=Result+'''''';
        inc(p);
      end else begin
        StartP:=p;
        repeat
          inc(p,IsValidUTF8(p));
          c:=p^;
        until ((c=#0) and (p-PChar(s)=length(s))) or IsSpecialChar(p) or (c='''');
        Result:=Result+copy(s,StartP-PChar(s)+1,p-StartP);
      end;
    end;
  until false;
  if InLit then
    Result:=Result+'''';
end;

function TCompWriterPas.GetWStringLiteral(p: PWideChar; Count: integer): string;

  function IsSpecialChar(w: PWideChar): boolean;
  const
    SpecialChars = [#0..#31,#127];
  begin
    if w^ in SpecialChars then exit(true);
    if cwpoSrcCodepageUTF8 in FOptions then begin
      Result:=IsValidUTF16(w)=0;
    end else begin
      Result:=w^>=#$7f;
    end;
  end;

var
  InLit: Boolean;
  c: WideChar;
  FirstP, StartP: PWideChar;
  AddLen: SizeUInt;
  s: string;
  OldLen: Integer;
begin
  Result:='';
  if Count=0 then exit;
  FirstP:=p;
  InLit:=false;
  s:='';
  repeat
    c:=p^;
    if (c=#0) and (p-FirstP=Count) then
      break
    else if IsSpecialChar(p) then
    begin
      if InLit then begin
        InLit:=false;
        Result:=Result+'''';
      end;
      Result:=Result+'#'+Format('%.4d',[ord(c)]);
      inc(p);
    end else begin
      if not InLit then begin
        InLit:=true;
        Result:=Result+'''';
      end;
      if c='''' then begin
        Result:=Result+'''''';
        inc(p);
      end else begin
        StartP:=p;
        repeat
          inc(p,IsValidUTF16(p));
          c:=p^;
        until ((c=#0) and (p-FirstP=Count)) or IsSpecialChar(p) or (c='''');
        AddLen:=p-StartP;
        if length(s)<AddLen*3 then SetLength(s,AddLen*3);
        if ConvertUTF16ToUTF8(@s[1],length(s),StartP,AddLen,
            [toInvalidCharError,toUnfinishedCharError],AddLen)=trNoError then
          dec(AddLen); // omit #0
        OldLen:=length(Result);
        SetLength(Result,OldLen+AddLen);
        System.Move(s[1],Result[OldLen+1],AddLen);
      end;
    end;
  until false;
  if InLit then
    Result:=Result+'''';
end;

function TCompWriterPas.GetFloatLiteral(const e: Extended): string;
var
  s: String;
begin
  s:='';
  str(e,s);
  Result:=ShortenFloat(s);
end;

function TCompWriterPas.GetCurrencyLiteral(const c: currency): string;
var
  i: int64 absolute c;
var
  s: String;
begin
  if i mod 10000=0 then
    s:=IntToStr(i div 10000)
  else begin
    s:=IntToStr(i);
    while length(s)<4 do
      s:='0'+s;
    if length(s)=4 then
      s:='0.'+s
    else
      system.insert('.',s,length(s)-3);
  end;
  Result:=s;
end;

function TCompWriterPas.ShortenFloat(s: string): string;
var
  p, i: SizeInt;
begin
  // remove unneeded leading 0 of exponent
  p:=Pos('E',s);
  if p<1 then exit(s);
  i:=p;
  if s[i+1]='+' then inc(i);
  while (i<length(s)) and (s[i+1]='0') do
    inc(i);
  if i>p then
    if i=length(s) then
      Delete(s,p,i-p+1) // delete whole exponent
    else
      Delete(s,p+1,i-p);
  // remove trailing 0 of base
  i:=p;
  while (i>2) and (s[i-1]='0') do
    dec(i);
  if not (s[i-1] in ['0'..'9']) then inc(i);
  if i<p then
    Delete(s,i,p-i);
  // remove leading space
  if s[1]=' ' then
    Delete(s,1,1);
  Result:=s;
end;

function TCompWriterPas.GetEnumExpr(TypeInfo: PTypeInfo; Value: integer;
  AllowOutOfRange: boolean): string;
var
  PT: PTypeData;
begin
  PT:=GetTypeData(TypeInfo);
  if (Value>=PT^.MinValue) and (Value<=PT^.MaxValue) then
    case TypeInfo^.Kind of
    tkBool: Result:=GetBoolLiteral(Value=ord(true));
    tkChar: Result:=GetCharLiteral(Value);
    tkEnumeration: Result:=GetEnumName(TypeInfo,Value);
    else Result:=IntToStr(Value);
    end
  else if AllowOutOfRange then
    Result:=TypeInfo^.Name+'('+IntToStr(Value)+')'
  else
    raise EWriteError.Create('enum '+IntToStr(Value)+' is out of range of type "'+TypeInfo^.Name+'"');
end;

function TCompWriterPas.GetVersionStatement: string;
begin
  Result:='// Format version '+IntToStr(CSPVersion);
end;

constructor TCompWriterPas.Create(AStream: TStream);
var
  C: TAccessComp;
begin
  FIndentStep:=2;
  FStream:=AStream;
  FLineEnding:=system.LineEnding;
  FAssignOp:=CSPDefaultAssignOp;
  FSignatureBegin:=CSPDefaultSignatureBegin;
  FSignatureEnd:=CSPDefaultSignatureEnd;
  FMaxColumn:=CSPDefaultMaxColumn;
  FExecCustomProc:=CSPDefaultExecCustomProc;
  FExecCustomProcUnit:=CSPDefaultExecCustomProcUnit;
  FNeededUnits:=TStringList.Create;
  FAccessClass:=CSPDefaultAccessClass;
  C:=TAccessComp.Create(nil);
  FDefaultDefineProperties:=TMethod(@C.DefineProperties).Code;
  C.Free;
end;

destructor TCompWriterPas.Destroy;
begin
  FreeAndNil(FNeededUnits);
  inherited Destroy;
end;

procedure TCompWriterPas.WriteComponentCreate(Component: TComponent);
var
  OldAncestor: TPersistent;
  OldRoot, OldRootAncestor: TComponent;
  HasAncestor: boolean;
begin
  if (Component=LookupRoot) then exit;
  OldRoot:=FRoot;
  OldAncestor:=FAncestor;
  OldRootAncestor:=FRootAncestor;
  Try
    DetermineAncestor(Component);
    HasAncestor:=FAncestor is TComponent;
    if not CreatedByAncestor(Component) then
      WriteAssign(Component.Name,Component.ClassName+'.Create('+GetComponentPath(Root)+')');
    if HasAncestor then begin
      if (csInline in Component.ComponentState)
      and not (csInline in TComponent(Ancestor).ComponentState) then
      begin
        NeedAccessClass:=true;
        WriteStatement(AccessClass+'(TComponent('+Component.Name+')).SetInline('+GetBoolLiteral(true)+');');
      end;
      if (csAncestor in Component.ComponentState)
      and not (csAncestor in TComponent(Ancestor).ComponentState) then
      begin
        NeedAccessClass:=true;
        WriteStatement(AccessClass+'(TComponent('+Component.Name+')).SetAncestor('+GetBoolLiteral(true)+');');
      end;
    end;
    if not IgnoreChildren then
      WriteChildren(Component,cwpcsCreate);
  finally
    FAncestor:=OldAncestor;
    FRoot:=OldRoot;
    FRootAncestor:=OldRootAncestor;
  end;
end;

procedure TCompWriterPas.WriteComponent(Component: TComponent);
var
  OldAncestor : TPersistent;
  OldRoot, OldRootAncestor : TComponent;
begin
  OldRoot:=FRoot;
  OldAncestor:=FAncestor;
  OldRootAncestor:=FRootAncestor;
  Try
    // Component.ComponentState:=Component.FComponentState+[csWriting];
    DetermineAncestor(Component);
    WriteComponentData(Component);
  finally
    FAncestor:=OldAncestor;
    FRoot:=OldRoot;
    FRootAncestor:=OldRootAncestor;
  end;
end;

procedure TCompWriterPas.WriteDescendant(ARoot: TComponent; AAncestor: TComponent);
begin
  FRoot := ARoot;
  FAncestor := AAncestor;
  FRootAncestor := AAncestor;
  FLookupRoot := ARoot;
  FNeedAccessClass := false;
  if not (cwpoNoSignature in Options) then
    WriteStatement(SignatureBegin);
  WriteStatement(GetVersionStatement);
  if cwpoNoSelf in Options then
    WriteWithDo(ARoot.Name);
  WriteComponent(ARoot);
  if cwpoNoSelf in Options then
    WriteWithEnd;
  if not (cwpoNoSignature in Options) then
    WriteStatement(SignatureEnd);
end;

procedure TCompWriterPas.WriteIndent;
begin
  Write(StringOfChar(' ',CurIndent));
end;

procedure TCompWriterPas.Write(const s: string);
begin
  if s='' then exit;
  FStream.Write(s[1],length(s));
end;

procedure TCompWriterPas.WriteLn;
begin
  Write(LineEnding);
end;

procedure TCompWriterPas.WriteStatement(const s: string);
begin
  WriteIndent;
  Write(s);
  WriteLn;
end;

procedure TCompWriterPas.WriteAssign(const LHS, RHS: string);
begin
  WriteIndent;
  Write(LHS);
  Write(AssignOp);
  Write(RHS);
  Write(';');
  WriteLn;
end;

procedure TCompWriterPas.WriteWithDo(const Expr: string);
begin
  if not (cwpoNoWithBlocks in Options) then
    WriteStatement('with '+Expr+' do begin');
  Indent;
end;

procedure TCompWriterPas.WriteWithEnd;
begin
  Unindent;
  if not (cwpoNoWithBlocks in Options) then
    WriteStatement('end;');
end;

function TCompWriterPas.CreatedByAncestor(Component: TComponent): boolean;
begin
  Result:=(FAncestor is TComponent)
    and (TComponent(FAncestor).Owner = FRootAncestor)
    and (Component.Owner = Root)
    and SameText(Component.Name,TComponent(FAncestor).Name)
end;

procedure TCompWriterPas.AddNeededUnit(const AnUnitName: string);
begin
  if FNeededUnits.IndexOf(AnUnitName)>=0 then exit;
  FNeededUnits.Add(AnUnitName);
end;

procedure TCompWriterPas.Indent;
begin
  CurIndent:=CurIndent+IndentStep;
end;

procedure TCompWriterPas.Unindent;
begin
  CurIndent:=CurIndent-IndentStep;
end;

finalization
  DefinePropertiesEvents.Free;

end.

