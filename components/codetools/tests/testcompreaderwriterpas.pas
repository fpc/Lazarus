{
 Test with:
     ./runtests --format=plain --suite=TTestCompReaderWriterPas
     ./runtests --format=plain --suite=TTestCompReaderWriterPas.TestWriteProperties

ToDo:
- root properties
- base types
- UTF-8 string
- unicodestring
- enum
- set
- variant
- datetime
- TComponent.Left/Right
- custom range TColor
- subcomponents
- cycle in subcomponents
- ancestor
- childpos
- inline component
- collection
- DefineProperty
}
unit TestCompReaderWriterPas;

{$mode objfpc}{$H+}

{$DEFINE VerboseCompWriterPas}

interface

uses
  Classes, SysUtils, typinfo, RtlConsts, LazLoggerBase, fpcunit,
  testregistry, CodeToolManager, LinkScanner, TestStdCodetools;

type
  TDummyComp = class(TComponent); // to access TComponent protected members
  TCWPFindAncestorEvent = procedure(Sender: TObject; Component: TComponent;
    const Name: string; var Ancestor, RootAncestor: TComponent) of object;
  TCWPGetMethodName = procedure(Sender: TObject; Instance: TPersistent;
    PropInfo: PPropInfo; out Name: String) of object;

  TCWPOption = (
    cwpoNoSignature,
    cwpoSetParentFirst,  // add "Parent:=" before properties
    cwpoWideStringAsUTF8
    );
  TCWPOptions = set of TCWPOption;

  { TCompWriterPas }

  TCompWriterPas = class
  private
    FAssignOp: String;
    FCurIndent: integer;
    FIndentStep: integer;
    FLineEnding: string;
    FOnGetMethodName: TCWPGetMethodName;
    FOptions: TCWPOptions;
    FParent: TComponent;
    FPropPath: string;
    FSignature: String;
    FStream: TStream;
    FRoot: TComponent;
    FLookupRoot: TComponent;
    FAncestor: TPersistent;
    FRootAncestor: TComponent;
    FAncestors: TStringList;
    FAncestorPos: Integer;
    //FCurrentPos: Integer;
    FOnFindAncestor: TCWPFindAncestorEvent;
    procedure DetermineAncestor(Component: TComponent);
    procedure DoFindAncestor(Component: TComponent);
    procedure SetRoot(const AValue: TComponent);
    procedure WriteComponentData(Instance: TComponent);
    procedure WriteProperty(Instance: TPersistent; PropInfo: PPropInfo);
    procedure WriteProperties(Instance: TComponent);
    function GetStringLiteral(const s: string): string;
  public
    constructor Create(AStream: TStream);
    destructor Destroy; override;
    procedure WriteComponent(Component: TComponent);
    procedure WriteDescendant(ARoot: TComponent; AAncestor: TComponent = nil);
    procedure WriteSignature;
    procedure WriteIndent;
    procedure Write(const s: string);
    procedure WriteLn;
    procedure WriteAssign(const LHS, RHS: string);
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
  public
    // code snippets
    property LineEnding: string read FLineEnding write FLineEnding;
    property AssignOp: String read FAssignOp write FAssignOp;
    property Signature: String read FSignature write FSignature;
  end;

type
  TEnum = (red, green, blue, white, black);
  TEnumRg = green..white;
  TSetOfEnum = set of TEnum;
  TSetOfEnumRg = set of TEnumRg;

  { TCompBaseTypes }

  TCompBaseTypes = class(TComponent)
  private
    FABoolean: Boolean;
    FAByte: Byte;
    FAByteBool: ByteBool;
    FAChar: Char;
    FACurrency: Currency;
    FADouble: Double;
    FAExtended: Extended;
    FAInt64: Int64;
    FALongBool: LongBool;
    FALongInt: LongInt;
    FALongWord: LongWord;
    FAQWord: QWord;
    FAShortInt: ShortInt;
    FAShortString: ShortString;
    FASingle: Single;
    FASmallInt: SmallInt;
    FAString: String;
    FAUnicodeString: UnicodeString;
    FAWideChar: WideChar;
    FAWideString: WideString;
    FAWord: Word;
    FAWordBool: WordBool;
    FEnum: TEnum;
    FEnumRg: TEnumRg;
    FSetOfEnum: TSetOfEnum;
    FSetOfEnumRg: TSetOfEnumRg;
    function isACurrencyStored: Boolean;
    function isADoubleStored: Boolean;
    function isAExtendedStored: Boolean;
    function isAShortStringStored: Boolean;
    function isASingleStored: Boolean;
    function isAStringStored: Boolean;
    function isAUnicodeStringStored: Boolean;
    function isAWideStringStored: Boolean;
  published
    constructor Create(AOwner: TComponent); override;
    property ABoolean: Boolean read FABoolean write FABoolean default false;
  public
    property AByteBool: ByteBool read FAByteBool write FAByteBool default false;
    property AWordBool: WordBool read FAWordBool write FAWordBool default false;
    property ALongBool: LongBool read FALongBool write FALongBool default false;
    property AByte: Byte read FAByte write FAByte default 0;
    property AShortInt: ShortInt read FAShortInt write FAShortInt default 0;
    property AWord: Word read FAWord write FAWord default 0;
    property ASmallInt: SmallInt read FASmallInt write FASmallInt default 0;
    property ALongWord: LongWord read FALongWord write FALongWord default 0;
    property ALongInt: LongInt read FALongInt write FALongInt default 0;
    property AQWord: QWord read FAQWord write FAQWord default 0;
    property AInt64: Int64 read FAInt64 write FAInt64 default 0;
    property ACurrency: Currency read FACurrency write FACurrency stored isACurrencyStored;
    property ASingle: Single read FASingle write FASingle stored isASingleStored;
    property ADouble: Double read FADouble write FADouble stored isADoubleStored;
    property AExtended: Extended read FAExtended write FAExtended stored isAExtendedStored;
    property AChar: Char read FAChar write FAChar default #0;
    property AWideChar: WideChar read FAWideChar write FAWideChar default #0;
    property AString: String read FAString write FAString stored isAStringStored;
    property AShortString: ShortString read FAShortString write FAShortString stored isAShortStringStored;
    property AWideString: WideString read FAWideString write FAWideString stored isAWideStringStored;
    property AUnicodeString: UnicodeString read FAUnicodeString write FAUnicodeString stored isAUnicodeStringStored;
    property Enum: TEnum read FEnum write FEnum default low(TEnum);
    property EnumRg: TEnumRg read FEnumRg write FEnumRg default low(TEnumRg);
    property SetOfEnum: TSetOfEnum read FSetOfEnum write FSetOfEnum default [];
    property SetOfEnumRg: TSetOfEnumRg read FSetOfEnumRg write FSetOfEnumRg default [];
  end;

  { TTestCompReaderWriterPas }

  TTestCompReaderWriterPas = class(TCustomTestCTStdCodetools)
  published
    procedure TestWriteProperties;
  end;

implementation

Type

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

procedure TCompWriterPas.DetermineAncestor(Component: TComponent);
var
  i : Integer;
begin
  if not Assigned(FAncestors) then
    exit;
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

procedure TCompWriterPas.DoFindAncestor(Component: TComponent);
var
  C: TComponent;
begin
  if Assigned(FOnFindAncestor) then
    if (Ancestor=Nil) or (Ancestor is TComponent) then
    begin
      C:=TComponent(Ancestor);
      FOnFindAncestor(Self,Component,Component.Name,C,FRootAncestor);
      Ancestor:=C;
    end;
end;

procedure TCompWriterPas.SetRoot(const AValue: TComponent);
begin
  FRoot:=AValue;
  FLookupRoot:=FRoot;
end;

procedure TCompWriterPas.WriteComponentData(Instance: TComponent);

  procedure WriteSetParent;
  begin
    if Parent=nil then exit;
    WriteAssign('Parent',Parent.Name);
  end;

begin
  if Instance<>LookupRoot then
  begin
    WriteIndent;
    Write('with ');
    Write(Instance.Name);
    Write(' do begin');
    WriteLn;
    Indent;
  end;
  WriteAssign('Name',''''+Instance.Name+'''');
  if cwpoSetParentFirst in Options then
    WriteSetParent;
  WriteProperties(Instance);
  if not (cwpoSetParentFirst in Options) then
    WriteSetParent;
  if Instance<>LookupRoot then
  begin
    Unindent;
    WriteIndent;
    Write('end;');
    WriteLn;
  end;
end;

procedure TCompWriterPas.WriteProperty(Instance: TPersistent;
  PropInfo: PPropInfo);
type
  TSet = set of 0..31;
var
  PropType, CompType: PTypeInfo;
  ObjValue: TObject;
  HasAncestor, BoolValue, DefBoolValue: Boolean;
  Int32Value, DefValue: longint;
  PropName, Ident, s, StrValue, DefStrValue: String;
  IntToIdentFn: TIntToIdent;
  i, j: Integer;
  Int64Value, DefInt64Value: Int64;
  FloatValue, DefFloatValue: Extended;
  MethodValue, DefMethodValue: TMethod;
  WStrValue, WDefStrValue: WideString;
  UStrValue, UDefStrValue: UnicodeString;
  VarValue, DefVarValue: tvardata;
begin
  // do not stream properties without getter
  if not Assigned(PropInfo^.GetProc) then
    exit;
  // properties without setter are only allowed, if they are subcomponents
  PropType := PropInfo^.PropType;
  if not Assigned(PropInfo^.SetProc) then begin
    if PropType^.Kind<>tkClass then
      exit;
    ObjValue := TObject(GetObjectProp(Instance, PropInfo));
    if not ObjValue.InheritsFrom(TComponent) or
       not (csSubComponent in TComponent(ObjValue).ComponentStyle) then
      exit;
  end;

  { Check if the ancestor can be used }
  HasAncestor := Assigned(Ancestor) and ((Instance = Root) or
    (Instance.ClassType = Ancestor.ClassType));
  System.writeln('TWriter.WriteProperty Name=',PropType^.Name,' Kind=',GetEnumName(TypeInfo(TTypeKind),ord(PropType^.Kind)),' HasAncestor=',HasAncestor);

  PropName:=FPropPath + PropInfo^.Name;
  case PropType^.Kind of
    tkInteger, tkChar, tkEnumeration, tkSet, tkWChar:
      begin
        Int32Value := GetOrdProp(Instance, PropInfo);
        if HasAncestor then
          DefValue := GetOrdProp(Ancestor, PropInfo)
        else
          DefValue := PPropInfo(PropInfo)^.Default;
        //System.writeln(PropInfo^.Name,', HasAncestor=',HasAncestor,', Value=',Int32Value,', Default=',DefValue);
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
                else
                  // Integer has to be written just as number
                  WriteAssign(PropName,IntToStr(Int32Value));
              end;
            tkChar:
              case Int32Value of
              0..31,127..255:
                WriteAssign(PropName,'#'+IntToStr(Int32Value))
              else
                WriteAssign(PropName,''''+chr(Int32Value)+'''');
              end;
            tkWChar:
              case Int32Value of
              32..126:
                WriteAssign(PropName,''''+Chr(Int32Value)+'''');
              0..31,127..255,$D800..$DFFF:
                WriteAssign(PropName,'#'+IntToStr(Int32Value));
              else
                if cwpoWideStringAsUTF8 in Options then
                  WriteAssign(PropName,''''+UTF8Encode(WideChar(Int32Value))+'''')
                else
                  WriteAssign(PropName,'#'+IntToStr(Int32Value));
              end;
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
                  s:=s+GetEnumName(CompType, i);
                  j:=i;
                  while (i<31) and (byte(i+1) in TSet(Int32Value)) do
                    inc(i);
                  if i>j then
                    s:=s+'..'+GetEnumName(CompType, i);
                end;
                inc(i);
              end;
              WriteAssign(PropName,'['+s+']');
              end;
            tkEnumeration:
              // ToDo: store needed unit
              WriteAssign(PropName,GetEnumName(PropType, Int32Value));
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
        begin
          s:='';
          str(FloatValue,s);
          WriteAssign(PropName,s);
        end;
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

        if Assigned(OnGetMethodName) then
        begin
          if (MethodValue.Code <> DefMethodValue.Code) or
            (MethodValue.Data <> DefMethodValue.Data) then
          begin
            OnGetMethodName(Self,Instance,PropInfo,Ident);
            OnGetMethodName(Self,Ancestor,PropInfo,s);
            if Ident<>s then
            begin
              if Ident='' then
                WriteAssign(PropName,'nil')
              else
                WriteAssign(PropName,'@'+Ident);
            end;
          end;
        end else begin
          if (MethodValue.Code <> DefMethodValue.Code) then
          begin
            if not Assigned(MethodValue.Code) then
              s:=''
            else
              s:=FLookupRoot.MethodName(MethodValue.Code);
            if s='' then
              WriteAssign(PropName,'nil')
            else
              WriteAssign(PropName,'@'+s);
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

        if WStrValue <> WDefStrValue then begin
          {$IFDEF VerboseCompWriterPas}
          System.writeln('TCompWriterPas.WriteProperty Property="',PropName,'" Kind=',PropType^.Kind);
          raise EWriteError.Create('proptype not supported: '+GetEnumName(TypeInfo(PropType^.Kind),ord(PropType^.Kind)));
          {$ENDIF}
          //ToDo: WriteAssign(PropName,GetWStringLiteral(WStrValue));
        end;
      end;
    tkUString:
      begin
        UStrValue := GetUnicodeStrProp(Instance, PropInfo);
        if HasAncestor then
          UDefStrValue := GetUnicodeStrProp(Ancestor, PropInfo)
        else
          SetLength(UDefStrValue, 0);

        if UStrValue <> UDefStrValue then begin
          {$IFDEF VerboseCompWriterPas}
          System.writeln('TCompWriterPas.WriteProperty Property="',PropName,'" Kind=',PropType^.Kind);
          raise EWriteError.Create('proptype not supported: '+GetEnumName(TypeInfo(PropType^.Kind),ord(PropType^.Kind)));
          {$ENDIF}
          // ToDo: WriteAssign(PropName,GetWStringLiteral(UStrValue));
        end;
      end;
    tkVariant:
      begin
        { Ensure that a Variant manager is installed }
        if not assigned(VarClearProc) then
          raise EWriteError.Create(SErrNoVariantSupport);

        VarValue := tvardata(GetVariantProp(Instance, PropInfo));
        if HasAncestor then
          DefVarValue := tvardata(GetVariantProp(Ancestor, PropInfo))
        else
          FillChar(DefVarValue,sizeof(DefVarValue),0);

        if (CompareByte(VarValue,DefVarValue,sizeof(VarValue)) <> 0) then
          begin
            {$IFDEF VerboseCompWriterPas}
            System.writeln('TCompWriterPas.WriteProperty Property="',PropName,'" Kind=',PropType^.Kind);
            raise EWriteError.Create('proptype not supported: '+GetEnumName(TypeInfo(PropType^.Kind),ord(PropType^.Kind)));
            {$ENDIF}
            { can't use variant() typecast, pulls in variants unit }
            //ToDo WriteVariant(pvariant(@VarValue)^);
          end;
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
        //System.writeln(PropInfo^.Name,', HasAncestor=',HasAncestor,', BoolValue=',BoolValue,', DefBoolValue=',DefBoolValue,' Default=',DefValue);
        if (BoolValue<>DefBoolValue) or (DefValue=longint($80000000)) then
          WriteAssign(PropName,BoolToStr(BoolValue,'True','False'));
      end;
  else
    {$IFDEF VerboseCompWriterPas}
    System.writeln('TCompWriterPas.WriteProperty Property="',PropName,'" Kind=',PropType^.Kind);
    raise EWriteError.Create('proptype not supported: '+GetEnumName(TypeInfo(PropType^.Kind),ord(PropType^.Kind)));
    {$ENDIF}
  end;
end;

procedure TCompWriterPas.WriteProperties(Instance: TComponent);
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
  // ToDo: Instance.DefineProperties(Self);
end;

function TCompWriterPas.GetStringLiteral(const s: string): string;
const
  SpecialChars = [#0..#31,#127..#192];
var
  i, StartPos: Integer;
  InLit: Boolean;
begin
  Result:='';
  InLit:=false;
  i:=1;
  while i<=length(s) do begin
    if s[i] in SpecialChars then
    begin
      if InLit then begin
        InLit:=false;
        Result:=Result+'''';
      end;
      Result:=Result+'#'+IntToStr(ord(s[i]));
      inc(i);
    end else begin
      if not InLit then begin
        InLit:=true;
        Result:=Result+'''';
      end;
      if s[i]='''' then begin
        Result:=Result+'''''';
        inc(i);
      end else begin
        StartPos:=i;
        repeat
          inc(i);
        until (i>length(s)) or (s[i] in SpecialChars) or (s[i]='''');
        // ToDo: source codepage<>UTF-8
        Result:=Result+copy(s,StartPos,i-StartPos);
      end;
    end;
  end;
  if InLit then
    Result:=Result+'''';
end;

constructor TCompWriterPas.Create(AStream: TStream);
begin
  FIndentStep:=2;
  FStream:=AStream;
  FLineEnding:=system.LineEnding;
  FAssignOp:=':=';
  FSignature:='// Pascal writer V1.0';
end;

destructor TCompWriterPas.Destroy;
begin
  inherited Destroy;
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
    DoFindAncestor(Component); // Mainly for IDE when a parent form had an ancestor renamed...
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
  if not (cwpoNoSignature in Options) then
    WriteSignature;
  WriteComponent(ARoot);
end;

procedure TCompWriterPas.WriteSignature;
begin
  WriteIndent;
  Write(Signature);
  WriteLn;
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

procedure TCompWriterPas.WriteAssign(const LHS, RHS: string);
begin
  WriteIndent;
  Write(LHS);
  Write(AssignOp);
  Write(RHS);
  Write(';');
  WriteLn;
end;

procedure TCompWriterPas.Indent;
begin
  CurIndent:=CurIndent+IndentStep;
end;

procedure TCompWriterPas.Unindent;
begin
  CurIndent:=CurIndent-IndentStep;
end;

{ TCompBaseTypes }

function TCompBaseTypes.isACurrencyStored: Boolean;
begin
  Result:=ACurrency<>0;
end;

function TCompBaseTypes.isADoubleStored: Boolean;
begin
  Result:=ADouble<>0;
end;

function TCompBaseTypes.isAExtendedStored: Boolean;
begin
  Result:=AExtended<>0;
end;

function TCompBaseTypes.isAShortStringStored: Boolean;
begin
  Result:=AShortString<>'';
end;

function TCompBaseTypes.isASingleStored: Boolean;
begin
  Result:=ASingle<>0;
end;

function TCompBaseTypes.isAStringStored: Boolean;
begin
  Result:=AString<>'';
end;

function TCompBaseTypes.isAUnicodeStringStored: Boolean;
begin
  Result:=AUnicodeString<>'';
end;

function TCompBaseTypes.isAWideStringStored: Boolean;
begin
  Result:=AWideString<>'';
end;

constructor TCompBaseTypes.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  EnumRg:=low(TEnumRg);
end;

{ TTestCompReaderWriterPas }

procedure TTestCompReaderWriterPas.TestWriteProperties;
var
  AComponent: TCompBaseTypes;
  aStream: TMemoryStream;
  Actual: string;
  Writer: TCompWriterPas;
begin
  Writer:=nil;
  AComponent:=TCompBaseTypes.Create(nil);
  aStream:=TMemoryStream.Create;
  try
    AComponent.Name:=AComponent.ClassName+'1';
    AComponent.ABoolean:=true;
    Writer:=TCompWriterPas.Create(aStream);
    Writer.WriteDescendant(AComponent,nil);
    aStream.Position:=0;
    SetLength(Actual,aStream.size);
    aStream.Read(Actual[1],length(Actual));
    writeln('TTestCompReaderWriterPas.TestWriteProperties "',Actual,'"');
  finally
    aStream.Free;
    AComponent.Free;
  end;
end;

initialization
  RegisterTest(TTestCompReaderWriterPas);
end.

