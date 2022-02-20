unit idehtml2class;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, sax, sax_html, fpjson, pascodegen;

Type
  TSpecialMethod = (smConstructor,smBindElements,smBindElementEvents);
  TSpecialMethods = Set of TSpecialMethod;

  TFormOption = (foEvents,foFormFile,foBindInConstructor);
  TFormOptions = Set of TFormOption;


  { THTML2ClassOptions }

  THTML2ClassOptions = Class (TPersistent)
  Private
    FExcludeElements: TStrings;
    FFormOptions: TFormOptions;
    FMethods : Array[1..3] of TSpecialMethods;
    FBools : Array[1..2] of Boolean;
    FStrings : Array[1..10] of String;
    function GetB(AIndex: Integer): Boolean;
    function GetMethods(AIndex: Integer): TSpecialMethods;
    function GetS(AIndex: Integer): String;
    procedure SetB(AIndex: Integer; AValue: Boolean);
    procedure SetExcludeElements(AValue: TStrings);
    procedure SetMethods(AIndex: Integer; AValue: TSpecialMethods);
    procedure SetS(AIndex: Integer; AValue: String);
  Public
    Constructor Create;
    Destructor Destroy; override;
    Procedure Reset; virtual;
    Procedure toJSON(aObject : TJSONObject);
    Procedure FromJSON(aJSON : String);
    Procedure FromJSON(aObject : TJSONObject);
    Function asJSON(Formatted : Boolean) : String;
    Property OverrideMethods : TSpecialMethods index 1 Read GetMethods Write SetMethods;
    Property AddMethods : TSpecialMethods index 2 Read GetMethods Write SetMethods;
    Property VirtualMethods : TSpecialMethods index 3 Read GetMethods Write SetMethods;
    Property FormOptions : TFormOptions Read FFormOptions Write FFormOptions;
    Property ParentClassName : String Index 1 Read GetS Write SetS;
    Property GetElementFunction : String Index 2 Read GetS Write SetS;
    Property EventSignature : String Index 3 Read GetS Write SetS;
    Property EventModifiers : String Index 4 Read GetS Write SetS;
    Property ConstructorArgs : String Index 5 Read GetS Write SetS;
    Property BelowID : String Index 6 Read GetS Write SetS;
    Property HTMLFileName : String Index 7 Read GetS Write SetS;
    Property TagMapFileName : String Index 8 Read GetS Write SetS;
    Property FormClassname : String Index 9 Read GetS Write SetS;
    Property ExtraUnits: String index 10 Read GetS Write SetS;
    Property UseDefaultElements : Boolean Index 1 Read GetB Write SetB;
    Property AddHTMLToProject : Boolean Index 2 Read GetB Write SetB;
    Property ExcludeElements : TStrings Read FExcludeElements Write SetExcludeElements;
  end;

Type
  TLogEvent = Procedure (Sender : TObject; Const Msg : String) of object;

  { TFormElement }

  TFormElement = Class(TCollectionItem)
  private
    FHTMLID: String;
    FName: String;
    FType: String;
    FEvents : TStrings;
    function GetEvents: TStrings;
    function getName: String;
    procedure SetEvents(AValue: TStrings);
  Public
    Destructor Destroy; override;
    Function HasEvents : Boolean;
    Procedure Assign(Source : TPersistent); override;
  Published
    Property Name : String Read getName Write FName;
    Property HTMLID : String Read FHTMLID Write FHTMLID;
    Property ElementType : String Read FType Write FType;
    Property Events : TStrings Read GetEvents Write SetEvents;
  end;

  { TFormElementList }

  TFormElementList = CLass(TCollection)
  private
    function GetEl(aIndex : Integer): TFormElement;
  Public
    Function Add(Const aName : string) : TFormElement;
    Function IndexOf(Const aName : string) : Integer;
    Function Find(Const aName : string) : TFormElement;
    Property Elements[aIndex : Integer] : TFormElement Read GetEl; default;
  end;

  TAttributeOperation = (aoNotPresent,aoPresent,aoEqual,aoNotEqual,aoContains);

  { TAttributeCondition }

  TAttributeCondition = Class(TCollectionItem)
  private
    FAttribute: String;
    FOperation: TAttributeOperation;
    FValue: String;
  Public
    Procedure LoadFromJSON(aName : String; aValue: TJSONData);
    function IsMatch(aValue: String): Boolean;
    Property Attribute : String Read FAttribute Write FAttribute;
    Property Operation : TAttributeOperation Read FOperation Write FOperation;
    Property Value : String Read FValue Write FValue;
  end;

  { TAttributeConditionList }

  TAttributeConditionList = Class(TCollection)
  private
    function GetC(aIndex : Integer): TAttributeCondition;
  Public
    Procedure LoadFromJSON(aJSON : TJSONObject);
    Function IsMatch(Attrs: TSAXAttributes): Boolean;
    Property Conditions[aIndex : Integer] : TAttributeCondition Read GetC; default;
  end;

(* // Structure of accepted JSON
  [
   {
     "class" : "TWebComboBox",
     "tag" : "input",
     "attrs" : {
        name0 : null, // name0 Not present
        name1 : "value",  // name1 equals value
        name2 ; "-value", // name2 does not equal value
        name3 : "~value"  // name3 contains value
       }
   }
  ]
*)

  { THTMLElementMap }

  THTMLElementMap = Class(TCollectionItem)
  private
    FConditionList : TAttributeConditionList;
    FControlClass: String;
    FTag: String;
    function GetAttrConditionList: TAttributeConditionList;
  Protected
    Function CreateConditionList : TAttributeConditionList; virtual;
  Public
    Destructor Destroy; override;
    Procedure LoadFromJSON(aJSON : TJSONObject);
    Function HasConditions : Boolean;
    Function IsMatch(aTag: SAXString; Attrs: TSAXAttributes): Boolean;
    Property Tag : String Read FTag Write FTag;
    Property ControlClass : String Read FControlClass Write FControlClass;
    Property Attributes : TAttributeConditionList Read GetAttrConditionList;
  end;

  { THTMLElementMapList }

  THTMLElementMapList = Class(TCollection)
  private
    function GetM(aIndex : Integer): THTMLElementMap;
  Public
    Procedure LoadFromFile(Const aFileName : String);
    Procedure LoadFromStream(aStream : TStream); virtual;
    Procedure LoadFromJSON(aJSON : TJSONArray); virtual;
    Function IndexOfMap(aTag: SAXString; Attrs: TSAXAttributes): Integer;
    Function FindMap(aTag: SAXString; Attrs: TSAXAttributes): THTMLElementMap;
    Property Maps[aIndex : Integer] : THTMLElementMap Read GetM; default;
  end;


  { THTMLToFormELements }

  THTMLToFormELements = class(TComponent)
  private
    FBelowID: String;
    FDefaultElements: Boolean;
    FExcludeIDS: TStrings;
    FFormElements: TFormElementList;
    FLevel : Integer;
    FMap: THTMLElementMapList;
    FOnLog: TLogEvent;
    function MakeValidName(aID: string): string;
    procedure SetExcludeIDS(AValue: TStrings);
    procedure SetFormElements(AValue: TFormElementList);
  protected
    Procedure DoLog(Const Msg : String);
    Procedure DoLog(Const Fmt : String; Args : Array of const);
    function CreateHTMLElementMapList: THTMLElementMapList; virtual;
    procedure GetEvents(aEl: TFormElement; Atts: TSAXAttributes); virtual;
    procedure DoEndElement(Sender: TObject; const {%H-}NamespaceURI, {%H-}LocalName,
      {%H-}QName: SAXString);  virtual;
    procedure DoStartElement(Sender: TObject; const {%H-}NamespaceURI, LocalName,
      {%H-}QName: SAXString; Atts: TSAXAttributes);  virtual;
    function Maptype(aTag: SAXString; Atts: TSAXAttributes): String; virtual;
    Class Function CreateElementList : TFormElementList;  virtual;
    Property Level : Integer Read FLevel Write FLevel;
  Public
    Constructor Create(aOwner : TComponent); override;
    Destructor Destroy; override;
    Procedure Clear;
    Procedure LoadFromStream(aInput : TStream);
    Procedure LoadFromFile(Const aFileName : String);
    Procedure LoadOptions(aOptions : THTML2ClassOptions);
    Property FormElements : TFormElementList Read FFormElements Write SetFormElements;
    Property BelowID : String Read FBelowID Write FBelowID;
    Property ExcludeIDS : TStrings Read FExcludeIDS Write SetExcludeIDS;
    Property Map : THTMLElementMapList Read FMap;
    Property DefaultElements : Boolean Read FDefaultElements Write FDefaultElements;
    Property OnLog : TLogEvent Read FOnLog Write FOnLog;
  end;

  { THTMLExtractIDS }

  THTMLExtractIDS = Class(TComponent)
  Private
    FBelowID: String;
    FLevel: Integer;
    FList: TStrings;
  Protected
    procedure DoStartElement(Sender: TObject; const {%H-}NamespaceURI, LocalName,
      {%H-}QName: SAXString; Atts: TSAXAttributes);  virtual;
    procedure DoEndElement(Sender: TObject; const {%H-}NamespaceURI, {%H-}LocalName,
      {%H-}QName: SAXString);  virtual;
    Property List : TStrings Read FList;
    Property Level : Integer Read FLevel Write FLevel;
  Public
    Procedure ExtractIDS(aInput : TStream; aList : TStrings);
    Function ExtractIDS(aInput : TStream) : TStringArray;
    Procedure ExtractIDS(Const aFileName : String; aList : TStrings);
    function ExtractIDS(const aFileName: String): TStringArray;
    Property BelowID : String Read FBelowID Write FBelowID;
  end;

  { TFormCodeGen }



  { TFormFileCodeGen }

  TFormFileCodeGen = Class(TPascalCodeGenerator)
  private
    FElementHeight: Word;
    FElementHSpacing: Word;
    FElementVSpacing: Word;
    FElementWidth: Word;
    FDoEvents: Boolean;
    FFormClassName: String;
    FFormElements: TFormElementList;
    FIDProperty: String;
    FLeft: Word;
    FMaxHeight: Word;
    FMaxWidth: Word;
    FTop: Word;
  Protected
    function GetFormName(const aClassName: string): String; virtual;
    procedure GenerateElements; virtual;
    procedure EmitElementEvents(El: TFormElement); virtual;
    procedure EmitElementProps(El: TFormElement); virtual;
    procedure NextPosition; virtual;
    Property ELeft : Word Read FLeft Write FLeft;
    Property ETop : Word Read FTop Write FTop;
  Public
    Constructor Create(aOwner : TComponent);override;
    Procedure Execute;
    Property FormElements: TFormElementList read FFormElements write FFormElements;
    Property FormClassName : String read FFormClassName write FFormClassName;
    Property DoEvents : Boolean read FDoEvents write FDoEvents;
    Property IDProperty : String Read FIDProperty Write FIDProperty;
    Property ElementHeight : Word Read FElementHeight Write FElementHeight;
    Property ElementWidth : Word Read FElementWidth Write FElementWidth;
    Property MaxWidth : Word Read FMaxWidth Write FMaxWidth;
    Property MaxHeight : Word Read FMaxHeight Write FMaxHeight;
    Property ElementHSpacing : Word Read FElementHSpacing Write FElementHSpacing;
    Property ElementVSpacing : Word Read FElementVSpacing Write FElementVSpacing;
  end;

  TFormCodeGen = Class(TPascalCodeGenerator)
  private
    FAddMethods: TSpecialMethods;
    FConstructorArgs: String;
    FEventModifiers: String;
    FEventSignature: string;
    FFormClassName: string;
    FFormElements: TFormElementList;
    FFormFileGenerator: TFormFileCodeGen;
    FFormSource: Tstrings;
    FGetElementFunction: string;
    FOptions: TFormOptions;
    FOverrideMethods: TSpecialMethods;
    FParentClassName: string;
    FVirtualMethods: TSpecialMethods;
    procedure SetFormElements(AValue: TFormElementList);
  Protected
    function BaseUnits : String; override;
    Function CreateHTMLToFormELements: THTMLToFormELements;  virtual;
    Class Function CreateElementList : TFormElementList;  virtual;
    procedure EmitFormFile; virtual;
    function CreateFormFileGen : TFormFileCodeGen; virtual;
    procedure EmitFormElement(aEL: TFormElement); virtual;
    procedure EmitFormEvents(aEL: TFormElement);virtual;
    procedure EmitImplementation; virtual;
    procedure EmitPublicSection; virtual;
    procedure EmitPublishedSection; virtual;
    procedure EmitFormBindElements; virtual;
    procedure EmitFormBindEvents; virtual;
    procedure EmitFormConstructor; virtual;
    function VirtualOverride(M: TSpecialMethod; const Decl: String): string; virtual;
   Public
    Constructor Create(aOwner : TComponent); override;
    Destructor Destroy; override;
    class function Pretty(const S: String): string; virtual;
    class procedure GetEventNameAndHandler(const S,aFieldName: String; out aName, aHandler: string);
    Procedure Execute;
    Procedure LoadOptions(aOptions : THTML2ClassOptions);
    Property FormFileGenerator  : TFormFileCodeGen Read fFormFileGenerator Write FFormFileGenerator;
    Property FormElements : TFormElementList Read FFormElements Write SetFormElements;
    Property FormClassName : string Read FFormClassName Write FFormClassName;
    Property ParentClassName : string Read FParentClassName Write FParentClassName;
    Property GetElementFunction : string Read FGetElementFunction Write FGetElementFunction;
    Property EventSignature: string Read FEventSignature Write FEventSignature;
    Property EventModifiers : String Read FEventModifiers Write FEventModifiers;
    Property ConstructorArgs : String Read FConstructorArgs Write FConstructorArgs;
    Property Options : TFormOptions Read FOptions Write FOptions;
    Property AddMethods : TSpecialMethods Read  FAddMethods Write FAddMethods;
    Property OverrideMethods : TSpecialMethods Read  FOverrideMethods Write FOverrideMethods;
    Property VirtualMethods : TSpecialMethods Read  FVirtualMethods Write FVirtualMethods;
    Property FormSource : Tstrings Read FFormSource;
  end;



implementation

uses TypInfo, bufstream;

{ THTMLExtractIDS }

procedure THTMLExtractIDS.DoStartElement(Sender: TObject; const NamespaceURI, LocalName, QName: SAXString; Atts: TSAXAttributes);

Var
  aID,aType : String;
  El : TFormElement;

begin
  if Not Assigned(atts) then exit;
  aID:=UTF8Encode(Atts.GetValue('','id'));
  if (aID<>'') then
    begin
    if (Level=0) and (BelowID=aID) then
      begin
      Level:=1;
      exit;
      end
    else if (BelowID<>'') and (Level<=0) then
      Exit;
    FList.Add(aID);
    end;
end;

procedure THTMLExtractIDS.DoEndElement(Sender: TObject; const NamespaceURI, LocalName, QName: SAXString);
begin
  if Level>0 then
    Dec(FLevel);
end;

procedure THTMLExtractIDS.ExtractIDS(aInput: TStream; aList: TStrings);
var
  MyReader : THTMLReader;

begin
  FList:=aList;
  MyReader:=THTMLReader.Create;
  Try
    MyReader.OnStartElement:=@DoStartElement;
    MyReader.OnEndElement:=@DoEndElement;
    MyReader.ParseStream(aInput);
  finally
    FreeAndNil(MyReader);
  end;

end;

function THTMLExtractIDS.ExtractIDS(aInput: TStream): TStringArray;

Var
  L : TStringList;
  I : Integer;

begin
  L:=TStringList.Create;
  try
    ExtractIDS(aInput,L);
    L.Sort;
    Setlength(Result,L.Count);
    For I:=0 to L.Count-1 do
      Result[I]:=L[i];
  finally
    L.Free;
  end;
end;

procedure THTMLExtractIDS.ExtractIDS(const aFileName: String; aList: TStrings);

Var
  F : TFileStream;
  B : TBufStream;

begin
  F:=TFileStream.Create(aFileName,fmOpenRead or fmShareDenyWrite);
  try
    B:=TReadBufStream.Create(F,4096);
    B.SourceOwner:=True;
    ExtractIDS(B,aList);
  finally
    B.Free;
  end;
end;

function THTMLExtractIDS.ExtractIDS(const aFileName : String): TStringArray;

Var
  L : TStringList;
  I : Integer;

begin
  L:=TStringList.Create;
  try
    ExtractIDS(aFileName,L);
    L.Sort;
    Setlength(Result,L.Count);
    For I:=0 to L.Count-1 do
      Result[I]:=L[i];
  finally
    L.Free;
  end;
end;

{ ----------------------------------------------------------------------

  ----------------------------------------------------------------------}

{ THTML2ClassOptions }

function THTML2ClassOptions.GetB(AIndex: Integer): Boolean;
begin
  Result:=FBools[aindex];
end;

function THTML2ClassOptions.GetMethods(AIndex: Integer): TSpecialMethods;
begin
  Result:=FMethods[aindex];
end;

function THTML2ClassOptions.GetS(AIndex: Integer): String;
begin
  Result:=FStrings[aindex];
end;

procedure THTML2ClassOptions.SetB(AIndex: Integer; AValue: Boolean);
begin
  FBools[aIndex]:=aValue;
end;

procedure THTML2ClassOptions.SetExcludeElements(AValue: TStrings);
begin
  if FExcludeElements=AValue then Exit;
  FExcludeElements.Assign(AValue);
end;

procedure THTML2ClassOptions.SetMethods(AIndex: Integer; AValue: TSpecialMethods);
begin
  FMethods[aIndex]:=aValue;
end;

procedure THTML2ClassOptions.SetS(AIndex: Integer; AValue: String);
begin
  FStrings[aIndex]:=aValue;
end;

constructor THTML2ClassOptions.Create;
begin
  FExcludeElements:=TStringList.Create;
  Reset;
end;

destructor THTML2ClassOptions.Destroy;
begin
  FreeAndNil(FExcludeElements);
  inherited Destroy;
end;

procedure THTML2ClassOptions.Reset;
begin
  // Assume class is TComponent descendant
  ConstructorArgs:='aOwner : TComponent';
  FormClassName:='TMyForm';
  ParentClassName:='TComponent';
  EventSignature:='Event : TJSEvent';
  EventModifiers:='virtual; abstract;';
  GetElementFunction:='document.getelementByID';
  AddMethods:=[smConstructor,smBindElements,smBindElementEvents];
  VirtualMethods:=[smBindElementEvents,smBindElements];
  OverrideMethods:=[smConstructor];
  FormOptions:=[foBindInConstructor];
  FExcludeElements.Clear;
  ExtraUnits:='Classes'
end;

procedure THTML2ClassOptions.toJSON(aObject: TJSONObject);

  Function GenToArray(aMethods : TSpecialMethods) : TJSONArray;

  Var
    M : TSpecialMethod;

  begin
    Result:=TJSONArray.Create;
    For M in TSpecialMethods do
      If M in aMethods then
         Result.Add(GetEnumName(TypeInfo(TSpecialMethod),Ord(M)));
  end;

  Function OptionsToArray(aOptions : TFormOptions) : TJSONArray;

  Var
    F : TFormOption;

  begin
    Result:=TJSONArray.Create;
    For F in TFormOptions do
      If F in aOptions then
         Result.Add(GetEnumName(TypeInfo(TFormOptions),Ord(F)));
  end;


Var
  arr : TJSONArray;
  S : String;

begin
  With aObject do
    begin
    Add('OverrideMethods',GenToArray(OverrideMethods));
    Add('AddMethods',GenToArray(AddMethods));
    Add('VirtualMethods',GenToArray(VirtualMethods));
    Add('FormOptions',OptionsToArray(FormOptions));
    Add('GetElementFunction',GetElementFunction);
    Add('EventSignature',EventSignature);
    Add('EventModifiers',EventModifiers);
    Add('ConstructorArgs',ConstructorArgs);
    Add('BelowID',BelowID);
    Add('HTMLFileName',HTMLFileName);
    Add('FormClassname',FormClassname);
    Add('UseDefaultElements',UseDefaultElements);
    Add('AddHTMLToProject',AddHTMLToProject);
    arr:=TJSONArray.Create;
    Add('ExcludeElements',Arr);
    For S in ExcludeElements do
      arr.Add(S);
    end;
end;

procedure THTML2ClassOptions.FromJSON(aJSON: String);

Var
  D : TJSONData;
  J : TJSONObject Absolute D;

begin
  D:=GetJSON(aJSON);
  try
    if D is TJSONObject then
      FromJSON(J);
  finally
    D.Free;
  end;

end;

procedure THTML2ClassOptions.FromJSON(aObject: TJSONObject);

  Function GenFromArray(Arr : TJSONArray) : TSpecialMethods;

  Var
    I,Idx : integer;

  begin
    Result:=[];
    if Assigned(Arr) then
      For I:=0 to Arr.Count-1 do
        if (Arr.types[I]=jtString) then
          begin
          Idx:=GetEnumValue(TypeInfo(TSpecialMethod),Arr.Strings[I]);
          If Idx<>-1 then
            include(Result,TSpecialMethod(Idx));
          end;
  end;

  Function OptionsFromArray(arr : TJSONArray) : TFormOptions;

  Var
    I,Idx : integer;

  begin
    Result:=[];
    if Assigned(Arr) then
      For I:=0 to Arr.Count-1 do
        if (Arr.types[I]=jtString) then
          begin
          Idx:=GetEnumValue(TypeInfo(TFormOption),Arr.Strings[I]);
          If Idx<>-1 then
            include(Result,TFormOption(Idx));
          end;
  end;

Var
  arr : TJSONArray;
  I : integer;

begin
  With aObject do
    begin
    OverrideMethods:=GenFromArray(Get('OverrideMethods',TJSONArray(Nil)));
    AddMethods:=GenFromArray(Get('AddMethods',TJSONArray(Nil)));
    VirtualMethods:=GenFromArray(Get('VirtualMethods',TJSONArray(Nil)));
    FormOptions:=OptionsFromArray(Get('FormOptions',TJSONArray(Nil)));
    GetElementFunction:=Get('GetElementFunction','');
    EventSignature:=Get('EventSignature','');
    EventModifiers:=Get('EventModifiers','');
    ConstructorArgs:=Get('ConstructorArgs','');
    BelowID:=Get('BelowID','');
    HTMLFileName:=Get('HTMLFileName','');
    FormClassname:=Get('FormClassname','');
    UseDefaultElements:=Get('UseDefaultElements',False);
    AddHTMLToProject:=Get('AddHTMLToProject',False);
    ExcludeElements.Clear;
    Arr:=Get('ExcludeElements',TJSONArray(Nil));
    if Assigned(Arr) then
      For I:=0 to Arr.Count-1 do
        if (Arr.types[I]=jtString) then
           ExcludeElements.Add(Arr.Strings[I]);
    end;

end;

function THTML2ClassOptions.asJSON(Formatted: Boolean): String;

Var
  J : TJSONObject;

begin
  J:=TJSONObject.Create;
  try
    ToJSON(J);
    if Formatted then
      Result:=J.FormatJSON()
    else
      Result:=J.asJSON;
  finally
    J.Free;
  end;
end;



{ TFormFileCodeGen }

function TFormFileCodeGen.GetFormName(const aClassName: string): String;

begin
  Result:=aClassName;
  if SameText(Copy(Result,1,1),'T') then
    Delete(Result,1,1);
end;

(*
procedure TFormFileCodeGen.LoadFromStream(const AStream: TStream);
begin
  if aStream=Nil then exit;
end;
*)

constructor TFormFileCodeGen.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  IDProperty:='ElementID';
  ElementHeight:=24;
  ElementWidth:=72;
  ElementVSpacing:=8;
  ElementHSpacing:=16;
  MaxWidth:=800;
  MaxHeight:=600;
end;

procedure TFormFileCodeGen.NextPosition;
begin
  ELeft:=ELeft+ElementWidth+ElementHSpacing;
  if ELeft+ElementWidth>=MaxWidth then
    begin
    ELeft:=8;
    ETop:=ETop+ElementHeight+ElementVSpacing;
    end;
end;

procedure TFormFileCodeGen.EmitElementProps(El : TFormElement);

begin
  AddLn('Top = %d',[ETop]);
  AddLn('Left = %d',[ELeft]);
  Addln('Width = %d',[ElementWidth]);
  Addln('Height = %d',[ElementHeight]);
  addLn('%s = ''%s''',[IDProperty,El.Name]);
end;

procedure TFormFileCodeGen.EmitElementEvents(El : TFormElement);

Var
  S,EN,EH : String;

begin
  For S in El.Events do
    begin
    TFormCodeGen.GetEventNameAndHandler(S,El.Name,EN,EH);
    Addln('%s = %s',[EN,EH]);
    end;
end;

procedure TFormFileCodeGen.GenerateElements;

Var
  I : Integer;
  El : TFormElement;

begin
  For I:=0 to FormElements.Count-1 do
    begin
    el:=FormElements[i];
    With El do
      begin
      Addln('object %s: %s',[Name,ElementType]);
      Indent;
      EmitElementProps(EL);
      if DoEvents then
        EmitElementEvents(El);
      Undent;
      AddLn('end');
      NextPosition;
      end;
    end;
end;

procedure TFormFileCodeGen.Execute;


begin
  ETop:=8;
  ELeft:=8;
  AddLn('object %s : %s',[GetFormName(FormClassName),FormClassName]);
  Indent;
  AddLn('Width = %d',[MaxWidth]);
  AddLn('Height = %d',[MaxHeight]);
  GenerateElements;
  Undent;
  AddLn('end');
end;

{ THTMLElementMapList }

function THTMLElementMapList.GetM(aIndex : Integer): THTMLElementMap;
begin
  Result:=Items[aIndex] as THTMLElementMap;
end;

procedure THTMLElementMapList.LoadFromFile(const aFileName: String);

Var
  F : TFileStream;

begin
  F:=TFileStream.Create(aFileName,fmOpenRead or fmShareDenyWrite);
  try
    LoadFromStream(F);
  finally
    F.Free;
  end;
end;

procedure THTMLElementMapList.LoadFromStream(aStream: TStream);

Var
  D : TJSONData;

begin
  D:=GetJSON(aStream);
  try
    if D is TJSONArray then
      LoadFromJSON(D as TJSONArray);
  finally
    D.Free;
  end;
end;

procedure THTMLElementMapList.LoadFromJSON(aJSON: TJSONArray);

Var
  E : TJSONEnum;

begin
  For E in aJSON do
    if E.Value is TJSONObject then
      (Add as THTMLElementMap).LoadFromJSON(e.Value as TJSONObject);
end;

function THTMLElementMapList.IndexOfMap(aTag: SAXString; Attrs: TSAXAttributes
  ): Integer;
begin
  Result:=0;
  While (Result<Count) and Not GetM(Result).IsMatch(aTag,Attrs) do
    Inc(Result);
  if Result=Count then
    Result:=-1;
end;

function THTMLElementMapList.FindMap(aTag: SAXString; Attrs: TSAXAttributes
  ): THTMLElementMap;

Var
  Idx : Integer;

begin
  Idx:=IndexOfMap(aTag,Attrs);
  If Idx=-1 then
    Result:=Nil
  else
    Result:=GetM(Idx);
end;

{ THTMLElementMap }

function THTMLElementMap.GetAttrConditionList: TAttributeConditionList;
begin
  If FConditionList=Nil then
    FConditionList:=CreateConditionList;
  Result:=FConditionList
end;

function THTMLElementMap.CreateConditionList: TAttributeConditionList;
begin
  Result:=TAttributeConditionList.Create(TAttributeCondition);
end;

destructor THTMLElementMap.Destroy;
begin
  FreeAndNil(FConditionList);
  inherited Destroy;
end;

procedure THTMLElementMap.LoadFromJSON(aJSON: TJSONObject);

Var
  A : TJSONObject;

begin
  FTag:=aJSON.Get('tag','');
  ControlClass:=aJSON.Get('class','');
  A:=aJSON.Get('attrs',TJSONObject(Nil));
  If Assigned(A) then
    Attributes.LoadFromJSON(A);
end;

function THTMLElementMap.HasConditions: Boolean;
begin
  Result:=Assigned(FConditionList) and (FConditionList.Count>0);
end;

function THTMLElementMap.IsMatch(aTag: SAXString; Attrs: TSAXAttributes): Boolean;
begin
  Result:=SameText(UTF8Encode(aTag),FTag);
  if Result and HasConditions then
    Result:=Attributes.IsMatch(Attrs);
end;

{ TAttributeConditionList }

function TAttributeConditionList.GetC(aIndex : Integer): TAttributeCondition;
begin
  Result:=TAttributeCondition(Items[aIndex]);
end;

procedure TAttributeConditionList.LoadFromJSON(aJSON: TJSONObject);

Var
  E : TJSONEnum;
  A : TAttributeCondition;

begin
  For E in aJSON do
    begin
    A:=Add as TAttributeCondition;
    A.LoadFromJSON(E.Key,E.Value);
    end;
end;

function TAttributeConditionList.IsMatch(Attrs: TSAXAttributes): Boolean;

  function GetIndex(const aName: SAXString): Integer;

  begin
    Result := Attrs.Length-1;
    while (Result>=0) and not SameText(UTF8Encode(Attrs.LocalNames[Result]),UTF8Encode(aName)) do
      Dec(Result);
  end;

Var
  I,Idx : Integer;
  A : TAttributeCondition;
begin
  Result:=True;
  I:=0;
  While Result and (I<Count) do
    begin
    A:=GetC(I);
    Idx:=GetIndex(UTF8Decode(A.Attribute));
    if A.Operation=aoNotPresent then
      Result:=Idx<0
    else
      Result:=A.IsMatch(UTF8Encode(Attrs.GetValue(Idx)));
    Inc(I);
    end;
end;

{ TAttributeCondition }

procedure TAttributeCondition.LoadFromJSON(aName: String; aValue: TJSONData);

Var
  S : TJSONStringType;
  C : Char;

begin
  Attribute:=aName;
  if aValue.JSONType=jtNull then
    Operation:=aoNotPresent
  else if aValue.JSONType=jtBoolean then
    begin
    if aValue.AsBoolean then
      Operation:=aoPresent
    else
      Operation:=aoNotPresent
    end
  else
    begin
    S:=aValue.AsString;
    If S<>'' then
      C:=S[1]
    else
      C:=#0;

    Case C of
      '-' : Operation:=aoNotEqual;
      '~' : Operation:=aoContains;
    else
      Operation:=aoEqual;
      Value:=S;
    end;
    if Operation in [aoNotEqual,aoContains] then
      Value:=Copy(S,2,Length(S)-1);
    end;
end;

function TAttributeCondition.IsMatch(aValue: String): Boolean;
begin
  Case Operation of
    aoPresent : Result:=True;
    aoNotEqual : Result:=Not SameText(aValue,Value);
    aoEqual : Result:=SameText(aValue,Value);
    aoContains : Result:=Pos(LowerCase(Value),LowerCase(aValue))>0;
  end;
end;


{ THTMLToFormELements }

procedure THTMLToFormELements.SetFormElements(AValue: TFormElementList);
begin
  if FFormElements=AValue then Exit;
  FFormElements:=AValue;
end;

procedure THTMLToFormELements.DoLog(const Msg: String);
begin
  if Assigned(FOnLog) then
    FOnLog(Self,Msg);
end;

procedure THTMLToFormELements.DoLog(const Fmt: String; Args: array of const);
begin
  DoLog(Format(Fmt,Args));
end;

function THTMLToFormELements.Maptype(aTag: SAXString; Atts: TSAXAttributes): String;

var
  t : string;
  m : THTMLElementMap;

begin
  Result:='';
  if Map.Count>0 then
    begin
    M:=Map.FindMap(aTag,Atts);
    if Assigned(m) then
      Exit(M.ControlClass)
    else if not DefaultElements then
      begin
      DoLog('Could not map tag %s',[aTag]);
      Exit;
      end;
    end;
  t:=lowercase(Utf8Encode(aTag));
  case t of
    'input' : Result:='TJSHTMLInputElement';
    'button' : Result:='TJSHTMLButtonElement';
    'select' : Result:='TJSHTMLSelectElement';
    'textarea' : Result:='TJSHTMLTextAreaElement';
    'option' : Result:='';
  else
    Result:='TJSHTMLElement';
  end;
end;

function THTMLToFormELements.MakeValidName(aID: string): string;

Var
  C : Char;

begin
  Result:='';
  for C in aID do
    if C in ['_','a'..'z','A'..'Z','0'..'9'] then
      Result:=Result+C
    else
      Result:=Result+'_';
end;

procedure THTMLToFormELements.SetExcludeIDS(AValue: TStrings);
begin
  if FExcludeIDS=AValue then Exit;
  FExcludeIDs.AddStrings(AValue,True);
end;

procedure THTMLToFormELements.DoStartElement(Sender: TObject;
  const NamespaceURI, LocalName, QName: SAXString; Atts: TSAXAttributes);

Var
  aID,aType : String;
  El : TFormElement;
begin
  if Not Assigned(atts) then exit;
  aID:=UTF8Encode(Atts.GetValue('','id'));
  if (aID='') or (FExcludeIDS.IndexOf(aID)>=0) then
    exit;
  if (Level=0) and (BelowID=aID) then
    Level:=1
  else if (BelowID<>'') and (Level<=0) then
    Exit;
  aType:=MapType(LocalName,Atts);
  if aType='' then
    DoLog('Ignoring tag %s with id %s',[LocalName,aID])
  else
    begin
    El:=FormElements.Add(MakeValidName(aID));
    EL.ElementType:=aType;
    EL.HTMLID:=aId;
    GetEvents(El,Atts);
    end
end;

procedure THTMLToFormELements.GetEvents(aEl : TFormElement; Atts : TSAXAttributes);

Var
  I,aLen : Integer;
  aName : string;

begin
  for I:=0 to Atts.Length-1 do
    begin
    aName:=UTF8Encode(Atts.GetLocalName(i));
    aLen:=Length(aName);
    if (aLen>3) and (Copy(aName,1,1)='_') and (Copy(aName,aLen,1)='_') then
      aEl.Events.Add(Copy(aName,2,aLen-2)+'='+UTF8Encode(Atts.GetValue(i)));
    end;
end;

procedure THTMLToFormELements.DoEndElement(Sender: TObject; const NamespaceURI,
  LocalName, QName: SAXString);
begin
  if Level>0 then
  Dec(FLevel);
end;

class function THTMLToFormELements.CreateElementList: TFormElementList;
begin
  Result:=TFormElementList.Create(TFormElement);
end;

function THTMLToFormELements.CreateHTMLElementMapList: THTMLElementMapList;

begin
  Result:=THTMLElementMapList.Create(THTMLElementMap);
end;

constructor THTMLToFormELements.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  FMap:=CreateHTMLElementMapList;
  FFormElements:=CreateElementList;
  FExcludeIDS:=TStringList.Create;
  TStringList(FExcludeIDS).Sorted:=True;
end;

destructor THTMLToFormELements.Destroy;
begin
  FreeAndNil(FMap);
  FreeAndNil(FExcludeIDS);
  FreeAndNil(FFormElements);
  inherited Destroy;
end;

procedure THTMLToFormELements.Clear;
begin
  FFormElements.Clear;
end;

procedure THTMLToFormELements.LoadFromStream(aInput: TStream);

var
  MyReader : THTMLReader;

begin
  MyReader:=THTMLReader.Create;
  Try
    MyReader.OnStartElement:=@DoStartElement;
    MyReader.OnEndElement:=@DoEndElement;
    MyReader.ParseStream(aInput);
  finally
    FreeAndNil(MyReader);
  end;
end;

procedure THTMLToFormELements.LoadFromFile(const aFileName: String);
var
  F : TFileStream;
begin
  F:=TFileStream.Create(aFileName,fmOpenRead or fmShareDenyWrite);
  try
    LoadFromStream(F);
  finally
    F.Free;
  end;
end;

procedure THTMLToFormELements.LoadOptions(aOptions: THTML2ClassOptions);
begin
  BelowID:=aoptions.BelowID;
  ExcludeIDS:=aOptions.ExcludeElements;
  DefaultElements:=aOptions.UseDefaultElements;
  if (aOptions.TagMapFileName<>'') and FileExists(aOptions.TagMapFileName) then
    Map.LoadFromFile(aOptions.TagMapFileName);
end;

{ TFormCodeGen }

procedure TFormCodeGen.SetFormElements(AValue: TFormElementList);
begin
  if FFormElements=AValue then Exit;
  FFormElements.Assign(AValue);
end;

function TFormCodeGen.BaseUnits: String;
begin
  Result:='js, web';
end;

class function TFormCodeGen.CreateElementList: TFormElementList;
begin
  Result:=TFormElementList.Create(TFormElement);
end;

constructor TFormCodeGen.Create(aOwner: TComponent);

Var
  Defs : THTML2ClassOptions;

begin
  inherited Create(aOwner);
  FFormElements:=CreateElementList;
  FFormFileGenerator:=CreateFormFileGen;
  FFormSource:=TStringList.Create;
  // Defaults must be set in
  Defs:=THTML2ClassOptions.Create;
  try
    Defs.Reset;
    Loadoptions(Defs);

  finally
    Defs.Free;
  end;
end;

destructor TFormCodeGen.Destroy;
begin
  FreeAndNil(FFormSource);
  FreeAndNil(fFormFileGenerator) ;
  FreeAndNil(FFormElements);
  inherited Destroy;
end;

procedure TFormCodeGen.EmitFormElement(aEL : TFormElement);

begin
  With aEl do
    AddLn('%s : %s;',[Name,ElementType]) ;
end;

procedure TFormCodeGen.EmitFormEvents(aEL : TFormElement);

Var
  S,EN,EH : String;

begin
  if not aEl.HasEvents then
    exit;
  For S in aEl.Events do
    begin
    GetEventNameAndHandler(S,aEl.Name,EN,EH);
    Addln('Procedure %s(%s); %s',[EH, EventSignature,EventModifiers]);
    end;
end;

procedure TFormCodeGen.EmitPublishedSection;

var
  I : Integer;

begin
  For I:=0 to FormElements.Count-1 do
    EmitFormElement(FormElements[i]);
  if foEvents in Options then
    For I:=0 to FormElements.Count-1 do
      EmitFormEvents(FormElements[i]);
end;

function TFormCodeGen.VirtualOverride(M: TSpecialMethod; const Decl: String): string;

begin
  Result:=Decl;
  if M in OverrideMethods then
    Result:=Result+' override;'
  else if M in VirtualMethods then
    Result:=Result+' virtual;'
end;

procedure TFormCodeGen.EmitPublicSection;

begin
  if smConstructor in AddMethods then
    Addln(VirtualOverride(smConstructor,'Constructor create('+ConstructorArgs+');'));
  if smBindElements in AddMethods then
  Addln(VirtualOverride(smBindElements, 'Procedure BindElements;'));
  if (smBindElementEvents in AddMethods) and (foEvents in Options) then
    Addln(VirtualOverride(smBindElementEvents,'Procedure BindElementEvents;'));
end;

procedure TFormCodeGen.Execute;

begin
   Source.Clear;
   Addln('unit %s;',[OutputUnitName]);
   CreateHeader;
   Addln('Type');
   Indent;
   ClassHeader(FormClassName);
   AddLn('%s = class(%s) ',[FormClassName,ParentClassName]);
   Addln('Published');
   Indent;
   EmitPublishedSection;
   Undent;
   Addln('Public');
   Indent;
   EmitPublicSection;
   Undent;
   Addln('end;');
   Undent;
   Addln('');
   Addln('implementation');
   AddLn('');
   if (foFormFile in Options) then
     begin
     EmitFormFile;
     AddLn('');
     AddLn('{$R *.dfm}');
     AddLn('');
     end;
   ClassHeader(FormClassName);
   EmitImplementation;
   AddLn('');
   AddLn('end.');
end;

procedure TFormCodeGen.LoadOptions(aOptions: THTML2ClassOptions);
begin
  ExtraUnits:=aOptions.ExtraUnits;
  FormClassName:=aOptions.FormClassname;
  ParentClassName:=aOptions.ParentClassName;
  GetElementFunction:=aOptions.GetElementFunction;
  EventSignature:=aOptions.EventSignature;
  EventModifiers:=aOptions.EventModifiers;
  ConstructorArgs:=aOptions.ConstructorArgs;
  Options:=aOptions.FormOptions;
  AddMethods:=aOptions.AddMethods;
  OverrideMethods:=aOptions.OverrideMethods;
  VirtualMethods:=aOptions.VirtualMethods;
end;

procedure TFormCodeGen.EmitFormFile;

begin
  FormFileGenerator.FormElements:=Self.FormElements;
  FormFileGenerator.DoEvents:=foEvents in Options;
  FormFileGenerator.FormClassName:=Self.FormClassName;
  FormFileGenerator.Execute;
  FormSource.Assign(FormFileGenerator.Source);
end;

function TFormCodeGen.CreateFormFileGen: TFormFileCodeGen;
begin
  Result:=TFormFileCodeGen.Create(Nil);
end;

function TFormCodeGen.CreateHTMLToFormELements: THTMLToFormELements;

begin
  Result:=THTMLToFormELements.Create(Self);
end;


procedure TFormCodeGen.EmitFormConstructor;

begin
  Addln('');
  Addln('Constructor %s.create(%s);',[FormClassName,ConstructorArgs]);
  if not (foBindInConstructor in Options) then
    SimpleMethodBody(['Inherited;'])
  else
    begin
    if foEvents in Options then
      SimpleMethodBody(['Inherited;','BindElements;','BindElementEvents;'])
    else
      SimpleMethodBody(['Inherited;','BindElements;']);
    end;
  Addln('');
end;

procedure TFormCodeGen.EmitImplementation;

begin
  if smConstructor in AddMethods then
    EmitFormConstructor;
  if (smBindElements in AddMethods) then
    EmitFormBindElements;
  if (foEvents in Options) and Not (foFormFile in Options) and (smBindElementEvents in AddMethods) then
    EmitFormBindEvents;
end;

procedure TFormCodeGen.EmitFormBindElements;

var
  I : integer;
  El : TFormElement;

begin
  Addln('');
  Addln('Procedure %s.BindElements;',[FormClassName]);
  Addln('');
  AddLn('begin');
  Indent;
  if smBindElements in OverrideMethods then
    AddLn('inherited;');
  For I:=0 to FormElements.Count-1 do
    begin
    el:=FormElements[i];
    With El do
      Addln('%s:=%s(%s(''%s''));',[Name,ElementType,GetElementFunction,HTMLID]);
    end;
  Undent;
  Addln('end;');
  Addln('');
end;

class function TFormCodeGen.Pretty(const S: String): string;

begin
  Result:=UpperCase(Copy(S,1,1))+LowerCase(Copy(S,2,Length(S)-1));
end;

class procedure TFormCodeGen.GetEventNameAndHandler(const S,
  aFieldName: String; out aName, aHandler: string);

Var
  P : Integer;

begin
  P:=Pos('=',S);
  if (P=0) then
    P:=Length(S)+1;
  aName:=Copy(S,1,P-1);
  aHandler:=Copy(S,P+1,Length(S)-P);
  if AHandler='' then
    aHandler:=aFieldName+Pretty(aName);
//  Writeln(aFieldName,': ',S,' -> ',aName,' & ',aHandler);
end;


procedure TFormCodeGen.EmitFormBindEvents;

var
  I : integer;
  El : TFormElement;
  S,EN,EH : String;

begin
  Addln('Procedure %s.BindElementEvents;',[FormClassName]);
  Addln('');
  AddLn('begin');
  Indent;
  if smBindElementEvents in OverrideMethods then
    AddLn('inherited;');
  For I:=0 to FormElements.Count-1 do
    begin
    el:=FormElements[i];
    With El do
      if HasEvents then
        For S in El.Events do
          begin
          GetEventNameAndHandler(S,Name,EN,EH);
          Addln('%s.AddEventListener(''%s'',@%s);',[Name,EN,EH]);
          end;
    end;
  Undent;
  Addln('end;');
end;

{ TFormElementList }

function TFormElementList.GetEl(aIndex : Integer): TFormElement;
begin
  Result:=Items[aIndex] as TFormElement;
end;

function TFormElementList.Add(const aName: string): TFormElement;
begin
  if IndexOf(aName)<>-1 then
    Raise Exception.CreateFmt('Duplicate name : %s' ,[aName]);
  Result:=(Inherited Add) as TFormElement;
  Result.Name:=aName;
end;

function TFormElementList.IndexOf(const aName: string): Integer;
begin
  Result:=Count-1;
  While (Result>=0) and Not SameText(aName,GetEl(Result).Name) do
    Dec(Result);
end;

function TFormElementList.Find(const aName: string): TFormElement;

var
  Idx : Integer;

begin
  Idx:=IndexOf(aName);
  if Idx>=0 then
    Result:=GetEl(Idx)
  else
    Result:=Nil;
end;

{ TFormElement }

function TFormElement.GetEvents: TStrings;
begin
  If (FEvents=Nil) then
    FEvents:=TStringList.Create;
  Result:=FEvents;
end;

function TFormElement.getName: String;
begin
  Result:=FName;
  if Result='' then
    Result:=HTMLID;
end;

procedure TFormElement.SetEvents(AValue: TStrings);
begin
  If AValue=FEVents then exit;
  Events.Assign(aValue);
end;

destructor TFormElement.Destroy;
begin
  FreeAndNil(FEvents);
  inherited Destroy;
end;

function TFormElement.HasEvents: Boolean;
begin
  Result:=Assigned(FEvents) and (FEvents.Count>0);
end;

procedure TFormElement.Assign(Source: TPersistent);

Var
  FE : TFormElement absolute Source;

begin
  if Source is TFormElement then
    begin
    FHTMLID:=FE.HTMLID;
    FName:=FE.FName;
    FType:=FE.FType;
    if FE.HasEvents then
      Events:=FE.Events;
    end
  else
    inherited Assign(Source);
end;


end.

