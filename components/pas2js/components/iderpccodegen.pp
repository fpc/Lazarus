unit iderpccodegen;

{$mode ObjFPC}
{$h+}
interface

uses
  Classes, SysUtils, fpjson, pascodegen;

type

  { TAPIClientCodeGen }
  TClientCodeOption = (ccoPreferNativeInt,ccoForceJSValueResult);
  TClientCodeOptions = set of TClientCodeOption;

  { TAPIMethodParam }

  TAPIMethodParam = Class(TCollectionItem)
  private
    FDefaultValue: String;
    FJSType: TJSONtype;
    FName: String;
    FPasName: String;
    FPasType: String;
    FRequired: Boolean;
  Public
    Procedure Assign(Source : TPersistent); override;
    Property Name : String Read FName Write FName;
    Property PasName : String Read FPasName Write FPasName;
    Property JSType : TJSONtype Read FJSType Write FJSType;
    Property PasType : String Read FPasType Write FPasType;
    Property Required : Boolean Read FRequired Write FRequired;
    Property DefaultValue : String Read FDefaultValue Write FDefaultValue;
  end;

  { TAPIService }

  { TAPIMethodParams }

  TAPIMethodParams = Class(TCollection)
  private
    function GetParam(aIndex : Integer): TAPIMethodParam;
  Public
    Constructor Create; overload;
    Function AddParam : TAPIMethodParam;
    Property Params [aIndex : Integer] : TAPIMethodParam Read GetParam; default;
  end;

  { TAPIServiceMethod }

  TAPIServiceMethod = Class(TCollectionItem)
  private
    FName: String;
    FParams: TAPIMethodParams;
    FPasName: String;
    FPasReturnType: String;
    FReturnType: TJSONtype;
    procedure SetParams(AValue: TAPIMethodParams);
  Public
    Constructor Create(aCollection : TCollection) ; override;
    Destructor Destroy; override;
    Procedure Assign(Source : TPersistent); override;
    Property Name : String Read FName Write FName;
    Property PasName : String Read FPasName Write FPasName;
    Property ReturnType : TJSONtype Read FReturnType Write FReturnType;
    Property PasReturnType : String Read FPasReturnType Write FPasReturnType;
    Property Params : TAPIMethodParams Read FParams Write SetParams;
  end;

  { TAPIServiceMethods }

  TAPIServiceMethods = Class(TCollection)
  private
    function GetMethod(aIndex : Integer): TAPIServiceMethod;
  Public
    Constructor Create; overload;
    Function AddMethod : TAPIserviceMethod;
    Property Methods [aIndex : Integer] : TAPIServiceMethod Read GetMethod; default;
  end;

  TAPIService = Class(TCollectionItem)
  private
    FMethods: TAPIServiceMethods;
    FName: String;
    FPasName: String;
    procedure SetMethods(AValue: TAPIServiceMethods);
  Public
    Constructor Create(aCollection : TCollection) ; override;
    Destructor Destroy; override;
    Procedure Assign(aSource : TPersistent); override;
    Property Methods : TAPIServiceMethods Read FMethods Write SetMethods;
    Property Name : String Read FName Write FName;
    Property PasName : String Read FPasName Write FPasName;
  end;

  { TAPService }

  TAPIServices = Class(TCollection)
  private
    function GetAPIService(aIndex : Integer): TAPIService;
  Public
    Constructor Create; overload;
    Function AddService : TAPIservice;
    Property Service [aIndex : Integer] : TAPIService Read GetAPIService; default;
  end;

  TAPIClientCodeGen = Class(TPascalCodeGenerator)
  private
    FAPI: TJSONObject;
    FOptions: TClientCodeOptions;
    FServiceParentClass: String;
    procedure SetAPI(AValue: TJSONObject);
  protected
    // Overrides
    Function BaseUnits : String; override;
    function StringToJSType(S: String): TJSONtype;
    // High-level decl
    procedure GenerateServiceClassDeclarations(aServices: TAPIServices); virtual;
    procedure GenerateServiceDeclaration(aService: TAPIService); virtual;
    procedure GenerateServiceMethodDeclaration(aSvc : TAPIService; aMeth : TAPIServiceMethod); virtual;
    // High-level impl
    procedure GenerateServiceClassImplementations(aServices: TAPIServices); virtual;
    procedure GenerateServiceImplementation(aService: TAPIService); virtual;
    procedure GenerateServiceMethodImplementation(aSvc : TAPIService; aMeth : TAPIServiceMethod); virtual;
    procedure GenerateRPCClassNameImplementation(aService: TAPIService); virtual;
    // Get names. All incoming names are the original names of the API
    function GetServiceClassName(const aName: string): String; virtual;
    function GetServiceMethodName(const aClassName, aMethodName: string): String; virtual;
    function GetServiceMethodParamName(const aClassName, aMethodName, aParamName: string): String; virtual;
    function GetServiceMethodParamType(const aClassName, aMethodName, aParamName: String; aParamType: TJSONtype): String; virtual;
    function GetServiceMethodParamDefault(const aClassName, aMethodName, aParamName: string; aParamType : TJSONType): String; virtual;
    function GetServiceMethodResultHandler(const aClassName, aMethodName: string; aResultType: TJSONType): String; virtual;
    // Convert JSON to API structures
    Procedure FillAPIServices(aAPI : TAPIServices); virtual;
    procedure FillAPIMethod(aSvc: TAPIService; aMeth: TAPIServiceMethod; aJSParams: TJSONArray); virtual;
    procedure FillAPIMethodParam(aSvc: TAPIService; aMeth: TAPIServiceMethod; aParam: TAPIMethodParam; aJSON: TJSONObject); virtual;
    procedure FillAPIService(aSvc: TAPIService; aJSService: TJSONArray); virtual;
  Public
    Constructor Create(aOwner : TComponent); override;
    Procedure Execute;
    Property API : TJSONObject Read FAPI Write SetAPI;
    Property Options : TClientCodeOptions Read FOptions Write FOptions;
    Property ServiceParentClass : String Read FServiceParentClass Write FServiceParentClass;
  end;

implementation

{ TAPIMethodParams }

function TAPIMethodParams.GetParam(aIndex : Integer): TAPIMethodParam;
begin
  Result:=TAPIMethodParam(Items[aIndex]);
end;

constructor TAPIMethodParams.Create;
begin
  Inherited Create(TAPIMethodParam);
end;

function TAPIMethodParams.AddParam: TAPIMethodParam;
begin
  Result:=TAPIMethodParam(Add);
end;

{ TAPIMethodParam }

procedure TAPIMethodParam.Assign(Source: TPersistent);

Var
  P : TAPIMethodParam absolute Source;

begin
  if Source is TAPIMethodParam then
    begin
    FName:=P.FName;
    FPasName:=P.FPasName;
    FPasType:=P.FPasType;
    FRequired:=P.FRequired;
    FDefaultValue:=P.FDefaultValue;
    FJSType:=P.FJSType;
    end
  else
    inherited Assign(Source);
end;

{ TAPIServiceMethod }

procedure TAPIServiceMethod.SetParams(AValue: TAPIMethodParams);
begin
  if FParams=AValue then Exit;
  FParams.Assign(AValue);
end;

constructor TAPIServiceMethod.Create(aCollection: TCollection);
begin
  inherited Create(aCollection);
  FParams:=TAPIMethodParams.Create;
end;

destructor TAPIServiceMethod.Destroy;
begin
  FreeAndNil(FParams);
  Inherited;
end;

procedure TAPIServiceMethod.Assign(Source: TPersistent);

Var
  M : TAPIServiceMethod absolute Source;

begin
  if Source is TAPIServiceMethod then
    begin
    FName:=M.FName;
    FPasName:=M.FPasName;
    FReturnType:=M.FReturnType;
    FPasReturnType:=M.FPasReturnType;
    FParams.Assign(M.Params);
    end
  else
    inherited Assign(Source);
end;

{ TAPIServiceMethods }

function TAPIServiceMethods.GetMethod(aIndex : Integer): TAPIServiceMethod;
begin
  Result:=TAPIServiceMethod(Items[aIndex]);
end;

constructor TAPIServiceMethods.Create;
begin
  Inherited Create(TAPIServiceMethod);
end;

function TAPIServiceMethods.AddMethod: TAPIserviceMethod;
begin
  Result:=Add as TAPIserviceMethod
end;

{ TAPIService }

procedure TAPIService.SetMethods(AValue: TAPIServiceMethods);
begin
  if FMethods=AValue then Exit;
  FMethods.Assign(AValue);
end;

constructor TAPIService.Create(aCollection: TCollection);
begin
  inherited Create(aCollection);
  FMethods:=TAPIServiceMethods.Create;
end;

destructor TAPIService.Destroy;
begin
  FreeAndNil(FMethods);
  Inherited;
end;

procedure TAPIService.Assign(aSource: TPersistent);

Var
  svc : TAPIService absolute aSource;

begin
  if aSource is TAPIService then
    begin
    FName:=svc.FName;
    FPasName:=svc.FPasName;
    FMethods.Assign(svc.Methods);
    end
  else
    inherited Assign(aSource);
end;

{ TAPIServices }

function TAPIServices.GetAPIService(aIndex : Integer): TAPIService;
begin
  Result:=TAPIService(Items[aIndex])
end;

constructor TAPIServices.Create;
begin
  Inherited Create(TAPIService);
end;

function TAPIServices.AddService: TAPIservice;
begin
  Result:=Add as TAPIservice;
end;

{ TAPIClientCodeGen }

procedure TAPIClientCodeGen.SetAPI(AValue: TJSONObject);
begin
  if FAPI=AValue then Exit;
  FAPI.Free;
  FAPI:=AValue;
end;

procedure TAPIClientCodeGen.GenerateServiceClassDeclarations(aServices: TAPIServices);

Var
  I : Integer;

begin
  For I:=0 to aServices.Count-1 do
    GenerateServiceDeclaration(aServices[i]);
end;

procedure TAPIClientCodeGen.GenerateServiceClassImplementations(aServices: TAPIServices);

Var
  I : Integer;

begin
  For I:=0 to aServices.Count-1 do
    GenerateServiceImplementation(aServices[i]);
end;


procedure TAPIClientCodeGen.Execute;

Var
  Services : TAPIServices;
begin
  CreateUnitClause;
  CreateHeader;
  AddLn('Type');
  Indent;
  Services:=TAPIServices.Create;
  try
    FillAPIServices(Services);
    GenerateServiceClassDeclarations(Services);
    Addln('');
    Addln('implementation');
    Addln('');
    GenerateServiceClassImplementations(Services);
    Addln('');
    Addln('end.');


  finally
    Services.Free;
    Undent;
  end;

end;

function TAPIClientCodeGen.GetServiceClassName(const aName: string): String;

begin
  Result:='T'+EscapeKeyWord(aName)+'Service';
end;

function TAPIClientCodeGen.GetServiceMethodName(const aClassName,
  aMethodName: string): String;

begin
  Result:=EscapeKeyWord(aMethodName);
end;

function TAPIClientCodeGen.GetServiceMethodParamName(const aClassName, aMethodName, aParamName: string): String;

begin
  Result:=EscapeKeyWord(aParamName);
end;

function TAPIClientCodeGen.GetServiceMethodParamType(const aClassName,
  aMethodName, aParamName: String; aParamType: TJSONtype): String;

begin
  case aParamtype of
    jtString : Result:='String';
    jtBoolean : Result:='Boolean';
    jtNumber : begin
                 if ccoPreferNativeInt in Options then
                   Result:='NativeInt'
                 else
                   Result:='Double';
                 end;
    jtArray : Result:='TJSArray';
    jtObject : Result:='TJSObject';
  else
    Result:='JSValue';
  end;
end;

function TAPIClientCodeGen.GetServiceMethodParamDefault(const aClassName, aMethodName, aParamName: string; aParamType : TJSONType): String;

begin
  case aParamtype of
    jtString  : Result:='''''';
    jtBoolean : Result:='False';
    jtNumber  : begin
                if ccoPreferNativeInt in Options then
                  Result:='0'
                else
                  Result:='0.0';
               end;
    jtArray   : Result:='Nil';
    jtObject  : Result:='Nil';
  else
    Result:='Nil';
  end;
end;

function TAPIClientCodeGen.GetServiceMethodResultHandler(const aClassName,
  aMethodName: string; aResultType: TJSONType): String;

begin
  {
  TEmptyResultHandler = reference to procedure;
  TBooleanResultHandler = reference to procedure (aResult : Boolean);
  TNativeIntResultHandler = reference to procedure (aResult : NativeInt);
  TDoubleResultHandler = reference to procedure (aResult : Double);
  TStringResultHandler = reference to procedure (aResult : String);
  TArrayResultHandler = reference to procedure (aResult : TJSArray);
  TObjectResultHandler = reference to procedure (aResult : TJSObject);
  TJSValueResultHandler = reference to procedure (aResult : JSValue);

  }
  if ccoForceJSValueResult in options then
    Result:='TJSValueResultHandler'
  else
    case aResultType of
      jtString  : Result:='TStringResultHandler';
      jtBoolean : Result:='TBooleanResultHandler';
      jtNumber  : begin
                  if ccoPreferNativeInt in Options then
                    Result:='TNativeIntResultHandler'
                  else
                    Result:='TDoubleResultHandler';
                  end;
      jtArray   : Result:='TArrayResultHandler';
      jtObject  : Result:='TObjectResultHandler';
      jtNull    : Result:='TEmptyResultHandler';
      jtUnknown : Result:='TJSValueResultHandler';
    else
      Result:='TEmptyResultHandler';
    end;
end;

procedure TAPIClientCodeGen.FillAPIServices(aAPI: TAPIServices);

Var
  Actions : TJSONObject;
  I : Integer;
  AService : TJSONArray;
  svc : TAPIService;

begin
  Actions:=API.Get('actions',TJSONObject(Nil));
  If Not Assigned(Actions) then
    exit;
  For I:=0 to Actions.Count-1 do
    begin
    svc:=aAPI.AddService;
    svc.Name:=Actions.Names[i];
    svc.PasName:=GetServiceClassName(svc.Name);
    aService:=Actions.Arrays[svc.Name];
    FillAPIService(svc,aService);
    end;
end;

function TAPIClientCodeGen.StringToJSType(S : String) : TJSONtype;

begin
  S:=LowerCase(S);
  Case S of
    'jtunknown' : Result:=jtUnknown;
    'jtnumber'  : Result:=jtNumber;
    'jtstring'  : Result:=jtString;
    'jtboolean' : Result:=jtBoolean;
    'jtnull'    : Result:=jtNull;
    'jtarray'   : Result:=jtArray;
    'jtobject'  : Result:=jtObject;
  else
    Result:=jtUnknown;
  end;
end;

procedure TAPIClientCodeGen.FillAPIService(aSvc : TAPIService; aJSService : TJSONArray);

Var
  I : Integer;
  aJSON : TJSONObject;
  aMeth : TAPIServiceMethod;
  aParams : TJSONArray;

begin
  For I:=0 to aJSService.Count-1 do
    begin
    aJSON:=aJSService.Objects[i];
    aMeth:=aSvc.Methods.AddMethod;
    aMeth.Name:=aJSON.Get('name','');
    aMeth.PasName:=GetServiceMethodName(aSvc.Name,aMeth.Name);
    aMeth.ReturnType:=StringToJSType(aJSON.Get('resulttype',''));
    aParams:=aJSON.Get('paramdefs',TJSONarray(Nil));
    if (aJSON.Get('len',0)>0) and Assigned(aParams) then
      FillAPIMethod(aSvc,aMeth,aParams);
    end;
end;

constructor TAPIClientCodeGen.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  FServiceParentClass:='TRPCCustomService';
end;

procedure TAPIClientCodeGen.FillAPIMethodParam(aSvc : TAPIService; aMeth : TAPIServiceMethod; aParam :TAPIMethodParam; aJSON : TJSONObject);

begin
  aParam.Name:=aJSON.get('name','');
  aParam.PasName:=GetServiceMethodParamName(aSvc.Name,aMeth.Name,aParam.Name);
  aParam.JSType:=StringToJSType(aJSON.Get('type',''));
  aParam.PasType:=GetServiceMethodParamType(aSvc.Name,aMeth.Name,aParam.Name,aParam.JSType);
  aParam.Required:=aJSON.Get('required',true);
  aParam.DefaultValue:=GetServiceMethodParamDefault(aSVC.Name,aMeth.Name,aParam.Name,aParam.JSType);
end;

procedure TAPIClientCodeGen.FillAPIMethod(aSvc : TAPIService; aMeth : TAPIServiceMethod; aJSParams : TJSONArray);

var
  I : Integer;
  aJSON : TJSONObject;
  aParam : TAPIMethodParam;

begin
  For I:=0 to aJSParams.Count-1 do
    begin
    aJSON:=aJSParams.Objects[i];
    aParam:=aMeth.Params.AddParam;
    FillAPIMethodParam(aSvc,aMeth,aParam,aJSON);
    end;
end;

procedure TAPIClientCodeGen.GenerateServiceMethodDeclaration(aSvc : TAPIService; aMeth : TAPIServiceMethod);

Var
  I : Integer;
  ResType,ParamLine : String;
  aParam : TAPIMethodParam;

begin
  resType:=GetServiceMethodResultHandler(aSvc.Name, aMeth.Name, aMeth.ReturnType);
  ParamLine:='';
  For I:=0 to aMeth.Params.Count-1 do
    begin
    aParam:=aMeth.Params[i];
    if ParamLine<>'' then
      ParamLine:=ParamLine+'; ';
    ParamLine:=ParamLine+aParam.PasName+' : '+aParam.PasType;
    if (not aParam.Required) and (aParam.DefaultValue<>'') then
      ParamLine:=ParamLine+' = '+aParam.DefaultValue;
    end;
  if ParamLine<>'' then
    ParamLine:=ParamLine+'; ';
  ParamLine:=ParamLine+'aOnSuccess : '+ResType+' = Nil; aOnFailure : TRPCFailureCallBack = Nil';
  AddLn('Function %s (%s) : NativeInt;',[aMeth.PasName,ParamLine]);
//  For I:=0 to
end;

procedure TAPIClientCodeGen.GenerateServiceMethodImplementation(aSvc : TAPIService; aMeth : TAPIServiceMethod);

Var
  I : Integer;
  ResType,ParamLine : String;
  aParam : TAPIMethodParam;

begin
  resType:=GetServiceMethodResultHandler(aSvc.Name, aMeth.Name, aMeth.ReturnType);
  ParamLine:='';
  For I:=0 to aMeth.Params.Count-1 do
    begin
    aParam:=aMeth.Params[i];
    if ParamLine<>'' then
      ParamLine:=ParamLine+'; ';
    ParamLine:=ParamLine+aParam.PasName+' : '+aParam.PasType;
    if (not aParam.Required) and (aParam.DefaultValue<>'') then
      ParamLine:=ParamLine+' = '+aParam.DefaultValue;
    end;
  if ParamLine<>'' then
    ParamLine:=ParamLine+'; ';
  ParamLine:=ParamLine+'aOnSuccess : '+ResType+' = Nil; aOnFailure : TRPCFailureCallBack = Nil';
  AddLn('Function %s.%s (%s) : NativeInt;',[aSvc.PasName,aMeth.PasName,ParamLine]);
  AddLn('');
  Indent;
  Addln('Procedure DoSuccess(Sender : TObject; const aResult : JSValue);');
  AddLn('');
  Addln('begin');
  indent;
    Addln('If Assigned(aOnSuccess) then');
    Indent;
      Addln('aOnSuccess(%s(aResult))',[aMeth.PasReturnType]);
    undent;
  undent;
  Addln('end;');
  Undent;
  AddLn('');
  Addln('Var');
  Indent;
    Addln('_Params : JSValue;');
  Undent;
  AddLn('');
  Addln('begin');
  Indent;
  Addln('StartParams;');
  For I:=0 to aMeth.Params.Count-1 do
    begin
    aParam:=aMeth.Params[i];
    AddLn('AddParam(''%s'',%s);',[aParam.Name,aParam.PasName]);
    end;
  Addln('_Params:=EndParams;');
  AddLn('Result:=ExecuteRequest(RPCClassName,''%s'',_Params,@DoSuccess,aOnFailure);',[aMeth.Name]);
  Undent;
  Addln('end;');
  AddLn('');
  AddLn('');
end;

procedure TAPIClientCodeGen.GenerateServiceDeclaration(aService: TAPIService);

Var
  I : integer;

begin
  ClassHeader(aService.PasName);
  AddLn('%s = Class(TRPCCustomService)',[aService.PasName]);
  Addln('Protected');
  Indent;
    AddLn('Function RPCClassName : string; override;');
  Undent;
  Addln('Public');
  Indent;
  For I:=0 to aService.Methods.Count-1 do
    GenerateServiceMethodDeclaration(aService,aService.Methods[i]);
  Undent;
  Addln('end;');
end;

procedure TAPIClientCodeGen.GenerateRPCClassNameImplementation(aService: TAPIService);

begin
  Addln('Function %s.RPCClassName : string;',[aService.PasName]);
  Addln('');
  AddLn('begin');
  indent;
  AddLn('Result:=''%s'';',[aService.Name]);
  undent;
  Addln('end;');
  Addln('');
  Addln('');
end;

procedure TAPIClientCodeGen.GenerateServiceImplementation(aService: TAPIService);

Var
  I : integer;

begin
  ClassHeader(aService.PasName);
  Addln('');
  GenerateRPCClassNameImplementation(aService);
  For I:=0 to aService.Methods.Count-1 do
    GenerateServiceMethodImplementation(aService,aService.Methods[i]);
  Addln('');
end;


function TAPIClientCodeGen.BaseUnits: String;

begin
  Result:='fprpcclient';
end;



end.

