unit reglazwebextra;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpwebdata,
  sqldbwebdata, LazIDEIntf,
  ProjectIntf, fpextjs,
  extjsjson, extjsxml,
  fpjsonrpc,
  fpextdirect,
  webjsonrpc;

Type

  { TFileDescWebProviderDataModule }

  TFileDescWebProviderDataModule = class(TFileDescPascalUnitWithResource)
  public
    constructor Create; override;
    function GetInterfaceUsesSection: string; override;
    function GetLocalizedName: string; override;
    function GetLocalizedDescription: string; override;
    function GetImplementationSource(const Filename, SourceName, ResourceName: string): string;override;
  end;

  { TFileDescWebJSONRPCModule }

  TFileDescWebJSONRPCModule = class(TFileDescPascalUnitWithResource)
  public
    constructor Create; override;
    function GetInterfaceUsesSection: string; override;
    function GetLocalizedName: string; override;
    function GetLocalizedDescription: string; override;
    function GetImplementationSource(const Filename, SourceName, ResourceName: string): string;override;
  end;


  { TFileDescExtDirectModule }

  TFileDescExtDirectModule = class(TFileDescPascalUnitWithResource)
  public
    constructor Create; override;
    function GetInterfaceUsesSection: string; override;
    function GetLocalizedName: string; override;
    function GetLocalizedDescription: string; override;
    function GetImplementationSource(const Filename, SourceName, ResourceName: string): string;override;
  end;

Procedure Register;

resourcestring
  rsWebDataProvi = 'Web DataProvider Module';
  rsWEBDataProvi2 = 'WEB DataProvider Module%sA datamodule to handle data '
    +'requests for WEB (HTTP) applications using WebDataProvider components.';
  rsWebJSONRPCMo = 'Web JSON-RPC Module';
  rsWEBJSONRPCMo2 = 'WEB JSON-RPC Module%sA datamodule to dispatch JSON-RPC '
    +'requests in WEB (HTTP) applications using TJSONRPCHandler components.';
  rsWebExtDirect = 'Web Ext.Direct Module';
  rsWEBExtDirect2 = 'WEB Ext.Direct Module%sA datamodule to dispatch Ext.'
    +'Direct requests in WEB (HTTP) applications using TJSONRPCHandler '
    +'components.';

Var
   FileDescriptorWebProviderDataModule: TFileDescWebProviderDataModule;
   FileDescriptorJSONRPCModule : TFileDescWebJSONRPCModule;
   FileDescriptorExtDirectModule : TFileDescExtDirectModule;

implementation

uses FormEditingIntf, controls, forms,frmrpcmoduleoptions;

Procedure Register;

begin
   RegisterComponents('fpWeb',[TWebdataInputAdaptor,TFPWebDataProvider, TSQLDBWebDataProvider,
                               TExtJSJSonWebdataInputAdaptor,TExtJSJSONDataFormatter,
                               TExtJSXMLWebdataInputAdaptor,TExtJSXMLDataFormatter,
                               TJSONRPCHandler,TJSONRPCDispatcher,TSessionJSONRPCDispatcher,
                               TJSONRPCContentProducer,
                               TExtDirectDispatcher,TExtDirectContentProducer]);
   FileDescriptorWebProviderDataModule:=TFileDescWebProviderDataModule.Create;
   FileDescriptorJSONRPCModule:=TFileDescWebJSONRPCModule.Create;
   FileDescriptorExtDirectModule:=TFileDescExtDirectModule.Create;
   RegisterProjectFileDescriptor(FileDescriptorWebProviderDataModule);
   RegisterProjectFileDescriptor(FileDescriptorJSONRPCModule);
   RegisterProjectFileDescriptor(FileDescriptorExtDirectModule);
   FormEditingHook.RegisterDesignerBaseClass(TFPCustomWebProviderDataModule);
   FormEditingHook.RegisterDesignerBaseClass(TFPWebProviderDataModule);
   FormEditingHook.RegisterDesignerBaseClass(TJSONRPCModule);
   FormEditingHook.RegisterDesignerBaseClass(TExtDirectModule);
end;

{ TFileDescWebProviderDataModule }

constructor TFileDescWebProviderDataModule.Create;
begin
  inherited Create;
  Name:='Web DataProvider Module';
  ResourceClass:=TFPWebProviderDataModule;
  UseCreateFormStatements:=False;
end;

function TFileDescWebProviderDataModule.GetInterfaceUsesSection: string;
begin
  Result:=inherited GetInterfaceUsesSection;
  Result:=Result+', HTTPDefs, websession, fpHTTP, fpWeb, fpwebdata';
end;

function TFileDescWebProviderDataModule.GetLocalizedName: string;
begin
  Result:=rsWebDataProvi;
end;

function TFileDescWebProviderDataModule.GetLocalizedDescription: string;
begin
  Result:=Format(rsWEBDataProvi2, [#13]);
end;

function TFileDescWebProviderDataModule.GetImplementationSource(const Filename,
  SourceName, ResourceName: string): string;
begin
  Result:=Inherited GetImplementationSource(FileName,SourceName,ResourceName);
  Result:=Result+'  RegisterHTTPModule(''T'+ResourceName+''',T'+ResourceName+');'+LineEnding;
end;



{ TFileDescWebJSONFPCModule }

constructor TFileDescWebJSONRPCModule.Create;
begin
  inherited Create;
  Name:='JSON-RPC Module';
  ResourceClass:=TJSONRPCModule;
  UseCreateFormStatements:=False;
end;

function TFileDescWebJSONRPCModule.GetInterfaceUsesSection: string;
begin
  Result:=inherited GetInterfaceUsesSection;
  Result:=Result+', HTTPDefs, websession, fpHTTP, fpWeb, fpjsonrpc, webjsonrpc';
end;

function TFileDescWebJSONRPCModule.GetLocalizedName: string;
begin
  Result:=rsWebJSONRPCMo;
end;

function TFileDescWebJSONRPCModule.GetLocalizedDescription: string;
begin
  Result:=Format(rsWEBJSONRPCMo2, [#13]);
end;

function TFileDescWebJSONRPCModule.GetImplementationSource(const Filename,
  SourceName, ResourceName: string): string;

Var
  RH,RM : Boolean;
  CN,HP : String;

begin
  RH:=False;
  RM:=False;
  CN:=ResourceName;
  HP:=ResourceName;
  With TJSONRPCModuleOptionsForm.Create(Application) do
    try
      If (ShowModal=mrOK) then
        begin
        RH:=RegisterHandlers;
        If RH Then
          CN:=JSONRPCClass;
        RM:=RegisterModule;
        If RM then
          HP:=HTTPPath;
        end;
    finally
      Free;
    end;
  Result:=Inherited GetImplementationSource(FileName,SourceName,ResourceName);
  If RM then
    Result:=Result+'  RegisterHTTPModule('''+HP+''',T'+ResourceName+');'+LineEnding;
  If RH then
    Result:=Result+'  JSONRPCHandlerManager.RegisterDatamodule(T'+ResourceName+','''+HP+''',);'+LineEnding;
end;

{ TFileDescExtDirectModule }

constructor TFileDescExtDirectModule.Create;
begin
  inherited Create;
  Name:='Ext.Direct Module';
  ResourceClass:=TExtDirectModule;
  UseCreateFormStatements:=False;
end;

function TFileDescExtDirectModule.GetInterfaceUsesSection: string;
begin
  Result:=inherited GetInterfaceUsesSection;
  Result:=Result+', HTTPDefs, websession, fpHTTP, fpWeb, fpjsonrpc, webjsonrpc, fpextdirect';
end;

function TFileDescExtDirectModule.GetLocalizedName: string;
begin
  Result:=inherited GetLocalizedName;
  Result:=rsWebExtDirect;
end;

function TFileDescExtDirectModule.GetLocalizedDescription: string;
begin
  Result:=Format(rsWEBExtDirect2, [#13]);
end;

function TFileDescExtDirectModule.GetImplementationSource(const Filename,
  SourceName, ResourceName: string): string;

Var
  RH,RM : Boolean;
  CN,HP : String;

begin
  RH:=False;
  RM:=False;
  CN:=ResourceName;
  HP:=ResourceName;
  With TJSONRPCModuleOptionsForm.Create(Application) do
    try
      If (ShowModal=mrOK) then
        begin
        RH:=RegisterHandlers;
        If RH Then
          CN:=JSONRPCClass;
        RM:=RegisterModule;
        If RM then
          HP:=HTTPPath;
        end;
    finally
      Free;
    end;
  Result:=Inherited GetImplementationSource(FileName,SourceName,ResourceName);
  If RM then
    Result:=Result+'  RegisterHTTPModule('''+HP+''',T'+ResourceName+');'+LineEnding;
  If RH then
    Result:=Result+'  JSONRPCHandlerManager.RegisterDatamodule(T'+ResourceName+','''+HP+''',);'+LineEnding;
end;

end.

