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
  Result:='Web DataProvider Module';
end;

function TFileDescWebProviderDataModule.GetLocalizedDescription: string;
begin
  Result:='WEB DataProvider Module'#13
         +'A datamodule to handle data requests for WEB (HTTP) applications using WebDataProvider components.';
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
  Result:='Web JSON-RPC Module';
end;

function TFileDescWebJSONRPCModule.GetLocalizedDescription: string;
begin
  Result:='WEB JSON-RPC Module'#13
         +'A datamodule to dispatch JSON-RPC requests in WEB (HTTP) applications using TJSONRPCHandler components.';
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
  Result:='Web Ext.Direct Module';
end;

function TFileDescExtDirectModule.GetLocalizedDescription: string;
begin
  Result:='WEB Ext.Direct Module'#13
         +'A datamodule to dispatch Ext.Direct requests in WEB (HTTP) applications using TJSONRPCHandler components.';
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

