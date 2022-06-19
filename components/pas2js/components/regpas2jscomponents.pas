unit regpas2jscomponents;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ProjectIntf, PropEdits, ComponentEditors, fpjsondataset, dbpropedits,
  db, stub.htmlfragment, stub.htmlactions, stub.data.htmlactions, stub.restdataset,
  stub.webwidget, stub.bootstrapwidgets, stub.bootstraptablewidget, stub.bulmawidgets,
  stub.templateloader, stub.jsondataset, stub.dbwebwidget, stub.dbhtmlwidgets,
  stub.fprpcclient;


Type

  { TFileDescHTMLFragment }

  TFileDescHTMLFragment = class(TFileDescPascalUnitWithResource)
  public
    constructor Create; override;
    Class Function HTMLBaseDir : String;
    Procedure CheckHTMLFile(Sender : TObject; aComponent : TComponent; var aHTMLFile : String);
    function GetInterfaceUsesSection: string; override;
    function GetLocalizedName: string; override;
    function GetLocalizedDescription: string; override;
    function GetImplementationSource(const Filename, SourceName, ResourceName: string): string;override;
  end;

  { TStubRequirements }

  TSQLDBRestStubRequirements = Class(TComponentRequirements)
  public
    Procedure RequiredUnits(Units: TStrings); override;
  end;

  { THTMLElementActionListRequirements }

  THTMLElementActionListRequirements = Class(TComponentRequirements)
  public
    Procedure RequiredUnits(Units: TStrings); override;
  end;

  { TDBElementActionRequirements }

  TDBElementActionRequirements = Class(TComponentRequirements)
  public
    Procedure RequiredUnits(Units: TStrings); override;
  end;


  { THTMLBootstrapWidgetRequirements }

  THTMLBootstrapWidgetRequirements = Class(TComponentRequirements)
  public
    Procedure RequiredUnits(Units: TStrings); override;
  end;

  { TRTLTemplateLoaderRequirements }

  TRTLTemplateLoaderRequirements = Class(TComponentRequirements)
  public
    Procedure RequiredUnits(Units: TStrings); override;
  end;

  { TRTLTemplateLoaderRequirements }

  { TJSONDatasetRequirements }

  TJSONDatasetRequirements = Class(TComponentRequirements)
  public
    Procedure RequiredUnits(Units: TStrings); override;
  end;

  { TDBBootstrapTableWidgetRequirements }

  TDBBootstrapTableWidgetRequirements = Class(TComponentRequirements)
  public
    Procedure RequiredUnits(Units: TStrings); override;
  end;

  { TDBWebWidgetRequirements }

  TDBWebWidgetRequirements = Class(TComponentRequirements)
  public
    Procedure RequiredUnits(Units: TStrings); override;
  end;

  { TDBHTMLWebWidgetRequirements }

  TDBHTMLWebWidgetRequirements = Class(TComponentRequirements)
  public
    Procedure RequiredUnits(Units: TStrings); override;
  end;

  { TBulmaWidgetRequirements }

  TBulmaWidgetRequirements = Class(TComponentRequirements)
  public
    Procedure RequiredUnits(Units: TStrings); override;
  end;

  { TFPRPCClientRequirements }

  TFPRPCClientRequirements = Class(TComponentRequirements)
  public
    Procedure RequiredUnits(Units: TStrings); override;
  end;

  { TLogBridge }

  TLogBridge = Class(TObject)
  private
    FView: String;
  Public
    Procedure DoLog(Sender : TObject; Const Msg : String);
    Property View : String Read FView Write FView;
  end;

procedure register;

implementation

uses
  // LCL/Lazarus
  Types, IDEWindowIntf, controls, forms, dialogs, formeditingintf, lazideintf, idemsgintf, IDEExternToolIntf, Menuintf,
  // Pas2jsDesign
  pjscontroller, idehtmltools,
  // pas2jscomponents
  frmHTMLActionsEditor, strpas2jscomponents, pas2jsrestutils, pas2jsrestcmd, frmpas2jsedithtml, p2jselementactions,
  frmcreaterpcserviceclient, pas2jscompedits;

{$r pas2jsc_images.res}


Var
  FragmentDescr : TFileDescHTMLFragment;
  LogBridge : TLogBridge;


Procedure RegisterRequirements;

begin
  RegisterComponentRequirements(TSQLDBRestConnection,TSQLDBRestStubRequirements);
  RegisterComponentRequirements(TSQLDBRestDataset,TSQLDBRestStubRequirements);
  RegisterComponentRequirements(THTMLElementActionList,THTMLElementActionListRequirements);
  RegisterComponentRequirements(THTMLElementAction,THTMLElementActionListRequirements);
  RegisterComponentRequirements(THTMLCustomElementAction,THTMLElementActionListRequirements);
  RegisterComponentRequirements(TDBHTMLInputElementAction,TDBElementActionRequirements);
  RegisterComponentRequirements(TDBHTMLButtonElementAction,TDBElementActionRequirements);
  RegisterComponentRequirements(TBootstrapModal,THTMLBootstrapWidgetRequirements);
  RegisterComponentRequirements(TBootstrapToastWidget,THTMLBootstrapWidgetRequirements);
  RegisterComponentRequirements(TBulmaModal,TBulmaWidgetRequirements);
  RegisterComponentRequirements(TBulmaToastWidget,TBulmaWidgetRequirements);
  RegisterComponentRequirements(TTemplateLoader,TRTLTemplateLoaderRequirements);
  RegisterComponentRequirements(TLocalJSONDataset,TJSONDatasetRequirements);
  RegisterComponentRequirements(TDBBootstrapTableWidget,TDBBootstrapTableWidgetRequirements);
  RegisterComponentRequirements(TDBLoopTemplateWidget,TDBWebWidgetRequirements);
  RegisterComponentRequirements(TDBSelectWidget,TDBHTMLWebWidgetRequirements);
  RegisterComponentRequirements(TPas2jsRPCClient,TFPRPCClientRequirements);
end;

Procedure RegisterStandardHTMLActions;

begin
  RegisterPas2JSAction(THTMLElementAction,rsStandardHTMLAction);
  RegisterPas2JSAction(TDBHTMLInputElementAction,rsDBEditHTMLAction);
  RegisterPas2JSAction(TDBHTMLButtonElementAction,rsDBButtonHTMLAction);
end;


Procedure RegisterComponentEditors;

begin
  RegisterComponentEditor(THTMLElementActionList,THTMLElementActionListComponentEditor);
  RegisterComponentEditor(TBootstrapModal,TBootstrapModalComponentEditor);
  RegisterComponentEditor(TPas2JSRPCClient,TPas2JSRPCClientComponentEditor);
end;

Procedure RegisterPropertyEditors;

begin
  RegisterPropertyEditor(TypeInfo(String),THTMLCustomElementAction,'ElementID',TElementIDPropertyEditor);
  RegisterPropertyEditor(TypeInfo(String),TDBHTMLInputElementAction,'FieldName',TDBHTMLElementActionFieldProperty);
  RegisterPropertyEditor(TypeInfo(String),TCustomHTMLFragment,'HTMLFileName',THTMLFileNamePropertyEditor);
  RegisterPropertyEditor(TypeInfo(String),TCustomHTMLFragment,'ParentID',TProjectElementIDPropertyEditor);
  RegisterPropertyEditor(TypeInfo(String),TSQLDBRestDataset,'ResourceName',TSQLDBRestResourceNamePropertyEditor);
  RegisterPropertyEditor(TypeInfo(String),TSQLDBRestDataset,'DatabaseConnection',TSQLDBRestDatabaseConnectionPropertyEditor);
  RegisterPropertyEditor(TypeInfo(String),TBootstrapModal,'ParentID',TElementIDPropertyEditor);
  RegisterPropertyEditor(TypeInfo(String),TBootstrapModal,'ElementID',TElementIDPropertyEditor);
  RegisterPropertyEditor(TypeInfo(String),TDBBootstrapTableWidget,'ElementID',TElementIDPropertyEditor);
  RegisterPropertyEditor(TypeInfo(String),TDBBootstrapTableWidget,'ParentIDID',TElementIDPropertyEditor);
  RegisterPropertyEditor(TypeInfo(String),TReferenceItem,'Selector',TElementIDSelectorPropertyEditor);
//  RegisterPropertyEditor(TypeInfo(String),TDBHTMLButtonElementAction,'ElementID',TElementIDPropertyEditor);
  RegisterPropertyEditor(TypeInfo(String),TBootstrapModal,'Template',TTemplatePropertyEditor);
  RegisterPropertyEditor(TypeInfo(String),TBootstrapModal,'TemplateName',TBootstrapModalTemplateNamePropertyEditor);
  RegisterPropertyEditor(TypeInfo(string),TBSTableColumn, 'FieldName', TBSColumnFieldProperty);
  RegisterPropertyEditor(TypeInfo(string),TBSTableColumn, 'WidthUnits', TWidthUnitsProperty);
  RegisterPropertyEditor(TypeInfo(string),TBSTableColumn, 'ButtonURLTarget', TTargetProperty);
  //
  RegisterPropertyEditor(TypeInfo(String),TBulmaModal,'ParentID',TElementIDPropertyEditor);
  RegisterPropertyEditor(TypeInfo(String),TBulmaModal,'ElementID',TElementIDPropertyEditor);
  RegisterPropertyEditor(TypeInfo(String),TBulmaModal,'Template',TTemplatePropertyEditor);
  RegisterPropertyEditor(TypeInfo(String),TBulmaToastWidget,'Header',TTemplatePropertyEditor);
  RegisterPropertyEditor(TypeInfo(String),TBulmaToastWidget,'Body',TTemplatePropertyEditor);

  // Maybe make a THTMLFragmentString = type string
  // Same for ElementID/ParentID : TDOMIDString = type string
  RegisterPropertyEditor(TypeInfo(String),TCustomLoopTemplateWidget,'HeaderTemplate',THTMLFragmentPropertyEditor);
  RegisterPropertyEditor(TypeInfo(String),TCustomLoopTemplateWidget,'ItemTemplate',THTMLFragmentPropertyEditor);
  RegisterPropertyEditor(TypeInfo(String),TCustomLoopTemplateWidget,'FooterTemplate',THTMLFragmentPropertyEditor);
  RegisterPropertyEditor(TypeInfo(String),TCustomLoopTemplateWidget,'ParentID',TElementIDPropertyEditor);
  RegisterPropertyEditor(TypeInfo(String),TCustomLoopTemplateWidget,'ElementID',TElementIDPropertyEditor);
  RegisterPropertyEditor(TypeInfo(String),TLoopTemplateGroup,'HeaderTemplate',THTMLFragmentPropertyEditor);
  RegisterPropertyEditor(TypeInfo(String),TLoopTemplateGroup,'FooterTemplate',THTMLFragmentPropertyEditor);

  RegisterPropertyEditor(TypeInfo(string),TDBSelectWidget, 'ItemField', TFieldProperty);
  RegisterPropertyEditor(TypeInfo(string),TDBSelectWidget, 'ValueField', TFieldProperty);
  RegisterPropertyEditor(TypeInfo(String),TDBSelectWidget,'ParentID',TElementIDPropertyEditor);
  RegisterPropertyEditor(TypeInfo(String),TDBSelectWidget,'ElementID',TElementIDPropertyEditor);
end;

procedure RegisterHTMLFragmentHandling;

begin
  FragmentDescr:=TFileDescHTMLFragment.Create;
  RegisterProjectFileDescriptor(FragmentDescr);
  HTMLTools.RegisterComponent2HTMLFileHandler(THTMLFragment,@FragmentDescr.CheckHTMLFile);
  FormEditingHook.RegisterDesignerBaseClass(THTMLFragment);
end;

procedure RegisterRESTHandling;

begin
  LogBridge:=TLogBridge.Create;
  LogBridge.View:='IDE REST utils';
  IDERestUtils.OnLog:=@LogBridge.DoLog;
  IDERestUtils.LogEnabled:=True;
  IDERestUtils.ShowRaw:=True;
  // Main menu
  With RestCmdHandler do
    begin
    OnLog:=@LogBridge.DoLog;
    RegisterCommands;
    end;
end;

procedure register;

begin
  RegisterComponents('Pas2js',[THTMLElementActionList,TBootstrapModal,TBootstrapToastWidget,TTemplateLoader,TPas2jsRPCClient]);
  RegisterComponents('Pas2js Bulma',[TBulmaModal,TBulmaToastWidget]);
  RegisterComponents('Pas2js Data Access',[TSQLDBRestConnection,TSQLDBRestDataset,TLocalJSONDataset]);
  RegisterComponents('Pas2js Data Controls',[TDBBootstrapTableWidget,TDBLoopTemplateWidget,TDBSelectWidget]);
  RegisterClasses([TJSONDateField, TJSONTimeField, TJSONDateTimeField]);
  RegisterNoIcon([THTMLCustomElementAction,THTMLElementAction,TDBHTMLInputElementAction,TDBHTMLButtonElementAction]);
  RegisterRequirements;
  RegisterStandardHTMLActions;
  RegisterComponentEditors;
  RegisterPropertyEditors;
  RegisterHTMLFragmentHandling;
  RegisterRESTHandling;
end;


{ TFPRPCClientRequirements }

procedure TFPRPCClientRequirements.RequiredUnits(Units: TStrings);
begin
  Units.text:='fprpcclient';
end;

{ TBulmaWidgetRequirements }

procedure TBulmaWidgetRequirements.RequiredUnits(Units: TStrings);
begin
  Units.text:='bulmawidgets';
end;

{ TDBHTMLWebWidgetRequirements }

procedure TDBHTMLWebWidgetRequirements.RequiredUnits(Units: TStrings);
begin
  Units.Clear;
  Units.Add('dbhtmlwidgets');
  Units.Add('webwidget');
end;

{ TDBWebWidgetRequirements }

procedure TDBWebWidgetRequirements.RequiredUnits(Units: TStrings);
begin
  Units.Clear;
  Units.Add('dbwebwidget');
  Units.Add('webwidget');
end;




{ TDBBootstrapTableWidgetRequirements }

procedure TDBBootstrapTableWidgetRequirements.RequiredUnits(Units: TStrings);
begin
  Units.Clear;
  Units.Add('bootstraptablewidget');
end;

{ TJSONDatasetRequirements }

procedure TJSONDatasetRequirements.RequiredUnits(Units: TStrings);
begin
  Units.Clear;
  Units.Add('localjsondataset');
  Units.Add('jsondataset');
end;

{ TDBElementActionRequirements }

procedure TDBElementActionRequirements.RequiredUnits(Units: TStrings);
begin
  Units.Clear;
  Units.Add('Data.HTMLActions');
  Units.Add('Rtl.HTMLActions');
end;



{ TFileDescHTMLFragment }

constructor TFileDescHTMLFragment.Create;
begin
  inherited Create;
  Name:='HTML Module';
  ResourceClass:=THTMLFragment;
  UseCreateFormStatements:=False;
end;

class function TFileDescHTMLFragment.HTMLBaseDir: String;
begin
  Result:=TIDEHTMLTools.HTMLBaseDir;
end;

procedure TFileDescHTMLFragment.CheckHTMLFile(Sender: TObject;
  aComponent: TComponent; var aHTMLFile: String);


begin
  if aComponent is THTMLFragment then
    begin
    aHTMLFile:=THTMLFragment(aComponent).HTMLFileName;
    if (aHTMLFile<>'') then
      begin
      // If it is a relative filename, assume it is relative to project filename.
      if ExpandFileName(aHTMLFile)<>aHTMLFile then
        begin
        aHTMLFile:=IncludeTrailingPathDelimiter(HTMLBaseDir)+aHTMLFile;
        if Not FileExists(aHTMLFile) then
          aHTMLFile:='';
        end;
      end;
    end;
end;

function TFileDescHTMLFragment.GetInterfaceUsesSection: string;
begin
  Result:='SysUtils, Classes, htmlfragment';
end;

function TFileDescHTMLFragment.GetLocalizedName: string;
begin
  Result:=rsHTMLFragment;
end;

function TFileDescHTMLFragment.GetLocalizedDescription: string;
begin
  Result:=rsHTMLFragmentDescr;
end;

function TFileDescHTMLFragment.GetImplementationSource(const Filename,
  SourceName, ResourceName: string): string;
begin
  Result:=inherited GetImplementationSource(Filename, SourceName, ResourceName);
end;



{ TRTLTemplateLoaderRequirements }

procedure TRTLTemplateLoaderRequirements.RequiredUnits(Units: TStrings);
begin
  Units.Text:='Rtl.TemplateLoader';
end;

{ THTMLBootstrapWidgetRequirements }

procedure THTMLBootstrapWidgetRequirements.RequiredUnits(Units: TStrings);
begin
  Units.Text:='bootstrapwidgets';
end;

{ TLogBridge }

procedure TLogBridge.DoLog(Sender: TObject; const Msg: String);
begin
  IDEMessagesWindow.AddCustomMessage(TMessageLineUrgency.mluProgress,Msg,'',0,0,View);
end;

{ THTMLElementActionListRequirements }


procedure THTMLElementActionListRequirements.RequiredUnits(Units: TStrings);
begin
  Units.Text:='Rtl.HTMLActions';
end;

{ TSQLDBRestStubRequirements }

procedure TSQLDBRestStubRequirements.RequiredUnits(Units: TStrings);
begin
  Units.Clear;
  Units.Add('sqldbrestdataset');
  Units.Add('jsondataset');
end;

initialization

Finalization
  LogBridge.Free;
end.

