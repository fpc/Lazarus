unit regpas2jscomponents;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ProjectIntf, PropEdits, ComponentEditors, fpjsondataset, dbpropedits,
  db, stub.htmlfragment, stub.htmlactions, stub.data.htmlactions, stub.restdataset,
  stub.webwidget, stub.bootstrapwidgets, stub.bootstraptablewidget, stub.bulmawidgets,
  stub.templateloader, stub.jsondataset, stub.dbwebwidget, stub.dbhtmlwidgets;

Type

  { THTMLElementActionListComponentEditor }

  THTMLElementActionListComponentEditor = class(TComponentEditor)
  private
    FActionList: THTMLElementActionList;
    FCompDesigner: TComponentEditorDesigner;
  protected
  public
    constructor Create(AComponent: TComponent;
                       ADesigner: TComponentEditorDesigner); override;
    destructor Destroy; override;
    procedure Edit; override;
    procedure CreateMissing;
    property ActionList: THTMLElementActionList read FActionList write FActionList;
    function GetVerbCount: Integer; override;
    function GetVerb({%H-}Index: Integer): string; override;
    procedure ExecuteVerb({%H-}Index: Integer); override;
  end;

  { TBootstrapModalComponentEditor }

  TBootstrapModalComponentEditor = class(TComponentEditor)
  private
    FModal: TBootstrapModal;
  protected
  public
    constructor Create(AComponent: TComponent;
                       ADesigner: TComponentEditorDesigner); override;
    destructor Destroy; override;
    procedure Edit; override;
    Procedure EditTemplate;
    property Modal : TBootstrapModal read FModal write FModal;
    function GetVerbCount: Integer; override;
    function GetVerb({%H-}Index: Integer): string; override;
    procedure ExecuteVerb({%H-}Index: Integer); override;
  end;

  { TElementIDPropertyEditor }
  // Get element ID list from HTML file associated with component
  TElementIDPropertyEditor = class(TStringPropertyEditor)
  Public
    Function GetHTMLFileName : String;
    function GetAttributes: TPropertyAttributes; override;
    Function ProcessID(S : String) : String; virtual;
    Procedure GetValues(Proc: TGetStrProc); override;
  end;

  { TElementIDSelectorPropertyEditor }

  TElementIDSelectorPropertyEditor = class(TElementIDPropertyEditor)
  Public
    Function ProcessID(S : String) : String; override;
  end;


  // Get element ID list from HTML file associated with project

  { TProjectElementIDPropertyEditor }

  TProjectElementIDPropertyEditor = class(TElementIDPropertyEditor)
    Function GetHTMLFileName : String;
  end;

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

  { THTMLFileNamePropertyEditor }

  THTMLFileNamePropertyEditor = class(TStringPropertyEditor)
  Public
    Procedure Edit; override;
    Function GetAttributes: TPropertyAttributes; override;
  end;

  { TSQLDBRestResourceNamePropertyEditor }

  TSQLDBRestResourceNamePropertyEditor = class(TStringPropertyEditor)
  Public
    function GetAttributes: TPropertyAttributes; override;
    Procedure GetValues(Proc: TGetStrProc); override;
  end;

  { TSQLDBRestDatabaseConnectionPropertyEditor }

  TSQLDBRestDatabaseConnectionPropertyEditor = class(TStringPropertyEditor)
  Public
    function GetAttributes: TPropertyAttributes; override;
    Procedure GetValues(Proc: TGetStrProc); override;
  end;

  { TTemplatePropertyEditor }

  THTMLFragmentPropertyEditor = Class(TStringPropertyEditor)
  Public
    function GetAttributes: TPropertyAttributes; override;
    Procedure Edit; override;
  end;

  // Edit a HTML fragment
  TTemplatePropertyEditor = THTMLFragmentPropertyEditor;

  { TTemplateNamePropertyEditor }

  TTemplateNamePropertyEditor = Class(TStringPropertyEditor)
  private
  Protected
    function GetTemplateLoader: TCustomTemplateLoader; virtual;
  Public
    function GetAttributes: TPropertyAttributes; override;
    Procedure GetValues(Proc: TGetStrProc); override;
  end;

  { TBootStrapModalTemplateNamePropertyEditor }

  TBootStrapModalTemplateNamePropertyEditor = Class(TTemplateNamePropertyEditor)
  Protected
    function GetTemplateLoader: TCustomTemplateLoader; override;
  end;


  { TWidthUnitsProperty }

  TWidthUnitsProperty = class(TStringPropertyEditor)
  public
    function  GetAttributes: TPropertyAttributes; override;
    procedure GetValues(Proc: TGetStrProc); override;
  end;

  { TBSColumnFieldProperty }

  TBSColumnFieldProperty = Class(TFieldProperty)
    procedure FillValues(const Values: TStringList); override;
  end;

  { TTargetProperty }

  TTargetProperty = class(TStringPropertyEditor)
  public
    function  GetAttributes: TPropertyAttributes; override;
    procedure GetValues(Proc: TGetStrProc); override;
  end;

  { TSelectFieldProperty }

  TDBSelectFieldProperty = Class(TFieldProperty)
    procedure FillValues(const Values: TStringList); override;
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
  Types, IDEWindowIntf, forms, dialogs, formeditingintf, lazideintf, idemsgintf, IDEExternToolIntf, Menuintf,
  // Pas2jsDesign
  pjscontroller, idehtmltools,
  // pas2jscomponents
  frmHTMLActionsEditor, strpas2jscomponents, pas2jsrestutils, pas2jsrestcmd, frmpas2jsedithtml, p2jselementactions;

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
end;

Procedure RegisterPropertyEditors;

begin
  RegisterPropertyEditor(TypeInfo(String),THTMLCustomElementAction,'ElementID',TElementIDPropertyEditor);
  RegisterPropertyEditor(TypeInfo(String),TCustomHTMLFragment,'HTMLFileName',THTMLFileNamePropertyEditor);
  RegisterPropertyEditor(TypeInfo(String),TCustomHTMLFragment,'ParentID',TProjectElementIDPropertyEditor);
  RegisterPropertyEditor(TypeInfo(String),TSQLDBRestDataset,'ResourceName',TSQLDBRestResourceNamePropertyEditor);
  RegisterPropertyEditor(TypeInfo(String),TSQLDBRestDataset,'DatabaseConnection',TSQLDBRestDatabaseConnectionPropertyEditor);
  RegisterPropertyEditor(TypeInfo(String),TBootstrapModal,'ParentID',TElementIDPropertyEditor);
  RegisterPropertyEditor(TypeInfo(String),TBootstrapModal,'ElementID',TElementIDPropertyEditor);
  RegisterPropertyEditor(TypeInfo(String),TDBBootstrapTableWidget,'ElementID',TElementIDPropertyEditor);
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
  RegisterComponents('Pas2js',[THTMLElementActionList,TBootstrapModal,TBootstrapToastWidget,TTemplateLoader]);
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

{ TBulmaWidgetRequirements }

procedure TBulmaWidgetRequirements.RequiredUnits(Units: TStrings);
begin
  Units.text:='bulmawidgets';
end;

{ TSelectFieldProperty }

procedure TDBSelectFieldProperty.FillValues(const Values: TStringList);

Var
  L : TDBSelectWidget;
  DataSource: TDataSource;

begin
  L:=TDBSelectWidget(GetComponent(0));
  DataSource := L.Datasource;
  ListDataSourceFields(DataSource, Values);
  ShowMessage('Fields: '+Values.Text)
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

{ TTargetProperty }

function TTargetProperty.GetAttributes: TPropertyAttributes;
begin
  Result:= [paValueList, paSortList, paMultiSelect];
end;

procedure TTargetProperty.GetValues(Proc: TGetStrProc);

Const
  targets : array[0..3] of string = ('_self','_blank','_parent','_top');

Var S : String;

begin
  for S in Targets do
    Proc(S);
end;

{ TBSColumnFieldProperty }

procedure TBSColumnFieldProperty.FillValues(const Values: TStringList);

Var
  BS : TBSTableColumn;
  BSTW : TDBBootstrapTableWidget;

begin
  BS:=TBSTableColumn(GetComponent(0));
  BSTW:=(BS.Collection as TBSTableColumns).Owner as TDBBootstrapTableWidget;
  if Assigned(BSTW) and Assigned(BSTW.Datasource) then
  ListDataSourceFields(BSTW.Datasource,Values)
end;

{ TWidthUnitsProperty }

function TWidthUnitsProperty.GetAttributes: TPropertyAttributes;
begin
  Result:= [paValueList, paSortList, paMultiSelect];
end;

procedure TWidthUnitsProperty.GetValues(Proc: TGetStrProc);

Const
  Units : array[0..7] of string = ('px','ch','em','ic','lg','rem','vw','%');

Var S : String;

begin
  for S in Units do
    Proc(S);
end;




{ TDBBootstrapTableWidgetRequirements }

procedure TDBBootstrapTableWidgetRequirements.RequiredUnits(Units: TStrings);
begin
  Units.Clear;
  Units.Add('bootstraptablewidget');
end;

{ TElementIDSelectorPropertyEditor }

function TElementIDSelectorPropertyEditor.ProcessID(S: String): String;
begin
  Result:='#'+S;
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


{ TBootstrapModalComponentEditor }

constructor TBootstrapModalComponentEditor.Create(AComponent: TComponent;
  ADesigner: TComponentEditorDesigner);
begin
  inherited Create(AComponent, ADesigner);
  FModal:=aComponent as TBootstrapModal;
end;

destructor TBootstrapModalComponentEditor.Destroy;
begin
  FModal:=Nil;
  inherited Destroy;
end;

procedure TBootstrapModalComponentEditor.Edit;
begin
  EditTemplate;
end;


procedure TBootstrapModalComponentEditor.EditTemplate;

Var
  HTML : String;

begin
  HTML:=Modal.Template;
  if EditHTML(Modal.Name+'.Template',HTML) then
    begin
    Modal.Template:=HTML;
    Designer.Modified;
    end;
end;

function TBootstrapModalComponentEditor.GetVerbCount: Integer;
begin
  Result:=1;
end;

function TBootstrapModalComponentEditor.GetVerb(Index: Integer): string;
begin
  case Index of
    0 : Result:=rsEditTemplate;
  else
    Result:=inherited GetVerb(Index);
  end;
end;

procedure TBootstrapModalComponentEditor.ExecuteVerb(Index: Integer);
begin
  Case Index of
    0 : EditTemplate;
  else
    inherited ExecuteVerb(Index);
  end;
end;

{ TBootStrapModalTemplateNamePropertyEditor }

function TBootStrapModalTemplateNamePropertyEditor.GetTemplateLoader: TCustomTemplateLoader;
begin
  Result:=(GetComponent(0) as TBootstrapModal).TemplateLoader;
end;

{ TTemplateNamePropertyEditor }

function TTemplateNamePropertyEditor.GetAttributes: TPropertyAttributes;
begin
  Result:=[paMultiSelect,paValueList];
end;

Function TTemplateNamePropertyEditor.GetTemplateLoader : TCustomTemplateLoader;

begin
  Result:=Nil;
end;

procedure TTemplateNamePropertyEditor.GetValues(Proc: TGetStrProc);

Var
  T : TCustomTemplateLoader;
  I : Integer;

begin
  T:=GetTemplateLoader;
  if Not Assigned(T) then
    exit;
  For I:=0 to T.PreloadTemplates.Count-1 do
    Proc(T.PreloadTemplates[I].Name);
end;

{ TTemplateEditor }

function THTMLFragmentPropertyEditor.GetAttributes: TPropertyAttributes;
begin
  Result:=[paMultiSelect,paDialog];
end;

procedure THTMLFragmentPropertyEditor.Edit;

Var
  HTML : String;
  aName : string;

begin
  HTML:=GetStrValue;
  aName:=GetPropertyPath(0);
  If EditHTML(aName,HTML) then
    SetStrValue(HTML);
end;

{ TSQLDBRestDatabaseConnectionPropertyEditor }

function TSQLDBRestDatabaseConnectionPropertyEditor.GetAttributes: TPropertyAttributes;
begin
  Result:=[paMultiSelect,paValueList];
end;

procedure TSQLDBRestDatabaseConnectionPropertyEditor.GetValues(Proc: TGetStrProc
  );
Var
  aList : TStringList;
  aDataset : TSQLDBRestDataset;
  S : String;

begin
  aDataset:=GetComponent(0) as TSQLDBRestDataset;
  if not assigned(aDataset.Connection) then
    exit;
  aList:=TStringList.Create;
  try
    IDERestUtils.GetConnectionList(aDataset.Connection,aList);
    For S in aList do
      Proc(S);
  finally
    aList.Free;
  end;
end;

{ TSQLDBRestResourceNamePropertyEditor }

function TSQLDBRestResourceNamePropertyEditor.GetAttributes: TPropertyAttributes;
begin
  Result:=[paMultiSelect,paValueList];
end;

procedure TSQLDBRestResourceNamePropertyEditor.GetValues(Proc: TGetStrProc);
Var
  aList : TStringList;
  aDataset : TSQLDBRestDataset;
  S : String;

begin
  aDataset:=GetComponent(0) as TSQLDBRestDataset;
  if not assigned(aDataset.Connection) then
    exit;
  aList:=TStringList.Create;
  try
    IDERestUtils.GetResourceList(aDataset.Connection,aList);
    For S in aList do
      Proc(S);
  finally
    aList.Free;
  end;
end;

{ TProjectElementIDPropertyEditor }

function TProjectElementIDPropertyEditor.GetHTMLFileName: String;
begin
  Result:=HTMLTools.GetProjectHTMLFile;
end;

{ THTMLFileNamePropertyEditor }

procedure THTMLFileNamePropertyEditor.Edit;

var
  Dlg : TOpenDialog;
  FN : String;


begin
  Dlg:=TOpenDialog.Create(Application);
  With Dlg do
    try
      Filter:=rsHTMLFIleFilters;
      FN:=GetStrValue;
      if FN<>ExpandFileName(FN) then
        FN:=TFileDescHTMLFragment.HTMLBaseDir+FN;
      FileName:=FN;
      if Execute then
        begin
        FN:=FileName;
        FN:=ExtractRelativePath(TFileDescHTMLFragment.HTMLBaseDir,FN);
        SetStrValue(FN);
        end;
    finally
      Free;
    end;
end;

function THTMLFileNamePropertyEditor.GetAttributes: TPropertyAttributes;
begin
  Result:=[paMultiSelect,paDialog];
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
  Result:=LazarusIDE.ActiveProject.CustomData.Values[PJSProjectHTMLBaseDir];
  if Result='' then
    Result:=ExtractFilePath(LazarusIDE.ActiveProject.ProjectInfoFile);
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

{ TElementIDPropertyEditor }

function TElementIDPropertyEditor.GetHTMLFileName: String;

var
  aComponent : TPersistent;

begin
  Result:='';
  aComponent:=GetComponent(0);
  if (aComponent is TComponent) then
    Result:=HTMLTools.GetHTMLFileForComponent(TComponent(aComponent));
end;

function TElementIDPropertyEditor.GetAttributes: TPropertyAttributes;
begin
  Result:=[paMultiSelect,paValueList];
end;

function TElementIDPropertyEditor.ProcessID(S: String): String;
begin
  Result:=S;
end;

procedure TElementIDPropertyEditor.GetValues(Proc: TGetStrProc);

Var
  FN,aTag : String;
  aTags : TStringDynArray;

begin
  FN:=GetHTMLFileName;
  if FN<>'' then
    begin
    aTags:=HTMLTools.GetTagIDs(FN);
    For aTag in aTags do
      Proc(ProcessID(aTag));
    end;

end;


{ THTMLElementActionListComponentEditor }

constructor THTMLElementActionListComponentEditor.Create(AComponent: TComponent;
  ADesigner: TComponentEditorDesigner);
begin
  inherited Create(AComponent, ADesigner);
  FCompDesigner := ADesigner;
end;

destructor THTMLElementActionListComponentEditor.Destroy;
begin
  inherited Destroy;
end;

procedure THTMLElementActionListComponentEditor.Edit;
var
  AActionList: THTMLCustomElementActionList;
  AEditor: THTMLActionListEditorForm;
begin
  AActionList := GetComponent as THTMLCustomElementActionList;
  if Not Assigned(AActionList) then
    raise Exception.Create('THTMLElementActionListComponentEditor.Edit AActionList=nil');
  AEditor := FindActionEditor(AActionList);
  if not Assigned(AEditor) then begin
    AEditor := THTMLActionListEditorForm.Create(Application);
    AEditor.lstActionName.ItemIndex := -1;
    AEditor.ComponentDesigner := Self.FCompDesigner;
    AEditor.ComponentEditor := Self;
    AEditor.HTMLActionList:=AActionList;
  end;
  SetPopupModeParentForPropertyEditor(AEditor);
  AEditor.ShowOnTop;
end;

procedure THTMLElementActionListComponentEditor.CreateMissing;

var
  AActionList: THTMLCustomElementActionList;
  aCount : Integer;

begin
  AActionList := GetComponent as THTMLCustomElementActionList;
  if Not Assigned(AActionList) then
    raise Exception.Create('THTMLElementActionListComponentEditor.Edit AActionList=nil');
  aCount:=CreateMissingActions(Self,aActionList);
  if (aCount<>-1) then
    ShowMessage(Format(rsHTMLActionsCreated,[aCount]));
end;

function THTMLElementActionListComponentEditor.GetVerbCount: Integer;
begin
  Result := 2;
end;

function THTMLElementActionListComponentEditor.GetVerb(Index: Integer): string;
begin
  case Index of
    0 : Result := rsActionListComponentEditor;
    1 : Result := rsActionListCreateMissing;
  end;
end;

procedure THTMLElementActionListComponentEditor.ExecuteVerb(Index: Integer);
begin
  case Index of
    0 : Edit;
    1 : CreateMissing;
  end;
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

