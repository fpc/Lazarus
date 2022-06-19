unit pas2jscompedits;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ProjectIntf, PropEdits, ComponentEditors, fpjsondataset, dbpropedits,
  db, stub.htmlfragment, stub.htmlactions, stub.data.htmlactions, stub.restdataset,
  stub.webwidget, stub.bootstrapwidgets, stub.bootstraptablewidget, stub.bulmawidgets,
  stub.templateloader, stub.jsondataset, stub.dbwebwidget, stub.dbhtmlwidgets,
  stub.fprpcclient;

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


  { TPas2JSRPCClientComponentEditor }

  TPas2JSRPCClientComponentEditor = class(TComponentEditor)
  private
    FClient : TPas2JSRPCClient;
  protected
    Procedure CreateServiceClient;
  public
    property Client : TPas2JSRPCClient read FClient write FClient;
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


  { TDBHTMLElementActionFieldProperty }

  TDBHTMLElementActionFieldProperty = Class(TFieldProperty)
    procedure FillValues(const Values: TStringList); override;
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


implementation

uses
  // LCL/Lazarus
  Types, IDEWindowIntf, controls, forms, dialogs, formeditingintf, lazideintf, idemsgintf, IDEExternToolIntf, Menuintf,
  idehtmltools,
  frmHTMLActionsEditor, strpas2jscomponents, pas2jsrestutils, pas2jsrestcmd, frmpas2jsedithtml, p2jselementactions,
  frmcreaterpcserviceclient;

{ TDBHTMLElementActionFieldProperty }

procedure TDBHTMLElementActionFieldProperty.FillValues(const Values: TStringList
  );
Var
  Act : TDBHTMLInputElementAction;

begin
  Act:=TDBHTMLInputElementAction(GetComponent(0));
  if Assigned(Act) and Assigned(Act.DataSource) then
  ListDataSourceFields(Act.DataSource,Values)
end;

{ TPas2JSRPCClientComponentEditor }

procedure TPas2JSRPCClientComponentEditor.CreateServiceClient;

Var
  aFile,aSvcFile : TLazProjectFile;
  aFileName : string;
  frm : TCreateRPCClientServiceForm;

begin
  aFileName:='';
  aFile:=LazarusIDE.GetProjectFileWithRootComponent(Client.Owner);
  if Assigned(aFile) then
    aFileName:=aFile.CustomData[Client.Name+'_filename'];
  if aFileName<>'' then
    begin
    aSvcFile:=LazarusIDE.ActiveProject.FindFile(aFileName,[pfsfOnlyProjectFiles]);
    // look harder - not sure if the IDE does not do this by itself.
    if aSvcFile=Nil then
      aSvcFile:=LazarusIDE.ActiveProject.FindFile(ExtractFileName(aFileName),[pfsfOnlyProjectFiles]);
    end;
  frm:=TCreateRPCClientServiceForm.Create(Application);
  try
    frm.Client:=Self.Client;
    frm.ProjectDir:=ExtractFilePath(LazarusIDE.ActiveProject.ProjectInfoFile);
    if (aFileName<>'') then
      begin
      frm.UnitFileName:=aFileName;
      frm.ServiceUnitName:=ChangeFileExt(ExtractFileName(aFileName),'');
      frm.AllowAddUnitToProject:=(aSvcFile=Nil);
      end;
    if (frm.ShowModal=mrOK) and (frm.UnitFileName<>'') and FileExists(frm.UnitFileName) then
      begin
      if Assigned(aFile) then
        aFile.CustomData[Client.Name+'_filename']:=frm.UnitFileName;
      Modified;
      if frm.AddUnitToProject then
        LazarusIDE.DoOpenEditorFile(frm.UnitFileName,0,0,[ofAddToProject]);
      end;
  finally
    frm.Free;
  end;
end;

function TPas2JSRPCClientComponentEditor.GetVerbCount: Integer;
begin
  Result:=1;
end;

function TPas2JSRPCClientComponentEditor.GetVerb(Index: Integer): string;
begin
  Case Index of
    0 : Result:=rsCreateServiceClient;
  else
    Result:=inherited GetVerb(Index);
  end;
end;

procedure TPas2JSRPCClientComponentEditor.ExecuteVerb(Index: Integer);
begin
  FClient:=GetComponent as TPas2jsRPCClient;
  case Index of
    0 : CreateServiceClient;
  else
    inherited ExecuteVerb(Index);
  end;
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


{ TElementIDSelectorPropertyEditor }

function TElementIDSelectorPropertyEditor.ProcessID(S: String): String;
begin
  Result:='#'+S;
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
    aList.Sort;
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
    aList.Sort;
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
        FN:=TIDEHTMLTools.HTMLBaseDir+FN;
      FileName:=FN;
      if Execute then
        begin
        FN:=FileName;
        FN:=ExtractRelativePath(TIDEHTMLTools.HTMLBaseDir,FN);
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


end.

