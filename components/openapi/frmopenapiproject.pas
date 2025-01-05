unit frmopenapiproject;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, EditBtn, ComCtrls, ButtonPanel, Spin,
  fpopenapi.codegen, fraopenapisettings, lazopenapictrl;

type
  { TOpenAPIProjectForm }

  TOpenAPIProjectForm = class(TForm)
    BPOpenAPIProject: TButtonPanel;
    cbThreadedServer: TCheckBox;
    DEBaseDir: TDirectoryEdit;
    edtUnitsBaseName: TEdit;
    fraSettings: TGeneratorSettingsFrame;
    lblUnitsBaseName: TLabel;
    lblBaseDir: TLabel;
    lblPort: TLabel;
    PCProjects: TPageControl;
    RBServerHTTP: TRadioButton;
    CBServerConsole: TRadioButton;
    rbClientGUI: TRadioButton;
    rbClientCommandLine: TRadioButton;
    RBServerGUI: TRadioButton;
    SEPort: TSpinEdit;
    TSClient: TTabSheet;
    TSServer: TTabSheet;
    TSAPI: TTabSheet;
    procedure DEBaseDirEditingDone(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormCreate(Sender: TObject);
  private
    FallowedTypes: TOpenAPIProjectTypes;
    FGenerator: TOpenAPICodeGen;
    function GetBaseDir: String;
    function GetClientProjectType: TIDEProjectType;
    function GetHTTPPort: Word;
    function GetOpenAPIFileName: String;
    function GetServerProjectType: TIDEProjectType;
    function GetThreadedServer: Boolean;
    function GetUnitsBaseName: String;
    procedure SetAllowedTypes(AValue: TOpenAPIProjectTypes);
    procedure SetBaseDir(AValue: String);
    procedure SetGenerator(AValue: TOpenAPICodeGen);
    procedure SetOpenAPIFileName(AValue: String);
    procedure SetUnitsBaseName(AValue: String);
  public
    Property Generator : TOpenAPICodeGen read FGenerator Write SetGenerator;
    Property BaseDir : String Read GetBaseDir Write SetBaseDir;
    Property UnitsBaseName : String Read GetUnitsBaseName Write SetUnitsBaseName;
    Property OpenAPIFileName : String Read GetOpenAPIFileName Write SetOpenAPIFileName;
    Property AllowedTypes : TOpenAPIProjectTypes Read FallowedTypes Write SetAllowedTypes;
    Property ClientProjectType : TIDEProjectType Read GetClientProjectType;
    Property ServerProjectType : TIDEProjectType Read GetServerProjectType;
    Property HTTPPort : Word Read GetHTTPPort;
    Property ThreadedServer : Boolean Read GetThreadedServer;
  end;

var
  OpenAPIProjectForm: TOpenAPIProjectForm;

implementation

{$R *.lfm}

{ TOpenAPIProjectForm }

procedure TOpenAPIProjectForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  if ModalResult=mrOK then
    FraSettings.SaveSettings;
end;

procedure TOpenAPIProjectForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  CanClose:=(ModalResult<>mrOK);
  CanClose:=CanClose or ((DEBaseDir.Directory<>'') and (fraSettings.OpenAPIFileName<>''))
end;

procedure TOpenAPIProjectForm.FormCreate(Sender: TObject);
begin
  fraSettings.Clear;
end;

procedure TOpenAPIProjectForm.DEBaseDirEditingDone(Sender: TObject);
begin
  fraSettings.InitFileNameEdits(BaseDir);
end;

function TOpenAPIProjectForm.GetOpenAPIFileName: String;
begin
  Result:=fraSettings.OpenAPIFileName;
end;

function TOpenAPIProjectForm.GetClientProjectType: TIDEProjectType;
begin
  if rbClientGUI.Checked then
    Result:=iptGUI
  else
    Result:=iptCmdLine;
end;

function TOpenAPIProjectForm.GetHTTPPort: Word;
begin
  Result:=SEPort.Value;
end;

function TOpenAPIProjectForm.GetBaseDir: String;
begin
  Result:=DEBaseDir.Directory;
end;

function TOpenAPIProjectForm.GetServerProjectType: TIDEProjectType;
begin
  if rbServerGUI.Checked then
    Result:=iptGUI
  else if RBServerHTTP.Checked then
    Result:=iptHTTPServer
  else
    Result:=iptCmdLine;
end;

function TOpenAPIProjectForm.GetThreadedServer: Boolean;
begin
  Result:=cbThreadedServer.Checked;
end;

function TOpenAPIProjectForm.GetUnitsBaseName: String;
begin
  Result:=edtUnitsBaseName.Text;
end;

procedure TOpenAPIProjectForm.SetAllowedTypes(AValue: TOpenAPIProjectTypes);
var
  lClient,lServer : Boolean;
begin
  FallowedTypes:=AValue;
  lClient:=optClient in aValue;
  lServer:=optServer in aValue;
  TSServer.TabVisible:=lServer;
  TSClient.TabVisible:=lClient;
  fraSettings.HideAdditionalControls(lClient,lServer);
end;

procedure TOpenAPIProjectForm.SetBaseDir(AValue: String);
begin
  DEBaseDir.Directory:=aValue;
  fraSettings.InitFileNameEdits(aValue);
end;

procedure TOpenAPIProjectForm.SetGenerator(AValue: TOpenAPICodeGen);
begin
  if FGenerator=AValue then Exit;
  FGenerator:=AValue;
  fraSettings.Generator:=AValue;
  fraSettings.ShowSettings;
end;

procedure TOpenAPIProjectForm.SetOpenAPIFileName(AValue: String);
begin
  fraSettings.OpenAPIFileName:=aValue;
end;

procedure TOpenAPIProjectForm.SetUnitsBaseName(AValue: String);
begin
  edtUnitsBaseName.Text:=aValue;
end;

end.

