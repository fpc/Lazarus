unit frmnewhttpapp;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, EditBtn,
  StdCtrls, Spin, ButtonPanel;

type

  { TNewHTTPApplicationForm }
  TServeFiles = (sfNoFiles, sfSingleRoute, sfDefaultRoute);
  TStandardModule = (smNone,smWeb,smHTTP,smFile,smRPC,smWebData, smExtDirect);

  TNewHTTPApplicationForm = class(TForm)
    ButtonPanel1: TButtonPanel;
    CBthreads: TCheckBox;
    cbStandardModule: TComboBox;
    DEDocumentroot: TDirectoryEdit;
    ELocation: TEdit;
    GBFileServing: TGroupBox;
    lblStandardModule: TLabel;
    LDEDocumentRoot: TLabel;
    LSEPort: TLabel;
    RBSingleRoute: TRadioButton;
    RBDefaultRoute: TRadioButton;
    RBNoFiles: TRadioButton;
    SEPort: TSpinEdit;
    procedure FormCreate(Sender: TObject);
    procedure RBNoFilesChange(Sender: TObject);
  private
    function GetD: String;
    function GetR: String;
    function GetP: Integer;
    function GetS: TServeFiles;
    function GetSM: TStandardModule;
    function GetT: Boolean;
    procedure LocalizeForm;
    { private declarations }
  public
    { public declarations }
    Property ServeFiles : TServeFiles Read GetS;
    Property FileRoute : String Read GetR;
    Property Directory : String Read GetD;
    Property Port: Integer Read GetP;
    Property Threaded : Boolean Read Gett;
    Property StandardModule : TStandardModule Read GetSM;
  end;

var
  NewHTTPApplicationForm: TNewHTTPApplicationForm;

Function StandardModuleToString(aModule : TStandardModule) : String;

implementation

uses fpWebStrConsts;

{$R *.lfm}

Function StandardModuleToString(aModule : TStandardModule) : String;

begin
  case aModule of
    smNone : Result:=rsNoModule;
    smWeb  : Result:=rsWebModule;
    smHTTP : Result:=rsHTMLWebModul;
    smFile : Result:=rsFileModule;
    smRPC :  Result:=rsWebJSONRPCMo;
    smWebData : Result:=rsWebDataProvi;
    smExtDirect : Result:=rsWebExtDirect;
  end;
end;

{ TNewHTTPApplicationForm }

procedure TNewHTTPApplicationForm.FormCreate(Sender: TObject);
var
  SF: TServeFiles;
  SM : TStandardModule;
begin
  SF:=ServeFiles;
  ELocation.Enabled:=(Sf=sfSingleRoute);
  DEDocumentRoot.Enabled:=(Sf<>sfNoFiles);
  LocalizeForm;
  For SM in TStandardModule do
    cbStandardModule.Items.Add(StandardModuleToString(SM));
end;

procedure TNewHTTPApplicationForm.RBNoFilesChange(Sender: TObject);
var
  SF: TServeFiles;
begin
  SF:=ServeFiles;
  ELocation.Enabled:=(Sf=sfSingleRoute);
  if not ELocation.Enabled then
    ELocation.Text:='';
  DEDocumentRoot.Enabled:=(Sf<>sfNoFiles);
  if not DEDocumentRoot.Enabled then
    DEDocumentRoot.Directory:='';

end;


procedure TNewHTTPApplicationForm.LocalizeForm;

begin
  Caption:=sNewHTTPApp;
  GBFileServing.Caption:=sFileServing;
  RBNoFiles.Caption:=sNoFiles;
  RBSingleRoute.Caption:=sRegisterFiles;
  RBDefaultRoute.Caption:=sDefaultRouteServesFiles;
  LDEDocumentRoot.Caption:=sDocumentRoot;
  LSEPort.Caption:=sHTTPPort;
  CBthreads.Caption:=sUseThreads;
  lblStandardModule.Caption:=sStandardModule;
end;

function TNewHTTPApplicationForm.GetD: String;
begin
  Result:=DEDocumentRoot.Text;
end;

function TNewHTTPApplicationForm.GetR: String;
begin
  Result:=ELocation.Text;
end;

function TNewHTTPApplicationForm.GetP: Integer;
begin
  Result:=SEPort.Value;
end;

function TNewHTTPApplicationForm.GetS: TServeFiles;
begin
  if RBNoFiles.Checked then
    Result:=sfNoFiles
  else if RBSingleRoute.Checked then
    Result:=sfSingleRoute
  else
    Result:=sfDefaultRoute;
end;

function TNewHTTPApplicationForm.GetSM: TStandardModule;
begin
  if cbStandardModule.ItemIndex<0 then
    Result:=smNone
  else
    Result:=TStandardModule(cbStandardModule.ItemIndex);
end;


function TNewHTTPApplicationForm.GetT: Boolean;
begin
  Result:=CBThreads.Checked;
end;

end.

