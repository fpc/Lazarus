unit frmnewhttpapp;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, EditBtn,
  StdCtrls, Spin, ButtonPanel;

type

  { TNewHTTPApplicationForm }
  TServeFiles = (sfNoFiles, sfSingleRoute, sfDefaultRoute);
  TStandardModule = (smHTTPRoute,smWeb,smHTML,smFile,smRPC,smWebData,smExtDirect);

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
    procedure cbStandardModuleChange(Sender: TObject);
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
  public
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
    smHTTPRoute : Result:=rsNoModule;
    smWeb  : Result:=rsWebModule;
    smHTML : Result:=rsHTMLWebModul;
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
  cbStandardModule.ItemIndex:=0;
  cbStandardModuleChange(cbStandardModule);
end;

procedure TNewHTTPApplicationForm.cbStandardModuleChange(Sender: TObject);
begin
  // Disable competing file route when using smHTTPRoute.
  RBDefaultRoute.Enabled:=(Sender as TComboBox).ItemIndex<>0;
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
  Assert(cbStandardModule.ItemIndex>=0, 'TNewHTTPApplicationForm.GetSM: ItemIndex<0');
  Result:=TStandardModule(cbStandardModule.ItemIndex);
end;

function TNewHTTPApplicationForm.GetT: Boolean;
begin
  Result:=CBThreads.Checked;
end;

end.

