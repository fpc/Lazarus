unit frmcreaterpcserviceclient;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fphttpclient, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ButtonPanel, EditBtn, fpjson, stub.fprpcclient;

type

  { TCreateRPCClientServiceForm }

  TCreateRPCClientServiceForm = class(TForm)
    btnPreview: TButton;
    bpForm: TButtonPanel;
    cbAddToProject: TCheckBox;
    cbForceJSValueResult: TCheckBox;
    cbPreferNativeInt: TCheckBox;
    cbCopyToClipBoard: TCheckBox;
    edtUnitName: TEdit;
    edtURL: TEdit;
    edtFileName: TFileNameEdit;
    APIClient: TFPHTTPClient;
    lblPreview: TLabel;
    lblFileName: TLabel;
    lblUnitName: TLabel;
    lblURL: TLabel;
    mPreview: TMemo;
    procedure btnPreviewClick(Sender: TObject);
    procedure edtUnitNameEditingDone(Sender: TObject);
    procedure OKButtonClick(Sender: TObject);
  private
    FClient: TPas2jsRPCClient;
    FAPI : String;
    FProjectDir: String;
    procedure GenerateFile;
    procedure GenerateUnit(aSource: TStrings);
    function GetAddUnitToProject: Boolean;
    function GetAllowAddUnitToProject: Boolean;
    function GetJSONAPI: TJSONObject;
    function GetServiceUnitName: string;
    function GetUnitFileName: string;
    function GetURL: String;
    procedure SetAddUnitToProject(AValue: Boolean);
    procedure SetAllowAddUnitToProject(AValue: Boolean);
    procedure SetClient(AValue: TPas2jsRPCClient);
    procedure SetProjectDir(AValue: String);
    procedure SetServiceUnitName(AValue: string);
    procedure SetUnitFileName(AValue: string);
    procedure SetURL(AValue: String);

  public
    Property Client : TPas2jsRPCClient Read FClient Write SetClient;
    Property URL : String Read GetURL Write SetURL;
    Property ServiceUnitName : string Read GetServiceUnitName Write SetServiceUnitName;
    Property UnitFileName : string Read GetUnitFileName Write SetUnitFileName;
    Property AddUnitToProject : Boolean Read GetAddUnitToProject Write SetAddUnitToProject;
    Property AllowAddUnitToProject : Boolean Read GetAllowAddUnitToProject Write SetAllowAddUnitToProject;
    Property ProjectDir : String Read FProjectDir Write SetProjectDir;
  end;

var
  CreateRPCClientServiceForm: TCreateRPCClientServiceForm;

implementation

{$R *.lfm}

uses clipBrd, iderpccodegen, strpas2jscomponents;

{ TCreateRPCClientServiceForm }

procedure TCreateRPCClientServiceForm.GenerateFile;

Var
  L : TStrings;

begin
  L:=TStringList.Create;
  try
    GenerateUnit(L);
    if edtFileName.FileName<>''  then
      L.SaveToFile(edtFileName.FileName);
    if (edtFileName.FileName='') or cbCopyToClipBoard.Checked then
      clipBoard.AsText:=L.text;
  finally
    L.Free;
  end;
end;

function TCreateRPCClientServiceForm.GetJSONAPI: TJSONObject;

Var
  D : TJSONData;

begin
  if FAPI='' then
    FAPI:=APIClient.Get(edtURL.Text);
  D:=GetJSON(FAPI);
  if D is TJSONObject then
    Result:=D as TJSONObject
  else
    begin
    D.Free;
    ShowMessage(Format(rsInvalidAPIReturned,[edtURL.Text,FAPI]));
    FAPI:='';
    end;
end;

function TCreateRPCClientServiceForm.GetServiceUnitName: string;
begin
   Result:=edtUnitName.text;
end;

function TCreateRPCClientServiceForm.GetUnitFileName: string;
begin
  Result:=edtFileName.text;
end;

procedure TCreateRPCClientServiceForm.GenerateUnit(aSource: TStrings);

Var
  Gen : TAPIClientCodeGen;
  Opts : TClientCodeOptions;
  API : TJSONObject;

begin
  Gen:=Nil;
  API:=GetJSONAPI;
  try
    Opts:=[];
    if cbForceJSValueResult.checked then
      Include(Opts,ccoForceJSValueResult);
    if cbPreferNativeInt.Checked then
      Include(Opts,ccoPreferNativeInt);
    Gen:=TAPIClientCodeGen.Create(Self);
    Gen.API:=API;
    Gen.Options:=Opts;
    Gen.OutputUnitName:=edtUnitName.Text;
    Gen.Execute;
    aSource.AddStrings(Gen.Source,True);
  finally
    Gen.Free;
    API.Free;
  end;
end;

function TCreateRPCClientServiceForm.GetAddUnitToProject: Boolean;
begin
  Result:=cbAddToProject.Checked;
end;

function TCreateRPCClientServiceForm.GetAllowAddUnitToProject: Boolean;
begin
  Result:=cbAddToProject.Enabled;
end;

procedure TCreateRPCClientServiceForm.OKButtonClick(Sender: TObject);
begin
  GenerateFile;
end;

procedure TCreateRPCClientServiceForm.btnPreviewClick(Sender: TObject);
begin
  GenerateUnit(mPreview.Lines);
end;

procedure TCreateRPCClientServiceForm.edtUnitNameEditingDone(Sender: TObject);
begin
  if edtFileName.FileName='' then
    edtFileName.FileName:=IncludeTrailingPathDelimiter(FProjectDir)+edtUnitName.Text+'.pp';
end;

procedure TCreateRPCClientServiceForm.SetClient(AValue: TPas2jsRPCClient);
var
  aURL : String;

begin
  if FClient=AValue then Exit;
  FClient:=AValue;
  if Assigned(FClient) then
    begin
    aURL:=FClient.URL;
    if (aURL<>'') and (aURL[Length(aURL)]<>'/') then
      aURL:=aURL+'/';
    Self.URL:=aURL+'API';
    end;
end;

procedure TCreateRPCClientServiceForm.SetProjectDir(AValue: String);
begin
  if FProjectDir=AValue then Exit;
  FProjectDir:=AValue;
  edtFileName.InitialDir:=FProjectDir;
end;

procedure TCreateRPCClientServiceForm.SetServiceUnitName(AValue: string);
begin
  edtUnitName.Text:=aValue;
end;

procedure TCreateRPCClientServiceForm.SetUnitFileName(AValue: string);
begin
  edtFileName.Text:=aValue;
end;

function TCreateRPCClientServiceForm.GetURL: String;
begin
  Result:=edtURL.text;
end;

procedure TCreateRPCClientServiceForm.SetAddUnitToProject(AValue: Boolean);
begin
  cbAddToProject.Checked:=aValue;
end;

procedure TCreateRPCClientServiceForm.SetAllowAddUnitToProject(AValue: Boolean);
begin
  cbAddToProject.Enabled:=aValue;
end;

procedure TCreateRPCClientServiceForm.SetURL(AValue: String);
begin
  edtURL.text:=aValue;
end;

end.

