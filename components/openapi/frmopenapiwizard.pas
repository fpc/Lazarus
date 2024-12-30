unit frmopenapiwizard;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ButtonPanel, ExtCtrls, EditBtn, StdCtrls, fraopenapisettings, fpopenapi.codegen;

type

  { TOpenapiWizardForm }

  TOpenapiWizardForm = class(TForm)
    bpOpenAPICodegen: TButtonPanel;
    edtBaseFileName: TFileNameEdit;
    fraSettings: TGeneratorSettingsFrame;
    lblBaseOutput: TLabel;
    pnlBottom: TPanel;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
  private
    FGenerator: TOpenAPICodeGen;
    function GetAddToProject: Boolean;
    function GetBaseFileName: String;
    function GetOpenAPIFileName: String;
    function GetOpenGeneratedFiles: Boolean;
    procedure SetBaseFileName(AValue: String);
    procedure SetGenerator(AValue: TOpenAPICodeGen);
    procedure SetOpenAPIFileName(AValue: String);
  public
    procedure InitFileNameEdits(const aBaseDir: String);
    Property Generator : TOpenAPICodeGen read FGenerator Write SetGenerator;
    Property BaseFileName : String Read GetBaseFileName Write SetBaseFileName;
    Property OpenAPIFileName : String Read GetOpenAPIFileName Write SetOpenAPIFileName;
    Property OpenGeneratedFiles : Boolean Read GetOpenGeneratedFiles;
    Property AddToProject : Boolean Read GetAddToProject;
  end;

var
  OpenapiWizardForm: TOpenapiWizardForm;

implementation

{$R *.lfm}

{ TOpenapiWizardForm }

procedure TOpenapiWizardForm.FormCreate(Sender: TObject);
begin
  OpenAPIFileName:='';
  BaseFileName:='';
end;

procedure TOpenapiWizardForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  if ModalResult=mrOK then
    FraSettings.SaveSettings;
end;

function TOpenapiWizardForm.GetBaseFileName: String;
begin
  Result:=edtBaseFileName.FileName;
end;

function TOpenapiWizardForm.GetAddToProject: Boolean;
begin
  Result:=fraSettings.AddFilesToProject;
end;

function TOpenapiWizardForm.GetOpenAPIFileName: String;
begin
  Result:=fraSettings.OpenAPIFileName;
end;

function TOpenapiWizardForm.GetOpenGeneratedFiles: Boolean;
begin
  Result:=fraSettings.OpenGeneratedFiles;
end;

procedure TOpenapiWizardForm.SetBaseFileName(AValue: String);
begin
  edtBaseFileName.FileName:=aValue;
end;

procedure TOpenapiWizardForm.SetGenerator(AValue: TOpenAPICodeGen);
begin
  if FGenerator=AValue then Exit;
  FGenerator:=AValue;
  fraSettings.Generator:=AValue;
  fraSettings.ShowSettings;
end;

procedure TOpenapiWizardForm.SetOpenAPIFileName(AValue: String);
begin
  fraSettings.OpenAPIFileName:=aValue;
end;

procedure TOpenapiWizardForm.InitFileNameEdits(const aBaseDir: String);
begin
  fraSettings.InitFileNameEdits(aBaseDir);
  edtBaseFileName.InitialDir:=aBaseDir;
end;

end.

