unit frmnewhttpapp;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, EditBtn,
  StdCtrls, Spin, ButtonPanel;

type

  { TNewHTTPApplicationForm }

  TNewHTTPApplicationForm = class(TForm)
    ButtonPanel1: TButtonPanel;
    CBRegisterFiles: TCheckBox;
    CBthreads: TCheckBox;
    CBDefaultFileLocation: TCheckBox;
    DEDocumentroot: TDirectoryEdit;
    ELocation: TEdit;
    LSEPort: TLabel;
    LELocation: TLabel;
    LDEDocumentRoot: TLabel;
    SEPort: TSpinEdit;
    procedure CBDefaultFileLocationChange(Sender: TObject);
    procedure CBRegisterFilesChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    function GetD: String;
    function GetL: String;
    function GetP: Integer;
    function GetS: Boolean;
    function GetSD: Boolean;
    function Gett: Boolean;
    procedure LocalizeForm;
    { private declarations }
  public
    { public declarations }
    Property ServeFiles : Boolean Read GetS;
    Property Location : String Read GetL;
    Property Directory : String Read GetD;
    Property Port: Integer Read GetP;
    Property Threaded : Boolean Read Gett;
    Property ServeFilesDefault : Boolean Read GetSD;
  end;

var
  NewHTTPApplicationForm: TNewHTTPApplicationForm;

implementation

uses fpWebStrConsts;

{$R *.lfm}

{ TNewHTTPApplicationForm }

procedure TNewHTTPApplicationForm.FormCreate(Sender: TObject);
begin
  LocalizeForm;
end;

procedure TNewHTTPApplicationForm.CBRegisterFilesChange(Sender: TObject);

Var
  B : Boolean;

begin
  B:=GetS;
  ELocation.Enabled:=B;
  DEDocumentRoot.Enabled:=B;
  CBDefaultFileLocation.Enabled:=Not B;
  if not CBDefaultFileLocation.Enabled then
    CBDefaultFileLocation.Checked:=False
end;

procedure TNewHTTPApplicationForm.CBDefaultFileLocationChange(Sender: TObject);
begin
  CBRegisterFiles.Enabled:=Not CBDefaultFileLocation.Checked;
  ELocation.Enabled:=Not CBDefaultFileLocation.Checked;
  if not CBRegisterFiles.Enabled then
    begin
    CBRegisterFiles.Checked:=False;
    ELocation.Text:='';
    end;
end;

procedure TNewHTTPApplicationForm.LocalizeForm;

begin
  Caption:=sNewHTTPApp;
  CBRegisterFiles.Caption:=sRegisterFiles;
  LELocation.Caption:=sDocumentLocation;
  LDEDocumentRoot.Caption:=sDocumentRoot;
  LSEPort.Caption:=sHTTPPort;
  CBthreads.Caption:=sUseThreads;
end;

function TNewHTTPApplicationForm.GetD: String;
begin
  Result:=DEDocumentRoot.Text;
end;

function TNewHTTPApplicationForm.GetL: String;
begin
  Result:=ELocation.Text;
end;

function TNewHTTPApplicationForm.GetP: Integer;
begin
  Result:=SEPort.Value;
end;

function TNewHTTPApplicationForm.GetS: Boolean;
begin
  Result:=CBRegisterFiles.Checked;
end;

function TNewHTTPApplicationForm.GetSD: Boolean;
begin
  Result:=CBDefaultFileLocation.Checked;
end;

function TNewHTTPApplicationForm.Gett: Boolean;
begin
  Result:=CBThreads.Checked;
end;

end.

