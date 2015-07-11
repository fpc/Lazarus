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
    DEDocumentroot: TDirectoryEdit;
    ELocation: TEdit;
    LSEPort: TLabel;
    LELocation: TLabel;
    LDEDocumentRoot: TLabel;
    SEPort: TSpinEdit;
    procedure CBRegisterFilesChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    function GetD: String;
    function GetL: String;
    function GetP: Integer;
    function GetS: Boolean;
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

function TNewHTTPApplicationForm.Gett: Boolean;
begin
  Result:=CBThreads.Checked;
end;

end.

