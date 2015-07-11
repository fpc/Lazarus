unit frmrpcmoduleoptions; 

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  ButtonPanel, StdCtrls;

type

  { TJSONRPCModuleOptionsForm }

  TJSONRPCModuleOptionsForm = class(TForm)
    ButtonPanel1: TButtonPanel;
    CBRegisterHandlers: TCheckBox;
    CBRegisterModule: TCheckBox;
    ERegModuleClassName: TEdit;
    ERegModuleName: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    procedure CBRegisterHandlersChange(Sender: TObject);
    procedure CBRegisterModuleChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    function GetHP: String;
    function GetJC: String;
    function GetRH: Boolean;
    function GetRM: Boolean;
    procedure SetHP(const AValue: String);
    procedure SetJC(const AValue: String);
    { private declarations }
  public
    { public declarations }
    Property RegisterHandlers : Boolean Read GetRH;
    Property RegisterModule  : Boolean Read GetRM;
    Property JSONRPCClass : String Read GetJC Write SetJC;
    Property HTTPPath : String Read GetHP Write SetHP;
  end;

var
  JSONRPCModuleOptionsForm: TJSONRPCModuleOptionsForm; 

implementation

resourcestring
  sCaption = 'Create a new JSON-RPC module';
  sRegisterJSON = 'Register JSON-RPC handlers in factory';
  sJSONClass = 'JSON-RPC class';
  sRegisterWebM = 'Register web module';
  sHTTPPath = 'HTTP Path';

{$R *.lfm}

{ TJSONRPCModuleOptionsForm }

procedure TJSONRPCModuleOptionsForm.CBRegisterHandlersChange(Sender: TObject);
begin
   ERegModuleCLassName.Enabled:=CBRegisterHandlers.Checked;
end;

procedure TJSONRPCModuleOptionsForm.CBRegisterModuleChange(Sender: TObject);
begin
   ERegModuleName.Enabled:=CBRegisterModule.Checked;
end;

procedure TJSONRPCModuleOptionsForm.FormCreate(Sender: TObject);
begin
  Caption := sCaption;
  CBRegisterHandlers.Caption := sRegisterJSON;
  CBRegisterModule.Caption := sRegisterWebM;
  Label1.Caption:= sJSONClass;
  Label2.Caption:= sHTTPPath;
end;

function TJSONRPCModuleOptionsForm.GetHP: String;
begin
  Result:=ERegModuleName.Text;
end;

function TJSONRPCModuleOptionsForm.GetJC: String;
begin
  Result:=ERegModuleClassName.Text;
end;

function TJSONRPCModuleOptionsForm.GetRH: Boolean;
begin
  Result:=CBRegisterHandlers.Checked;
end;

function TJSONRPCModuleOptionsForm.GetRM: Boolean;
begin
  Result:=CBRegisterModule.Checked;
end;

procedure TJSONRPCModuleOptionsForm.SetHP(const AValue: String);
begin
  ERegModuleName.Text:=AValue;
end;

procedure TJSONRPCModuleOptionsForm.SetJC(const AValue: String);
begin
  ERegModuleClassName.Text:=AValue;
end;

end.

