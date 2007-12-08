unit frmconfdatadict;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, EditBtn,
  StdCtrls, idedatadict;

type

  { TConfigIDEDataDictForm }

  TConfigIDEDataDictForm = class(TForm)
    BOK: TButton;
    BCancel: TButton;
    DEDD: TDirectoryEdit;
    FEDD: TFileNameEdit;
    Label1: TLabel;
    Label2: TLabel;
    procedure BOKClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { private declarations }
    procedure CheckData;
    procedure DataDictToForm;
    procedure FormToDataDict;
  public
    { public declarations }
  end; 

var
  ConfigIDEDataDictForm: TConfigIDEDataDictForm;

Function ShowConfigIDEDataDictDialog : Boolean;

implementation

Resourcestring
  SErrSelectExe = 'Please select an existing database desktop application';
  SErrSelectDir = 'Please select an existing directory';

Function ShowConfigIDEDataDictDialog : Boolean;

begin
  With TConfigIDEDataDictForm.Create(Application) do
    try
      Result:=(ShowModal=MROK)
    finally
      Free;
    end;
end;


{ TConfigIDEDataDictForm }


procedure TConfigIDEDataDictForm.BOKClick(Sender: TObject);
begin
  CheckData;
  FormToDataDict;
end;

procedure TConfigIDEDataDictForm.CheckData;

begin
  If (FEDD.FileName='') or not (FileExists(FEDD.FileName)) then
    Raise Exception.Create(SErrSelectExe);
  If (DEDD.Text='') or not (DirectoryExists(DEDD.Text)) then
    Raise Exception.Create(SErrSelectDir)
end;


procedure TConfigIDEDataDictForm.FormCreate(Sender: TObject);
begin
  InitDDSettings;
  DataDictToForm;
end;

procedure TConfigIDEDataDictForm.FormToDataDict;

begin
  DataDesktopBinary:=FEDD.FileName;
  DefaultDictionaryDir:=DEDD.Text;
end;


procedure TConfigIDEDataDictForm.DataDictToForm;

begin
  FEDD.FileName:=DataDesktopBinary;
  DEDD.Text:=DefaultDictionaryDir;
end;


initialization
  {$I frmconfdatadict.lrs}

end.

