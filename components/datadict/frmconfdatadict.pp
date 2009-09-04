unit frmconfdatadict;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  EditBtn, StdCtrls, idedatadict, ButtonPanel, ldd_consts;

type

  { TConfigIDEDataDictForm }

  TConfigIDEDataDictForm = class(TForm)
    ButtonPanel1: TButtonPanel;
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
  If (FEDD.FileName='') or not (FileExistsUTF8(FEDD.FileName)) then
    Raise Exception.Create(SErrSelectExe);
  If (DEDD.Text='') or not (DirectoryExistsUTF8(DEDD.Text)) then
    Raise Exception.Create(SErrSelectDir)
end;


procedure TConfigIDEDataDictForm.FormCreate(Sender: TObject);
begin
  //
  Caption := ldd_Configuredatadictionary;
  Label1.Caption:= ldd_Databasedesktopapplication;
  FEDD.Filter:= ldd_Filenameapplicationsfilter;
  Label2.Caption:= ldd_Defaultdatadictdirectory;
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

