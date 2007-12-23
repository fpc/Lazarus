unit frmmain; 

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, ActnList,
  Menus, ComCtrls, ExtCtrls, DbCtrls, dbf, db, DBGrids, fpdataexporter, fpstdexports;

type

  { TMainForm }

  TMainForm = class(TForm)
    AExport: TAction;
    AQuit: TAction;
    AOpen: TAction;
    ANew: TAction;
    ALMain: TActionList;
    DSData: TDatasource;
    DBFData: TDbf;
    Exporter: TFPDataExporter;
    GData: TDBGrid;
    MIExport: TMenuItem;
    NBData: TDBNavigator;
    ILMain: TImageList;
    MMMain: TMainMenu;
    MIQuit: TMenuItem;
    MINew: TMenuItem;
    MIOpen: TMenuItem;
    MISep: TMenuItem;
    MFile: TMenuItem;
    ODDBF: TOpenDialog;
    PButtons: TPanel;
    SDExport: TSaveDialog;
    SDDBF: TSaveDialog;
    StandardExportFormats1: TStandardExportFormats;
    TBMain: TToolBar;
    ToolButton1: TToolButton;
    TBRTFExport: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    procedure AExportExecute(Sender: TObject);
    procedure ANewExecute(Sender: TObject);
    procedure AOpenExecute(Sender: TObject);
    procedure AQuitExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure HaveData(Sender: TObject);
  private
    { private declarations }
    FDesignCaption: string;
    procedure CreateNewDataset(AFileName: String);
    procedure OpenDataset(AFileName: String);
  public
    { public declarations }
  end; 

var
  MainForm: TMainForm;

implementation

uses gendata;

{ TMainForm }


procedure TMainForm.AOpenExecute(Sender: TObject);
begin
  If ODDBF.Execute then
    OpenDataset(ODDBF.FileName);
end;

procedure TMainForm.AQuitExecute(Sender: TObject);
begin
  Close;
end;

procedure TMainForm.ANewExecute(Sender: TObject);
begin
  if SDDBF.Execute then
    begin
    CreateNewDataset(SDDBF.FileName);
    OpenDataset(SDDBF.FileName);
    end;
end;

procedure TMainForm.AExportExecute(Sender: TObject);
begin
  Exporter.Execute;
end;

procedure TMainForm.CreateNewDataset(AFileName : String);

begin
  With TDBFGenerator.Create do
    try
      OutputFile:=AFileName;
      GenerateData;
    finally
      Free;
    end;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  FDesignCaption:=Caption;
end;

procedure TMainForm.HaveData(Sender: TObject);
begin
  (Sender as TAction).Enabled:=DBFData.Active and Not (DBFData.EOF and DBFDATA.BOF);
end;

procedure TMainForm.OpenDataset(AFileName : String);

begin
  DBFData.Close;
  DBFData.TableName:=AFileName;
  DBFData.Open;
  Caption:=Format('%s (%s)',[FDesignCaption,AFileName]);
end;

initialization
  {$I frmmain.lrs}

end.

