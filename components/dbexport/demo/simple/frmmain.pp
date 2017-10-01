unit frmmain; 

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, ActnList,
  Menus, ComCtrls, ExtCtrls, DbCtrls, dbf, db, DBGrids, fpdbexport, fpcsvexport,
  fpfixedexport, fpSQLExport, fpSimpleXMLExport, fpsimplejsonexport,
  fpdbfexport, fptexexport, fprtfexport;

type

  { TMainForm }

  TMainForm = class(TForm)
    AExportRTF: TAction;
    AExportTeX: TAction;
    AExportSQL: TAction;
    AExportDBF: TAction;
    AExportXML: TAction;
    AExportJSON: TAction;
    AExportFixed: TAction;
    AExportCSV: TAction;
    AQuit: TAction;
    AOpen: TAction;
    ANew: TAction;
    ALMain: TActionList;
    ExCSV: TCSVExporter;
    DSData: TDatasource;
    DBFData: TDbf;
    ExFixed: TFixedLengthExporter;
    ExDBF: TFPDBFExport;
    GData: TDBGrid;
    NBData: TDBNavigator;
    ILMain: TImageList;
    MMMain: TMainMenu;
    MExport: TMenuItem;
    MIRTFExport: TMenuItem;
    MITeXExport: TMenuItem;
    MISQLExport: TMenuItem;
    MIExportDLG: TMenuItem;
    MIExportSep: TMenuItem;
    MIExportDBF: TMenuItem;
    MIExportXML: TMenuItem;
    MIExportCSV: TMenuItem;
    MIExportFixed: TMenuItem;
    MIExportJSON: TMenuItem;
    MIQuit: TMenuItem;
    MINew: TMenuItem;
    MIOpen: TMenuItem;
    MISep: TMenuItem;
    MFile: TMenuItem;
    ODDBF: TOpenDialog;
    ExRTF: TRTFExporter;
    SDExport: TSaveDialog;
    SDDBF: TSaveDialog;
    ExJSON: TSimpleJSONExporter;
    ExXML: TSimpleXMLExporter;
    ExSQL: TSQLExporter;
    TBMain: TToolBar;
    ExTeX: TTeXExporter;
    ToolButton1: TToolButton;
    TBTexExport: TToolButton;
    TBRTFExport: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    TBCSVExport: TToolButton;
    TBFixedExport: TToolButton;
    TBJSONExport: TToolButton;
    TBXMLExport: TToolButton;
    TBDBFExport: TToolButton;
    TBSQLExport: TToolButton;
    procedure AExportCSVExecute(Sender: TObject);
    procedure AExportDBFExecute(Sender: TObject);
    procedure AExportFixedExecute(Sender: TObject);
    procedure AExportJSONExecute(Sender: TObject);
    procedure AExportRTFExecute(Sender: TObject);
    procedure AExportSQLExecute(Sender: TObject);
    procedure AExportTeXExecute(Sender: TObject);
    procedure AExportXMLExecute(Sender: TObject);
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
    procedure DoExport(E: TCustomDatasetExporter; const ATitle, AFilter: String);
  public
    { public declarations }
  end; 

var
  MainForm: TMainForm;

implementation

{$R frmmain.lfm}

uses frmBaseConfigExport,gendata;

Resourcestring
  SCSVTitle  = 'Export data to CSV file.';
  SCSVFilter = 'CSV files|*.csv|All Files|*.*';
  SXMLTitle  = 'Export data to XML file.';
  SXMLFilter = 'XML files|*.xml|All Files|*.*';
  SDBFTitle  = 'Export data to DBase file.';
  SDBFFilter = 'DBF files|*.dbf|All Files|*.*';
  SSQLTitle  = 'Export data to SQL file.';
  SSQLFilter = 'SQL files|*.sql|All Files|*.*';
  SRTFTitle  = 'Export data to RTF file.';
  SRTFFilter = 'RTF files|*.rtf|All Files|*.*';
  STEXTitle  = 'Export data to TeX file.';
  STeXFilter = 'TeX files|*.tex|All Files|*.*';
  SJSONTitle = 'Export data to JSON file.';
  SJSONFilter = 'JSON files|*.json|All Files|*.*';
  SFixedTitle = 'Export data to fixed-length text file.';
  SFixedFilter = 'Text files|*.txt|All Files|*.*';

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

procedure TMainForm.DoExport(E : TCustomDatasetExporter; Const ATitle,AFilter : String);
var
  s: String;
begin
  if MIExportDLG.Checked then
  begin
    If not ShowBaseExportConfig(E) then
      Exit;
  end
  else
  begin
    SDExport.FileName := '';
    SDExport.Title:=ATitle;
    SDExport.Filter:=AFilter;
    s := AFilter.Split('|')[1];
    Delete(s, 1, 1);
    SDExport.DefaultExt:= s;
    E.Dataset.First;
    if SDExport.Execute then
    begin
      if (E is TFPDBFExport) then
        (E as TFPDBFExport).FileName:=SDExport.FileName
      else
      if (E is TCustomFileExporter) then
        (E as TCustomFileExporter).FileName:=SDExport.FileName;
    end else
      exit;
  end;
  E.Execute;
end;

procedure TMainForm.AExportCSVExecute(Sender: TObject);
begin
  DoExport(ExCSV,SCSVTitle,SCSVFilter);
end;

procedure TMainForm.AExportDBFExecute(Sender: TObject);
begin
  DoExport(ExDBF,SDBFTitle,SDBFFilter);
end;

procedure TMainForm.AExportFixedExecute(Sender: TObject);
begin
  DoExport(ExFixed,SFixedTitle,SFixedFilter);
end;

procedure TMainForm.AExportJSONExecute(Sender: TObject);
begin
  DoExport(ExJSON,SJSONTitle,SJSONFilter);
end;

procedure TMainForm.AExportRTFExecute(Sender: TObject);
begin
  DoExport(ExRTF,SRTFTitle,SRTFFilter);
end;

procedure TMainForm.AExportSQLExecute(Sender: TObject);
begin
  DoExport(ExSQL,SSQLTitle,SSQLFilter);
end;

procedure TMainForm.AExportTeXExecute(Sender: TObject);
begin
  DoExport(ExTeX,STeXTitle,STeXFilter);
end;

procedure TMainForm.AExportXMLExecute(Sender: TObject);
begin
  DoExport(ExXML,SXMLTitle,SXMLFilter);
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

end.

