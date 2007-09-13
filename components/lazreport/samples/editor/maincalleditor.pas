 {
  This library is free software; you can redistribute it and/or modify it
  under the terms of the GNU Library General Public License as published by
  the Free Software Foundation; either version 2 of the License, or (at your
  option) any later version.

  This program is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public License
  for more details.

  You should have received a copy of the GNU Library General Public License
  along with this library; if not, write to the Free Software Foundation,
  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
  
  Author : Jesus Reyes
  
  Abstract:
    Show how to use TFrReport component for call editor
}

unit maincalleditor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, Buttons,
  StdCtrls, LR_Class, LR_Desgn, Dbf, DB, DBGrids, LR_DBSet, LR_PGrid, Menus,
  ComCtrls, ActnList, Lr_e_txt, Lr_e_htm, LR_E_CSV;

type

  { TfrmMain }

  TfrmMain = class(TForm)
    accNewReport: TAction;
    accEditReport: TAction;
    accPreviewReport: TAction;
    accPrintGrid: TAction;
    accPrintReport: TAction;
    accClose: TAction;
    accExportToText: TAction;
    accExportToHtml: TAction;
    accOpenReport: TAction;
    accExportToCSV: TAction;
    ActionList1: TActionList;
    ApplicationProperties1: TApplicationProperties;
    btnCallEditor: TButton;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    btnOpenReport: TButton;
    Datasource1: TDatasource;
    Dbf1: TDbf;
    dbGrid1: TdbGrid;
    frCSVExport1: TfrCSVExport;
    frDBDataSet1: TfrDBDataSet;
    MainMenu1: TMainMenu;
    MenuItem1: TMenuItem;
    MenuItem10: TMenuItem;
    MenuItem11: TMenuItem;
    MenuItem12: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    MenuItem7: TMenuItem;
    MenuItem8: TMenuItem;
    MenuItem9: TMenuItem;
    OpenDialog1: TOpenDialog;
    PG: TfrPrintGrid;
    StatusBar1: TStatusBar;
    TheReport: TfrReport;
    procedure accExportToCSVExecute(Sender: TObject);
    procedure accExportToHtmlExecute(Sender: TObject);
    procedure accExportToTextExecute(Sender: TObject);
    procedure ApplicationProperties1ShowHint(var HintStr: string;
      var CanShow: Boolean; var HintInfo: THintInfo);
    procedure accCloseExecute(Sender: TObject);
    procedure accEditReportExecute(Sender: TObject);
    procedure accNewReportExecute(Sender: TObject);
    procedure accOpenReportExecute(Sender: TObject);
    procedure accPreviewReportExecute(Sender: TObject);
    procedure accPrintGridExecute(Sender: TObject);
    procedure accPrintReportExecute(Sender: TObject);
    procedure frmMainCreate(Sender: TObject);
  private
    { private declarations }
    procedure TranslateGUI;
  public
    { public declarations }
  end; 

var
  frmMain: TfrmMain;

implementation

uses gettext,translations;

resourcestring
  cerOpenReport     = 'Open report';
  cerNewReport      = 'New report';
  cerEditReport     = 'Edit Report';
  cerPreviewReport  = 'Preview report';
  cerPrintReport    = 'Print report';
  cerPrintGrid      = 'Print grid';
  cerNotImplemented = 'This feature is not yet implemented!';
  cerPrepareFailed  = 'PrepareReport Failed!';

{ TfrmMain }

procedure TfrmMain.ApplicationProperties1ShowHint(var HintStr: string;
  var CanShow: Boolean; var HintInfo: THintInfo);
begin
  StatusBar1.SimpleText := HintStr;
  CanShow := False;
end;

procedure TfrmMain.accExportToTextExecute(Sender: TObject);
begin
  TheReport.LoadFromFile(ExtractFilePath(ParamStr(0))+'salida.lrf');
  if TheReport.PrepareReport then
    ShowMessage(cerNotImplemented)
    //TheReport.ExportTo(TfrTextExportFilter, 'salida.txt')
  else
    ShowMessage(cerPrepareFailed);
end;

procedure TfrmMain.accExportToHtmlExecute(Sender: TObject);
begin
  TheReport.LoadFromFile(ExtractFilePath(ParamStr(0))+'salida.lrf');
  if TheReport.PrepareReport then begin
    TheReport.ExportTo(TfrHTMExportFilter, 'salida.html');
    ShowMessage(cerNotImplemented);
  end else
    ShowMessage(cerPrepareFailed);
end;

procedure TfrmMain.accExportToCSVExecute(Sender: TObject);
begin
  TheReport.LoadFromFile(ExtractFilePath(ParamStr(0))+'salida.lrf');
  if TheReport.PrepareReport then begin
    TheReport.ExportTo(TfrCSVExportFilter, 'salida.csv');
  end else
    ShowMessage(cerPrepareFailed);
end;

procedure TfrmMain.accCloseExecute(Sender: TObject);
begin
  Close;
end;

procedure TfrmMain.accEditReportExecute(Sender: TObject);
begin
  TheReport.LoadFromFile(ExtractFilePath(ParamStr(0))+'salida.lrf');
  TheReport.DesignReport;
end;

procedure TfrmMain.accNewReportExecute(Sender: TObject);
begin
  TheReport.Pages.Clear;
  TheReport.DesignReport;
end;

procedure TfrmMain.accOpenReportExecute(Sender: TObject);
begin
  if OpenDialog1.Execute then begin
    TheReport.LoadFromFile(OpenDialog1.FileName);
    TheReport.DesignReport;
  end;
end;

procedure TfrmMain.accPreviewReportExecute(Sender: TObject);
begin
  TheReport.LoadFromFile(ExtractFilePath(ParamStr(0))+'salida.lrf');
  TheReport.ShowReport;
end;

procedure TfrmMain.accPrintGridExecute(Sender: TObject);
begin
  PG.PreviewReport;
end;

procedure TfrmMain.accPrintReportExecute(Sender: TObject);
begin
  TheReport.LoadFromFile(ExtractFilePath(ParamStr(0))+'salida.lrf');
  if TheReport.PrepareReport then
    TheReport.PrintPreparedReport('1',1)
  else
    ShowMessage(cerPrepareFailed);
end;

procedure TfrmMain.frmMainCreate(Sender: TObject);
var
  Lang, FallbackLang: String;
begin
  GetLanguageIDs(Lang,FallbackLang); // in unit gettext
  TranslateUnitResourceStrings('LCLStrConsts','../../../../lcl/languages/lclstrconsts.%s.po', Lang,FallbackLang);
  TranslateUnitResourceStrings('MaincallEditor','languages/maincalleditor.%s.po', Lang,FallbackLang);
  TranslateUnitResourceStrings('Lr_const','../../languages/lr_const.%s.po', Lang,FallbackLang);
  
  TranslateGUI;
  
  dbf1.close;
  dbf1.FilePath := 'db/';
  dbf1.TableName := 'disco.dbf';
  dbf1.open;
end;

procedure TfrmMain.TranslateGUI;
begin
  accOpenReport.Caption := cerOpenReport;
  accNewReport.Caption := cerNewReport;
  accEditReport.Caption := cerEditReport;
  accPreviewReport.Caption := cerPreviewReport;
  accPrintReport.Caption := cerPrintReport;
  accPrintGrid.Caption := cerPrintGrid;
end;

initialization
  {$I maincalleditor.lrs}

end.

