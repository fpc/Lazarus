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
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs, Buttons,
  StdCtrls, LR_Const, LR_Class, LR_Desgn, Dbf, DB, DBGrids, LR_DBSet, LR_PGrid,
  Menus, ComCtrls, ActnList, Lr_e_txt, Lr_e_htm, LR_E_CSV;

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
    comboIndex: TComboBox;
    Datasource1: TDatasource;
    Dbf1: TDbf;
    dbGrid1: TdbGrid;
    frCSVExport1: TfrCSVExport;
    frDBDataSet1: TfrDBDataSet;
    lblExpr: TLabel;
    lblIndex: TLabel;
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
    sbar: TStatusBar;
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
    procedure comboIndexSelect(Sender: TObject);
    procedure dbGrid1TitleClick(Column: TColumn);
    procedure frmMainCreate(Sender: TObject);
  private
    { private declarations }
    procedure UpdateAppTranslation;
    procedure SetIndex(const aIndexName: string);
    procedure OpenReport(const aFileName:string);
    procedure UpdateActiveReport;
  public
    { public declarations }
  end; 

var
  frmMain: TfrmMain;

implementation

uses gettext,translations,lclproc;

resourcestring
  cerOpenReport     = 'Open report';
  cerNewReport      = 'New report';
  cerEditReport     = 'Edit Report';
  cerPreviewReport  = 'Preview report';
  cerPrintReport    = 'Print report';
  cerPrintGrid      = 'Print grid';
  cerNotImplemented = 'This feature is not yet implemented!';
  cerPrepareFailed  = 'PrepareReport Failed!';
  cerIndex          = 'Index';
  cerNone           = 'none';
  cerIndexFields    = 'Index Fields: %s';
  cerOpenReportFirst= 'Open report first';
  cerActiveReport   = 'Active report: %s';
  cerHintNewReport  = 'Create and edit a empty report';
  cerHintOpenReport = 'Open an existing report';
  cerHintEditReport = 'Edit active report';
  cerHintPrevReport = 'Preview active report';
  cerHintPrevGrid   = 'Print preview current DbGrid content';
  cerHintPrnReport  = 'Print directly the active report (i.e. without preview)';
  cerHintCloseApp   = 'Close application';
  cerAppCaption     = 'LazReport Test Suite';
  

{ TfrmMain }

procedure TfrmMain.UpdateAppTranslation;
begin
  accOpenReport.Caption := cerOpenReport;
  accNewReport.Caption := cerNewReport;
  accEditReport.Caption := cerEditReport;
  accPreviewReport.Caption := cerPreviewReport;
  accPrintReport.Caption := cerPrintReport;
  accPrintGrid.Caption := cerPrintGrid;
  lblIndex.Caption:=cerIndex;

  accNewReport.Hint := cerHintNewReport;
  accOpenReport.Hint := cerHintOpenReport;
  accEditReport.Hint := cerHintEditReport;
  accPreviewReport.Hint := cerHintPrevReport;
  accPrintGrid.Hint := cerHintPrevGrid;
  accPrintReport.Hint := cerHintPrnReport;
  accClose.Hint := cerHintCloseApp;

  caption := cerAppCaption;
end;

procedure TfrmMain.ApplicationProperties1ShowHint(var HintStr: string;
  var CanShow: Boolean; var HintInfo: THintInfo);
begin
  sbar.SimpleText := HintStr;
  CanShow := False;
  sbar.SimplePanel := HintStr<>'';
end;

procedure TfrmMain.accExportToTextExecute(Sender: TObject);
begin
  TheReport.LoadFromFile(ExtractFilePath(ParamStrUTF8(0))+'salida.lrf');
  if TheReport.PrepareReport then
    ShowMessage(cerNotImplemented)
    //TheReport.ExportTo(TfrTextExportFilter, 'salida.txt')
  else
    ShowMessage(cerPrepareFailed);
end;

procedure TfrmMain.accExportToHtmlExecute(Sender: TObject);
begin
  TheReport.LoadFromFile(ExtractFilePath(ParamStrUTF8(0))+'salida.lrf');
  if TheReport.PrepareReport then begin
    TheReport.ExportTo(TfrHTMExportFilter, 'salida.html');
    ShowMessage(cerNotImplemented);
  end else
    ShowMessage(cerPrepareFailed);
end;

procedure TfrmMain.accExportToCSVExecute(Sender: TObject);
begin
  TheReport.LoadFromFile(ExtractFilePath(ParamStrUTF8(0))+'salida.lrf');
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
  if TheReport.FileName='' then
    raise Exception.Create(cerOpenReportFirst);
  TheReport.DesignReport;
end;

procedure TfrmMain.accNewReportExecute(Sender: TObject);
begin
  TheReport.Pages.Clear;
  TheReport.FileName:=SUntitled;
  TheReport.DesignReport;
  UpdateActiveReport;
end;

procedure TfrmMain.accOpenReportExecute(Sender: TObject);
begin
  if OpenDialog1.Execute then begin
    OpenReport(OpenDialog1.FileName);
  end;
end;

procedure TfrmMain.accPreviewReportExecute(Sender: TObject);
begin
  DataSource1.Enabled:=false;
  TheReport.ShowReport;
  DataSource1.Enabled:=true;
end;

procedure TfrmMain.accPrintGridExecute(Sender: TObject);
begin
  PG.PreviewReport;
end;

procedure TfrmMain.accPrintReportExecute(Sender: TObject);
begin
  if TheReport.PrepareReport then
    TheReport.PrintPreparedReport('1',1)
  else
    ShowMessage(cerPrepareFailed);
end;

procedure TfrmMain.comboIndexSelect(Sender: TObject);
var
  i: Integer;
begin
  i := comboIndex.ItemIndex;
  if i<=0 then begin
    SetIndex('');
  end else begin
    SetIndex(comboIndex.Items[i]);
  end;
end;

procedure TfrmMain.dbGrid1TitleClick(Column: TColumn);
begin
  if CompareText(Column.FieldName,'year')=0 then
    SetIndex('ByYear')
  else
  if CompareText(Column.FieldName,'company')=0 then
    SetIndex('ByCompany')
  else
  if CompareText(Column.FieldName,'country')=0 then
    SetIndex('ByCountry')
  else
    SetIndex('');
end;

function dbgwstr(w: widestring):string;
var
  p: pchar;
  i: integer;
begin
  p := @w[1];
  i := length(w)*2;

  result := '';

  while i>0 do begin
    if p^ in [#0..#31,#128..#255] then
      result := result + '#'+IntToStr(ord(p^))
    else
      result := result + p^;

    inc(p);
    dec(i);
  end;
end;

procedure TfrmMain.frmMainCreate(Sender: TObject);
var
  i: integer;
  w: widestring;
  s: string;
begin

  UpdateAppTranslation;
  
  dbf1.close;
  dbf1.FilePath := 'db/';
  dbf1.TableName := 'disco.dbf';
  dbf1.open;
  
  comboIndex.Clear;
  comboIndex.Items.Add(cerNone);
  for i:=0 to Dbf1.Indexes.Count-1 do
    comboIndex.Items.Add(Dbf1.Indexes[i].Name);
  SetIndex('');
  
  if FileExistsUTF8(ExtractFilePath(ParamStrUTF8(0))+'salida.lrf') then
    OpenReport(ExtractFilePath(ParamStrUTF8(0))+'salida.lrf');

  s := 'LazReport/Title/Value';
  SetLength(W, UTF8Length(s));
  UTF8ToUnicode(PUnicodeChar(w), pchar(s), UTF8Length(s)+1);

  WriteLn('W Len=',Length(W),' data="',dbgwstr(w),'"');
end;

procedure TfrmMain.SetIndex(const aIndexName: string);
begin
  dbf1.IndexName := aIndexName;
  lblExpr.Caption:= format(cerIndexFields, [dbf1.IndexFieldNames]);
end;

procedure TfrmMain.OpenReport(const aFileName: string);
begin
  TheReport.LoadFromFile(aFileName);
  UpdateActiveReport;
end;

procedure TfrmMain.UpdateActiveReport;
begin
  SBar.Panels[0].Text:= format(cerActiveReport, [TheReport.FileName]);
end;

procedure TranslateResStrings;
var
  Lang, FallbackLang: String;
begin
  GetLanguageIDs(Lang,FallbackLang); // in unit gettext
  TranslateUnitResourceStrings('LCLStrConsts','../../../../lcl/languages/lclstrconsts.%s.po', Lang,FallbackLang);
  TranslateUnitResourceStrings('MainCallEditor','languages/calleditorwithpkg.%s.po', Lang,FallbackLang);
  TranslateUnitResourceStrings('Lr_const','../../languages/lr_const.%s.po', Lang,FallbackLang);
end;

initialization
  {$I maincalleditor.lrs}
  TranslateResStrings;
  

end.

