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
  Inc., 51 Franklin Street - Fifth Floor, Boston, MA 02110-1335, USA.
  
  Author : Jesus Reyes
  
  Abstract:
    Show how to use TFrReport component for call editor
}

unit maincalleditor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Variants, FileUtil, LResources, Forms, Controls, Graphics,
  Dialogs, Buttons, StdCtrls, LR_Const, LR_Class, LR_Desgn, Dbf, DB, DBGrids,
  LR_DBSet, IniFiles, LR_PGrid, Menus, ComCtrls, ActnList, Lr_e_txt, Lr_e_htm,
  LR_E_CSV, LR_DSet, LR_BarC, LR_RRect, LR_Shape, LR_ChBox, lconvencoding,
  lr_e_gen, lr_utils, LCLProc, ExtCtrls, custompreview, LR_Pars, LR_e_htmldiv,
  lrAddFunctionLibrary, LazLogger, LazFileUtils, LCLTranslator;

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
    accThumbnails: TAction;
    accExportToDbg: TAction;
    accComposite: TAction;
    accCustomPreview: TAction;
    ActionList1: TActionList;
    ApplicationProperties1: TApplicationProperties;
    btnMasterDetail: TToggleBox;
    btnNewReport: TButton;
    btnCustomPreview: TButton;
    btnPrintGrid: TButton;
    btnPreviewReport: TButton;
    btnEditReport: TButton;
    btnPrintReport: TButton;
    btnOpenReport: TButton;
    btnImageList: TButton;
    btnComposite: TButton;
    btnExportLast: TButton;
    comboIndex: TComboBox;
    frDbMaster: TfrDBDataSet;
    frHtmlDivExport1: TfrHtmlDivExport;
    lrAddFunctionLibrary1: TlrAddFunctionLibrary;
    mastergrid: TDBGrid;
    Panel1: TPanel;
    srcMaster: TDatasource;
    master: TDbf;
    srcDetail: TDatasource;
    DetailAUTHOR: TStringField;
    DetailCOMPANY1: TStringField;
    DetailCOMPANYID: TLongintField;
    DetailCOUNTRY1: TStringField;
    DetailCOUNTRYID: TLongintField;
    DetailIN_STOCK: TBooleanField;
    DetailLAST_SELL: TDateField;
    DetailNOTE: TStringField;
    DetailPRICE: TFloatField;
    DetailQTY: TSmallintField;
    DetailTITLE: TStringField;
    DetailYEAR: TSmallintField;
    Detail: TDbf;
    LookCountries: TDbf;
    LookCompanies: TDbf;
    grid: TDBGrid;
    frBarCodeObject1: TfrBarCodeObject;
    frCheckBoxObject1: TfrCheckBoxObject;
    Composite: TfrCompositeReport;
    frCSVExport1: TfrCSVExport;
    frDbDetail: TfrDBDataSet;
    frRoundRectObject1: TfrRoundRectObject;
    frShapeObject1: TfrShapeObject;
    frUserDataset1: TfrUserDataset;
    lblExpr: TLabel;
    lblIndex: TLabel;
    MainMenu1: TMainMenu;
    MenuItem1: TMenuItem;
    MenuItem10: TMenuItem;
    MenuItem11: TMenuItem;
    MenuItem12: TMenuItem;
    MenuItem13: TMenuItem;
    MenuItem14: TMenuItem;
    MenuItem15: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    MenuItem7: TMenuItem;
    MenuItem8: TMenuItem;
    MenuItem9: TMenuItem;
    OpenDialog1: TOpenDialog;
    PrintGrid: TFrPrintGrid;
    sbar: TStatusBar;
    TheReport: TfrReport;
    procedure accCompositeExecute(Sender: TObject);
    procedure accCustomPreviewExecute(Sender: TObject);
    procedure accExportToCSVExecute(Sender: TObject);
    procedure accExportToDbgExecute(Sender: TObject);
    procedure accExportToHtmlExecute(Sender: TObject);
    procedure accExportToTextExecute(Sender: TObject);
    procedure accThumbnailsExecute(Sender: TObject);
    procedure ApplicationProperties1ShowHint(var HintStr: string;
      var CanShow: Boolean; var HintInfo: THintInfo);
    procedure accCloseExecute(Sender: TObject);
    procedure accEditReportExecute(Sender: TObject);
    procedure accNewReportExecute(Sender: TObject);
    procedure accOpenReportExecute(Sender: TObject);
    procedure accPreviewReportExecute(Sender: TObject);
    procedure accPrintGridExecute(Sender: TObject);
    procedure accPrintReportExecute(Sender: TObject);
    procedure btnExportLastClick(Sender: TObject);
    procedure btnMasterDetailClick(Sender: TObject);
    procedure comboIndexSelect(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure gridTitleClick(Column: TColumn);
    procedure frmMainCreate(Sender: TObject);
    procedure TheReportBeginDoc;
    procedure TheReportEnterRect(Memo: TStringList; View: TfrView);
    procedure TheReportExportFilterSetup(Sender: TfrExportFilter);
    procedure TheReportGetValue(const ParName: String; var ParValue: Variant);
    procedure TheReportUserFunction(const AName: String; p1, p2, p3: Variant;
      var Val: Variant);
  private
    { private declarations }
    FImageList: TStringList;
    FImageListIndex: Integer;
    FObjCount: Integer;
    FCountryIndex: Integer;
    FCurReport: string;
    procedure UpdateAppTranslation;
    procedure SetIndex(const aIndexName: string);
    procedure OpenReport(const aFileName:string);
    procedure UpdateActiveReport;
    function GetDataPath: string;
    procedure MasterDetail;
    procedure LoadConfig;
    procedure SaveConfig;
  public
    { public declarations }
  end; 

var
  frmMain: TfrmMain;

implementation

uses gettext,translations, lazutf8;

{$R *.lfm}

resourcestring
  cerOpenReport     = 'Open report';
  cerNewReport      = 'New report';
  cerEditReport     = 'Edit Report';
  cerPreviewReport  = 'Preview report';
  cerPrintReport    = 'Print report';
  cerPrintGrid      = 'Print grid';
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
  cerHintThumbnails = 'This Action will Load and Preview a thumbnails Report';
  cerThumbnails     = 'Thumbnails';
  cerHintComposite  = 'This Action will Load and Preview a composite Report';
  cerComposite      = 'Composite';


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

  accThumbnails.Hint := cerHintThumbnails;
  accThumbnails.Caption := cerThumbnails;

  accComposite.Hint := cerHintComposite;
  accComposite.Caption := cerComposite;
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
  if TheReport.PrepareReport then
    TheReport.ExportTo(TfrTextExportFilter, 'salida.txt')
  else
    ShowMessage(cerPrepareFailed);
end;

procedure TfrmMain.accThumbnailsExecute(Sender: TObject);
begin
  OpenReport(ExtractFilePath(ParamStrUTF8(0))+'thumbnails.lrf');
  FImageListIndex := 0;
  FImageList := FindAllFiles('../../../../images/components','*.png',false);
  try
    if FImageList.Count>0 then begin
      frUserDataset1.RangeEndCount:=FImageList.Count;
      TheReport.ShowReport;
    end else
      ShowMessage('png images not found');
  finally
    FImageList.Free;
    FImageList:=nil;
  end;
end;

procedure TfrmMain.accExportToHtmlExecute(Sender: TObject);
begin
  if TheReport.PrepareReport then begin
    TheReport.ExportTo(TfrHTMExportFilter, 'salida.html');
  end else
    ShowMessage(cerPrepareFailed);
end;

procedure TfrmMain.accExportToCSVExecute(Sender: TObject);
begin
  if TheReport.PrepareReport then begin
    TheReport.ExportTo(TfrCSVExportFilter, 'salida.csv');
  end else
    ShowMessage(cerPrepareFailed);
end;

procedure TfrmMain.accCompositeExecute(Sender: TObject);
var
  i: Integer;
  r: TFrReport;
begin
  // force reload of reports in case modified reports
  // were modified in designer at run-time
  for i := 0 to Composite.Reports.Count-1 do begin
    r := TfrReport(Composite.Reports[i]);
    r.LoadFromFile(r.FileName);
  end;
  Composite.ShowReport;
end;

procedure TfrmMain.accCustomPreviewExecute(Sender: TObject);
var
  F: TfrmCustomPreview;
begin
  F := TfrmCustomPreview.Create(self);
  try
    TheReport.Preview := F.frPreview1;
    TheReport.ShowReport;
    F.Caption := accCustomPreview.Caption;
    F.ShowModal;
  finally
    TheReport.Preview := nil;
    F.Free;
  end;
end;

procedure TfrmMain.accExportToDbgExecute(Sender: TObject);
begin
  if TheReport.PrepareReport then begin
    TheReport.ExportTo(TfrDbgExportFilter, 'salida.dbg');
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
  if btnMasterDetail.Checked then begin
    grid.DataSource := nil;
    MasterGrid.Datasource := nil;
  end;
  TheReport.ShowReport;
  if btnMasterDetail.Checked then begin
    MasterGrid.DataSource := srcMaster;
    grid.Datasource := srcDetail;
  end;
end;

procedure TfrmMain.accPrintGridExecute(Sender: TObject);
begin
  PrintGrid.PreviewReport;
end;

procedure TfrmMain.accPrintReportExecute(Sender: TObject);
begin
  if TheReport.PrepareReport then
    TheReport.PrintPreparedReport('1',1)
  else
    ShowMessage(cerPrepareFailed);
end;

procedure TfrmMain.btnExportLastClick(Sender: TObject);
begin
  if TheReport.PrepareReport then
    TheReport.ExportTo(nil, '')
  else
    ShowMessage(cerPrepareFailed);
end;

procedure TfrmMain.btnMasterDetailClick(Sender: TObject);
begin
  MasterDetail;
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

procedure TfrmMain.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  SaveConfig;
end;

procedure TfrmMain.gridTitleClick(Column: TColumn);
begin
  if CompareText(Column.FieldName,'year')=0 then
    SetIndex('ByYear')
  else
  if CompareText(Column.FieldName,'company1')=0 then
    SetIndex('ByCompany')
  else
  if CompareText(Column.FieldName,'country1')=0 then
    SetIndex('ByCountry')
  else
    SetIndex('');
end;

procedure TfrmMain.frmMainCreate(Sender: TObject);
const
  rptarr:array[0..1] of string[10] = ('rpta.lrf','rptb.lrf');
var
  i: integer;
  r: TfrReport;
  dataPath: string;
begin

  DebugLogger.MaxNestPrefixLen:=100;

  UpdateAppTranslation;

  DataPath := GetDataPath;

  LookCountries.Close;
  LookCountries.FilePath := datapath + 'db/';
  LookCountries.TableName := 'countries.dbf';
  LookCountries.open;

  LookCompanies.Close;
  LookCompanies.FilePath := datapath + 'db/';
  LookCompanies.TableName := 'companies.dbf';
  LookCompanies.Open;

  Detail.close;
  Detail.FilePath := datapath + 'db/';
  Detail.TableName := 'disco.dbf';
  Detail.open;

  Master.Close;
  Master.FilePath := datapath + 'db/';
  Master.TableName := 'countries.dbf';

  LoadConfig;

  comboIndex.Clear;
  comboIndex.Items.Add(cerNone);
  for i:=0 to Detail.Indexes.Count-1 do
    comboIndex.Items.Add(Detail.Indexes[i].Name);
  FCountryIndex := ComboIndex.Items.IndexOf('BYCOUNTRY');
  SetIndex('');

  if FileExistsUTF8(fCurReport) then
    OpenReport(fCurReport);

  for i:=Low(rptArr) to High(rptArr) do begin
    r := TfrReport.Create(self);
    r.LoadFromFile(rptArr[i]);
    Composite.Reports.Add(r);
  end;
  Composite.DoublePass:=true;

  frSelectHyphenDictionary(dataPath + 'hyph_es_ANY.dic');
end;

procedure TfrmMain.TheReportBeginDoc;
begin
  FObjCount := 0;
end;

procedure TfrmMain.TheReportEnterRect(Memo: TStringList; View: TfrView);
begin
  if (FImageList<>nil) and (View.Name='thumbnail') then begin
    TFrPictureView(View).Picture.LoadFromFile(FImageList[FImageListIndex]);
    Inc(FImageListIndex);
  end;
end;

procedure TfrmMain.TheReportExportFilterSetup(Sender: TfrExportFilter);
begin
  // parameters that all export filters can change
  Sender.UseProgressbar:=false;
  //Sender.BandTypes := [btMasterData]; // by default all bandtypes are processed

  if sender is TfrHtmExportFilter then begin
    //TfrTextExportFilter(sender).UseBOM := true; // false by default
    //TfrTextExportFilter(Sender).UsedFont:=0; // force to show used font dialog
    TfrHtmExportFilter(Sender).UseCSS:=false;  // true by default
  end else
  if sender is TfrCSVExportFilter then begin
    TfrCSVExportFilter(sender).UseBOM := true;    // default settings + BOM = excel compatible
    //TfrCSVExportFilter(sender).QuoteChar := '''';
    //TfrCSVExportFilter(sender).QuoteType := qtNone;
    //TfrCSVExportFilter(sender).Separator := ';';
  end else
  if Sender is TfrTextExportFilter then begin
    TfrTextExportFilter(sender).UseBOM := false;
    TfrTextExportFilter(Sender).UsedFont:=0; // force to show used font dialog
  end;
end;

procedure TfrmMain.TheReportGetValue(const ParName: String;
  var ParValue: Variant);
begin
  if ParName='vv1' then begin
    if FObjCount mod 3 = 0 then
      ParValue := 'áñâàÑ€¼2³¤€¼½¾'
    else
      ParValue := '12345678901234';
    Inc(FObjCount);
  end;
end;

procedure TfrmMain.TheReportUserFunction(const AName: String; p1, p2,
  p3: Variant; var Val: Variant);
var
  v: variant;
begin
  if AName='STOCKFLAG' then begin
    v := frParser.Calc(P1);
    if VarIsNull(V) or not boolean(v) then
      Val := ':('
    else
      Val:=  ':)';
  end;
end;

procedure TfrmMain.SetIndex(const aIndexName: string);
begin
  Detail.IndexName := aIndexName;
  lblExpr.Caption:= format(cerIndexFields, [Detail.IndexFieldNames]);
end;

procedure TfrmMain.OpenReport(const aFileName: string);
begin
  TheReport.LoadFromFile(aFileName);
  fCurReport := TheReport.FileName;
  UpdateActiveReport;
end;

procedure TfrmMain.UpdateActiveReport;
begin
  SBar.Panels[0].Text:= format(cerActiveReport, [TheReport.FileName]);
end;

function TfrmMain.GetDataPath: string;
begin
  result := ExtractFilePath(ParamStr(0));
  {$ifdef Darwin}
  result := IncludeTrailingPathDelimiter(ExpandFileName(result + '../../..'));
  {$endif}
end;

procedure TfrmMain.MasterDetail;
begin
  if mastergrid.Visible then begin
    MasterGrid.Visible := false;
    Grid.AnchorSideLeft.Control := Self;
    Grid.AnchorSideLeft.Side := asrTop;
    detail.MasterSource := nil;
    detail.MasterFields := '';
    detail.IndexName := '';
    comboIndexSelect(self);
    comboIndex.Enabled := true;
  end else begin
    Master.Open;
    detail.MasterSource := srcMaster;
    detail.MasterFields := 'CountryID';
    comboIndex.ItemIndex := FCountryIndex;
    comboIndexSelect(self);
    MasterGrid.Visible := true;
    Grid.AnchorSideLeft.Control := MasterGrid;
    Grid.AnchorSideLeft.Side := asrBottom;
    comboIndex.Enabled := false;
  end;
end;

procedure TfrmMain.LoadConfig;
var
  ini: TIniFile;
  s: string;
begin
  ini := TIniFile.Create(GetDataPath+'config.ini');
  s := ini.ReadString('General', 'LastReport', '');
  if s<>'' then
    fCurreport := s
  else
    fCurReport := GetDataPath + 'salida.lrf';
  TheReport.DefExportFileName := ini.ReadString('General','LastExportFilename','');
  TheReport.DefExportFilterClass := ini.ReadString('General','LastExportFilterClass', '');
  ini.Free;
end;

procedure TfrmMain.SaveConfig;
var
  ini: TIniFile;
begin
  ini := TIniFile.Create(GetDataPath+'config.ini');
  ini.WriteString('General', 'LastReport', fCurReport);
  ini.WriteString('General', 'LastExportFilename', theReport.DefExportFileName);
  ini.WriteString('General', 'LastExportFilterClass', theReport.DefExportFilterClass);
  ini.free;
end;

procedure TranslateResStrings;
var
  Lang, FallbackLang: String;
begin
  GetLanguageIDs(Lang,FallbackLang); // in unit gettext
  TranslateUnitResourceStrings('LCLStrConsts','../../../../lcl/languages/lclstrconsts.%s.po', Lang,FallbackLang);
  TranslateUnitResourceStrings('Lr_const','../../source/languages/lr_const.%s.po', Lang,FallbackLang);
  TranslateUnitResourceStrings('lr_add_function_const','../../source/addons/addfunction/languages/lr_add_function_const.%s.po'
     ,Lang,FallbackLang);
  SetDefaultLang(Lang, '', '', false); // needed to translate strings extracted from forms
end;

initialization
  TranslateResStrings;
end.

