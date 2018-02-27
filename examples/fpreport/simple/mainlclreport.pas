unit mainLCLReport;

{$mode objfpc}{$H+}

{$define ColorBands}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, ComCtrls, fpreport;

const
  TESTIMAGE = 'powered_by.png';

type

  { TFrmSimpleReportLCL }

  TFrmSimpleReportLCL = class(TForm)
    BuRenderCanvas: TButton;
    BuRenderPrerview: TButton;
    BuRenderPdf: TButton;
    lblPage: TLabel;
    PanelRender: TPanel;
    panelMain: TPanel;
    UpDown1: TUpDown;
    procedure BuRenderCanvasClick(Sender: TObject);
    procedure BuRenderPdfClick(Sender: TObject);
    procedure BuRenderPrerviewClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure PanelRenderPaint(Sender: TObject);
    procedure UpDown1Changing(Sender: TObject; var AllowChange: Boolean);
  private
    FInit: Boolean;
    // Report
    FReport: TFPReport;
    FDataParent : TComponent;
    lReportData : TFPReportUserData;
    sl: TStringList;
    // Exporter
    rptExporter : TFPReportExporter;
    procedure CreateDemoReport;
    procedure GetReportDataEOF(Sender: TObject; var IsEOF: Boolean);
    procedure GetReportDataFirst(Sender: TObject);
    procedure GetReportDataNames(Sender: TObject; List: TStrings);
    procedure GetReportDataValue(Sender: TObject; const AValueName: String;
      var AValue: Variant);
    procedure InitialiseData;
    procedure CleanUp;
    procedure ButtonSet(state:Boolean);
  public

  end;

var
  FrmSimpleReportLCL: TFrmSimpleReportLCL;

implementation

uses
  fpTTF,
  fpreportformexport,
  fpreportlclexport,
  fpreportpdfexport;

{$R *.lfm}

procedure TFrmSimpleReportLCL.PanelRenderPaint(Sender: TObject);
begin
  rptExporter.Execute;
end;

procedure TFrmSimpleReportLCL.UpDown1Changing(Sender: TObject; var AllowChange: Boolean);
begin
  if (UpDown1.Position > UpDown1.Max)
     or (UpDown1.Position < UpDown1.Min) then begin
    AllowChange:=false;
    exit;
  end;
  TFPReportExportCanvas(rptExporter).PageIndex:=UpDown1.Position;
  lblPage.Caption:= 'Page: ' + IntToStr(TFPReportExportCanvas(rptExporter).PageIndex);
  Invalidate;
end;

procedure TFrmSimpleReportLCL.BuRenderCanvasClick(Sender: TObject);
begin
  ButtonSet(false);
  try
    CreateDemoReport;
    FReport.RunReport;

    if Assigned(rptExporter) then
      FreeAndNil(rptExporter);
    rptExporter:= TFPReportExportCanvas.Create(nil);
    rptExporter.Report:= FReport;
    rptExporter.AutoRun:=True;
    TFPReportExportCanvas(rptExporter).Canvas:= PanelRender.Canvas;
    FReport.RenderReport(rptExporter);
    UpDown1.Min:= 0;
    UpDown1.Max:= TFPReportExportCanvas(rptExporter).PageCount-1;
    UpDown1.Position:= 0;
    TFPReportExportCanvas(rptExporter).PageIndex:=UpDown1.Position;
    lblPage.Caption:= 'Page: ' + IntToStr(TFPReportExportCanvas(rptExporter).PageIndex);
    PanelRender.OnPaint:= @PanelRenderPaint;
    Invalidate;
  finally
    ButtonSet(true);
  end;
end;

procedure TFrmSimpleReportLCL.BuRenderPdfClick(Sender: TObject);
begin
  ButtonSet(false);
  try
    CreateDemoReport;
    FReport.RunReport;

    if Assigned(rptExporter) then
      FreeAndNil(rptExporter);
    rptExporter:= TFPReportExportPDF.Create(nil);
    TFPReportExportPDF(rptExporter).FileName:= ApplicationName+'-report.pdf';
    rptExporter.Report:= FReport;
    rptExporter.AutoRun:=True;
    FReport.RenderReport(rptExporter);
    ShowMessage('PDF created: '+TFPReportExportPDF(rptExporter).FileName);
  finally
    ButtonSet(true);
  end;
end;

procedure TFrmSimpleReportLCL.BuRenderPrerviewClick(Sender: TObject);
begin
  ButtonSet(false);
  try
    CreateDemoReport;
    FReport.RunReport;

    if Assigned(rptExporter) then
      FreeAndNil(rptExporter);
    rptExporter:= TFPreportPreviewExport.Create(nil);
    rptExporter.Report:= FReport;
    rptExporter.AutoRun:=True;
    FReport.RenderReport(rptExporter);
  finally
    ButtonSet(true);
  end;
end;

procedure TFrmSimpleReportLCL.FormActivate(Sender: TObject);
begin
  if not FInit then begin
    gTTFontCache.ReadStandardFonts;
    gTTFontCache.BuildFontCache;
    if PaperManager.PaperCount=0 then
      PaperManager.RegisterStandardSizes;
    FInit:= true;
  end;
end;

procedure TFrmSimpleReportLCL.FormCreate(Sender: TObject);
begin
  FReport:= nil;
  FDataParent:= nil;
  lReportData:= nil;
  rptExporter:= nil;
  sl:= nil;
  FInit:= False;
end;

procedure TFrmSimpleReportLCL.FormDestroy(Sender: TObject);
begin
  CleanUp;
end;


procedure TFrmSimpleReportLCL.CleanUp;
begin
  if Assigned(rptExporter) then FreeAndNil(rptExporter);
  if Assigned(FReport) then FreeAndNil(FReport);
  if Assigned(FDataParent) then FreeAndNil(FDataParent);
  if Assigned(lReportData) then FreeAndNil(lReportData);
  if Assigned(sl) then FreeAndNil(sl);
end;

procedure TFrmSimpleReportLCL.ButtonSet(state: Boolean);
begin
  BuRenderCanvas.Enabled:= state;
  BuRenderPrerview.Enabled:= state;
  BuRenderPdf.Enabled:= state;
  Application.ProcessMessages;
end;

procedure TFrmSimpleReportLCL.CreateDemoReport;

const
{$ifdef Windows}
  defaultFont = 'ArialMT';
{$else}
  defaultFont = 'Ubuntu';
{$endif}

var
  p: TFPReportPage;
  TitleBand: TFPReportTitleBand;
  DataBand: TFPReportDataBand;
  Memo: TFPReportMemo;
  PageFooter: TFPReportPageFooterBand;
  ColumnBand: TFPReportColumnHeaderBand;
  DataFooterBand: TFPReportDataFooterBand;
begin
  CleanUp;

  lReportData := TFPReportUserData.Create(Self);
  lReportData.Name:='UserData';
  lReportData.OnGetValue := @GetReportDataValue;
  lReportData.OnGetEOF := @GetReportDataEOF;
  lReportData.OnFirst := @GetReportDataFirst;
  lReportData.OnGetNames := @GetReportDataNames;
  lReportData.InitFieldDefs;
  InitialiseData;

  FReport:=TFPReport.Create(Self);
  FReport.Author := 'Andreas';
  FReport.Title := 'LCL Report Demo';
  FReport.Variables.AddVariable('Var1').AsString:='Value1';
  FReport.Variables.AddVariable('Var2').AsString:='Value2';

  p := TFPReportPage.Create(FReport);
  p.Orientation := poPortrait;
  p.PageSize.PaperName := 'A4';
  { page margins }
  p.Margins.Left := 20;
  p.Margins.Top := 20;
  p.Margins.Right := 10;
  p.Margins.Bottom := 20;
  p.Data := lReportData;

  p.ColumnCount:= 2;
  p.ColumnGap:= 5;

  TitleBand := TFPReportTitleBand.Create(p);
  TitleBand.Layout.Height := 30;
  TitleBand.Frame.Shape := fsRectangle;
  TitleBand.Frame.BackgroundColor := clNone;

  Memo := TFPReportMemo.Create(TitleBand);
  Memo.UseParentFont:=False;
  Memo.Layout.Top := 0;
  Memo.Layout.Height := 10;
  Memo.Layout.Left := Memo.Layout.Height + 5; // to make room for the image
  Memo.Layout.Width := TitleBand.Layout.Width - (Memo.Layout.Height + 5);
  Memo.Font.Name := defaultFont;
  Memo.Font.Size:= 20;
  Memo.Text := 'Demo Titleband Memo1';
  Memo.StretchMode := smActualHeight;
  Memo.TextAlignment.Vertical := tlCenter;
  Memo.TextAlignment.Horizontal := taCentered;
  Memo.Frame.Shape := fsRectangle;
  Memo.Frame.BackgroundColor:= clLtGray;

  With TFPReportImage.Create(TitleBand) do
    begin
    Layout.Left := 0;
    Layout.Top := 0;
    Layout.Width := Memo.Layout.Height;
    Layout.Height := Memo.Layout.Height;
    Stretched:=True;
    LoadFromFile(ExtractFilePath(ParamStr(0))+TESTIMAGE);
    end;

  Memo := TFPReportMemo.Create(TitleBand);
  Memo.UseParentFont:=False;
  Memo.Layout.Left := 0;
  Memo.Layout.Top := 10;
  Memo.Layout.Width := TitleBand.Layout.Width;
  Memo.Layout.Height := 5;
  Memo.Font.Name := defaultFont;
  Memo.Font.Size:= 12;
  Memo.Text := 'Demo Titleband Memo2';
  Memo.StretchMode := smActualHeight;
  Memo.TextAlignment.Vertical := tlCenter;
  Memo.TextAlignment.Horizontal := taLeftJustified;
  Memo.Frame.Shape := fsRectangle;

  Memo := TFPReportMemo.Create(TitleBand);
  Memo.UseParentFont:=False;
  Memo.Layout.Left := 0;
  Memo.Layout.Top := 15;
  Memo.Layout.Width := TitleBand.Layout.Width;
  Memo.Layout.Height := 5;
  Memo.Font.Name := defaultFont;
  Memo.Font.Size:= 12;
  Memo.Text := 'Demo Titleband Memo3';
  Memo.StretchMode := smActualHeight;
  Memo.TextAlignment.Vertical := tlCenter;
  Memo.TextAlignment.Horizontal := taLeftJustified;
  Memo.Frame.Shape := fsRectangle;

  Memo := TFPReportMemo.Create(TitleBand);
  Memo.UseParentFont:=False;
  Memo.Layout.Left := 0;
  Memo.Layout.Top := 20;
  Memo.Layout.Width := TitleBand.Layout.Width;
  Memo.Layout.Height := 5;
  Memo.Font.Name := defaultFont;
  Memo.Font.Size:= 12;
  Memo.Text := 'Date: ' + FormatDateTime('DD.MM.YYYY HH:MM',now);
  Memo.StretchMode := smActualHeight;
  Memo.TextAlignment.Vertical := tlCenter;
  Memo.TextAlignment.Horizontal := taLeftJustified;
  Memo.Frame.Shape := fsRectangle;

  ColumnBand:= TFPReportColumnHeaderBand.Create(p);
  ColumnBand.Layout.Height := 5;
  ColumnBand.Layout.Width:=50;
  ColumnBand.Frame.Shape := fsRectangle;
  ColumnBand.Frame.BackgroundColor := clNone;


  Memo := TFPReportMemo.Create(ColumnBand);
  Memo.UseParentFont:=False;
  Memo.Layout.Left := 0;
  Memo.Layout.Top := 20;
  Memo.Layout.Width := 50;
  Memo.Layout.Height := 5;
  Memo.Font.Name := defaultFont;
  Memo.Font.Size:= 12;
  Memo.Text := 'Column';
  Memo.StretchMode := smActualHeight;
  Memo.TextAlignment.Vertical := tlCenter;
  Memo.TextAlignment.Horizontal := taLeftJustified;
  Memo.Frame.Shape := fsRectangle;

  DataBand := TFPReportDataBand.Create(p);
  DataBand.Layout.Height := 10;
  {$ifdef ColorBands}
  DataBand.Frame.Shape := fsRectangle;
  DataBand.Frame.BackgroundColor := clDataBand;
  {$endif}

  Memo := TFPReportMemo.Create(DataBand);
  Memo.Layout.Left := 5;
  Memo.Layout.Top := 0;
  Memo.Layout.Width := 60;
  Memo.Layout.Height := 5;
  Memo.Font.Name := defaultFont;
  Memo.Text := 'Hello world <[userdata.string]>.';


  DataFooterBand := TFPReportDataFooterBand.Create(p);
  DataFooterBand.Layout.Height := 10;
  {$ifdef ColorBands}
  DataFooterBand.Frame.Shape := fsRectangle;
  DataFooterBand.Frame.BackgroundColor := clNone;
  {$endif}

  With TFPReportShape.Create(DataFooterBand) do
  begin
    Layout.Left := 5;
    Layout.Top := 15;
    Layout.Width := 20;
    Layout.Height := 20;
    ShapeType:=stCircle;
  end;

  PageFooter := TFPReportPageFooterBand.Create(p);
  PageFooter.Layout.Height := 10;
  {$ifdef ColorBands}
  PageFooter.Frame.Shape := fsRectangle;
  PageFooter.Frame.BackgroundColor := clPageHeaderFooter;
  {$endif}

  Memo := TFPReportMemo.Create(PageFooter);
  Memo.Layout.Left := 135;
  Memo.Layout.Top := 13;
  Memo.Layout.Width := 15;
  Memo.Layout.Height := 5;
  Memo.Options:=[moDisableWordWrap];
  Memo.Font.Name := defaultFont;
  Memo.Text := 'Page [PageNo] ';
 end;

procedure TFrmSimpleReportLCL.GetReportDataFirst(Sender: TObject);
begin
  // Nothing to do yet, but needed
end;

procedure TFrmSimpleReportLCL.GetReportDataValue(Sender: TObject;
  const AValueName: String; var AValue: Variant);
begin
  if (AValueName = 'element') or (AValueName = 'string') then
  begin
    AValue := sl[lReportData.RecNo-1];
  end
  else
    AValue:=AValueName+IntToStr(lReportData.RecNo);
end;

procedure TFrmSimpleReportLCL.GetReportDataEOF(Sender: TObject;
  var IsEOF: Boolean);
begin
  if lReportData.RecNo > sl.Count then
    IsEOF := True
  else
    IsEOF := False;
end;

procedure TFrmSimpleReportLCL.GetReportDataNames(Sender: TObject;
  List: TStrings);
begin
  List.Add('element');
  List.Add('string');
  List.Add('FirstName');
  List.Add('LastName');
  List.Add('DateOfBirth');
  List.Add('Gender');
  List.Add('Email');
end;

procedure TFrmSimpleReportLCL.InitialiseData;
var
  i: integer;
begin
  sl := TStringList.Create;
  for i := 1 to 50 do
    sl.Add(Format('Item %d', [i]));
end;





end.

