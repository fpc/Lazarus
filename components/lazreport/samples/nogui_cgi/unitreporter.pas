unit UnitReporter;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LazLogger, Graphics, LR_Class, LR_DBSet, LR_BarC, LR_Shape,
  LR_RRect, LR_ChBox, LR_CrossTab, LR_CodeReport,
  lr_e_pdf, lr_e_fclpdf, LR_E_TXT, LR_E_CSV, LR_E_HTM, LR_e_img, LR_e_htmldiv,
  le_e_spreadsheet,
  dbf, DB;

const
  PDF_FILE = 'report.pdf';
  DATAPATH = 'data' + PathDelim;

type

  TExportBackend = (ebPowerPDF, ebFCLPDF, ebTxt, ebCSV, ebHtml, ebHtmlDiv,
                    ebBmp, ebJpg, ebPng,
                    ebOpenDoc, ebXLS, ebOOXML);

  { TReporter }

  TReporter=class
  private
    fDbf: TDbf;
    fExportedFile: string;
    fExportBackend: TExportBackend;
    fSrc: TDatasource;
    fReport: TfrReport;
    flrDataset: TfrDBDataset;
    flrCode: TlrCodeReport;
    procedure CheckReport;
    procedure OnExportFilterSetup(Sender: TfrExportFilter);
    procedure OnLRCodeReportTest(Sender: TObject);
    function  DoExportReport: boolean;
    function  BackendName: string;
    function  BackendExt: string;
  public
    constructor create;
    destructor destroy; override;
    procedure LoadReport(filename: string);
    procedure PrepareDisksReport;
    procedure PrepareCrossTabReport;
    function PrepareLRCodeReport: boolean;
    function ProcessExportReport(reportName:string): boolean;

    property ExportBackend: TExportBackend read fExportBackend write fExportBackend;
    property ExportedFile:string read fExportedFile;
    property Report: TfrReport read fReport;
  end;

implementation

{ TReporter }

procedure TReporter.CheckReport;
begin
  if fReport=nil then begin
    fReport := TfrReport.Create(nil);
    fReport.OnExportFilterSetup := @OnExportFilterSetup;
    //flrPDFExport := TfrTNPDFExport.Create(nil);
  end;
end;

procedure TReporter.OnExportFilterSetup(Sender: TfrExportFilter);
begin
  //if Sender is TfrHtmlDivExportFilter then begin
  //  TfrHtmlDivExportFilter(Sender).EmbeddedImages := false;
  //end;
end;

procedure TReporter.OnLRCodeReportTest(Sender: TObject);
var
  BoxText: TlrTextRectStyle;
  n: integer;
  X: double;
  Picture: TPicture;
begin
  with Sender as TlrCodeReport do
  begin
    // Important. Before drawing, add a page
    NewPage;
    // Set paper...  1=Letter 9=A4....
    //SetPaper(1, poLandscape);    // try uncomment this line to test another paper size
    // Set up a custom style
    BoxText := GetDefaultTextRectStyle;
    BoxText.FontName := 'Times';
    BoxText.FontSize := 12;
    BoxText.FontStyle := [fsBold, fsItalic];
    BoxText.FontColor := clBlue;
    BoxText.FillColor := clYellow;
    BoxText.Line.LColor := clRed;
    BoxText.Line.LWidth := 2;
    BoxText.BorderLines := [frbLeft, frbTop, frbRight, frbBottom];
    BoxText.Alignment := taRightJustify;
    //*******************************************************************
    //SetRatio(1, 1);  // working with pixels
    //NOTE: by default values are in pixels
    LineStyle.LColor := clBlue;
    DrawHLine(0, 5, GetPageWidth);
    DrawVLine(5, 0, GetPageHeight);

    // check values   uncomment to try
    //ShowMessage('Width: ' + FormatFloat('0.00', GetPageWidth) +
    //  'pixels' + 'Height: ' + FormatFloat('0.00', GetPageHeight) + 'pixels.');

    //  working with mm
    EnableMillimeters; // workign in millimeters

    //// check values   uncomment to try
    //ShowMessage('Width: ' + FormatFloat('0.00', GetPageWidth) +
    //  ' mm.' + 'Height: ' + FormatFloat('0.00', GetPageHeight) + ' mm.');

    // Draw text
    DrawText(0, 0, GetPageWidth, 10, 'Text example áéóâ € jgÑ€', BoxText);
    DrawText(0, 15, GetPageWidth, 10, 'Text example áéóâ E jgNE', BoxText);
    DrawText(0, 30, GetPageWidth, 10, '1234', BoxText);
    // Testing cursor
    // Set AutoSize
    BoxText.Autosize := True;
    DrawText(0, Cursor.YBottom, GetPageWidth, 6, 'Testing cursors', BoxText);
    DrawText(0, Cursor.YBottom, GetPageWidth, 6, 'next line', BoxText);
    DrawText(0, Cursor.YBottom, GetPageWidth, 6, 'another line', BoxText);
    // Align Left
    BoxText.Alignment := taLeftJustify;
    DrawText(0, Cursor.YBottom, GetPageWidth, 6, 'Testing cursors', BoxText);
    DrawText(0, Cursor.YBottom, GetPageWidth, 6, 'next line', BoxText);
    DrawText(0, Cursor.YBottom, GetPageWidth, 6, 'another line', BoxText);
    // center it
    BoxText.FontName := 'Arial';
    BoxText.Alignment := taCenter;
    BoxText.Autosize := False;
    DrawText(0, Cursor.YBottom, GetPageWidth, 6, 'Testing cursors', BoxText);
    DrawText(0, Cursor.YBottom, GetPageWidth, 6, 'next line', BoxText);
    DrawText(0, Cursor.YBottom, GetPageWidth, 6, 'another line', BoxText);
    // Layout
    x := Cursor.YBottom + 5;
    BoxText.FillColor := clSilver;
    BoxText.Line.LColor := clGreen;
    BoxText.FontColor := clRed;
    BoxText.Layout := tlTop;
    BoxText.Alignment := taLeftJustify;
    DrawText(20, x, 50, 15, 'TopLeft', BoxText);
    BoxText.Alignment := taCenter;
    DrawText(70, x, 50, 15, 'TopCenter', BoxText);
    BoxText.Alignment := taRightJustify;
    DrawText(120, x, 50, 15, 'TopRight', BoxText);
    x := Cursor.YBottom;
    BoxText.Layout := tlCenter;
    BoxText.Alignment := taLeftJustify;
    DrawText(20, x, 50, 15, 'CenterLeft', BoxText);
    BoxText.Alignment := taCenter;
    DrawText(70, x, 50, 15, 'CenterCenter', BoxText);
    BoxText.Alignment := taRightJustify;
    DrawText(120, x, 50, 15, 'CenterRight', BoxText);
    x := Cursor.YBottom;
    BoxText.Layout := tlBottom;
    BoxText.Alignment := taLeftJustify;
    DrawText(20, x, 50, 15, 'BottomLeft', BoxText);
    BoxText.Alignment := taCenter;
    DrawText(70, x, 50, 15, 'BottomCenter', BoxText);
    BoxText.Alignment := taRightJustify;
    DrawText(120, x, 50, 15, 'BottomRight', BoxText);
    LineStyle.LColor := clMaroon;
    LineStyle.LWidth := 1;
    LineStyle.LStyle := frsDashDotDot;
    DrawHLine(0, 15, GetPageWidth);
    DrawVLine(15, 0, GetPageHeight);
    NewPage;
    LineStyle.LColor := clRed;
    LineStyle.LStyle := frsDash;
    DrawHLine(0, 15, GetPageWidth);
    DrawVLine(15, 0, GetPageHeight);

    NewPage;
    LineStyle.LColor := clYellow;
    DrawHLine(0, 15, GetPageWidth);
    DrawVLine(15, 0, GetPageHeight);

    NewPage;
    // Testing TextOutRectXY
    ResetTextRectStyle;   // restart default style
    TextOutRectXY(10, 10, 15, 5, 'This text will be cut');
    TextRectStyle.FontName := 'Times';
    TextRectStyle.FontSize := 10;
    TextRectStyle.FontStyle := [fsBold];
    TextOutRectXY(10, 50, 15, 45, 'This is a non clipping test', taCenter, False);
    ResetTextRectStyle;

    // TextOut* testing. write/writeln equivalent
    NewPage;
    PageMargin.Top := 10;
    PageMargin.Bottom := 10;
    PageMargin.Left := 10;
    PageMargin.Right := 10;
    TextOut('World World ');
    TextOut('World');
    TextOut('!');
    TextOut('___');
    TextOutLn('.');
    TextOutLn('Hello');
    TextRectStyle.FontSize := 12;
    TextOutLn('World - Size 12');
    TextRectStyle.FontSize := 10;
    TextOutLn('End! - Size 10');
    for n := 0 to 250 do
    begin
      TextOutLn('Line ' + IntToStr(n));
    end;
    NewLine;
    TextOutLn('1 line below');
    NewLine(3);
    TextOutLn('3 lines below');

    NewPage;
    // Testing TextOutXY
    TextOutXY(0, 0, 'UL Corner');    // default is left aligned
    TextOutXY(GetPageWidth, 0, 'UR Corner', taRightJustify);
    TextOutXY(GetPageWidth / 2, 0, 'Center', taCenter);
    TextOutXY(GetPageWidth / 2, 13, 'LLLL');
    TextOutXY(GetPageWidth / 2, 13, 'RRRR', taRightJustify);
    TextOutXY(0, GetPageHeight - 4, 'LL Corner');    // default is left aligned
    TextOutXY(GetPageWidth, GetPageHeight - 4, 'LR Corner', taRightJustify);
    TextOutXY(GetPageWidth / 2, GetPageHeight - 4, 'Center', taCenter);

    NewPage;
    // Testing rotated up text
    TextOutXYUp(5, 1, 'Rotated Text UL Corner', taRightJustify);
    TextOutXYUp(5, GetPageHeight / 2, 'Rotated Text Center', taCenter);
    TextOutXYUp(5, GetPageHeight - 1, 'Rotated Text LL Corner', taLeftJustify);

    NewPage;
    // Testing frames
    DrawFrame(10, 10, 25, 10);
    FrameStyle.FillColor := clYellow;
    FrameStyle.Line.LColor := clBlue;
    DrawFrame(10, 35, 25, 10);
    FrameStyle.Line.LColor := clNavy;
    FrameStyle.FillColor := clNavy; // No borders
    DrawFrame(15, 40, 25, 10);
    ResetFrameStyle;   // start new default style
    FrameStyle.FillColor := clRed;
    FrameStyle.Line.LColor := clGreen;
    FrameStyle.Line.LWidth := 2;
    FrameStyle.BorderLines := [frbLeft, frbTop, frbBottom];  // no line right side
    DrawFrame(150, 10, 25, 10);
    ResetFrameStyle;
    DrawFrame(10, 100, 24, 10);
    DrawFrame(34, 100, 24, 10);
    DrawFrame(58, 100, 24, 10);
    DrawFrame(82, 100, 24, 10);

    NewPage;
    // Testing image
    // using sharedname, this allows us to define one image and reuse it
    // resulting in less resources usage

    Picture := TPicture.Create;
    Picture.LoadFromResourceName(Hinstance, 'LOGO1');
    DrawImage(10, 10, 60, 60, Picture, 'logo1');
    DrawImage(10, 80, 60, 30, Picture, 'logo1');
    // keepaspect=false
    DrawImage(71, 80, 60, 30, Picture, 'logo1', True, False, False);
    Picture.Free;

    NewPage;
    // Testing shapes
    DrawShape(10, 10, 50, 20, ShapeStyle);  // full power procedure
    ShapeStyle.FillColor := clYellow;
    ShapeStyle.FrameColor := clBlue;
    DrawRectangle(10, 30, 50, 20);
    DrawRoundRectangle(10, 50, 50, 20);
    DrawDiagonalDownRight(10, 70, 50, 20);
    DrawDiagonalUpRight(10, 90, 50, 20);
    DrawEllipse(10, 110, 50, 20);
    DrawTriangle(10, 130, 50, 20);

    NewPage;
    // Testing BarCodes
    DrawBarCode(10, 10, 0, 15, 'lazarus-123456789', BarCodeStyle); // Default is Code39
    BarCodeStyle.Angle := 90;
    DrawBarCode(10, 30, 15, 0, 'lazarus-123456789', BarCodeStyle);
    ResetBarCodeStyle;
    BarCodeStyle.BorderLines := [frbLeft, frbTop, frbRight, frbBottom];
    BarCodeStyle.FrameColor := clYellow;
    DrawBarCode(10, 90, 0, 15, 'lazarus-123456789', BarCodeStyle);

    // Testing active page change
    ResetTextRectStyle;
    TextRectStyle.FontSize := 7;
    TextRectStyle.FontColor := clDkGray;
    for n := 1 to PageCount do
    begin
      SetActivePage(n);  // move to page n
      if (n mod 2) = 0 then
      begin
        X := PageMargin.Left;
        TextOutXY(X, GetPageHeight - PageMargin.Bottom,
          Format('Page %d of %d', [GetActivePage, PageCount]), taLeftJustify);
      end
      else
      begin
        X := GetPageWidth - PageMargin.Right;
        TextOutXY(X, GetPageHeight - PageMargin.Bottom,
          Format('Page %d of %d', [GetActivePage, PageCount]), taRightJustify);
      end;
    end;

    // For a really big report (10015 pages), try uncommenting next lines

    //for n:= 1 to 10000 do
    //begin
    //  NewPage;
    //  TextOut(Format('Page %d', [GetActivePage]));
    //end;
  end;
end;

constructor TReporter.create;
begin
  inherited create;
  fDbf := TDBF.Create(nil);
  FDbf.Name := 'Dbf1';
  fDbf.FilePath := DATAPATH;
  fSrc := TDatasource.Create(nil);
  fSrc.DataSet := fDbf;
  //DebugLogger.MaxNestPrefixLen := 100;
end;

destructor TReporter.destroy;
begin
  flrCode.Free;
  flrDataset.Free;
  fSrc.Free;
  fDbf.Free;
  fReport.Free;
  inherited destroy;
end;

procedure TReporter.LoadReport(filename: string);
begin
  CheckReport;
  fReport.LoadFromFile(filename);
end;

procedure TReporter.PrepareDisksReport;
begin

  fDbf.close;
  fDbf.TableName := 'disco.dbf';
  fDbf.open;

  CheckReport;

  if flrDataset=nil then begin
    flrDataset := TfrDBDataset.Create(nil);
    flrDataset.DataSet := fDbf;
  end;

  fReport.Dataset := flrDataset;

  LoadReport('disks.lrf');
end;

procedure TReporter.PrepareCrossTabReport;
begin
  fDbf.close;
  fDbf.TableName := 'SalesCustomer.dbf';
  fDbf.open;

  CheckReport;

  if flrDataset=nil then begin
    flrDataset := TfrDBDataset.Create(nil);
    flrDataset.DataSet := fDbf;
  end;

  fReport.Dataset := flrDataset;

  LoadReport('demo_cross.lrf');
end;

function TReporter.DoExportReport: boolean;
var
  FilterClass: TfrExportFilterClass;
begin
  CheckReport;
  result := fReport.PrepareReport;
  if result then begin
    case fExportBackend of
      ebPowerPDF: filterClass := TfrTNPDFExportFilter;
      ebFCLPDF:   filterClass := TlrPdfExportFilter;
      ebTxt:      filterClass := TfrTextExportFilter;
      ebCSV:      filterClass := TfrCSVExportFilter;
      ebHtml:     filterClass := TfrHTMExportFilter;
      ebHtmlDiv:  filterClass := TfrHtmlDivExportFilter;
      ebBmp:      filterClass := TfrImageExportFilter;
      ebJpg:      filterClass := TfrImageExportFilter;
      ebPng:      filterClass := TfrImageExportFilter;
      ebOpenDoc:  filterClass := TlrSpreadSheetExportFilter;
      ebXLS:      filterClass := TlrSpreadSheetExportFilter;
      ebOOXML:    filterClass := TlrSpreadSheetExportFilter;
    end;
    //WriteLn('Class: ', filterclass.ClassName);
    result := fReport.ExportTo(filterClass, fExportedFile);
  end;
end;

function TReporter.BackendName: string;
begin
  case fExportBackend of
    ebPowerPDF: result := '_powerpdf';
    ebFCLPDF:   result := '_fcl';
    ebHtml:     result := '_html';
    ebHtmlDiv:  result := '_htmldiv';
    else        result := '';
  end;
end;

function TReporter.BackendExt: string;
begin
  case fExportBackend of
    ebPowerPDF: result := '.pdf';
    ebFCLPDF:   result := '.pdf';
    ebHtml:     result := '.htm';
    ebHtmlDiv:  result := '.html';
    ebTxt:      result := '.txt';
    ebCSV:      result := '.csv';
    ebBmp:      result := '.bmp';
    ebJpg:      result := '.jpg';
    ebPng:      result := '.png';
    ebOpenDoc:  result := '.ods';
    ebXLS:      result := '.xls';
    ebOOXML:  result := '.xlsx';
  end;
end;

function TReporter.PrepareLRCodeReport: boolean;
begin
  CheckReport;
  if flrCode=nil then begin
    flrCode := TlrCodeReport.Create(nil);
    flrCode.report := fReport;
  end;
  flrCode.Report.Clear;                         // reset report
  flrCode.OnBeginReport := @OnLRCodeReportTest;
  flrCode.RunReport(false);                     // execute code
  //fReport.SaveToXMLFile('intermedio.lrf');
  result := true;
end;

function TReporter.ProcessExportReport(reportName: string): boolean;
var
  aFilename: String;
begin
  aFilename := ChangeFileExt(PDF_FILE, backendExt);
  aFilename := StringReplace(aFilename, '.', '_%s%s.', []);
  fExportedFile := format(aFilename,[reportName, backendName]);
  result := DoExportReport;
end;

end.

