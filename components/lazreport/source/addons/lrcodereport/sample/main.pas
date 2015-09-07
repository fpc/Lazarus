unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LR_Class, LR_Shape, LR_BarC, lr_e_pdf, Forms,
  Graphics, Dialogs, ExtCtrls, StdCtrls, LR_CodeReport;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    frBarCodeObject1: TfrBarCodeObject;
    frShapeObject1: TfrShapeObject;
    frTNPDFExport1: TfrTNPDFExport;
    Image1: TImage;
    CodeReport1: TlrCodeReport;
    SaveDialog1: TSaveDialog;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure CodeReport1BeginReport(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.Button1Click(Sender: TObject);
begin
  with CodeReport1 do
  begin
    Report.Clear;   // restart the report (delete existing pages)
    RunReport;      // execute code and show report
  end;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  SaveDialog1.FileName := 'mycodereport.pdf';
  if SaveDialog1.Execute then
    with CodeReport1 do
    begin
      Report.Clear;                         // reset report
      RunReport(false);                     // execute code
      Report.PrepareReport;
      Report.ExportTo(TfrTNPDFExportFilter, SaveDialog1.FileName);
    end;
end;

procedure TForm1.Button3Click(Sender: TObject);
begin
  with CodeReport1 do
  begin
    Report.Clear;                         // reset report
    RunReport(false);                     // execute code
    Report.PrepareReport;
    Report.PrintPreparedReport('', 1);    // empty string print all the pages
    // '1-5' print pages from 1 to 5
    // '1,3,5' print pages 1, 3 and 5
  end;
end;

procedure TForm1.CodeReport1BeginReport(Sender: TObject);
var
  BoxText: TlrTextRectStyle;
  n: integer;
  X: double;
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
    DrawImage(10, 10, 60, 60, Image1, 'logo1');
    DrawImage(10, 80, 60, 30, Image1, 'logo1');
    // keepaspect=false
    DrawImage(71, 80, 60, 30, Image1, 'logo1', True, False, False);

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

end.
