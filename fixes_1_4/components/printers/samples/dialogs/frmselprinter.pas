{*****************************************************************************
  This file is part of the Lazarus Component Library (LCL)

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************

  Author: Olivier GUILBAUD

  Abstract:
    Little sample for show how to use PrintersDlgs unit

------------------------------------------------------------------------------}
unit frmselprinter;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Types, LResources, Forms, Controls, Graphics, Dialogs, Buttons,
  PrintersDlgs, StdCtrls, Grids, Menus, EditBtn, ExtCtrls;

type

  { TForm1 }

  TForm1 = class(TForm)
    btnTPrintDialog: TButton;
    Button2: TButton;
    Button3: TButton;
    btnDirectPrint: TButton;
    Button5: TButton;
    Button6: TButton;
    Button7: TButton;
    btnRotateBin: TButton;
    btnRestoreDefaultBin: TButton;
    btnPrintWithDlg: TButton;
    chkOutputFile: TCheckBox;
    chkTestImgs: TCheckBox;
    comboTests: TComboBox;
    Label5: TLabel;
    txtPageSetupDlgTitle: TEdit;
    txtPrinterSetupDlgTitle: TEdit;
    txtPrintDialogTitle: TEdit;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    txtOutputFile: TFileNameEdit;
    Label1: TLABEL;
    PAGED: TPageSetupDialog;
    PD: TPrintDialog;
    PopupMenu1: TPopupMenu;
    PSD: TPrinterSetupDialog;
    SGrid: TStringGrid;
    procedure btnPrintWithDlgClick(Sender: TObject);
    procedure btnTPrintDialogClick(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure btnDirectPrintClick(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure Button7Click(Sender: TObject);
    procedure btnRotateBinClick(Sender: TObject);
    procedure btnRestoreDefaultBinClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure SGridSelectCell(Sender: TObject; aCol, aRow: Integer;
      var CanSelect: Boolean);
  private
    ck : Integer;
    procedure UpdatePrinterInfo;
    procedure AddInfo(const Desc : String; Const Info : String);
    procedure DrawGraphic(X,Y,AWidth,AHeight:Integer; Graphic: TGraphic);
    function CM(Avalue: Double; VertRes:boolean=true): Integer;
    function MM(AValue: Double; VertRes:boolean=true): Integer;
    function Inch(AValue: Double; VertRes:boolean=true): Integer;
    function Per(AValue: Double; VertRes:boolean=true): Integer;
    procedure CenterText(const X,Y: Integer; const AText: string);
    function FormatDots(Dots: Integer):string;
    procedure PrintSamplePage(PrintImgs:boolean);
    procedure PrintMultiPage;
    procedure doPrintDialog(withSample: boolean);
    procedure PrintTest;
  public
  
    { public declarations }
  end; 

var
  Form1: TForm1; 

implementation
uses
  Printers,OsPrinters,LCLType,LClProc;

{$R *.lfm}

{ TForm1 }

procedure TForm1.AddInfo(const Desc: String; const Info: String);
begin
  SGrid.Cells[0,ck] := Desc;
  SGrid.Cells[1,ck] := Info;
  Inc(ck);
end;

procedure TForm1.DrawGraphic(X, Y, AWidth, AHeight: Integer; Graphic: TGraphic);
var
  Ratio: Double;
begin
  if (AWidth<=0) or (AHeight<=0) then begin
    if Graphic.Width=0 then
      ratio := 1
    else
      ratio := Graphic.Height/Graphic.Width;
    if AWidth<=0 then
      AWidth := round(AHeight/ratio)
    else
    if AHeight<=0 then
      AHeight := round(AWidth * ratio);
  end;

  if (AWidth>0) and (AHeight>0) then
    Printer.Canvas.StretchDraw(Bounds(X,Y,AWidth,AHeight), Graphic);
end;

function TForm1.CM(Avalue: Double; VertRes: boolean=true): Integer;
begin
  result := MM(AValue*10, vertRes);
end;

function TForm1.MM(AValue: Double; VertRes:boolean=true): Integer;
begin
  if VertRes then
    result := Round(AValue*Printer.YDPI/25.4)
  else
    result := Round(AValue*Printer.XDPI/25.4);
end;

function TForm1.Inch(AValue: Double; VertRes:boolean=true): Integer;
begin
  if VertRes then
    result := Round(AValue*Printer.YDPI)
  else
    result := Round(AValue*Printer.XDPI);
end;

function TForm1.Per(AValue: Double; VertRes:boolean=true): Integer;
begin
  if VertRes then
    result := Round(AValue*Printer.PageHeight/100)
  else
    result := Round(AValue*Printer.PageWidth/100);
end;

procedure TForm1.CenterText(const X, Y: Integer; const AText: string);
var
  Sz: TSize;
begin
  Sz := Printer.Canvas.TextExtent(AText);
  //WriteLn('X=',X,' Y=',Y,' Sz.Cx=',Sz.Cx,' Sz.Cy=',Sz.Cy);
  Printer.Canvas.TextOut(X - Sz.cx div 2, Y - Sz.cy div 2, AText);
end;

function TForm1.FormatDots(Dots: Integer): string;
begin
  result := format('%d dots (%f mm, %f in)',[Dots, Dots*25.4/Printer.YDPI, Dots/Printer.YDPI]);
end;

procedure TForm1.PrintSamplePage(PrintImgs: boolean);
var
  Pic: TPicture;
  d, pgw,pgh: Integer;
  Hin: Integer; // half inch
  s: string;
begin
  try
    Printer.Title := 'Printer test for printers4lazarus package';
    if chkOutputFile.Checked then
      Printer.FileName := txtOutputFile.FileName
    else
      Printer.FileName := '';
    Printer.BeginDoc;

    // some often used consts
    pgw := Printer.PageWidth-1;
    pgh := Printer.PageHeight-1;
    Hin := Inch(0.5);
    DebugLn('Page width=%d height=%d',[pgw, pgh]);

    // center title text on page width
    Printer.Canvas.Font.Size := 12;
    Printer.Canvas.Font.Color:= clBlue;
    CenterText(pgw div 2, CM(0.5), 'This is test for lazarus printer4lazarus package');

    // print margins marks, assumes XRes=YRes
    Printer.Canvas.Pen.Color:=clBlack;
    Printer.Canvas.Line(0, HIn, 0, 0);            // top-left
    Printer.Canvas.Line(0, 0, HIn, 0);

    Printer.Canvas.Brush.Color := clSilver;
    Printer.Canvas.EllipseC(Hin,Hin,Hin div 2,Hin div 2);
    CenterText(Hin, Hin, '1');

    Printer.Canvas.Pen.Color := clRed;
    Printer.Canvas.Pen.Width := 3;
    Printer.Canvas.Frame(0,0,pgw,pgh);

    Printer.Canvas.Pen.Color := clBlack;
    Printer.Canvas.Pen.Width := 3;
    Printer.Canvas.Line(0, pgh-HIn, 0, pgh);      // bottom-left
    Printer.Canvas.Line(0, pgh, HIn, pgh);
    Printer.Canvas.Line(pgw-Hin, pgh, pgw, pgh);  // bottom-right
    Printer.Canvas.Line(pgw,pgh,pgw,pgh-HIn);
    Printer.Canvas.Line(pgw-Hin, 0, pgw, 0);      // top-right
    Printer.Canvas.Line(pgw,0,pgw,HIn);

    // Image test
    if PrintImgs then
    begin
      Pic := TPicture.Create;
      Pic.LoadFromFile('../../../../images/splash_logo.png');
      // draw logo scaled down to 7 centimeters wide preserving image aspect
      DrawGraphic(CM(1.5), CM(1.5), MM(70), 0, Pic.Graphic);
      // left 3 mm at the right and do it again but using 2 inch tall image
      DrawGraphic(CM(1.5+7)+MM(3), CM(1.5), 0, Inch(2), Pic.Graphic);
      Pic.Free;
    end;

    Printer.EndDoc;

  except
    on E:Exception do
    begin
      Printer.Abort;
      Application.MessageBox(pChar(e.message),'Error',mb_iconhand);
    end;
  end;

  UpdatePrinterInfo;
end;

procedure TForm1.PrintMultiPage;
const
  PAGE_COUNT = 2;
  ColorArray: array[1..PAGE_COUNT] of TColor = (clAqua, clLime);
var
  pgw, pgh, Hin, Page: Integer;
  R: TRect;
  te: TTextStyle;
begin
  try
    Printer.Title := 'Multipage Sample';

    if chkOutputFile.Checked then
      Printer.FileName := txtOutputFile.FileName
    else
      Printer.FileName := '';

    Printer.BeginDoc;

    // some often used consts
    pgw := Printer.PageWidth-1;
    pgh := Printer.PageHeight-1;
    Hin := Inch(0.5);

    Te := Printer.Canvas.TextStyle;
    Te.Alignment := taCenter;
    Te.Layout := tlCenter;

    Printer.Canvas.Font.Size:=20;

    for Page:=1 to PAGE_COUNT do begin
      Printer.Canvas.Pen.Color := clBlack;
      Printer.Canvas.Brush.Color := ColorArray[Page];
      R := Rect(0, 0, 3*Hin, Hin);
      Printer.Canvas.Rectangle(R);
      Printer.canvas.TextRect(R, R.Left, R.Top, format('Page %d',[Page]), Te);
      if Page<>PAGE_COUNT then
        Printer.NewPage;
    end;

    Printer.EndDoc;

  except
    on E:Exception do
    begin
      Printer.Abort;
      Application.MessageBox(pChar(e.message),'Error',mb_iconhand);
    end;
  end;

end;

procedure TForm1.UpdatePrinterInfo;
var
  i: Integer;
  s: string;
  hRes,vRes: Integer;
begin
  try
    ck := SGrid.FixedRows;
    SGrid.Clean;
    with Printer do
    begin
      if Printers.Count=0 then
      begin
        AddInfo('printer', 'no printers are installed');
        exit;
      end;
      AddInfo('Printer',Printers[PrinterIndex]);
      case Orientation of
        poPortrait : AddInfo('Orientation','Portrait');
        poLandscape : AddInfo('Orientation','Landscape');
        poReverseLandscape : AddInfo('Orientation','ReverseLandscape');
        poReversePortrait  :AddInfo('Orientation','ReversePortrait');
      end;
      case PrinterType of
        ptLocal: AddInfo('PrinterType','Local');
        ptNetWork: AddInfo('PrinterType','Network');
      end;
      case PrinterState of
        psNoDefine: AddInfo('PrinterState','Undefined');
        psReady:AddInfo('PrinterState','Ready');
        psPrinting:AddInfo('PrinterState','Printing');
        psStopped:AddInfo('PrinterState','Stopped');
      end;
      hRes := XDPI;
      vRes := YDPI;
      AddInfo('Resolution X,Y', format('%d,%d dpi',[hRes,vRes]));
      AddInfo('PaperSize',PaperSize.PaperName);
      with Printer.PaperSize.PaperRect.PhysicalRect do
      begin
        AddInfo('Paper Width', FormatDots(Right-Left));
        AddInfo('Paper Height', FormatDots(Bottom-Top));
      end;
      AddInfo('Printable Width',FormatDots(PageWidth));
      AddInfo('Printable Height',FormatDots(PageHeight));
      AddInfo('Copies',IntToStr(Copies));
      if CanRenderCopies then
        AddInfo('CanRenderCopies','true')
      else
        AddInfo('CanRenderCopies','false');
      AddInfo('Default Bin', DefaultBinName);
      i := SupportedBins.IndexOf(BinName);
      s := BinName; // <- workaround for ugly FPC 2.7.1 string encoding conversion
      s := BinName + format(' (%d of %d)',[i+1, SupportedBins.Count]); // ditto
      //AddInfo('Current Bin', BinName + ' ');
      AddInfo('Current Bin', s);

      if not CanPrint then
        Application.MessageBox('Selected printer cannot print currently!',
          'Warning',mb_iconexclamation);
    end;
  except on E:Exception do
      Application.MessageBox(PChar(e.message),'Error',mb_iconhand);
  end;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  PSD.Title := txtPrinterSetupDlgTitle.Text;
  if PSD.Execute then
    UpdatePrinterInfo;
end;

procedure TForm1.Button3Click(Sender: TObject);
begin
  Printer.PrinterIndex := -1;
  UpdatePrinterInfo;
end;

procedure TForm1.btnDirectPrintClick(Sender: TObject);
begin
  PrintTest;
end;

procedure TForm1.Button5Click(Sender: TObject);
begin
  UpdatePrinterInfo;
end;

procedure TForm1.Button6Click(Sender: TObject);
begin
  {$IFDEF MSWindows}
  TWinPrinter(Printer).AdvancedProperties;
  {$ELSE}
  ShowMessage('Printer.AdvancedProperties is not yet implemented for this platform');
  {$ENDIF}
  UpdatePrinterInfo;
end;

procedure TForm1.Button7Click(Sender: TObject);
var
 s : String;
begin
  PageD.Title:= txtPageSetupDlgTitle.Text;
  if PAGED.Execute then
  begin
    UpdatePrinterInfo;
    with PAGED  do begin
      if PAGED.Units = unMM then
      begin
        s :=' milimeters';
        s := Format('[%d,%d,%d,%d] %s',[Margins.Top div 100,
          Margins.Left div 100, Margins.Bottom div 100, Margins.Right div 100,
          s]);
      end
      else
      begin
        s :=' inches';
        s := Format('[%d,%d,%d,%d] %s',[Margins.Top div 1000,
          Margins.Left div 1000,Margins.Bottom div 1000,Margins.Right div 1000,
          s]);
      end;
      AddInfo('Margins',s);
    end;
  end;
end;

procedure TForm1.btnRotateBinClick(Sender: TObject);
var
  i,j: Integer;
  cur,def: String;
  Lst: TStrings;
begin

  // get list of bins
  Lst := Printer.SupportedBins;
  cur := Printer.BinName;

  // get current bin index and find next bin in list
  if Lst.Count>0 then begin
    i := Lst.IndexOf(cur);
    inc(i);
    if i>Lst.Count-1 then
      i := 0;
    cur := Lst[i];
  end else
    cur := '';

  // select next bin
  Printer.BinName:=cur;
  UpdatePrinterInfo;
end;

procedure TForm1.btnRestoreDefaultBinClick(Sender: TObject);
begin
  Printer.RestoreDefaultBin;
  UpdatePrinterInfo;
end;

procedure TForm1.FormCreate(Sender: TObject);
var
  rowCount: Integer;
begin
  rowCount := SGrid.RowCount;
  if SGrid.FixedRows=1 then
    SGrid.RowHeights[0] := btnTPrintDialog.Height;
  UpdatePrinterInfo;
end;

procedure TForm1.SGridSelectCell(Sender: TObject; aCol, aRow: Integer;
  var CanSelect: Boolean);
begin
  CanSelect := ACol>0;
end;

procedure TForm1.doPrintDialog(withSample: boolean);
var
 s,x : String;
begin
  PD.Title := txtPrintDialogTitle.Text;
  PD.PrintToFile := false;
  if PD.Execute then
  begin
    UpdatePrinterInfo;
    if PD.Collate then AddInfo('Collate','true')
    else               AddInfo('Collate','false');
    if PD.PrintRange=prPageNums then x :='Pages range,';
    if PD.PrintRange=prSelection then x :='Selection,';
    if PD.PrintToFile then x := x + ' ,PrintToFile,';
    if withSample then
      PrintTest
    else begin
      s := Format(x + ' From : %d to %d,Copies:%d',[PD.FromPage,PD.ToPage,PD.Copies]);
      Application.MessageBox(pChar(s),'Info',mb_iconinformation);
    end;
  end;
end;

procedure TForm1.PrintTest;
begin
  case comboTests.ItemIndex of
    1:
      PrintSamplePage(true);
    2:
      PrintMultiPage;
    else
      PrintSamplePage(false);
  end;
end;

procedure TForm1.btnTPrintDialogClick(Sender: TObject);
begin
  doPrintDialog(false);
end;

procedure TForm1.btnPrintWithDlgClick(Sender: TObject);
begin
  doPrintDialog(true);
end;

end.
