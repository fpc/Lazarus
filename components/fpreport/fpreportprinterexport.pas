{
    This file is part of the Free Component Library.
    Copyright (c) 2016 Michael Van Canneyt, member of the Free Pascal development team

    FPReport export - render fpReport to printer.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit fpreportprinterexport;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpreport, fpreportlclexport, printers, Dialogs, PrintersDlgs;

Type

  { TFPreportPrinterExport }

  TFPreportPrinterExport = Class(TFPReportExporter)
  private
    FCurrentPage: Integer;
    FFirstPage: Integer;
    FLastPage: Integer;
    FPrinter: TPrinter;
    FShowPrinterDialog: Boolean;
    function DoShowPrinterDialog(AMaxPageCount: Integer): Boolean;
    function MustPrintPage(PageNo: Integer): Boolean;
    procedure PrintPage(P: TPrinter; E: TFPReportExportCanvas; PageNo: Integer);
  Protected
    procedure DoExecute(const ARTObjects: TFPList); override;
  Public
    Constructor Create(AOwner: TComponent); override;
    Class Function Name : String; override;
    Class Function Description : String; override;
    Property Printer : TPrinter Read FPrinter Write FPrinter;
  Published
    Property ShowPrinterDialog : Boolean Read FShowPrinterDialog Write FShowPrinterDialog;
    Property FirstPage : Integer Read FFirstPage Write FFirstPage;
    Property LastPage : Integer Read FLastPage Write FLastPage;
    Property CurrentPage : Integer Read FCurrentPage Write FCurrentPage;
  end;

implementation


Resourcestring
  SErrNoPrinter = 'No printer configured.';

{ TFPreportPrinterExport }

function TFPreportPrinterExport.MustPrintPage(PageNo: Integer): Boolean;

begin
  Result:=(PageNo>=FirstPage)
          and ((LastPage=0) or (PageNo<=LastPage));
end;

procedure TFPreportPrinterExport.PrintPage(P : TPrinter; E : TFPReportExportCanvas; PageNo : Integer);

begin
  E.Canvas:=P.Canvas;
  E.PageIndex:=PageNo;
  E.Execute;
end;

function TFPreportPrinterExport.DoShowPrinterDialog(AMaxPageCount: Integer
  ): Boolean;

var
  Dlg : TPrintDialog;

begin
  Dlg:=TPrintDialog.Create(Self);
  try
    Dlg.FromPage:=FirstPage;
    Dlg.ToPage:=LastPage;
    Dlg.MaxPage:=AMaxPageCount;
    Dlg.MinPage:=1;
    Dlg.Options:=[poPrintToFile, poPageNums];
    if (CurrentPage>0) and (CurrentPage<=AMaxPageCount) then

    Dlg.PrintRange:=prAllPages;
    Result:=Dlg.Execute;
    if Result then
      begin
      if Dlg.PrintRange=prPageNums then
        begin
        Dlg.FromPage:=FirstPage;
        Dlg.ToPage:=LastPage;
        end;
      end;
  finally
    Dlg.Free;
  end;
end;

procedure TFPreportPrinterExport.DoExecute(const ARTObjects: TFPList);

Var
  P : TPrinter;
  E : TFPReportExportCanvas;
  I : Integer;
  First : Boolean;

begin
  if ShowPrinterDialog then
    if not DoShowPrinterDialog(ARTObjects.Count) then exit;
  P:=Printer;
  if P=Nil then
    P:=Printers.Printer;
  if P=Nil then
    Raise EReportExportError.Create(SErrNoPrinter);
  E:=TFPReportExportCanvas.Create(Self);
  try
    E.Report:=Self.Report;
    E.HDPI:=P.XDPI;
    E.VDPI:=P.YDPI;
    First:=true;
    For I:=0 to ARTObjects.Count-1 do
      if MustPrintPage(I+1) and Not P.Aborted then
        begin
        if First then
          P.BeginDoc
        else
          P.NewPage;
        PrintPage(P,E,I);
        First:=False;
        end;
    P.EndDoc;
  finally
    E.Free;
  end;
end;

constructor TFPreportPrinterExport.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ShowPrinterDialog:=True;
end;


class function TFPreportPrinterExport.Name: String;
begin
  Result:='Print';
end;

class function TFPreportPrinterExport.Description: String;
begin
  Result:='Print to printer';
end;


initialization
  TFPreportPrinterExport.RegisterExporter;
end.

