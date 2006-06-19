{*****************************************************************************
 *                                                                           *
 *  This file is part of the Lazarus Component Library (LCL)                 *
 *                                                                           *
 *  See the file COPYING.modifiedLGPL, included in this distribution,        *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************

  Author: Olivier GUILBAUD

  Abstract:
    Little sample for show how to use PrintersDlgs unit

------------------------------------------------------------------------------}
unit frmselprinter;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, Buttons,
  PrintersDlgs, StdCtrls, Grids, Menus;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    Button6: TButton;
    Button7: TButton;
    Label1: TLABEL;
    PAGED: TPageSetupDialog;
    PD: TPrintDialog;
    PopupMenu1: TPopupMenu;
    PSD: TPrinterSetupDialog;
    SGrid: TStringGrid;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure Button7Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    ck : Integer;
    procedure UpdatePrinterInfo;
    procedure AddInfo(const Desc : String; Const Info : String);
  public
  
    { public declarations }
  end; 

var
  Form1: TForm1; 

implementation
uses
  Printers,OsPrinters,LCLType,LClProc;

{ TForm1 }

procedure TForm1.AddInfo(const Desc : String; Const Info : String);
begin
  SGrid.Cells[0,ck] := Desc;
  SGrid.Cells[1,ck] := Info;
  Inc(ck);
end;

procedure TForm1.UpdatePrinterInfo;
begin
  try
    ck := 1;
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
      AddInfo('XDPI',IntToStr(XDPI)+' dpi');
      AddInfo('YDPI',IntToStr(YDPI)+' dpi');
      AddInfo('Copies',IntToStr(Copies));
      AddInfo('PageHeight',IntToStr(PageHeight)+' dots (printable size)');
      AddInfo('PageWidth',IntToStr(PageWidth)+' dots (printable size)');
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
      AddInfo('PaperSize',PaperSize.PaperName);
      if CanRenderCopies then AddInfo('CanRenderCopies','true')
      else
      AddInfo('CanRenderCopies','false');

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
  if PSD.Execute then
    UpdatePrinterInfo;
end;

procedure TForm1.Button3Click(Sender: TObject);
begin
  Printer.PrinterIndex := -1;
  UpdatePrinterInfo;
end;

procedure TForm1.Button4Click(Sender: TObject);
begin
  try
    Printer.Title := 'Printer test for printers4lazarus package';
    Printer.BeginDoc;
    Printer.Canvas.Font.Color:= clBlue;
    Printer.Canvas.Font.Size := 12;
    Printer.Canvas.TextOut(0,0,'This is test for lazarus printer4lazarus package');
    Printer.EndDoc;
  except
    on E:Exception do
    begin
      Printer.Abort;
      Application.MessageBox(pChar(e.message),'Error',mb_iconhand);
    end;
  end;
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

procedure TForm1.FormCreate(Sender: TObject);
begin
  UpdatePrinterInfo;
end;

procedure TForm1.Button1Click(Sender: TObject);
var
 s,x : String;
begin
  if PD.Execute then
  begin
   UpdatePrinterInfo;
   if PD.Collate then AddInfo('Collate','true')
   else
   AddInfo('Collate','false');
   if PD.PrintRange=prPageNums then x :='Pages range,';
   if PD.PrintRange=prSelection then x :='Selection,';
   if PD.PrintToFile then x := x + ' ,PrintToFile,';
   s := Format(x + ' From : %d to %d,Copies:%d',[PD.FromPage,PD.ToPage,PD.Copies]);
   Application.MessageBox(pChar(s),'Info',mb_iconinformation);
  end;
end;

initialization
  {$I frmselprinter.lrs}

end.
