{*****************************************************************************
 *                                                                           *
 *  This file is part of the Lazarus Component Library (LCL)                 *
 *                                                                           *
 *  See the file COPYING.LCL, included in this distribution,                 *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************

  Author: Olivier GUILBAUD

  Abstract:
    Common component dialogs for select or setup printers

------------------------------------------------------------------------------}
unit PrintersDlgs;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Dialogs, Printers, osPrinters, LResources;

type
  { Type for compatibility with delphi }

  TPrinterSetupDialog = class(TCustomPrinterSetupDialog)
  public
    function Execute: Boolean; override;
  end;
  
  { TPrintDialog }

  TPrintDialog = class(TCustomPrintDialog)
  public
    function Execute: Boolean; override;
  published
    property Collate;
    property Copies;
    property FromPage;
    property MinPage;
    property MaxPage;
    property Options;
    property PrintToFile;
    property PrintRange;
    property ToPage;
  end;

procedure Register;


implementation


procedure Register;
begin
  RegisterComponents('Dialogs',[TPrinterSetupDialog,TPrintDialog]);
end;

{ TPrinterSetupDialog }

function TPrinterSetupDialog.Execute: Boolean;
begin
  Result:=Printer.PrinterSetup;
end;

{ TPrinterDialog }

function TPrintDialog.Execute: Boolean;
begin
  Result:=Printer.PrintDialog;
end;

initialization
  {$I printersdlgs.lrs}
  
end.

