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
  Classes, SysUtils,dialogs,Printers,
  {$IFDEF WIN32}
  WinPrinters,
  {$ENDIF}
  LResources;

type
  TPrinterSetupDialog = class(TCommonDialog)
  public
    function Execute: Boolean; override;
  end;
  
  TPrinterDialog = class(TCommonDialog)
  public
    function Execute: Boolean; override;
  end;

procedure Register;


implementation


procedure Register;
begin
  RegisterComponents('Dialogs',[TPrinterSetupDialog,TPrinterDialog]);
end;

{ TPrinterSetupDialog }

function TPrinterSetupDialog.Execute: Boolean;
begin
  Result:=Printer.ExecuteSetup;
end;

{ TPrinterDialog }

function TPrinterDialog.Execute: Boolean;
begin
  Result:=Printer.ExecuteSetup;
end;

INITIALIZATION
{$I printersdlgs.lrs}
end.

