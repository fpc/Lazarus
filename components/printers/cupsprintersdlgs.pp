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
    Common component dialogs for setup and more printers

  See : /usr/share/doc/cups/licence.txt for the CUPS licence

  History
   Fev  26 2004 - Create
   Mar  09 2004 OG - Add TPrintJobsDialog component
   Mar  16 2004 OG - add LResources units
------------------------------------------------------------------------------}
unit CUPSPrintersDlgs;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,dialogs,CupsPrinters,udlgPrintersJobs,LResources;

type
  TPrinterSetupDialog = class(TCommonDialog)
  public
    function Execute: Boolean; override;
  end;

  TPrintJobsDialog    = class(TCommonDialog)
  public
    function Execute: Boolean; override;
  end;
  
procedure Register;


implementation


procedure Register;
begin
  RegisterComponents('Dialogs',[TPrinterSetupDialog,TPrintJobsDialog]);
end;

{ TPrinterSetupDialog }

function TPrinterSetupDialog.Execute: Boolean;
begin
  Result:=(Printer.ExecuteSetup<>srCancel);
end;

{ TPrintJobsDialog }

function TPrintJobsDialog.Execute: Boolean;
begin
  Result:=True;
  With TdlgPrintersJobs.Create(nil) do
  try
    ShowModal;
  finally
    Free;
  end;
end;

INITIALIZATION
{$I cupsprintersdlgs.lrs}
end.

