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
  Classes, SysUtils,dialogs,Printers,osPrinters,LResources;

type
  {Type for compatibility with delphi}
  TPrintRange = (prAllPages,prSelection,prPageNums);
  TPrintDialogOption= (poPrintToFile,poPageNums,poSelection,
                       poWarning,poHelp,poDisablePrintToFile);
  TPrintDialogOptions = set of TPrintDialogOption;
  
  TPrinterSetupDialog = class(TCommonDialog)
  public
    function Execute: Boolean; override;
  end;
  
  { TPrintDialog }

  TPrintDialog = class(TCommonDialog)
  private
    fFromPage    : Integer;
    fToPage      : Integer;
    fCollate     : Boolean;
    fOptions     : TPrintDialogOptions;
    fPrintToFile : Boolean;
    fPrintRange  : TPrintRange;
    fMinPage     : Integer;
    fMaxPage     : Integer;
    fCopies      : Integer;
  public
    function Execute: Boolean; override;
    constructor Create(TheOwner: TComponent); override;
  published
    property Collate: Boolean read fCollate write fCollate default False;
    property Copies: Integer read fCopies write fCopies default 0;
    property FromPage: Integer read fFromPage write fFromPage default 0;
    property MinPage: Integer read fMinPage write fMinPage default 0;
    property MaxPage: Integer read fMaxPage write fMaxPage default 0;
    property Options: TPrintDialogOptions read fOptions write fOptions default [];
    property PrintToFile: Boolean read fPrintToFile write fPrintToFile default False;
    property PrintRange: TPrintRange read fPrintRange write fPrintRange default prAllPages;
    property ToPage: Integer read fToPage write fToPage default 0;
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

constructor TPrintDialog.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  fCollate:=False;
  fCopies :=1;
  fFromPage:=0;
  fToPage:=0;
  fMinPage:=0;
  fMaxPage:=0;
  fOptions:=[];
  fPrintToFile:=False;
  fPrintRange:=prAllPages;
end;

INITIALIZATION
{$I printersdlgs.lrs}
end.

