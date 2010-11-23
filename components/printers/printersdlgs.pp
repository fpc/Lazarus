{*****************************************************************************
 *                                                                           *
 *  This file is part of the Lazarus Component Library (LCL)                 *
 *                                                                           *
 *  See the file COPYING.modifiedLGPL.txt, included in this distribution,    *
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
  Classes, SysUtils, Forms, Dialogs, LResources, Printers, OsPrinters;

type

  TMeasureUnits = (unMM,unInch);
  { Type for compatibility with delphi }
  
  { TPageSetupDialog }
  
  TPageSetupDialog = class(TCustomPrinterSetupDialog)
  private
   fMargins : TRect;
   fUnits : TMeasureUnits;
  public
    constructor Create(TheOwner: TComponent); override;
    function Execute: Boolean; override;
    property Margins : TRect read fMargins write fMargins;
    property Units : TMeasureUnits read fUnits;
  end;

  { TPrinterDialog }
  
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

{$IFDEF UNIX}
  {$IFDEF LCLCarbon}
    {$IFNDEF NativePrint}
    
// add units as needed for carbon, for the moment use cups ones.
uses Controls, udlgSelectPrinter, udlgPropertiesPrinter, FileUtil;
{$I cupsprndialogs.inc}

    {$ELSE}
    
uses
  Controls, Math, CarbonProc,
{$ifdef ver2_2_0}
  FPCMacOSAll,
{$else}
  MacOSAll,
{$endif}
  LCLProc;
{$I carbonprndialogs.inc}

    {$ENDIF}
  {$ELSE}
    {$IFDEF LCLQt}
    uses Controls, qtobjects, qt4, FileUtil;
    {$I qtprndialogs.inc}
   {$ELSE}
uses Controls, udlgSelectPrinter, udlgPropertiesPrinter, udlgPageSetup, FileUtil;
{$I cupsprndialogs.inc}
  {$ENDIF}
  {$ENDIF}
{$ENDIF}

{$IFDEF MSWindows}
  {$IFDEF LCLQt}
    uses Windows, Controls,
    qtobjects, qtwidgets, qt4, LCLIntf, LCLType, FileUtil;
    {$I qtprndialogs.inc}
  {$ELSE}

  uses Windows, WinUtilPrn, InterfaceBase, LCLIntf, LCLType, WinVer;
  {$I winprndialogs.inc}
  {$ENDIF}

{$ENDIF}

constructor TPageSetupDialog.Create(TheOwner: TComponent);
begin
 inherited Create(TheOwner);
 fMargins.Bottom := 0;
 fMargins.Left := 0;
 fMargins.Right := 0;
 fMargins.Top := 0;
end;

procedure Register;
begin
  RegisterComponents('Dialogs',[TPrinterSetupDialog,TPrintDialog,TPageSetupDialog]);
end;

initialization
  {$I printersdlgs.lrs}
end.
