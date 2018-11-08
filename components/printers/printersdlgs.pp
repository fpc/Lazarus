{
 *****************************************************************************
  This file is part of the Printer4Lazarus package

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************

  Author: Olivier GUILBAUD

  Abstract:
    Common component dialogs for select or setup printers

------------------------------------------------------------------------------}
unit PrintersDlgs;

{$mode objfpc}
{$IFDEF LCLCocoa}
  {$modeswitch objectivec1}
{$ENDIF}
{$H+}

interface

{$IFDEF WinCE}
{$FATAL This unit (and therefore the Printers4Lazarus package) cannot be built for WinCE}
{$ENDIF}

uses
  Classes, SysUtils, Forms, Controls, Dialogs,
  LResources,
  Printer4LazStrConst,
  Printers, OsPrinters;

type
  TPageMeasureUnits = (
    pmDefault,
    pmMillimeters,
    pmInches
    );

type
  TPageSetupDialogOption = (
    psoDefaultMinMargins,
    psoDisableMargins,
    psoDisableOrientation,
    psoDisablePagePainting,
    psoDisablePaper,
    psoDisablePrinter,
    psoMargins,
    psoMinMargins,
    psoShowHelp,
    psoWarning,
    psoNoNetworkButton
    );

  TPageSetupDialogOptions = set of TPageSetupDialogOption;

const
  cDefaultPageSetupDialogOptions = [psoDefaultMinMargins];
  cDefaultPageSetupMargin = 0;
  cDefaultPageSetupMinMargin = 400; //400: in mm it's 4mm, in inches it's ~10mm

type
  { TPageSetupDialog }
  
  TPageSetupDialog = class(TCustomPrinterSetupDialog)
  private
    FPageWidth: integer;
    FPageHeight: integer;
    FMarginLeft: integer;
    FMarginTop: integer;
    FMarginRight: integer;
    FMarginBottom: integer;
    FMinMarginLeft: integer;
    FMinMarginTop: integer;
    FMinMarginRight: integer;
    FMinMarginBottom: integer;
    FUnits: TPageMeasureUnits;
    FOptions: TPageSetupDialogOptions;
  protected
    function DoExecute: Boolean; override;
  public
    constructor Create(TheOwner: TComponent); override;
    property OnDialogResult;
    property AttachTo;
  published
    property PageWidth: integer read FPageWidth write FPageWidth default 0;
    property PageHeight: integer read FPageHeight write FPageHeight default 0;
    property MarginLeft: integer read FMarginLeft write FMarginLeft default cDefaultPageSetupMargin;
    property MarginTop: integer read FMarginTop write FMarginTop default cDefaultPageSetupMargin;
    property MarginRight: integer read FMarginRight write FMarginRight default cDefaultPageSetupMargin;
    property MarginBottom: integer read FMarginBottom write FMarginBottom default cDefaultPageSetupMargin;
    property MinMarginLeft: integer read FMinMarginLeft write FMinMarginLeft default cDefaultPageSetupMinMargin;
    property MinMarginTop: integer read FMinMarginTop write FMinMarginTop default cDefaultPageSetupMinMargin;
    property MinMarginRight: integer read FMinMarginRight write FMinMarginRight default cDefaultPageSetupMinMargin;
    property MinMarginBottom: integer read FMinMarginBottom write FMinMarginBottom default cDefaultPageSetupMinMargin;
    property Options: TPageSetupDialogOptions read FOptions write FOptions default cDefaultPageSetupDialogOptions;
    property Units: TPageMeasureUnits read FUnits write FUnits default pmDefault;
  end;
  
  { TPrinterSetupDialog }

  TPrinterSetupDialog = class(TCustomPrinterSetupDialog)
  protected
    function DoExecute: Boolean; override;
  public
    property OnDialogResult;
    property AttachTo;
  end;

  { TPrintDialog }

  TPrintDialog = class(TCustomPrintDialog)
  protected
    function DoExecute: Boolean; override;
  public
    property OnDialogResult;
    property AttachTo;
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

{$R printersdlgs.res}

{$IFDEF UNIX}
  {$IFDEF DARWIN}
    {$IFDEF LCLCarbon}
      {$IFNDEF NativePrint}
        // add units as needed for carbon, for the moment use cups ones.
        uses udlgSelectPrinter, udlgPropertiesPrinter, udlgPageSetup, FileUtil;
        {$I cupsprndialogs.inc}
      {$ELSE}
        uses Math, CarbonProc, MacOSAll, LCLProc;
        {$I carbonprndialogs.inc}
      {$ENDIF}
    {$ENDIF}
    {$IFDEF LCLCocoa}
      uses Math, CocoaAll, MacOSAll, LCLProc, cocoaprndelegate;
      {$I cocoaprndialogs.inc}
    {$ENDIF}
    {$IFDEF LCLQt}
      uses qtobjects, qt4, qtint, LazUTF8;
      {$I qtprndialogs.inc}
    {$ENDIF}
    {$IFDEF LCLQt5}
      uses qtobjects, qt5, qtint, LazUTF8;
      {$I qtprndialogs.inc}
    {$ENDIF}    
    {$IFDEF LCLGtk2}
      uses udlgSelectPrinter, udlgPropertiesPrinter, udlgPageSetup;
      {$I cupsprndialogs.inc}
    {$ENDIF}
  {$ELSE}
    {$IFDEF LCLQt}
      uses qtobjects, qt4, qtint, LazUTF8;
      {$I qtprndialogs.inc}
    {$ELSE}
    {$IFDEF LCLQt5}
      uses qtobjects, qt5, qtint, LazUTF8;
      {$I qtprndialogs.inc}
    {$ELSE}    
      uses udlgSelectPrinter, udlgPropertiesPrinter, udlgPageSetup;
      {$I cupsprndialogs.inc}
    {$ENDIF}
    {$ENDIF}    
  {$ENDIF}
{$ENDIF}

{$IFDEF MSWindows}
  {$IFDEF LCLQt}
    uses Windows,
    qtobjects, qtwidgets, qt4, LCLIntf, LCLType, LazUTF8;
    {$I qtprndialogs.inc}
  {$ELSE}
  {$IFDEF LCLQt5}
    uses Windows,
    qtobjects, qtwidgets, qt5, LCLIntf, LCLType, LazUTF8;
    {$I qtprndialogs.inc}
  {$ELSE}  
    uses Windows, WinUtilPrn, InterfaceBase, LCLIntf, LCLType, WinVer;
    {$I winprndialogs.inc}
  {$ENDIF}
  {$ENDIF}  

{$ENDIF}

constructor TPageSetupDialog.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FPageWidth:= 0;
  FPageHeight:= 0;
  FMarginLeft:= cDefaultPageSetupMargin;
  FMarginTop:= cDefaultPageSetupMargin;
  FMarginRight:= cDefaultPageSetupMargin;
  FMarginBottom:= cDefaultPageSetupMargin;
  FMinMarginLeft:= cDefaultPageSetupMinMargin;
  FMinMarginTop:= cDefaultPageSetupMinMargin;
  FMinMarginRight:= cDefaultPageSetupMinMargin;
  FMinMarginBottom:= cDefaultPageSetupMinMargin;
  FOptions:= cDefaultPageSetupDialogOptions;
  FUnits:= pmDefault;
end;

procedure Register;
begin
  RegisterComponents('Dialogs',[TPrinterSetupDialog,TPrintDialog,TPageSetupDialog]);
end;

end.
