unit cocoaprndelegate;

{$mode objfpc}{$H+}
{$modeswitch objectivec1}

interface

uses
  Classes, SysUtils, Math, MacOSAll, CocoaAll, OSPrinters, Printers, Dialogs,
  PrintersDlgs;

type

  { PrintDialogDelegate }

  PrintDialogDelegate = objcclass(NSObject)
  private
    _collate: boolean;
    _copies, _firstPage, _lastPage: UInt32;
    procedure RawPrintJobDidRun(panel: NSPrintPanel; returnCode:NSInteger; contextInfo:Pointer); message 'RawPrintJobDidRun:returnCode:contextInfo:';
    procedure PrintJobDidRun(op:NSPrintOperation; success:boolean; contextInfo:Pointer); message 'PrintJobDidRun:success:contextInfo:';
    procedure SetupPrinterDidRun(panel: NSPrintPanel; returnCode:NSInteger; contextInfo:Pointer); message 'SetupPrinterDidRun:returnCode:contextInfo:';
    procedure UpdatePrinter(checkList:Boolean; returnCode:NSInteger); message 'UpdatePrinter:returnCode:';
    procedure BackupPrintSettings; message 'BackupPrintSettings';
    procedure PrintSettingsFromDialog; message 'PrintSettingsFromDialog';
    procedure PrintSettingsToDialog; message 'PrintSettingsToDialog';
    procedure RestorePrintSettings; message 'RestorePrintSettings';
  public
    response: TDialogResultEvent;
    attachToWindow: NSWindow;
    renderView: NSView;
    sender: TObject;
    printDialog: TPrintDialog;
    onStartJob: TNotifyEvent;

    function RunSetupPrinter: boolean; message 'RunSetupPrinter';
    function RunPrintJob: boolean; message 'RunPrintJob';
  end;

var
  printDelegate: PrintDialogDelegate;

implementation

{ PrintDialogDelegate }

procedure PrintDialogDelegate.SetupPrinterDidRun(panel: NSPrintPanel;
  returnCode: NSInteger; contextInfo: Pointer);
begin
  UpdatePrinter(true, returnCode);
  if Assigned(response) then
    response(sender, returnCode=NSOKButton);
  self.release;
end;

function PrintDialogDelegate.RunSetupPrinter: boolean;
var
  PrintPanel: NSPrintPanel;
  pInfo: NSPrintInfo;
  res: NSInteger;
begin
  PrintPanel := NSPrintPanel.printPanel;
  //PrintPanel.setJobStyleHint(NSPrintNoPresetsJobStyleHint);
  PrintPanel.setOptions((PrintPanel.options or NSPrintPanelShowsPaperSize) and not NSPrintPanelShowsPageRange);
  PrintPanel.setDefaultButtonTitle(NSSTR('OK'));
  pInfo := NSPrintInfo.sharedPrintInfo;

  if Assigned(response) and (attachToWindow<>nil) then
  begin
    PrintPanel.beginSheetWithPrintInfo_modalForWindow_delegate_didEndSelector_contextInfo(
      pInfo,
      attachToWindow,
      self,
      objcselector('SetupPrinterDidRun:returnCode:contextInfo:'),
      nil
      );
    result := true;
  end else
  begin
    res := PrintPanel.runModalWithPrintInfo(pInfo);
    UpdatePrinter(true, res);
    self.release;
  end;
end;

procedure PrintDialogDelegate.PrintJobDidRun(op: NSPrintOperation;
  success: boolean; contextInfo: Pointer);
begin
  if not success then
    RestorePrintSettings
  else
    PrintSettingsToDialog;

  if success and assigned(onStartJob) then
    OnStartJob(sender);

  if assigned(response) then
    response(sender, success);

  self.release;
end;

procedure PrintDialogDelegate.RawPrintJobDidRun(panel: NSPrintPanel;
  returnCode: NSInteger; contextInfo: Pointer);
var
  success: boolean;
begin
  success := (returnCode=NSOKButton);
  PrintJobDidRun(nil, success, nil);
end;

function PrintDialogDelegate.RunPrintJob: boolean;
var
  pInfo: NSPrintInfo;
  printOp: NSPrintOperation;
  printPanel: NSPrintPanel;
  res: NSInteger;
begin
  pInfo := NSPrintInfo.sharedPrintInfo;

  BackupPrintSettings;
  PrintSettingsFromDialog;

  if renderView=nil then
  begin
    printPanel := NSPrintPanel.printPanel;
    if assigned(response) and (attachToWindow<>nil) then
    begin
      result := true;
      printPanel.beginSheetWithPrintInfo_modalForWindow_delegate_didEndSelector_contextInfo(
        pInfo,
        attachToWindow,
        self,
        ObjCSelector('RawPrintJobDidRun:returnCode:contextInfo:'),
        nil
      );
    end else
    begin
      res := printPanel.runModalWithPrintInfo(pInfo);
      result := (res=NSOKButton);
      PrintJobDidRun(nil, result, nil);
    end;
  end else
  begin
    printOp := NSPrintOperation.printOperationWithView_printInfo(renderView, pInfo);
    if Assigned(response) and (attachToWindow<>nil) then
    begin
      result := true;
      printOp.runOperationModalForWindow_delegate_didRunSelector_contextInfo(
        attachToWindow,
        self,
        objcselector('PrintJobDidRun:success:contextInfo:'),
        nil
      );
    end else
    begin
      result := printOP.runOperation;
      PrintJobDidRun(printOp, result, nil);
    end;
  end;

end;

procedure PrintDialogDelegate.UpdatePrinter(checkList:Boolean; returnCode: NSInteger);
var
  CocoaPrinter: TCocoaPrinter;
begin
  CocoaPrinter := Printer as TCocoaPrinter;
  if checkList then
    CocoaPrinter.CheckPrinterList;
  if returnCode=NSOKButton then
    CocoaPrinter.UpdatePrinter;
end;

procedure PrintDialogDelegate.BackupPrintSettings;
var
  pInfo: NSPrintInfo;
begin
  pInfo := NSPrintInfo.sharedPrintInfo;

  _Collate := false;
  _Copies := 1;
  _firstPage := 1;
  _lastPage := 1;
  PMGetCollate(pInfo.PMPrintSettings, _Collate);
  PMGetCopies(pInfo.PMPrintSettings, _Copies);
  PMGetFirstPage(pInfo.PMPrintSettings, _firstPage);
  PMGetLastPage(pInfo.PMPrintSettings, _lastPage);
end;

procedure PrintDialogDelegate.PrintSettingsFromDialog;
var
  PMin, PMax, PFrom, PTo: Integer;
  pInfo: NSPrintInfo;
  s: string;
begin
  pInfo := NSPrintInfo.sharedPrintInfo;

  PMSetCollate(pInfo.PMPrintSettings, printDialog.Collate);
  PMSetCopies(pInfo.PMPrintSettings, printDialog.Copies, False);

  PMin := printDialog.MinPage;
  PMax := Math.Max(PMin, printDialog.MaxPage);
  PFrom := Math.Min(Math.Max(printDialog.FromPage, PMin), PMax);
  PTo := Max(PFrom, Min(printDialog.ToPage, PMax));
  PMSetPageRange(pInfo.PMPrintSettings, PMin, PMax);

  if printDialog.PrintRange <> prAllPages then
  begin
    PMSetFirstPage(pInfo.PMPrintSettings, PFrom, False);
    PMSetLastPage(pInfo.PMPrintSettings, PTo, False);
  end;
  // TODO: PrintToFile

  pInfo.updateFromPMPrintSettings;
end;

procedure PrintDialogDelegate.PrintSettingsToDialog;
var
  pInfo: NSPrintInfo;
  Collate: Boolean;
  Copies, firstPage, lastPage: UInt32;
begin
  pInfo := NSPrintInfo.sharedPrintInfo;

  PMGetCollate(pInfo.PMPrintSettings, Collate);
  PMGetCopies(pInfo.PMPrintSettings, Copies);
  PMGetFirstPage(pInfo.PMPrintSettings, firstPage);
  PMGetLastPage(pInfo.PMPrintSettings, lastPage);

  if lastPage>$FFFF then
  begin
    printDialog.PrintRange := prAllPages;
    printDialog.FromPage := printDialog.MinPage;
    printDialog.ToPage := printDialog.MaxPage;
  end else
  begin
    printDialog.FromPage := firstPage;
    printDialog.ToPage := lastPage;
  end;
  printDialog.Collate := Collate;
  printDialog.Copies := Copies;
  //TODO: PrintToFile
end;

procedure PrintDialogDelegate.RestorePrintSettings;
var
  pInfo: NSPrintInfo;
begin
  pInfo := NSPrintInfo.sharedPrintInfo;

  PMSetCollate(pInfo.PMPrintSettings, _Collate);
  PMSetCopies(pInfo.PMPrintSettings, _Copies, False);
  PMSetFirstPage(pInfo.PMPrintSettings, _firstPage, False);
  PMSetLastPage(pInfo.PMPrintSettings, _lastPage, False);

  pInfo.updateFromPMPrintSettings;
end;



end.

