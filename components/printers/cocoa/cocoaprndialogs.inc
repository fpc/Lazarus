{%MainUnit ../printersdlgs.pp}


const
  SExecute = 'Execute';

{ TPageSetupDialog }

function TPageSetupDialog.DoExecute: Boolean;
var
  CocoaPrinter: TCocoaPrinter;
begin
  Result := False;
  // TODO: set and get paper margins, title

  if not Assigned(Printer) then Exit;
  
  CocoaPrinter := Printer as TCocoaPrinter;
 
{  if OSError(PMSessionPageSetupDialog(CarbonPrinter.PrintSession,
      CarbonPrinter.PageFormat, Result),
    Self, SExecute, 'PMSessionPageSetupDialog') then Exit;

  if Result then CarbonPrinter.Validate;   }
end;


{ TPrinterSetupDialog }

function TPrinterSetupDialog.DoExecute: Boolean;
begin
  Result := False;
  if not Assigned(Printer) then Exit;
  if Printer.Printers.Count <= 0 then Exit;
  
  raise Printers.EPrinter.Create('TPrinterSetupDialog is not supported in Cocoa!');
end;


{ TPrintDialog }

function TPrintDialog.DoExecute: Boolean;
var
  CocoaPrinter: TCocoaPrinter;
  PrintPanel: NSPrintPanel;
  PrintPanelInfo: NSPrintInfo;
  PrintSettings: PMPrintSettings;
  V: UInt32;
  B: Boolean;
  PMin, PMax, PFrom, PTo: Integer;
  lDialogResult: NSInteger;
begin
  Result := False;
  // TODO: Options, Title

  if not Assigned(Printer) then Exit;

  CocoaPrinter := Printer as TCocoaPrinter;
  //DebugLn('TPrintDialog.Execute ' + CarbonPrinter.CurrentPrinterName);

  // Create the panel and the info object
  PrintPanel := NSPrintPanel.printPanel();
  PrintPanelInfo := NSPrintInfo.alloc.initWithDictionary(NSDictionary.dictionary());
  try
    // Set all properties
    PrintSettings := PrintPanelInfo.PMPrintSettings();

    PMSetCollate(PrintSettings, Collate);
    PMSetCopies(PrintSettings, Copies, False);

    PMin := MinPage;
    PMax := Max(PMin, MaxPage);
    PFrom := Min(Max(FromPage, PMin), PMax);
    PTo := Max(PFrom, Min(ToPage, PMax));

    PMSetPageRange(PrintSettings, PMin, PMax);
    if PrintRange <> prAllPages then
    begin
      PMSetFirstPage(PrintSettings, PFrom, False);
      PMSetLastPage(PrintSettings, PTo, False);
    end;

    // Run the dialog
    PrintPanelInfo.updateFromPMPrintSettings();
    lDialogResult := PrintPanel.runModalWithPrintInfo(PrintPanelInfo);

    if lDialogResult = NSOKButton then // NSCancelButton for Cancel
    begin
      Result := True;

      PrintSettings := PrintPanelInfo.PMPrintSettings();

      B := Collate;
      PMGetCollate(PrintSettings, B);
      Collate := B;

      V := Copies;
      PMGetCopies(PrintSettings, V);
      Copies := V;

      PMGetLastPage(PrintSettings, V);
      if V > $FFFF then
      begin
        PrintRange := prAllPages;
        FromPage := PMin;
        ToPage := PMax;
      end
      else
      begin
        PrintRange := prSelection;
        ToPage := V;
        PMGetFirstPage(PrintSettings, V);
        FromPage := V;
      end;

      PMCopyPrintSettings(PrintSettings, CocoaPrinter.PrintSettings);
      CocoaPrinter.PrintInfo.updateFromPMPrintSettings();

      CocoaPrinter.PrintView.PageMin := PMin;
      CocoaPrinter.PrintView.PageMax := PMax;
      CocoaPrinter.PrintView.PageFrom := FromPage;
      CocoaPrinter.PrintView.PageTo := ToPage;
    end;
  finally
  end;
end;

