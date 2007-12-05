{%MainUnit ../printersdlgs.pp}


const
  SExecute = 'Execute';

{ TPageSetupDialog }

function TPageSetupDialog.Execute: Boolean;
var
  CarbonPrinter: TCarbonPrinter;
begin
  Result := False;
  // TODO: set and get paper margins, title

  if not Assigned(Printer) then Exit;
  if Printer.Printers.Count <= 0 then Exit;
  
  CarbonPrinter := Printer as TCarbonPrinter;
 
  if OSError(PMSessionPageSetupDialog(CarbonPrinter.PrintSession,
      CarbonPrinter.PageFormat, Result),
    Self, SExecute, 'PMSessionPageSetupDialog') then Exit;

  if Result then CarbonPrinter.Validate;
end;


{ TPrinterSetupDialog }

function TPrinterSetupDialog.Execute: Boolean;
begin
  Result := False;
  if not Assigned(Printer) then Exit;
  if Printer.Printers.Count <= 0 then Exit;
  
  raise Printers.EPrinter.Create('TPrinterSetupDialog is not supported in Carbon!');
end;


{ TPrintDialog }

function TPrintDialog.Execute: Boolean;
var
  CarbonPrinter: TCarbonPrinter;
  DialogSettings: PMPrintSettings;
  V: UInt32;
begin
  Result := False;
  // TODO: Options, Title

  if not Assigned(Printer) then Exit;
  if Printer.Printers.Count <= 0 then Exit;
  
  CarbonPrinter := Printer as TCarbonPrinter;
  //DebugLn('TPrintDialog.Execute ' + CarbonPrinter.CurrentPrinterName);
  
  if OSError(PMCreatePrintSettings(DialogSettings),
    Self, SExecute, 'PMCreatePrintSettings') then Exit;
  try
    if OSError(PMCopyPrintSettings(CarbonPrinter.PrintSettings, DialogSettings),
      Self, SExecute, 'PMCopyPrintSettings') then Exit;

    OSError(PMSetCollate(DialogSettings, Collate), Self, SExecute, 'PMSetCollate');
    OSError(PMSetCopies(DialogSettings, Copies, False), Self, SExecute, 'PMSetCopies');
    OSError(PMSetPageRange(DialogSettings, MinPage, MaxPage),
      Self, SExecute, 'PMSetPageRange');
    OSError(PMSetFirstPage(DialogSettings, FromPage, False), Self, SExecute, 'PMSetFirstPage');
    OSError(PMSetLastPage(DialogSettings, ToPage, False), Self, SExecute, 'PMSetLastPage');

    if OSError(PMSessionPrintDialog(CarbonPrinter.PrintSession, CarbonPrinter.PrintSettings, CarbonPrinter.PageFormat, Result),
      Self, SExecute, 'PMSessionPrintDialog') then Exit;

    if Result then
    begin
      PrintRange := prSelection;
      
      OSError(PMGetCollate(DialogSettings, Collate), Self, SExecute, 'PMGetCollate');
      
      V := Copies;
      OSError(PMGetCopies(DialogSettings, V), Self, SExecute, 'PMGetCopies');
      Copies := V;

      V := FromPage;
      OSError(PMGetFirstPage(DialogSettings, V), Self, SExecute, 'PMGetFirstPage');
      FromPage := V;
      V := ToPage;
      OSError(PMGetLastPage(DialogSettings, V), Self, SExecute, 'PMGetLastPage');
      ToPage := V;

    
      if OSError(PMCopyPrintSettings(DialogSettings, CarbonPrinter.PrintSettings),
        Self, SExecute, 'PMCopyPrintSettings') then Exit;
        
      CarbonPrinter.UpdatePrinter;
    end;
  finally
    PMRelease(PMObject(DialogSettings));
  end;
end;

