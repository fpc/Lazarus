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
  U, V: UInt32;
  B: Boolean;
  PMin, PMax, PFrom, PTo: Integer;
begin
  Result := False;
  // TODO: Options, Title

  if not Assigned(Printer) then Exit;

  CarbonPrinter := Printer as TCarbonPrinter;
  //DebugLn('TPrintDialog.Execute ' + CarbonPrinter.CurrentPrinterName);
  
  if OSError(PMCreatePrintSettings(DialogSettings),
    Self, SExecute, 'PMCreatePrintSettings') then Exit;
  try
    if OSError(PMCopyPrintSettings(CarbonPrinter.PrintSettings, DialogSettings),
      Self, SExecute, 'PMCopyPrintSettings') then Exit;

    OSError(PMSetCollate(DialogSettings, Collate), Self, SExecute, 'PMSetCollate');
    OSError(PMSetCopies(DialogSettings, Copies, False), Self, SExecute, 'PMSetCopies');
    
    PMin := MinPage;
    PMax := Max(PMin, MaxPage);
    PFrom := Min(Max(FromPage, PMin), PMax);
    PTo := Max(PFrom, Min(ToPage, PMax));

    OSError(PMSetPageRange(DialogSettings, PMin, PMax),
        Self, SExecute, 'PMSetPageRange');
    if PrintRange <> prAllPages then
    begin
      OSError(PMSetFirstPage(DialogSettings, PFrom, False), Self, SExecute, 'PMSetFirstPage');
      OSError(PMSetLastPage(DialogSettings, PTo, False), Self, SExecute, 'PMSetLastPage');
    end;
    
    if OSError(PMSessionPrintDialog(CarbonPrinter.PrintSession, DialogSettings, CarbonPrinter.PageFormat, Result),
      Self, SExecute, 'PMSessionPrintDialog') then Exit;

    if Result then
    begin
      B := Collate;
      OSError(PMGetCollate(DialogSettings, B), Self, SExecute, 'PMGetCollate');
      Collate := B;
      
      V := Copies;
      OSError(PMGetCopies(DialogSettings, V), Self, SExecute, 'PMGetCopies');
      Copies := V;
      
      OSError(PMGetLastPage(DialogSettings, V), Self, SExecute, 'PMGetLastPage');
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
        OSError(PMGetFirstPage(DialogSettings, V), Self, SExecute, 'PMGetFirstPage');
        FromPage := V;
      end;
    
      if OSError(PMCopyPrintSettings(DialogSettings, CarbonPrinter.PrintSettings),
        Self, SExecute, 'PMCopyPrintSettings') then Exit;
        
      CarbonPrinter.UpdatePrinter;
    end;
  finally
    PMRelease(PMObject(DialogSettings));
  end;
end;

