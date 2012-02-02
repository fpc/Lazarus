{%MainUnit ../printersdlgs.pp}


{ TPageSetupDialog }

function TPageSetupDialog.Execute: Boolean;
var
  PgDlg: QPageSetupDialogH;
begin
  Result := False;
  if not Assigned(Printer) then Exit;
  if Printer.Printers.Count <= 0 then Exit;
  PgDlg := QPageSetupDialog_create(QtDefaultPrinter.Handle, nil);
  try
    Result := QPageSetupDialog_exec(PgDlg) = Ord(QDialogAccepted);
  finally
    QPageSetupDialog_destroy(PgDlg);
  end;
end;

{ TPrinterSetupDialog }

function TPrinterSetupDialog.Execute: Boolean;
var
  PgDlg: QPageSetupDialogH;
begin
  Result := False;
  if not Assigned(Printer) then Exit;
  if Printer.Printers.Count <= 0 then Exit;
  {There's no special dialog for printer setup under Qt,
   so we'll use QPageSetupDialog here.}
  PgDlg := QPageSetupDialog_create(QtDefaultPrinter.Handle, nil);
  try
    Result := QPageSetupDialog_exec(PgDlg) = Ord(QDialogAccepted);
  finally
    QPageSetupDialog_destroy(PgDlg);
  end;
end;

{ TPrintDialog }

function TPrintDialog.Execute: Boolean;
var
  QtPrnDlg: QPrintDialogH;
  PrnOptions: QAbstractPrintDialogPrintDialogOptions;
  Str: WideString;
  S: String;
begin
  Result := False;

  if not Assigned(Printer) then Exit;
  if Printer.Printers.Count <= 0 then Exit;

  QtPrnDlg := QPrintDialog_create(QtDefaultPrinter.Handle, nil);
  try

    if Title <> '' then
      Str := UTF8Decode(Title)
    else
      Str := UTF8Decode(DefaultTitle);

    QWidget_setWindowTitle(QtPrnDlg, @Str);

    if (Width > 0) and (Height > 0) then
      QWidget_setBaseSize(QtPrnDlg, Width, Height);

    {By default, full page printing is disabled. In this case, the origin of
     the QPrinter's coordinate system coincides with the top-left corner
     of the printable area. If full page printing is enabled, the origin of
     the QPrinter's coordinate system coincides with the top-left corner of
     the paper itself. In this case, the device metrics will report the
     exact same dimensions as indicated by PageSize. It may not be
     possible to print on the entire physical page because of the printer's
     margins, so the application must account for the margins itself.
     We can set this property from QtLCL OsPrinters too.
     QtDefaultPrinter.FullPage := True;
     THIS IS FIXED IN Qt-4.4, so PageRect returns correct dimensions !}

    QAbstractPrintDialog_setMinMax(QtPrnDlg, MinPage, MaxPage);
    QAbstractPrintDialog_setFromTo(QtPrnDlg, FromPage, ToPage);

    PrnOptions := QAbstractPrintDialogPrintCollateCopies
      or QAbstractPrintDialogPrintShowPageSize;

    if (poPrintToFile in Options) then
      PrnOptions := PrnOptions or QAbstractPrintDialogPrintToFile;

    if (poSelection in Options) then
      PrnOptions := PrnOptions or QAbstractPrintDialogPrintSelection;

    if (poPageNums) in Options then
      PrnOptions := PrnOptions or QAbstractPrintDialogPrintPageRange;

    {this function does not have effect on Darwin}
    QAbstractPrintDialog_setEnabledOptions(QtPrnDlg, PrnOptions);

    QtDefaultPrinter.numCopies := Copies;

    if PrintToFile then
      QtDefaultPrinter.OutputFormat := QPrinterPdfFormat;

    Result := QPrintDialog_exec(QtPrnDlg) = Ord(QDialogAccepted);

    if Result then
    begin
      S := QtDefaultPrinter.PrinterName;
      if Printer.Printers.IndexOf(S) <> -1 then
        Printer.SetPrinter(S);
      Collate := QtDefaultPrinter.Collate;
      MinPage := QAbstractPrintDialog_minPage(QtPrnDlg);
      MaxPage := QAbstractPrintDialog_maxPage(QtPrnDlg);
      FromPage := QtDefaultPrinter.fromPage;
      ToPage := QtDefaultPrinter.toPage;
      PrintToFile := QtDefaultPrinter.OutputFormat <> QPrinterNativeFormat;
      Copies := QtDefaultPrinter.numCopies;

      case QtDefaultPrinter.PrintRange of
        QPrinterAllPages: PrintRange := prAllPages;
        QPrinterSelection: PrintRange := prSelection;
        QPrinterPageRange: PrintRange := prPageNums;
        else
          PrintRange := prCurrentPage;
      end;
    end;
  finally
    QPrintDialog_destroy(QtPrnDlg);
  end;
end;

