Uses
 Windows,WinUtilPrn, InterfaceBase, Win32Int, LCLIntf,LCLType,WinVer,Printers,OSPrinters;


{ TPageSetupDialog }

function TPageSetupDialog.Execute: Boolean;
var
    lpp        : tagPSD;
    PDev       : TPrinterDevice;
    DevMode    : PDeviceMode;
    DeviceMode : THandle;
    DevNames   : PDevNames;
    St         : PChar;
begin
  Result:=False;
  if not Assigned(Printer) then Exit;
  if Printer.Printers.Count>0 then
  begin
    FillChar(lpp,SizeOf(lpp),0);
    with lpp do
    begin
      lStructSize:=SizeOf(lpp);
      hInstance:=LCLType.HInstance;
      Flags := PSD_MARGINS;
      hWndOwner:=TWin32WidgetSet(WidgetSet).AppHandle;
      rtMargin := fMargins;
      PDev:=TPrinterDevice(Printer.Printers.Objects[Printer.PrinterIndex]);

      DeviceMode:=GlobalAlloc(GHND,SizeOf(PDev.DevMode));
      try
      DevMode:=PDeviceMode(GlobalLock(DeviceMode));
      try
       //some plotters crash when DEVMODE contains other options set
       //so only minimal options passed to print dialog
       StrCopy(DevMode^.dmDeviceName,Pdev.DevMode.dmDeviceName);
       DevMode^.dmSize := Word(SizeOf(PDev.DevMode));
       DevMode^.dmOrientation := Pdev.DevMode.dmOrientation;
       DevMode^.dmFields := DM_ORIENTATION or DM_PAPERSIZE or DM_COPIES;
       DevMode^.dmPaperSize := Pdev.DevMode.dmPaperSize;
       DevMode^.dmCopies := Pdev.DevMode.dmCopies;
      finally
        GlobalUnLock(DeviceMode);
      end;

      hDevMode:= DeviceMode;
      if PageSetupDlg(lpp) then
        begin
          St:='';
          //Change Selected printer
          if lpp.hDevNames<>0 then
          begin
            DevNames:=PDevNames(GlobalLock(lpp.hDevNames));
            try
              St:=PChar(DevNames)+DevNames^.wDeviceOffset;
              Printer.SetPrinter(St);
            finally
              GlobalUnlock(lpp.hDevNames);
              GlobalFree(lpp.hDevNames);
            end;
          end;

          Result:=True;
          if (Flags and PSD_INHUNDREDTHSOFMILLIMETERS)>0 then fUnits := unMM
          else
          fUnits := unInch;
          fMargins := rtMargin;

          if lpp.hDevMode<>0 then
          begin
            DevMode:=PDeviceMode(GlobalLock(lpp.hDevMode));
            try
              //Set the properties for the selected printer
              PDev:=TPrinterDevice(Printer.Printers.Objects[Printer.PrinterIndex]);
              PDev.DevMode.dmSize := SizeOf(PDev.DevMode);
              StrCopy(@PDev.DevMode.dmDeviceName,DevMode^.dmDeviceName);
              PDev.DevMode.dmOrientation := DevMode^.dmOrientation;
              PDev.DevMode.dmFields := DevMode^.dmFields;
              PDev.DevMode.dmPaperSize := DevMode^.dmPaperSize;
              PDev.DevMode.dmPaperLength := DevMode^.dmPaperLength;
              PDev.DevMode.dmPaperWidth := DevMode^.dmPaperWidth;
              PDev.DevMode.dmScale := DevMode^.dmScale;
              PDev.DevMode.dmCopies := DevMode^.dmCopies;
              PDev.DevMode.dmPrintQuality := DevMode^.dmPrintQuality;
              PDev.DevMode.dmColor := DevMode^.dmColor;
              PDev.DevMode.dmDuplex := DevMode^.dmDuplex;
              PDev.DevMode.dmYResolution := DevMode^.dmYResolution;
              PDev.DevMode.dmTTOption := DevMode^.dmTTOption;
              PDev.DefaultPaper := DevMode^.dmPaperSize;
            finally
              GlobalUnlock(lpp.hDevMode);
           end;
          end;
        end;
      finally
        GlobalFree(DeviceMode);
      end;
    end;
  end;
end;


{ TPrinterSetupDialog }

function TPrinterSetupDialog.Execute: Boolean;
var
    lpp        : tagPD;
    PDev       : TPrinterDevice;
    DevMode    : PDeviceMode;
    DeviceMode : THandle;
    DevNames   : PDevNames;
    St         : PChar;
begin
  Result:=False;
  if not Assigned(Printer) then Exit;
  if Printer.Printers.Count>0 then
  begin
    FillChar(lpp,SizeOf(lpp),0);
    with lpp do
    begin
      lStructSize:=SizeOf(lpp);
      hInstance:=LCLType.HInstance;
      Flags:= PD_PRINTSETUP or PD_RETURNDC;
      hWndOwner:=TWin32WidgetSet(WidgetSet).AppHandle;

      PDev:=TPrinterDevice(Printer.Printers.Objects[Printer.PrinterIndex]);

      DeviceMode:=GlobalAlloc(GHND,SizeOf(PDev.DevMode));
      try
      DevMode:=PDeviceMode(GlobalLock(DeviceMode));
      try
       //some plotters crash when DEVMODE contains other options set
       //so only minimal options passed to print dialog
       StrCopy(DevMode^.dmDeviceName,Pdev.DevMode.dmDeviceName);
       DevMode^.dmSize := Word(SizeOf(PDev.DevMode));
       DevMode^.dmOrientation := Pdev.DevMode.dmOrientation;
       DevMode^.dmFields := DM_ORIENTATION or DM_PAPERSIZE or DM_COPIES;
       DevMode^.dmPaperSize := Pdev.DevMode.dmPaperSize;
       DevMode^.dmCopies := Pdev.DevMode.dmCopies;
      finally
        GlobalUnLock(DeviceMode);
      end;

      hDevMode:= DeviceMode;
      if PrintDlg(lpp) then
        begin
          St:='';
          //Change Selected printer
          if lpp.hDevNames<>0 then
          begin
            DevNames:=PDevNames(GlobalLock(lpp.hDevNames));
            try
              St:=PChar(DevNames)+DevNames^.wDeviceOffset;
              Printer.SetPrinter(St);
            finally
              GlobalUnlock(lpp.hDevNames);
              GlobalFree(lpp.hDevNames);
            end;
          end;

          Result:=True;

          if lpp.hDevMode<>0 then
          begin
            DevMode:=PDeviceMode(GlobalLock(lpp.hDevMode));
            try
              //Set the properties for the selected printer
              PDev:=TPrinterDevice(Printer.Printers.Objects[Printer.PrinterIndex]);
              PDev.DevMode.dmSize := SizeOf(PDev.DevMode);
              StrCopy(@PDev.DevMode.dmDeviceName,DevMode^.dmDeviceName);
              PDev.DevMode.dmOrientation := DevMode^.dmOrientation;
              PDev.DevMode.dmFields := DevMode^.dmFields;
              PDev.DevMode.dmPaperSize := DevMode^.dmPaperSize;
              PDev.DevMode.dmPaperLength := DevMode^.dmPaperLength;
              PDev.DevMode.dmPaperWidth := DevMode^.dmPaperWidth;
              PDev.DevMode.dmScale := DevMode^.dmScale;
              PDev.DevMode.dmCopies := DevMode^.dmCopies;
              PDev.DevMode.dmPrintQuality := DevMode^.dmPrintQuality;
              PDev.DevMode.dmColor := DevMode^.dmColor;
              PDev.DevMode.dmDuplex := DevMode^.dmDuplex;
              PDev.DevMode.dmYResolution := DevMode^.dmYResolution;
              PDev.DevMode.dmTTOption := DevMode^.dmTTOption;
              PDev.DefaultPaper := DevMode^.dmPaperSize;
              TWinPrinter(Printer).Handle := hDC;
            finally
              GlobalUnlock(lpp.hDevMode);
           end;
          end;
        end;
      finally
        GlobalFree(DeviceMode);
      end;
    end;
  end;
end;


{ TPrintDialog }

function TPrintDialog.Execute: Boolean;
var lpp        : tagPD;
    PDev       : TPrinterDevice;
    DevMode    : PDeviceMode;
    DeviceMode : THandle;
    DevNames   : PDevNames;
    St         : PChar;
    Old   : Integer;
begin
  Result:=False;
  if not Assigned(Printer) then Exit;
  Old := Copies;
  if Printer.Printers.Count>0 then
  begin
    FillChar(lpp,SizeOf(lpp),0);
    with lpp do
    begin
      lStructSize:=SizeOf(lpp);
      hInstance:=LCLType.HInstance;
      Flags := PD_COLLATE or PD_USEDEVMODECOPIES or PD_RETURNDC;
      case PrintRange of
       prPageNums : Flags := Flags or PD_PAGENUMS;
       prSelection : Flags := Flags or PD_SELECTION;
      end;

      if PrintToFile  then Flags := Flags or PD_PRINTTOFILE;
      if not (poPrintToFile in Options) then Flags :=  Flags or PD_HIDEPRINTTOFILE;
      if not (poPageNums in Options) then Flags := Flags or PD_NOPAGENUMS;
      if not (poSelection in Options) then Flags := Flags or PD_NOSELECTION;
      if (poPrintToFile in Options ) and (poDisablePrintToFile in Options) then Flags := Flags or PD_DISABLEPRINTTOFILE;

      hWndOwner:=TWin32WidgetSet(WidgetSet).AppHandle;

      PDev:=TPrinterDevice(Printer.Printers.Objects[Printer.PrinterIndex]);

      DeviceMode:=GlobalAlloc(GHND,SizeOf(PDev.DevMode));
      try
      DevMode:=PDeviceMode(GlobalLock(DeviceMode));
      try
       //some plotters crash when DEVMODE contains other options set
       //so only minimal options passed to print dialog
       DevMode^.dmSize := Word(SizeOf(PDev.DevMode));
       StrCopy(DevMode^.dmDeviceName,Pdev.DevMode.dmDeviceName);
       DevMode^.dmOrientation := Pdev.DevMode.dmOrientation;
       DevMode^.dmFields := DM_ORIENTATION or DM_PAPERSIZE or DM_COPIES;
       DevMode^.dmPaperSize := Pdev.DevMode.dmPaperSize;
       DevMode^.dmCopies := Copies;
      finally
        GlobalUnLock(DeviceMode);
      end;

      hDevMode:= DeviceMode;
      nCopies:= Word(Copies);
      nFromPage := Word(FromPage);
      nToPage := Word(ToPage);
      nMinPage := Word(MinPage);
      nMaxPage := Word(MaxPage);
      if PrintDlg(lpp) then
        begin
          St:='';
          //Change Selected printer
          if lpp.hDevNames<>0 then
          begin
            DevNames:=PDevNames(GlobalLock(lpp.hDevNames));
            try
              St:=PChar(DevNames)+DevNames^.wDeviceOffset;
              Printer.SetPrinter(St);
            finally
              GlobalUnlock(lpp.hDevNames);
              GlobalFree(lpp.hDevNames);
            end;
          end;

          Result:=True;


          if lpp.hDevMode<>0 then
          begin
            DevMode:=PDeviceMode(GlobalLock(lpp.hDevMode));
            try
              //Set the properties for the selected printer
              PDev:=TPrinterDevice(Printer.Printers.Objects[Printer.PrinterIndex]);
              PDev.DevMode.dmSize := SizeOf(PDev.DevMode);
              StrCopy(@PDev.DevMode.dmDeviceName,DevMode^.dmDeviceName);
              PDev.DevMode.dmOrientation := DevMode^.dmOrientation;
              PDev.DevMode.dmFields := DevMode^.dmFields;
              PDev.DevMode.dmPaperSize := DevMode^.dmPaperSize;
              PDev.DevMode.dmPaperLength := DevMode^.dmPaperLength;
              PDev.DevMode.dmPaperWidth := DevMode^.dmPaperWidth;
              PDev.DevMode.dmScale := DevMode^.dmScale;
              PDev.DevMode.dmCopies := DevMode^.dmCopies;
              PDev.DevMode.dmPrintQuality := DevMode^.dmPrintQuality;
              PDev.DevMode.dmColor := DevMode^.dmColor;
              PDev.DevMode.dmDuplex := DevMode^.dmDuplex;
              PDev.DevMode.dmYResolution := DevMode^.dmYResolution;
              PDev.DevMode.dmTTOption := DevMode^.dmTTOption;
              PDev.DefaultPaper := DevMode^.dmPaperSize;
              if DevMode^.dmCopies>1 then Copies := 1
              else
              Copies := Old;
              TWinPrinter(Printer).Handle := hDC;
            finally
              GlobalUnlock(lpp.hDevMode);
           end;
          end;
          
          PrintRange := prAllPages;
          PrintToFile := false;
          if (Flags and PD_SELECTION)>0 then PrintRange := prSelection;
          if (Flags and PD_PAGENUMS)>0 then PrintRange := prPageNums;
          if (Flags and PD_PRINTTOFILE)>0 then PrintToFile := true;
          FromPage := Integer(nFromPage);
          ToPage := Integer(nToPage);
          MinPage := Integer(nMinPage);
          MaxPage := Integer(nMaxPage);
        end;
      finally
        GlobalFree(DeviceMode);
      end;
    end;
  end;
end;

