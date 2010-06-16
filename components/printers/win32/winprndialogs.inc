{%MainUnit ../printersdlgs.pp}

function PrintHookProc(hdlg: HWND; uiMsg: UINT; wParam: WPARAM; lParam: LPARAM): PtrUInt; stdcall;

  // update from win32wsdialogs.pp
  procedure Reposition(ADialogWnd: Handle);
  var
    Left, Top: Integer;
    ABounds, DialogRect: TRect;
  begin
    // Btw, setting width and height of dialog doesnot reposition child controls :(
    // So no way to set another height and width at least here

    if (GetParent(ADialogWnd) = Widgetset.AppHandle) then
    begin
      if Screen.ActiveCustomForm <> nil then
        ABounds := Screen.ActiveCustomForm.Monitor.BoundsRect
      else
      if Application.MainForm <> nil then
        ABounds := Application.MainForm.Monitor.BoundsRect
      else
        ABounds := Screen.PrimaryMonitor.BoundsRect;
    end
    else
      ABounds := Screen.MonitorFromWindow(GetParent(ADialogWnd)).BoundsRect;
    GetWindowRect(ADialogWnd, DialogRect);
    Left := (ABounds.Right - DialogRect.Right + DialogRect.Left) div 2;
    Top := (ABounds.Bottom - DialogRect.Bottom + DialogRect.Top) div 2;
    SetWindowPos(ADialogWnd, HWND_TOP, Left, Top, 0, 0, SWP_NOSIZE);
  end;

begin
  if uiMsg = WM_INITDIALOG then
    Reposition(hdlg);
  Result := 0;
end;

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
    with lpp do begin
      lStructSize:=SizeOf(lpp);
      hInstance:=System.HInstance;
      lpfnPageSetupHook := @PrintHookProc;
      Flags := PSD_MARGINS or PSD_ENABLEPAGESETUPHOOK;
      hWndOwner:=Widgetset.AppHandle;
      rtMargin := fMargins;
      PDev:=TPrinterDevice(Printer.Printers.Objects[Printer.PrinterIndex]);
      // Pdev.DevMode has the required size, just copy to the global memory
      DeviceMode:=GLobalAlloc(GHND, PDev.DevModeSize);
      try
        DevMode:=PDeviceMode(GlobalLock(DeviceMode));
        try
          CopyMemory(DevMode, PDev.DevMode, Pdev.DevModeSize);
        finally
          GlobalUnlock(DeviceMode);
        end;
        hDevMode:=DeviceMode;
        if PageSetupDlg(Lpp) then begin
          St :='';
          if Lpp.HdevNames<>0 then begin
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
          if (Flags and PSD_INHUNDREDTHSOFMILLIMETERS)>0 then
            fUnits := unMM
          else
            fUnits := unInch;
          fMargins := rtMargin;

          if lpp.hDevMode<>0 then
          begin
            DevMode:=PDeviceMode(GlobalLock(lpp.hDevMode));
            try
              //Set the properties for the selected printer
              PDev:=TPrinterDevice(Printer.Printers.Objects[Printer.PrinterIndex]);
              CopyMemory(PDev.DevMode,DevMode,PDev.DevModeSize);
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
      hInstance:=System.HInstance;
      lpfnSetupHook := @PrintHookProc;
      Flags:= PD_PRINTSETUP or PD_RETURNDC or PD_ENABLESETUPHOOK;
      hWndOwner:=Widgetset.AppHandle;
      PDev:=TPrinterDevice(Printer.Printers.Objects[Printer.PrinterIndex]);
      // Pdev.DevMode has the required size, just copy to the global memory
      DeviceMode:=GlobalAlloc(GHND, PDev.DevModeSize);
      try
        DevMode:=PDeviceMode(GlobalLock(DeviceMode));
        try
          CopyMemory(DevMode, Pdev.DevMode, Pdev.DevModeSize);
        finally
          GlobalUnlock(DeviceMode);
        end;
        hDevMode := DeviceMode;
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
              CopyMemory(PDev.DevMode,DevMode,PDev.DevModeSize);
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
begin
  Result:=False;
  if not Assigned(Printer) then Exit;
  if Printer.Printers.Count>0 then
  begin
    FillChar(lpp,SizeOf(lpp),0);
    with lpp do
    begin
      lStructSize:=SizeOf(lpp);
      hInstance:=System.HInstance;
      lpfnPrintHook := @PrintHookProc;
      lpfnSetupHook := @PrintHookProc;
      Flags := PD_ENABLEPRINTHOOK or PD_ENABLESETUPHOOK;
      if not Printer.RawMode then
        Flags := Flags or PD_RETURNDC;
      if Collate then Flags := Flags or PD_COLLATE;
      case PrintRange of
       prPageNums : Flags := Flags or PD_PAGENUMS;
       prSelection : Flags := Flags or PD_SELECTION;
      end;

      if PrintToFile  then Flags := Flags or PD_PRINTTOFILE;
      if not (poPrintToFile in Options) then Flags :=  Flags or PD_HIDEPRINTTOFILE;
      if not (poPageNums in Options) then Flags := Flags or PD_NOPAGENUMS;
      if not (poSelection in Options) then Flags := Flags or PD_NOSELECTION;
      if (poPrintToFile in Options ) and (poDisablePrintToFile in Options) then Flags := Flags or PD_DISABLEPRINTTOFILE;
      if (poHelp in Options) then Flags := Flags or PD_SHOWHELP;
      if not (poWarning in Options) then Flags := Flags or PD_NOWARNING;
      
      hWndOwner:=Widgetset.AppHandle;
      PDev:=TPrinterDevice(Printer.Printers.Objects[Printer.PrinterIndex]);
      // Pdev.DevMode has the required size, just copy to the global memory
      DeviceMode:=GlobalAlloc(GHND, PDEV.DevModeSize);
      try
        DevMode:=PDeviceMode(GlobalLock(DeviceMode));
        try
          CopyMemory(DevMode, PDev.DevMode, PDev.DevModeSize);
        finally
          GlobalUnlock(DeviceMode);
        end;
        
        hDevMode:= DeviceMode;
        nCopies:= Word(Copies);
        nFromPage := Word(FromPage);
        nToPage := Word(ToPage);
        nMinPage := Word(MinPage);
        nMaxPage := Word(MaxPage);
        DevMode^.dmCopies := nCopies;
        
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
          
          // printer might have changed, check if new printer
          // support extended device modes
          PDev:=TPrinterDevice(Printer.Printers.Objects[Printer.PrinterIndex]);
          if (lpp.hDevMode<>0) and (Pdev.DevMode<>nil) then
          begin
            DevMode:=PDeviceMode(GlobalLock(lpp.hDevMode));
            try
              CopyMemory(PDev.DevMode,DevMode,PDev.DevModeSize);
              
              if Printer.PaperSize.SupportedPapers.IndexOfObject(TObject(ptrint(DevMode^.dmPaperSize))) <> -1
               then
                PDev.DevMode^.dmPaperSize := DevMode^.dmPaperSize
              else
                PDev.DevMode^.dmPaperSize := PDev.DefaultPaper;
              if nCopies=1 then
                Copies := DevMode^.dmCopies
              else
                Copies := nCopies;
              Printer.Copies := Copies;
              
              if not Printer.RawMode then
                TWinPrinter(Printer).Handle := hDC;
                
            finally
              GlobalUnlock(lpp.hDevMode);
            end;
          end;
          
          PrintRange := prAllPages;
          PrintToFile := false;
          Collate := false;
          if (Flags and PD_SELECTION)>0 then PrintRange := prSelection;
          if (Flags and PD_PAGENUMS)>0 then PrintRange := prPageNums;
          if (Flags and PD_PRINTTOFILE)>0 then PrintToFile := true;
          if (Flags and PD_COLLATE)>0 then Collate := true;
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

