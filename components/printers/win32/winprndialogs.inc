{%MainUnit ../printersdlgs.pp}

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

function PrintHookProc(hdlg: HWND; uiMsg: UINT; wParam: WPARAM; lParam: LPARAM): UINT_PTR; stdcall;
var
  lpp: PtagPD;
begin
  if uiMsg = WM_INITDIALOG then
  begin
    lpp := PtagPD(lParam);
    if (lParam<>0) and (lpp^.lCustData<>0) then
      if UseUnicode then
        SetWindowTextW(hdlg, pwidechar(lpp^.lCustData))
      else
        SetWindowText(hdlg,pChar(lpp^.lCustData));
    Reposition(hdlg);
  end;
  Result := 0;
end;

function PageSetupHookProc(hdlg: HWND; uiMsg: UINT; wParam: WPARAM; lParam: LPARAM): PtrUInt; stdcall;
var
 lpp : PtagPSD;
begin
  if uiMsg = WM_INITDIALOG then
  begin
    lpp := PtagPSD(lParam);
    if (lParam<>0) and (lpp^.lCustData<>0) then
      if UseUnicode then
        SetWindowTextW(hdlg, pwidechar(lpp^.lCustData))
      else
        SetWindowText(hdlg,pChar(lpp^.lCustData));
    Reposition(hdlg);
  end;
  Result := 0;
end;


{ TPageSetupDialog }

function TPageSetupDialog.Execute: Boolean;
var
  lpp        : tagPSD;
  PDev       : TPrinterDevice;
  DeviceMode : THandle;
  DevNames   : PDevNames;
  DevModeW   : PDeviceModeW;
  StW        : PWidechar;
  DevModeA   : PDeviceMode;
  StA        : PChar;
  BoolRes: BOOL;
begin
  Result := False;
  if not Assigned(Printer) then Exit;
  if Printer.Printers.Count > 0 then
  begin
    FillChar(lpp, SizeOf(lpp), 0);
    with lpp do
    begin
      lStructSize := SizeOf(lpp);
      hInstance := System.HInstance;
      lpfnPageSetupHook := @PageSetupHookProc;
      if Title<>'' then
      begin
        if UseUnicode then
          lCustData := LPARAM(pWideChar(UTF8Decode(Title)))
        else
          lCustData := LPARAM(pChar(Utf8ToAnsi(Title)))
      end
      else
        lCustData := 0;
      Flags := PSD_MARGINS or PSD_ENABLEPAGESETUPHOOK;
      hWndOwner := Widgetset.AppHandle;
      rtMargin := fMargins;
      PDev := TPrinterDevice(Printer.Printers.Objects[Printer.PrinterIndex]);
      // Pdev.DevMode has the required size, just copy to the global memory
      DeviceMode := GLobalAlloc(GHND, PDev.DevModeSize);
      try
        if UseUnicode then
          DevModeW := PDeviceModeW(GlobalLock(DeviceMode))
        else
          DevModeA := PDeviceModeA(GlobalLock(DeviceMode));
        try
          if UseUnicode then
            CopyMemory(DevModeW, PDev.DevModeW, Pdev.DevModeSize)
          else
            CopyMemory(DevModeA, PDev.DevModeA, Pdev.DevModeSize);
        finally
          GlobalUnlock(DeviceMode);
        end;
        hDevMode := DeviceMode;
        if UseUnicode then
          BoolRes := PageSetupDlgW(@Lpp)
        else
          BoolRes := PageSetupDlg(@Lpp);
        if BoolRes then
        begin
          if UseUnicode then StW := '' else StA := '';
          if Lpp.HdevNames <> 0 then
          begin
            DevNames := PDevNames(GlobalLock(lpp.hDevNames));
            try
              if UseUnicode then
              begin
                StW := PWidechar(DEVNames) + DevNames^.wDeviceOffset;
                Printer.SetPrinter(UTF8Encode(widestring(StW)));
              end
              else
              begin
                StA := PChar(DevNames) + DevNames^.wDeviceOffset;
                Printer.SetPrinter(StA);
              end
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

          if lpp.hDevMode <> 0 then
          begin
            if UseUnicode then
              DevModeW := PDeviceModeW(GlobalLock(lpp.hDevMode))
            else
              DevModeA := PDeviceModeA(GlobalLock(lpp.hDevMode));
            try
              //Set the properties for the selected printer
              PDev := TPrinterDevice(Printer.Printers.Objects[Printer.PrinterIndex]);
              if UseUnicode then
                CopyMemory(PDev.DevModeW, DevModeW, PDev.DevModeSize)
              else
                CopyMemory(PDev.DevModeA, DevModeA, PDev.DevModeSize);
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
  DeviceMode : THandle;
  DevNames   : PDevNames;
  DevModeW   : PDeviceModeW;
  StW        : PWidechar;
  DevModeA   : PDeviceMode;
  StA        : PChar;
  BoolRes: BOOL;
begin
  Result:=False;
  if not Assigned(Printer) then Exit;
  if Printer.Printers.Count>0 then
  begin
    FillChar(lpp, SizeOf(lpp), 0);
    with lpp do
    begin
      lStructSize := SizeOf(lpp);
      hInstance := System.HInstance;
      lpfnSetupHook := @PrintHookProc;
      if Title<>'' then
        if UseUnicode then
          lCustData := LPARAM(pWideChar(UTF8Decode(Title)))
        else
          lCustData := LPARAM(pChar(Utf8ToAnsi(Title)))
      else
        lCustData := 0;
      Flags := PD_PRINTSETUP or PD_RETURNDC or PD_ENABLESETUPHOOK;
      hWndOwner := Widgetset.AppHandle;
      PDev := TPrinterDevice(Printer.Printers.Objects[Printer.PrinterIndex]);
      // Pdev.DevMode has the required size, just copy to the global memory
      DeviceMode := GlobalAlloc(GHND, PDev.DevModeSize);
      try
        if UseUnicode then
          DevModeW := PDeviceModeW(GlobalLock(DeviceMode))
        else
          DevModeA := PDeviceModeA(GlobalLock(DeviceMode));
        try
          if useUnicode then
            CopyMemory(DevModeW, Pdev.DevModeW, Pdev.DevModeSize)
          else
            CopyMemory(DevModeA, Pdev.DevModeA, Pdev.DevModeSize);
        finally
          GlobalUnlock(DeviceMode);
        end;
        hDevMode := DeviceMode;
        if UseUnicode then
          BoolRes := PrintDlgW(@lpp)
        else
          BoolRes := PrintDlg(@lpp);
        if BoolRes then
        begin
          if UseUnicode then StW := '' else StA := '';
          //Change Selected printer
          if lpp.hDevNames <> 0 then
          begin
            DevNames := PDevNames(GlobalLock(lpp.hDevNames));
            try
              if UseUnicode then
              begin
                StW := PWidechar(DevNames) + DevNames^.wDeviceOffset;
                Printer.SetPrinter(UTF8Encode(widestring(StW)));
              end
              else
              begin
                StA := PChar(DevNames) + DevNames^.wDeviceOffset;
                Printer.SetPrinter(StA);
              end;
            finally
              GlobalUnlock(lpp.hDevNames);
              GlobalFree(lpp.hDevNames);
            end;
          end;

          Result := True;

          if lpp.hDevMode <> 0 then
          begin
            if UseUnicode then
              DevModeW := PDeviceModeW(GlobalLock(lpp.hDevMode))
            else
              DevModeA := PDeviceMode(GlobalLock(lpp.hDevMode));
            try
              //Set the properties for the selected printer
              PDev := TPrinterDevice(Printer.Printers.Objects[Printer.PrinterIndex]);
              if UseUnicode then
                CopyMemory(PDev.DevModeW, DevModeW, PDev.DevModeSize)
              else
                CopyMemory(PDev.DevModeA, DevModeA, PDev.DevModeSize);
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
var
  lpp        : tagPD;
  PDev       : TPrinterDevice;
  DeviceMode : THandle;
  DevNames   : PDevNames;
  DevModeW   : PDeviceModeW;
  StW        : PWidechar;
  DevModeA   : PDeviceModeA;
  StA        : PChar;
  BoolRes: BOOL;
  Index: Integer;
begin
  Result := False;
  if not Assigned(Printer) then Exit;
  if Printer.Printers.Count > 0 then
  begin
    FillChar(lpp, SizeOf(lpp), 0);
    with lpp do
    begin
      lStructSize := SizeOf(lpp);
      hInstance := System.HInstance;
      lpfnPrintHook := @PrintHookProc;
      lpfnSetupHook := @PrintHookProc;
      if Title<>'' then
      begin
        if UseUnicode then
          lCustData := LPARAM(pWideChar(UTF8Decode(Title)))
        else
          lCustData := LPARAM(pChar(Utf8ToAnsi(Title)));
      end
      else
        lCustData := 0;
      Flags := PD_ENABLEPRINTHOOK or PD_ENABLESETUPHOOK;
      if not Printer.RawMode then
        Flags := Flags or PD_RETURNDC;
      if Collate then
        Flags := Flags or PD_COLLATE;
      case PrintRange of
        prPageNums: Flags := Flags or PD_PAGENUMS;
        prSelection: Flags := Flags or PD_SELECTION;
      end;

      if PrintToFile  then Flags := Flags or PD_PRINTTOFILE;
      if not (poPrintToFile in Options) then Flags :=  Flags or PD_HIDEPRINTTOFILE;
      if not (poPageNums in Options) then Flags := Flags or PD_NOPAGENUMS;
      if not (poSelection in Options) then Flags := Flags or PD_NOSELECTION;
      if (poPrintToFile in Options ) and (poDisablePrintToFile in Options) then Flags := Flags or PD_DISABLEPRINTTOFILE;
      if (poHelp in Options) then Flags := Flags or PD_SHOWHELP;
      if not (poWarning in Options) then Flags := Flags or PD_NOWARNING;
      
      hWndOwner := Widgetset.AppHandle;
      PDev := TPrinterDevice(Printer.Printers.Objects[Printer.PrinterIndex]);
      // Pdev.DevMode has the required size, just copy to the global memory
      DeviceMode := GlobalAlloc(GHND, PDEV.DevModeSize);
      try
        if UseUnicode then
          DevModeW := PDeviceModeW(GlobalLock(DeviceMode))
        else
          DevModeA := PDeviceModeA(GlobalLock(DeviceMode));
        try
          if UseUnicode then
            CopyMemory(DevModeW, PDev.DevModeW, PDev.DevModeSize)
          else
            CopyMemory(DevModeA, PDev.DevModeA, PDev.DevModeSize);
        finally
          GlobalUnlock(DeviceMode);
        end;
        
        hDevMode := DeviceMode;
        nCopies := Word(Copies);
        nFromPage := Word(FromPage);
        nToPage := Word(ToPage);
        nMinPage := Word(MinPage);
        nMaxPage := Word(MaxPage);
        if UseUnicode then
          DevModeW^.dmCopies := nCopies
        else
          DevModeA^.dmCopies := nCopies;
        if UseUnicode then
          BoolRes := PrintDlgW(@lpp)
        else
          BoolRes := PrintDlg(@lpp);
        if BoolRes then
        begin
          if UseUnicode then StW := '' else StA:='';
          //Change Selected printer
          if lpp.hDevNames <> 0 then
          begin
            DevNames := PDevNames(GlobalLock(lpp.hDevNames));
            try
              if UseUnicode then
              begin
                StW := PWidechar(DevNames) + DevNames^.wDeviceOffset;
                Printer.SetPrinter(UTF8Encode(widestring(stW)));
              end
              else
              begin
                StA := PChar(DevNames) + DevNames^.wDeviceOffset;
                Printer.SetPrinter(StA);
              end;
            finally
              GlobalUnlock(lpp.hDevNames);
              GlobalFree(lpp.hDevNames);
            end;
          end;

          Result:=True;
          
          // printer might have changed, check if new printer
          // support extended device modes
          PDev:=TPrinterDevice(Printer.Printers.Objects[Printer.PrinterIndex]);

          if (lpp.hDevMode<>0) and ( (UseUnicode and (Pdev.DevModeW<>nil)) or
             ((not UseUnicode) and (Pdev.DevModeA<>nil)))then

          begin
            if UseUnicode then
              DevModeW := PDeviceModeW(GlobalLock(lpp.hDevMode))
            else
              DevModeA := PDeviceModeA(GlobalLock(lpp.hDevMode));
            try
              if UseUnicode then
                CopyMemory(PDev.DevModeW,DevModeW,PDev.DevModeSize)
              else
                CopyMemory(PDev.DevModeA,DevModeA,PDev.DevModeSize);
              if UseUnicode then
                Index := Printer.PaperSize.SupportedPapers.IndexOfObject(TObject(ptrint(DevModeW^.dmPaperSize)))
              else
                Index := Printer.PaperSize.SupportedPapers.IndexOfObject(TObject(ptrint(DevModeA^.dmPaperSize)));
              if Index <> -1 then
              begin
                if UseUnicode then
                  PDev.DevModeW^.dmPaperSize := DevModeW^.dmPaperSize
                else
                  PDev.DevModeA^.dmPaperSize := DevModeA^.dmPaperSize
              end
              else
              begin
                if Useunicode then
                  PDev.DevModeW^.dmPaperSize := PDev.DefaultPaper
                else
                  PDev.DevModeA^.dmPaperSize := PDev.DefaultPaper
              end;
              if nCopies=1 then
              begin
                if UseUnicode then
                  Copies := DevModeW^.dmCopies
                else
                  Copies := DevModeA^.dmCopies
              end
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

