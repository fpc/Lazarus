{ TPrinterSetupDialog }


function TPrinterSetupDialog.DoExecute: Boolean;
Var Dlg : Tdlgpropertiesprinter;
begin
  Dlg:=TdlgPropertiesPrinter.Create(nil);
  try
    if (Title='') then
    begin
      if Printer<>nil then
        Dlg.Caption := Printer.PrinterName;
    end else
      Dlg.Caption := Title;
    Result:=(Dlg.ShowModal=mrOk);
    if Result then
      Dlg.InitProperties;
  finally
    Dlg.Free;
  end;
end;

{ TPrintDialog }

function TPrintDialog.DoExecute: Boolean;
Var
  Dlg : TdlgSelectPrinter;
  i   : Integer;
begin
  Dlg:=TdlgSelectPrinter.Create(nil);
  Dlg.Options := Self.Options;
  Dlg.PrintRange := Self.PrintRange;
  Dlg.cbCollate.Checked := Self.Collate;
  Dlg.edCopies.Value := Self.Copies;
  if Title<>'' then
    Dlg.Caption := Title
  else
    Dlg.Caption := DefaultTitle;
  if FromPage<1 then FromPage:=1;
  if ToPage<FromPage then ToPage:=FromPage;
  Dlg.edRange.Text := IntToStr(Self.FromPage) +'-'+ IntToStr(Self.ToPage);
  case Dlg.PrintRange of
    prAllPages: Dlg.rbAllPage.Checked := True;
    prSelection: Dlg.rbSelection.Checked := True;
    prPageNums: Dlg.rbRange.Checked := True;
    prCurrentPage: Dlg.rbCurrentPage.Checked := True;
  end;
  try
    Dlg.btnPreview.Visible:=False;
    Result:=(Dlg.ShowModal=mrOk);
    if Result then begin
      // TDlgSelectPrinter will setup directoy cups printer options
      // yet, TPrintDialog should return information about user choice
      // modifying fields accordingly.

      self.PrintRange:=dlg.PrintRange;
      self.Collate := dlg.cbCollate.Checked;
      self.Copies := dlg.edCopies.Value;

      // Page range. This migth get really complex because it's free enty
      //             textbox. To fill FromPage and ToPage we will use some
      //             simple rules.
      i := pos('-', Dlg.edRange.Text);
      if i<>0 then begin
        FromPage := StrToIntDef(Trim(Copy(Dlg.edRange.Text, 1, i-1)), FromPage);
        ToPage   := StrToIntDef(Trim(Copy(Dlg.edRange.Text, i+1, MaxInt)), ToPage);
        if ToPage<FromPage then begin
          i := ToPage;
          ToPage := FromPage;
          FromPage := i;
        end;
      end else begin
        Self.FromPage := StrToIntDef(Trim(Dlg.edRange.Text), Self.FromPage);
        Self.ToPage   := Self.FromPage;
      end;
    end;
  finally
    Dlg.Free;
  end;
end;


{ TPageSetupDialog }

function TPageSetupDialog.DoExecute: Boolean;
var
  Dlg: TDlgPageSetup;
  NDigits, NScale: integer;
  NInc: double;
begin
  Dlg:= TDlgPageSetup.Create(nil);
  try
    if Title<>'' then
      Dlg.Caption:= Title
    else
      Dlg.Caption:= DefaultTitle;

    if FUnits=pmInches then
    begin
      Dlg.frmPageSetup.UnitInches:= true;
      with Dlg.frmPageSetup.gpMargins do
        Caption:= Caption+' (inches)';
      NDigits:= 2;
      NInc:= 0.01;
      // according to MSDN, WinAPI margin rect needs values in 1/1000 of inches,
      // or in 1/100 on mm.
      // https://msdn.microsoft.com/en-us/library/windows/desktop/ms646842(v=vs.85).aspx
      NScale:= 1000;
    end
    else
    begin
      Dlg.frmPageSetup.UnitInches:= false;
      with Dlg.frmPageSetup.gpMargins do
        Caption:= Caption+' (mm)';
      NDigits:= 0;
      NInc:= 1.0;
      NScale:= 100;
    end;

    // after setting UnitInches
    Dlg.SetControls(
      not (psoDisablePagePainting in Options),
      not (psoDisableMargins in Options),
      not (psoDisablePaper in Options),
      not (psoDisableOrientation in Options)
      );

    Dlg.frmPageSetup.txtLeft.DecimalPlaces:= NDigits;
    Dlg.frmPageSetup.txtTop.DecimalPlaces:= NDigits;
    Dlg.frmPageSetup.txtRight.DecimalPlaces:= NDigits;
    Dlg.frmPageSetup.txtBottom.DecimalPlaces:= NDigits;

    if psoMargins in Options then
    begin
      Dlg.frmPageSetup.txtLeft.Value:= FMarginLeft/NScale;
      Dlg.frmPageSetup.txtTop.Value:= FMarginTop/NScale;
      Dlg.frmPageSetup.txtRight.Value:= FMarginRight/NScale;
      Dlg.frmPageSetup.txtBottom.Value:= FMarginBottom/NScale;
    end
    else
    begin
      // set margins 1 inch or 10 mm
      Dlg.frmPageSetup.txtLeft.Value:= 1000/NScale;
      Dlg.frmPageSetup.txtTop.Value:= 1000/NScale;
      Dlg.frmPageSetup.txtRight.Value:= 1000/NScale;
      Dlg.frmPageSetup.txtBottom.Value:= 1000/NScale;
    end;

    if psoDefaultMinMargins in Options then
    begin
      Dlg.frmPageSetup.SetDefaultMinMargins;
    end;

    if psoMinMargins in Options then
    begin
      Dlg.frmPageSetup.txtLeft.MinValue:= FMinMarginLeft/NScale;
      Dlg.frmPageSetup.txtTop.MinValue:= FMinMarginTop/NScale;
      Dlg.frmPageSetup.txtRight.MinValue:= FMinMarginRight/NScale;
      Dlg.frmPageSetup.txtBottom.MinValue:= FMinMarginBottom/NScale;
    end;

    Dlg.frmPageSetup.txtLeft.Increment:= NInc;
    Dlg.frmPageSetup.txtTop.Increment:= NInc;
    Dlg.frmPageSetup.txtRight.Increment:= NInc;
    Dlg.frmPageSetup.txtBottom.Increment:= NInc;

    Result:= Dlg.ShowModal=mrOk;
    if Result then
    begin
      FMarginLeft:= Round(Dlg.frmPageSetup.txtLeft.Value*NScale);
      FMarginTop:= Round(Dlg.frmPageSetup.txtTop.Value*NScale);
      FMarginRight:= Round(Dlg.frmPageSetup.txtRight.Value*NScale);
      FMarginBottom:= Round(Dlg.frmPageSetup.txtBottom.Value*NScale);

      FPageWidth:= Round(Dlg.frmPageSetup.CurPageWidth*NScale);
      FPageHeight:= Round(Dlg.frmPageSetup.CurPageHeight*NScale);
    end;
  finally
    Dlg.Free;
  end;
end;


