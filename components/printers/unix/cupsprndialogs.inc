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
  NDigits: integer;
begin
  Dlg:= TDlgPageSetup.Create(nil);
  try
    if Title<>'' then
      Dlg.Caption:= Title
    else
      Dlg.Caption:= DefaultTitle;

    if FUnits=pmInches then
    begin
      with Dlg.frmPageSetup.gpMargins do
        Caption:= Caption+' (inches)';
      NDigits:= 3;
    end
    else
    begin
      with Dlg.frmPageSetup.gpMargins do
        Caption:= Caption+' (mm)';
      NDigits:= 0;
    end;

    Dlg.frmPageSetup.txtLeft.DecimalPlaces:= NDigits;
    Dlg.frmPageSetup.txtTop.DecimalPlaces:= NDigits;
    Dlg.frmPageSetup.txtRight.DecimalPlaces:= NDigits;
    Dlg.frmPageSetup.txtBottom.DecimalPlaces:= NDigits;

    // Delphi wiki: MarginNNN holds value in 1/1000 parts of current Unit
    Dlg.frmPageSetup.txtLeft.Value:= FMarginLeft/1000;
    Dlg.frmPageSetup.txtTop.Value:= FMarginTop/1000;
    Dlg.frmPageSetup.txtRight.Value:= FMarginRight/1000;
    Dlg.frmPageSetup.txtBottom.Value:= FMarginBottom/1000;

    Result:= Dlg.ShowModal=mrOk;
    if Result then
    begin
      FMarginLeft:= Round(Dlg.frmPageSetup.txtLeft.Value*1000);
      FMarginTop:= Round(Dlg.frmPageSetup.txtTop.Value*1000);
      FMarginRight:= Round(Dlg.frmPageSetup.txtRight.Value*1000);
      FMarginBottom:= Round(Dlg.frmPageSetup.txtBottom.Value*1000);
    end;
  finally
    Dlg.Free;
  end;
end;


