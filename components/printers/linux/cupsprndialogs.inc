uses Controls, udlgSelectPrinter,udlgpropertiesprinter, FileUtil;

{ TPrinterSetupDialog }


function TPrinterSetupDialog.Execute: Boolean;
Var Dlg : Tdlgpropertiesprinter;
begin
  Dlg:=TdlgPropertiesPrinter.Create(nil);
  try
    Result:=(Dlg.ShowModal=mrOk);
    if Result then
        Dlg.InitProperties;
  finally
    Dlg.Free;
  end;
end;


{ TPrintDialog }

function TPrintDialog.Execute: Boolean;
Var Dlg : TdlgSelectPrinter;
begin
  Dlg:=TdlgSelectPrinter.Create(nil);
  Dlg.Options := Self.Options;
  Dlg.PrintRange := Self.PrintRange;
  Dlg.cbCollate.Checked := Self.Collate;
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
  finally
    Dlg.Free;
  end;
end;


{ TPrintDialog }

function TPageSetupDialog.Execute: Boolean;
begin
  Result := true;
end;



