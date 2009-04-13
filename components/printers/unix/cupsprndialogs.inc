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

function GetNumber(FromLeft: boolean; s:ansistring; aDefault: Integer): Integer;
var
  i: Integer;
  res: string;
begin

  if FromLeft then
    i := 1
  else
    i := Length(s);
  
  Res := '';
  while (i>0)and(i<=Length(s)) do
  begin
    if s[i] in ['0'..'9'] then begin
      if Fromleft then
        Res:=Res+S[i]
      else
        Res:=S[i]+Res;
    end else begin
      if Res<>'' then
        break;
    end;
    if FromLeft then
      Inc(i)
    else
      Dec(i);
  end;

  Result := StrToIntDef(Res, aDefault);
end;

{ TPrintDialog }

function TPrintDialog.Execute: Boolean;
Var
  Dlg : TdlgSelectPrinter;
  i   : Integer;
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
    if Result then begin
      // TDlgSelectPrinter will setup directoy cups printer options
      // yet, TPrintDialog should return information about user choice
      // modifying fields accordingly.
      
      // Page range. This migth get really complex because it's free enty
      //             textbox. To fill FromPage and ToPage we will use some
      //             simple rules.
      i := pos('-', Dlg.edRange.Text);
      if i<>0 then begin
        FromPage := GetNumber(False, copy(Dlg.edRange.Text, 1, i-1), FromPage);
        ToPage   := GetNumber(True,  copy(Dlg.edRange.Text, i+1, 255), ToPage);
        if ToPage<FromPage then begin
          i := ToPage;
          ToPage := FromPage;
          FromPage := i;
        end;
      end else begin
        Self.FromPage := GetNumber(True,  copy(Dlg.edRange.Text, i+1, 255), Self.FromPage);
        Self.ToPage   := Self.FromPage;
      end;
    end;
  finally
    Dlg.Free;
  end;
end;


{ TPrintDialog }

function TPageSetupDialog.Execute: Boolean;
var
  Dlg: TDlgPageSetup;
begin
  Dlg:=TdlgPageSetup.Create(nil);
  try
    Result:=(Dlg.ShowModal=mrOk);
    if Result then begin
    end;
  finally
    Dlg.Free;
  end;
end;



