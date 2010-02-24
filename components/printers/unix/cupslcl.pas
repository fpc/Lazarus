unit cupslcl;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, StdCtrls, OsPrinters, Printers, CupsDyn;

const
  C_SPACE       = 6;
  C_GROUPSPACE  = 4*C_SPACE;
  C_BOTHSPACES  = 2*C_SPACE;

  procedure SetupCupsCombo(Combo: TComboBox; Option:Pppd_option_t;
                           OptionStr:String='');
  procedure CheckCupsComboChanges(Combo: TComboBox);
  function  GetCupsComboKeyValue(Combo: TComboBox; AIndex:Integer=-1): string;

implementation

Type
  THackCUPSPrinter=Class(TCUPSPrinter);

procedure SetupCupsCombo(Combo: TCombobox; Option: Pppd_option_t;
  OptionStr: String);
var
  c,j: Integer;
  choice: Pppd_choice_t;
  St: string;
begin

  combo.Tag:=-1;
  combo.Enabled:=false;

  if THackCUPSPrinter(Printer).CupsPPD=nil then
    exit;
  if Option=nil then
  begin
    if OptionStr='' then
      exit;
    Option := ppdFindOption(THackCUPSPrinter(Printer).CupsPPD,pchar(OptionStr));
    if Option=nil then
      exit;
  end;

  c := 0;
  Choice := Option^.choices;
  St := THackCUPSPrinter(Printer).cupsGetOption(Option^.keyword);
  {$IFDEF DebugCUPS}
  DbgOut('Combo: Keyword="%s" Default="%s" CurValue="%s"',
                                        [Option^.keyword,Option^.defchoice,St]);
  {$ENDIF}
  if St='' then
    St := Option^.defchoice;
  while (Choice<>nil) and (c<Option^.num_choices) do
  begin
    j := combo.items.AddObject(choice^.Text, TObject(Choice));
    if strcomp(choice^.choice, pchar(St))=0 then
      combo.Tag := j;
    inc(Choice);
    inc(c);
  end;
  combo.ItemIndex := combo.Tag;
  combo.Enabled:=combo.Tag<>-1;
  {$IFDEF DebugCUPS}
  DebugLn(' SelValue="%s" ItemIndex=%d',[St,combo.ItemIndex]);
  {$ENDIF}
end;

procedure CheckCupsComboChanges(Combo: TCombobox);
var
  choice   : Pppd_choice_t;
begin
  if Combo.Enabled and (Combo.ItemIndex<>Combo.Tag) then
  begin
    Choice := Pppd_choice_t(Combo.Items.Objects[Combo.ItemIndex]);
    if (Choice<>nil) and (Choice^.option<>nil) then
      THackCUPSPrinter(Printer).cupsAddOption(
                    Pppd_option_t(Choice^.option)^.keyword, Choice^.choice);
  end;
end;

function GetCupsComboKeyValue(Combo: TComboBox; AIndex:Integer=-1): string;
var
  choice   : Pppd_choice_t;
  option   : Pppd_option_t;
begin
  result := '';

  if AIndex<0 then
    AIndex := Combo.ItemIndex;

  if Combo.Enabled then
  begin
    if AIndex>=0 then
    begin
      Choice := Pppd_choice_t(Combo.Items.Objects[AIndex]);
      if Choice<>nil then
        result := Choice^.choice;
    end else
    begin
      Choice := Pppd_choice_t(Combo.Items.Objects[0]);
      Option := Pppd_option_t(Choice^.option);
      if Option<>nil then
        result := Option^.defchoice;
    end;
  end;
end;

end.

