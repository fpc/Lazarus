unit IdeDebuggerValueFormatterSetup;

{$mode objfpc}{$H+}

interface

uses
  LazIDEIntf, IdeDebuggerWatchValueIntf, IdeDebuggerOpts,
  IdeDebuggerValueFormatter, IdeDebuggerValueFormatterDateTime,
  IdeDebuggerValueFormatterColor, IdeDebuggerValueFormatterCurrency;

implementation

procedure DoEnvOptsLoaded; // DebuggerOpts are also loaded now
var
  vc: TIdeDbgValueFormatterSelectorList;
  f: TIdeDbgValueFormatterSelector;
  dt: TIdeDbgValueFormatterDateTime;
  cl: TIdeDbgValueFormatterColor;
begin
  vc := DebuggerOptions.ValueFormatterConfig;
  if vc.DefaultsAdded < 1 then begin

    f := TIdeDbgValueFormatterSelector.Create(TIdeDbgValueFormatterRegistryDateTime);
    f.Name := 'TDateTime formatter';
    f.Enabled := True;
    f.MatchTypeNames.Add('TDateTime');
    f.OriginalValue := vfovAtEnd;
    dt := TIdeDbgValueFormatterDateTime(f.ValFormatter.GetObject);
    dt.DateTimeFormat := 'f';
    dt.DateFormat := 'f';
    dt.TimeFormat := 'f';
    vc.Add(f);

    f := TIdeDbgValueFormatterSelector.Create(TIdeDbgValueFormatterRegistryDateTime);
    f.Name := 'TDate formatter';
    f.Enabled := True;
    f.MatchTypeNames.Add('TDate');
    f.OriginalValue := vfovAtEnd;
    dt := TIdeDbgValueFormatterDateTime(f.ValFormatter.GetObject);
    dt.DateTimeFormat := 'f';
    dt.DateFormat := 'ddddd';
    dt.TimeFormat := 'f';
    vc.Add(f);

    f := TIdeDbgValueFormatterSelector.Create(TIdeDbgValueFormatterRegistryDateTime);
    f.Name := 'TTime formatter';
    f.Enabled := True;
    f.MatchTypeNames.Add('TTime');
    f.OriginalValue := vfovAtEnd;
    dt := TIdeDbgValueFormatterDateTime(f.ValFormatter.GetObject);
    dt.DateTimeFormat := 'f';
    dt.DateFormat := 'f';
    dt.TimeFormat := 'tt';
    vc.Add(f);

    f := TIdeDbgValueFormatterSelector.Create(TIdeDbgValueFormatterRegistryColor);
    f.Name := 'Color formatter';
    f.Enabled := True;
    f.MatchTypeNames.Add('TColor');
    f.OriginalValue := vfovAtEnd;
    cl := TIdeDbgValueFormatterColor(f.ValFormatter.GetObject);
    cl.ShowName := True;
    cl.ShowRgb := True;
    cl.ShowRgbAsDec := False;
    vc.Add(f);

    f := TIdeDbgValueFormatterSelector.Create(TIdeDbgValueFormatterRegistryCurrency);
    f.Name := 'Currency formatter';
    f.Enabled := True;
    f.FilterDisplayFormats := [low(TValueDisplayFormat)..high(TValueDisplayFormat)] - [vdfCategoryMemDump, vdfBaseDecimal..vdfBaseChar];
    f.MatchTypeNames.Add('Currency');
    f.OriginalValue := vfovHide;
    vc.Add(f);

    vc.DefaultsAdded := 1;
  end;
end;

initialization
  AddBootHandler(libhEnvironmentOptionsLoaded, @DoEnvOptsLoaded)
end.

