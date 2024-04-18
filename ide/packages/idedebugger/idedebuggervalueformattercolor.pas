unit IdeDebuggerValueFormatterColor;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Math, Classes, Forms, Controls,
  Graphics, StdCtrls, ExtCtrls,
  System.UITypes,  // register TColor
  {$IF FPC_FULLVERSION>030202}
  System.UIConsts, // required to register Alphacolors
  {$ENDIF}
  // DebuggerIntf
  DbgIntfDebuggerBase, DbgIntfBaseTypes,
  // LazDebuggerIntf
  LazDebuggerIntf,
  // IdeIntf
  IdeDebuggerValueFormatterIntf, IdeDebuggerWatchValueIntf, LazIDEIntf,
  // IdeDebugger
  IdeDebuggerOpts, IdeDebuggerValueFormatter, IdeDebuggerStringConstants;

type

  { TIdeDebuggerValueFormatterColorFrame }

  TIdeDebuggerValueFormatterColorFrame = class(TFrame, ILazDbgIdeValueFormatterSettingsFrameIntf)
    cbRgbDec: TCheckBox;
    rgNameOrRgb: TRadioGroup;
  protected
    procedure ReadFrom(AFormatter: ILazDbgIdeValueFormatterIntf);
    function  WriteTo(AFormatter: ILazDbgIdeValueFormatterIntf): Boolean;
  public
    constructor Create(TheOwner: TComponent); override;
  end;

  { TIdeDbgValueFormatterColorBase }

  TIdeDbgValueFormatterColorBase = class(specialize TLazDbgIdeValueFormatterGeneric<TObject>)
  private
    FShowName: Boolean;
    FShowRgb: Boolean;
    FShowRgbAsDec: Boolean;
  protected
    procedure Init; override;
    procedure Assign(AnOther: TObject); override;
  public
    function SupportedFeatures: TLazDbgIdeValFormatterFeatures; override;
  published
    property ShowName: Boolean read FShowName write FShowName;
    property ShowRgb:  Boolean read FShowRgb write FShowRgb;
    property ShowRgbAsDec: Boolean read FShowRgbAsDec write FShowRgbAsDec;
  end;

  TIdeDbgValueFormatterColor = class(TIdeDbgValueFormatterColorBase)
    class function GetRegisteredDisplayName: String;
    function FormatValue(AWatchValue: IWatchResultDataIntf;
      ADisplayFormat: TWatchDisplayFormat;
      AWatchResultPrinter: IWatchResultPrinter; out APrintedValue: String
      ): Boolean; override; experimental;
    function FormatValue(aDBGType: TDBGType;
                         aValue: string;
                         ADisplayFormat: TWatchDisplayFormat;
                         out APrintedValue: String
                        ): boolean; override; deprecated 'For values from older backends only - to be removed as backends are upgraded';
  end;
  TIdeDbgValueFormatterRegistryColor =
    specialize TLazDbgIdeValueFormatterFrameRegistryEntryGeneric<TIdeDbgValueFormatterColor, TIdeDebuggerValueFormatterColorFrame>;

  { TIdeDbgValueFormatterColorAlpha }

  TIdeDbgValueFormatterColorAlpha = class(TIdeDbgValueFormatterColorBase)
    class function GetRegisteredDisplayName: String;
    function FormatValue(AWatchValue: IWatchResultDataIntf;
      ADisplayFormat: TWatchDisplayFormat;
      AWatchResultPrinter: IWatchResultPrinter; out APrintedValue: String
      ): Boolean; override; experimental;
    function FormatValue(aDBGType: TDBGType;
                         aValue: string;
                         ADisplayFormat: TWatchDisplayFormat;
                         out APrintedValue: String
                        ): boolean; override; deprecated 'For values from older backends only - to be removed as backends are upgraded';
  end;
  TIdeDbgValueFormatterRegistryColorAlpha =
    specialize TLazDbgIdeValueFormatterFrameRegistryEntryGeneric<TIdeDbgValueFormatterColorAlpha, TIdeDebuggerValueFormatterColorFrame>;

implementation

{$R *.lfm}

{ TIdeDebuggerValueFormatterColorFrame }

procedure TIdeDebuggerValueFormatterColorFrame.ReadFrom(
  AFormatter: ILazDbgIdeValueFormatterIntf);
var
  f: TIdeDbgValueFormatterColorBase;
begin
  f := AFormatter.GetObject as TIdeDbgValueFormatterColorBase;
  if f.ShowName and f.ShowRgb then
    rgNameOrRgb.ItemIndex := 2
  else
  if f.ShowRgb then
    rgNameOrRgb.ItemIndex := 1
  else
    rgNameOrRgb.ItemIndex := 0;

  cbRgbDec.Checked := f.ShowRgbAsDec;
end;

function TIdeDebuggerValueFormatterColorFrame.WriteTo(
  AFormatter: ILazDbgIdeValueFormatterIntf): Boolean;
var
  f: TIdeDbgValueFormatterColorBase;
begin
  f := AFormatter.GetObject as TIdeDbgValueFormatterColorBase;
  case rgNameOrRgb.ItemIndex of
    0: Result := (f.ShowName <> True)  or (f.ShowRgb <> False);
    1: Result := (f.ShowName <> False) or (f.ShowRgb <> True);
    2: Result := (f.ShowName <> True)  or (f.ShowRgb <> True);
    else Result := True;
  end;
  Result := Result or (cbRgbDec.Checked <> f.ShowRgbAsDec);

  case rgNameOrRgb.ItemIndex of
    0: begin
      f.ShowName := True;
      f.ShowRgb  := False;
    end;
    1: begin
      f.ShowName := False;
      f.ShowRgb  := True;
    end;
    else begin
      f.ShowName := True;
      f.ShowRgb  := True;
    end;
  end;

  f.ShowRgbAsDec :=  cbRgbDec.Checked;
end;

constructor TIdeDebuggerValueFormatterColorFrame.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  rgNameOrRgb.Items.Add(ValFormatterColorShowName);
  rgNameOrRgb.Items.Add(ValFormatterColorShowRgb);
  rgNameOrRgb.Items.Add(ValFormatterColorShowBoth);
  cbRgbDec.Caption := ValFormatterColorRgbDec;
end;

{ TIdeDbgValueFormatterColorBase }

procedure TIdeDbgValueFormatterColorBase.Init;
begin
  inherited Init;
  FShowName     := True;
  FShowRgb      := True;
  FShowRgbAsDec := False;
end;

procedure TIdeDbgValueFormatterColorBase.Assign(AnOther: TObject);
var
  f: TIdeDbgValueFormatterColorBase;
begin
  inherited Assign(AnOther);

  if AnOther is TIdeDbgValueFormatterColorBase then begin
    f := AnOther as TIdeDbgValueFormatterColorBase;

    FShowName     := f.FShowName;
    FShowRgb      := f.FShowRgb;
    FShowRgbAsDec := f.FShowRgbAsDec;
  end;
end;

function TIdeDbgValueFormatterColorBase.SupportedFeatures: TLazDbgIdeValFormatterFeatures;
begin
  Result := [vffFormatValue, vffFormatOldValue, vffValueData];
end;

class function TIdeDbgValueFormatterColor.GetRegisteredDisplayName: String;
begin
  Result := ValFormatterColorName;
end;

{ TIdeDbgValueFormatterColor }

function TIdeDbgValueFormatterColor.FormatValue(
  AWatchValue: IWatchResultDataIntf; ADisplayFormat: TWatchDisplayFormat;
  AWatchResultPrinter: IWatchResultPrinter; out APrintedValue: String): Boolean;
var
  IntToIdentFn: TIntToIdent;
  c: Int64;
begin
  Result := (AWatchValue.ValueKind in [rdkSignedNumVal, rdkUnsignedNumVal]);
  if not Result then
    exit;

  Result := False;
  c := AWatchValue.AsInt64;
  if (c > high(Integer)) or (c < low(Integer)) then
    exit;

  APrintedValue := '';
  if ShowName then begin
    IntToIdentFn := FindIntToIdent(TypeInfo(TColor));
    Result := ( Assigned(IntToIdentFn) and
                IntToIdentFn(c, APrintedValue)
              );
    if not Result then
      APrintedValue := '';
  end;

  if ShowRgb and ((c and $ffffff) = c) then begin
    if APrintedValue <> '' then
      APrintedValue := APrintedValue + ' = ';
    APrintedValue := APrintedValue + 'RGB(';
    if ShowRgbAsDec then begin
      APrintedValue := APrintedValue + IntToStr((c and $0000FF)) +', ';
      APrintedValue := APrintedValue + IntToStr((c and $00FF00)>>8) +', ';
      APrintedValue := APrintedValue + IntToStr((c and $FF0000)>>16) +')';
    end
    else begin
      APrintedValue := APrintedValue + IntToHex((c and $0000FF), 2) +', ';
      APrintedValue := APrintedValue + IntToHex((c and $00FF00)>>8, 2) +', ';
      APrintedValue := APrintedValue + IntToHex((c and $FF0000)>>16, 2) +')';
    end;
    Result := True;
  end;
end;

function TIdeDbgValueFormatterColor.FormatValue(aDBGType: TDBGType;
  aValue: string; ADisplayFormat: TWatchDisplayFormat; out APrintedValue: String
  ): boolean;
var
  c: integer;
  IntToIdentFn: TIntToIdent;
begin
  Result := (aDBGType <> nil) and
            (aDBGType.Kind in [skSimple, skInteger, skCardinal]);
  if not Result then
    exit;

  Result := False;
  if not TryStrToInt(aValue, c) then
    exit;

  APrintedValue := '';
  if ShowName then begin
    IntToIdentFn := FindIntToIdent(TypeInfo(TColor));
    Result := ( Assigned(IntToIdentFn) and
                IntToIdentFn(c, APrintedValue)
              );
    if not Result then
      APrintedValue := '';
  end;

  if ShowRgb and ((c and $ffffff) = c) then begin
    if APrintedValue <> '' then
      APrintedValue := APrintedValue + ' = ';
    APrintedValue := APrintedValue + 'RGB(';
    if ShowRgbAsDec then begin
      APrintedValue := APrintedValue + IntToStr((c and $0000FF)) +', ';
      APrintedValue := APrintedValue + IntToStr((c and $00FF00)>>8) +', ';
      APrintedValue := APrintedValue + IntToStr((c and $FF0000)>>16) +')';
    end
    else begin
      APrintedValue := APrintedValue + IntToHex((c and $0000FF), 2) +', ';
      APrintedValue := APrintedValue + IntToHex((c and $00FF00)>>8, 2) +', ';
      APrintedValue := APrintedValue + IntToHex((c and $FF0000)>>16, 2) +')';
    end;
    Result := True;
  end;
end;

{ TIdeDbgValueFormatterColorAlpha }

class function TIdeDbgValueFormatterColorAlpha.GetRegisteredDisplayName: String;
begin
  Result := ValFormatterColorNameAlpha;
end;

function TIdeDbgValueFormatterColorAlpha.FormatValue(
  AWatchValue: IWatchResultDataIntf; ADisplayFormat: TWatchDisplayFormat;
  AWatchResultPrinter: IWatchResultPrinter; out APrintedValue: String): Boolean;
var
  IntToIdentFn: TIntToIdent;
  c: Int64;
begin
  Result := (AWatchValue.ValueKind in [rdkSignedNumVal, rdkUnsignedNumVal]);
  if not Result then
    exit;

  Result := False;
  c := AWatchValue.AsInt64;
  if (c > high(Cardinal)) or (c < -$ffffffff) then
    exit;

  APrintedValue := '';
  if ShowName then begin
    IntToIdentFn := FindIntToIdent(TypeInfo(TAlphaColor));
    Result := ( Assigned(IntToIdentFn) and
                IntToIdentFn(Integer(c), APrintedValue)
              );
    if not Result then
      APrintedValue := '';
  end;

  if ShowRgb then begin
    if APrintedValue <> '' then
      APrintedValue := APrintedValue + ' = ';
    APrintedValue := APrintedValue + 'ARGB(';
    if ShowRgbAsDec then begin
      APrintedValue := APrintedValue + IntToStr((c and $FF000000)>>24) +', ';
      APrintedValue := APrintedValue + IntToStr((c and $0000FF)) +', ';
      APrintedValue := APrintedValue + IntToStr((c and $00FF00)>>8) +', ';
      APrintedValue := APrintedValue + IntToStr((c and $FF0000)>>16) +')';
    end
    else begin
      APrintedValue := APrintedValue + IntToHex((c and $FF000000)>>24, 2) +', ';
      APrintedValue := APrintedValue + IntToHex((c and $0000FF), 2) +', ';
      APrintedValue := APrintedValue + IntToHex((c and $00FF00)>>8, 2) +', ';
      APrintedValue := APrintedValue + IntToHex((c and $FF0000)>>16, 2) +')';
    end;
    Result := True;
  end;
end;

function TIdeDbgValueFormatterColorAlpha.FormatValue(aDBGType: TDBGType;
  aValue: string; ADisplayFormat: TWatchDisplayFormat; out APrintedValue: String
  ): boolean;
var
  c: int64;
  IntToIdentFn: TIntToIdent;
begin
  Result := (aDBGType <> nil) and
            (aDBGType.Kind in [skSimple, skInteger, skCardinal]);
  if not Result then
    exit;

  Result := False;
  if not TryStrToInt64(aValue, c) then
    exit;
  if (c > high(Cardinal)) or (c < -$ffffffff) then
    exit;

  APrintedValue := '';
  if ShowName then begin
    IntToIdentFn := FindIntToIdent(TypeInfo(TAlphaColor));
    Result := ( Assigned(IntToIdentFn) and
                IntToIdentFn(integer(c), APrintedValue)
              );
    {$IF FPC_FULLVERSION>030202}
    if not Result then
      Result := AlphaColorToIdent(integer(c), APrintedValue);
    {$ENDIF}

    if not Result then
      APrintedValue := '';
  end;

  if ShowRgb and ((c and $ffffff) = c) then begin
    if APrintedValue <> '' then
      APrintedValue := APrintedValue + ' = ';
    APrintedValue := APrintedValue + 'ARGB(';
    if ShowRgbAsDec then begin
      APrintedValue := APrintedValue + IntToStr((c and $FF000000)>>24) +', ';
      APrintedValue := APrintedValue + IntToStr((c and $0000FF)) +', ';
      APrintedValue := APrintedValue + IntToStr((c and $00FF00)>>8) +', ';
      APrintedValue := APrintedValue + IntToStr((c and $FF0000)>>16) +')';
    end
    else begin
      APrintedValue := APrintedValue + IntToHex((c and $FF000000)>>24, 2) +', ';
      APrintedValue := APrintedValue + IntToHex((c and $0000FF), 2) +', ';
      APrintedValue := APrintedValue + IntToHex((c and $00FF00)>>8, 2) +', ';
      APrintedValue := APrintedValue + IntToHex((c and $FF0000)>>16, 2) +')';
    end;
    Result := True;
  end;
end;

initialization
  ValueFormatterRegistry.Add(TIdeDbgValueFormatterRegistryColor);
  ValueFormatterRegistry.Add(TIdeDbgValueFormatterRegistryColorAlpha);
  //{$IF FPC_FULLVERSION>030202}
  //RegisterAlphaColorIntegerConsts;
  //{$ENDIF}

end.

