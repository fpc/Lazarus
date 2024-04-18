unit IdeDebuggerValueFormatterDateTime;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Math, Classes, Forms, StdCtrls,
  // DebuggerIntf
  DbgIntfDebuggerBase, DbgIntfBaseTypes,
  // LazDebuggerIntf
  LazDebuggerIntf,
  // IdeIntf
  IdeDebuggerValueFormatterIntf, IdeDebuggerWatchValueIntf,
  // IdeDebugger
  IdeDebuggerOpts, IdeDebuggerValueFormatter, IdeDebuggerStringConstants;

type

  { TIdeDebuggerValueFormatterDateTimeFrame }

  TIdeDebuggerValueFormatterDateTimeFrame = class(TFrame, ILazDbgIdeValueFormatterSettingsFrameIntf)
    edDateTime: TEdit;
    edDate: TEdit;
    edTime: TEdit;
    lbDateTime: TLabel;
    lbDate: TLabel;
    lbTime: TLabel;
  protected
    procedure ReadFrom(AFormatter: ILazDbgIdeValueFormatterIntf);
    function  WriteTo(AFormatter: ILazDbgIdeValueFormatterIntf): Boolean;
  public
    constructor Create(TheOwner: TComponent); override;
  end;

  { TIdeDbgValueFormatterDateTime }

  TIdeDbgValueFormatterDateTime = class(specialize TLazDbgIdeValueFormatterGeneric<TObject>)
  private
    FDateTimeFormat: String;
    FDateFormat: String;
    FTimeFormat: String;
  protected
    procedure Init; override;
    procedure Assign(AnOther: TObject); override;
  public
    class function GetRegisteredDisplayName: String;
    //constructor Create;
    function FormatValue(AWatchValue: IWatchResultDataIntf;
      ADisplayFormat: TWatchDisplayFormat;
      AWatchResultPrinter: IWatchResultPrinter; out APrintedValue: String
      ): Boolean; override; experimental;
    function FormatValue(aDBGType: TDBGType;
                         aValue: string;
                         ADisplayFormat: TWatchDisplayFormat;
                         out APrintedValue: String
                        ): boolean; override; deprecated 'For values from older backends only - to be removed as backends are upgraded';

    function SupportedFeatures: TLazDbgIdeValFormatterFeatures; override;
    function SupportedDataKinds: TWatchResultDataKinds; override;
  published
    property DateTimeFormat: String read FDateTimeFormat write FDateTimeFormat;
    property DateFormat: String read FDateFormat write FDateFormat;
    property TimeFormat: String read FTimeFormat write FTimeFormat;
  end;
  TIdeDbgValueFormatterRegistryDateTime =
    specialize TLazDbgIdeValueFormatterFrameRegistryEntryGeneric<TIdeDbgValueFormatterDateTime, TIdeDebuggerValueFormatterDateTimeFrame>;

implementation

{$R *.lfm}

{ TIdeDebuggerValueFormatterDateTimeFrame }

procedure TIdeDebuggerValueFormatterDateTimeFrame.ReadFrom(
  AFormatter: ILazDbgIdeValueFormatterIntf);
var
  f: TIdeDbgValueFormatterDateTime;
begin
  f := AFormatter.GetObject as TIdeDbgValueFormatterDateTime;
  edDateTime.Text := f.DateTimeFormat;
  edDate.Text     := f.DateFormat;
  edTime.Text     := f.TimeFormat;
end;

function TIdeDebuggerValueFormatterDateTimeFrame.WriteTo(
  AFormatter: ILazDbgIdeValueFormatterIntf): Boolean;
var
  f: TIdeDbgValueFormatterDateTime;
begin
  f := AFormatter.GetObject as TIdeDbgValueFormatterDateTime;
  Result :=
    (edDateTime.Text <> f.DateTimeFormat) or
    (edDate.Text     <> f.DateFormat) or
    (edTime.Text     <> f.TimeFormat);
  if not Result then
    exit;

  f.DateTimeFormat := edDateTime.Text;
  f.DateFormat     := edDate.Text;
  f.TimeFormat     := edTime.Text;
end;

constructor TIdeDebuggerValueFormatterDateTimeFrame.Create(TheOwner: TComponent
  );
begin
  inherited Create(TheOwner);
  lbDateTime.Caption := ValFormatterDateTimeFormatDT;
  lbDate.Caption := ValFormatterDateTimeFormatD;
  lbTime.Caption := ValFormatterDateTimeFormatT;
end;

{ TIdeDbgValueFormatterDateTime }

procedure TIdeDbgValueFormatterDateTime.Init;
begin
  inherited Init;
  FDateTimeFormat := 'f';
  FDateFormat     := 'ddddd';
  FTimeFormat     := 'tt';
end;

procedure TIdeDbgValueFormatterDateTime.Assign(AnOther: TObject);
var
  f: TIdeDbgValueFormatterDateTime;
begin
  inherited Assign(AnOther);

  if AnOther is TIdeDbgValueFormatterDateTime then begin
    f := AnOther as TIdeDbgValueFormatterDateTime;

    DateTimeFormat := f.DateTimeFormat;
    DateFormat     := f.DateFormat;
    TimeFormat     := f.TimeFormat;
  end;
end;

class function TIdeDbgValueFormatterDateTime.GetRegisteredDisplayName: String;
begin
  Result := ValFormatterDateTimeName;
end;

function TIdeDbgValueFormatterDateTime.FormatValue(
  AWatchValue: IWatchResultDataIntf; ADisplayFormat: TWatchDisplayFormat;
  AWatchResultPrinter: IWatchResultPrinter; out APrintedValue: String): Boolean;
var
  MyDate: Extended;
begin
  Result := (AWatchValue.ValueKind = rdkFloatVal);
  if not Result then
    exit;

  MyDate := AWatchValue.AsFloat;
  // it is important to know datetime for all TDate/TTime/TDateTime
  if SameValue(Frac(MyDate), 0) then
    DateTimeToString(APrintedValue, FDateFormat, MyDate)
  else
  if SameValue(Int(MyDate), 0) then
    DateTimeToString(APrintedValue, FTimeFormat, MyDate)
  else
    DateTimeToString(APrintedValue, FDateTimeFormat, MyDate);

  Result := True;
end;

function TIdeDbgValueFormatterDateTime.FormatValue(aDBGType: TDBGType;
  aValue: string; ADisplayFormat: TWatchDisplayFormat; out APrintedValue: String
  ): boolean;
var
  FS: TFormatSettings;
  MyDate: Extended;
begin
  Result := (aDBGType <> nil) and (aDBGType.Kind in [skSimple, skFloat]);
  if not Result then
    exit;

  FillChar(FS{%H-}, SizeOf(TFormatSettings), 0);
  FS.DecimalSeparator := '.';
  if TryStrToFloat(aValue, MyDate, FS) then
  begin
    // it is important to know datetime for all TDate/TTime/TDateTime
    if SameValue(Frac(MyDate), 0) then
      DateTimeToString(APrintedValue, FDateFormat, MyDate)
    else
    if SameValue(Int(MyDate), 0) then
      DateTimeToString(APrintedValue, FTimeFormat, MyDate)
    else
      DateTimeToString(APrintedValue, FDateTimeFormat, MyDate);

    Result := True;
  end;
end;

function TIdeDbgValueFormatterDateTime.SupportedFeatures: TLazDbgIdeValFormatterFeatures;
begin
  Result := [vffFormatValue, vffFormatOldValue, vffValueData];
end;

function TIdeDbgValueFormatterDateTime.SupportedDataKinds: TWatchResultDataKinds;
begin
  Result := [rdkFloatVal];
end;

initialization
  ValueFormatterRegistry.Add(TIdeDbgValueFormatterRegistryDateTime);

end.

