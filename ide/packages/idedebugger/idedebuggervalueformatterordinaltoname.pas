unit IdeDebuggerValueFormatterOrdinalToName;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fgl, Forms, Controls, StdCtrls,
  // DebuggerIntf
  DbgIntfDebuggerBase, DbgIntfBaseTypes,
  // IdeIntf
  IdeDebuggerValueFormatterIntf, IdeDebuggerWatchValueIntf,
  // IdeDebugger
  IdeDebuggerStringConstants;

type

  { TIdeDebuggerValueFormatterOrdinalToNameFrame }

  TIdeDebuggerValueFormatterOrdinalToNameFrame = class(TFrame, ILazDbgIdeValueFormatterSettingsFrameIntf)
    Label1: TLabel;
    Memo1: TMemo;
  private
  protected
    procedure ReadFrom(AFormatter: ILazDbgIdeValueFormatterIntf);
    function  WriteTo(AFormatter: ILazDbgIdeValueFormatterIntf): Boolean;
  public
    constructor Create(TheOwner: TComponent); override;
  end;

  { TIdeDbgValueFormatterOrdinalToName }

  TIdeDbgValueFormatterOrdinalToName = class( specialize TLazDbgIdeValueFormatterGeneric<TObject> )
  private type
    TNameMap = specialize TFPGMap<int64, AnsiString>;
  private
    FMapData: String;
    FNameMap: TNameMap;

    procedure SetMapData(AValue: String);
  protected
    procedure Init; override;
    procedure Assign(AnOther: TObject); override;
  public
    destructor Destroy; override;
    class function GetRegisteredDisplayName: String;
    function FormatValue(AWatchValue: IWatchResultDataIntf;
      const ADisplayFormat: TWatchDisplayFormat;
      AWatchResultPrinter: IWatchResultPrinter; out APrintedValue: String
      ): Boolean; override;
    function FormatValue(aDBGType: TDBGType;
                         aValue: string;
                         const ADisplayFormat: TWatchDisplayFormat;
                         out APrintedValue: String
                        ): boolean; override;

    function SupportedFeatures: TLazDbgIdeValFormatterFeatures; override;
//    function SupportedDataKinds: TWatchResultDataKinds; override;
  published
    property MapData: String read FMapData write SetMapData;
  end;

  TIdeDbgValueFormatterRegistryOrdinalToName =
    specialize TLazDbgIdeValueFormatterFrameRegistryEntryGeneric<TIdeDbgValueFormatterOrdinalToName, TIdeDebuggerValueFormatterOrdinalToNameFrame>;


implementation

{$R *.lfm}

{ TIdeDebuggerValueFormatterOrdinalToNameFrame }

function DoCompareIdents(const Data1, Data2: string): Integer;
begin
  Result := CompareStr(UpperCase(Data1), UpperCase(Data2));
end;

procedure TIdeDebuggerValueFormatterOrdinalToNameFrame.ReadFrom(
  AFormatter: ILazDbgIdeValueFormatterIntf);
var
  df: TIdeDbgValueFormatterOrdinalToName;
begin
  df := TIdeDbgValueFormatterOrdinalToName(AFormatter.GetObject);

  Memo1.Text := df.MapData;
end;

function TIdeDebuggerValueFormatterOrdinalToNameFrame.WriteTo(
  AFormatter: ILazDbgIdeValueFormatterIntf): Boolean;
var
  df: TIdeDbgValueFormatterOrdinalToName;
begin
  df := TIdeDbgValueFormatterOrdinalToName(AFormatter.GetObject);

  Result := TrimRight(df.MapData) <> TrimRight(Memo1.Text);
  df.MapData := TrimRight(Memo1.Text);
end;

constructor TIdeDebuggerValueFormatterOrdinalToNameFrame.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  Label1.Caption := 'Enter mappings "name=123;"';
end;

{ TIdeDbgValueFormatterOrdinalToName }

procedure TIdeDbgValueFormatterOrdinalToName.SetMapData(AValue: String);
var
  p: PChar;
  l, i: integer;
  tk, tk2: String;
  add, n: Int64;

  function SkipSpaceAndComment: boolean;
  begin
    while (l > 0) do begin
      while (l > 0) and ( (p^ = ' ') or (p^ = #9) ) do begin
        inc(p);
        dec(l);
      end;

      if (p^ = '/') and (p[1] = '/') then begin
        while (l > 0) and (p^ <> #10) and (p^ <> #13) do begin
          inc(p);
          dec(l);
        end;
        exit(l>0);
      end;

      if (p^ = '{') then begin
        while (l > 0) and (p^ <> '}')  do begin
          inc(p);
          dec(l);
        end;
        if l > 0 then begin
          inc(p);
          dec(l);
        end;
        continue;
      end;

      if (p^ = '(') and (p[1] = '*') then begin
        while (l > 0) and (p^ <> '*') and (p[1] <> ')') do begin
          inc(p);
          dec(l);
        end;
        if l > 0 then begin
          inc(p);
          dec(l);
        end;
        if l > 0 then begin
          inc(p);
          dec(l);
        end;
        continue;
      end;

      break;
    end;
    Result := l>0;
  end;

  function AtNewLine: boolean; inline;
  begin
    result := (l > 0) and ((p^ = #10) or (p^ = #13));
    while (l > 0) and ((p^ = #10) or (p^ = #13)) do begin
      inc(p);
      dec(l);
    end;
  end;

  function SkipToNextLine: boolean;
  begin
    repeat
      if not SkipSpaceAndComment then
        break;
      while (l > 0) and
            not ( (p^ in [' ', #9, #10, #13, '{']) or ( (p^ = '/') and (p[1] = '/') ) or ((p^ = '(') and (p[1] = '*')) )
      do begin
        inc(p);
        dec(l);
      end;
    until (l=0) or AtNewLine;
    Result := l>0;
  end;

  function AtEqual: boolean; inline;
  begin
    result := (l > 0) and (p^ = '=');
    if result then begin
      inc(p);
      dec(l);
    end;
  end;

  function GetIdent: string;
  var
    s: PChar;
  begin
    result := '';
    if (l = 0) or not (p^ in ['a'..'z', 'A'..'Z', '_']) then
      exit;
    s := p;
    while (l > 0) and (p^ in ['a'..'z', 'A'..'Z', '_', '0'..'9']) do begin
      inc(p);
      dec(l);
    end;
    SetString(Result, s, p-s);
  end;

  function GetNum(out ANum: Int64): boolean;
  var
    s: PChar;
    t: string;
  begin
    result := False;
    if (l > 0) and (p^ = '#') then begin
      inc(p);
      dec(l);
    end;

    s := p;
    if (l > 0) and (p^ = '-') then begin
      inc(p);
      dec(l);
    end;
    if (l > 0) and (p^ in ['$', '%', '&']) then begin
      inc(p);
      dec(l);
    end;

    result := (l > 0) and (p^ in ['0'..'9', 'a'..'f', 'A'..'F']);
    if not Result then
      exit;

    while (l > 0) and (p^ in ['0'..'9']) do begin
      inc(p);
      dec(l);
    end;
    SetString(t, s, p-s);
    Result := TryStrToInt64(t, ANum);
  end;

begin
  if FMapData = AValue then Exit;
  FMapData := AValue;

  FNameMap.Clear;
  p := PChar(FMapData);
  l := Length(FMapData);
  while l > 0 do begin
    if not SkipSpaceAndComment then
      break;

    if AtNewLine then
      continue;

    tk := GetIdent;
    if tk = '' then begin
      SkipToNextLine;
      continue;
    end;

    SkipSpaceAndComment;
    if not AtEqual then begin
      SkipToNextLine;
      continue;
    end;

    SkipSpaceAndComment;
    tk2 := GetIdent;
    SkipSpaceAndComment;
    add := 0;
    if tk2 <> '' then begin
      if (l > 0) and (P^ = '+') then begin
        i := FNameMap.IndexOfData(tk2);
        if i < 0 then begin
          SkipToNextLine;
          continue;
        end;
        add := FNameMap.Keys[i];
      end
      else
      if (l = 0) or (P^ <> '(') then begin // not typecast
        SkipToNextLine;
        continue;
      end;
      inc(p);
      dec(l);
      if not SkipSpaceAndComment then
        continue;
    end;

    if not GetNum(n) then begin
      SkipToNextLine;
      continue;
    end;
    n := n + add;

    FNameMap.Add(n, tk);

    SkipToNextLine;
  end;

end;

procedure TIdeDbgValueFormatterOrdinalToName.Init;
begin
  FNameMap := TNameMap.Create;
  FNameMap.OnDataCompare := @DoCompareIdents;
  inherited Init;
end;

procedure TIdeDbgValueFormatterOrdinalToName.Assign(AnOther: TObject);
var
  df: TIdeDbgValueFormatterOrdinalToName absolute AnOther;
begin
  inherited Assign(AnOther);
  if AnOther is TIdeDbgValueFormatterOrdinalToName then begin
    MapData := df.FMapData;
  end;
end;

destructor TIdeDbgValueFormatterOrdinalToName.Destroy;
begin
  inherited Destroy;
  FNameMap.Destroy;
end;

class function TIdeDbgValueFormatterOrdinalToName.GetRegisteredDisplayName: String;
begin
  Result := dbgConvertOrdinalToName;
end;

function TIdeDbgValueFormatterOrdinalToName.FormatValue(AWatchValue: IWatchResultDataIntf;
  const ADisplayFormat: TWatchDisplayFormat; AWatchResultPrinter: IWatchResultPrinter; out
  APrintedValue: String): Boolean;
var
  c: Int64;
  i: Integer;
begin
  Result := (AWatchValue.ValueKind in [rdkSignedNumVal, rdkUnsignedNumVal]);
  if not Result then
    exit;

  c := AWatchValue.AsInt64;

  i := FNameMap.IndexOf(c);
  if i < 0 then
    APrintedValue := IntToStr(c)
  else
    APrintedValue := FNameMap[c];
end;

function TIdeDbgValueFormatterOrdinalToName.FormatValue(aDBGType: TDBGType; aValue: string;
  const ADisplayFormat: TWatchDisplayFormat; out APrintedValue: String): boolean;
var
  c: int64;
  i: Integer;
begin
  Result := (aDBGType <> nil) and
            (aDBGType.Kind in [skSimple, skInteger, skCardinal]);
  if not Result then
    exit;
  if not TryStrToInt64(aValue, c) then
    exit(False);

  i := FNameMap.IndexOf(c);
  if i < 0 then
    APrintedValue := IntToStr(c)
  else
    APrintedValue := FNameMap[c];
end;

function TIdeDbgValueFormatterOrdinalToName.SupportedFeatures: TLazDbgIdeValFormatterFeatures;
begin
  Result := [vffFormatValue, vffFormatOldValue, vffValueData];
end;

initialization
  ValueFormatterRegistry.Add(TIdeDbgValueFormatterRegistryOrdinalToName);

end.

