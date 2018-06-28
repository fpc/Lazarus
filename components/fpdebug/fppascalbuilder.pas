unit FpPascalBuilder;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DbgIntfBaseTypes, DbgIntfDebuggerBase, FpDbgInfo, FpdMemoryTools,
  FpErrorMessages, LazLoggerBase;

type
  TTypeNameFlag = (
    tnfOnlyDeclared,    // do not return a substitute with ^ symbol
    tnfIncludeOneRef    // If it is a pointer, and the pointed-to name is known, return ^TXxx
                        //   without tnfOnlyDeclared, may return ^^^TXxx if needed

  );
  TTypeNameFlags = set of TTypeNameFlag;

  TTypeDeclarationFlag = (
    tdfNoFirstLineIndent,
    tdfIncludeVarName,     // like i: Integer
    tdfSkipClassBody,      // shorten class
    tdfSkipRecordBody,      // shorten class

    tdfDynArrayWithPointer, // TODO, temp, act like gdb
    tdfStopAfterPointer
  );
  TTypeDeclarationFlags = set of TTypeDeclarationFlag;

  TFpPrettyPrintValueFlag = (
    ppvCreateDbgType,
    ppvSkipClassBody, ppvSkipRecordBody
  );
  TFpPrettyPrintValueFlags = set of TFpPrettyPrintValueFlag;

  TFpPrettyPrintOption = (
    ppoStackParam
  );
  TFpPrettyPrintOptions = set of TFpPrettyPrintOption;

const
  PV_FORWARD_FLAGS = [ppvSkipClassBody, ppvSkipRecordBody];

type

  PDBGType = ^TDBGType;

  { TFpPascalPrettyPrinter }

  TFpPascalPrettyPrinter = class
  private
    FAddressSize: Integer;
    FMemManager: TFpDbgMemManager;
    function InternalPrintValue(out APrintedValue: String;
                                AValue: TFpDbgValue;
                                AnAddressSize: Integer;
                                AFlags: TFpPrettyPrintValueFlags;
                                ANestLevel: Integer; AnIndent: String;
                                ADisplayFormat: TWatchDisplayFormat;
                                ARepeatCount: Integer = -1;
                                ADBGTypeInfo: PDBGType = nil;
                                AOptions: TFpPrettyPrintOptions = []
                               ): Boolean;
  public
    constructor Create(AnAddressSize: Integer);
    function PrintValue(out APrintedValue: String;
                        AValue: TFpDbgValue;
                        ADisplayFormat: TWatchDisplayFormat = wdfDefault;
                        ARepeatCount: Integer = -1;
                        AOptions: TFpPrettyPrintOptions = []
                       ): Boolean;
    function PrintValue(out APrintedValue: String;
                        out ADBGTypeInfo: TDBGType;
                        AValue: TFpDbgValue;
                        ADisplayFormat: TWatchDisplayFormat = wdfDefault;
                        ARepeatCount: Integer = -1
                       ): Boolean;
    property AddressSize: Integer read FAddressSize write FAddressSize;
    property MemManager: TFpDbgMemManager read FMemManager write FMemManager;
  end;



function GetTypeName(out ATypeName: String; ADbgSymbol: TFpDbgSymbol; AFlags: TTypeNameFlags = []): Boolean;
function GetTypeAsDeclaration(out ATypeDeclaration: String; ADbgSymbol: TFpDbgSymbol;
  AFlags: TTypeDeclarationFlags = []; AnIndent: Integer = 0): Boolean;

implementation

function GetTypeName(out ATypeName: String; ADbgSymbol: TFpDbgSymbol;
  AFlags: TTypeNameFlags): Boolean;
var
  s: String;
begin
  ATypeName := '';
  Result := ADbgSymbol <> nil;
  if not Result then
    exit;
  if ADbgSymbol.SymbolType = stValue then begin
    ADbgSymbol := ADbgSymbol.TypeInfo;
    Result := ADbgSymbol <> nil;
    if not Result then
      exit;
  end;

  ATypeName := ADbgSymbol.Name;
  Result := ATypeName <> '';

  if (tnfIncludeOneRef in AFlags) or
     ((not Result) and (not (tnfOnlyDeclared in AFlags)))
  then begin
    ATypeName := '^';
    while ADbgSymbol.Kind = skPointer do begin
      ADbgSymbol := ADbgSymbol.TypeInfo;
      s := ADbgSymbol.Name;
      if s <> '' then begin
        ATypeName := ATypeName + s;
        Result := True;
        exit;
      end;

      if (tnfOnlyDeclared in AFlags) then // only one level
        exit;
      ATypeName := ATypeName + '^';
    end;

    ATypeName := '';
    Result := False;
  end;

end;

function GetTypeAsDeclaration(out ATypeDeclaration: String; ADbgSymbol: TFpDbgSymbol;
  AFlags: TTypeDeclarationFlags; AnIndent: Integer): Boolean;
var
  IndentString: String;

  function GetIndent: String;
  begin
    if (IndentString = '') and (AnIndent > 0) then
      IndentString := StringOfChar(' ', AnIndent);
    Result := IndentString;
  end;

  function NeedBracket(S: String): Boolean;
  var
    i, l: Integer;
  begin
    l := 0;
    i := length(s);
    while (i > 0) do begin
      case s[i] of
        'a'..'z', 'A'..'Z', '0'..'9', '_', '$', '^': ;
         '(': dec(l);
         ')': inc(l);
        else
          if l = 0 then break;
      end;
      dec(i);
    end;
    Result := i > 0;
  end;

  Function MembersAsGdbText(out AText: String; WithVisibilty: Boolean; AFlags: TTypeDeclarationFlags = []): Boolean;
  var
    CurVis: TDbgSymbolMemberVisibility;

    procedure AddVisibility(AVis: TDbgSymbolMemberVisibility; AFirst: Boolean);
    begin
      if not (WithVisibilty and ((CurVis <> AVis) or AFirst)) then
        exit;
      CurVis := AVis;
      case AVis of
        svPrivate:   AText := AText + GetIndent + '  private' + LineEnding;
        svProtected: AText := AText + GetIndent + '  protected' + LineEnding;
        svPublic:    AText := AText + GetIndent + '  public' + LineEnding;
      end;
    end;

  var
    c, i: Integer;
    m: TFpDbgSymbol;
    s: String;
  begin
    Result := True;
    AText := '';
    c := ADbgSymbol.MemberCount;
    i := 0;
    while (i < c) and Result do begin
      m := ADbgSymbol.Member[i];
      AddVisibility(m.MemberVisibility, i= 0);
      if tdfStopAfterPointer in AFlags then
        Result := GetTypeName(s, m)
      else
        Result := GetTypeAsDeclaration(s, m, [tdfIncludeVarName, tdfStopAfterPointer] + AFlags, AnIndent + 4);
      if Result then
        AText := AText + GetIndent + s + ';' + LineEnding;
      inc(i);
    end;
  end;

  function GetPointerType(out ADeclaration: String): Boolean;
  var
    s: String;
  begin
    s := '';
    while (ADbgSymbol.Kind = skPointer) and (ADbgSymbol.TypeInfo <> nil) do begin
      ADbgSymbol := ADbgSymbol.TypeInfo;
      s := s + '^';
    end;
    if (tdfStopAfterPointer in AFlags) then begin
      Result := GetTypeName(ADeclaration, ADbgSymbol, []);
    end
    else begin
      Result := GetTypeAsDeclaration(ADeclaration, ADbgSymbol, AFlags + [tdfStopAfterPointer]);
      if not Result then
        Result := GetTypeName(ADeclaration, ADbgSymbol, []);
    end;
    if NeedBracket(ADeclaration)
    then ADeclaration := s + '(' + ADeclaration + ')'
    else ADeclaration := s + ADeclaration;
  end;

  function GetBaseType(out ADeclaration: String): Boolean;
  var
    s1, s2: String;
  begin
    if sfSubRange in ADbgSymbol.Flags then begin
      case ADbgSymbol.Kind of
        // TODO: check bound are in size
        skInteger: begin
            Result := ADbgSymbol.HasBounds;
            if Result then ADeclaration := Format('%d..%d', [ADbgSymbol.OrdLowBound, ADbgSymbol.OrdHighBound]);
          end;
        skCardinal: begin
            Result := ADbgSymbol.HasBounds;
            if Result then ADeclaration := Format('%u..%u', [QWord(ADbgSymbol.OrdLowBound), QWord(ADbgSymbol.OrdHighBound)]);
          end;
        skChar: begin
            Result := ADbgSymbol.HasBounds;
            if (ADbgSymbol.OrdLowBound >= 32) and (ADbgSymbol.OrdLowBound <= 126)
            then s1 := '''' + chr(ADbgSymbol.OrdLowBound) + ''''
            else s1 := '#'+IntToStr(ADbgSymbol.OrdLowBound);
            if (ADbgSymbol.OrdHighBound >= 32) and (ADbgSymbol.OrdHighBound <= 126)
            then s2 := '''' + chr(ADbgSymbol.OrdHighBound) + ''''
            else s2 := '#'+IntToStr(ADbgSymbol.OrdHighBound);
            if Result then ADeclaration := Format('%s..%s', [s1, s2]);
          end;
        else
          Result := False; // not sure how to show a subrange of skFloat, skBoolean, :
      end;
    end
    else
      Result := GetTypeName(ADeclaration, ADbgSymbol, []);
  end;

  function GetFunctionType(out ADeclaration: String): Boolean;
  var
    s: String;
  begin
    // Todo param
    GetTypeAsDeclaration(s, ADbgSymbol.TypeInfo, AFlags);
    ADeclaration := 'function ' + ADbgSymbol.Name + ' () : ' + s + '';
    if sfVirtual in ADbgSymbol.Flags then ADeclaration := ADeclaration + '; virtual';
    Result := true;
  end;

  function GetProcedureType(out ADeclaration: String): Boolean;
  begin
    // Todo param
    ADeclaration := 'procedure ' + ADbgSymbol.Name + ' ()';
    if sfVirtual in ADbgSymbol.Flags then ADeclaration := ADeclaration + '; virtual';
    Result := true;
  end;

  function GetClassType(out ADeclaration: String): Boolean;
  var
    s, s2: String;
  begin
    Result := tdfSkipClassBody in AFlags;
    if Result then begin
      GetTypeName(s, ADbgSymbol);
      ADeclaration := s + ' {=class}';
      exit;
    end;
    Result := MembersAsGdbText(s, True, [tdfSkipClassBody]);
    if not GetTypeName(s2, ADbgSymbol.TypeInfo) then
      s2 := '';
    if Result then
      ADeclaration := Format('class(%s)%s%s%send',
                             [s2, LineEnding, s, GetIndent]);
  end;

  function GetRecordType(out ADeclaration: String): Boolean;
  var
    s: String;
  begin
    if tdfSkipRecordBody in AFlags then begin
      Result := True;
      if GetTypeName(s, ADbgSymbol) then
        ADeclaration := s + ' {=record}'
      else
        ADeclaration := Format('record {...};%s%send', [LineEnding, GetIndent]);
      exit;
    end;
    Result := MembersAsGdbText(s, False);
    if Result then
      ADeclaration := Format('record%s%s%send', [LineEnding, s, GetIndent]);
  end;

  function GetEnumType(out ADeclaration: String): Boolean;
  var
    i, j, val: Integer;
    m: TFpDbgSymbol;
  begin
    // TODO assigned value (a,b:=3,...)
    Result := True;
    ADeclaration := '(';
    j := 0;
    for i := 0 to ADbgSymbol.MemberCount - 1 do begin
      m := ADbgSymbol.Member[i];
      if i > 0 then ADeclaration := ADeclaration + ', ';
      ADeclaration := ADeclaration + m.Name;
      if m.HasOrdinalValue then begin
        val := m.OrdinalValue;
        if j <> val then begin
          ADeclaration := ADeclaration + ' := ' + IntToStr(val);
          j := val;
          continue;
        end;
      end;
      inc(j);
    end;
    ADeclaration := ADeclaration + ')'
  end;

  function GetSetType(out ADeclaration: String): Boolean;
  var
    t: TFpDbgSymbol;
    s: String;
  begin
    // TODO assigned value (a,b:=3,...)
    t := ADbgSymbol.TypeInfo;
    Result := t <> nil;
    if not Result then exit;

    case t.Kind of
      skInteger: begin
          Result := t.HasBounds;
          ADeclaration := format('set of %d..%d', [t.OrdLowBound, t.OrdHighBound]);
        end;
      skCardinal: begin
          Result := t.HasBounds;
          ADeclaration := format('set of %u..%u', [QWord(t.OrdLowBound), QWord(t.OrdHighBound)]);
        end;
      skEnum: begin
          if t.Name <> '' then begin
            Result := True;
            s := t.Name;
          end
          else
            Result := GetTypeAsDeclaration(s, t, AFlags);
          ADeclaration := 'set of ' + s;
        end;
      else
        Result := False;
    end;
  end;

  function GetArrayType(out ADeclaration: String): Boolean;
  var
    t: TFpDbgSymbol;
    s: String;
    i: Integer;
  begin
    // TODO assigned value (a,b:=3,...)
    t := ADbgSymbol.TypeInfo;
    Result := (t <> nil);
    if not Result then exit;

    s := t.Name;
    if s = '' then begin
      if tdfStopAfterPointer in AFlags then
        Result := GetTypeName(s, t)
      else
        Result := GetTypeAsDeclaration(s, t, [tdfNoFirstLineIndent, tdfStopAfterPointer] + AFlags, AnIndent + 4); // no class ?
      if not Result then exit;
    end;


    if sfDynArray in ADbgSymbol.Flags then begin //supprts only one level
      ADeclaration := 'array of ' + s;
      if tdfDynArrayWithPointer in AFlags then
        ADeclaration := '^(' + ADeclaration + ')';
    end
    else begin
      ADeclaration := 'array [';
      for i := 0 to ADbgSymbol.MemberCount - 1 do begin
        if i > 0 then
          ADeclaration := ADeclaration + ', ';
        t := ADbgSymbol.Member[i];
        if t.Kind = skCardinal
        then ADeclaration := ADeclaration + Format('%u..%u', [QWord(t.OrdLowBound), QWord(t.OrdHighBound)])
        else ADeclaration := ADeclaration + Format('%d..%d', [t.OrdLowBound, t.OrdHighBound]);
      end;
      ADeclaration := ADeclaration + '] of ' + s;
    end;
  end;

var
  VarName: String;
begin
  Result := ADbgSymbol <> nil;
  if not Result then
    exit;
  VarName := '';
  if (ADbgSymbol.SymbolType = stValue) and
     not((ADbgSymbol.Kind = skProcedure) or (ADbgSymbol.Kind = skFunction))
  then begin
    if tdfIncludeVarName in AFlags then
      VarName := ADbgSymbol.Name;
    ADbgSymbol := ADbgSymbol.TypeInfo;
    Result := ADbgSymbol <> nil;
    if not Result then
      exit;
  end;

  case ADbgSymbol.Kind of
    skPointer:   Result := GetPointerType(ATypeDeclaration);
    skInteger, skCardinal, skBoolean, skChar, skFloat:
                 Result := GetBaseType(ATypeDeclaration);
    skFunction:  Result := GetFunctionType(ATypeDeclaration);
    skProcedure: Result := GetProcedureType(ATypeDeclaration);
    skClass:     Result := GetClassType(ATypeDeclaration);
    skRecord:    Result := GetRecordType(ATypeDeclaration);
    skEnum:      Result := GetEnumType(ATypeDeclaration);
    skset:       Result := GetSetType(ATypeDeclaration);
    skArray:     Result := GetArrayType(ATypeDeclaration);
  end;

  if VarName <> '' then
    ATypeDeclaration := VarName + ': ' + ATypeDeclaration;
  if (AnIndent <> 0) and not(tdfNoFirstLineIndent in AFlags) then
    ATypeDeclaration := GetIndent + ATypeDeclaration;
end;

{ TFpPascalPrettyPrinter }

function TFpPascalPrettyPrinter.InternalPrintValue(out APrintedValue: String;
  AValue: TFpDbgValue; AnAddressSize: Integer; AFlags: TFpPrettyPrintValueFlags;
  ANestLevel: Integer; AnIndent: String; ADisplayFormat: TWatchDisplayFormat;
  ARepeatCount: Integer; ADBGTypeInfo: PDBGType; AOptions: TFpPrettyPrintOptions): Boolean;


  function ResTypeName: String;
  begin
    if not((AValue.TypeInfo<> nil) and
           GetTypeName(Result, AValue.TypeInfo, []))
    then
      Result := '';
  end;
  function ResTypeName(AVal : TFpDbgValue): String;
  begin
    if not((AVal.TypeInfo<> nil) and
           GetTypeName(Result, AVal.TypeInfo, []))
    then
      Result := '';
  end;

  procedure DoPointer(AnAddress: boolean);
  var
    s: String;
    v: QWord;
  begin
    if ((ADisplayFormat = wdfDefault) and (ANestLevel=0)) or // default for unested: with typename
       (ADisplayFormat = wdfStructure)
    then
      s := ResTypeName
    else
      s := '';

    if AnAddress then
      v := AValue.Address.Address
    else
      v := AValue.AsCardinal;
    if (ppvCreateDbgType in AFlags) then begin
      ADBGTypeInfo^ := TDBGType.Create(skPointer, s);
      ADBGTypeInfo^.Value.AsPointer := Pointer(v);  // TODO: no cut off
    end;

    case ADisplayFormat of
      wdfDecimal, wdfUnsigned: APrintedValue := IntToStr(v);
      wdfHex: APrintedValue := '$'+IntToHex(v, AnAddressSize*2);
      else begin //wdfPointer/Default ;
          if v = 0 then
            APrintedValue := 'nil'
          else
            APrintedValue := '$'+IntToHex(v, AnAddressSize*2);
        end;
    end;

    if s <> '' then
      APrintedValue := s + '(' + APrintedValue + ')';

    if ADisplayFormat = wdfPointer then exit; // no data
    if svfString in AValue.FieldFlags then
      APrintedValue := APrintedValue + ' ' + AValue.AsString;

    Result := True;
  end;

  procedure DoInt;
  var
    n: Integer;
  begin
    case ADisplayFormat of
      wdfUnsigned: APrintedValue := IntToStr(QWord(AValue.AsInteger));
      wdfHex: begin
          if svfSize in AValue.FieldFlags then
            n := AValue.Size * 2
          else begin
            n := 16;
            if QWord(AValue.AsInteger) <= high(Cardinal) then n := 8;
            if QWord(AValue.AsInteger) <= high(Word) then n := 3;
            if QWord(AValue.AsInteger) <= high(Byte) then n := 2;
          end;
          APrintedValue := '$'+IntToHex(QWord(AValue.AsInteger), n);
        end;
      // TODO wdfChar:
      else
          APrintedValue := IntToStr(AValue.AsInteger);
    end;

    if (ppvCreateDbgType in AFlags) then begin
      ADBGTypeInfo^ := TDBGType.Create(skSimple, ResTypeName);
      //ADBGTypeInfo^.Value.As64Bits := QWord(AValue.AsInteger); // TODO: no cut off
    end;
    Result := True;
  end;

  procedure DoCardinal;
  var
    n: Integer;
  begin
    case ADisplayFormat of
      wdfDecimal: APrintedValue := IntToStr(Int64(AValue.AsCardinal));
      wdfHex: begin
          if svfSize in AValue.FieldFlags then
            n := AValue.Size * 2
          else begin
            n := 16;
            if AValue.AsCardinal <= high(Cardinal) then n := 8;
            if AValue.AsCardinal <= high(Word) then n := 4;
            if AValue.AsCardinal <= high(Byte) then n := 2;
          end;
          APrintedValue := '$'+IntToHex(AValue.AsCardinal, n);
        end;
      // TODO wdfChar:
      else
          APrintedValue := IntToStr(AValue.AsCardinal);
    end;

    if (ppvCreateDbgType in AFlags) then begin
      ADBGTypeInfo^ := TDBGType.Create(skSimple, ResTypeName);
      //ADBGTypeInfo^.Value.As64Bits := QWord(AValue.AsiCardinal); // TODO: no cut off
    end;
    Result := True;
  end;

  procedure DoBool;
  begin
    if AValue.AsBool then begin
      APrintedValue := 'True';
      if AValue.AsCardinal <> 1 then
        APrintedValue := APrintedValue + '(' + IntToStr(AValue.AsCardinal) + ')';
    end
    else
      APrintedValue := 'False';

    if (ppvCreateDbgType in AFlags) then begin
      ADBGTypeInfo^ := TDBGType.Create(skSimple, ResTypeName);
    end;
    Result := True;
  end;

  procedure DoChar;
  begin
    APrintedValue := '''' + AValue.AsString + ''''; // Todo escape
    if (ppvCreateDbgType in AFlags) then begin
      ADBGTypeInfo^ := TDBGType.Create(skSimple, ResTypeName);
    end;
    Result := True;
  end;

  procedure DoFloat;
  begin
    APrintedValue := FloatToStr(AValue.AsFloat);
    if (ppvCreateDbgType in AFlags) then begin
      ADBGTypeInfo^ := TDBGType.Create(skSimple, ResTypeName);
    end;
    Result := True;
  end;

  procedure DoEnum;
  var
    s: String;
  begin
    APrintedValue := AValue.AsString;
    if APrintedValue = '' then begin
      s := ResTypeName;
      APrintedValue := s + '(' + IntToStr(AValue.AsCardinal) + ')';
    end
    else if (ppvCreateDbgType in AFlags) then
      s := ResTypeName;

    if (ppvCreateDbgType in AFlags) then begin
      ADBGTypeInfo^ := TDBGType.Create(skEnum, s);
    end;
    Result := True;
  end;

  procedure DoEnumVal;
  begin
    APrintedValue := AValue.AsString;
    if APrintedValue <> '' then
      APrintedValue := APrintedValue + ':=';
    APrintedValue := APrintedValue+ IntToStr(AValue.AsCardinal);

    if (ppvCreateDbgType in AFlags) then begin
      ADBGTypeInfo^ := TDBGType.Create(skSimple, ResTypeName);
    end;
    Result := True;
  end;

  procedure DoSet;
  var
    s: String;
    i: Integer;
    m: TFpDbgValue;
  begin
    APrintedValue := '';
    for i := 0 to AValue.MemberCount-1 do begin
      m := AValue.Member[i];
      if svfIdentifier in m.FieldFlags then
        s := m.AsString
      else
      if svfOrdinal in m.FieldFlags then // set of byte
        s := IntToStr(m.AsCardinal)
      else
        Continue; // Todo: missing member
      if APrintedValue = ''
      then APrintedValue := s
      else APrintedValue := APrintedValue + ', ' + s;
    end;
    APrintedValue := '[' + APrintedValue + ']';

    if (ppvCreateDbgType in AFlags) then begin
      ADBGTypeInfo^ := TDBGType.Create(skSet, ResTypeName);
    end;
    Result := True;
  end;

  procedure DoStructure;
  var
    s, s2, MbName, MbVal: String;
    i: Integer;
    m: TFpDbgValue;
    fl: TFpPrettyPrintValueFlags;
    f: TDBGField;
    ti: TFpDbgSymbol;
    Cache: TFpDbgMemCacheBase;
  begin
    if (AValue.Kind = skClass) and (AValue.AsCardinal = 0) then begin
      APrintedValue := 'nil';
      if (ppvCreateDbgType in AFlags) then begin
        ADBGTypeInfo^ := TDBGType.Create(skSimple, ResTypeName);
      end;
      Result := True;
      exit;
    end;

    Cache := MemManager.CacheManager.AddCache(AValue.DataAddress.Address, AValue.DataSize);
    try

      if (ppvCreateDbgType in AFlags) then begin
        s := ResTypeName;
        case AValue.Kind of
          skRecord: ADBGTypeInfo^ := TDBGType.Create(skRecord, s);
          skObject: ADBGTypeInfo^ := TDBGType.Create(skClass, s);
          skClass:  ADBGTypeInfo^ := TDBGType.Create(skClass, s);
        end;
      end;

      if (ADisplayFormat = wdfPointer) or (ppoStackParam in AOptions) then begin
        if not (ppvCreateDbgType in AFlags) then
          s := ResTypeName;
        APrintedValue := '$'+IntToHex(AValue.AsCardinal, AnAddressSize*2);
        if s <> '' then
          APrintedValue := s + '(' + APrintedValue + ')';
        Result := True;
        if not (ppvCreateDbgType in AFlags) then
          exit;
      end
      else
      if ( (AValue.Kind in [skClass, skObject]) and (ppvSkipClassBody in AFlags) ) or
         ( (AValue.Kind in [skRecord]) and (ppvSkipRecordBody in AFlags) )
      then begin
        APrintedValue := ResTypeName;
        case AValue.Kind of
          skRecord: APrintedValue := '{record:}' + APrintedValue;
          skObject: APrintedValue := '{object:}' + APrintedValue;
          skClass:  APrintedValue := '{class:}' + APrintedValue + '(' + '$'+IntToHex(AValue.AsCardinal, AnAddressSize*2) + ')';
        end;
        Result := True;
        if not (ppvCreateDbgType in AFlags) then
          exit;
      end;

      s2 := LineEnding;
      if AFlags <> [] then s2 := ' ';;
      fl := [ppvSkipClassBody];
      //if ppvSkipClassBody in AFlags then
      //  fl := [ppvSkipClassBody, ppvSkipRecordBody];

      if (ppvCreateDbgType in AFlags) and (AValue.Kind in [skObject, skClass]) then begin
        ti := AValue.TypeInfo;
        if (ti <> nil) and (ti.TypeInfo <> nil) then
          ADBGTypeInfo^.Ancestor := ti.TypeInfo.Name;
      end;

      if not Result then
        APrintedValue := '';
      for i := 0 to AValue.MemberCount-1 do begin
        m := AValue.Member[i];
        if (m = nil) or (m.Kind in [skProcedure, skFunction]) then
          continue;
        s := '';
        InternalPrintValue(MbVal, m, AnAddressSize, fl, ANestLevel+1, AnIndent, ADisplayFormat, -1, nil, AOptions);
        if m.DbgSymbol <> nil then begin
          MbName := m.DbgSymbol.Name;
          s := MbName + ' = ' + MbVal;
        end
        else begin
          MbName := '';
          s := MbVal;
        end;

        if not Result then begin
          if APrintedValue = ''
          then APrintedValue := s
          else APrintedValue := APrintedValue + '; ' + s2 + s;
        end;
        if (ppvCreateDbgType in AFlags) then begin
          s := '';
          if m.ContextTypeInfo <> nil then s := m.ContextTypeInfo.Name;
          f := TDBGField.Create(MbName, TDBGType.Create(skSimple, ResTypeName(m)),
                                flPublic, [], s);
          f.DBGType.Value.AsString := MbVal;
          ADBGTypeInfo^.Fields.Add(f);
        end;
      end;
      if not Result then
        APrintedValue := '(' + APrintedValue + ')';
      Result := True;
    finally
      MemManager.CacheManager.RemoveCache(Cache)
    end;
  end;

  procedure DoArray;
  var
    s: String;
    i: Integer;
    m: TFpDbgValue;
    Cnt, FullCnt, d: Integer;
  begin
    APrintedValue := '';

    if (ppvCreateDbgType in AFlags) then begin
      ADBGTypeInfo^ := TDBGType.Create(skArray, ResTypeName);
    //ATypeInfo.Len;
    //ATypeInfo.BoundLow;
    //ATypeInfo.BoundHigh;
    end;

    Cnt := AValue.MemberCount;
    FullCnt := Cnt;
    if (Cnt = 0) and (svfOrdinal in AValue.FieldFlags) then begin  // dyn array
      APrintedValue := 'nil';
      Result := True;
      exit;
    end;
    if (ANestLevel > 2) then begin
      s := ResTypeName;
      APrintedValue := s+'({'+IntToStr(FullCnt)+' elements})'; // TODO len and addr (dyn array)
      Result := True;
      exit;
    end;
    If ARepeatCount > 0                     then Cnt := ARepeatCount
    else if (ANestLevel > 1) and (Cnt > 3)  then Cnt := 3
    else if (ANestLevel > 0) and (Cnt > 10) then Cnt := 10
    else if (Cnt > 300)                     then Cnt := 300;
    d := 0;
    // TODO: use valueobject for bounds
    if (AValue.IndexTypeCount > 0) and AValue.IndexType[0].HasBounds then
      d := AValue.IndexType[0].OrdLowBound;
    for i := d to d + Cnt - 1 do begin
      m := AValue.Member[i];
      if m <> nil then
        InternalPrintValue(s, m, AnAddressSize, AFlags * PV_FORWARD_FLAGS, ANestLevel+1, AnIndent, ADisplayFormat, -1, nil, AOptions)
      else
        s := '{error}';
      if APrintedValue = ''
      then APrintedValue := s
      else APrintedValue := APrintedValue + ', ' + s;
    end;
    if Cnt < FullCnt then
      APrintedValue := APrintedValue + ', {'+IntToStr(FullCnt-Cnt)+' more elements}';
    APrintedValue := '(' + APrintedValue + ')';
    Result := True;
  end;
var
  MemAddr: TFpDbgMemLocation;
  MemSize: Integer;
  MemDest: array of Byte;
  i: Integer;
begin
  if ADBGTypeInfo <> nil then ADBGTypeInfo^ := nil;
  if ANestLevel > 0 then begin
    AnIndent := AnIndent + '  ';
  end;

  if ADisplayFormat = wdfMemDump then begin
    if FMemManager <> nil then begin
      MemAddr := UnInitializedLoc;
      if svfDataAddress in AValue.FieldFlags then begin
        MemAddr := AValue.DataAddress;
        MemSize := AValue.DataSize;
      end
      else
      if svfAddress in AValue.FieldFlags then begin
        MemAddr := AValue.Address;
        MemSize := AValue.Size;
      end;
      if MemSize < ARepeatCount then MemSize := ARepeatCount;
      if MemSize <= 0 then MemSize := 256;

      if IsTargetAddr(MemAddr) then begin
        SetLength(MemDest, MemSize);
        if FMemManager.ReadMemory(MemAddr, MemSize, @MemDest[0]) then begin
          APrintedValue := IntToHex(MemAddr.Address, AnAddressSize*2)+ ':' + LineEnding;
          for i := 0 to high(MemDest) do begin
            if (i > 0) and (i mod 16 = 0) then
              APrintedValue := APrintedValue + LineEnding
            else
            if (i > 0) and (i mod 8 = 0) then
              APrintedValue := APrintedValue + '  '
            else
            if (i > 0)  then
              APrintedValue := APrintedValue + ' ';
            APrintedValue := APrintedValue + IntToHex(MemDest[i], 2);
          end;
        end
        else begin
          APrintedValue := 'Cannot read memory at address '+ IntToHex(MemAddr.Address, AnAddressSize*2);
        end;
        exit;
      end;
    end;

    APrintedValue := 'Cannot read memory for expression';
    exit
  end;

  Result := False;
  case AValue.Kind of
    skUnit: ;
    skProcedure: ;
    skFunction: ;
    skPointer:   DoPointer(False);
    skInteger:   DoInt;
    skCardinal:  DoCardinal;
    skBoolean:   DoBool;
    skChar:      DoChar;
    skFloat:     DoFloat;
    skString: ;
    skAnsiString: ;
    skCurrency: ;
    skVariant: ;
    skWideString: ;
    skEnum:      DoEnum;
    skEnumValue: DoEnumVal;
    skSet:       DoSet;
    skRecord:    DoStructure;
    skObject:    DoStructure;
    skClass:     DoStructure;
    skInterface: ;
    skArray:     DoArray;
    skNone:      DoPointer(true);
  end;

  if (ADBGTypeInfo <> nil) and (ADBGTypeInfo^ <> nil) then
    ADBGTypeInfo^.Value.AsString := APrintedValue;

  if IsError(AValue.LastError) then
    APrintedValue := ErrorHandler.ErrorAsString(AValue.LastError);
end;

constructor TFpPascalPrettyPrinter.Create(AnAddressSize: Integer);
begin
  FAddressSize := AnAddressSize;
end;

function TFpPascalPrettyPrinter.PrintValue(out APrintedValue: String; AValue: TFpDbgValue;
  ADisplayFormat: TWatchDisplayFormat; ARepeatCount: Integer;
  AOptions: TFpPrettyPrintOptions): Boolean;
begin
  Result := InternalPrintValue(APrintedValue, AValue,
                               AddressSize, [], 0, '', ADisplayFormat, ARepeatCount, nil, AOptions);
end;

function TFpPascalPrettyPrinter.PrintValue(out APrintedValue: String; out
  ADBGTypeInfo: TDBGType; AValue: TFpDbgValue; ADisplayFormat: TWatchDisplayFormat;
  ARepeatCount: Integer): Boolean;
begin
  Result := InternalPrintValue(APrintedValue, AValue,
                               AddressSize, [ppvCreateDbgType], 0, '',
                               ADisplayFormat, ARepeatCount, @ADBGTypeInfo);
end;

end.

