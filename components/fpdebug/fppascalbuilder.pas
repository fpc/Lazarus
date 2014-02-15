unit FpPascalBuilder;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DbgIntfBaseTypes, FpDbgInfo;

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

function GetTypeName(out ATypeName: String; ADbgSymbol: TDbgSymbol; AFlags: TTypeNameFlags = []): Boolean;
function GetTypeAsDeclaration(out ATypeDeclaration: String; ADbgSymbol: TDbgSymbol;
  AFlags: TTypeDeclarationFlags = []; AnIndent: Integer = 0): Boolean;

implementation

function GetTypeName(out ATypeName: String; ADbgSymbol: TDbgSymbol;
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

function GetTypeAsDeclaration(out ATypeDeclaration: String; ADbgSymbol: TDbgSymbol;
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
    m: TDbgSymbol;
    s: String;
  begin
    Result := True;
    AText := '';
    c := ADbgSymbol.MemberCount;
    i := 0;
    while (i < c) and Result do begin
      m := ADbgSymbol.Member[i];
      AddVisibility(m.MemberVisibility, i= 0);
      Result := GetTypeAsDeclaration(s, m, [tdfIncludeVarName] + AFlags, AnIndent + 4);
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
    while ADbgSymbol.Kind = skPointer do begin
      ADbgSymbol := ADbgSymbol.TypeInfo;
      s := s + '^';
    end;
    if not(tdfStopAfterPointer in AFlags) then begin
      Result := GetTypeAsDeclaration(ADeclaration, ADbgSymbol, AFlags);
      if not Result then
        Result := GetTypeName(ADeclaration, ADbgSymbol, []);
    end
    else begin
      Result := GetTypeName(ADeclaration, ADbgSymbol, []);
      if not Result then
        Result := GetTypeAsDeclaration(ADeclaration, ADbgSymbol, AFlags);
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
    m: TDbgSymbol;
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
    t: TDbgSymbol;
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
    t: TDbgSymbol;
    s: String;
    i: Integer;
  begin
    // TODO assigned value (a,b:=3,...)
    t := ADbgSymbol.TypeInfo;
    Result := (t <> nil);
    if not Result then exit;

    s := t.Name;
    if s = '' then begin
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


end.

