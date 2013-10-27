unit FpPascalBuilder;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FpDbgClasses;

type
  TTypeNameFlag = (
    tnfOnlyDeclared,    // do not return a substitute with ^ symbol
    tnfIncludeOneRef    // If it is a pointer, and the pointed-to name is known, return ^TXxx
                        //   without tnfOnlyDeclared, may return ^^^TXxx if needed

  );
  TTypeNameFlags = set of TTypeNameFlag;

  TTypeDeclarationFlag = (
    tdfIncludeVarName,   // like i: Integer
    tdfSkipClassBody     // shorten class
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
    Result := GetTypeName(ADeclaration, ADbgSymbol, []);
// TODO brackets
    ADeclaration := s + ADeclaration;
  end;

  function GetBaseType(out ADeclaration: String): Boolean;
  begin
    Result := GetTypeName(ADeclaration, ADbgSymbol, []);
  end;

  function GetFunctionType(out ADeclaration: String): Boolean;
  var
    s: String;
  begin
    // Todo param
    GetTypeAsDeclaration(s, ADbgSymbol.TypeInfo);
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
    GetTypeName(s2, ADbgSymbol.TypeInfo);
    if Result then
      ADeclaration := Format('class(%s)%s%s%send%s',
                             [s2, LineEnding, s, LineEnding, GetIndent]);
  end;

  function GetRecordType(out ADeclaration: String): Boolean;
  var
    s: String;
  begin
    Result := MembersAsGdbText(s, True);
    if Result then
      ADeclaration := Format('record%s%s%send%s', [LineEnding, s, LineEnding, GetIndent]);
  end;

  function GetEnumType(out ADeclaration: String): Boolean;
  var
    i: Integer;
    m: TDbgSymbol;
  begin
    // TODO assigned value (a,b:=3,...)
    Result := True;
    ADeclaration := '(';
    for i := 0 to ADbgSymbol.MemberCount - 1 do begin
      m := ADbgSymbol.Member[i];
      if i > 0 then ADeclaration := ADeclaration + ', ';
      ADeclaration := ADeclaration + m.Name;
    end;
    ADeclaration := ADeclaration + ')'
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
  end;

  if VarName <> '' then
    ATypeDeclaration := VarName + ': ' + ATypeDeclaration;
  if AnIndent <> 0 then
    ATypeDeclaration := GetIndent + ATypeDeclaration;
end;


end.

