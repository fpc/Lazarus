{ * This file is part of ObjCParser tool
  * Copyright (C) 2008-2009 by Dmitry Boyarintsev under the GNU LGPL
  * license version 2.0 or 2.1.  You should have received a copy of the
  * LGPL license along with at http://www.gnu.org/
}

unit ObjCParserUtils;

interface

{$ifdef fpc}
  {$mode delphi}{$H+}
{$else}
  {$warn unsafe_code off}
  {$warn unsafe_type off}
  {$warn unsafe_cast off}
{$endif}

uses
  Classes, SysUtils, ObjCParserTypes, ObjCToPas;

type
  { TConvertSettings }
  //todo: hash table
  TReplace = class(TObject)
    Src : AnsiString;
    Dst : AnsiString;
  end;

  TReplaceItem = class(TObject)
    ReplaceStr : AnsiString;
  end;

  TReplaceList = class(TObject)
  private
    fItems  : TStringList;
  protected
    function GetReplace(const ARepl: AnsiString): AnsiString;
    procedure SetReplace(const ARepl, AValue: AnsiString);

    function GetCaseSense: Boolean;
    procedure SetCaseSense(AValue: Boolean);
  public
    constructor Create;
    destructor Destroy; override;
    procedure GetReplaces(strings: TStrings);
    property Replace[const s: AnsiString]: AnsiString read GetReplace write SetReplace; default;
    property CaseSensetive: Boolean read GetCaseSense write SetCaseSense;
  end;

  TConvertSettings = class(TObject)
  private
    fCallConv     : AnsiString;
    fExternPrefix : AnsiString;
  protected
    procedure SetCallConv(const ACallConv: String);
  public
    IgnoreIncludes  : TStringList;
    DefineReplace   : TReplaceList;
    TypeDefReplace  : TReplaceList; // replaces for C types
    PtrTypeReplace  : TReplaceList; // replaces for C types pointers
    TokenReplace    : TReplaceList;

    //IgnoreTokens    : TStringList; //todo: Remote. Use TokenReplace instead

    ConvertPrefix   : TStringList;

    FloatTypes      : TStringList;
    StructTypes     : TStringList;
    ObjCClassTypes  : TStringList;

    CustomTypes     : TStringList;

    ObjcIDReplace    : AnsiString; // = 'objc.id';
    fExternVarPrefix : AnsiString; // always '_'?

    UseRefClassType  : Boolean;
    RefClassPostfix  : AnsiString;


    constructor Create;
    destructor Destroy; override;

    procedure AssignNewTypeName(const AName, TypeDefStr: AnsiString; var NewTypeName: AnsiString);
    function GetCallConv(withSemiColon: Boolean = true): AnsiString;

    function GetObjCClassReplaceName(const ObjCName: AnsiString): AnsiString;

    property CallConv: AnsiString read fCallConv write SetCallConv;
    property ExternFuncPrefix: AnsiString read fExternPrefix; // external function name prefix
    property ExternVarPrefix: AnsiString read fExternVarPrefix; // external function var prefix

  end;

var
  ConvertSettings : TConvertSettings;

type
  TObjcConvertVarType = (vt_Int, vt_FloatPoint, vt_Struct, vt_Object);

procedure WriteOutIncludeFile(hdr: TObjCHeader; st: TStrings);
procedure WriteOutMainFramework(hdr: TObjCHeader; st: TStrings);

function ObjCToDelphiType(const objcType: AnsiString; isPointer: Boolean): AnsiString;
function ObjCResultToDelphiType(Res: TObjCResultTypeDef) : AnsiString;
function CToDelphiFuncType(AFuncType: TFunctionTypeDef): AnsiString;

function StrFromFile(const FileName: AnsiString): AnsiString;

function IsMethodConstructor(cl: TClassDef; m: TClassMethodDef): Boolean;
function GetMethodStr(cl: TClassDef; m: TClassMethodDef; ForImplementation: Boolean): AnsiString;
function GetProcFuncHead(const FuncName, OfClass, Params, ResType: AnsiString; const FuncDest: AnsiString = ''): AnsiString;
function GetMethodParams(const m: TClassMethodDef; NamesOnly: Boolean): AnsiString;
function GetMethodResultType(const m: TClassMethodDef): AnsiString;

function IsPascalFloatType(const TypeName: AnsiString): Boolean;

function GetObjCVarType(const TypeName: AnsiString):TObjcConvertVarType; //): Boolean; = (vt_Int, vt_FloatPoint, vt_Struct, vt_Object);

implementation

function GetterSetterName(const PropName: AnsiString; etterName: AnsiString; isSetter: Boolean): AnsiString;
begin
  if etterName = '' then begin
    if isSetter then  Result := 'set'+PropName
    else Result := 'get'+PropName;
  end else
    Result := etterName;
end;


procedure WriteOutRecordField(AField: TStructField; const Prefix: AnsiString; subs: TStrings); forward;
procedure WriteOutRecord(struct: TEntityStruct; const Prefix, RecPrefix : AnsiString; subs: TStrings); forward;

function GetObjCVarType(const TypeName: AnsiString):TObjcConvertVarType;
begin
  Result := vt_Int;
  if IsPascalFloatType(TypeName) then begin
    Result := vt_FloatPoint;
    Exit;
  end;

  if ConvertSettings.FloatTypes.IndexOf(TypeName) >= 0 then
    Result := vt_FloatPoint
  else if ConvertSettings.StructTypes.IndexOf(TypeName) >= 0 then
    Result := vt_Struct
  else if ConvertSettings.ObjCClassTypes.IndexOf(TypeName) >= 0 then
    Result := vt_Object;
end;

function IsPascalFloatType(const TypeName: AnsiString): Boolean;
var
  nm  : AnsiString;
begin
  Result := false;
  if TypeName = '' then Exit;
  case TypeName[1] of
    'd','D','f','F': begin
      nm := AnsiLowerCase(typeName);
      Result := (nm = 'double') or (TypeName = 'float');
    end;
  end;
end;


function FixIfReserved(const AName: AnsiString; NotUse: TStrings = nil): AnsiString;
begin
  Result := AName;
  if isPascalReserved(AName) then
    Result := '_'+AName;
  if Assigned(NotUse) then begin
    while (NotUse.IndexOf(Result) >= 0) do
      Result := '_' + Result;
  end;
end;

function GetMethodResultType(const m: TClassMethodDef): AnsiString;
var
  res : TObjCResultTypeDef;
  tp  : TTypeDef;
begin
  res := m.GetResultType;
  if not Assigned(res) then Result := ''
  else begin

    if m.GetResultType._Type is TTypeDef then begin
      tp := TTypeDef(m.GetResultType._Type);
      Result := ObjCToDelphiType(tp._Name, tp._IsPointer);
    end else begin
      ConvertSettings.AssignNewTypeName('', CToDelphiFuncType(TFunctionTypeDef(m.GetResultType._Type)), Result);
    end;
  end;
end;

function GetMethodParams(const m: TClassMethodDef; NamesOnly: Boolean): AnsiString;
var
  i     : Integer;
  p     : TObject;
  vname : AnsiString;
  vtype : AnsiString;

  tp    : TTypeDef;
  prc   : AnsiString;
  
begin
  Result := '';
  vname := '';
  vtype := '';
  for i := 0 to m.Items.Count - 1 do begin
    p := TObject(m.Items[i]);

    if p is TParamDescr then begin
      if vname = '' then vname := TParamDescr(p)._Descr

    end else if p is TObjCParameterDef then begin
      vname := TObjCParameterDef(p)._Name;

      if (TObjCParameterDef(p)._Type._Type) is TTypeDef then begin
        tp := TTypeDef(TObjCParameterDef(p)._Type._Type);
        vtype := ObjCToDelphiType(tp._Name, tp._IsPointer);
      end else begin
        prc := 'TProc' + TClassDef(m.Owner)._ClassName + TObjCParameterDef(p)._Name + IntToStr(ConvertSettings.CustomTypes.Count);
        ConvertSettings.AssignNewTypeName(prc, CToDelphiFuncType(TFunctionTypeDef(TObjCParameterDef(p)._Type._Type)), vtype);
        tp := TTypeDef.Create(TObjCParameterDef(p)._Type);
        tp._Name := vtype;
        TObjCParameterDef(p)._Type._Type.Free; // replace function type with typename
        TObjCParameterDef(p)._Type._Type := tp;
      end;
      {if IsPascalReserved(vname) then }
      vname := '_'+vname;
      
      if not NamesOnly then begin
        if Result <> '' then Result := Result + '; ';
        if Copy(vtype, 1, 5) = 'array' then Result := Result + 'const '+vname + ': ' + vtype
        else Result := Result + ''+vname + ': ' + vtype;
      end else begin
        if Result = '' then Result := vname
        else Result := Result + ', ' + vname;
      end;
      vname := '';
    end;
  end;

end;

function GetProcFuncHead(const FuncName, OfClass, Params, ResType, FuncDest: AnsiString): AnsiString;
begin
  if FuncDest = '' then begin
    if ResType = '' then Result := 'procedure '
    else Result := 'function ';
  end else
    Result := FuncDest + ' ';

  if OfClass <> '' then Result := Result + OfClass+'.';
  Result := Result + FuncName;
  if Params <> '' then
    Result := Result + '('+Params+')';
  if ResType <> '' then Result := Result+': '+ResType;
  Result := Result + ';';
end;

function StrFromFile(const FileName: AnsiString): AnsiString;
var
  fs  : TFileStream;
begin
  fs := TFileStream.Create(FileName, fmOpenRead or fmShareDenyNone);
  try
    SetLength(Result, fs.Size);
    fs.Read(Result[1], fs.Size);
  finally
    fs.Free;
  end;
end;

function ObjCToDelphiType(const objcType: AnsiString; isPointer: Boolean): AnsiString;
var
  l : AnsiString;
  r : AnsiString;
begin
  Result := objcType;
  l := AnsiLowerCase(objcType);
  if l = '' then Exit;
  case l[1] of
    'v':
      if l = 'void' then begin
        if not isPointer then Result := ''
        else begin
          Result := 'Pointer';
          Exit;
        end;
      end;
    'i':
      if l = 'id' then Result := ConvertSettings.ObjCIDReplace
      else if l = 'int' then Result := 'Integer';
    'b':
      if l = 'bool' then Result := 'LongBool';
    'l':
      if l = 'long' then Result := 'Integer';
    's':
      if l = 'short' then Result := 'SmallInt';
    'u':
      if (l = 'unsigned long') or (l = 'usigned int') then
        Result := 'LongWord'
      else if (l = 'unsigned short') then
        Result := 'Word';
    'f':
      if l = 'float' then Result := 'Single';
  end;


  if Result = objcType then begin
    if isPointer then r := ConvertSettings.PtrTypeReplace[objcType]
    else r := ConvertSettings.TypeDefReplace[objcType];
    if r <> '' then
      Result := r;
  end;

  if isPointer then begin
    if ((objctype = 'char') or (objctype = 'const char')) then
      Result := 'PChar'
  end;
end;

function ObjCResultToDelphiType(Res: TObjCResultTypeDef) : AnsiString;
begin
  if Res._Type is TTypeDef then
    Result := ObjCToDelphiType( TTypeDef(Res._Type)._Name, TTypeDef(Res._Type)._IsPointer)
  else begin
  end;
end;

function IsMethodConstructor(cl: TClassDef; m: TClassMethodDef): Boolean;
var
  res : TObjCResultTypeDef;
  l   : AnsiString;
begin
  Result := m._IsClassMethod;
  if not Result then begin
    //todo: C is case sensetive, so is it possible to have a initialing function name like
    //      'InitWithSomething', rather than 'initWithSomething' (that is should be)???
    //todo: to make sure, it's not a name,like 'Initialzation';
    l := AnsiLowerCase(m._Name);
    if Pos('init', l) = 1 then Result := true;
  end;
  if not Result then Exit;

  res := m.GetResultType;
  if res._Type is TTypeDef then
    l := TTypeDef(res._Type)._Name
  else
    l := '!!!todo function';
  Result := (l = 'id') or (l = cl._ClassName);
end;

function GetMethodStr(cl: TClassDef; m: TClassMethodDef; ForImplementation: Boolean): AnsiString;
var
//  i     : integer;
  nm    : AnsiString;
  ft    : AnsiString;
  res   : AnsiString;
begin
  res := GetMethodResultType(m);
  if IsMethodConstructor(cl, m) then begin
    ft := 'constructor';
    res := '';
  end else
    ft := '';

  nm := m._Name;
  if ForImplementation
    then Result := GetProcFuncHead(nm, cl._ClassName, GetMethodParams(m, false), res, ft)
    else Result := GetProcFuncHead(nm, '', GetMethodParams(m, false), res, ft);

  if ft = '' then
    if m._IsClassMethod then
      Result := 'class ' + Result;
end;

// returns define pas file name form Objective C name, like
// NSApplication.h  ->  NSAPPLICATION_PAS_H
// SomePath/SomePath/SomeFileName.h -> SOMEFILENAME_PAS_H
function GetIfDefFileName(const FileName, DefExt: AnsiString): AnsiString;
var
  i : integer;
//  s : AnsiString;
begin
  //todo: don't like it...
  Result := Copy(FileName, 1, length(FileName) - length(ExtractFileExt(FileName)));
  Result := AnsiUpperCase(Result);
  for i := 1 to length(Result) do
    if Result[i] = '.' then
      Result[i] := '_';
  Result := Result + '_PAS_'+DefExt;
end;

// returns include pas file name form Objective C name, like
// <AppKit/NSApplication.h>  ->  NSApplication.inc
// "SomePath/SomePath/SomeFileName.h> -> SomeFileName.h
function GetIncludeFile(const s: AnsiString): AnsiString;
var
  i   : Integer;
  vs  : AnsiString;
  pth : AnsiString;
begin
  //todo: still, i don't like it...
  Result :='';
  i := 1;
  ScanWhile(s, i, InvsChars);
  vs := Copy(s, i, length(s) - i + 1); 
  if vs = '' then Exit;

  
  if (vs[1] = '<') or (vs[1] = '"') then vs := Copy(vs, 2, length(vs) - 1);
  if vs = '' then Exit;
  
  i := length(vs);
  if (vs[i] = '>') or (vs[i] = '"') then vs := Copy(vs, 1, length(vs) - 1);
  if vs = '' then Exit;

  pth := vs;

  while (pth <> '') and (length(pth)>1) do begin
    if ConvertSettings.IgnoreIncludes.IndexOf(pth) >= 0 then
      Exit; // file must be excluded;
    pth := ExtractFilePath(ExcludeTrailingPathDelimiter(pth));
  end;

  Result := ExtractFileName(vs);
  Result := Copy(Result, 1, length(Result) - length(ExtractFileExt(vs))) + '.inc';
(*
  Result := '';
  if s = '' then Exit;
//  i := length(s);
{  if (s[i] = '"') or (s[i] = '>') then
    dec(i);}
  i := length(s) - 1;
  // dummy, but it works =)
  while (i > 0) and (s[i] in ['.', 'A'..'Z', 'a'..'z', '0'..'9']) do dec(i);

  Result := Copy(s, i + 1, length(s) - i);*)
end;

// returns pascal style of precomiler "if defined" section
// exclusion is done for Cocoa known precompiler definion, for ex:
// MAC_OS_X_VERSION_MAX_ALLOWED >= MAC_OS_X_VERSION_10_3 -> MAC_OS_X_VERSION_10_3
// any other #ifdef excpresions would be passed "as is" even if are incorrect
// for pascal
function PrecompileIfDefToPascal(const prm: AnsiString; var isDef: Boolean): AnsiString;
var
  i   : Integer;
  vs  : AnsiString;
begin
  i := 1;
  ScanWhile(prm, i, InvsChars);
  if prm[i] = '!' then begin
    isDef := false;
    inc(i);
    ScanWhile(prm, i, InvsChars);
  end else
    isDef :=true;
  vs := Copy(prm, i, length(prm) - i + 1);

  // really slow... and... don't like this anyway!
  vs := ConvertSettings.DefineReplace[vs];
  if vs <> ''
    then Result := vs 
    else Result := prm;
{  for i := 0 to ConvertSettings.DefineReplace.C
  Result := prm;
  i := Pos(VerExclude, prm);
  if i > 0 then begin
    i := i + length(VerExclude);
    while (i <= length(Result)) and (Result[i] = ' ') do inc(i);
    if i <= length(Result) then
      Result := Copy(prm, i, length(Result) - i + 1);
  end;}
end;

// converts TProcpmiler entity to pascal entity
// #import or #include -> {$Include Something.inc}
// #define SOMETHING -> {$define SOMETHING}
// #ifdef SOMETHING -> {$ifdef SOMETHING}
// etc...
function WriteOutPrecompToPascal(Prec: TPrecompiler): AnsiString;
var
  dir : AnsiString;
  prm : AnsiString;
  isdef : Boolean;
const
  isdefConst : array [Boolean] of AnsiString = ('ifndef', 'ifdef');
begin
  dir := AnsiLowerCase(Prec._Directive);
  if (dir = '#import') or (dir = '#include') then begin

    prm := GetIncludeFile(Prec._Params);
    if (prm <> '') and (prm <> ' .inc') then
      Result := Format('{$include %s}', [prm]);

  end else if (dir = '#if') then begin
    prm := PrecompileIfDefToPascal(Prec._Params, isdef);
    Result := Format('{.$%s %s}', [isdefConst[isdef], prm]);
  end else if (dir = '#else') then
    Result := '{.$else}'
  else if (dir = '#endif') then
    Result := '{.$endif}';
end;

procedure WriteOutCommentStr(const AComment, Prefix: AnsiString; Subs: TStrings);
var
  i     : Integer;
  j     : Integer;
  k     : Integer;
  cmtln : AnsiString;
begin
  i := 1;
  while i <= length(AComment) do begin
    // scan for multylined comments
    cmtln := ScanTo(AComment, i, EoLnChars);
    i := SkipEndOfLineChars(AComment, i);

    // break long comments into lines
    j := 1;
    while j <= length(cmtln) do begin
      k := j;
      inc(j, 80);
      if j > length(cmtln) then j := length(cmtln);
      ScanTo(cmtln, j, WhiteSpaceChars);
      subs.Add(Prefix + '// ' + Copy(cmtln, k, j - k));
      inc(j);
    end;
  end;
end;

procedure WriteOutIfComment(Items: TList; Index: Integer; const Prefix: AnsiString; Subs: TStrings);
var
  j   : integer;
begin
  if (Index < 0) or (Index >= Items.Count) then Exit;

  j := Index;
  while (j >= 0) and (TObject(Items[j]) is TComment) do dec(j);
  inc(j);
  for j := j to index do
  //if TObject(Items[Index]) is TComment then
    WriteOutCommentStr( TComment(Items[j])._Comment, Prefix, Subs);
end;

// clears empty precompile statements, like
// {$ifdef SOMETHING}
// {$endif}
// and
// {$ifdef SOMETHING}
// {$else}
// {$endif}
// will be removed
procedure ClearEmptyPrecompile(subs: TStrings);
var
  i   : integer;
  j   : Integer;
begin
  // don't like it either...
  i := subs.Count - 1; if i < 0 then Exit;
  j := i;
  
  if Pos('{$endif', subs[i]) = 0 then Exit;
  dec(i); if i < 0 then Exit;
  
  if Pos('{$else', subs[i]) > 0 then
    dec(i); if i < 0 then Exit;
  
  if Pos('{$ifdef', subs[i]) > 0 then
    for i := j downto i do
      subs.Delete(i);
end;

procedure BeginSection(const SectionName: AnsiString; st: TStrings);
begin
  st.Add('');
  st.Add('{$ifdef '+SectionName+'}');
end;

procedure BeginExcludeSection(const DefineName: AnsiString; st: TStrings);
begin
  st.Add('{$ifndef '+DefineName+'}');
  st.Add('{$define '+DefineName+'}');
  st.Add('');
end;

procedure EndSection(st: TStrings);
begin
  st.Add('{$endif}');
end;

// todo: remove Prefix param...
procedure WriteOutIfDefPrecompiler(prec: TPrecompiler; const Prefix: AnsiString; subs: TStrings);
var
  ppas  : AnsiString;
  isend : Boolean;
begin
  ppas := WriteOutPrecompToPascal(prec);
  isend := IsSubStr('{.$endif', ppas, 1);
  if isend or IsSubStr('{.$ifndef', ppas, 1) or IsSubStr('{.$ifdef', ppas, 1) or IsSubStr('{.$else', ppas, 1) then
    subs.Add(Prefix + ppas);
  if isend then ClearEmptyPrecompile(subs);
end;

function GetClassConst(const ClassName, ConstName: AnsiString): AnsiString;
begin
  Result := Format('Str%s_%s', [ClassName, ConstName]);
end;


function GetMethodConstName(mtd: TClassMethodDef): AnsiString;
var
  i   : Integer;
  obj : TObject;
begin
  Result := mtd._Name;
  for i := 0 to mtd.Items.Count - 1 do begin
    obj := mtd.Items[i];
    if not Assigned(obj) then Continue;
    if obj is TParamDescr then
      Result := Result + TParamDescr(obj)._Descr
    else if obj is TObjCParameterDef then
      Result := Result + ':';
  end;
end;


procedure WriteOutClassToConsts(cl : TClassDef; subs, conststr: TStrings);
var
  i   : Integer;
//  j   : Integer;
  s   : AnsiString;
  ss  : AnsiString;
  mtd : TClassMethodDef;
  obj : TObject;
  cs  : AnsiString;
  objcname  : AnsiString;
begin
  cs := GetClassConst(cl._ClassName, cl._ClassName);
  if conststr.IndexOf(cs) < 0 then begin
    conststr.Add(cs);
    s := Format('  %s = ''%s'';', [cs, cl._ClassName]);
    subs.Add(s);
  end;

  for i := 0 to cl.Items.Count - 1 do begin
    obj := TObject(cl.Items[i]);
    if obj is TClassMethodDef then begin
      mtd := TClassMethodDef(cl.Items[i]);

      objcName := GetMethodConstName(mtd);
      mtd._Name := ObjCToPasMethodName(mtd);
      cs := GetClassConst(cl._ClassName, mtd._Name);
      if conststr.IndexOf(cs) < 0 then begin
        conststr.Add(cs);
        ss := Format('  %s = ''%s'';', [cs, objcname]);
        subs.add(ss);
      end;

    end;
  end; {of for}
  subs.Add('');
end;

procedure ParseDefine(const s: AnsiString; var DefWhat, DefTo: AnsiString);
var
  i   : Integer;
begin
  i := 1;
  ScanWhile(s, i, WhiteSpaceChars);
  if i < length(s) then begin
    DefWhat := ScanTo(s, i, WhiteSpaceChars);
    ScanWhile(s, i, InvsChars);
    DefTo := Copy(s, i, length(s) - i + 1);
  end else
    DefTo := '';
end;

procedure WriteOutPrecompDefine(const Prec: TPrecompiler; Prefix: AnsiString; st: TStrings; var IsConstant: Boolean);
var
  a, b: AnsiString;
begin
   IsConstant:=false;
  if Prec._Directive = '#define' then begin
    ParseDefine(Prec._Params, a, b);
    if b <> ''  then begin
      st.Add(Prefix + Format('%s = %s;', [a, b]));
      IsConstant:=True;
    end else 
      st.Add(Prefix + Format('{$define %s}', [a]));
  end;
end;

procedure WriteOutPrecompInclude(Prec: TPrecompiler; st: TStrings);
var
  dlph  : AnsiString;
begin
  dlph := WriteOutPrecompToPascal(Prec);
  if IsSubStr('{$include', dlph, 1) then
    st.Add(dlph);
end;

function GetPascalEnumValue(const Name, Param: AnsiString): AnsiString;
begin
  Result := Name;
  if Param <> '' then
    Result := Result + ' = ' + Param
end;


function ReplaceStr(const sub, subrep, s: AnsiString): AnsiString;
var
  i   : Integer;
  j   : Integer;
begin
  i := Pos(sub, s);
  if i = 0 then begin
    Result := s;
    Exit;
  end;
  j := i + length(sub);
  Result := Copy(s, 1, i - 1) + subrep + Copy(s, j, length(s) - j + 1);
end;

function GetPascalConstValue(const Vl: AnsiString): AnsiString;
var
  ws  : AnsiString;
begin
  Result := Vl;
  //todo: improve! check at h2pas
  repeat ws := Result; Result := ReplaceStr('<<', 'shl', ws); until Result = ws;
  repeat ws := Result; Result := ReplaceStr('>>', 'shr', ws); until Result = ws;
  repeat ws := Result; Result := ReplaceStr('||', 'or',  ws); until Result = ws;
  repeat ws := Result; Result := ReplaceStr('|',  'or',  ws); until Result = ws;
  repeat ws := Result; Result := ReplaceStr('&&', 'and', ws); until Result = ws;
  repeat ws := Result; Result := ReplaceStr('&',  'and', ws); until Result = ws;
end;

procedure WriteOutEnumValues(enm: TEnumTypeDef; const Prefix: AnsiString; st: TStrings);
var
  vl  : TEnumValue;
  s   : AnsiString;
  i   : Integer;
  j   : Integer;
begin
  j := st.Count;
  for i := 0 to enm.Items.Count - 1 do begin
    if TObject(enm.Items[i]) is TEnumValue then begin
      vl := TEnumValue(enm.Items[i]);
      if st.Count > j then st[st.Count-1]:=st[st.Count-1]+', ';
      s := GetPascalEnumValue(vl._Name, GetPascalConstValue(vl._Value));
      s := Prefix + s;
      st.Add(s);
    end;
  end;
end;

function Min(a, b: Integer): Integer;
begin
  if a < b then Result := a
  else Result := b;
end;

procedure MatchFixes(const Name: AnsiString; var prefix, postfix: AnsiString);
var
  i     : integer;
  ni, pi: integer;
//  nc, pc: AnsiChar;
begin
  for i := 1 to Min(length(Name), length(prefix)) do
    if Name[i] <> prefix[i] then begin
      prefix := Copy(prefix, 1, i - 1);
      Break;
    end;
    
  ni := length(Name);
  pi := length(postfix);
  for i := 1 to Min(length(Name), length(postfix)) do begin
    if Name[ni] <> postfix[pi] then begin // this cause a bug
      postfix := Copy(Name, ni + 1, length(Name) - ni);
      Break;
    end;
    dec(ni);
    dec(pi);
  end;
end;

function EvaluateEnumName(enm: TEnumTypeDef): AnsiString;
var
  prefix  : AnsiString;
  postfix : AnsiSTring;
  vl      : TEnumValue;
  known   : integer;
  i       : Integer;
begin
  known := 0;
  Result := '';
  for i := 0 to enm.Items.Count - 1 do begin
    if TObject(enm.Items[i]) is TEnumValue then begin
      vl := TEnumValue(enm.Items[i]);
      if known = 0 then begin
        prefix := vl._Name;
        postfix := vl._Name;
      end else
        MatchFixes(vl._Name, prefix, postfix);
      inc(known)
    end;
  end;
  if (known <= 1) or (length(Result) < 3) then Result := 'todoEnumName' // if only one enumaration or none, name cannot be defined...
  else Result := prefix + postfix;
end;

procedure WriteOutVariableToHeader(v: TVariable; const SpacePrefix: String; Vars: TStrings);
var
  tp  : TTypeDef;
  s   : AnsiString;
  vartype : AnsiString;
begin
  tp := TTypeDef(v._Type);
  vartype := ObjCToDelphiType(tp._Name, tp._IsPointer);
  s := Format('%s : %s; external name ''%s%s''; ', [v._Name, vartype, ConvertSettings.ExternVarPrefix, v._Name] );
  Vars.Add(SpacePrefix + s);
end;

function CParamsListToPascalStr(Params: TFunctionParamsList): AnsiString;
var
  i   : integer;
  num : Integer;
  prm : TFunctionParam;
  vs  : AnsiString;
begin
  Result := '';
  num := 1;
  for i := 0 to Params.Items.Count - 1 do
    if TObject(Params.Items[i]) is TFunctionParam then begin
      prm := TFunctionParam(Params.Items[i]);
      if prm._IsAny then Continue;
      vs := ObjCToDelphiType( GetTypeNameFromEntity(prm._Type), IsTypeDefIsPointer(prm._Type));
      if prm._Name = ''
        then vs := '_param'+IntToStr(num) + ': ' + vs
        else vs := prm._Name + ': ' + vs;
      if Result <> '' then
        Result := Result + '; ' + vs
      else
        Result := vs;
      inc(num);
    end;
end;

procedure WriteOutFunctionToHeader(f: TFunctionDef; st: TStrings);
var
  restype : AnsiString;
  fntype  : AnsiString;
  isptr   : Boolean;
  s       : AnsiString;
begin
  if not Assigned(f._ResultType) then begin
    isptr := false;
    fntype := 'int';
  end else if (f._ResultType is TTypeDef) then begin
    isptr := TTypeDef(f._ResultType)._IsPointer;
    fntype := TTypeDef(f._ResultType)._Name;
  end else begin
    isptr := false;
    s := 'see .h file for proper type';
    if f._ResultType is TEntityStruct then begin
      s := 'struct ' + TEntityStruct(f._ResultType)._Name;
      if  TEntityStruct(f._ResultType)._isPointer then s := s + '*'
      else s := s + '{...}';
    end;
    fntype := '? {'+s+'}';
  end;

  restype := ObjCToDelphiType(fntype, isptr);
  s:= GetProcFuncHead(f._Name, '', CParamsListToPascalStr(f._ParamsList), restype) + ' ';

  if f._isInLine then s := s + ' inline; '
  else s:=s+ConvertSettings.GetCallConv(true);

  if f._isExternal then
    s := s + Format(' external name ''%s%s'';', [ConvertSettings.ExternFuncPrefix, f._Name]);
  st.Add(s);
  //st.Add(s);
end;

procedure WriteOutEnumToHeader(enm: TEnumTypeDef; st: TStrings);
var
  i   : Integer;
//  ent : TEnumValue;
  obj : TObject;
  pre : TEnumValue;
  vl  : TEnumValue;
  vls : AnsiString;
  vli : Integer;
begin
  if enm._Name = '' then begin
    // unnamed enums are written out as constants
    pre := nil;
    st.Add('const');
    vli := 1;
    for i := 0 to enm.Items.Count - 1 do begin
      obj := TObject(enm.Items[i]);
      if obj is TEnumValue then begin
        vl := TEnumValue(obj);
        if vl._Value = '' then begin
          if not Assigned(pre) then begin
            vls := '0';
            pre := vl;
          end else begin
            vls := pre._Name + ' + ' + IntToStr(vli);
            inc(vli);
          end;
        end else begin
          vls := vl._Value;
          vli := 1;
          pre := vl;
        end;
        st.Add(Format('  %s = %s;', [vl._Name,  GetPascalConstValue(vls)]));
      end;
    end;
    st.Add('');
    //st.Add('type');
  end else begin
    st.Add('type');
    // named enums are written out as delphi enumerations
    st.Add(Format('  %s = (', [enm._Name] ));
    WriteOutEnumValues(enm, '    ', st );
    st.Add('  );');
    st.Add('');
  end;
end;

procedure WriteOutUnion(AField: TUnionTypeDef; const Prefix: AnsiString; subs: TStrings);
var
  i   : integer;
  n   : integer;
  c   : Integer;
  s   : AnsiString;
begin
  n := 0;
  subs.Add(Prefix + 'case Integer of');
  for i := 0 to AField.Items.Count  - 1 do begin
    if TObject(AField.Items[i]) is TStructField then begin
      subs.Add(Prefix + Format('%d: (', [n]));
      c := subs.Count;
      WriteOutRecordField(TStructField(AField.Items[i]), Prefix + '  ', subs);
      subs[subs.Count-1] := subs[subs.Count-1] + ');';

      if subs.Count - 1 = c then begin
        s := subs[subs.Count - 1];
        Delete(s, 1, length(Prefix + '  '));
        subs.Delete(subs.Count - 1);
        subs[subs.Count - 1] := subs[subs.Count - 1] + s;
      end;

      inc(n);
    end;
  end;
end;

function CToDelphiFuncType(AFuncType: TFunctionTypeDef): AnsiString;
var
  restype : AnsiString;
  fntype  : AnsiString;
  isptr   : Boolean;
begin
  if not Assigned(AFuncType._ResultType) then begin
    isptr := false;
    fntype := 'int';
  end else if (AFuncType._ResultType is TTypeDef) then begin
    isptr := TTypeDef(AFuncType._ResultType)._IsPointer;
    fntype := TTypeDef(AFuncType._ResultType)._Name;
  end else begin
    isptr := false;
    fntype := '{todo: not implemented... see .h file for type}';
  end;
  restype := ObjCToDelphiType(fntype, isptr);
  Result := GetProcFuncHead('', '', CParamsListToPascalStr(AFuncType._ParamsList), restype) + ' '+ConvertSettings.GetCallConv(true);
  //Result := Copy(Result, 1, length(Result) - 1);
  //Result := Result + '; cdecl';
end;

procedure WriteOutRecordField(AField: TStructField; const Prefix: AnsiString; subs: TStrings);
var
  pastype : AnsiString;
  nm      : AnsiString;
  i       : Integer;

begin
  //todo:!
  if Assigned(AField._Type) then begin
    if (AField._Type is TUnionTypeDef) then
      WriteOutUnion(TUnionTypeDef(AField._Type), Prefix, subs)
    else if AField._Type is TEntityStruct then begin
      i := subs.Count;
      WriteOutRecord(TEntityStruct(AField._Type), Prefix, 'packed', subs);
      if i < subs.Count then begin
        nm := subs[i];
        Delete(nm, 1, length(Prefix));
        nm := Prefix + Format('%s : %s', [AField._Name, nm]);
        subs[i] := nm;
      end;
    end else begin

      if (AField._Type is TFunctionTypeDef) then
        pastype := CToDelphiFuncType(AField._Type as TFunctionTypeDef)
      else
        pastype := ObjCToDelphiType(AField._TypeName, IsTypePointer(AField._Type, false));

      nm := FixIfReserved(AField._Name);
      if (AField._IsArray) and (AField._ArraySize <> '') then
        subs.Add(Prefix + Format('%s : array [0..%s-1] of %s;', [nm, AField._ArraySize, pastype]))
      else
        subs.Add(Prefix + Format('%s : %s; ', [nm, pastype]));
    end;
  end;
end;

procedure WriteOutBitFields(const prefix, fieldname: AnsiString; var Index: Integer; subs: TStrings; bitsize: Integer);
var
  ts  : AnsiString;
begin
  while bitsize > 0 do begin
    if bitsize > 16 then begin
      ts := 'LongWord';
      dec(bitsize, 32);
    end else if bitsize > 8 then begin
      ts := 'Word';
      dec(bitsize, 16);
    end else begin
      ts := 'Byte';
      dec(bitsize, 8);
    end;

    subs.Add(Prefix + Format('%s : %s;', [fieldname + IntToStr(index), ts]));
    inc(index);
  end;
end;

procedure WriteOutRecord(struct: TEntityStruct; const Prefix, RecPrefix : AnsiString; subs: TStrings);
var
  i         : integer;
  bits      : Integer;
  sf        : TStructField;
  bitfname  : AnsiString;
  bitfx     : Integer;
begin
  bitfname := '_bitflags';
  bitfx := 1;

  subs.Add(Prefix + Format('%s record ', [RecPrefix]));
  bits := 0;
  for i := 0 to struct.Items.Count - 1 do
    if Assigned(struct.ITems[i]) and (TObject(struct.Items[i]) is TStructField) then begin
      sf := TStructField(struct.Items[i]);
      if sf._BitSize <> 0 then
        inc(bits, sf._BitSize)
      else begin
        if bits > 0 then begin
          WriteOutBitFields(Prefix+'  ', bitfname, bitfx, subs, bits);
          bits :=0;
        end;
        WriteOutRecordField(sf, Prefix + '  ', subs);
      end;
    end;
  if bits > 0 then
    WriteOutBitFields(Prefix+'  ', bitfname, bitfx, subs, bits);
  subs.Add(Prefix + 'end;');
end;

procedure WriteOutTypeDefRecord(struct: TEntityStruct; const Prefix, RecPrefix : AnsiString; subs: TStrings);
var
  i : integer;
  s : AnsiString;
begin
  i := subs.Count;
  if not isEmptyStruct(struct) then begin
    WriteOutRecord(struct, Prefix, RecPrefix, subs);
    s := subs[i];
    Delete(s, 1, length(Prefix));
    s := Prefix + struct._Name + ' = ' + s;
    subs[i] := s;
  end else begin
    subs.Add(Prefix + struct._Name + ' = Pointer;'); 
  end;
end;

function WriteOutTypeDefName(const NewType, FromType: AnsiSTring; isPointer: Boolean): AnsiString;
var
  wrType: AnsiString;
begin
  wrType := ObjCToDelphiType(fromType, isPointer);
  Result := Format('%s = %s;', [NewType, wrType]);
  {else
    Result := Format('%s = ^%s;', [NewType, wrType]);}
    
  case GetObjCVarType(FromType) of
    vt_FloatPoint: ConvertSettings.FloatTypes.Add(NewType);
    vt_Object: ConvertSettings.ObjCClassTypes.Add(NewType);
    vt_Struct: ConvertSettings.StructTypes.Add(NewType);
  end;
end;

procedure WriteOutTypeDefToHeader(typedef: TTypeNameDef; const Prefix: AnsiString; subs: TStrings);
var
  vs    : AnsiString;
  tmp   : AnsiString;
begin
  vs := ConvertSettings.TypeDefReplace[typedef._Inherited];
  if vs = '' then vs := typedef._Inherited;
  if not Assigned(typedef._Type) or (typedef._Type is TTypeDef) then begin
    subs.Add('type');
    subs.Add(Prefix + WriteOutTypeDefName(typedef._TypeName, vs, IsTypePointer(typedef._Type, false)));
  end else if typedef._Type is TEnumTypeDef then begin
    tmp := TEnumTypeDef(typedef._Type)._Name;
    TEnumTypeDef(typedef._Type)._Name := typedef._TypeName;
    WriteOutEnumToHeader(TEnumTypeDef(typedef._Type), subs);
    TEnumTypeDef(typedef._Type)._Name := tmp;
  end else if typedef._Type is TEntityStruct then begin
    subs.Add('type');
    if TEntityStruct(typedef._Type)._Name <> '' then begin
      WriteOutTypeDefRecord(typedef._Type as TEntityStruct, '  ', 'packed ', subs);
      subs.Add(Prefix + WriteOutTypeDefName(typedef._TypeName, TEntityStruct(typedef._Type)._Name, IsTypePointer(typedef._Type, false)));
      ConvertSettings.StructTypes.Add(TEntityStruct(typedef._Type)._Name);
    end else begin
      TEntityStruct(typedef._Type)._Name := typedef._TypeName;
      WriteOutTypeDefRecord(typedef._Type as TEntityStruct, '  ', 'packed ', subs);
      ConvertSettings.StructTypes.Add(typedef._TypeName);
    end;
  end else if typedef._Type is TFunctionTypeDef then begin
    subs.Add('type');
    tmp := Format('  %s = %s', [typedef._TypeName, CToDelphiFuncType(TFunctionTypeDef(typedef._Type))]);
    Subs.Add(tmp);
  end;

  subs.Add('');
end;

procedure WriteOutHeaderSection(hdr: TObjCHeader; st: TStrings);
var
  i       : Integer;
  subs    : TStringList;
  consts  : TStringList;
  cmt     : TStringList;
  cl      : TClassDef;
  clName  : String;
  isConstant  : Boolean;
  b : Boolean;

  PasSection  : String;

  procedure StartSection(NewSection: String);
  begin
    if NewSection = PasSection then Exit;
    st.Add('');
    if NewSection <> '' then st.Add(NewSection);
    PasSection := NewSection;
  end;

const
  SpacePrefix = '  ';
begin
  PasSection := '';

  BeginSection('HEADER', st);
  BeginExcludeSection( GetIfDefFileName(hdr._FileName, 'H'), st);
  subs := TStringList.Create;
  consts := TStringList.Create;
  cmt := TStringList.Create;
  isConstant := false;
  try
    for i := 0 to hdr.Items.Count - 1 do
      if Assigned(hdr.Items[i]) then
        if (TObject(hdr.Items[i]) is TPrecompiler) then begin
          WriteOutIfDefPrecompiler(TPrecompiler(hdr.Items[i]), SpacePrefix, st);
          WriteOutPrecompInclude(TPrecompiler(hdr.Items[i]), st);
          WriteOutPrecompDefine(TPrecompiler(hdr.Items[i]), '  ', subs, b);
          isConstant := isConstant or b; 
        end;

    if subs.Count > 0 then begin
      if isConstant then StartSection('const');
      st.AddStrings(subs);
      subs.Clear;
    end;

    if ConvertSettings.UseRefClassType then 
      for i := 0 to hdr.Items.Count - 1 do
        if (TObject(hdr.Items[i]) is TClassDef) then begin
          cl := TClassDef(TObject(hdr.Items[i]));
          if cl._Category = '' then begin
            StartSection('type');
            clName := ConvertSettings.GetObjCClassReplaceName( TClassDef(hdr.Items[i])._ClassName);          
            st.Add(Format('  %s = %s;', [clName, ConvertSettings.ObjcIDReplace]) );
          end;
        end;

    for i := 0 to hdr.Items.Count - 1 do
      if Assigned(hdr.Items[i]) then begin

        if (TObject(hdr.Items[i]) is TEnumTypeDef) then begin
          WriteOutEnumToHeader(TEnumTypeDef(hdr.Items[i]), st);
          PasSection := 'const';
        end else if (TObject(hdr.Items[i]) is TPrecompiler) then begin
          WriteOutIfDefPrecompiler(TPrecompiler(hdr.Items[i]), SpacePrefix, st)
        end else if (TObject(hdr.Items[i]) is TTypeNameDef) then begin
          WriteOutTypeDefToHeader(TTypeNameDef(hdr.Items[i]), SpacePrefix, st);
          //hack. MUST CHANGE CODE
          PasSection := 'type';
        end else if (TObject(hdr.Items[i]) is TSkip) then
          st.Add('//'+ TSkip(hdr.Items[i])._Skip)
        else if (TObject(hdr.Items[i]) is TComment) then begin
          //WriteOutIfComment(hdr.Items, i, SpacePrefix, subs);
          WriteOutCommentStr( TComment(hdr.Items[i])._Comment, SpacePrefix, st);
        end else if (TObject(hdr.Items[i]) is TVariable) then begin
          StartSection('var');
          WriteOutVariableToHeader(TVariable(hdr.Items[i]), SpacePrefix, st)
        end else if (TObject(hdr.Items[i]) is TFunctionDef) then begin
          StartSection('');
          WriteOutFunctionToHeader(TFunctionDef(hdr.Items[i]), st);
        end;
      end; {of if}

    st.add('');
{    if subs.Count > 0 then begin
      //if subs[0] <> 'const' then st.Add('type');
      st.AddStrings(subs);
      subs.Clear;
    end;}


  finally
    EndSection(st);
    EndSection(st);
    subs.Add('');
    subs.Free;
    consts.Free;
    cmt.Free;
  end;
end;

procedure WriteOutClassToClasses(cl: TClassDef; subs: TStrings);
var
  i   : Integer;
//  cnt : Integer;
  s   : AnsiString;
  nm  : AnsiString;
  cmt : AnsiString;
  j   : Integer;
  obj : TObject; // or TEntity

  mtds   : TStringList; // name of methods
  restype: TObjCResultTypeDef;

//  over   : TStringList; // overloaded names

  isProtEmpty  : Boolean;
  protidx  : Integer;
  pr       : TObjCClassProperty;
const
  SpacePrefix = '    ';
begin
  isProtEmpty := true;
  
  subs.Add('');
  subs.Add('  { '+cl._ClassName +' }');
  subs.Add('');
  s := '  ' + cl._ClassName + ' = class';
  if cl._SuperClass <> '' then begin
    subs.Add(s + '('+cl._SuperClass+')');
    protidx := subs.Count;
    subs.Add('  public');
    subs.Add('    class function getClass: '+ConvertSettings.ObjCIDReplace+'; override;');
  end else begin
    subs.Add(s + '{from category '+ cl._Category +'}');
    protidx := subs.Count;
    subs.Add('  public');
  end;

  mtds := TStringList.Create;
  try
    for j := 0 to cl.Items.Count - 1 do begin
      obj := TObject(cl.Items[j]);
      if obj is TClassMethodDef then begin
        nm := TClassMethodDef(obj)._Name;
        i := mtds.indexOf(nm);
        if i < 0 then
          mtds.Add(nm)
        else
          mtds.Objects[i] := TObject(Integer(mtds.Objects[i]) + 1);
      end;
    end;

    for j := 0 to cl.Items.Count - 1 do begin
      obj := TObject(cl.Items[j]);
      if obj is TClassMethodDef then begin
        WriteOutIfComment(cl.Items, j - 1, '    ', subs);
        s := GetMethodStr(cl, TClassMethodDef(cl.Items[j]), false);
        nm := TClassMethodDef(cl.Items[j])._Name;
        i := mtds.IndexOf(nm);
        if Integer(mtds.Objects[i]) > 0 then s := s + ' overload;';


        restype := TClassMethodDef(cl.Items[j]).GetResultType;
        if Assigned(restype) then begin
          cmt := TClassMethodDef(cl.Items[j]).GetResultType.TagComment;
          (*if cmt <> '' then
            s := s + '{'+cmt+'}';*)
        end;

        subs.Add(SpacePrefix + s);
      end else if obj is TPrecompiler then begin
        WriteOutIfDefPrecompiler(TPrecompiler(obj), SpacePrefix, subs);
      end;
    end;

    for j := 0 to cl.Items.Count - 1 do begin
      obj := TObject(cl.Items[j]);
      if obj is TObjCClassProperty then begin
        pr := obj as TObjCClassProperty;
        subs.Add('    property ' + pr._Name+';');

        if isProtEmpty then begin
          subs.Insert(protidx, '  protected'); inc(protidx);
          isProtEmpty := false;
        end;
        subs.Insert(protidx, '    function  '+GetterSetterName(pr._Name, pr._Getter, false)+';');
        inc(protidx);
        subs.Insert(protidx, '    procedure '+GetterSetterName(pr._Name, pr._Setter, true) +';');
        inc(protidx);
      end;
    end;

  finally
    mtds.Free;
  end;

  subs.Add('  end;');
  subs.Add('');
end;

procedure WriteOutClassesSection(hdr: TObjCHeader; st: TStrings);
var
  i     : integer;
  subs  : TStringList;
begin
  subs := TStringList.Create;
  try
    for i := 0 to hdr.Items.Count - 1 do
      if Assigned(hdr.Items[i]) and (TObject(hdr.Items[i]) is TPrecompiler) then
        WriteOutPrecompInclude(TPrecompiler(hdr.Items[i]), subs);

    for i := 0 to hdr.Items.Count - 1 do
      if Assigned(hdr.Items[i]) then begin
        if TObject(hdr.Items[i]) is TPrecompiler then
          WriteOutIfDefPrecompiler(TPrecompiler(hdr.Items[i]), '    ', subs)
        else if (TObject(hdr.Items[i]) is TClassDef) then begin
          WriteOutIfComment(hdr.Items, i - 1, '    ', subs);
          WriteOutClassToClasses(TClassDef(hdr.Items[i]), subs);
        end;
      end;

    if subs.Count = 0 then Exit;
    BeginSection('CLASSES', st);
    BeginExcludeSection( GetIfDefFileName(hdr._FileName, 'C'), st);
    try
      if ConvertSettings.CustomTypes.Count > 0 then begin
        with ConvertSettings do
          for i := 0 to CustomTypes.Count - 1 do
            CustomTypes[i] := '  ' + CustomTypes[i];

        st.AddStrings(ConvertSettings.CustomTypes);
        st.Add('');
        ConvertSettings.CustomTypes.Clear;
      end;
      
      st.AddStrings(subs);
    finally
      EndSection(st);
      EndSection(st);
    end;
    
  finally
    subs.Free;
  end;
end;

function isAnyParam(mtd: TClassMethodDef): boolean;
var
  i   : integer;
begin
  Result := false;
  for i := 0 to mtd.Items.Count - 1 do
    if TObject(mtd.Items[i]) is TObjCParameterDef then begin
      Result := true;
      Exit;
    end;
end;

const
  MtdPrefix = 'TMtd_';
  MtdPostfix = '';

procedure ObjCMethodToProcType(mtd: TClassMethodDef; var typeName: AnsiString; subs: TStrings; isResultStruct: Boolean);
var
  s   : AnsiString;
  ms  : AnsiString;
  restype : AnsiString;
  funchdr : AnsiString;
  callcnv : AnsiString;
begin
  //typeName := MtdPrefix + mtd._Name + MtdPostFix;
  typeName := 'TmsgSendWrapper';

  subs.Add('type');
  ms := GetMethodParams(mtd, false);
  if ms = '' then ms := 'param1: '+ConvertSettings.ObjCIDReplace+'; param2: SEL'
  else ms := 'param1: '+ConvertSettings.ObjCIDReplace+'; param2: SEL' + ';' + ms;

  if isResultStruct then begin
    restype := '';
    ms := 'result_param: Pointer; ' + ms;
  end else begin
    restype := GetMethodResultType(mtd);
    if IsMethodConstructor(mtd.Owner as TClassDef, mtd) then restype := ConvertSettings.ObjCIDReplace;
  end;

  funchdr := GetProcFuncHead('', '', ms, restype, '' );
  callcnv := ConvertSettings.GetCallConv(false);
  s := Format('  %s = %s%s;',[typeName, funchdr, callcnv]);
  subs.Add(s);
end;

(*function GetParamsNames(mtd: TClassMethodDef): AnsiString;
var
  i   : Integer;
  obj : TObject;
  vname : AnsiString;
begin
  vname := '';
  Result := '';
  for i := 0 to mtd.Items.Count - 1 do begin
    obj := TObject(mtd.Items[i]);
    if obj is TParamDescr then begin
      if vName <> '' then Result := Result + vname + ', ';
      vname := 'A'+TParamDescr(obj)._Descr;
    end else if obj is TObjCParameterDef then begin
      if vname = '' then vname := 'A'+TObjCParameterDef(obj)._Name;
    end;
  end;
  Result := Result + vname;
end;*)


// procedure writes out constructor entity to the implementation section
// with the followind structure
// assignes object's ClassID usinng GetClass method
// creates ObjC object calling objc_method Alloc
// adds procedure type and variable of objC init??? method, to wrap obj_SendMsg
// initialize ObjC object structure calling init??? method

{function RefixName(const mtdName: AnsiString): AnsiString;
begin
  Result := mtdName;
  if mtdName = '' then Exit;
  if mtdName[length(mtdName)] = '_' then
    Result := Copy(mtdName, 1, length(mtdName) - 1);
end;}

procedure WriteOutConstructorMethod(mtd: TClassMethodDef; subs: TStrings);
var
  typeName  : AnsiString;
  cl  : TClassDef;
  prms  : AnsiString;
begin
  cl  := TClassDef(mtd.Owner);
  ObjCMethodToProcType(mtd, typeName, subs, false);
  prms := GetMethodParams(mtd, true);
  if prms <> '' then prms := ', ' + prms;

  if (Pos('init', mtd._Name) = 1) and (not mtd._IsClassMethod) then begin
    //todo: check if object is allocated with 'alloc...' or 'init...' or else =)
    subs.Add('var');
    subs.Add(
      Format('  vmethod: %s;', [typeName]));
    subs.Add('begin');
    subs.Add('  ClassID := getClass();');
    subs.Add('  allocbuf := objc_msgSend(ClassID, sel_registerName(PChar(Str_alloc)), []);');
    subs.Add(
      Format('  vmethod := %s(@objc_msgSend);', [typeName]));
    subs.Add(
      Format('  Handle := vmethod(allocbuf, sel_registerName(PChar(Str%s_%s))%s);', [cl._ClassName, mtd._Name, prms]));
    subs.Add('end;');
  end else begin
    subs.Add('var');
    subs.Add(
      Format('  vmethod: %s;', [typeName]));
    subs.Add('begin');
    subs.Add('  ClassID := getClass();');
    subs.Add(
      Format('  vmethod := %s(@objc_msgSend);', [typeName]));
    subs.Add(
      Format('  Handle := vmethod(ClassID, sel_registerName(PChar(Str%s_%s))%s);', [cl._ClassName, mtd._Name, prms]));
    subs.Add('end;');
  end;
end;

const
  ClassMethodCaller : array [ Boolean] of AnsiString = (
    'Handle', 'getClass'
  );

// writes out a method to implementation section
procedure WriteOutMethod(mtd: TClassMethodDef; subs: TStrings);
var
  s         : AnsiString;
  typeName  : AnsiString;
  cl        : TClassDef;
  tp      : TObjcConvertVarType;
  res       : AnsiString;
  callobj   : AnsiString;
  mnm       : AnsiString;
  prms      : AnsiString;
begin
  cl := TClassDef(mtd.Owner);
  callobj := ClassMethodCaller[mtd._IsClassMethod];

  res := GetMethodResultType(mtd);
  mnm :=mtd._Name; //RefixName(mtd._Name);
  //s := Format('vmethod(%s, sel_registerName(PChar(Str%s_%s)), %s)', [callobj, cl._ClassName, RefixName(mtd._Name), GetParamsNames(mtd)]);
  tp := GetObjCVarType(res);
  prms := GetMethodParams(mtd, true);
  case tp of
    vt_Int, vt_Object: s := Format('vmethod(%s, sel_registerName(PChar(Str%s_%s)), %s)', [callobj, cl._ClassName, mnm, prms]);
    vt_FloatPoint: s := Format('vmethod(%s, sel_registerName(PChar(Str%s_%s)), %s)', [callobj, cl._ClassName, mnm, prms]);
    vt_Struct: s := Format('vmethod(@Result, %s, sel_registerName(PChar(Str%s_%s)), %s)', [callobj, cl._ClassName, mnm, prms]);
  end;

  if (tp <> vt_Struct) and (ObjCResultToDelphiType(mtd.GetResultType) <> '') then begin
      if tp <> vt_FloatPoint then
        s := Format('Result := %s(%s)', [res, s])
      else
        s := Format('Result := %s', [s]);
    //s := 'Result := ' res(' + s+')';
  end;
  

  ObjCMethodToProcType(mtd, typeName, subs, tp=vt_Struct);
  subs.Add('var');
  subs.Add(
    Format('  vmethod: %s;', [typeName]));
  subs.Add('begin');
  case tp of
    vt_Struct:     subs.Add(Format('  vmethod := %s(@objc_msgSend_fpret);', [typeName]));
    vt_FloatPoint: subs.Add(Format('  vmethod := %s(@objc_msgSend_stret);', [typeName]));
  else
    subs.Add( Format('  vmethod := %s(@objc_msgSend);', [typeName]));
  end;
  subs.Add(
    Format('  %s;', [s]));
  subs.Add('end;');
end;

// writes out a method to implementation section, that has no params
procedure WriteOutMethodNoParams(mtd: TClassMethodDef; subs: TStrings);
var
  s       : AnsiString;
  res     : AnsiString;
  cl      : TClassDef;
  callobj : AnsiString;
  tp      : TObjcConvertVarType;
  mnm     : AnsiString;
begin
  cl := TClassDef(mtd.owner);
  callobj := ClassMethodCaller[mtd._IsClassMethod];
  res := GetMethodResultType(mtd);
  tp := GetObjCVarType(res);

  //mnm := RefixName(mtd._Name);
  mnm := mtd._Name;               
  case tp of
    vt_Int, vt_Object:
      s := Format('objc_msgSend(%s, sel_registerName(PChar(Str%s_%s)), [])', [callobj, cl._ClassName, mnm ]);
    vt_FloatPoint:
      s := Format('objc_msgSend_fpret(%s, sel_registerName(PChar(Str%s_%s)), [])', [callobj, cl._ClassName, mnm ]);
    vt_Struct:
      s := Format('objc_msgSend_stret(@Result, %s, sel_registerName(PChar(Str%s_%s)), [])', [callobj, cl._ClassName, mnm ]);
  end;
  
  if (tp <> vt_Struct) and (res <> '') then begin
    if tp <> vt_FloatPoint then
      s := Format('Result := %s(%s)', [res, s])
    else
      s := Format('Result := %s', [s]);
  end;
  s := s + ';';

  subs.Add('begin');
  subs.Add('  ' + s);
  subs.Add('end;');

end;

procedure WriteOutMethodToImplementation(mtd: TClassMethodDef; subs: TStrings);
var
  cl        : TClassDef;
  typeName  : AnsiString;
begin
  typeName := '';
  if not Assigned(mtd.Owner) or (not (TObject(mtd.Owner) is TClassDef)) then Exit; // method cannot be without owning class
  cl := TClassDef(mtd.Owner);
  
  subs.Add(GetMethodStr(cl, mtd, true));//writes out method header, like function NsType.NsName(params): Result
  if IsMethodConstructor(cl, mtd) then
    WriteOutConstructorMethod(mtd, subs)
  else if not isAnyParam(mtd) then
    WriteOutMethodNoParams(mtd, subs)
  else
    WriteOutMethod(mtd, subs);
  subs.Add('');
end;


procedure WriteOutClassToImplementation(cl: TClassDef; subs: TStrings);
var
  i   : integer;
  obj : TObject;
begin
  subs.Add('{ '+cl._ClassName + ' }');
  
  if cl._Category <> '' then begin
    subs.Add('  //todo: classes of category');
    Exit;
  end;
  
  subs.Add('');
  subs.Add('class ' + GetProcFuncHead('getClass', cl._ClassName, '', ConvertSettings.ObjCIdReplace));
  subs.Add('begin');
  subs.Add(
    Format('  Result := objc_getClass(Str%s_%s);', [cl._ClassName, cl._ClassName]));
  subs.Add('end;');
  subs.Add('');
  
  for i := 0 to cl.Items.Count - 1 do begin
    obj := TObject(cl.Items[i]);
    if obj is TClassMethodDef then
      WriteOutMethodToImplementation ( TClassMethodDef(cl.Items[i]), subs)
    else if obj is TPrecompiler then
      WriteOutIfDefPrecompiler( TPrecompiler(obj), '', subs);
  end;
end;

procedure WriteOutFuncitionToImplemenation(f: TFunctionDef; st: TStrings);
var
  restype : AnsiString;
  fntype  : AnsiString;
  isptr   : Boolean;
  s       : AnsiString;
  txt     : AnsiString;
  line    : AnsiString;
  idx     : Integer;
begin
  if not Assigned(f) or (f._isExternal) then Exit; // external functions does not have a body

  if not Assigned(f._ResultType) then begin
    isptr := false;
    fntype := 'int';
  end else if (f._ResultType is TTypeDef) then begin
    isptr := TTypeDef(f._ResultType)._IsPointer;
    fntype := TTypeDef(f._ResultType)._Name;
  end else begin
    isptr := false;
    fntype := '{todo: not implemented... see .h file for type}';
  end;

  restype := ObjCToDelphiType(fntype, isptr);
  s:= GetProcFuncHead(f._Name, '', CParamsListToPascalStr(f._ParamsList), restype) + ' ';
  if f._isInline then s := s + 'inline; '
  else s:=s+ConvertSettings.GetCallConv(true);
  st.Add(s);
  st.Add('begin ');
  if not Assigned(f._Body) then
    st.Add('  //body is missing... Probably declared somethere in .c (.cpp, .cxx) file? or the parser bug?')
  else begin
    txt := TFunctionBody(f._Body)._RawText;
    idx := 1;
    st.Add('  // Sorry, but the parser cannot convert the function''s body. ');
    while idx <= length(txt) do begin
      line := ScanTo(txt, idx, EoLnChars);
      idx := SkipEndOfLineChars(txt, idx);
      st.Add('  //'+Line); 
    end;
  end;
  st.Add('end;');
  st.Add('');
end;

procedure WriteOutImplementationSection(hdr: TObjCHeader; st: TStrings; consts: TStringList);
var
  i     : Integer;
  subs  : TStringList;
begin
  subs := TStringList.Create;
  try

    if consts.Count > 0 then begin
      subs.add('const');
      subs.AddStrings(consts);
    end;

    for i := 0 to hdr.Items.Count - 1 do
      if Assigned(hdr.Items[i]) then
        if (TObject(hdr.Items[i]) is TClassDef) then
          WriteOutClassToImplementation(TClassDef(hdr.Items[i]), subs)
        else if (TObject(hdr.Items[i]) is TFunctionDef) then begin
          WriteOutFuncitionToImplemenation(TFunctionDef(hdr.Items[i]), subs);
        end;

    if subs.Count = 0 then Exit;
    
    BeginSection('IMPLEMENTATION', st);
    try
      st.AddStrings(subs);
    finally
      EndSection(st);
    end;
  
  finally
    subs.Free;
  end;
end;


//Removed, must not be used, because enumerations must be converted to constants
function AppleEnumType(items: TList; TypeDefIdx: Integer): Boolean;
begin
  Result := false;
{var
  EnumIdx : integer;
  typedef : TTypeNameDef;
  enumdef : TEnumTypeDef;
const
  AppleInherit = 'NSUInteger';
begin
  Result := false;
  EnumIdx := TypeDefIdx - 1;
  if (EnumIdx < 0) or (EnumIdx >= items.Count) then Exit;

  if (TObject(items.Items[TypeDefIdx]) is TTypeNameDef) and
   (TObject(items.Items[EnumIdx]) is TEnumTypeDef) then begin
    typedef := TTypeNameDef(items.Items[TypeDefIdx]);
    enumdef := TEnumTypeDef(items.Items[EnumIdx]);
  end else
    Exit;

  if typedef._Inherited = AppleInherit then begin
    enumdef._Name := typedef._TypeName;
    Result := true;
  end;
}
end;

procedure FixAppleCategories(Items: TList; category: TClassDef);
var
  i   : Integer;
  j   : Integer;
  cl  : TClassdef;
begin
  for i := 0 to Items.Count - 1 do
    if TObject(Items[i]) is TClassDef then begin
      cl := TClassDef(Items[i]);
      if (cl._ClassName = category._ClassName) and (cl._Category = '') then
        for j := 0 to category.Items.Count - 1 do begin
          cl.Items.Add(category.Items[j]);
          TEntity(category.Items[j]).owner := cl;
        end; {of if}
    end; {of if}
end;

procedure FixAppleClassDef(cl: TClassDef);
var
  i         : integer;
  j         : integer;
  res       : TClassMethodDef;
  mtd       : TClassMethodDef;
  mtdnames  : TStringList;
begin
  //todo: use hash table
  mtdnames := TStringList.Create;
  try
    for i := 0 to cl.Items.Count - 1 do
      if TObject(cl.Items[i]) is TClassMethodDef then begin
        mtd := TClassMethodDef(cl.Items[i]);
        j := mtdnames.IndexOf(mtd._Name);
        if j < 0 then
          mtdnames.AddObject(mtd._Name, mtd)
        else begin
          res := TClassMethodDef(mtdnames.Objects[j]);
          if res._IsClassMethod then res._Name := res._Name + '_'
          else if mtd._IsClassMethod then mtd._Name := mtd._Name + '_';
        end;
        if IsPascalReserved(mtd._Name) then
          mtd._Name := mtd._Name + '_';
      end;
  finally
    mtdnames.Free;
  end;
//nothing todo...
end;

procedure FastPack(Items: TList);
var
  i,  j : INteger;
begin
  j := 0;
  for i := 0 to Items.Count - 1 do
    if Assigned(Items[i]) then begin
      Items[j] := Items[i];
      inc(j);
    end;
  Items.Count := j;
end;

procedure FixObjCClassTypeDef(ent: TEntity);
var
  i   : Integer;
  j   : Integer;
  cl  : TClassDef;
begin
  for i := 0 to ent.Items.Count - 1 do begin
    if not (TObject(ent.Items[i]) is TClassDef) then Continue;
    cl := TClassDef(ent.Items[i]);
    for j := 0 to cl.Items.Count - 1 do begin
      if not IsTypeDefEntity(cl.Items[j]) then Continue;
      ent.Items.Add(cl.Items[j]);
      TEntity(cl.Items[j]).Owner := ent;
      cl.Items[j] := nil;
    end;
  end;
  FastPack(ent.Items);
end;

procedure AppleHeaderFix(ent : TEntity);
var
  i   : Integer;
  j   : Integer;
  obj : TEntity;
  prm : TObjCParameterDef;
  res : TObjCResultTypeDef;
  td  : TTypeDef;
  nm  : AnsiString;
begin
//  i := 0;
  for i := 0 to ent.Items.Count - 1 do begin
    obj := TEntity(ent.Items[i]);
    if (obj is TTypeNameDef) and (AppleEnumType(ent.Items, i)) then begin
      ent.Items[i] := nil;
      obj.Free;
    end else if (obj is TClassDef) and ((TClassDef(obj)._SuperClass = '') and (TClassDef(obj)._Category <> ''))then begin
      FixAppleCategories(ent.Items, TClassDef(obj));
      ent.Items[i] := nil;
      obj.Free;
    end else if (obj is TClassDef) and ((TClassDef(obj)._Category = '') and (TClassDef(obj)._ClassName = 'NSObject')) then begin
      if TClassDef(obj)._SuperClass = '' then
        TClassDef(obj)._SuperClass := 'TObject'
    {end else if (obj is TParamDescr) then begin
      if IsPascalReserved(TParamDescr(obj)._Descr) then
        TParamDescr(obj)._Descr := '_'+TParamDescr(obj)._Descr}
    end else if (obj is TClassMethodDef) and not IsMethodConstructor(TClassDef(obj.Owner ), TClassMethodDef(obj)) then begin
      res := TClassMethodDef(obj).GetResultType;
      nm := ObjCResultToDelphiType(res);
      if ConvertSettings.ObjCClassTypes.IndexOf(nm)>= 0 then
        if res._Type is TTypeDef then begin
          td := TTypeDef(res._Type);
          res.tagComment := td._Name;
          td._Name := ConvertSettings.GetObjCClassReplaceName(nm); //Format('objc.id', [td._Name] );
        end;
    end else if (obj is TObjCParameterDef) then begin
      prm := TObjCParameterDef(obj);

      nm := ObjCResultToDelphiType(prm._Type);
      if ConvertSettings.ObjCClassTypes.IndexOf(nm) >= 0 then begin
        if prm._Type._Type is TTypeDef then begin
          TTypeDef(prm._Type._Type)._Name := ConvertSettings.GetObjCClassReplaceName(nm); //Format('objc.id {%s}', [TTypeDef(prm._Type._Type)._Name] );
        end;
      end;

      if IsPascalReserved(prm._Name) then
        prm._Name := '_' + prm._Name;

    end else if (obj is TStructField) then begin
      // should _TypeName to be removed?
      nm := TStructField(obj)._TypeName;
      if ConvertSettings.ObjCClassTypes.IndexOf(nm) >= 0 then begin
        TStructField(obj)._TypeName := ConvertSettings.GetObjCClassReplaceName(nm);
      end;
    end else if (obj is TClassesForward) then begin
      for j := 0 to TClassesForward(obj)._Classes.Count - 1 do
        ConvertSettings.ObjCClassTypes.Add( TClassesForward(obj)._Classes[j]);
    end;

    
  end;

  // packing list, removing nil references.
  FastPack(ent.Items);
  
  for i := 0 to ent.Items.Count - 1 do 
    AppleHeaderFix( TEntity(ent.Items[i]));

end;

procedure WriteOutForwardSection(hdr: TObjCHeader; st: TStrings);
var
  i : integer;
  subs  : TStringList;
begin
  subs := TStringList.Create;
  try
    for i := 0 to hdr.Items.Count - 1 do
      if TObject(hdr.Items[i]) is TClassDef then
        subs.Add(Format ('  %s = class;', [TClassDef(hdr.Items[i])._ClassName]));
    if subs.Count > 0 then begin
      BeginSection('FORWARD', st);
      // BeginExcludeSection( GetIfDefFileName(hdr._FileName, '_FORWARD'), st);
      try
        st.AddStrings(subs);
        st.Add('');
      finally
        //EndSection(st);
        EndSection(st);
      end;
    end;
  finally
    subs.Free;
  end;
end;

procedure WriteOutIncludeFile(hdr: TObjCHeader; st: TStrings);
var
  i       : integer;
  cmt     : TComment;
  cl      : TClassDef;
  subs    : TStringList;
  consts  : TStringList;
  used    : TStringList;
begin
  subs := TStringList.Create;
  consts := TStringList.Create;
  try
    st.AddStrings(ConvertSettings.ConvertPrefix);

    used := TStringList.Create;
    try
      for i := 0 to hdr.Items.Count - 1 do begin
        if (TObject(hdr.Items[i]) is TClassDef) then begin
          cl := TClassDef(hdr.Items[i]);
          if (cl._Category = '') then begin
            WriteOutClassToConsts(cl, subs, consts);
            used.Add(cl._ClassName);
          end else if used.IndexOf(cl._Classname) >= 0 then begin
            WriteOutClassToConsts(cl, subs, consts);
          end;
        end;
      end;
    finally
      used.Free;
      //used := nil;
    end;

    if hdr.Items.Count <= 0 then Exit;
    AppleHeaderFix(hdr);

    FixObjCClassTypeDef(hdr);

    // .inc header-comment is the first comment entity in .h file , if any
    if TObject(hdr.Items[0]) is TComment then begin
      cmt := TComment(hdr.Items[0]);
      st.Add('(*' + cmt._Comment + '*)');
      cmt.Free;
      hdr.Items.Delete(0);
    end;                        


    WriteOutHeaderSection(hdr, st);
    WriteOutForwardSection(hdr, st);

    for i := 0 to hdr.Items.Count - 1 do
      if TObject(hdr.Items[i]) is TClassDef then
        FixAppleClassDef(TClassDef(hdr.Items[i]));

    WriteOutClassesSection(hdr, st);
    WriteOutImplementationSection(hdr, st, subs);
  finally
    subs.Free;
    consts.Free;
  end;
end;

procedure WriteOutMainFramework(hdr: TObjCHeader; st: TStrings);
//var
//  i   : integer;
//  nm  : AnsiString;
begin
end;

{ TConvertSettings }

procedure TConvertSettings.AssignNewTypeName(const AName, TypeDefStr: AnsiString; var NewTypeName: AnsiString);
var
  typeName : AnsiSTring;
begin
  typeName := AName;
  if typeName = '' then typeName := 'CnvType' + IntToStr(CustomTypes.Count);

  NewTypeName := typeName;
  CustomTypes.Add( Format('%s = %s;', [NewTypeName, TypeDefStr]) );
  // todo: add! a new type!
end;

constructor TConvertSettings.Create;
begin
  TokenReplace := TReplaceList.Create;
  //IgnoreTokens := TStringList.Create;
  IgnoreIncludes := TStringList.Create;
  IgnoreIncludes.CaseSensitive := false;
  DefineReplace := TReplaceList.Create;
  TypeDefReplace := TReplaceList.Create; // replaces for default types
  PtrTypeReplace := TReplaceList.Create; // replaces for C types pointers
  ConvertPrefix := TStringList.Create;

  FloatTypes := TStringList.Create;
  FloatTypes.CaseSensitive := false;

  StructTypes := TStringList.Create;
  StructTypes.CaseSensitive := false;

  ObjCClassTypes := TStringList.Create;
  ObjCClassTypes.CaseSensitive := false;

  CustomTypes := TStringList.Create;

  ObjcIDReplace := 'objc.id';
  CallConv := 'cdecl';
  fExternVarPrefix := '_';
  RefClassPostfix := 'Ref';
end;

destructor TConvertSettings.Destroy;
begin
  TokenReplace.Free;
  FloatTypes.Free;
  StructTypes.Free;
  ObjCClassTypes.Free;

  //IgnoreTokens.Free;
  IgnoreIncludes.Free;
  TypeDefReplace.Free;
  PtrTypeReplace.Free;
  DefineReplace.Free;
  ConvertPrefix.Free;
  CustomTypes.Free;
  inherited Destroy;
end;

procedure InitConvertSettings;
begin
  with ConvertSettings.IgnoreIncludes do begin
    // must not be $included, because they are used
  end;

  with ConvertSettings do begin
    TypeDefReplace['unsigned char'] := 'byte';
    TypeDefReplace['uint8_t'] := 'byte';
    PtrTypeReplace['uint8_t'] := 'PByte';
    PtrTypeReplace['unsigned char'] := 'PChar';
    PtrTypeReplace['char'] := 'PChar';

    TypeDefReplace['short'] := 'SmallInt';
    TypeDefReplace['short int'] := 'SmallInt';

    TypeDefReplace['unsigned short'] := 'Word';
    TypeDefReplace['unsigned short int'] := 'Word';
    TypeDefReplace['uint16_t'] := 'Word';

    TypeDefReplace['int'] := 'Integer';
    TypeDefReplace['signed'] := 'Integer';
    TypeDefReplace['signed int'] := 'Integer';
    TypeDefReplace['int32_t'] := 'Integer';
    TypeDefReplace['NSInteger'] := 'Integer';

    TypeDefReplace['unsigned'] := 'LongWord';
    TypeDefReplace['unsigned int'] := 'LongWord';
    TypeDefReplace['uint32_t'] := 'LongWord';
    TypeDefReplace['NSUInteger'] := 'LongWord';

    PtrTypeReplace['int'] := 'PInteger';
    PtrTypeReplace['signed'] := 'PInteger';
    PtrTypeReplace['signed int'] := 'PInteger';
    PtrTypeReplace['int32_t'] := 'PInteger';
    PtrTypeReplace['NSInteger'] := 'PInteger';

    PtrTypeReplace['unsigned'] := 'PLongWord';
    PtrTypeReplace['unsigned int'] := 'PLongWord';
    PtrTypeReplace['uint32_t'] := 'PLongWord';
    PtrTypeReplace['NSUInteger'] := 'PLongWord';

    TypeDefReplace['long long'] := 'Int64';
    TypeDefReplace['singned long long'] := 'Int64';
    TypeDefReplace['unsigned long long'] := 'Int64';
    TypeDefReplace['int64_t'] := 'Int64';
    TypeDefReplace['uint64_t'] := 'Int64';

    PtrTypeReplace['long long'] := 'PInt64';
    PtrTypeReplace['singned long long'] := 'PInt64';
    PtrTypeReplace['unsigned long long'] := 'PInt64';
    PtrTypeReplace['int64_t'] := 'PInt64';
    PtrTypeReplace['uint64_t'] := 'PInt64';

    TypeDefReplace['float'] := 'Single';
    TypeDefReplace['CGFloat'] := 'Single';
    PtrTypeReplace['double'] := 'PDouble';
    PtrTypeReplace['float'] := 'PSingle';
    PtrTypeReplace['CGFloat'] := 'PSingle';

    TypeDefReplace['Class'] := '_Class';

    TypeDefReplace['SRefCon'] := 'Pointer';
    TypeDefReplace['va_list'] := 'array of const';

    TypeDefReplace['uint8_t']:='byte';
    TypeDefReplace['unsigned long long']:='Int64';
    TypeDefReplace['long long']:='Int64';
    TypeDefReplace['signed long long']:='Int64';
    TypeDefReplace['unsigned']:='LongWord';
    PtrTypeReplace['uint8_t']:='Pbyte';
    PtrTypeReplace['unsigned long long']:='PInt64';
    PtrTypeReplace['long long']:='Int64';
    PtrTypeReplace['signed long long']:='PInt64';
    PtrTypeReplace['unsigned']:='PLongWord';

    StructTypes.Add('Int64');
  end;
end;

function TConvertSettings.GetCallConv(withSemiColon: Boolean): AnsiString;
begin
  Result := CallConv;
  if (Result <> '') and withSemiColon then
    Result := Result + ';';
end;

function TConvertSettings.GetObjCClassReplaceName(const ObjCName: AnsiString): AnsiString;
begin
  if UseRefClassType then Result := ObjCName + RefClassPostfix
  else Result := ObjcIDReplace;
end;

procedure TConvertSettings.SetCallConv(const ACallConv: String);
begin
  fCallConv := ACallConv;
  if AnsiLowerCase(fCallConv) = 'cdecl' then
    fExternPrefix := ''
  else
    fExternPrefix := '_';
end;

{ TReplaceList }

constructor TReplaceList.Create;
begin
  inherited Create;
  fItems := TStringList.Create;
end;

destructor TReplaceList.Destroy;
begin
  fItems.Free;
  inherited;
end;

function TReplaceList.GetCaseSense: Boolean;
begin
  Result := fItems.CaseSensitive;
end;

procedure TReplaceList.SetCaseSense(AValue: Boolean);
begin
  fITems.CaseSensitive := AValue;
end;

function TReplaceList.GetReplace(const ARepl: AnsiString): AnsiString;
var
  i : integer;
begin
  i := fItems.IndexOf(ARepl);
  if i < 0 then Result := ''
  else Result := TReplaceItem(fItems.Objects[i]).ReplaceStr;
end;

procedure TReplaceList.GetReplaces(strings: TStrings);
var
  i  : Integer;
  s  : AnsiString;
const
  EmptyString = ' ';
begin
  for i := 0 to fItems.Count - 1 do begin
    s := TReplaceItem(fItems.Objects[i]).ReplaceStr;
    if s = '' then s := EmptyString; // otherwise it's lost
    strings.Values[ fitems[i]] := s;
  end;
end;

procedure TReplaceList.SetReplace(const ARepl, AValue: AnsiString);
var
  i   : integer;
  it  : TReplaceItem;
begin
  i := fItems.IndexOf(ARepl);
  if i < 0 then begin
    it := TReplaceItem.Create;
    it.ReplaceStr := AValue;
    fItems.AddObject(Arepl, it);
  end else
    TReplaceItem(fItems.Objects[i]).ReplaceStr := AValue;
end;


initialization
  ConvertSettings := TConvertSettings.Create;
  InitConvertSettings;

finalization
  ConvertSettings.Free;

end.
