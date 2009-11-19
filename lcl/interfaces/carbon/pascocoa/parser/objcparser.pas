{ * This file is part of ObjCParser tool
  * Copyright (C) 2008-2009 by Dmitry Boyarintsev under the GNU LGPL
  * license version 2.0 or 2.1.  You should have received a copy of the
  * LGPL license along with at http://www.gnu.org/
}

program objcparser;

{$ifdef fpc}
  {$mode delphi}{$H+}
{$else}
  {$APPTYPE CONSOLE}
  {$warn unsafe_code off}
  {$warn unsafe_type off}
  {$warn unsafe_cast off}
{$endif}

uses
  Classes, 
  IniFiles,
  SysUtils,
  ObjCParserUtils,
  ObjCParserTypes,
  ObjCTemplate,
  gnuccFeatures;

type
  // this object is used only for precomile directives handling

  { TPrecompileHandler }
  TPrecompileHandler = class(TObject)
  public
    hdr   : TObjCHeader;
    procedure OnPrecompile(Sender: TObject; Precomp: TObject);
    procedure OnComment(Sender: TObject; const Comment: AnsiString);
    constructor Create(AHeader: TObjCHeader);
  end;

var
  updIni      : AnsiString = '';
  doOutput    : Boolean = false;
  doparseAll  : Boolean = false;

const
  TokenReplaceSec = 'TokenReplace';
  TypeDefsSec = 'TypeDefs';
  TypeReplaceSec = 'TypeReplace';
  IgnoreIncludesSec = 'IgnoreIncludes';
  CommonSec = 'Common';

function FindMax(const c: array of Integer; len: Integer): Integer;
var
  i   : integer;
  mn  : Integer;
begin
  Result := -1;
  if len = 0 then Exit;

  mn := 0;
  for i := 1 to len - 1 do begin
    if c[i] < c[mn] then mn := i;
  end;
  Result := mn;
end;

procedure TPrecompileHandler.OnPrecompile(Sender: TObject; Precomp: TObject);
var
  parser    : TTextParser;
  preEntity : TPrecompiler;
  lst       : TEntity;
  prc       : TPrecompilerEvent;
begin
  parser := Sender as TTextParser;
  //todo: change for something nicier =)
  prc := parser.OnPrecompile;
  parser.OnPrecompile := nil;
  try
    if parser.Stack.Count > 0 then
      lst := TEntity(parser.Stack[parser.Stack.Count-1])
    else
      lst := nil;

    preEntity := TPrecompiler.Create(lst);
    preEntity.Parse(parser);
    lst.Items.Add(preEntity);
  finally
    parser.OnPrecompile := prc;
  end;
end;

procedure TPrecompileHandler.OnComment(Sender: TObject; const Comment: AnsiString);
var
  parser  : TTextParser;
  cmt     : TComment;
  ent     : TEntity;
begin
  if length(Comment) < 2 then Exit;
  parser := TTextParser(Sender);

  if parser.Stack.Count > 0
    then ent := TEntity(parser.Stack[parser.Stack.Count-1])
    else ent := nil;

  if not Assigned(ent) then Exit;
  cmt := TComment.Create(ent);
  cmt._Comment := Comment;
  if IsSubStr('/*', cmt._Comment, 1) then begin
    cmt._Comment[1] := '(';
    if isSubStr('*/', cmt._Comment, length(cmt._Comment) - 1) then
      cmt._Comment[ length(cmt._Comment)] := ')';
  end;
  ent.Items.Add(cmt);
end;

constructor TPrecompileHandler.Create(AHeader: TObjCHeader);
begin
  hdr := AHeader;
end;

procedure UpdateIniWithEntity(Sets: TConvertSettings; Ini: TIniFile; Entity: TEntity);
var
  cnv : AnsiString;
  i   : Integer;
begin
  if Entity is TClassDef then begin
    Ini.WriteString(TypeDefsSec, TClassDef(Entity)._ClassName, 'objcclass');
  end else if Entity is TEntityStruct then begin
    Ini.WriteString(TypeDefsSec, TEntityStruct(Entity)._Name, 'struct');
  end else if Entity is TTypeNameDef then begin
    if Assigned(Sets) then begin
      cnv := AnsiLowerCase(ObjCToDelphiType(TTypeNameDef(Entity)._Inherited, false ));
      if (cnv = 'float') or (cnv = 'double') then
        Ini.WriteString(TypeDefsSec, TTypeNameDef(Entity)._TypeName, 'float')
      else if (cnv = 'Int64') then
        Ini.WriteString(TypeDefsSec, TTypeNameDef(Entity)._TypeName, 'struct')
    end;
  end;

  for i := 0 to Entity.Items.Count - 1 do
    UpdateIniWithEntity(Sets, Ini, Entity.Items[i]);
end;

function ReadAndParseFile(const FileName: AnsiString; outdata: TStrings; var Err: AnsiString): Boolean;
var
  hdr     : TObjCHeader;
  parser  : TTextParser;
  prec    : TPrecompileHandler ;
  s       : AnsiString;
  i, cnt  : integer;
  upini   : TIniFile;

  repl    : TStringList;
begin
  Result :=false;
  if not FileExists(FileName) then begin
    Err :=  'File not found: ' + FileName;
    Exit;
  end;

  s := StrFromFile(FileName);
  hdr := TObjCHeader.Create;
  prec := TPrecompileHandler.Create(hdr);
  parser := CreateCParser(s, true);
  try
    repl := TStringList.Create;
    ConvertSettings.TokenReplace.GetReplaces(repl);
    for i := 0 to repl.Count - 1 do begin
      TCMacroHandler(parser.MacroHandler).AddSimpleMacro(repl.Names[i], repl.ValueFromIndex[i]);
    end;
    parser.Buf := s;
    try
      parser.UsePrecompileEntities := false;
      parser.UseCommentEntities := false;
      parser.OnPrecompile := prec.OnPrecompile;
      parser.OnComment := prec.OnComment;

      {for i := 0 to repl.Count - 1 do begin
        TCMacroHandler(parser.MacroHandler).AddSimpleMacro(
          ConvertSettings.IgnoreTokens[i], '');
      //parser.IgnoreTokens.AddStrings(ConvertSettings.IgnoreTokens);
      end;}

      hdr._FileName := ExtractFileName(FileName);
      Result := hdr.Parse(parser);
      if not Result then begin
        if parser.Errors.Count > 0 then Err := parser.Errors[0]
        else Err := 'undesribed error';

        Err := Err + #13#10;
        cnt := 120;
        i := parser.Index - cnt;
        if i <= 0 then begin
          i := 1;
          cnt := parser.Index;
        end;
        Err := Err + Copy(parser.Buf, i, cnt);
      end;

    except
    end;

    if updIni <> '' then begin
      upIni := TIniFile.Create(updIni);
      try
        UpdateIniWithEntity(ConvertSettings, upIni, hdr);
      finally
        upIni.Free;
      end;
    end;
    WriteOutIncludeFile(hdr, outdata);
  finally
    parser.TokenTable.Free;
    parser.Free;
    prec.Free;
    //FreeEntity(hdr);
  end;
end;

procedure ParseAll;
var
//  ch    : char;
  srch  : TSearchRec;
  res   : Integer;
  i     : Integer;
  pth   : AnsiString;
  incs  : AnsiString;
  st    : TStringList;
  f     : Text;
  err   : AnsiString;
begin
 { err := '';
  writeln('would you like to parse all current directory files .h to inc?');
  readln(ch);
  if (ch <> 'Y') and (ch <> 'y') then begin
    writeln('as you wish, bye!');
    Exit;
  end;}

  pth := IncludeTrailingPathDelimiter( GetCurrentDir);
  res := FindFirst(pth + '*.h', -1, srch);
  if res = 0 then begin
    st := TStringList.Create;
    try
      repeat
        write('found: ', srch.Name);
        write(' parsing...');
        //writeln('parsing: ', pth+srch.Name);
        if ReadAndParseFile(pth+srch.Name, st, err) then begin
          write(' parsed ');
          incs := pth + Copy(srch.Name,1, length(srch.Name) - length(ExtractFileExt(srch.Name)));
          incs := incs + '.inc';
          //writeln(incs);
          if doOutput then begin
            assignfile(f, incs); rewrite(f);
            try
              for i := 0 to st.Count - 1 do
                writeln(f, st[i]);
            finally
              closefile(f);
            end;
          end;

          st.Clear;
        end else begin
        end;
      until FindNext(srch) <> 0;

    finally
      FindClose(srch);
      st.Free;
    end;
  end;
end;

const
  ParamKey = '-';

function isParamValue(const s: AnsiString; var ParName, ParValue: AnsiString): Boolean;
var
  i   : Integer;
begin
  Result := false;
  if s = '' then Exit;
  Result := (s[1] = ParamKey);
  if not Result then Exit;
  i := 1;
  ScanWhile(s, i, [ParamKey]);
  ParName := ScanTo(s, i, [#32, #9, '=']);
  ScanWhile(s, i, [#32, #9, '=']);
  ParValue := Copy(s, i, length(s) - i + 1);
end;

procedure AddSpaceSeparated(const s: AnsiString; Strings: TStringList);
var
  i   : Integer;
  ns  : AnsiString;
begin
  i := 1;
  while i <= length(s) do begin
    ScanTo(s, i, ['A'..'Z', 'a'..'z']);
    ns := ScanTo(s, i, [#32, #9, '"']);
    if ns <> '' then Strings.Add(ns);
  end;
end;


function isNameofPointer(const name: AnsiString): Boolean;
begin
  Result := false;
  if name = '' then Exit;
  Result := name[length(name)] = '*';
end;

procedure ReadIniFile(Settings: TConvertSettings; const FileName: AnsiString);
var
  ini     : TIniFile;
  values  : TStringList;
  a, b    : AnsiString;
  i       : Integer;
  IniName : AnsiString;
begin
//  uikit.ini
  if not FileExists(FileName) then begin
    Exit;
  end;
  {$ifndef fpc}
  if ExtractFileName(FileName) = FileName then
    IniName := IncludeTrailingPathDelimiter( GetCurrentDir) + FileName
  else
    IniName := FileName;
  {$else}
  IniName := FileName;
  {$endif}
  ini := TIniFile.Create(IniName);
  values  := TStringList.Create;
  try
    values.Clear;
{    ini.ReadSection('TypeReplace', values);
    for i := 0 to values.Count - 1 do begin
      a := values.ValueFromIndex[i];
      b := values.Values[a];
      if b <> '' then begin
      ense
      Settings.TypeDefReplace[a] := b;
    end;}

    //[Common]
    values.Clear;
    a := ini.ReadString(CommonSec, 'mainunit', '');
    if a <> '' then begin
      b := '{%mainunit '+ a + '}';
      for i := 0 to ConvertSettings.ConvertPrefix.Count - 1 do
        if Pos(ConvertSettings.ConvertPrefix[i], '{%mainunit') = 1 then begin
          ConvertSettings.ConvertPrefix[i] := b;
          a := '';
          Break;
        end;
      if a <> '' then
        ConvertSettings.ConvertPrefix.Add(b);
    end;

    a := ini.ReadString(CommonSec, 'ignoreincludes', '');
    ini.ReadSection('Common', values);
    for i := 0 to values.Count - 1 do begin
      if Pos('ignoreincludes', values[i]) = 1 then begin
        b := ini.ReadString(CommonSec,values[i], '');
        AddSpaceSeparated(b, ConvertSettings.IgnoreIncludes);
      end;
    end;
    {ini.ReadSectionValues(IgnoreIncludesSec, values);
    for i := 0 to values.Count - 1 do begin
      ConvertSettings.IgnoreIncludes.AddStrings(values);
    end;}

    // [TokenReplace]
    Values.Clear;
    ini.ReadSection(TokenReplaceSec, values);
    for i := 0 to values.Count - 1 do begin
      a := Values[i];
      b := ini.ReadString(TokenReplaceSec, a, '');
      {if b ='' then
        Settings.IgnoreTokens.Add(a)
      else}
      Settings.TokenReplace[a] := b;
    end;

    // [TypeReplace]
    values.Clear;
    ini.ReadSection(TypeDefsSec, values);
    for i := 0 to values.Count - 1 do begin
      a := Values[i];
      b := AnsiLowerCase(ini.ReadString(TypeDefsSec, a, ''));
      if b = 'objcclass' then
        Settings.ObjCClassTypes.Add(a)
      else if b = 'struct' then
        Settings.StructTypes.Add(a)
      else if b = 'float' then
        Settings.FloatTypes.Add(a);
    end;

    values.Clear;
    ini.ReadSection(TypeReplaceSec, values);
    for i := 0 to values.Count - 1 do begin
      a := Values[i];
      b := ini.ReadString(TypeReplaceSec, a, '');
      if isNameofPointer(a) then
        Settings.PtrTypeReplace[ Copy(a, 1, length(a) - 1)] := b
      else
        Settings.TypeDefReplace[a] := b;
    end;

  finally
    values.Free;
    ini.Free;
  end;
end;


function GetConvertSettings(Settings : TConvertSettings; var FileName: AnsiString): Boolean;
var
  i   : integer;
  prm : AnsiString;
  vlm : AnsiString;
  Params  : TStringList;
begin
  prm := '';
  vlm := '';
  Params := TStringList.Create;
  Params.CaseSensitive := false;
  try
    for i := 1 to ParamCount do begin
      if isParamValue(ParamStr(i), prm, vlm) then begin
        prm := AnsiLowerCase(prm);
        if prm = 'noout' then doOutput:=false
        else if prm = 'all' then doparseAll:=true
        else if (prm = 'id') and (vlm <> '') then ConvertSettings.ObjcIDReplace:=vlm
        else if (prm = 'call') then ConvertSettings.CallConv:=vlm
        else if (prm = 'userefs') then ConvertSettings.UseRefClassType := true
        else if (prm = 'refpostfix') then ConvertSettings.RefClassPostfix := vlm
        else if prm = 'ini' then begin
          ReadIniFile(Settings, vlm);
        end else
          Params.Values[prm] := vlm;
      end else
        FileName := ParamStr(i);
    end;

    vlm := Params.Values['uini'];
    if vlm <> '' then
      updIni := vlm;


  finally
    Params.Free;
  end;
  Result := true;
end;

procedure TypeHelp;
begin
  writeln('Obj-C parser usage:');
  writeln('objcparser [switches] objcheaderfilename');
  writeln('');
  writeln('keys:');
  writeln(' -ini=filename.ini  config file to use for pascal file generation');
  writeln('                    multiple "-ini" switches are allowed');
  writeln(' -uini=filename.ini config file to update the data');
  writeln(' -noout             prevents from .inc files generated');
  writeln(' -all               parses headers (*.h) in the current directory');
  writeln('');
  writeln(' hidden keys (they''re temporary, and will be removed in future versions)');
  writeln(' -id=IDENTIFIER     the identifier to replace objective-c id type name');
  writeln('                    default = objc.id');
  writeln(' -call=IDENTIFIER   specifies the function''s calling convention.');
  writeln('                    default is cdecl. Please note, that calling convention');
  writeln('                    also effect external functions name. Thus, using ');
  writeln('                    if calling convention is not cdecl, the external name');
  writeln(' -useRefs           enables additional types to be created, for objc.id  ');
  writeln('                    replacements at the parameter and result types');
  writeln(' -refPostFix        post-fix for each ref type. The default postfix is ''Ref''');
end;

var
  inpf  : AnsiString = '';
  st    : TStrings = nil;
  err   : AnsiString = '';
  i     : integer;


function FileToString(const FileName: WideString): AnsiString;
var
  fs  : TFileStream; 
begin                 
  Result := '';
  try 
    fs := TfileStream.Create(FileName, fmOpenRead or fmShareDenyNone); 
    try
      SetLength(Result, fs.Size);
      fs.Read(Result[1], fs.Size) 
    finally
      fs.Free; 
    end; 
  except
  end;
end; 

{procedure DoTest(const InputFile: AnsiString);
var
  hdr   : TObjCHeader;

  wrt   : TStringsWriter;
  cnv   : TDefaultConverter;
  i     : Integer;
  names : TPascalNames;
begin
  hdr := TObjCHeader.Create;
  wrt := TStringsWriter.Create;
  wrt.Strings := TStringList.Create;
  try
    if not ParserCHeader( FileToString(InputFile), hdr) then Exit;

    cnv := TDefaultConverter.Create;
    names :=  CreateDefaultPascalNames;
    try
      cnv.WriteCHeader(hdr, wrt, names);
    finally
      cnv.Free;
    end;

    for i := 0 to wrt.Strings.Count - 1 do
      writeln(wrt.Strings[i]);

  finally
    wrt.Strings.Free;
    wrt.Free;
    hdr.Free;
  end;
end;}


procedure TestTemplate;
var
  fn  : TFileStream;
  tmp : AnsiString;

  tp  : TTemplateProc;
  s   : string;
  pv  : TPascalValues;
  root: TTemplateList;
  cl  : TTemplateList;
begin
  root:=TTemplateList.Create(nil);
  cl:=TTemplateList.Create(root);
  cl.Name :='class';
  cl.Params.Values['class_objcname'] := 'NSNotebook';
  cl.Params.Values['class_objcsupername'] := 'NSObject';

  root.SubLists.Add(cl);

  fn := TFileStream.Create('templatesample.txt', fmOpenRead or fmShareDenyNone);
  tp := TTemplateProc.Create;
  pv := TPascalValues.Create;
  try
    SetLength(tmp, fn.Size);
    fn.Read(tmp[1], fn.Size);


    s := tp.Parse(tmp, root, pv);
    writeln(s);
    readln;
  finally
    pv.Free;
    tp.Free;
    fn.Free;
  end;
end;

begin
//  TestTemplate;
//  Exit;

  doOutput := true;
  try
    GetConvertSettings(ConvertSettings, inpf);
    if doParseAll then begin
      ParseAll;
      Exit;
    end else if not FileExists(inpf) then begin
      TypeHelp;
      Exit;
    end;
   
    st := TStringList.Create;
    try
      if not ReadAndParseFile(inpf, st, err) then 
        writeln('Error: ', err)
      else begin
        if doOutput then
          for i := 0 to st.Count - 1 do
            writeln(st[i]);
      end;
    except
    end;
    st.Free;
  except
    on e: exception do
      writeln(e.Message);
  end;
end.


