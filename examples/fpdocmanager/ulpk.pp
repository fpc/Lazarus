unit uLpk;
(* Convert LPK package into FPDoc project/package.
Relevant entries:
  <Name Value="LCLBase"/> //here: rename into LCL
  <IncludeFiles Value="..."/> //-Fi
  <OtherUnitFiles Value="forms;widgetset"/> //-Fu
  <CustomOptions Value="$(IDEBuildOptions)"/> ???
<Files Count="291">  //Item1..Item291
  <Filename Value="...pas"/>  //ignore .inc etc.
  <LazDoc Paths="../docs/xml/lcl"/>
<RequiredPkgs Count="1">
  <PackageName Value="LazUtils"/> //required
*)

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  uManager;

function ImportLpk(const AFile: string): TDocPackage;
//function ImportCompiled(const LpkFile: string): boolean;

implementation

uses
  umakeskel;

type
  eKey = (kvEof, kvName, kvIncl, kvOther, kvFilename, kvDocPaths, kvReq, kvTitle
  );
const
  aKey: array[eKey] of string = (
    '', 'Name', 'IncludeFiles', 'OtherUnitFiles',
    'Filename', 'LazDoc' ,'PackageName', 'Title'
  );
  FirstKeys = 'NIOFLPT';

var
  f: TextFile;
  ln, value, ext: string;
  lt, eq, q2: integer;
  key: eKey;

function ImportCompiled(const LpkFile: string; APkg: TDocPackage): boolean;
var
  mfc: string;
  f: TextFile;
begin
(* Makefile.compiled is an XML file, containing
  <Params Value=" ..." --> extract -Fi and other options.
  -Fu is quite unusable, contains units/%(CPU_TARGET)-%(OS_TARGET)
  other options seem omittable?

  Try use <prj>.compiled in unit output dir?
*)
  mfc := ExtractFilePath(LpkFile) + 'Makefile.compiled';
  Result := FileExists(mfc);
  if not Result then
    exit;
//import Makefile.compiled
  AssignFile(f, mfc);
  Reset(f);
  try
  //parse
    while not EOF(f) do begin
      ReadLn(f, ln);
      if Pos('<Params ', ln) < 1 then
        continue;
      eq := Pos('"', ln);
      if eq < 1 then
        continue;
      ln := Copy(ln, eq+1, Length(ln) - eq);
      while ln <> '' do begin
        value := GetNextWord(ln);
        if (Copy(value,1,2) = '-d') or (Copy(value,1,3) = '-Fi') then begin
          APkg.CompOpts := value; //collects all options
        end;
      end;
      //todo...
    end;
  finally
    CloseFile(f);
  end;
end;

function GetLine: boolean;
var
  i: integer;
begin
  while not EOF(f) do begin
    ReadLn(f, ln);
  //get key
    lt := Pos('<', ln);
    if lt <= 0 then
      continue;
  //filter key
    i := Pos(ln[lt+1], FirstKeys);
    if i < 1 then
      continue;
    key := eKey(i);
    if CompareText(Copy(ln, lt+1, Length(aKey[key])), aKey[key]) <> 0 then
      continue; //diff. case in "FileName", "Filename"
    if key = kvTitle then
      key := kvName; //LPR
  //check value
    eq := Pos('=', ln);
    if (eq <= lt) or (ln[eq+1] <> '"') then
      continue;
    q2 := Length(ln) - 2;
    if ln[q2] <> '"' then
      continue;
    value:=Copy(ln, eq+2, q2-eq-2);
    exit(True);
  end;
  Result := False;
end;

function ImportLpk(const AFile: string): TDocPackage;
var
  pkg: TDocPackage;
  dir: string;
begin
  Result := Nil;  // False; //assume fail
  AssignFile(f, AFile);
  Reset(f);
  try
  //read lines
  //get Name
    if not GetLine or (key <> kvName) then
      exit; //missing package name
  //fix case and LCLBase
    value := LowerCase(value);
    if value = 'lclbase' then
      value := 'lcl';
    pkg := Manager.AddPackage(value);
    pkg.LazPkg := AFile;
    dir := ExtractFilePath(AFile);
    pkg.ProjectDir := dir; //ChDir on exec
  //remaining keys
    while GetLine do begin
      case key of
      kvName: ; //ignore any but first occurence
      kvIncl: pkg.IncludePath:=value;
      kvOther: pkg.UnitPath := value;
      kvFilename:
        begin
          if not FileExists(dir + value) then
            continue;
          ext := ExtractFileExt(value);
          if (ext = '.pas') or (ext = '.pp') then
            //pkg.Units.Add(value + '='); //!!! no dupes!? no options so far?
            pkg.AddUnit(value);
        end;
      kvDocPaths: pkg.DescrDir := value;
      kvReq: pkg.Requires.Add(LowerCase(value));
      end;
    end;
    Result := pkg;  // True;
  finally
    CloseFile(f);
  end;
  ImportCompiled(AFile, pkg); //ignore result, is optional
end;

end.

