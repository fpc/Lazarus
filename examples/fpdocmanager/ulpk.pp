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

//function ImportLpk(const AFile: string): boolean;
function ImportLpk(const AFile: string): TDocPackage;

implementation

//uses

type
  eKey = (kvEof, kvName, kvIncl, kvOther, kvFilename, kvDocPaths, kvReq
  );
const
  aKey: array[eKey] of string = (
    '', 'Name', 'IncludeFiles', 'OtherUnitFiles',
    'Filename', 'LazDoc' ,'PackageName'
  );
  FirstKeys = 'NIOFLP';

var
  f: TextFile;
  ln, value, ext: string;
  lt, eq, q2: integer;
  key: eKey;

function ImportCompiled(const LpkFile: string): boolean;
var
  mfc: string;
  f: TextFile;
begin
  mfc := ExtractFilePath(LpkFile) + 'Makefile.compiled';
  Result := FileExists(mfc);
  if not Result then
    exit;
//import Makefile.compiled
  AssignFile(f, mfc);
  Reset(f);
  try
  //parse
    ReadLn(f, ln);
    //todo...
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
    if Copy(ln, lt+1, Length(aKey[key])) <> aKey[key] then
      continue;
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
    //Manager.Package := pkg; //!DocPkg
  //remaining keys
    while GetLine do begin
      case key of
      kvName: ; //ignore any but first occurence
      kvIncl: pkg.IncludePath:=value;
      kvOther: pkg.UnitPath := value;
      kvFilename:
        begin
          ext := ExtractFileExt(value);
          if (ext = '.pas') or (ext = '.pp') then
            pkg.Units.Add(value); //!!! no dupes!?
        end;
      kvDocPaths: pkg.DescrDir := value;
      kvReq: pkg.Requires.Add(LowerCase(value));
      {
        begin
          ext := Manager.RootDir + '/' + value + ',../' + value;
          pkg.Imports.Add(ext);
        end;
      }
      end;
    end;
    Result := pkg;  // True;
  finally
    CloseFile(f);
  end;
end;

end.

