unit uLpk;
(* Convert LPK package into FPDoc project/package.
Relevant entries:
  <Package Version=n/>
    <Name Value="LCLBase"/> //here: rename into LCL - packages ONLY!
or
  <ProjectOptions>
    <MainUnit Values=n/>

  <IncludeFiles Value="..."/> //-Fi
  <OtherUnitFiles Value="forms;widgetset"/> //-Fu
  <CustomOptions Value="$(IDEBuildOptions)"/> ???
<Files Count="291">  //Item1..Item291
  <Filename Value="...pas"/>  //ignore .inc etc.
    <LazDoc Paths="docs" PackageName="LazDE"/>
<RequiredPkgs Count="1">
<RequiredPackages Count="1">
  <PackageName Value="LazUtils"/> //required
*)

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  uManager;

function ImportLpk(const AFile: string): TDocPackage;

var
  PkgName: string;

implementation

uses
  umakeskel;

type
  eKey = (kvEof, kvName, kvIncl, kvOther, kvFilename, kvLazDoc, kvReq
  );
const
  aKey: array[eKey] of string = (
    '', 'Name', 'IncludeFiles', 'OtherUnitFiles',
    'Filename', 'LazDoc' ,'PackageName'
  );
  FirstKeys = 'NIOFLP';

var
  f: TextFile;
  ln, skey, attr, value, ext: string;
  lt, att, eq, q2: integer;
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
    skey:=aKey[key]; //the real key
  //check value
    eq := Pos('=', ln);
    if (eq <= lt) or (ln[eq+1] <> '"') then
      continue;
  {$IFDEF SingleAttributeOnly}
    q2 := Length(ln) - 2;
    if ln[q2] <> '"' then
      continue;
    value:=Copy(ln, eq+2, q2-eq-2);
  {$ELSE}
  //extract attribute
    att := lt+Length(skey)+2;
    attr := Copy(ln, att, eq-att);
    value := Copy(ln, eq+2, Length(ln));
    q2 := Pos('"', value);
    if q2 < 1 then
      continue; //???
    ln := Copy(value, q2+2, Length(value)); //to be parsed next
    SetLength(value, q2-1);
  {$ENDIF}
    exit(True);
  end;
  Result := False;
end;

function NextAttr: boolean;
begin
  //ln := Copy(ln, q2+2, Length(ln)); //now contains remaining attributes
  eq := Pos('=', ln);
  Result := eq > 1;
  if not Result then
    exit;
  attr := Copy(ln, 1, eq-1);
  value := Copy(ln, eq+2, Length(ln));
  q2 := Pos('"', value);
  if q2 < 1 then
    exit(false);
  ln := Copy(value, q2+2, Length(value)); //to be parsed next
  SetLength(value, q2-1);
end;

function ImportLpk(const AFile: string): TDocPackage;
var
  pkg: TDocPackage;
  dir: string;
begin
  PkgName := LowerCase(ChangeFileExt(ExtractFileName(AFile), ''));
  Result := Nil;  // False; //assume fail
  AssignFile(f, AFile);
  Reset(f);
  try
  //read lines
  //get Name
    if not GetLine then
      exit; //missing package name
    if key = kvName then begin
    //lpk
    //fix case and LCLBase
      PkgName := LowerCase(value);
      if PkgName = 'lclbase' then
        PkgName:='lcl';
      pkg := Manager.AddPackage(PkgName);
    end else if key = kvLazDoc then begin
    //lpi
      if attr = 'Paths' then begin
        //pkg.DescrDir := value;
        dir := value;
        if not NextAttr then
          exit;
      end;
      if attr <> 'PackageName' then
        exit;
      PkgName := LowerCase(value);
      pkg := Manager.AddPackage(PkgName);
      pkg.DescrDir := dir;
    end else
      exit; //need pkg Name or PackageName
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
      kvLazDoc: //followed by Path and/or PackageName (must be handled before!)
        begin
          if attr = 'Paths' then
            pkg.DescrDir := value;
        end;
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

