{
 Test all with:
   ./runtests --format=plain --suite=TTestPas2js

 Test specific with:
   ./runtests --format=plain --suite=TestPas2js_ReadSettings
}
unit TestCTPas2js;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, CodeToolManager, FileProcs, DefineTemplates, LinkScanner,
  LazLogger, LazFileUtils, LazUTF8, fpcunit, testregistry;

type

  { TCustomTestPas2js }

  TCustomTestPas2js = class(TTestCase)
  private
    FAutoSearchPas2js: boolean;
    FPas2jsFilename: string;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  public
    constructor Create; override;
    function FindPas2js: string;
    property AutoSearchPas2js: boolean read FAutoSearchPas2js write FAutoSearchPas2js;
    property Pas2jsFilename: string read FPas2jsFilename write FPas2jsFilename;
  end;

  { TTestPas2js }

  TTestPas2js = class(TCustomTestPas2js)
  published
    procedure TestPas2js_ReadSettings;
  end;

implementation

{ TCustomTestPas2js }

procedure TCustomTestPas2js.SetUp;
begin
  inherited SetUp;
  if (Pas2jsFilename='') and AutoSearchPas2js then begin
    FPas2jsFilename:=FindPas2js;
    AutoSearchPas2js:=false;
  end;
  //CodeToolBoss.CompilerDefinesCache;
end;

procedure TCustomTestPas2js.TearDown;
begin
  inherited TearDown;
end;

constructor TCustomTestPas2js.Create;
begin
  inherited Create;
  FAutoSearchPas2js:=true;
end;

function TCustomTestPas2js.FindPas2js: string;
var
  ShortFilename: String;
begin
  Result:=GetEnvironmentVariable('PAS2JS');
  if Result<>'' then begin
    if not FileExistsUTF8(Result) then
      Fail('Environment variable PAS2JS has non existing "'+Result+'"');
    exit;
  end;
  ShortFilename:='pas2js'+ExeExt;
  Result:=SearchFileInPath(ShortFilename,'',
                           GetEnvironmentVariableUTF8('PATH'),PathSeparator,ctsfcDefault);
end;

{ TTestPas2js }

procedure TTestPas2js.TestPas2js_ReadSettings;
var
  UnitSetCache: TFPCUnitSetCache;
begin
  if Pas2jsFilename='' then exit;

  exit;
  UnitSetCache:=CodeToolBoss.CompilerDefinesCache.FindUnitSet(Pas2jsFilename,
    '','','','',true);
  // parse compiler settings
  UnitSetCache.Init;
  AssertEquals('compiler kind',dbgs(pcPas2js),dbgs(UnitSetCache.GetCompilerKind));
end;

initialization
  RegisterTest(TTestPas2js);
end.

