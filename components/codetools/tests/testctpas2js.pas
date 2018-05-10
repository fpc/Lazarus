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
  CodeCache, TestGlobals, LazLogger, LazFileUtils, LazUTF8, fpcunit,
  testregistry;

type

  { TCustomTestPas2js }

  TCustomTestPas2js = class(TTestCase)
  private
    FAutoSearchPas2js: boolean;
    FCode: TCodeBuffer;
    FPas2jsFilename: string;
    FUnitSetCache: TFPCUnitSetCache;
    FVirtualDirDefines: TDefineTemplate;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
    procedure DoParseModule(aCode: TCodeBuffer; out Tool: TCodeTool); virtual;
  public
    constructor Create; override;
    procedure Add(const s: string);
    procedure Add(Args: array of const);
    function FindPas2js: string;
    function StartProgram: boolean; virtual;
    procedure ParseModule; virtual;
    procedure WriteSource(CleanPos: integer; Tool: TCodeTool);
    procedure WriteSource(const CursorPos: TCodeXYPosition);
    property AutoSearchPas2js: boolean read FAutoSearchPas2js write FAutoSearchPas2js;
    property Code: TCodeBuffer read FCode;
    property Pas2jsFilename: string read FPas2jsFilename write FPas2jsFilename;
    property UnitSetCache: TFPCUnitSetCache read FUnitSetCache write FUnitSetCache;
    property VirtualDirDefines: TDefineTemplate read FVirtualDirDefines write FVirtualDirDefines;
  end;

  { TTestPas2js }

  TTestPas2js = class(TCustomTestPas2js)
  published
    procedure TestPas2js_ReadSettings;
    procedure TestPas2js_FindDeclaration;
  end;

implementation

{ TCustomTestPas2js }

procedure TCustomTestPas2js.SetUp;
var
  CurUnitSet: TFPCUnitSetCache;
  UnitSetID: String;
begin
  inherited SetUp;
  if (Pas2jsFilename='') and AutoSearchPas2js then begin
    FPas2jsFilename:=FindPas2js;
    AutoSearchPas2js:=false;
  end;
  if FPas2jsFilename<>'' then begin
    if UnitSetCache=nil then begin
      UnitSetCache:=CodeToolBoss.CompilerDefinesCache.FindUnitSet(Pas2jsFilename,
        '','','','',true);
      // parse compiler settings
      UnitSetCache.Init;
    end;
    UnitSetID:=UnitSetCache.GetUnitSetID;

    // set pas2js for virtual directory
    if VirtualDirDefines=nil then begin
      VirtualDirDefines:=TDefineTemplate.Create(
        'VirtualDirPas2js', 'set pas2js as compiler for virtual directory',
        '',VirtualDirectory,da_Directory);
      VirtualDirDefines.AddChild(TDefineTemplate.Create('UnitSet','UnitSet identifier',
                      UnitSetMacroName,UnitSetID,da_DefineRecurse));
      CodeToolBoss.DefineTree.Add(VirtualDirDefines);
    end;

    // check
    CurUnitSet:=CodeToolBoss.GetUnitSetForDirectory('');
    if CurUnitSet=nil then
      Fail('CodeToolBoss.GetUnitSetForDirectory=nil');
    if CurUnitSet<>UnitSetCache then
      AssertEquals('UnitSet VirtualDirectory should be pas2js',UnitSetID,CurUnitSet.GetUnitSetID);
  end;
  FCode:=CodeToolBoss.CreateFile('test1.pas');
end;

procedure TCustomTestPas2js.TearDown;
begin
  FCode:=nil;
  CodeToolBoss.DefineTree.RemoveDefineTemplate(VirtualDirDefines);

  inherited TearDown;
end;

procedure TCustomTestPas2js.DoParseModule(aCode: TCodeBuffer; out
  Tool: TCodeTool);
var
  i: Integer;
  Line: String;
begin
  if not CodeToolBoss.Explore(aCode,Tool,true) then begin
    debugln(aCode.Filename+'------------------------------------------');
    for i:=1 to aCode.LineCount do begin
      Line:=aCode.GetLine(i-1,false);
      if i=CodeToolBoss.ErrorLine then
        System.Insert('|',Line,CodeToolBoss.ErrorColumn);
      debugln(Format('%:4d: ',[i]),Line);
    end;
    debugln('Error: '+CodeToolBoss.ErrorDbgMsg);
    Fail('PascalParser failed: '+CodeToolBoss.ErrorMessage);
  end;
end;

constructor TCustomTestPas2js.Create;
begin
  inherited Create;
  FAutoSearchPas2js:=true;
end;

procedure TCustomTestPas2js.Add(const s: string);
begin
  FCode.Source:=FCode.Source+s+LineEnding;
end;

procedure TCustomTestPas2js.Add(Args: array of const);
begin
  FCode.Source:=FCode.Source+LinesToStr(Args);
end;

function TCustomTestPas2js.FindPas2js: string;
var
  ShortFilename: String;
begin
  Result:=GetEnvironmentVariable('PAS2JS');
  if Result<>'' then begin
    if not FileExistsUTF8(Result) then
      Fail('Environment variable PAS2JS is non existing file "'+Result+'"');
    exit;
  end;
  ShortFilename:='pas2js'+ExeExt;
  Result:=SearchFileInPath(ShortFilename,'',
                           GetEnvironmentVariableUTF8('PATH'),PathSeparator,ctsfcDefault);
end;

function TCustomTestPas2js.StartProgram: boolean;
begin
  if FPas2jsFilename='' then exit(false);
  Result:=true;
  AssertEquals('compiler kind',PascalCompilerNames[pcPas2js],PascalCompilerNames[UnitSetCache.GetCompilerKind]);
end;

procedure TCustomTestPas2js.ParseModule;
var
  Tool: TCodeTool;
begin
  Add('end.');
  DoParseModule(Code,Tool);
end;

procedure TCustomTestPas2js.WriteSource(CleanPos: integer; Tool: TCodeTool);
var
  Caret: TCodeXYPosition;
begin
  if Tool=nil then
    Fail('TCustomTestPas2js.WriteSource: missing Tool');
  if not Tool.CleanPosToCaret(CleanPos,Caret) then
    Fail('TCustomTestPas2js.WriteSource: invalid cleanpos '+IntToStr(CleanPos)+' Tool='+Tool.MainFilename);
  WriteSource(Caret);
end;

procedure TCustomTestPas2js.WriteSource(const CursorPos: TCodeXYPosition);
var
  CurCode: TCodeBuffer;
  i: Integer;
  Line: String;
begin
  CurCode:=CursorPos.Code;
  if CurCode=nil then
    Fail('TCustomTestPas2js.WriteSource CurCode=nil');
  for i:=1 to CurCode.LineCount do begin
    Line:=CurCode.GetLine(i-1,false);
    if (i=CursorPos.Y) then begin
      write('*');
      Line:=LeftStr(Line,CursorPos.X-1)+'|'+copy(Line,CursorPos.X,length(Line));
    end;
    writeln(Format('%:4d: ',[i]),Line);
  end;
end;

{ TTestPas2js }

procedure TTestPas2js.TestPas2js_ReadSettings;
var
  Cfg: TPCTargetConfigCache;
  aFilename, AnUnitName, InFilename, SystemUnit: String;
begin
  if Pas2jsFilename='' then exit;

  AssertEquals('compiler kind',PascalCompilerNames[pcPas2js],PascalCompilerNames[UnitSetCache.GetCompilerKind]);
  Cfg:=UnitSetCache.GetConfigCache(false);
  if not Cfg.Defines.Contains('PAS2JS_FULLVERSION') then
    Fail('macro PAS2JS_FULLVERSION is misssing');
  SystemUnit:=Cfg.Units['system'];
  if SystemUnit='' then
    Fail('pas2js.cfg is missing path to system unit');

  AnUnitName:='system';
  InFilename:='';
  aFilename:=CodeToolBoss.DirectoryCachePool.FindUnitSourceInCompletePath('',AnUnitName,InFilename,true);
  if aFilename='' then
    Fail('system unit not found from virtual directory');
  if CompareFilenames(aFilename,SystemUnit)<>0 then
    AssertEquals('pas2js system unit',SystemUnit,aFilename);
end;

procedure TTestPas2js.TestPas2js_FindDeclaration;
begin
  if not StartProgram then exit;
  Add([
  'var Cow: longint;',
  'begin',
  '  cow{declaration:Cow}:=3;',
  '  test1{declaration:Test1}.cow{declaration:Cow}:=3;',
  'end.',
  '']);
  ParseModule;
  //FindDeclarations(Code);
end;

initialization
  RegisterTest(TTestPas2js);
end.

