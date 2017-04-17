{
 Test all with:
   ./runtests --format=plain --suite=TTestCodetoolsCompleteBlock

 Test specific with:
   ./runtests --format=plain --suite=TestCompleteBlockClassStart
   ./runtests --format=plain --suite=TestCompleteBlockBegin
   ./runtests --format=plain --suite=TestCompleteBlockRepeat
   ./runtests --format=plain --suite=TestCompleteBlockCase
   ./runtests --format=plain --suite=TestCompleteBlockTry
   ./runtests --format=plain --suite=TestCompleteBlockAsm
   ./runtests --format=plain --suite=TestCompleteBlockIf
}
unit TestCompleteBlock;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry, testglobals, FileProcs,
  CodeToolManager, CodeCache, CustomCodeTool;

type

  { TTestCodetoolsCompleteBlock }

  TTestCodetoolsCompleteBlock = class(TTestCase)
  private
    function CreateFullSrc(Src: string; out Cursor: integer): string;
    procedure TestCompleteBlocks;
  public
    procedure CompleteBlock(Src, ExpectedSrc: string;
                OnlyIfCursorBlockIndented: boolean = false);
    procedure CompleteBlock(Src: string; OnlyIfCursorBlockIndented: boolean = false);
    procedure CompleteBlockFail(Src: string;
                OnlyIfCursorBlockIndented: boolean = false);
  published
    procedure TestCompleteBlockClassStart;
    procedure TestCompleteBlockBegin;
    procedure TestCompleteBlockRepeat;
    procedure TestCompleteBlockCase;
    procedure TestCompleteBlockTry;
    procedure TestCompleteBlockAsm;
    procedure TestCompleteBlockIf;
  end;

implementation

{ TTestCodetoolsCompleteBlock }

procedure TTestCodetoolsCompleteBlock.TestCompleteBlocks;

  procedure CompareComplete(a,b,c: string);
  begin
    writeln('CompareComplete ',a,',',b,',',c);
  end;

begin
  CompareComplete('ifbeginelse1.inc','6 28 ifbeginelse fpcunit', 'ifbeginelse1_result.inc');
  CompareComplete('whilebegin1.inc','5 10 whilebegin fpcunit', 'whilebegin1_result.inc');
  CompareComplete('beginwithoutindent1.inc','4 21 beginwithoutindent fpcunit', 'beginwithoutindent1_result1.inc');
  CompareComplete('beginwithoutindent1.inc','5 6 beginwithoutindent fpcunit', 'beginwithoutindent1_result2.inc');
  CompareComplete('casecolon1.inc','5 5 casecolon fpcunit', 'casecolon1_result.inc');
  CompareComplete('caseelseend1.inc','5 7 caseelseend fpcunit', 'caseelseend1_result.inc');
  CompareComplete('caseend1.inc','4 12 caseend fpcunit', 'caseend1_result.inc');
  CompareComplete('class1.inc','3 19 class fpcunit', 'class1_result.inc');
  CompareComplete('ifbegin1.inc','4 21 ifbegin fpcunit', 'ifbegin1_result.inc');
  CompareComplete('ifbeginelse1.inc','6 28 ifbeginelse fpcunit', 'ifbeginelse1_result.inc');
  CompareComplete('procedurebegin1.inc','3 6 procedurebegin fpcunit', 'procedurebegin1_result.inc');
  CompareComplete('procedurebeginend1.inc','4 8 procedurebeginend fpcunit', 'procedurebeginend1_result.inc');
  CompareComplete('procedurebeginifbegin1.inc','11 74 procedurebeginifbegin fpcunit', 'procedurebeginifbegin1_result.inc');
  CompareComplete('record1.inc','3 22 record fpcunit', 'record1_result1.inc');
  CompareComplete('repeatifelse1.inc','11 18 repeatifelse fpcunit', 'repeatifelse1_result.inc');
  CompareComplete('tryif1.inc','4 6 tryif fpcunit', 'tryif1_result.inc');
end;

function TTestCodetoolsCompleteBlock.CreateFullSrc(Src: string;
  out Cursor: integer): string;
begin
  Result:=Src;
  {Result:='unit testcompleteblock;'+LineEnding
         +'interface'+LineEnding
         +Src;}
  if not (Result[length(Result)] in [#10,#13]) then
    Result:=Result+LineEnding;
  Cursor:=System.Pos('|',Result);
  System.Delete(Result,Cursor,1);
end;

procedure TTestCodetoolsCompleteBlock.CompleteBlock(Src, ExpectedSrc: string;
  OnlyIfCursorBlockIndented: boolean);
var
  Code: TCodeBuffer;
  p: integer;
  Y: integer;
  X: integer;
  NewCode: TCodeBuffer;
  NewX: integer;
  NewY: integer;
  NewTopLine: integer;
  ExpectedCode: TCodeBuffer;
  ep: integer;
  eY: integer;
  eX: integer;
  FullSrc: String;
  FullExpectedSrc: String;
  TrimExpected: String;
  TrimResult: String;
begin
  AssertEquals('Src is empty',Trim(Src)<>'',true);
  AssertEquals('ExpectedSrc is empty',Trim(ExpectedSrc)<>'',true);

  ExpectedCode:=TCodeBuffer.Create;
  try
    // replace cursor | marker in Src
    Code:=CodeToolBoss.CreateFile('TestCompleteBlock.pas');
    FullSrc:=CreateFullSrc(Src,p);
    if p<1 then
      AssertEquals('missing cursor | in test source: "'+dbgstr(Src)+'"',true,false);
    Code.Source:=FullSrc;
    Code.AbsoluteToLineCol(p,Y,X);

    // replace cursor | marker in ExpectedSrc
    FullExpectedSrc:=CreateFullSrc(ExpectedSrc,ep);
    if ep<1 then
      AssertEquals('missing cursor | in expected source: "'+dbgstr(ExpectedSrc)+'"',true,false);
    ExpectedCode.Source:=FullExpectedSrc;
    ExpectedCode.AbsoluteToLineCol(ep,eY,eX);

    if not CodeToolBoss.CompleteBlock(Code,X,Y,OnlyIfCursorBlockIndented,
      NewCode,NewX,NewY,NewTopLine)
    then begin
      AssertEquals('CodeToolBoss.CompleteBlock returned false for src="'+dbgstr(Src)+'"',true,false);
      exit;
    end;
    TrimExpected:=dbgstr(Trim(FullExpectedSrc));
    TrimResult:=dbgstr(Trim(Code.Source));
    if TrimExpected<>TrimResult then begin
      debugln(['TTestCodetoolsCompleteBlock.CompleteBlock FAILED Expected:']);
      debugln(FullExpectedSrc);
      debugln(['TTestCodetoolsCompleteBlock.CompleteBlock FAILED Found:']);
      debugln(Code.Source);
      debugln(['TTestCodetoolsCompleteBlock.CompleteBlock FAILED end']);
    end;
    AssertEquals('CompleteBlock did no or the wrong completion: ',TrimExpected,TrimResult);

  finally
    ExpectedCode.Free;
  end;
end;

procedure TTestCodetoolsCompleteBlock.CompleteBlock(Src: string;
  OnlyIfCursorBlockIndented: boolean);
begin
  CompleteBlock(Src,Src,OnlyIfCursorBlockIndented);
end;

procedure TTestCodetoolsCompleteBlock.CompleteBlockFail(Src: string;
  OnlyIfCursorBlockIndented: boolean);
var
  Code: TCodeBuffer;
  p: integer;
  FullSrc: String;
  Y: integer;
  X: integer;
  NewCode: TCodeBuffer;
  NewX: integer;
  NewY: integer;
  NewTopLine: integer;
begin
  AssertEquals('Src is empty',Trim(Src)<>'',true);

  // replace cursor | marker in Src
  Code:=CodeToolBoss.CreateFile('TestCompleteBlock.pas');
  FullSrc:=CreateFullSrc(Src,p);
  if p<1 then
    AssertEquals('missing cursor | in test source: "'+dbgstr(Src)+'"',true,false);
  Code.Source:=FullSrc;
  Code.AbsoluteToLineCol(p,Y,X);

  if CodeToolBoss.CompleteBlock(Code,X,Y,OnlyIfCursorBlockIndented,
    NewCode,NewX,NewY,NewTopLine)
  then begin
    debugln(['TTestCodetoolsCompleteBlock.CompleteBlockFail completion: ',dbgstr(Code.Source)]);
    AssertEquals('CodeToolBoss.CompleteBlock returned true for incompletable src="'+dbgstr(Src)+'"',true,false);
  end;
end;

procedure TTestCodetoolsCompleteBlock.TestCompleteBlockClassStart;
begin
  CompleteBlock('type'+LineEnding
               +'  TTestClass = class(TObject)|',
                'type'+LineEnding
               +'  TTestClass = class(TObject)'+LineEnding
               +'  |end;');
  CompleteBlock('type'+LineEnding
               +'  TTestClass = class(TObject)|'+LineEnding
               +'  TSecondClass =',
                'type'+LineEnding
               +'  TTestClass = class(TObject)'+LineEnding
               +'  |end;'+LineEnding
               +LineEnding
               +'  TSecondClass =');
  CompleteBlock('type'+LineEnding
               +'  TTestClass = class(TObject)|'+LineEnding
               +'implementation',
                'type'+LineEnding
               +'  TTestClass = class(TObject)'+LineEnding
               +'  |end;'+LineEnding
               +LineEnding
               +'implementation');
end;

procedure TTestCodetoolsCompleteBlock.TestCompleteBlockBegin;
begin
  CompleteBlock('begin'+LineEnding
               +'  begin|'+LineEnding
               +'end.',
                'begin'+LineEnding
               +'  begin|'+LineEnding
               +'  end;'+LineEnding
               +'end.');
  CompleteBlock('begin'+LineEnding
               +'  while do begin|'+LineEnding
               +'end.',
                'begin'+LineEnding
               +'  while do begin|'+LineEnding
               +'  end;'+LineEnding
               +'end.');
  CompleteBlock('begin'+LineEnding
               +'  while do'+LineEnding
               +'    begin|'+LineEnding
               +'end.',
                'begin'+LineEnding
               +'  while do'+LineEnding
               +'    begin|'+LineEnding
               +'    end;'+LineEnding
               +'end.');
  CompleteBlock('begin'+LineEnding
               +'  begin|'+LineEnding
               +'    writeln;'+LineEnding
               +'end.',
                'begin'+LineEnding
               +'  begin|'+LineEnding
               +'    writeln;'+LineEnding
               +'  end;'+LineEnding
               +'end.');
  CompleteBlock('begin'+LineEnding
               +'  begin|'+LineEnding
               +'  writeln;'+LineEnding
               +'end.',
                'begin'+LineEnding
               +'  begin|'+LineEnding
               +'  end;'+LineEnding
               +'  writeln;'+LineEnding
               +'end.');
  { Todo: Not implemented yet
  CompleteBlock('procedure a;'+LineEnding
               +'begin|'+LineEnding
               +'begin'+LineEnding
               +'end.',
                'procedure a;'+LineEnding
               +'begin|'+LineEnding
               +'end;'+LineEnding
               +'begin'+LineEnding
               +'end.');}
end;

procedure TTestCodetoolsCompleteBlock.TestCompleteBlockRepeat;
begin
  CompleteBlock('begin'+LineEnding
               +'  repeat|'+LineEnding
               +'end.',
                'begin'+LineEnding
               +'  repeat|'+LineEnding
               +'  until ;'+LineEnding
               +'end.');
  CompleteBlock(
     'begin'+LineEnding
    +'  if FindFirstUTF8(Dir+FileMask,faAnyFile,FileInfo)=0 then begin'+LineEnding
    +'    repeat'+LineEnding
    +'      // check if special file'+LineEnding
    +'      if (FileInfo.Name=''.'') or (FileInfo.Name=''..'') or (FileInfo.Name='''')'+LineEnding
    +'      then'+LineEnding
    +'        continue;'+LineEnding
    +'      |'+LineEnding
    +'      if FilenameIsPascalUnit(FileInfo.Name,false) then begin'+LineEnding
    +'        List.Add(Dir+FileInfo.Name);'+LineEnding
    +'      end else if (FileInfo.Attr and faDirectory)>0 then begin'+LineEnding
    +'        CollectUnits(Dir+);'+LineEnding
    +'      end;'+LineEnding
    +'    until FindNextUTF8(FileInfo)<>0;'+LineEnding
    +'  end;'+LineEnding
    +'  FindCloseUTF8(FileInfo);'+LineEnding);
end;

procedure TTestCodetoolsCompleteBlock.TestCompleteBlockCase;
begin
  CompleteBlock('begin'+LineEnding
               +'  case of|'+LineEnding
               +'end.',
                'begin'+LineEnding
                +'  case of|'+LineEnding
                +'  end;'+LineEnding
               +'end.');
  CompleteBlock('begin'+LineEnding
               +'  case of|'+LineEnding
               +'end.',
                'begin'+LineEnding
                +'  case of|'+LineEnding
                +'  end;'+LineEnding
               +'end.');
end;

procedure TTestCodetoolsCompleteBlock.TestCompleteBlockTry;
begin
  CompleteBlock('begin'+LineEnding
               +'  try|'+LineEnding
               +'end.',
                'begin'+LineEnding
               +'  try|'+LineEnding
               +'  finally'+LineEnding
               +'  end;'+LineEnding
               +'end.');
  CompleteBlock('begin'+LineEnding
               +'  try'+LineEnding
               +'  finally|'+LineEnding
               +'end.',
                'begin'+LineEnding
               +'  try'+LineEnding
               +'  finally|'+LineEnding
               +'  end;'+LineEnding
               +'end.');
  CompleteBlock('begin'+LineEnding
               +'  try'+LineEnding
               +'  except|'+LineEnding
               +'end.',
                'begin'+LineEnding
               +'  try'+LineEnding
               +'  except|'+LineEnding
               +'  end;'+LineEnding
               +'end.');
{ ToDo:
  if True then
    i:=1
  else
    if True then
      if True then
        i:=2
      else
        i:=3
    else
      i:=4;

  if True then Caption:='retz'
  else if False then Caption:='qretz';

  try

  except
    on e: Exception do begin
      fErrorMsg:=e.Message;
    end;
    else begin
      fErrorMsg:='Error';
    end;
    MyResult:=mrAbort;
  end;
  try
    try
    finally

    end;
  except
    on E: Exception do begin

    end;
  end;
}
end;

procedure TTestCodetoolsCompleteBlock.TestCompleteBlockAsm;
begin
  CompleteBlock('begin'+LineEnding
               +'  asm|'+LineEnding
               +'end.',
                'begin'+LineEnding
               +'  asm|'+LineEnding
               +'  end;'+LineEnding
               +'end.');
end;

procedure TTestCodetoolsCompleteBlock.TestCompleteBlockIf;
begin
  CompleteBlock('begin'+LineEnding
               +'  if then begin|'+LineEnding
               +'end.',
                'begin'+LineEnding
               +'  if then begin|'+LineEnding
               +'  end;'+LineEnding
               +'end.');
  CompleteBlock('begin'+LineEnding
               +'  if then begin|'+LineEnding
               +'  else'+LineEnding
               +'end.',
                'begin'+LineEnding
               +'  if then begin|'+LineEnding
               +'  end'+LineEnding
               +'  else'+LineEnding
               +'end.');
  CompleteBlockFail('begin'+LineEnding
                   +'  try'+LineEnding
                   +'    if|'+LineEnding
                   +'  finally'+LineEnding
                   +'  end;'+LineEnding
                   +'end.');
end;

initialization
  RegisterTest(TTestCodetoolsCompleteBlock);

end.

