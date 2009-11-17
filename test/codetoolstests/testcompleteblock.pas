unit TestCompleteBlock;

{$mode objfpc}{$H+}

interface

uses
  fpcunit, Classes, SysUtils;

type
  { TCodeBlocksTest }

  TCodeBlocksTest = class(TTestCase)
  protected
    function CompareComplete(const InputDefines, ResultFile: String): Boolean;
  published
    procedure TestCompleteBlocks;
  end;

implementation

{ TCodeBlocksTest }

function TCodeBlocksTest.CompareComplete(const InputFile, InputDefines, ResultFile: String): Boolean;
var
  st : TStringList;
  rs : TStringList;

  function StripSpaceChars(const s: string): String;
  begin
    // removes all [#10,#13,#9, #32] chars, giving a line: "beginwriteln('helloworld');end."
    Result:=s;
    for i:=length(Result) downto 1 do
      if Result[i] in [#10,#13,#9,' '] then
        System.Delete(Result,i,1);
  end;
begin
  // ToDo: fix path to completeblock, InputFile nd ResultFile
  st := GetProcessOutput('completeblock '+InputFile+' '+inputdefines); // reads all output from blockcompleted file
  // remove debugging output and take only the new source
  while (st.Count>0) and (st[0]<>'{%MainUnit unit1.pas}') do st.Delete(0);
  // reads the correct result file
  rs.LoadFromFile(resultfile);
  // check result
  AssertEquals(StripSpaceChars(st.text), StripSpaceChars(rs.text)); // compares resulting strings
end;

procedure TCodeBlocksTest.TestCompleteBlocks;
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

end.

