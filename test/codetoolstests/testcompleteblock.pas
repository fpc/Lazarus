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
  CompareComplete('ifbeginelse1.inc','6 28 ifbeginelse fpcunit', 'ifbeginelse1.result');
end;

end.

