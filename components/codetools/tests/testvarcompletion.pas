{
 Test with:
   ./testcodetools --format=plain --suite=TTestVarCompletion
   ./testcodetools --format=plain --suite=TestVarComplete_ForInSet
}
unit TestVarCompletion;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, CodeToolManager, CodeCache, CodeTree,
  BasicCodeTools, LazLogger, LazFileUtils, fpcunit, testregistry,
  TestFinddeclaration;

type

  { TTestVarCompletion }

  TTestVarCompletion = class(TTestCase)
  private
  published
    procedure TestVarComplete_ForInSet;
  end;

implementation

{ TTestVarCompletion }

procedure TTestVarCompletion.TestVarComplete_ForInSet;
begin

end;

initialization
  RegisterTests([TTestVarCompletion]);
end.

