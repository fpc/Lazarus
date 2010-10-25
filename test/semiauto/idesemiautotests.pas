unit idesemiautotests;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit,
  Interfaces, LCLType, LCLIntf,
  testglobals, semiautotest;

type

  { TTestIdeNew }

  TTestIdeNew = class(TSemiAutomaticTest)
  published
    procedure TestOne;
  end;

implementation

{ TTestIdeNew }

procedure TTestIdeNew.TestOne;
var
  Str: string;
begin
  Str := 'Please follow the following steps and mark if the test was successful:' + LineEnding
   + '1> Open Lazarus' + LineEnding
   + '2> Start a new Application project, but don''t save it' + LineEnding
   + '3> Run this new project' + LineEnding
   + 'Expected result> It should be saved to a temporary location and run or a message dialog should appear saying that the operation is impossible' + LineEnding
   ;
  AssertTrue(ShowResultDialog('TTestIdeNew.TestOne', Str));
end;

initialization
  AddToSemiAutoTestSuite(TTestIdeNew);

end.

