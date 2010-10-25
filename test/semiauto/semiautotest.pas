unit semiautotest;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit,
  Interfaces, Forms, LCLType,
  testglobals;

type

  { TSemiAutomaticTest }

  TSemiAutomaticTest = class(TTestCase)
  public
    function ShowResultDialog(const ATitle, AInstructions: string): Boolean;
  end;

implementation

{ TSemiAutomaticTest }

function TSemiAutomaticTest.ShowResultDialog(const ATitle, AInstructions: string
  ): Boolean;
begin
  Result := Application.MessageBox(PChar(AInstructions), PChar(ATitle), MB_YESNO) = IDYES;
end;

end.

