program ifps3;

{$mode objfpc}{$H+}

uses
  Interfaces, Forms, GuiTestRunner, CompilerTestBase, CompilerTestFunctions,
  CompilerTestSimple, CompileTestExtended, pascalscript;

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TGuiTestRunner, TestRunner);
  Application.Run;
end.

