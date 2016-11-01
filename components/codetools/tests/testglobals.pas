unit TestGlobals;

{$mode objfpc}{$H+}

interface

uses
  fpcunit, testregistry,
  classes, sysutils, process;

var
  BasicTestSuite: TTestSuite;
  FindDeclararionTestSuite: TTestSuite;
  StdToolsTestSuite: TTestSuite;
  RefactoringTestSuite: TTestSuite;
  NonPascalTestSuite: TTestSuite;
  BugsTestSuite: TTestSuite;

procedure AddToBasicTestSuite(ATestClass: TClass);
procedure AddToFindDeclarationTestSuite(ATestClass: TClass);
procedure AddToStdToolsTestSuite(ATestClass: TClass);
procedure AddToRefactoringTestSuite(ATestClass: TClass);
procedure AddToNonPascalTestSuite(ATestClass: TClass);
procedure AddToBugsTestSuite(ATest: TTest);

implementation

procedure AddToBasicTestSuite(ATestClass: TClass);
begin
  BasicTestSuite.AddTestSuiteFromClass(ATestClass);
end;

procedure AddToFindDeclarationTestSuite(ATestClass: TClass);
begin
  FindDeclararionTestSuite.AddTestSuiteFromClass(ATestClass);
end;

procedure AddToStdToolsTestSuite(ATestClass: TClass);
begin
  StdToolsTestSuite.AddTestSuiteFromClass(ATestClass);
end;

procedure AddToRefactoringTestSuite(ATestClass: TClass);
begin
  RefactoringTestSuite.AddTestSuiteFromClass(ATestClass);
end;

procedure AddToNonPascalTestSuite(ATestClass: TClass);
begin
  NonPascalTestSuite.AddTestSuiteFromClass(ATestClass);
end;

procedure AddToBugsTestSuite(ATest: TTest);
begin
  BugsTestSuite.AddTest(ATest);
end;

initialization
  GetTestRegistry.TestName := 'All tests';
  BasicTestSuite := TTestSuite.Create('Basic tests');
  GetTestRegistry.AddTest(BasicTestSuite);
  FindDeclararionTestSuite := TTestSuite.Create('FindDeclaration tests');
  GetTestRegistry.AddTest(FindDeclararionTestSuite);
  StdToolsTestSuite := TTestSuite.Create('StdTools tests');
  GetTestRegistry.AddTest(StdToolsTestSuite);
  RefactoringTestSuite := TTestSuite.Create('Refactoring tests');
  GetTestRegistry.AddTest(RefactoringTestSuite);
  NonPascalTestSuite := TTestSuite.Create('No Pascal tests');
  GetTestRegistry.AddTest(NonPascalTestSuite);
  BugsTestSuite := TTestSuite.Create('Bugs');
  GetTestRegistry.AddTest(BugsTestSuite);

end.

