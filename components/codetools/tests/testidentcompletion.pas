{
 Test with:
   ./testcodetools --suite=TTestFindDeclaration
   ./testcodetools --suite=TestFindDeclaration_Basic
   ./testcodetools --suite=TestFindDeclaration_ClassOf
   ./testcodetools --suite=TestFindDeclaration_With
   ./testcodetools --suite=TestFindDeclaration_NestedClasses
   ./testcodetools --suite=TestFindDeclaration_ClassHelper
   ./testcodetools --suite=TestFindDeclaration_TypeHelper
   ./testcodetools --suite=TestFindDeclaration_ObjCClass
   ./testcodetools --suite=TestFindDeclaration_ObjCCategory
   ./testcodetools --suite=TestFindDeclaration_Generics
   ./testcodetools --suite=TestFindDeclaration_FileAtCursor

 FPC tests:
   ./testcodetools --suite=TestFindDeclaration_FPCTests
   ./testcodetools --suite=TestFindDeclaration_FPCTests --filemask=t*.pp
   ./testcodetools --suite=TestFindDeclaration_FPCTests --filemask=tchlp41.pp
 Laz tests:
   ./testcodetools --suite=TestFindDeclaration_LazTests
   ./testcodetools --suite=TestFindDeclaration_LazTests --filemask=t*.pp
   ./testcodetools --suite=TestFindDeclaration_LazTests --filemask=tdefaultproperty1.pp
}
unit TestIdentCompletion;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  fpcunit, testregistry,
  FileProcs, LazFileUtils, LazLogger,
  CodeToolManager, ExprEval,
  CustomCodeTool, FindDeclarationTool, KeywordFuncLists,
  TestFindDeclaration;

type

  { TTestIdentCompletion }

  TTestIdentCompletion = class(TCustomTestFindDeclaration)
  published
    procedure Test_GetValuesOfCaseVariable_Enum;
  end;

implementation

{ TTestIdentCompletion }

procedure TTestIdentCompletion.Test_GetValuesOfCaseVariable_Enum;
var
  List: TStrings;
begin
  List:=TStringList.Create;
  try
    Code.Source:=
    'type TEnum = (red,green);'+LineEnding
    +'var e: TEnum;'+LineEnding
    +'begin'+LineEnding
    +'  case e of'+LineEnding
    +'end.';
    List.Clear;
    if not CodeToolBoss.GetValuesOfCaseVariable(Code,8,4,List) then begin
      Fail('GetValuesOfCaseVariable failed on case enum');
    end;
    //writeln('TTestIdentCompletion.Test_GetValuesOfCaseVariable_Enum ',List.Text);
    AssertEquals('case enum count',2,List.Count);
    AssertEquals('case enum[0]','red',List[0]);
    AssertEquals('case enum[1]','green',List[1]);
  finally
    List.Free;
  end;
end;

initialization
  RegisterTests([TTestIdentCompletion]);
end.

