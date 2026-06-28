unit TestChangeDeclaration;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, AVL_Tree,
  // FPCUnit
  TestRegistry,
  // LazUtils
  LazLoggerBase, LazFileUtils,
  // CodeTools
  CodeToolManager, CodeCache, PascalParserTool, CodeTree,
  // (project)
  TestGlobals, TestRefactoring;

type

  { TTestChangeDeclaration }

  TTestChangeDeclaration = class(TCustomTestRefactoring)
  protected
    FDefFilename: string;
    procedure SetUp; override;
    procedure TestChangeMethodVisibility(Src, Expected: string; NewVisibility: TCodeTreeNodeDesc);
  published
    procedure TestCTAddProcedureModifier;

    // change method visibility
    procedure TestCT_ChangeMethodVisibility_Already;
    procedure TestCT_ChangeMethodVisibility_Protected2Public;
    procedure TestCT_ChangeMethodVisibility_Public2Protected;
    procedure TestCT_ChangeMethodVisibility_Public2ProtectedComment;
    procedure TestCT_ChangeMethodVisibility_Private2Public;
    procedure TestCT_ChangeMethodVisibility_Private2PublicComment;
    procedure TestCT_ChangeMethodVisibility_Protected2PublicCommentBehind;
  end;

implementation

procedure TTestChangeDeclaration.SetUp;
begin
  inherited SetUp;
  FDefFilename:='TestChangeDeclaration.pas';
end;

procedure TTestChangeDeclaration.TestChangeMethodVisibility(Src, Expected: string;
  NewVisibility: TCodeTreeNodeDesc);
var
  p: integer;
  Line, Column: integer;
begin
  Code.Source:=Src;
  p:=Pos('{#A}',Src);
  if p<1 then
    Fail('Marker not found');
  Code.AbsoluteToLineCol(p,Line,Column);
  if not CodeToolBoss.ChangeMethodVisibility(Code,Column,Line,NewVisibility) then
    Fail('ChangeMethodVisibility failed: '+CodeToolBoss.ErrorMessage);
  CheckDiffStr(Code,Expected);
end;

procedure TTestChangeDeclaration.TestCTAddProcedureModifier;

  procedure Test(ProcCode, aModifier, Expected: string);
  var
    Code: TCodeBuffer;
    Src, ProcHead: String;
  begin
    Src:='unit '+FDefFilename+';'+sLineBreak
      +'interface'+sLineBreak
      +ProcCode+sLineBreak
      +'implementation'+sLineBreak
      +'end.';
    Code:=CodeToolBoss.CreateFile(FDefFilename);
    try
      Code.Source:=Src;
      if not CodeToolBoss.AddProcModifier(Code,3,3,aModifier) then
      begin
        Fail('AddProcModifier failed: '+CodeToolBoss.ErrorMessage);
      end else begin
        if not CodeToolBoss.ExtractProcedureHeader(Code,3,3,
          [phpWithStart,phpWithResultType,phpWithOfObject,phpWithProcModifiers,phpWithComments,phpDoNotAddSemicolon],
          ProcHead)
        then
          Fail('ExtractProcedureHeader failed: '+CodeToolBoss.ErrorMessage);
        if ProcHead<>Expected then begin
          debugln('Test ProcCode="',ProcCode,'"');
          Src:=Code.Source;
          debugln('SrcSTART:======================');
          debugln(Src);
          debugln('SrcEND:========================');
          AssertEquals('ProcHead',Expected,ProcHead);
        end;
      end;
    finally
      Code.IsDeleted:=true;
    end;
  end;

begin
  // remove first unit
  Test('procedure DoIt;','overload','procedure DoIt; overload;');
  Test('procedure DoIt ;','overload','procedure DoIt; overload ;');
  Test('procedure DoIt ; ;','overload','procedure DoIt; overload ;');
  Test('procedure DoIt; overload;','overload','procedure DoIt; overload;');
  Test('procedure DoIt; {$IFDEF FPC}overload{$ENDIF};','overload','procedure DoIt; {$IFDEF FPC}overload{$ENDIF};');
  Test('procedure DoIt; procedure Bla;','overload','procedure DoIt; overload;');
  Test('  procedure DoIt;'+sLineBreak+'  procedure Bla;',
    'overload','procedure DoIt; overload;');
  Test('  procedure DoIt; external name ''doit'';'+sLineBreak+'  procedure Bla;',
    'overload','procedure DoIt; external name ''doit''; overload;');
end;

procedure TTestChangeDeclaration.TestCT_ChangeMethodVisibility_Already;
begin
  TestChangeMethodVisibility(
  LinesToStr([
  'unit '+FDefFilename+';',
  'interface',
  'type',
  '  TBird = class',
  '  protected',
  '    procedure {#A}Fly; virtual; abstract;',
  '  end;',
  'implementation',
  'end.'
  ]),
  LinesToStr([
  'unit '+FDefFilename+';',
  'interface',
  'type',
  '  TBird = class',
  '  protected',
  '    procedure {#A}Fly; virtual; abstract;',
  '  end;',
  'implementation',
  'end.'
  ]),
  ctnClassProtected
  );
end;

procedure TTestChangeDeclaration.TestCT_ChangeMethodVisibility_Protected2Public;
begin
  TestChangeMethodVisibility(
  LinesToStr([
  'unit '+FDefFilename+';',
  'interface',
  'type',
  '  TBird = class',
  '  protected',
  '    procedure {#A}Fly; virtual; abstract;',
  '    procedure Jump; virtual; abstract;',
  '  end;',
  'implementation',
  'end.'
  ]),
  LinesToStr([
  'unit '+FDefFilename+';',
  'interface',
  'type',
  '  TBird = class',
  '  protected',
  '    procedure Jump; virtual; abstract;',
  '  public',
  '    procedure {#A}Fly; virtual; abstract;',
  '  end;',
  'implementation',
  'end.'
  ]),
  ctnClassPublic
  );
end;

procedure TTestChangeDeclaration.TestCT_ChangeMethodVisibility_Public2Protected;
begin
  // move a public method to a not yet existing protected section
  TestChangeMethodVisibility(
  LinesToStr([
  'unit '+FDefFilename+';',
  'interface',
  'type',
  '  TBird = class',
  '  public',
  '    procedure {#A}Fly; virtual; abstract;',
  '    procedure Jump; virtual; abstract;',
  '  end;',
  'implementation',
  'end.'
  ]),
  LinesToStr([
  'unit '+FDefFilename+';',
  'interface',
  'type',
  '  TBird = class',
  '  protected',
  '    procedure {#A}Fly; virtual; abstract;',
  '  public',
  '    procedure Jump; virtual; abstract;',
  '  end;',
  'implementation',
  'end.'
  ]),
  ctnClassProtected
  );
end;

procedure TTestChangeDeclaration.TestCT_ChangeMethodVisibility_Public2ProtectedComment;
begin
  // move a public method with a comment in front to a not yet existing protected section
  TestChangeMethodVisibility(
  LinesToStr([
  'unit '+FDefFilename+';',
  'interface',
  'type',
  '  TBird = class',
  '  public',
  '    // the bird can fly',
  '    procedure {#A}Fly; virtual; abstract;',
  '    procedure Jump; virtual; abstract;',
  '  end;',
  'implementation',
  'end.'
  ]),
  LinesToStr([
  'unit '+FDefFilename+';',
  'interface',
  'type',
  '  TBird = class',
  '  protected',
  '    // the bird can fly',
  '    procedure {#A}Fly; virtual; abstract;',
  '  public',
  '    procedure Jump; virtual; abstract;',
  '  end;',
  'implementation',
  'end.'
  ]),
  ctnClassProtected
  );
end;

procedure TTestChangeDeclaration.TestCT_ChangeMethodVisibility_Private2Public;
begin
  // move a private method to an already existing public section
  TestChangeMethodVisibility(
  LinesToStr([
  'unit '+FDefFilename+';',
  'interface',
  'type',
  '  TBird = class',
  '  private',
  '    procedure {#A}Fly; virtual; abstract;',
  '    procedure Walk; virtual; abstract;',
  '  public',
  '    procedure Jump; virtual; abstract;',
  '  end;',
  'implementation',
  'end.'
  ]),
  LinesToStr([
  'unit '+FDefFilename+';',
  'interface',
  'type',
  '  TBird = class',
  '  private',
  '    procedure Walk; virtual; abstract;',
  '  public',
  '    procedure Jump; virtual; abstract;',
  '    procedure {#A}Fly; virtual; abstract;',
  '  end;',
  'implementation',
  'end.'
  ]),
  ctnClassPublic
  );
end;

procedure TTestChangeDeclaration.TestCT_ChangeMethodVisibility_Private2PublicComment;
begin
  // move a private method with a comment in front to an already existing public section
  TestChangeMethodVisibility(
  LinesToStr([
  'unit '+FDefFilename+';',
  'interface',
  'type',
  '  TBird = class',
  '  private',
  '    // the bird can fly',
  '    procedure {#A}Fly; virtual; abstract;',
  '    procedure Walk; virtual; abstract;',
  '  public',
  '    procedure Jump; virtual; abstract;',
  '  end;',
  'implementation',
  'end.'
  ]),
  LinesToStr([
  'unit '+FDefFilename+';',
  'interface',
  'type',
  '  TBird = class',
  '  private',
  '    procedure Walk; virtual; abstract;',
  '  public',
  '    procedure Jump; virtual; abstract;',
  '    // the bird can fly',
  '    procedure {#A}Fly; virtual; abstract;',
  '  end;',
  'implementation',
  'end.'
  ]),
  ctnClassPublic
  );
end;

procedure TTestChangeDeclaration.TestCT_ChangeMethodVisibility_Protected2PublicCommentBehind;
begin
  // move a protected method with a comment behind it (on its last line)
  // to an already existing public section
  TestChangeMethodVisibility(
  LinesToStr([
  'unit '+FDefFilename+';',
  'interface',
  'type',
  '  TBird = class',
  '  protected',
  '    procedure {#A}Fly; virtual; abstract; // Flying',
  '    procedure Walk; virtual; abstract;',
  '  public',
  '    procedure Jump; virtual; abstract;',
  '  end;',
  'implementation',
  'end.'
  ]),
  LinesToStr([
  'unit '+FDefFilename+';',
  'interface',
  'type',
  '  TBird = class',
  '  protected',
  '    procedure Walk; virtual; abstract;',
  '  public',
  '    procedure Jump; virtual; abstract;',
  '    procedure {#A}Fly; virtual; abstract; // Flying',
  '  end;',
  'implementation',
  'end.'
  ]),
  ctnClassPublic
  );
end;

initialization
  RegisterTests([TTestChangeDeclaration]);
end.

