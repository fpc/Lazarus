unit TestCodeCompletion;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, CodeToolManager, CodeCache,
  LazLogger, LazFileUtils, fpcunit, testregistry,
  TestFinddeclaration, TestStdCodetools;

type

  { TTestCodeCompletion }

  TTestCodeCompletion = class(TCustomTestCTStdCodetools)
  private
    procedure Test(Title: string; Src: array of string; Line, Col: integer;
      Expected: array of string);
  published
    procedure TestIntfProcUpdateArgName;
    procedure TestCompleteMethodSignature_GenericObjFPC;
    procedure TestCompleteMethodBody_GenericObjFPC;
    procedure TestCompleteMethodBody_GenericDelphi; // ToDo
    procedure TestCompleteMethodBody_ParamSpecialize;
    procedure TestCompleteMethodBody_ParamDelphiSpecialize;
    procedure TestCompleteProperty_ObjFPC_SpecializeType;
  end;

implementation

{ TTestCodeCompletion }

procedure TTestCodeCompletion.Test(Title: string; Src: array of string; Line,
  Col: integer; Expected: array of string);
var
  i, NewX, NewY, NewTopLine, BlockTopLine, BlockBottomLine: Integer;
  s: String;
  NewCode, Code: TCodeBuffer;
begin
  Code:=CodeToolBoss.CreateFile('test1.pas');
  s:='';
  for i:=Low(Src) to High(Src) do
    s+=Src[i]+LineEnding;
  Code.Source:=s;

  if not CodeToolBoss.CompleteCode(Code,Col,Line,Line,NewCode,NewX,NewY,NewTopLine,
    BlockTopLine,BlockBottomLine,false) then
  begin
    NewCode:=Code;
    NewY:=Line;
    NewX:=Col;
    if (CodeToolBoss.ErrorCode<>nil) and (CodeToolBoss.ErrorLine>0) then begin
      NewY:=CodeToolBoss.ErrorLine;
      NewX:=CodeToolBoss.ErrorColumn;
      NewCode:=CodeToolBoss.ErrorCode;
    end;
    WriteSource(NewCode.Filename,NewY,NewX);
    Fail(Title+': call CompleteCode failed: "'+CodeToolBoss.ErrorDbgMsg+'"');
  end;
  s:='';
  for i:=Low(Expected) to High(Expected) do
    s+=Expected[i]+LineEnding;
  CheckDiff(Title,s,Code.Source);
end;

procedure TTestCodeCompletion.TestIntfProcUpdateArgName;
begin
  Test('TestIntfProcUpdateArgName',
    ['unit test1;'
    ,'interface'
    ,'procedure DoIt(NewName: longint);'
    ,'implementation'
    ,'procedure DoIt(OldName: longint);'
    ,'begin end;'
    ,'end.'],
    3,1,
    ['unit test1;'
    ,'interface'
    ,'procedure DoIt(NewName: longint);'
    ,'implementation'
    ,'procedure DoIt(NewName: longint);'
    ,'begin end;'
    ,'end.']);
end;

procedure TTestCodeCompletion.TestCompleteMethodSignature_GenericObjFPC;
begin
  Test('TestCompleteMethodSignature_GenericObjFPC',
    ['unit test1;',
    '{$mode objfpc}{$H+}',
    'interface',
    'type',
    '  generic TBird<T: class> = class',
    '    procedure DoIt(s: string);',
    '    procedure DoIt;',
    '  end;',
    'implementation',
    'procedure TBird.DoIt(s: char);',
    'begin',
    'end;',
    'procedure TBird.DoIt;',
    'begin',
    'end;',
    'end.'],
    6,1,
    ['unit test1;',
    '{$mode objfpc}{$H+}',
    'interface',
    'type',
    '',
    '  { TBird }',
    '',
    '  generic TBird<T: class> = class',
    '    procedure DoIt(s: string);',
    '    procedure DoIt;',
    '  end;',
    'implementation',
    'procedure TBird.DoIt(s: string);',
    'begin',
    'end;',
    'procedure TBird.DoIt;',
    'begin',
    'end;',
    'end.']);
end;

procedure TTestCodeCompletion.TestCompleteMethodBody_GenericObjFPC;
begin
  Test('TestCompleteMethodBody_GenericObjFPC',
    ['unit test1;',
    '{$mode objfpc}{$H+}',
    'interface',
    'type',
    '  generic TBird<T: class> = class',
    '    procedure DoIt;',
    '  end;',
    'implementation',
    'end.'],
    6,1,
    ['unit test1;',
    '{$mode objfpc}{$H+}',
    'interface',
    'type',
    '',
    '  { TBird }',
    '',
    '  generic TBird<T: class> = class',
    '    procedure DoIt;',
    '  end;',
    'implementation',
    '',
    'procedure TBird.DoIt;',
    'begin',
    'end;',
    '',
    'end.']);
end;

procedure TTestCodeCompletion.TestCompleteMethodBody_GenericDelphi;
begin
  exit; // ToDo
  Test('TestCompleteMethodBody_GenericObjFPC',
    ['unit test1;',
    '{$mode delphi}',
    'interface',
    'type',
    '  TBird<T: class> = class',
    '    procedure DoIt;',
    '  end;',
    'implementation',
    'end.'],
    6,1,
    ['unit test1;',
    '{$mode delphi}',
    'interface',
    'type',
    '',
    '  { TBird }',
    '',
    '  TBird<T: class> = class',
    '    procedure DoIt;',
    '  end;',
    'implementation',
    '',
    'procedure TBird<T>.DoIt;',
    'begin',
    'end;',
    '',
    'end.']);
end;

procedure TTestCodeCompletion.TestCompleteMethodBody_ParamSpecialize;
begin
  Test('TestCompleteMethodBody_ParamSpecialize',
    ['unit test1;',
    '{$mode objfpc}{$H+}',
    'interface',
    'type',
    '  TBird = class',
    '    procedure DoIt(i: specialize TGenList<longint>);',
    '  end;',
    'implementation',
    'end.'],
    6,1,
    ['unit test1;',
    '{$mode objfpc}{$H+}',
    'interface',
    'type',
    '',
    '  { TBird }',
    '',
    '  TBird = class',
    '    procedure DoIt(i: specialize TGenList<longint>);',
    '  end;',
    'implementation',
    '',
    'procedure TBird.DoIt(i: specialize TGenList<longint>);',
    'begin',
    'end;',
    '',
    'end.']);
end;

procedure TTestCodeCompletion.TestCompleteMethodBody_ParamDelphiSpecialize;
begin
  Test('TestCompleteMethodBody_ParamDelphiSpecialize',
    ['unit test1;',
    '{$mode delphi}',
    'interface',
    'type',
    '  TBird = class',
    '    procedure DoIt(i: TGenList<longint>);',
    '  end;',
    'implementation',
    'end.'],
    6,1,
    ['unit test1;',
    '{$mode delphi}',
    'interface',
    'type',
    '',
    '  { TBird }',
    '',
    '  TBird = class',
    '    procedure DoIt(i: TGenList<longint>);',
    '  end;',
    'implementation',
    '',
    'procedure TBird.DoIt(i: TGenList<longint>);',
    'begin',
    'end;',
    '',
    'end.']);
end;

procedure TTestCodeCompletion.TestCompleteProperty_ObjFPC_SpecializeType;
begin
  Test('TestCompleteMethodBody_ParamDelphiSpecialize',
    ['unit test1;',
    '{$mode objfpc}{$H+}',
    'interface',
    'type',
    '  generic TLimb<T> = class end;',
    '  TBird = class',
    '    property Wing: specialize TLimb<string>;',
    '  end;',
    'implementation',
    'end.'],
    7,1,
    ['unit test1;',
    '{$mode objfpc}{$H+}',
    'interface',
    'type',
    '',
    '  { TBird }',
    '',
    '  TBird = class',
    '    property Wing: specialize TLimb<string> read FWing write SetWing;',
    '  end;',
    'implementation',
    '',
    'end.']);
end;

initialization
  RegisterTests([TTestCodeCompletion]);
end.

