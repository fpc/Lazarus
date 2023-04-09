unit TestDesignerFormTools;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, CodeToolManager, CodeCache, DefineTemplates,
  LazLogger, LazFileUtils, fpcunit, testregistry,
  TestFinddeclaration, TestStdCodetools, Dsgn_BearControls, Dsgn_BearButtons;

type
  TBearForm1 = class(TBearForm)
  end;

  { TTestDesignerFormTools }

  TTestDesignerFormTools = class(TCustomTestCTStdCodetools)
  private
    procedure TestAddPublishedBearVars(Title: string; Src: array of string;
      Expected: array of string);
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  public
    BearForm1: TBearForm1;
  published
    // add published variables
    procedure TestAddPublishedVariables_Empty;
    procedure TestAddPublishedVariables_Button1;
  end;

implementation

{ TTestDesignerFormTools }

procedure TTestDesignerFormTools.TestAddPublishedBearVars(Title: string;
  Src: array of string; Expected: array of string);
var
  i, NewX, NewY: Integer;
  s, Dir: String;
  NewCode, Code: TCodeBuffer;
  DefTemp: TDefineTemplate;
begin
  Code:=CodeToolBoss.CreateFile('test1.pas');
  s:='';
  for i:=Low(Src) to High(Src) do
    s+=Src[i]+LineEnding;
  Code.Source:=s;

  Dir:=AppendPathDelim(GetCurrentDir)+'moduletests';
  DefTemp:=TDefineTemplate.Create('unitpath','add moduletests',UnitPathMacroName,Dir,da_Define);
  try
    CodeToolBoss.DefineTree.Add(DefTemp);

    //debugln(['TTestFindDeclaration.TestFindDeclaration_UnitSearch_CurrentDir ',CodeToolBoss.GetUnitPathForDirectory('')]);

    if not CodeToolBoss.AddPublishedVariables(Code,BearForm1,nil)
    and (CodeToolBoss.ErrorDbgMsg<>'') then
    begin
      NewCode:=Code;
      NewY:=1;
      NewX:=1;
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
  finally
    CodeToolBoss.DefineTree.RemoveDefineTemplate(DefTemp);
  end;
end;

procedure TTestDesignerFormTools.SetUp;
begin
  inherited SetUp;
  BearForm1:=TBearForm1.Create(nil);
end;

procedure TTestDesignerFormTools.TearDown;
begin
  FreeAndNil(BearForm1);
  inherited TearDown;
end;

procedure TTestDesignerFormTools.TestAddPublishedVariables_Empty;
begin
  TestAddPublishedBearVars('TestAddPublishedVariables_Empty',
    ['unit test1;'
    ,'{$mode objfpc}{$H+}'
    ,'interface'
    ,'uses Dsgn_BearControls;'
    ,'type'
    ,'  TBearForm1 = class(TBearForm)'
    ,'  end;'
    ,'implementation'
    ,'end.'],
    ['unit test1;'
    ,'{$mode objfpc}{$H+}'
    ,'interface'
    ,'uses Dsgn_BearControls;'
    ,'type'
    ,'  { TBearForm1 }'
    ,'  TBearForm1 = class(TBearForm)'
    ,'  end;'
    ,'implementation'
    ,'end.']);
end;

procedure TTestDesignerFormTools.TestAddPublishedVariables_Button1;
var
  Btn: TBearButton;
begin
  Btn:=TBearButton.Create(BearForm1);
  Btn.Name:='Button1';

  TestAddPublishedBearVars('TestAddPublishedVariables_Empty',
    ['unit test1;'
    ,'{$mode objfpc}{$H+}'
    ,'interface'
    ,'uses Dsgn_BearControls, Dsgn_BearButtons;'
    ,'type'
    ,'  TBearForm1 = class(TBearForm)'
    ,'  end;'
    ,'implementation'
    ,'end.'],
    ['unit test1;'
    ,'{$mode objfpc}{$H+}'
    ,'interface'
    ,'uses Dsgn_BearControls, Dsgn_BearButtons;'
    ,'type'
    ,'  { TBearForm1 }'
    ,'  TBearForm1 = class(TBearForm)'
    ,'    Button1: TBearButton;'
    ,'  end;'
    ,'implementation'
    ,'end.']);
end;

initialization
  RegisterTests([TTestDesignerFormTools]);

end.

