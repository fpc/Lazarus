unit TestDesignerFormTools;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, CodeToolManager, CodeCache, DefineTemplates,
  LazLogger, LazFileUtils, AvgLvlTree, Laz_AVL_Tree, fpcunit, testregistry,
  TestFinddeclaration, TestStdCodetools,
  Dsgn_AmbigBearBtn, Dsgn_BearControls, Dsgn_BearButtons;

type
  TBearForm1 = class(TBearForm)
  end;

  { TTestDesignerFormTools }

  TTestDesignerFormTools = class(TCustomTestCTStdCodetools)
  private
    procedure TestCompleteComponent(const Title: string; CheckUnits: boolean; const Src, Expected: array of string);
    procedure TestGatherPublishedVarType(const Title, aClassName: string; const Src, ExpectedVars: array of string);
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  public
    BearForm1: TBearForm1;
  published
    // add published variables
    procedure TestAddPublishedVariables_Empty;
    procedure TestAddPublishedVariables_Button1;
    procedure TestAddPublishedVariables_AmbiguousButtons1;
    procedure TestAddPublishedVariables_AmbiguousButtons2;
    // gather published variable types
    procedure TestGatherPublishedVarTypes_Empty;
    procedure TestGatherPublishedVarTypes_Button1;
    procedure TestGatherPublishedVarTypes_Button2;
  end;

implementation

{ TTestDesignerFormTools }

procedure TTestDesignerFormTools.TestCompleteComponent(const Title: string;
  CheckUnits: boolean; const Src, Expected: array of string);
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

    if not CodeToolBoss.CompleteComponent(Code,BearForm1,nil,CheckUnits)
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
      Fail(Title+': call CompleteComponent failed: "'+CodeToolBoss.ErrorDbgMsg+'"');
    end;
    s:='';
    for i:=Low(Expected) to High(Expected) do
      s+=Expected[i]+LineEnding;
    CheckDiff(Title,s,Code.Source);
  finally
    CodeToolBoss.DefineTree.RemoveDefineTemplate(DefTemp);
  end;
end;

procedure TTestDesignerFormTools.TestGatherPublishedVarType(const Title,
  aClassName: string; const Src, ExpectedVars: array of string);
var
  Code, NewCode: TCodeBuffer;
  VarNameToToType: TStringToStringTree;
  Expected, Actual, Dir: String;
  i, NewY, NewX: Integer;
  DefTemp: TDefineTemplate;
begin
  Code:=CodeToolBoss.CreateFile('test1.pas');
  Actual:='';
  for i:=Low(Src) to High(Src) do
    Actual+=Src[i]+LineEnding;
  Code.Source:=Actual;

  Dir:=AppendPathDelim(GetCurrentDir)+'moduletests';
  DefTemp:=TDefineTemplate.Create('unitpath','add moduletests',UnitPathMacroName,Dir,da_Define);
  VarNameToToType:=nil;
  try
    CodeToolBoss.DefineTree.Add(DefTemp);

    if not CodeToolBoss.GatherPublishedVarTypes(Code,aClassName,VarNameToToType)
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
      Fail(Title+': call GatherPublishedVarTypes failed: "'+CodeToolBoss.ErrorDbgMsg+'"');
    end;

    Actual:='';
    if VarNameToToType<>nil then begin
      Actual:=VarNameToToType.AsText;
      Actual:=StringReplace(Actual,'=',':',[rfReplaceAll]);
    end;
    Expected:='';
    for i:=Low(ExpectedVars) to High(ExpectedVars) do
      Expected+=ExpectedVars[i]+LineEnding;
    if Actual<>Expected then begin
      debugln(['TTestDesignerFormTools.TestGatherPublishedVarType Expected:']);
      debugln(Expected);
      debugln(['TTestDesignerFormTools.TestGatherPublishedVarType Actual:']);
      debugln(Actual);
      Fail('VarNameToToType differ');
    end;
  finally
    VarNameToToType.Free;
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
  TestCompleteComponent('TestAddPublishedVariables_Empty',true,
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
  Btn: Dsgn_BearButtons.TBearButton;
begin
  Btn:=Dsgn_BearButtons.TBearButton.Create(BearForm1);
  Btn.Name:='Button1';

  TestCompleteComponent('TestAddPublishedVariables_Button1',true,
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

procedure TTestDesignerFormTools.TestAddPublishedVariables_AmbiguousButtons1;
var
  Btn1: Dsgn_BearButtons.TBearButton;
begin
  Btn1:=Dsgn_BearButtons.TBearButton.Create(BearForm1);
  Btn1.Name:='Button1';

  TestCompleteComponent('TestAddPublishedVariables_AmbiguousButtons1',true,
    ['unit test1;'
    ,'{$mode objfpc}{$H+}'
    ,'interface'
    ,'uses Dsgn_BearControls, Dsgn_BearButtons, Dsgn_AmbigBearBtn;'
    ,'type'
    ,'  TBearForm1 = class(TBearForm)'
    ,'  end;'
    ,'implementation'
    ,'end.'],
    ['unit test1;'
    ,'{$mode objfpc}{$H+}'
    ,'interface'
    ,'uses Dsgn_BearControls, Dsgn_BearButtons, Dsgn_AmbigBearBtn;'
    ,'type'
    ,'  { TBearForm1 }'
    ,'  TBearForm1 = class(TBearForm)'
    ,'    Button1: Dsgn_BearButtons.TBearButton;'
    ,'  end;'
    ,'implementation'
    ,'end.']);
end;

procedure TTestDesignerFormTools.TestAddPublishedVariables_AmbiguousButtons2;
var
  Btn1: Dsgn_BearButtons.TBearButton;
  Btn2: Dsgn_AmbigBearBtn.TBearButton;
  Label1: TBearLabel;
begin
  Btn1:=Dsgn_BearButtons.TBearButton.Create(BearForm1);
  Btn1.Name:='Button1';
  Btn2:=Dsgn_AmbigBearBtn.TBearButton.Create(BearForm1);
  Btn2.Name:='Button2';
  Label1:=TBearLabel.Create(BearForm1);
  Label1.Name:='Label1';

  TestCompleteComponent('TestAddPublishedVariables_AmbiguousButtons2',true,
    ['unit test1;'
    ,'{$mode objfpc}{$H+}'
    ,'interface'
    ,'uses Dsgn_BearControls, Dsgn_BearButtons, Dsgn_AmbigBearBtn;'
    ,'type'
    ,'  TBearForm1 = class(TBearForm)'
    ,'  end;'
    ,'implementation'
    ,'end.'],
    ['unit test1;'
    ,'{$mode objfpc}{$H+}'
    ,'interface'
    ,'uses Dsgn_BearControls, Dsgn_BearButtons, Dsgn_AmbigBearBtn;'
    ,'type'
    ,'  { TBearForm1 }'
    ,'  TBearForm1 = class(TBearForm)'
    ,'    Button1: Dsgn_BearButtons.TBearButton;'
    ,'    Button2: Dsgn_AmbigBearBtn.TBearButton;'
    ,'    Label1: TBearLabel;'
    ,'  end;'
    ,'implementation'
    ,'end.']);
end;

procedure TTestDesignerFormTools.TestGatherPublishedVarTypes_Empty;
begin
  TestGatherPublishedVarType('TestGatherPublishedVarTypes_Empty',
    'TBearForm1',
    ['unit test1;'
    ,'{$mode objfpc}{$H+}'
    ,'interface'
    ,'uses Dsgn_BearControls, Dsgn_BearButtons, Dsgn_AmbigBearBtn;'
    ,'type'
    ,'  TBearForm1 = class(TBearForm)'
    ,'  end;'
    ,'implementation'
    ,'end.'],
   []
    );
end;

procedure TTestDesignerFormTools.TestGatherPublishedVarTypes_Button1;
begin
  TestGatherPublishedVarType('TestGatherPublishedVarTypes_Empty',
    'TBearForm1',
    ['unit test1;'
    ,'{$mode objfpc}{$H+}'
    ,'interface'
    ,'uses Dsgn_BearControls, Dsgn_BearButtons, Dsgn_AmbigBearBtn;'
    ,'type'
    ,'  TBearForm1 = class(TBearForm)'
    ,'    Button1: TBearButton;'
    ,'  end;'
    ,'implementation'
    ,'end.'],
   ['Button1:Dsgn_AmbigBearBtn.TBearButton']
    );
end;

procedure TTestDesignerFormTools.TestGatherPublishedVarTypes_Button2;
begin
  TestGatherPublishedVarType('TestGatherPublishedVarTypes_Empty',
    'TBearForm1',
    ['unit test1;'
    ,'{$mode objfpc}{$H+}'
    ,'interface'
    ,'uses Dsgn_BearControls, Dsgn_BearButtons, Dsgn_AmbigBearBtn;'
    ,'type'
    ,'  TBearForm1 = class(TBearForm)'
    ,'    Button1: TBearButton;'
    ,'    Button2: Dsgn_BearButtons.TBearButton;'
    ,'    Label1: TBearLabel;'
    ,'  end;'
    ,'implementation'
    ,'end.'],
   ['Button1:Dsgn_AmbigBearBtn.TBearButton',
    'Button2:Dsgn_BearButtons.TBearButton',
    'Label1:Dsgn_BearControls.TBearLabel']
    );
end;

initialization
  RegisterTests([TTestDesignerFormTools]);

end.

