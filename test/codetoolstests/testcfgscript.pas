{
 Test with:
     ./runtests --format=plain --suite=TTestCodetoolsCfgScript
     ./runtests --format=plain --suite=TestCfgScriptBase
     ./runtests --format=plain --suite=TestCfgScriptLCL
}
unit TestCfgScript;

{$mode objfpc}{$H+}

{$DEFINE VerboseTestCfgScript}

interface

uses
  Classes, SysUtils, fpcunit, testglobals, FileProcs,
  CodeToolsCfgScript;

type

  { TTestCodetoolsCfgScript }

  TTestCodetoolsCfgScript = class(TTestCase)
  protected
    procedure TestResult(Script, ExpectedResult: string; Vars: PCTCfgScriptVariables = nil);
    procedure TestSyntaxError(Script, ExpectedError: string);
  published
    procedure TestCfgScriptBase;
    procedure TestCfgScriptLCL;
  end;

implementation

{ TTestCodetoolsCfgScript }

procedure TTestCodetoolsCfgScript.TestResult(Script, ExpectedResult: string;
  Vars: PCTCfgScriptVariables);
var
  Engine: TCTConfigScriptEngine;
  ScriptResult: String;
  i: Integer;
begin
  Engine:=TCTConfigScriptEngine.Create;
  try
    Engine.MaxErrorCount:=1;
    if Vars<>nil then
      Engine.Variables.Assign(Vars^);
    if not Engine.Execute(Script) then begin
      writeln('Script failed to run:');
      for i:=0 to Engine.ErrorCount-1 do
        writeln(Engine.GetErrorStr(i));
      AssertEquals('Syntax error in script "'+Script+'"',true,false);
    end else begin
      ScriptResult:=Engine.Variables['Result'];
      if ScriptResult<>ExpectedResult then
        Engine.Variables.WriteDebugReport('Variables');
      AssertEquals(Script,ExpectedResult,ScriptResult);
    end;
    if Vars<>nil then
      Vars^.Assign(Engine.Variables);
  finally
    Engine.Free;
  end;
end;

procedure TTestCodetoolsCfgScript.TestSyntaxError(Script, ExpectedError: string
  );
var
  Engine: TCTConfigScriptEngine;
begin
  Engine:=TCTConfigScriptEngine.Create;
  try
    Engine.MaxErrorCount:=1;
    if Engine.Execute(Script) then begin
      AssertEquals('Syntax error in script not recognized: "'+Script+'"',true,false);
    end else begin
      //writeln('TTestCodetoolsCfgScript.TestSyntaxError ',Engine.Errors[0].Msg);
      AssertEquals(Script,ExpectedError,Engine.Errors[0].Msg);
    end;
  finally
    Engine.Free;
  end;
end;

procedure TTestCodetoolsCfgScript.TestCfgScriptBase;
begin
  TestResult('Result:=2;','2');
  TestResult('a:=2; b:=a; Result:=b;','2');
  TestResult('Result:=1+2;','3');
  TestResult('Result:=1=2;','0');
  TestResult('Result:=1<>2;','1');
  TestResult('Result:=2>1;','1');
  TestResult('Result:=2>2;','0');
  TestResult('Result:=2<2;','0');
  TestResult('Result:=1<2;','1');
  TestSyntaxError('{invalid operator * }Result:=2*3;','expected ; of statement, but found *');
end;

procedure TTestCodetoolsCfgScript.TestCfgScriptLCL;
var
  Vars: TCTCfgScriptVariables;
begin
  Vars:=TCTCfgScriptVariables.Create;
  try
    Vars['TargetOS']:='wince';
    Vars['TargetCPU']:='arm';
    Vars['SrcOS2']:='';
    Vars['SrcOS']:='win';
    TestResult(
       '// LCLWidgetType'#10
      +'if undefined(LCLWidgetType) then begin'#10
      +'  //if GetIDEValue(''OS'')=TargetOS then begin'#10
      +'    // use the same widgettype as the IDE'#10
      +'    //LCLWidgetType := GetIDEValue(''LCLWidgetType'');'#10
      +'    //if LCLWidgetType=''nogui'' then'#10
      +'      //LCLWidgetType:='''';'#10
      +'  //end;'#10
      +'  if (LCLWidgetType='''') or undefined(LCLWidgetType) then begin'#10
      +'    if (TargetOS=''win32'') or (TargetOS=''win64'') then'#10
      +'      LCLWidgetType := ''win32'''#10
      +'    else if TargetOS=''wince'' then'#10
      +'      LCLWidgetType := ''wince'''#10
      +'    else if TargetOS=''darwin'' then'#10
      +'      LCLWidgetType := ''carbon'''#10
      +'    else'#10
      +'      LCLWidgetType:=''gtk2'';'#10
      +'  end;'#10
      +'end;'#10
      ,'',@Vars);
    //Vars.WriteDebugReport('LCL','  ');
    AssertEquals('LCLWidgetType','wince',Vars['LCLWidgetType']);
  finally
    Vars.Free;
  end;
end;

initialization
  AddToCodetoolsTestSuite(TTestCodetoolsCfgScript);

end.

