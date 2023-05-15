unit LazDelphiReg;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, IDEIntf, IDEOptionsIntf, IDEOptEditorIntf, IDEExternToolIntf, delphitool,
  fradelphioptions, fraprojectdelphioptions;

var
  IDEDelphiCompilerParserClass : TDelphiCompilerParserClass = nil;

var
  DelphiToolsFrameID : integer = 1001;
  DelphiToolsOptionsIndex : Integer = ProjectOptionsMisc + 100;

procedure Register;

implementation

uses delphioptions;

procedure Register;
begin
  if IDEDelphiCompilerParserClass=Nil then
    IDEDelphiCompilerParserClass:=TDelphiCompilerParser;
  DelphiToolOptions.Load;
  ExternalToolList.RegisterParser(IDEDelphiCompilerParserClass);
  DelphiToolsFrameID:=RegisterIDEOptionsEditor(GroupEnvironment,TDelphiOptionsFrame, DelphiToolsFrameID)^.Index;
  DelphiToolsOptionsIndex:=RegisterIDEOptionsEditor(GroupProject,TProjectDelphiOptionsFrame, DelphiToolsOptionsIndex)^.Index;
  TDelphiTool.Instance.Hook;

end;


end.

