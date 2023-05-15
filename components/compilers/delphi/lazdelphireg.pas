unit LazDelphiReg;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, IDEIntf, IDEOptionsIntf, IDEOptEditorIntf, IDEExternToolIntf, delphitool, fradelphioptions;

var
  IDEDelphiCompilerParserClass : TDelphiCompilerParserClass = nil;

var
  DelphiToolsFrameID: integer = 1001;

procedure Register;

implementation

uses delphioptions;

procedure Register;
begin
  if IDEDelphiCompilerParserClass=Nil then
    IDEDelphiCompilerParserClass:=TDelphiCompilerParser;
  ExternalToolList.RegisterParser(IDEDelphiCompilerParserClass);
  DelphiToolsFrameID:=RegisterIDEOptionsEditor(  GroupEnvironment,TDelphiOptionsFrame, DelphiToolsFrameID)^.Index;
  DelphiToolOptions.Load;
  TDelphiTool.Instance.Hook;

end;


end.

