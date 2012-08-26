unit RegisterEMS;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SrcEditorIntf, EMScriptMacro;

procedure Register;

implementation

procedure Register;
begin
end;

initialization
  // Register is to late
  EditorMacroPlayerClass := TEMSEditorMacro;

end.

