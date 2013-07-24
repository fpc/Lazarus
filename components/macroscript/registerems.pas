unit RegisterEMS;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SrcEditorIntf, EMScriptMacro;

procedure Register;

implementation

procedure Register;
begin
  EditorMacroPlayerClass := TEMSEditorMacro;
end;

end.

