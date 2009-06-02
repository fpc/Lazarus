Library mod_fileupload;

{$mode objfpc}{$H+}

Uses
{$ifdef unix}
  cthreads,
{$endif}
  httpd,fpApache, webmodule;

Const

{ The following constant is used to export the module record. It must 
  always match the name in the LoadModule statement in the apache
  configuration file(s). It is case sensitive !}
  ModuleName='mod_fileupload';

{ The following constant is used to determine whether the module will
  handle a request. It should match the name in the SetHandler statement
  in the apache configuration file(s). It is not case sensitive. }

  HandlerName=ModuleName;

Var
  DefaultModule : module; {$ifdef unix} public name ModuleName;{$endif unix}

Exports defaultmodule name ModuleName;

{$IFDEF WINDOWS}{$R mod_fileupload.rc}{$ENDIF}

begin
  Application.Title:='mod_fileupload';
  Application.ModuleName:=ModuleName;
  Application.HandlerName:=HandlerName;
  Application.SetModuleRecord(DefaultModule);
  Application.Initialize;
end.

