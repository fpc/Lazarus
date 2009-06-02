program fileupload;

{$mode objfpc}{$H+}

uses
  fpWeb,fpCGI, webmodule;

{$IFDEF WINDOWS}{$R fileupload.rc}{$ENDIF}

begin
  Application.Title:='fileupload';
  Application.Initialize;
  Application.CreateForm(TFPWebModule1, FPWebModule1);
  Application.Run;
end.

