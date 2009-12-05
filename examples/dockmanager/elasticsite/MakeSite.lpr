program MakeSite;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, fMasterSite, fclientform, EasyDockMgr;

{$IFDEF WINDOWS}{$R MakeSite.rc}{$ENDIF}

begin
  Application.Initialize;
  Application.CreateForm(TMasterSite, MasterSite);
  Application.Run;
end.

