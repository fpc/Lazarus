program MakeSite;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, fMasterSite, fclientform, EasyDockMgr, LResources, uMakeSite
  { you can add units after this };

{$IFDEF WINDOWS}{$R MakeSite.rc}{$ENDIF}

begin
  {$I MakeSite.lrs}
  Application.Initialize;
  Application.CreateForm(TMasterSite, MasterSite);
  Application.Run;
end.

