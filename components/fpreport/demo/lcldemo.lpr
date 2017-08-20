program lcldemo;

uses
  udapp, regreports, rptcolumns, rptdataset, rptexpressions, rptframes,
  rptgrouping, rptimages, rptmasterdetail, rptmasterdetaildataset, rptshapes,
  rptsimplelist, rptttf;

Var
  Application : TReportDemoApplication;

begin
  Application:=TReportDemoApplication.Create(Nil);
  Application.Initialize;
  Application.Run;
end.

