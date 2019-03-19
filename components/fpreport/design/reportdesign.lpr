program reportdesign;

{$mode objfpc}{$H+}


uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  // Database connectivity
  sqldb,
  reportconns,
  fpreport,
  fplazreport,
  fpreportdb,
  fpreportbarcode,
  fpreportqrcode,
  // Exports
  fpreportpdfexport,
  fpreporthtmlexport,
  fpreportpreview,
  fpreportprinterexport,
  fpreportformexport,
  Interfaces, // this includes the LCL widgetset
  Forms,
  runtimetypeinfocontrols,
  // These configure various designer factories
  regfpdesigner,
  frmfpreportdesignermain,
  fpreportdatadbf,
  fpreportdatasqldb,
  fpreportdatajson,
  frafpreportcsvdata,
  frafpreportdbfdata,
  frafpreportjsondata,
  frafpreportsqldbdata,
  frmideselectreportdata,
  // Various forms
  frmfprdresizeelements,
  frmfpreportalignelements,
  frmfpreportdataconnectioneditor,
  frmconfigreportdata,
  frmfpreportvariables,
  frmfpreportproperties,
  frmfpreportpreviewdata,
  frmfpreportimageedit,

  frmlazreportimportlog,
  cfgfpreportpdfexport,
  cfgfpreportimageexport,
  cfgfpreporthtmlexport;

{$R *.res}

TYpe

  { Tlogger }

  Tlogger = Class(TObject)
    Constructor Create;
  private
    procedure DoLog(Sender: TSQLConnection; EventType: TDBEventType; const Msg: String);
  end;

var
  FPReportDesignerForm: TFPReportDesignerForm;

{ Tlogger }

constructor Tlogger.Create;
begin
  GlobalDBLogHook:=@DoLog;
end;

procedure Tlogger.DoLog(Sender: TSQLConnection; EventType: TDBEventType; const Msg: String);
begin
  Writeln(Eventtype:10,': ',Msg);
end;

begin
//  Tlogger.Create;
  RequireDerivedFormResource:=True;
  RegisterFPReportPropEditors;
  Application.Initialize;
  Application.CreateForm(TFPReportDesignerForm,FPReportDesignerForm);
  Application.Scaled:=False;
  if (ParamCount>0) then
    FPReportDesignerForm.InitialFileName:=ParamStr(1);
  // Improve this to check for options ?
  Application.Run;
end.

