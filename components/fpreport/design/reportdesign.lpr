program reportdesign;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms,
  runtimetypeinfocontrols,
  fpreportdb,
  // These configure various designer factories
  regfpdesigner,
  frmfpreportdesignermain,
  // Various report data
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
  cfgfpreportpdfexport,
  cfgfpreportimageexport,
  cfgfpreporthtmlexport,

  //
  fpreporthtmlexport,
  fpreportpreview,
  fpreportformexport;

{$R *.res}

var
  FPReportDesignerForm: TFPReportDesignerForm;

begin
  RequireDerivedFormResource:=True;
  RegisterFPReportPropEditors;
  Application.Initialize;
  Application.CreateForm(TFPReportDesignerForm,FPReportDesignerForm);
  Application.Scaled:=False;
  Application.Run;
end.

