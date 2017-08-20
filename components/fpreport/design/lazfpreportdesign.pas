{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit lazfpreportdesign;

{$warn 5023 off : no warning about unused units}
interface

uses
  designreportdata, drawruler, fpreportdesignctrl, fpreportdesignobjectlist, 
  fracsvdata, fradbfdata, frajsondata, frareportdata, 
  fraReportObjectInspector, frasqldbdata, frmalignelements, 
  frmconfigreportdata, frmreportdataconnectioneditor, frmreportdesignermain, 
  frmreportimageedit, frmreportmemoedit, frmreportpreviewdata, 
  frmreportproperties, frmreportshapeedit, frmreportvariables, 
  frmresizeelements, regfpdesigner, FPReportDesigner, LazarusPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('lazfpreportdesign', @Register);
end.
