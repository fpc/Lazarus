{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit weblaz;

{$warn 5023 off : no warning about unused units}
interface

uses
  extjsjson, extjsxml, fpcgi, fpdatasetform, fpextdirect, fpextjs, fphtml, 
  fphttp, fpIDEExtEditorInsertFileNameUnit, fpjsonrpc, fpTemplate, fpWeb, 
  fpwebdata, fpWebFieldSetTagUnit, fpWebHREFEditUnit, fpWebHtmlTagLegendUnit, 
  fpWebNewHTMLFileUnit, fpwebNewHTMLFormUnit, fpwebNewHTMLImgUnit, 
  fpwebNewHTMLInputUnit, fpwebNewHTMLListUnit, fpWebNewHtmlTableUnit, 
  fpWebNewHtmlTagPreUnit, fpwebNewHtmlTagTDUnit, fpwebNewHtmlTagTRUnit, 
  fpWebSelectOptionsUnit, fpWebSelectTagUnit, fpWebStrConsts, fpWebToolsUnit, 
  frmnewhttpapp, HTMLDefs, htmlelements, htmlwriter, HTTPDefs, lazweb, 
  sqldbwebdata, webjsonrpc, WebLazIDEIntf, webutil, frmrpcmoduleoptions, 
  iniwebsession, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('fpWebToolsUnit', @fpWebToolsUnit.Register);
  RegisterUnit('WebLazIDEIntf', @WebLazIDEIntf.Register);
end;

initialization
  RegisterPackage('weblaz', @Register);
end.

