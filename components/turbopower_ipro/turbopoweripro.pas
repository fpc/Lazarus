{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit TurboPowerIPro;

{$warn 5023 off : no warning about unused units}
interface

uses
  IpAnim, IpConst, Ipfilebroker, Iphttpbroker, IpHtml, IpMsg, IpStrms, 
  IpUtils, IpHtmlTabList, iphtmlprop, ipHtmlBlockLayout, ipHtmlTableLayout, 
  LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('Ipfilebroker', @Ipfilebroker.Register);
  RegisterUnit('Iphttpbroker', @Iphttpbroker.Register);
  RegisterUnit('IpHtml', @IpHtml.Register);
end;

initialization
  RegisterPackage('TurboPowerIPro', @Register);
end.

