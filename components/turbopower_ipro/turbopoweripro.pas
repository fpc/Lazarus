{ This file was automatically created by Lazarus. do not edit!
  This source is only used to compile and install the package.
 }

unit turbopoweripro; 

interface

uses
    IpAnim, IpConst, Ipfilebroker, IpHtml, IpHtmlPv, IpMsg, IpStrms, IpUtils, 
  LazarusPackageIntf;

implementation

procedure Register; 
begin
  RegisterUnit('Ipfilebroker', @Ipfilebroker.Register); 
  RegisterUnit('IpHtml', @IpHtml.Register); 
end; 

initialization
  RegisterPackage('TurboPowerIPro', @Register); 
end.
