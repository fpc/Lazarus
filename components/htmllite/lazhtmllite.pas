{  This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install
  the package LazHTMLLite 0.1.1.
 }

unit LazHTMLLite; 

interface

uses
  HTMLLite, LiteDith, LitePars, LiteReadThd, LiteSbs1, LiteSubs, LiteUn2, 
    LiteGIF2, LazarusPackageIntf; 

implementation

procedure Register; 
begin
  RegisterUnit('HTMLLite', @HTMLLite.Register); 
end; 

initialization
  RegisterPackage('LazHTMLLite', @Register)
end.
