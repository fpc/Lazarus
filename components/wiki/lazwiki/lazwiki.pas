{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit LazWiki;

interface

uses
  Wiki2FPDocConvert, WikiFormat, Wiki2XHTMLConvert, Wiki2CHMConvert, 
  WikiParser, Wiki2HTMLConvert, wikichmwriter, wikichmfilewriter, 
  wikichmfiftimain, wikichmsitemap, wikichmtypes, wikichmbase, 
  wikihtmlindexer, wikichmspecialfiles, LazarusPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('LazWiki', @Register);
end.
