{ This file was automatically created by Lazarus. Do not edit!
This source is only used to compile and install the package.
 }

unit TAChartLazarusPkg; 

interface

uses
  TASeries, TAEngine, TAGraph, LazarusPackageIntf; 

implementation

procedure Register; 
begin
  RegisterUnit('TAGraph', @TAGraph.Register); 
end; 

initialization
  RegisterPackage('TAChartLazarusPkg', @Register); 
end.
