{ This file was automatically created by Lazarus. Do not edit!
This source is only used to compile and install the package.
 }

unit lr_add_function; 

interface

uses
  lrAddFunctionLibrary, lr_add_function_const, frFuncDate, frFuncNum, 
    frFuncSQL, frFuncStr, LazarusPackageIntf; 

implementation

procedure Register; 
begin
  RegisterUnit('lrAddFunctionLibrary', @lrAddFunctionLibrary.Register); 
end; 

initialization
  RegisterPackage('lr_add_function', @Register); 
end.
