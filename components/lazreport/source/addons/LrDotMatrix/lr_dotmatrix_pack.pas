{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit lr_dotmatrix_pack;

interface

uses
  lr_dotmatrix_filter, lr_dotmatrix_dlg, lr_dotmatrix, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('lr_dotmatrix', @lr_dotmatrix.Register);
end;

initialization
  RegisterPackage('lr_dotmatrix_pack', @Register);
end.
