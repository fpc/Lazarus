{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit Cody; 

interface

uses
  PPUListDlg, CodyStrConsts, AddAssignMethodDlg, CodyCtrls, CodyFrm, 
  LazarusPackageIntf;

implementation

procedure Register; 
begin
  RegisterUnit('PPUListDlg', @PPUListDlg.Register); 
  RegisterUnit('AddAssignMethodDlg', @AddAssignMethodDlg.Register); 
  RegisterUnit('CodyCtrls', @CodyCtrls.Register); 
  RegisterUnit('CodyFrm', @CodyFrm.Register); 
end; 

initialization
  RegisterPackage('Cody', @Register); 
end.
