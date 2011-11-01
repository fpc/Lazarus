{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit lazreport; 

interface

uses
  LR_About, LR_BarC, LR_BndEd, LR_Class, LR_Const, LR_DBSet, LR_Desgn, 
  LR_Dopt, LR_E_CSV, lr_e_gen, LR_E_HTM, LR_E_TXT, LR_Edit, LR_Ev_ed, 
  lr_expres, LR_Flds, LR_fmted, lr_funct_editor_unit, lr_funct_editor_unit1, 
  LR_GEdit, LR_GrpEd, LR_IFlds, LR_Pars, LR_pgopt, LR_PGrid, LR_PrDlg, 
  LR_Prntr, LR_progr, lr_propedit, LR_Register, LR_RRect, LR_Shape, LR_Utils, 
  LR_Var, LR_Vared, LR_View, LR_Newrp, Barcode, LazarusPackageIntf;

implementation

procedure Register; 
begin
  RegisterUnit('LR_Register', @LR_Register.Register); 
end; 

initialization
  RegisterPackage('lazreport', @Register); 
end.
