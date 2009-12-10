{ Este archivo ha sido creado automáticamente por Lazarus. ¡No lo edite!
  Este fuente se utiliza sólo para compilar e instalar el paquete.
 }

unit lazreport; 

interface

uses
    LR_Class, LR_Desgn, LR_Register, LR_Flds, LR_DBSet, LR_BarC, LR_BndEd, 
  LR_PGrid, LR_View, lr_expres, lr_funct_editor_unit, lr_funct_editor_unit1, 
  LR_Prntr, LR_Edit, LR_Pars, LR_fmted, LR_Const, LR_pgopt, LR_Dopt, LR_GEdit, 
  LR_Utils, LR_GrpEd, lr_propedit, LR_progr, LR_IFlds, SysUtilsAdds, LR_RRect, 
  LR_Shape, LR_E_TXT, LR_E_HTM, LR_E_CSV, lr_e_gen, LazarusPackageIntf;

implementation

procedure Register; 
begin
  RegisterUnit('LR_Register', @LR_Register.Register); 
end; 

initialization
  RegisterPackage('lazreport', @Register); 
end.
