{ Этот файл был автоматически создан Lazarus. Н�
  � редактировать!
  Исходный код используется только для комп�
    �ляции и установки пакета.
 }

unit projtemplates;

interface

uses
    ProjectTemplates, IDETemplateProject, frmTemplateVariables, 
  LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('IDETemplateProject',@IDETemplateProject.Register);
end;

initialization
  RegisterPackage('ProjTemplates',@Register);
end.
