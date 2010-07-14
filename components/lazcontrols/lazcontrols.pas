{ Этот файл был автоматически создан Lazarus. Не редактировать!
  Исходный код используется только для компиляции и установки пакета.
 }

unit lazcontrols;

interface

uses
  DividerBevel, ExtendedNotebook, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('DividerBevel',@DividerBevel.Register);
  RegisterUnit('ExtendedNotebook',@ExtendedNotebook.Register);
end;

initialization
  RegisterPackage('LazControls',@Register);
end.
