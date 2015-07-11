{ Этот файл был автоматически создан Lazarus. Не редактировать!
  Исходный код используется только для компиляции и установки пакета.
 }

unit editortoolbar;

interface

uses
    jumpto_impl, editortoolbar_impl, EdtTbConfigFrm, editortoolbar_str, 
  LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('editortoolbar_impl',@editortoolbar_impl.Register);
end;

initialization
  RegisterPackage('editortoolbar',@Register);
end.
