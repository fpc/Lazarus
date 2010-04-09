{ Этот файл был автоматически создан Lazarus. Н�
  � редактировать!
  Исходный код используется только для комп�
    �ляции и установки пакета.
 }

unit sdflaz;

interface

uses
  RegisterSDF, sdfdata, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('RegisterSDF',@RegisterSDF.Register);
end;

initialization
  RegisterPackage('SDFLaz',@Register);
end.
