{ Этот файл был автоматически создан Lazarus. Не редактировать!
  Исходный код используется только для компиляции и установки пакета.
 }

unit lazopenglcontext;

interface

uses
  OpenGLContext, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('OpenGLContext',@OpenGLContext.Register);
end;

initialization
  RegisterPackage('LazOpenGLContext',@Register);
end.
