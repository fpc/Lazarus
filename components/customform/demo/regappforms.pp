unit regappforms;

{$mode objfpc}{$H+}

interface

uses
  custforms, appform, dbappform;

procedure Register;

implementation

procedure Register;
begin
  ; // Do nothing. Custom forms are registered during initialization.
    // For some reason does not work if registered here.
end;

initialization
  RegisterCustomForm(TAppForm,'AppForms');
  RegisterCustomForm(TDBAppForm,'AppForms');

end.

