unit regappforms;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, appform, dbappform;

procedure RegisterAppForms;

implementation

uses custforms;

procedure RegisterAppForms;

begin
  RegisterCustomForm(TAppForm,'AppForms');
  RegisterCustomForm(TDBAppForm,'AppForms');
end;

initialization
  RegisterAppForms;
end.

