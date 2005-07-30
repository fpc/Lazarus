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
  RegisterCustomForm(TCustomFormDescr.Create(TAppForm));
  RegisterCustomForm(TCustomFormDescr.Create(TDBAppForm));
end;

initialization
  RegisterAppForms;
end.

