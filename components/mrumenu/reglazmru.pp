unit reglazmru;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, mrumanager, lresources;

procedure Register;

implementation

procedure Register;

begin
  RegisterComponents('Misc',[TMRUMenuManager]);
end;

initialization
  {$i regmru.lrs}
end.

