unit ChangeParamList1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils; 

procedure DoNoParams;
procedure DoOneParam(c: char);
procedure DoTwoParams1(a: char; b: word);
procedure DoTwoParams2(a, b: string);

implementation

procedure DoNoParams;
begin
  DoTwoParams2('1','2');
end;

procedure DoOneParam(c: char);
begin
  DoNoParams;
end;

procedure DoTwoParams1(a: char; b: word);
begin
  DoOneParam(a);
end;

procedure DoTwoParams2(a, b: string);
begin
  DoOneParam(a[1]);
end;

end.

