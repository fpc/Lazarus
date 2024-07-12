unit LazDebuggerUtils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, StrUtils;

function Dec64ToNumb(N: QWord; Len, Base: Byte): string; overload;
function Dec64ToNumb(N: Int64; Len, Base: Byte): string; inline; overload;

implementation

function Dec64ToNumb(N: QWord; Len, Base: Byte): string; overload;
var
  C: Integer;
  Number: QWord;
begin
  if N=0 then
    Result:='0'
  else
  begin
    Number:=N;
    Result:='';
    while Number>0 do begin
      C := Number mod Base;
      if C>9 then
        C:=C+55
      else
        C:=C+48;
      Result:=Chr(C)+Result;
      Number:=Number div Base;
    end;
  end;
  if (Result<>'') then
    Result:=AddChar('0',Result,Len);
end;

function Dec64ToNumb(N: Int64; Len, Base: Byte): string; inline; overload;
begin
  Result := Dec64ToNumb(QWord(N), Len, Base);
end;

end.

