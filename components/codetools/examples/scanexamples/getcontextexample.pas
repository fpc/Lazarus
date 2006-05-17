unit GetContextExample; 

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils; 

implementation

procedure CallOverloadedProc;
begin
  Pos('','');
end;

end.

