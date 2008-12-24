unit GetContextExample; 

{$mode objfpc}{$H+}

interface

uses
  {$IFDEF Windows}Windows,{$ENDIF} Classes, SysUtils;

implementation

procedure CallOverloadedProc;
begin
  Pos('','');
end;

end.

