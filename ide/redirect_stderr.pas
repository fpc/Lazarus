unit redirect_stderr;

{$mode objfpc}{$H+}

interface

uses
  heaptrc, SysUtils, raw_window;
  
Var
  DoShowWindow : Boolean = True;

implementation

const
  ErrorBufferLength = 2 * 1024;

var
  ErrorBuf : array[0..ErrorBufferLength] of char;
  ErrorLen : SizeInt;

  ErrorMsg : String = '';
  MyStdErr : Text;

Function ErrorWrite(Var F: TextRec): Integer;
{
  An error message should always end with #13#10#13#10
}
var
  i : SizeInt;
Begin
  while F.BufPos>0 do
  begin
    if F.BufPos+ErrorLen>ErrorBufferLength then
      i:=ErrorBufferLength-ErrorLen
    else
      i:=F.BufPos;
    Move(F.BufPtr^,ErrorBuf[ErrorLen],i);
    inc(ErrorLen,i);
    ErrorBuf[ErrorLen]:=#0;
    if ErrorLen >= ErrorBufferLength then
    begin
      ErrorMsg := ErrorMsg + String(ErrorBuf);
      ErrorLen:=0;
      ErrorBuf[ErrorLen]:=#0;
    end;
    Dec(F.BufPos,i);
  end;
  ErrorWrite:=0;
End;


Function ErrorClose(Var F: TextRec): Integer;
begin
  if ErrorLen>0 then
  begin
    ErrorMsg := ErrorMsg + String(ErrorBuf);
    ErrorLen:=0;
  end;
  If (ErrorMsg <> '') And DoShowWindow Then Begin
    ShowWindow(ErrorMsg);
    ErrorMsg := '';
  end;
  ErrorLen:=0;
  ErrorBuf[ErrorLen]:=#0;
  ErrorClose:=0;
end;

Function ErrorFlush(Var F: TextRec): Integer;
begin
  ErrorWrite(F);
  if ErrorLen>0 then
   begin
     ErrorMsg := ErrorMsg + String(ErrorBuf);
     ErrorLen:=0;     
   end;
  ErrorLen:=0;
  ErrorBuf[ErrorLen]:=#0;
  ErrorFlush:=0;
end;

Function ErrorOpen(Var F: TextRec): Integer;
Begin
  TextRec(F).InOutFunc:=@ErrorWrite;
  TextRec(F).FlushFunc:=@ErrorFlush;
  TextRec(F).CloseFunc:=@ErrorClose;
  ErrorLen:=0;
  ErrorBuf[ErrorLen]:=#0;
  ErrorOpen:=0;
  ErrorMsg := '';
End;


procedure AssignError(Var T: Text);
begin
  Assign(T,'');
  TextRec(T).OpenFunc:=@ErrorOpen;
  Rewrite(T);
end;

initialization
  AssignError(MyStdErr);
  SetHeapTraceOutput(MyStdErr);

finalization

end.

