unit utrace;

{$mode objfpc}


interface

uses sysutils;

type
   TAssertErrorProc = procedure(Const Msg,FN : String;LineNo,TheAddr : Longint);

var
   TraceFileName : string;
   OldProcPointer : Pointer;  // the current Assert Error Handler


implementation

procedure TraceAssertHandler(Const Msg,FN : String;LineNo,TheAddr : Longint);
var
   fileH  : Text;

begin

   if LowerCase(LeftStr(Msg, 6)) = 'trace:' then
   begin
      Assign(fileH, TraceFileName);
      {$I-}
      if TraceFileName <> '' then
         if FileExists(TraceFileName) = False then
         begin
            Rewrite(fileH);
            Close(fileH);
         end;

      Append(fileH);

      if ioresult = 0 then
         Writeln(fileH, RightStr(Msg, Length(Msg) - 6));

      Close(fileH);
      {$I+}
   end
   else
      TAssertErrorProc(oldProcPointer)(Msg, FN, LineNo, TheAddr);

end;


initialization

   TraceFileName := '';
   OldProcPointer := AssertErrorProc;  // the current Assert Error Handler
   AssertErrorProc := @TraceAssertHandler  // set to new Assert Error Handler

end.
