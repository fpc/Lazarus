{
 *****************************************************************************
 *                                                                           *
 *  This file is part of the Lazarus Component Library (LCL)                 *
 *                                                                           *
 *  See the file COPYING.LCL, included in this distribution,                 *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************
}
unit utrace;

{$mode objfpc}{$H+}


interface

uses sysutils;

type
  TAssertErrorProc = procedure(Const Msg,FN :ShortString;
        LineNo: LongInt; TheAddr: {$IFDEF New1_1}Pointer{$ELSE}Longint{$ENDIF});

var
  TraceFileName : string;
  OldProcPointer : TAssertErrorProc;  // the current Assert Error Handler


implementation

procedure TraceAssertHandler(Const Msg,FN : ShortString;
  LineNo: LongInt; TheAddr: {$IFDEF New1_1}Pointer{$ELSE}Longint{$ENDIF});
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
      oldProcPointer(Msg, FN, LineNo, TheAddr);

end;


initialization

   TraceFileName := '';
   OldProcPointer := AssertErrorProc;  // the current Assert Error Handler
   AssertErrorProc := @TraceAssertHandler  // set to new Assert Error Handler

end.
