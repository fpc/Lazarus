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
unit UTrace;

{$mode objfpc}{$H+}


interface

uses sysutils;

type
  // The fpc 1.0.x and the old 1.1 (older than January 2003) defines the
  // type of TheAddr as longint.
  // The current fpc 1.1 defines it as pointer. There is no flag to distinguish
  // the new from the old 1.1, so we have to define our own. Because the current
  // 1.1 does not compile an expression in synedit and is much more unstable
  // than the old 1.1, the old is the default for the 1.1 series.
  // To compile the LCL with a current fpc, you have to add -dNEW1_1 to the
  // options. i.e. add -dNEW1_1 to the build lazarus options.
  TAssertErrorAddrType = {$IFDEF NEW1_1}Pointer{$ELSE}Longint{$ENDIF};

  TAssertErrorProc = procedure(Const Msg,FN :ShortString;
        LineNo: LongInt; TheAddr: TAssertErrorAddrType);

var
  TraceFileName : string;
  OldProcPointer : TAssertErrorProc;  // the current Assert Error Handler


implementation

procedure TraceAssertHandler(Const Msg,FN : ShortString;
  LineNo: LongInt; TheAddr: TAssertErrorAddrType);
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
