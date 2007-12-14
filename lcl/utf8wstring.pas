{
 /***************************************************************************
                                  utf8wstring.pas
                                  -----------
                             UTF-8 Widestring manager


 ***************************************************************************/

 *****************************************************************************
 *                                                                           *
 *  This file is part of the Lazarus Component Library (LCL)                 *
 *                                                                           *
 *  See the file COPYING.modifiedLGPL, included in this distribution,        *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************

  Ensures that we have a reliable encoding for ansistrings (UTF-8 Encoding),
  until a new type for utf-8 strings is created.
}
unit utf8wstring;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils; 

procedure UTF8Wide2AnsiMove(source:pwidechar;var dest:ansistring;len:SizeInt);
procedure UTF8Ansi2WideMove(source:pchar;var dest:widestring;len:SizeInt);

implementation

procedure UTF8Wide2AnsiMove(source: pwidechar; var dest: ansistring; len: SizeInt);
var
  Buffer: WideString;
begin
  Buffer := Copy(source, 1, len);
  dest := UTF8Encode(Buffer);
end;

procedure UTF8Ansi2WideMove(source: pchar; var dest: widestring; len: SizeInt);
var
  Buffer: ansistring;
begin
  Buffer := Copy(source, 1, len);
  dest := UTF8Decode(Buffer);
end;

initialization

  Widestringmanager.Wide2AnsiMoveProc := @UTF8Wide2AnsiMove;
  Widestringmanager.Ansi2WideMoveProc := @UTF8Ansi2WideMove;

end.

