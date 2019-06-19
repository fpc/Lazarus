{ $Id$ }
{
 ---------------------------------------------------------------------------
 fpdbgutil.pp  -  Native freepascal debugger - Utilities
 ---------------------------------------------------------------------------

 This unit contains utility functions

 ---------------------------------------------------------------------------

 @created(Mon Apr 10th WET 2006)
 @lastmod($Date$)
 @author(Marc Weustink <marc@@dommelstein.nl>)

 ***************************************************************************
 *                                                                         *
 *   This source is free software; you can redistribute it and/or modify   *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 *   This code is distributed in the hope that it will be useful, but      *
 *   WITHOUT ANY WARRANTY; without even the implied warranty of            *
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU     *
 *   General Public License for more details.                              *
 *                                                                         *
 *   A copy of the GNU General Public License is available on the World    *
 *   Wide Web at <http://www.gnu.org/copyleft/gpl.html>. You can also      *
 *   obtain it by writing to the Free Software Foundation,                 *
 *   Inc., 51 Franklin Street - Fifth Floor, Boston, MA 02110-1335, USA.   *
 *                                                                         *
 ***************************************************************************
}
unit FpDbgUtil;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils; 

type
  THexValueFormatFlag = (hvfSigned, hvfPrefixPositive, hvfIncludeHexchar);
  THexValueFormatFlags = set of THexValueFormatFlag;

  
function CompareUtf8BothCase(AnUpper, AnLower, AnUnknown: PChar): Boolean;
function AlignPtr(Src: Pointer; Alignment: Byte): Pointer;
function HexValue(const AValue; ASize: Byte; AFlags: THexValueFormatFlags): String;
procedure Log(const AText: String; const AParams: array of const); overload;
procedure Log(const AText: String); overload;
function FormatAddress(const AAddress): String;


implementation

uses
  LazLoggerBase,
  FpDbgClasses;

function CompareUtf8BothCase(AnUpper, AnLower, AnUnknown: PChar): Boolean;
var
  p: PChar;
begin
  Result := False;
  while (AnUpper^ <> #0) and (AnUnknown^ <> #0) do begin
    p := AnUnknown;

    if (AnUpper^ = AnUnknown^) then begin
      // maybe uppercase
      inc(AnUpper);
      inc(AnUnknown);
      while ((byte(AnUpper^) and $C0) = $C0) and (AnUpper^ = AnUnknown^) do begin
        inc(AnUpper);
        inc(AnUnknown);
      end;

      if ((byte(AnUpper^) and $C0) <> $C0) then begin // equal to upper
        inc(AnLower);
        while ((byte(AnLower^) and $C0) = $C0) do
          inc(AnLower);
        Continue;
      end;
    end
    else begin
      // skip the first byte / continuation bytes are skipped if lower matches
      inc(AnUpper);
      inc(AnUnknown);
    end;

    // Not upper, try lower
    if (AnLower^ = p^) then begin
      inc(AnLower);
      inc(p);
      while ((byte(AnLower^) and $C0) = $C0) and (AnLower^ = p^) do begin
        inc(AnLower);
        inc(p);
      end;

      if ((byte(AnLower^) and $C0) <> $C0) then begin // equal to lower
        // adjust upper and unknown to codepoint
        while ((byte(AnUpper^) and $C0) = $C0) do
          inc(AnUnknown);
        while ((byte(AnUnknown^) and $C0) = $C0) do
          inc(AnUnknown);
        Continue;
      end;
    end;

    Result := False;
    exit;
  end;

  Result := AnUpper^ = AnUnknown^;  // both #0
end;

function AlignPtr(Src: Pointer; Alignment: Byte): Pointer;
begin
  Result := Pointer(((PtrUInt(Src) + Alignment - 1) and not PtrUInt(Alignment - 1)));
end;

function FormatAddress(const AAddress): String;
begin
  Result := HexValue(AAddress, DBGPTRSIZE[GMode], [hvfIncludeHexchar]);
end;

function HexValue(const AValue; ASize: Byte; AFlags: THexValueFormatFlags): String;
var
  i: Int64;
  p: PByte;
begin
  Result := '';
  if ASize > 8
  then begin
    Result := 'HexValue: size to large';
    Exit;
  end;
  if ASize = 0
  then begin
    Exit;
  end;

  p := @AValue;
  if p[ASize - 1] < $80
  then Exclude(AFlags, hvfSigned);

  if hvfSigned in AFlags
  then i := -1
  else i := 0;

  Move(AValue, i, ASize);
  if hvfSigned in AFlags
  then begin
    i := not i + 1;
    Result := '-';
  end
  else begin
    if hvfPrefixPositive in AFlags
    then Result := '+';
  end;
  if hvfIncludeHexchar in AFlags
  then Result := Result + '$';

  Result := Result + HexStr(i, ASize * 2);
end;

procedure Log(const AText: String; const AParams: array of const); overload;
begin
  DebugLn(Format(AText, AParams));
end;

procedure Log(const AText: String); overload;
begin
  DebugLn(AText);
end;

end.

