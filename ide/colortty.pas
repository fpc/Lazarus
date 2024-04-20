{
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

 Abstract:
   Unit for coloring Lazarus and LazBuild messages in LazLogger
}
unit ColorTTY;

{$mode objfpc}{$H+}

interface

uses
  LazTTY, LazLogger;

implementation

type
  TOutputColor = (oc_black, oc_red, oc_green, oc_orange, oc_blue, oc_magenta, oc_cyan, oc_lightgray);

  TTermColor = record
    fTerm: PChar;
    fColor: TOutputColor;
  end;

const
  cTermColors: array[0..5] of TTermColor =
  (
    (fTerm: 'Info:'   ; fColor: oc_lightgray),
    (fTerm: 'Note:'   ; fColor: oc_cyan     ),
    (fTerm: 'Hint:'   ; fColor: oc_cyan     ),
    (fTerm: 'Warning:'; fColor: oc_magenta  ),
    (fTerm: 'Error:'  ; fColor: oc_red      ),
    (fTerm: 'Fatal:'  ; fColor: oc_red      )
  );

function ColorizeTerm(aTerm: string; aColor: TOutputColor): ansistring; inline;
begin
  case aColor of
    oc_black    : result := #27'[1m'#27'[30m';
    oc_red      : result := #27'[1m'#27'[31m';
    oc_green    : result := #27'[1m'#27'[32m';
    oc_orange   : result := #27'[1m'#27'[33m';
    oc_blue     : result := #27'[1m'#27'[34m';
    oc_magenta  : result := #27'[1m'#27'[35m';
    oc_cyan     : result := #27'[1m'#27'[36m';
    oc_lightgray: result := #27'[1m'#27'[37m';
  end;
  result := result + aTerm + #27'[0m';
end;

{ TColorTTY }

type
  TColorTTY = class
    class procedure DoLazLoggerDebugLnEx({%H-}Sender: TObject; var LogTxt, {%H-}LogIndent: string;
      var {%H-}Handled: Boolean; const AnInfo: TLazLoggerWriteExEventInfo);
  end;

class procedure TColorTTY.DoLazLoggerDebugLnEx(Sender: TObject; var LogTxt, LogIndent: string;
  var Handled: Boolean; const AnInfo: TLazLoggerWriteExEventInfo);
var
  i, lTermPos, lTermLen: integer;
  lPart1, lPart2, lPart3: ansistring;
begin
  // do not change message if colors are not supported
  if not TTYCheckSupported then {%H-}exit;
  if not Assigned(AnInfo.LogText) then exit;
  if not IsATTY(AnInfo.LogText^) then exit;

  for i := 0 to high(cTermColors) do
  begin
    lTermPos := pos(cTermColors[i].fTerm, LogTxt);
    if lTermPos <= 0 then continue;
    lTermLen := length(cTermColors[i].fTerm);
    lPart1 := copy(LogTxt, 1, lTermPos - 1);
    lPart2 := copy(LogTxt, lTermPos, lTermLen);
    lPart3 := copy(LogTxt, lTermPos + lTermLen, length(LogTxt));
    LogTxt := lPart1 + ColorizeTerm(lPart2, cTermColors[i].fColor) + lPart3;
    exit;
  end;
end;

initialization
  DebugLogger.OnDebugLnEx := @TColorTTY.DoLazLoggerDebugLnEx;
end.

