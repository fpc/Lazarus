{
 *****************************************************************************
  This file is part of LazUtils.

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************

 This unit was renamed to LazSysUtils.
 Now just give "deprecated" warnings. Will be removed later.
}
unit LazUTF8SysUtils;

{$mode objfpc}{$H+}

interface

uses
  LazSysUtils;

function NowUTC: TDateTime; deprecated 'Use the function from unit LazSysUtils.';
function GetTickCount64: QWord; deprecated 'Use the function from unit LazSysUtils.';

implementation

function NowUTC: TDateTime;
begin
  result := LazSysUtils.NowUTC;
end;

function GetTickCount64: QWord;
begin
  Result := LazSysUtils.GetTickCount64;
end;

end.

