{  $Id$  }
{
 /***************************************************************************
                               lclversion.pas
                             -------------------
                             Version numbers for the LCL

 ***************************************************************************/

 *****************************************************************************
  This file is part of the Lazarus Component Library (LCL)

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************
}
unit LCLVersion;

{ At least 2.4.2 is required, except for wince which supports fpc 2.2.0+ too }
{$ifdef Wince}
  {$if defined(ver1) or (FPC_FULLVERSION<20200)}
    {$fatal Lazarus for WinCE requires at least FPC 2.2.0}
  {$endif}
{$else}
  {$if defined(ver1) or (FPC_FULLVERSION<20402) }
    {$fatal Lazarus requires at least FPC 2.4.2}
  {$endif}
{$endif}

{$mode objfpc}{$H+}

interface

uses
  LazVersion;

type
  TStringFunc = function: String;

const
  lcl_major = laz_major;
  lcl_minor = laz_minor;
  lcl_release = laz_release;
  lcl_patch = laz_patch;
  lcl_fullversion = laz_fullversion;
  lcl_version = laz_version;

var
  lcl_revision_func: TStringFunc;

implementation

function LCLRevisionFuncDummy: String;
begin
  Result := '';
end;

initialization
  lcl_revision_func := @LCLRevisionFuncDummy;

end.

