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

