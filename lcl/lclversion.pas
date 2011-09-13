{  $Id$  }
{
 /***************************************************************************
                               lclversion.pas
                             -------------------
                             Version numbers for the LCL

 ***************************************************************************/

 *****************************************************************************
 *                                                                           *
 *  This file is part of the Lazarus Component Library (LCL)                 *
 *                                                                           *
 *  See the file COPYING.modifiedLGPL.txt, included in this distribution,    *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
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

const
  lcl_major = 0;
  lcl_minor = 9;
  lcl_release = 31;
  lcl_patch = 0;
  lcl_fullversion = ((lcl_major *  100 + lcl_minor) * 100 + lcl_release) * 100 + lcl_patch;
  lcl_version = '0.9.31';

implementation

end.

