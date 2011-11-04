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

{$mode objfpc}{$H+}

interface

const
  lcl_major = 0;
  lcl_minor = 9;
  lcl_release = 30;
  lcl_patch = 2;
  lcl_version = '0.9.30.2';
  lcl_fullversion = ((lcl_major *  100 + lcl_minor) * 100 + lcl_release) * 100 + lcl_patch;

implementation

end.

