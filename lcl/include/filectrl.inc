{******************************************************************************
                                  Filectrl
 ******************************************************************************

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
 
{------------------------------------------------------------------------------
  DirectoryExists
 ------------------------------------------------------------------------------}
function DirectoryExists(const Name: String): Boolean;
var
  F: Longint;
  dirExist: Boolean;
begin
  //Result := FileExist(Name);
  dirExist := false; 

  F := FileGetAttr(Name);
  if F <> -1 then
    if (F and faDirectory) <> 0 then
      dirExist := true;
  Result := dirExist;
end;


{
  $Log$
  Revision 1.2  2002/05/10 06:05:52  lazarus
  MG: changed license to LGPL

  Revision 1.1  2000/07/13 10:28:25  michael
  + Initial import

  Revision 1.1  2000/04/24 05:02:43  lazarus
  Added filectrl unit for DirectoryExists function.     CAW


}

