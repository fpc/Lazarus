{  $Id$  }
{
 /***************************************************************************
                            pkgmanager.pas
                            --------------


 ***************************************************************************/

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
 *   Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.        *
 *                                                                         *
 ***************************************************************************

  Author: Mattias Gaertner

  Abstract:
    Methods and Types to access the IDE packages.
}
unit PackageIntf;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms;
  
type
  TPackageEditingInterface = class(TComponent)
  public
    function AddUnitDependenciesForComponentClasses(const UnitFilename: string;
                         ComponentClassnames: TStrings): TModalResult; virtual; abstract;
    function GetOwnersOfUnit(const UnitFilename: string): TList; virtual; abstract;
    procedure ExtendOwnerListWithUsedByOwners(OwnerList: TList); virtual; abstract;
    function GetSourceFilesOfOwners(OwnerList: TList): TStrings; virtual; abstract;
  end;
  
var
  PackageEditingInterface: TPackageEditingInterface; // will be set by the IDE

implementation

initialization
  PackageEditingInterface:=nil;

end.

