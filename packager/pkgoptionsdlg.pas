{  $Id$  }
{
 /***************************************************************************
                            pkgoptionsdlg.pas
                            -----------------


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
    TPackageOptionsDialog is the form for the general options of a package.
}
unit PkgOptionsDlg;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Buttons, LResources, ExtCtrls, StdCtrls,
  IDEProcs, PackageDefs, PackageSystem;
  
type
  TPackageOptionsDialog = class(TForm)
    Notebook: TNotebook;
    // description page
    DescriptionPage: TPage;
    DescriptionGroupBox: TGroupBox;
    DescriptionMemo: TMemo;
    UsageRadioGroup: TRadioGroup;
    BuildRadioGroup: TRadioGroup;
    AuthorLabel: TLabel;
    AuthorEdit: TEdit;
    // usage page
    UsagePage: TPage;
    
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

{ TPackageOptionsDialog }

constructor TPackageOptionsDialog.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
end;

destructor TPackageOptionsDialog.Destroy;
begin
  inherited Destroy;
end;

end.

