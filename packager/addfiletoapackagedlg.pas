{  $Id$  }
{
 /***************************************************************************
                         addfiletoapackagedlg.pas
                         ------------------------


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
    The dialog for selecting the package to add a file to.
}
unit AddFileToAPackageDlg;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Buttons, StdCtrls, LazarusIDEStrConsts,
  IDEProcs, IDEOptionDefs, PackageDefs, PackageSystem;
  
type
  TAddFileToAPackageDlg = class(TForm)
    procedure AddFileToAPackageDlgClose(Sender: TObject;
      var Action: TCloseAction);
  private
    procedure SetupComponents;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  end;
  
function ShowAddFileToAPackageDlg(const Filename: string): TModalResult;

implementation

function ShowAddFileToAPackageDlg(const Filename: string): TModalResult;
var
  Dialog: TAddFileToAPackageDlg;
begin
  Dialog:=TAddFileToAPackageDlg.Create(Application);
  Result:=Dialog.ShowModal;
  Dialog.Free;
end;

{ TAddFileToAPackageDlg }

procedure TAddFileToAPackageDlg.AddFileToAPackageDlgClose(Sender: TObject;
  var Action: TCloseAction);
begin
  IDEDialogLayoutList.SaveLayout(Self);
end;

procedure TAddFileToAPackageDlg.SetupComponents;
begin

end;

constructor TAddFileToAPackageDlg.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  Name:='AddFileToAPackageDlg';
  //fPackages:=TAVLTree.Create(@CompareLazPackageID);
  Position:=poScreenCenter;
  IDEDialogLayoutList.ApplyLayout(Self,500,300);
  SetupComponents;
  OnClose:=@AddFileToAPackageDlgClose;
end;

destructor TAddFileToAPackageDlg.Destroy;
begin
  inherited Destroy;
end;

end.

