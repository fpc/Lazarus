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
}
unit IdeBuildManager;

{$mode objfpc}{$H+}

interface

uses
  // RTL + FCL
  Classes, SysUtils, AVL_Tree, System.UITypes,
  // IdeIntf
  SrcEditorIntf,
  // IdeProject
  Project,
  // IDE
  EditableProject, BuildManager;

type

  { TIdeBuildManager }

  TIdeBuildManager = class(TBuildManager)
  private
  protected
    function EditorUnitInfoModified(AnUnitInfo: TUnitInfo): boolean; override;
  public
    //constructor Create(AOwner: TComponent); override;
    //destructor Destroy; override;
  end;

implementation

{ TIdeBuildManager }

function TIdeBuildManager.EditorUnitInfoModified(AnUnitInfo: TUnitInfo): boolean;
var
  EditComp: TSourceEditorInterface;
begin
  Result:=false;
  if AnUnitInfo=nil then exit;
  if TEditableUnitInfo(AnUnitInfo).EditorInfoCount=0 then exit;
  EditComp:=TEditableUnitInfo(AnUnitInfo).EditorInfo[0].EditorComponent;
  Result:=(EditComp<>nil) and EditComp.Modified;
end;

end.
