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

  Author: Juha Manninen

  Abstract:
    Define types and an interface needed for editing projects and packages.
}
unit ProjPackEditing;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  // LCL
  Forms, ComCtrls,
  // LazControls
  TreeFilterEdit,
  // BuildIntf
  PackageIntf,
  // IDE
  PackageDefs;

type

  TPENodeType = (
    penFile,
    penDependency
    );

  { TPENodeData }

  TPENodeData = class(TTFENodeData)
  public
    Typ: TPENodeType;
    Name: string; // file or package name
    Removed: boolean;
    FileType: TPkgFileType;
    Next: TPENodeData;
    constructor Create(aTyp: TPENodeType; aName: string; aRemoved: boolean);
  end;

  { IFilesEditorInterface
    An editor with a TTreeView with files and dependencies }

  IFilesEditorInterface = interface
    function FilesEditTreeView: TTreeView;
    function TVNodeFiles: TTreeNode;
    function TVNodeRequiredPackages: TTreeNode;
    function FilesEditForm: TCustomForm;
    function FilesOwner: TObject; // TProject or TLazPackage
    function FilesOwnerName: string; // for debugging purposes
    procedure BeginUpdate;
    procedure EndUpdate;
    function GetNodeDataItem(TVNode: TTreeNode; out NodeData: TPENodeData;
      out Item: TObject): boolean;
    function GetNodeFilename(Node: TTreeNode): string;
    function IsDirectoryNode(Node: TTreeNode): boolean;
    function FilesBaseDirectory: string;
    function FilesOwnerReadOnly: boolean;
    function FirstRequiredDependency: TPkgDependency;
    function ExtendUnitSearchPath(NewUnitPaths: string): boolean;
    function ExtendIncSearchPath(NewIncPaths: string): boolean;
    procedure UpdateAll(Immediately: boolean = false);
  end;

  TPEFlag = (
    pefNeedUpdateTitle,
    pefNeedUpdateFiles,
    pefNeedUpdateRemovedFiles,
    pefNeedUpdateRequiredPkgs,
    pefNeedUpdateProperties,
    pefNeedUpdateButtons,
    pefNeedUpdateApplyDependencyButton,
    pefNeedUpdateStatusBar
    );
  TPEFlags = set of TPEFlag;


implementation

{ TPENodeData }

constructor TPENodeData.Create(aTyp: TPENodeType; aName: string; aRemoved: boolean);
begin
  Typ:=aTyp;
  Name:=aName;
  Removed:=aRemoved;
end;

end.

