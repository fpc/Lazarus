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

  Author: Mattias Gaertner

  Abstract:
    GUI form used in InterPkgConflictFiles
}
unit InterPkgConflictFileDlg;

{$mode objfpc}{$H+}

interface

uses
  // RTL + FCL
  Classes, SysUtils, Types, Math, Contnrs,
  // LCL
  Forms, ComCtrls, Controls, ButtonPanel, Themes, Graphics, StdCtrls, Buttons,
  // CodeTools
  FileProcs,
  // LazUtils
  LazFileUtils, LazFileCache,
  // IDEIntf
  IDEWindowIntf,
  // IdeConfig
  DialogProcs,
  // IdePackager
  PackageDefs,
  // IDE
  InterPkgConflictFiles;

type
  TPGIPCategory = (
    pgipOrphanedCompiled,
    pgipDuplicateSource
    );

  { TPGIPConflictsDialog }

  TPGIPConflictsDialog = class(TForm)
    ButtonPanel1: TButtonPanel;
    ConflictsTreeView: TTreeView;
    IDEDialogLayoutStorage1: TIDEDialogLayoutStorage;
    ImageList1: TImageList;
    procedure ConflictsTreeViewAdvancedCustomDrawItem(Sender: TCustomTreeView;
      Node: TTreeNode; {%H-}State: TCustomDrawState; Stage: TCustomDrawStage;
      var {%H-}PaintImages, {%H-}DefaultDraw: Boolean);
    procedure ConflictsTreeViewMouseDown(Sender: TObject; {%H-}Button: TMouseButton;
      {%H-}Shift: TShiftState; X, Y: Integer);
    procedure DeleteSelectedFilesButtonClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var {%H-}CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure OkButtonClick(Sender: TObject);
  private
    DeleteSelectedFilesButton: TButton;
    FImgIndexChecked: integer;
    FImgIndexUnchecked: integer;
    FCategoryNodes: array[TPGIPCategory] of TTreeNode;
    procedure UpdateButtons;
    procedure IgnoreConflicts;
  public
    FileGroups: TObjectList; // list of TPGIPAmbiguousFileGroup
    FilesChanged: boolean;
    procedure Init(Groups: TObjectList);
  end;


implementation

{$R *.lfm}

function ShowGUIHandler(AmbiguousFileGroups: TObjectList): boolean;
// Returns True if files have changed
var
  Dlg: TPGIPConflictsDialog;
begin
  // show warnings
  Dlg:=TPGIPConflictsDialog.Create(nil);
  Dlg.Init(AmbiguousFileGroups);
  if Dlg.ShowModal<>mrOK then exit(false);
  Result:=Dlg.FilesChanged;
  Dlg.Free;
end;

{ TPGIPConflictsDialog }

procedure TPGIPConflictsDialog.ConflictsTreeViewAdvancedCustomDrawItem(
  Sender: TCustomTreeView; Node: TTreeNode; State: TCustomDrawState;
  Stage: TCustomDrawStage; var PaintImages, DefaultDraw: Boolean);
var
  Detail: TThemedButton;
  Details: TThemedElementDetails;
  aSize: TSize;
  NodeRect: Classes.TRect;
  r: TRect;
begin
  if Stage<>cdPostPaint then exit;
  if TObject(Node.Data) is TPGIPAmbiguousFileGroup then begin
    if Node.ImageIndex=FImgIndexChecked then
      Detail := tbCheckBoxCheckedNormal
    else
      Detail := tbCheckBoxUncheckedNormal;
    Details := ThemeServices.GetElementDetails(Detail);
    aSize := ThemeServices.GetDetailSizeForPPI(Details,PixelsPerInch);
    NodeRect:=Node.DisplayRect(false);
    r:=Bounds(Node.DisplayIconLeft+(ImageList1.Width-aSize.cx) div 2,
       NodeRect.Top+(NodeRect.Bottom-NodeRect.Top-aSize.cy) div 2,
       aSize.cx,aSize.cy);
    ThemeServices.DrawElement(ConflictsTreeView.Canvas.Handle,Details,r);
  end;
end;

procedure TPGIPConflictsDialog.ConflictsTreeViewMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  Node: TTreeNode;
begin
  Node:=ConflictsTreeView.GetNodeAt(X,Y);
  if Node=nil then exit;
  if TObject(Node.Data) is TPGIPAmbiguousFileGroup then begin
    if (X>=Node.DisplayIconLeft) and (X<Node.DisplayTextLeft) then begin
      if Node.ImageIndex=FImgIndexChecked then
        Node.ImageIndex:=FImgIndexUnchecked
      else
        Node.ImageIndex:=FImgIndexChecked;
      Node.SelectedIndex:=Node.ImageIndex;
      UpdateButtons;
    end;
  end;
end;

function DeleteFileAndAssociates(aFile: TPGInterPkgFile): boolean;
var
  aFilename: String;
begin
  if aFile=nil then exit(true);
  aFilename:=aFile.FullFilename;
  {$IFDEF VerboseCheckInterPkgFiles}
  debugln(['DeleteFileGroup ',aFilename]);
  {$ENDIF}
  if DeleteFileInteractive(aFilename)<>mrOk then exit(false);
  if FilenameIsPascalUnit(aFilename) then
  begin
    // unit source -> delete compiled files and resources
    DeleteFileUTF8(ChangeFileExt(aFilename,'.ppu'));
    DeleteFileUTF8(ChangeFileExt(aFilename,'.o'));
    DeleteFileUTF8(ChangeFileExt(aFilename,'.rst'));
    DeleteFileUTF8(ChangeFileExt(aFilename,'.rsj'));
    DeleteFileUTF8(ChangeFileExt(aFilename,'.lfm'));
    DeleteFileUTF8(ChangeFileExt(aFilename,'.dfm'));
    DeleteFileUTF8(ChangeFileExt(aFilename,'.xfm'));
    DeleteFileUTF8(ChangeFileExt(aFilename,'.fmx'));
  end else if FilenameIsCompiledSource(aFilename) then begin
    // compiled file -> delete compiled files. Keep sources.
    DeleteFileUTF8(ChangeFileExt(aFilename,'.ppu'));
    DeleteFileUTF8(ChangeFileExt(aFilename,'.o'));
    DeleteFileUTF8(ChangeFileExt(aFilename,'.rst'));
    DeleteFileUTF8(ChangeFileExt(aFilename,'.rsj'));
    if FileExistsCached(ChangeFileExt(aFilename,'.pas'))
    or FileExistsCached(ChangeFileExt(aFilename,'.pp'))
    or FileExistsCached(ChangeFileExt(aFilename,'.p')) then begin
      // delete only compiled file
    end else begin
      // no source in this directory => delete copied lfm file
      DeleteFileUTF8(ChangeFileExt(aFilename,'.lfm'));
      DeleteFileUTF8(ChangeFileExt(aFilename,'.dfm'));
      DeleteFileUTF8(ChangeFileExt(aFilename,'.xfm'));
      DeleteFileUTF8(ChangeFileExt(aFilename,'.fmx'));
    end;
  end;
  Result:=true;
end;

procedure TPGIPConflictsDialog.DeleteSelectedFilesButtonClick(Sender: TObject);
var
  Node: TTreeNode;
  NextNode: TTreeNode;
  FileGroup: TPGIPAmbiguousFileGroup;
  IndexInGroup: integer;
  ConflictCount: Integer;
begin
  ConflictsTreeView.Items.BeginUpdate;
  try
    Node:=ConflictsTreeView.Items.GetFirstNode;
    IndexInGroup:=-1;
    ConflictCount:=0;
    while Node<>nil do
    begin
      NextNode:=Node.GetNext;
      if TObject(Node.Data) is TPGIPAmbiguousFileGroup then
      begin
        FileGroup:=TPGIPAmbiguousFileGroup(Node.Data);
        inc(IndexInGroup);
        if Node.ImageIndex=FImgIndexChecked then
        begin
          if not DeleteFileAndAssociates(FileGroup.Sources[IndexInGroup]) then exit;
          if not DeleteFileAndAssociates(FileGroup.CompiledFiles[IndexInGroup]) then exit;
        end;
        if ((FileGroup.Sources[IndexInGroup]<>nil)
        and FileExistsUTF8(FileGroup.Sources[IndexInGroup].FullFilename))
        or ((FileGroup.CompiledFiles[IndexInGroup]<>nil)
        and FileExistsUTF8(FileGroup.CompiledFiles[IndexInGroup].FullFilename))
        then
          inc(ConflictCount);
        if IndexInGroup=length(FileGroup.Sources)-1 then
        begin
          if ConflictCount<=1 then begin
            // conflict does not exist anymore
            FilesChanged:=true;
            Node:=Node.Parent;
            NextNode:=Node.GetNextSkipChildren;
            Node.Delete;
          end;
          IndexInGroup:=-1;
          ConflictCount:=0;
        end;
      end;
      Node:=NextNode;
    end;
  finally
    ConflictsTreeView.Items.EndUpdate;
    UpdateButtons;
  end;
end;

procedure TPGIPConflictsDialog.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  IDEDialogLayoutList.SaveLayout(Self);
end;

procedure TPGIPConflictsDialog.FormCreate(Sender: TObject);
var
  Details: TThemedElementDetails;
  aSize: TSize;
  Img: TBitmap;
begin
  IDEDialogLayoutList.ApplyLayout(Self);

  DeleteSelectedFilesButton:=TButton.Create(Self);
  with DeleteSelectedFilesButton do
  begin
    Name:='DeleteSelectedFilesButton';
    Caption:='Delete selected files';
    Align:=alLeft;
    AutoSize:=true;
    OnClick:=@DeleteSelectedFilesButtonClick;
    Parent:=ButtonPanel1;
  end;

  ButtonPanel1.OKButton.Kind:=bkIgnore;
  ButtonPanel1.OKButton.Caption:='Ignore';
  ButtonPanel1.OKButton.OnClick:=@OkButtonClick;

  Details := ThemeServices.GetElementDetails(tbCheckBoxCheckedNormal);
  aSize := ThemeServices.GetDetailSizeForPPI(Details, PixelsPerInch);
  ImageList1.Width:=Max(16,aSize.cx);
  ImageList1.Height:=Max(16,aSize.cy);
  // add empty images
  Img:=TBitmap.Create;
  Img.TransparentMode:=tmFixed;
  Img.TransparentColor:=0;
  Img.Transparent:=true;
  Img.SetSize(ImageList1.Width,ImageList1.Height);
  FImgIndexChecked:=ImageList1.Add(Img,nil);
  FImgIndexUnchecked:=ImageList1.Add(Img,nil);
  Img.Free;
end;

procedure TPGIPConflictsDialog.OkButtonClick(Sender: TObject);
begin
  IgnoreConflicts;
end;

procedure TPGIPConflictsDialog.UpdateButtons;
var
  Node: TTreeNode;
  DeleteCount: Integer;
  ConflictCount: Integer;
begin
  DeleteCount:=0;
  ConflictCount:=0;
  Node:=ConflictsTreeView.Items.GetFirstNode;
  while Node<>nil do begin
    if TObject(Node.Data) is TPGIPAmbiguousFileGroup then
    begin
      inc(ConflictCount);
      if Node.ImageIndex=FImgIndexChecked then
        inc(DeleteCount);
    end;
    Node:=Node.GetNext;
  end;
  DeleteSelectedFilesButton.Enabled:=DeleteCount>0;
  if ConflictCount=0 then
    IgnoreConflicts;
end;

procedure TPGIPConflictsDialog.IgnoreConflicts;
begin
  // ToDo
  ModalResult:=mrOk;
end;

procedure TPGIPConflictsDialog.Init(Groups: TObjectList);

  function AddChild(ParentNode: TTreeNode; Caption: string): TTreeNode;
  begin
    Result:=ConflictsTreeView.Items.AddChild(ParentNode,Caption);
  end;

var
  i, j: Integer;
  ItemNode: TTreeNode;
  s: String;
  FileGroupNode: TTreeNode;
  FileGroup: TPGIPAmbiguousFileGroup;
  SrcFile: TPGInterPkgFile;
  CompiledFile: TPGInterPkgFile;
  CurFile: TPGInterPkgFile;
  c: TPGIPCategory;
begin
  FileGroups:=Groups;

  ConflictsTreeView.Items.BeginUpdate;
  ConflictsTreeView.Items.Clear;
  ConflictsTreeView.Images:=ImageList1;
  for c in TPGIPCategory do
    FCategoryNodes[c]:=nil;
  for i:=0 to FileGroups.Count-1 do
  begin
    FileGroup:=TPGIPAmbiguousFileGroup(FileGroups[i]);

    // category
    if FileGroup.Sources[0]=nil then
    begin
      // orphaned compiled file
      CurFile:=FileGroup.CompiledFiles[0];
      c:=pgipOrphanedCompiled;
      if FCategoryNodes[c]=nil then
        FCategoryNodes[c]:=
          ConflictsTreeView.Items.Add(nil,'Orphaned compiled files');
    end else begin
      // duplicate source file
      CurFile:=FileGroup.Sources[0];
      c:=pgipDuplicateSource;
      if FCategoryNodes[c]=nil then
        FCategoryNodes[c]:=
          ConflictsTreeView.Items.Add(nil,'Duplicate source files');
    end;

    // file group
    s:=ExtractFilename(CurFile.ShortFilename);
    FileGroupNode:=AddChild(FCategoryNodes[c],s);

    for j:=0 to length(FileGroup.Sources)-1 do
    begin
      SrcFile:=FileGroup.Sources[j];
      CompiledFile:=FileGroup.CompiledFiles[j];

      if SrcFile<>nil then
        CurFile:=SrcFile
      else
        CurFile:=CompiledFile;

      s:=ExtractFilename(CurFile.ShortFilename);
      if CurFile.OwnerInfo.Owner is TLazPackage then
        s+=' of package '+CurFile.OwnerInfo.Name
      else
        s+=' of '+CurFile.OwnerInfo.Name;
      ItemNode:=AddChild(FileGroupNode,s);
      if SrcFile=nil then
        ItemNode.ImageIndex:=FImgIndexChecked // default: delete
      else
        ItemNode.ImageIndex:=FImgIndexUnchecked; // default: keep
      ItemNode.SelectedIndex:=ItemNode.ImageIndex;
      ItemNode.Data:=FileGroup;
      begin
        // file paths of compiled and src
        if CompiledFile<>nil then
          AddChild(ItemNode,'Compiled: '+CompiledFile.FullFilename);
        if SrcFile<>nil then
          AddChild(ItemNode,'Source: '+SrcFile.FullFilename)
        else
          AddChild(ItemNode,'No source found');
      end;
    end;
  end;
  // expand all nodes
  for c in TPGIPCategory do
    if FCategoryNodes[c]<>nil then
      FCategoryNodes[c].Expand(true);

  ConflictsTreeView.Items.EndUpdate;

  UpdateButtons;
end;

initialization
  OnShowGUI:=@ShowGUIHandler;

end.

