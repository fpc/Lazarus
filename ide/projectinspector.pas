{  $Id$  }
{
 /***************************************************************************
                          projectinspector.pas
                          --------------------


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
    TProjectInspectorForm is the form of the project inspector.
}
unit ProjectInspector;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LCLProc, LResources, Forms, Controls, Buttons, ComCtrls,
  StdCtrls, ExtCtrls, Menus, Dialogs, Graphics, FileCtrl,
  LazarusIDEStrConsts, IDEProcs, IDEOptionDefs, EnvironmentOpts,
  Project;
  
type
  TProjectInspectorForm = class(TForm)
    OpenBitBtn: TBitBtn;
    AddBitBtn: TBitBtn;
    RemoveBitBtn: TBitBtn;
    OptionsBitBtn: TBitBtn;
    ItemsTreeView: TTreeView;
    ImageList: TImageList;
    procedure AddBitBtnClick(Sender: TObject);
    procedure ItemsTreeViewDblClick(Sender: TObject);
    procedure ItemsTreeViewSelectionChanged(Sender: TObject);
    procedure OpenBitBtnClick(Sender: TObject);
    procedure OptionsBitBtnClick(Sender: TObject);
    procedure ProjectInspectorFormResize(Sender: TObject);
    procedure ProjectInspectorFormShow(Sender: TObject);
    procedure RemoveBitBtnClick(Sender: TObject);
  private
    FUpdateLock: integer;
    FLazProject: TProject;
    FilesNode: TTreeNode;
    DependenciesNode: TTreeNode;
    ImageIndexFiles: integer;
    ImageIndexRequired: integer;
    ImageIndexRemovedRequired: integer;
    ImageIndexUnit: integer;
    ImageIndexRegisterUnit: integer;
    ImageIndexText: integer;
    ImageIndexBinary: integer;
    procedure SetLazProject(const AValue: TProject);
    procedure SetupComponents;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure BeginUpdate;
    procedure EndUpdate;
    function IsUpdating: boolean;
    procedure UpdateAll;
    procedure UpdateTitle;
    procedure UpdateButtons;
    procedure UpdateItems;
  public
    property LazProject: TProject read FLazProject write SetLazProject;
  end;
  
var
  ProjInspector: TProjectInspectorForm;


implementation

{ TProjectInspectorForm }

procedure TProjectInspectorForm.ProjectInspectorFormResize(Sender: TObject);
var
  x: Integer;
  y: Integer;
  w: Integer;
  h: Integer;
begin
  x:=0;
  y:=0;
  w:=(ClientWidth div 4);
  h:=25;
  with OptionsBitBtn do
    SetBounds(x,y,w,h);
  inc(x,w);
  
  with OpenBitBtn do
    SetBounds(x,y,w,h);
  inc(x,w);

  with AddBitBtn do
    SetBounds(x,y,w,h);
  inc(x,w);

  w:=ClientWidth-x;
  with RemoveBitBtn do
    SetBounds(x,y,w,h);
  inc(x,w);

  x:=0;
  inc(y,h);
  with ItemsTreeView do
    SetBounds(x,y,Parent.ClientWidth,Parent.ClientHeight-y);
end;

procedure TProjectInspectorForm.ItemsTreeViewDblClick(Sender: TObject);
begin

end;

procedure TProjectInspectorForm.ItemsTreeViewSelectionChanged(Sender: TObject);
begin

end;

procedure TProjectInspectorForm.AddBitBtnClick(Sender: TObject);
begin

end;

procedure TProjectInspectorForm.OpenBitBtnClick(Sender: TObject);
begin

end;

procedure TProjectInspectorForm.OptionsBitBtnClick(Sender: TObject);
begin

end;

procedure TProjectInspectorForm.ProjectInspectorFormShow(Sender: TObject);
begin
  UpdateAll;
end;

procedure TProjectInspectorForm.RemoveBitBtnClick(Sender: TObject);
begin

end;

procedure TProjectInspectorForm.SetLazProject(const AValue: TProject);
begin
  if FLazProject=AValue then exit;
  FLazProject:=AValue;
  UpdateAll;
end;

procedure TProjectInspectorForm.SetupComponents;

  procedure AddResImg(const ResName: string);
  var Pixmap: TPixmap;
  begin
    Pixmap:=TPixmap.Create;
    Pixmap.TransparentColor:=clWhite;
    Pixmap.LoadFromLazarusResource(ResName);
    ImageList.Add(Pixmap,nil)
  end;

begin
  ImageList:=TImageList.Create(Self);
  with ImageList do begin
    Width:=17;
    Height:=17;
    Name:='ImageList';
    ImageIndexFiles:=Count;
    AddResImg('pkg_files');
    ImageIndexRequired:=Count;
    AddResImg('pkg_required');
    ImageIndexRemovedRequired:=Count;
    AddResImg('pkg_removedrequired');
    ImageIndexUnit:=Count;
    AddResImg('pkg_unit');
    ImageIndexRegisterUnit:=Count;
    AddResImg('pkg_registerunit');
    ImageIndexText:=Count;
    AddResImg('pkg_text');
    ImageIndexBinary:=Count;
    AddResImg('pkg_binary');
  end;

  OpenBitBtn:=TBitBtn.Create(Self);
  with OpenBitBtn do begin
    Name:='OpenBitBtn';
    Parent:=Self;
    Caption:='Open';
    OnClick:=@OpenBitBtnClick;
  end;

  AddBitBtn:=TBitBtn.Create(Self);
  with AddBitBtn do begin
    Name:='AddBitBtn';
    Parent:=Self;
    Caption:='Add';
    OnClick:=@AddBitBtnClick;
  end;

  OptionsBitBtn:=TBitBtn.Create(Self);
  with OptionsBitBtn do begin
    Name:='OptionsBitBtn';
    Parent:=Self;
    Caption:='Options';
    OnClick:=@OptionsBitBtnClick;
  end;

  RemoveBitBtn:=TBitBtn.Create(Self);
  with RemoveBitBtn do begin
    Name:='RemoveBitBtn';
    Parent:=Self;
    Caption:='Remove';
    OnClick:=@RemoveBitBtnClick;
  end;

  ItemsTreeView:=TTreeView.Create(Self);
  with ItemsTreeView do begin
    Name:='ItemsTreeView';
    Parent:=Self;
    Images:=ImageList;
    Options:=Options+[tvoRightClickSelect];
    OnSelectionChanged:=@ItemsTreeViewSelectionChanged;
    OnDblClick:=@ItemsTreeViewDblClick;
    FilesNode:=Items.Add(nil,'Files');
    FilesNode.ImageIndex:=ImageIndexFiles;
    FilesNode.SelectedIndex:=FilesNode.ImageIndex;
    DependenciesNode:=Items.Add(nil,'Required Packages');
    DependenciesNode.ImageIndex:=ImageIndexRequired;
    DependenciesNode.SelectedIndex:=DependenciesNode.ImageIndex;
  end;
end;

constructor TProjectInspectorForm.Create(TheOwner: TComponent);
var
  ALayout: TIDEWindowLayout;
begin
  inherited Create(TheOwner);
  Name:=NonModalIDEWindowNames[nmiwProjectInspector];
  Caption:='Project Inspector';

  ALayout:=EnvironmentOptions.IDEWindowLayoutList.ItemByFormID(Name);
  ALayout.Form:=TForm(Self);
  ALayout.Apply;

  SetupComponents;
  OnResize:=@ProjectInspectorFormResize;
  OnResize(Self);
  OnShow:=@ProjectInspectorFormShow;
end;

destructor TProjectInspectorForm.Destroy;
begin
  inherited Destroy;
end;

procedure TProjectInspectorForm.BeginUpdate;
begin
  inc(FUpdateLock);
end;

procedure TProjectInspectorForm.EndUpdate;
begin
  if FUpdateLock=0 then RaiseException('TProjectInspectorForm.EndUpdate');
  dec(FUpdateLock);
  if FUpdateLock=0 then begin

  end;
end;

function TProjectInspectorForm.IsUpdating: boolean;
begin
  Result:=FUpdateLock>0;
end;

procedure TProjectInspectorForm.UpdateAll;
begin
  UpdateTitle;
  UpdateButtons;
  UpdateItems;
end;

procedure TProjectInspectorForm.UpdateTitle;
var
  NewCaption: String;
begin
  if LazProject=nil then
    Caption:='Project Inspector'
  else begin
    NewCaption:=LazProject.Title;
    if NewCaption='' then
      NewCaption:=ExtractFilenameOnly(LazProject.ProjectInfoFile);
    Caption:=NewCaption;
  end;
end;

procedure TProjectInspectorForm.UpdateButtons;
begin
  AddBitBtn.Enabled:=true;
  RemoveBitBtn.Enabled:=true;
  OpenBitBtn.Enabled:=true;
  OptionsBitBtn.Enabled:=true;
end;

procedure TProjectInspectorForm.UpdateItems;
begin
  // update project files
  
  // update required packages
  
  // update removed required packages
  
end;


initialization
  ProjInspector:=nil;

end.

