{  $Id$  }
{
 /***************************************************************************
                            packageeditor.pas
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
    TPackageEditorForm is the form of a package editor.
}
unit PackageEditor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, StdCtrls, ExtCtrls, ComCtrls, Buttons,
  LResources, LazarusIDEStrConsts, IDEOptionDefs, PackageDefs, AddToPackageDlg;
  
type
  { TPackageEditorForm }

  TPackageEditorForm = class(TBasePackageEditor)
    CompileBitBtn: TBitBtn;
    AddBitBtn: TBitBtn;
    RemoveBitBtn: TBitBtn;
    InstallBitBtn: TBitBtn;
    OptionsBitBtn: TBitBtn;
    FilesTreeView: TTreeView;
    FilePropsGroupBox: TGroupBox;
    CallRegisterProcCheckBox: TCheckBox;
    RegisteredPluginsGroupBox: TGroupBox;
    RegisteredListView: TListView;
    StatusBar: TStatusBar;
    procedure FilePropsGroupBoxResize(Sender: TObject);
    procedure PackageEditorFormResize(Sender: TObject);
  private
    FLazPackage: TLazPackage;
    FilesNode: TTreeNode;
    RequiredPackagesNode: TTreeNode;
    ConflictPackagesNode: TTreeNode;
    procedure SetLazPackage(const AValue: TLazPackage);
    procedure SetupComponents;
    procedure UpdateAll;
    procedure UpdateTitle;
    procedure UpdateButtons;
    procedure UpdateFiles;
    procedure UpdateRequiredPkgs;
    procedure UpdateConflictPkgs;
    procedure UpdateSelectedFile;
    procedure UpdateStatusBar;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  public
    property LazPackage: TLazPackage read FLazPackage write SetLazPackage;
  end;
  
  
  { TPackageEditors }
  
  TPackageEditors = class
  private
    FItems: TList; // list of TPackageEditorForm
    function GetEditors(Index: integer): TPackageEditorForm;
  public
    constructor Create;
    destructor Destroy; override;
    function Count: integer;
    procedure Clear;
    procedure Remove(Editor: TPackageEditorForm);
    function IndexOfPackage(Pkg: TLazPackage): integer;
    function FindEditor(Pkg: TLazPackage): TPackageEditorForm;
    function OpenEditor(Pkg: TLazPackage): TPackageEditorForm;
  public
    property Editors[Index: integer]: TPackageEditorForm read GetEditors;
  end;
  
var
  PackageEditors: TPackageEditors;


implementation


uses Math;

{ TPackageEditorForm }

procedure TPackageEditorForm.PackageEditorFormResize(Sender: TObject);
var
  x: Integer;
  y: Integer;
  w: Integer;
  h: Integer;
begin
  x:=0;
  y:=0;
  w:=70;
  h:=45;
  CompileBitBtn.SetBounds(x,y,w,h);
  inc(x,w+2);

  AddBitBtn.SetBounds(x,y,w,h);
  inc(x,w+2);

  RemoveBitBtn.SetBounds(x,y,w,h);
  inc(x,w+2);

  InstallBitBtn.SetBounds(x,y,w,h);
  inc(x,w+2);

  OptionsBitBtn.SetBounds(x,y,w,h);

  x:=0;
  inc(y,h+3);
  w:=ClientWidth;
  h:=Max(10,ClientHeight-y-123-StatusBar.Height);
  FilesTreeView.SetBounds(x,y,w,h);
  
  inc(y,h+3);
  h:=120;
  FilePropsGroupBox.SetBounds(x,y,w,h);
end;

procedure TPackageEditorForm.FilePropsGroupBoxResize(Sender: TObject);
var
  y: Integer;
begin
  with CallRegisterProcCheckBox do
    SetBounds(3,0,Parent.ClientWidth,Height);

  y:=CallRegisterProcCheckBox.Top+CallRegisterProcCheckBox.Height+3;
  with RegisteredPluginsGroupBox do begin
    SetBounds(0,y,Parent.ClientWidth,Parent.ClientHeight-y);
  end;
end;

procedure TPackageEditorForm.SetLazPackage(const AValue: TLazPackage);
var
  ARect: TRect;
begin
  if FLazPackage=AValue then exit;
  FLazPackage:=AValue;
  FLazPackage.Editor:=Self;
  // find a nice position for the editor
  ARect:=FLazPackage.EditorRect;
  if (ARect.Bottom<ARect.Top+50) or (ARect.Right<ARect.Left+50) then
    ARect:=CreateNiceWindowPosition(400,400);
  SetBounds(ARect.Left,ARect.Top,
            ARect.Right-ARect.Left,ARect.Bottom-ARect.Top);
  // update components
  UpdateAll;
end;

procedure TPackageEditorForm.SetupComponents;
begin
  CompileBitBtn:=TBitBtn.Create(Self);
  with CompileBitBtn do begin
    Name:='CompileBitBtn';
    Parent:=Self;
    Caption:='Compile';
  end;
  
  AddBitBtn:=TBitBtn.Create(Self);
  with AddBitBtn do begin
    Name:='AddBitBtn';
    Parent:=Self;
    Caption:='Add';
  end;

  RemoveBitBtn:=TBitBtn.Create(Self);
  with RemoveBitBtn do begin
    Name:='RemoveBitBtn';
    Parent:=Self;
    Caption:='Remove';
  end;

  InstallBitBtn:=TBitBtn.Create(Self);
  with InstallBitBtn do begin
    Name:='InstallBitBtn';
    Parent:=Self;
    Caption:='Install';
  end;

  OptionsBitBtn:=TBitBtn.Create(Self);
  with OptionsBitBtn do begin
    Name:='OptionsBitBtn';
    Parent:=Self;
    Caption:='Options';
  end;

  FilesTreeView:=TTreeView.Create(Self);
  with FilesTreeView do begin
    Name:='FilesTreeView';
    Parent:=Self;
    FilesNode:=Items.Add(nil,'Files');
    RequiredPackagesNode:=Items.Add(nil,'Required packages');
    ConflictPackagesNode:=Items.Add(nil,'Conflict packages');
  end;

  FilePropsGroupBox:=TGroupBox.Create(Self);
  with FilePropsGroupBox do begin
    Name:='FilePropsGroupBox';
    Parent:=Self;
    Caption:='File Properties';
    OnResize:=@FilePropsGroupBoxResize;
  end;

  CallRegisterProcCheckBox:=TCheckBox.Create(Self);
  with CallRegisterProcCheckBox do begin
    Name:='CallRegisterProcCheckBox';
    Parent:=FilePropsGroupBox;
    Caption:='Call Register procedure of unit';
  end;

  RegisteredPluginsGroupBox:=TGroupBox.Create(Self);
  with RegisteredPluginsGroupBox do begin
    Name:='RegisteredPluginsGroupBox';
    Parent:=FilePropsGroupBox;
    Caption:='Registered plugins';
  end;

  RegisteredListView:=TListView.Create(Self);
  with RegisteredListView do begin
    Name:='RegisteredListView';
    Parent:=RegisteredPluginsGroupBox;
    Align:=alClient;
  end;

  StatusBar:=TStatusBar.Create(Self);
  with StatusBar do begin
    Name:='StatusBar';
    Parent:=Self;
    Align:=alBottom;
  end;
end;

procedure TPackageEditorForm.UpdateAll;
begin
  UpdateTitle;
  UpdateButtons;
  UpdateFiles;
  UpdateRequiredPkgs;
  UpdateConflictPkgs;
  UpdateSelectedFile;
  UpdateStatusBar;
end;

procedure TPackageEditorForm.UpdateTitle;
begin
  Caption:='Package '+FLazPackage.Name;
end;

procedure TPackageEditorForm.UpdateButtons;
begin
  CompileBitBtn.Enabled:=(not LazPackage.IsVirtual);
  AddBitBtn.Enabled:=true;
  RemoveBitBtn.Enabled:=(FilesTreeView.Selected<>nil)
                        and (FilesTreeView.Selected.Parent<>nil);
  InstallBitBtn.Enabled:=(not LazPackage.IsVirtual);
  OptionsBitBtn.Enabled:=true;
end;

procedure TPackageEditorForm.UpdateFiles;
begin

end;

procedure TPackageEditorForm.UpdateRequiredPkgs;
begin

end;

procedure TPackageEditorForm.UpdateConflictPkgs;
begin

end;

procedure TPackageEditorForm.UpdateSelectedFile;
begin

end;

procedure TPackageEditorForm.UpdateStatusBar;
begin
  if LazPackage.IsVirtual then begin
    StatusBar.SimpleText:='package '+LazPackage.Name+' not saved';
  end else begin
    StatusBar.SimpleText:=LazPackage.Filename;
  end;
end;

constructor TPackageEditorForm.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  SetupComponents;
  OnResize:=@PackageEditorFormResize;
end;

destructor TPackageEditorForm.Destroy;
begin
  inherited Destroy;
end;

{ TPackageEditors }

function TPackageEditors.GetEditors(Index: integer): TPackageEditorForm;
begin
  Result:=TPackageEditorForm(FItems[Index]);
end;

constructor TPackageEditors.Create;
begin
  FItems:=TList.Create;
end;

destructor TPackageEditors.Destroy;
begin
  Clear;
  FItems.Free;
  inherited Destroy;
end;

function TPackageEditors.Count: integer;
begin
  Result:=FItems.Count;
end;

procedure TPackageEditors.Clear;
begin
  FItems.Clear;
end;

procedure TPackageEditors.Remove(Editor: TPackageEditorForm);
begin
  FItems.Remove(Editor);
end;

function TPackageEditors.IndexOfPackage(Pkg: TLazPackage): integer;
begin
  Result:=Count-1;
  while (Result>=0) and (Editors[Result].LazPackage<>Pkg) do dec(Result);
end;

function TPackageEditors.FindEditor(Pkg: TLazPackage): TPackageEditorForm;
var
  i: Integer;
begin
  i:=IndexOfPackage(Pkg);
  if i>=0 then
    Result:=Editors[i]
  else
    Result:=nil;
end;

function TPackageEditors.OpenEditor(Pkg: TLazPackage): TPackageEditorForm;
begin
  Result:=FindEditor(Pkg);
  if Result=nil then begin
    Result:=TPackageEditorForm.Create(Application);
    Result.LazPackage:=Pkg;
    FItems.Add(Result);
    Pkg.Open:=true;
  end;
end;

initialization
  PackageEditors:=nil;

end.

