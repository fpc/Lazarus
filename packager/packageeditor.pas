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
  LResources, LazarusIDEStrConsts, PackageDefs;
  
type
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
    procedure PackageEditorFormResize(Sender: TObject);
    procedure RegisteredPluginsGroupBoxResize(Sender: TObject);
  private
    FPackage: TLazPackage;
    procedure SetPackage(const AValue: TLazPackage);
    procedure SetupComponents;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  public
    property Package: TLazPackage read FPackage write SetPackage;
  end;


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
  w:=45;
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
  h:=Max(10,ClientHeight-130);
  FilesTreeView.SetBounds(x,y,w,h);
  
  inc(y,h+3);
  h:=80;
  FilePropsGroupBox.SetBounds(x,y,w,h);
end;

procedure TPackageEditorForm.RegisteredPluginsGroupBoxResize(Sender: TObject);
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

procedure TPackageEditorForm.SetPackage(const AValue: TLazPackage);
begin
  if FPackage=AValue then exit;
  FPackage:=AValue;
end;

procedure TPackageEditorForm.SetupComponents;
begin
  CompileBitBtn:=TBitBtn.Create(Self);
  with CompileBitBtn do begin
    Parent:=Self;
    Caption:='Compile';
  end;
  
  AddBitBtn:=TBitBtn.Create(Self);
  with AddBitBtn do begin
    Parent:=Self;
    Caption:='Add';
  end;

  RemoveBitBtn:=TBitBtn.Create(Self);
  with RemoveBitBtn do begin
    Parent:=Self;
    Caption:='Remove';
  end;

  InstallBitBtn:=TBitBtn.Create(Self);
  with InstallBitBtn do begin
    Parent:=Self;
    Caption:='Install';
  end;

  OptionsBitBtn:=TBitBtn.Create(Self);
  with OptionsBitBtn do begin
    Parent:=Self;
    Caption:='Options';
  end;

  FilesTreeView:=TTreeView.Create(Self);
  with FilesTreeView do begin
    Parent:=Self;
  end;

  FilePropsGroupBox:=TGroupBox.Create(Self);
  with FilePropsGroupBox do begin
    Parent:=Self;
    Caption:='File Properties';
  end;

  CallRegisterProcCheckBox:=TCheckBox.Create(Self);
  with CallRegisterProcCheckBox do begin
    Parent:=FilePropsGroupBox;
    Caption:='Call Register procedure of unit';
  end;

  RegisteredPluginsGroupBox:=TGroupBox.Create(Self);
  with RegisteredPluginsGroupBox do begin
    Parent:=FilePropsGroupBox;
    Caption:='Registered plugins';
    OnResize:=@RegisteredPluginsGroupBoxResize;
  end;

  RegisteredListView:=TListView.Create(Self);
  with RegisteredListView do begin
    Parent:=RegisteredPluginsGroupBox;
    Align:=alClient;
  end;

  StatusBar:=TStatusBar.Create(Self);
  with StatusBar do begin
    Parent:=Self;
  end;
end;

constructor TPackageEditorForm.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  OnResize:=@PackageEditorFormResize;
  SetupComponents;
  Resize;
end;

destructor TPackageEditorForm.Destroy;
begin
  inherited Destroy;
end;

end.

