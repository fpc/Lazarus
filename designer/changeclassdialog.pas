{ /***************************************************************************
                 ChangeClassDialog.pas - Lazarus IDE unit
                 ----------------------------------------

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
    Functions and Dialog to change the class of a designer component.
}
unit ChangeClassDialog;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Buttons, AVGLvlTree, LazarusIDEStrConsts, ComponentReg;

type
  TPersistentClass = class of TPersistent;

  TChangeClassDlg = class(TForm)
    NewClassComboBox: TComboBox;
    NewAncestorGroupBox: TGroupBox;
    NewAncestorsListBox: TListBox;
    OldAncestorGroupBox: TGroupBox;
    OldAncestorsListBox: TListBox;
    OldClassLabel: TLabel;
    OkButton: TButton;
    CancelButton: TButton;
    NewGroupBox: TGroupBox;
    OldGroupBox: TGroupBox;
    procedure ChangeClassDlgCreate(Sender: TObject);
  private
    FClasses: TAvgLvlTree;
    FNewClass: TClass;
    FThePersistent: TPersistent;
    procedure SetNewClass(const AValue: TClass);
    procedure SetThePersistent(const AValue: TPersistent);
    procedure UpdateInfo;
    procedure UpdateOldInfo;
    procedure UpdateNewInfo;
    procedure FillAncestorListBox(AClass: TClass; AListBox: TListBox);
    procedure AddClass(const AClass: TPersistentClass);
    procedure AddComponentClass(const AClass: TComponentClass);
  public
    destructor Destroy; override;
    procedure FillNewClassComboBox;
    property ThePersistent: TPersistent read FThePersistent write SetThePersistent;
    property NewClass: TClass read FNewClass write SetNewClass;
  end;


function ShowChangeClassDialog(APersistent: TPersistent): TModalResult;

implementation

function ShowChangeClassDialog(APersistent: TPersistent): TModalResult;
var
  ChangeClassDlg: TChangeClassDlg;
begin
  Result:=mrCancel;
  MessageDlg('Not implemented yet','Not implemented yet',mtInformation,[mbOk],0);
  exit;
  
  ChangeClassDlg:=TChangeClassDlg.Create(Application);
  try
    ChangeClassDlg.ThePersistent:=APersistent;
    ChangeClassDlg.FillNewClassComboBox;
  finally
    ChangeClassDlg.Free;
  end;
end;

function CompareClasses(Class1, Class2: TClass): integer;
begin
  // TODO
  Result:=0;
end;

{ TChangeClassDlg }

procedure TChangeClassDlg.ChangeClassDlgCreate(Sender: TObject);
begin
  OldGroupBox.Caption:='Old Class';
  NewGroupBox.Caption:='New Class';
  OldAncestorGroupBox.Caption:='Old Ancestors';
  NewAncestorGroupBox.Caption:='New Ancestors';
  OkButton.Caption:='Ok';
  CancelButton.Caption:='Cancel';
end;

procedure TChangeClassDlg.SetThePersistent(const AValue: TPersistent);
begin
  if FThePersistent=AValue then exit;
  FThePersistent:=AValue;
  UpdateInfo;
end;

procedure TChangeClassDlg.SetNewClass(const AValue: TClass);
begin
  if FNewClass=AValue then exit;
  FNewClass:=AValue;
  UpdateNewInfo;
end;

procedure TChangeClassDlg.UpdateInfo;
begin
  UpdateNewInfo;
end;

procedure TChangeClassDlg.UpdateOldInfo;
begin
  FillAncestorListBox(ThePersistent.ClassType,OldAncestorsListBox);
  if ThePersistent<>nil then begin
    if ThePersistent is TComponent then
      OldClassLabel.Caption:=
        TComponent(ThePersistent).Name+':'+ThePersistent.ClassName
    else
      OldClassLabel.Caption:=ThePersistent.ClassName;
  end else begin
    OldClassLabel.Caption:='no class';
  end;
end;

procedure TChangeClassDlg.UpdateNewInfo;
begin
  FillAncestorListBox(NewClass,NewAncestorsListBox);
  if NewClass<>nil then
    NewClassComboBox.Text:=NewClass.ClassName
  else
    NewClassComboBox.Text:='';
end;

procedure TChangeClassDlg.FillAncestorListBox(AClass: TClass; AListBox: TListBox
  );
var
  List: TStringList;
  
  procedure AddAncestor(CurClass: TClass);
  begin
    if CurClass=nil then exit;
    List.Insert(0,CurClass.ClassName);
    AddAncestor(CurClass.ClassParent);
  end;
  
begin
  List:=TStringList.Create;
  AddAncestor(AClass);
  AListBox.Items.Assign(List);
  List.Free;
end;

procedure TChangeClassDlg.AddClass(const AClass: TPersistentClass);
begin

end;

procedure TChangeClassDlg.AddComponentClass(const AClass: TComponentClass);
begin
  AddClass(AClass);
end;

destructor TChangeClassDlg.Destroy;
begin
  FClasses.Free;
  FClasses:=nil;
  inherited Destroy;
end;

procedure TChangeClassDlg.FillNewClassComboBox;
begin
  FClasses:=TAvgLvlTree.Create(@CompareClasses);
  if ThePersistent<>nil then
    AddClass(TPersistentClass(ThePersistent.ClassType));
  if (IDEComponentPalette<>nil) then
    IDEComponentPalette.IterateRegisteredClasses(@AddComponentClass);
end;

initialization
  {$I changeclassdialog.lrs}

end.

