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
  // FCL, LCL
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Buttons, AVGLvlTree,
  // IDE
  PropEdits, LazarusIDEStrConsts, ComponentReg, FormEditingIntf, CheckLFMDlg,
  CodeToolManager;

type
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
    procedure NewClassComboBoxEditingDone(Sender: TObject);
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
    function CompareClasses(Tree: TAvgLvlTree; Class1, Class2: TClass): integer;
  public
    destructor Destroy; override;
    procedure FillNewClassComboBox;
    property ThePersistent: TPersistent read FThePersistent write SetThePersistent;
    property NewClass: TClass read FNewClass write SetNewClass;
  end;


function ShowChangeClassDialog(ADesigner: TIDesigner;
  APersistent: TPersistent): TModalResult;
function ChangePersistentClass(ADesigner: TIDesigner; APersistent: TPersistent;
  NewClass: TClass): TModalResult;

implementation


function ShowChangeClassDialog(ADesigner: TIDesigner;
  APersistent: TPersistent): TModalResult;
var
  ChangeClassDlg: TChangeClassDlg;
begin
  Result:=mrCancel;
  //MessageDlg('Not implemented yet','Not implemented yet',mtInformation,[mbOk],0);
  //exit;
  
  ChangeClassDlg:=TChangeClassDlg.Create(Application);
  try
    ChangeClassDlg.ThePersistent:=APersistent;
    ChangeClassDlg.FillNewClassComboBox;
    if ChangeClassDlg.ShowModal=mrOk then begin
      Result:=ChangePersistentClass(ADesigner,APersistent,
                                    ChangeClassDlg.NewClass);
    end;
  finally
    ChangeClassDlg.Free;
  end;
end;

function ChangePersistentClass(ADesigner: TIDesigner; APersistent: TPersistent;
  NewClass: TClass): TModalResult;
var
  ComponentStream: TMemoryStream;
  PersistentName: String;

  procedure ShowAbortMessage(const Msg: string);
  begin
    MessageDlg('Error',
      Msg+#13
      +'Unable to change class of '+PersistentName+' to '+NewClass.ClassName,
      mtError,[mbCancel],0);
  end;

begin
  Result:=mrCancel;
  PersistentName:=APersistent.ClassName;
  if APersistent is TComponent then begin
    PersistentName:=TComponent(APersistent).Name+':'+PersistentName;
  end;
  ComponentStream:=nil;
  try
    // select only this persistent
    GlobalDesignHook.SelectOnlyThis(APersistent);
    // stream selection
    ComponentStream:=TMemoryStream.Create;
    if not FormEditingHook.SaveSelectionToStream(ComponentStream) then begin
      ShowAbortMessage('Unable to stream selected components');
      exit;
    end;
    // parse
    if not CodeToolBoss.GatherExternalChanges then begin
      ShowAbortMessage('Unable to gather editor changes');
      exit;
    end;
    
    // change classname

    // check properties

    // delete selection

    // insert streamed selection

  finally
    ComponentStream.Free;
  end;
  Result:=mrCancel;
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

procedure TChangeClassDlg.NewClassComboBoxEditingDone(Sender: TObject);
begin
  UpdateNewInfo;
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
  UpdateOldInfo;
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
var
  ANode: TAvgLvlTreeNode;
begin
  FNewClass:=nil;
  if FClasses<>nil then begin
    ANode:=FClasses.FindLowest;
    while (ANode<>nil) do begin
      FNewClass:=TClass(ANode.Data);
      if (CompareText(NewClass.ClassName,NewClassComboBox.Text)=0) then
        break
      else
        FNewClass:=nil;
      ANode:=FClasses.FindSuccessor(ANode);
    end;
  end;
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
  if FClasses.FindPointer(AClass)<>nil then exit;
  FClasses.Add(AClass);
end;

procedure TChangeClassDlg.AddComponentClass(const AClass: TComponentClass);
begin
  AddClass(AClass);
end;

function TChangeClassDlg.CompareClasses(Tree: TAvgLvlTree; Class1,
  Class2: TClass): integer;
// sort:
//   transforming ThePersistent to descending classes is easy
//   transforming ThePersistent to ascending classes is medium
//
//   count distance between, that means: find nearest shared ancestor, then
//   give two points for every step from ThePersistent to ancestor and one point
//   for every step from ancestor to class
//
//   otherwise sort for classnames

  function AncestorDistance(ChildClass, AncestorClass: TClass): integer;
  begin
    Result:=0;
    while (ChildClass<>nil) and (ChildClass<>AncestorClass) do begin
      ChildClass:=ChildClass.ClassParent;
      inc(Result);
    end;
  end;

  function RelationDistance(SrcClass, DestClass: TClass): integer;
  var
    Ancestor: TClass;
  begin
    // find shared ancestor of
    Ancestor:=SrcClass;
    while (Ancestor<>nil) and (not DestClass.InheritsFrom(Ancestor)) do
      Ancestor:=Ancestor.ClassParent;
    // going to the ancestor is normally more difficult than going away
    Result:=2*AncestorDistance(SrcClass,Ancestor)
             +AncestorDistance(DestClass,Ancestor);
  end;

var
  Dist1: LongInt;
  Dist2: LongInt;
begin
  Result:=0;
  if (ThePersistent<>nil) then begin
    Dist1:=RelationDistance(ThePersistent.ClassType,Class1);
    Dist2:=RelationDistance(ThePersistent.ClassType,Class2);
    Result:=Dist1-Dist2;
    if Result<>0 then exit;
  end;
  Result:=CompareText(Class1.ClassName,Class2.ClassName);
end;

destructor TChangeClassDlg.Destroy;
begin
  FClasses.Free;
  FClasses:=nil;
  inherited Destroy;
end;

procedure TChangeClassDlg.FillNewClassComboBox;
var
  ANode: TAvgLvlTreeNode;
  List: TStringList;
begin
  // create/clear tree
  if FClasses=nil then
    FClasses:=TAvgLvlTree.CreateObjectCompare(@CompareClasses)
  else
    FClasses.Clear;
  // add class of ThePersistent
  if ThePersistent<>nil then
    AddClass(TPersistentClass(ThePersistent.ClassType));
  // add all registered component classes
  if (IDEComponentPalette<>nil) then
    IDEComponentPalette.IterateRegisteredClasses(@AddComponentClass);
  // add list of classnames
  List:=TStringList.Create;
  ANode:=FClasses.FindLowest;
  while ANode<>nil do begin
    List.Add(TClass(ANode.Data).ClassName);
    ANode:=FClasses.FindSuccessor(ANode);
  end;
  // assign to combobox
  NewClassComboBox.Items.Assign(List);
  if (NewClassComboBox.Items.IndexOf(NewClassComboBox.Text)<0)
  and (NewClassComboBox.Items.Count>0) then
    NewClassComboBox.Text:=NewClassComboBox.Items[0];
  UpdateNewInfo;
  // clean up
  List.Free;
end;

initialization
  {$I changeclassdialog.lrs}

end.

