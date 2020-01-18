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
 *   Inc., 51 Franklin Street - Fifth Floor, Boston, MA 02110-1335, USA.   *
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
  Classes, SysUtils, Laz_AVL_Tree,
  // LCL
  LCLProc, LCLType, LResources, Forms, Controls, Dialogs, StdCtrls, ButtonPanel,
  // Codetools
  LFMTrees, CodeCache, CodeToolManager,
  // IdeIntf
  ComponentReg, PropEdits, ComponentEditors, FormEditingIntf, SrcEditorIntf, IDEDialogs,
  // IDE
  LazarusIDEStrConsts, CheckLFMDlg, Project, MainIntf, EnvironmentOpts;

type

  { TChangeClassDlg }

  TChangeClassDlg = class(TForm)
    BtnPanel: TButtonPanel;
    NewClassComboBox: TComboBox;
    NewAncestorsListBox: TListBox;
    OldAncestorsListBox: TListBox;
    OldClassLabel: TLabel;
    NewGroupBox: TGroupBox;
    OldGroupBox: TGroupBox;
    procedure ChangeClassDlgCreate(Sender: TObject);
    procedure NewClassComboBoxEditingDone(Sender: TObject);
    procedure NewClassComboBoxKeyUp(Sender: TObject; var Key: Word;
      {%H-}Shift: TShiftState);
  private
    FClasses: TAvlTree;
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
    function CompareClasses({%H-}Tree: TAvlTree; Class1, Class2: TClass): integer;
  public
    destructor Destroy; override;
    procedure FillNewClassComboBox;
    property ThePersistent: TPersistent read FThePersistent write SetThePersistent;
    property NewClass: TClass read FNewClass write SetNewClass;
  end;

function ShowChangeClassDialog(ADesigner: TIDesigner;
  APersistent: TPersistent): TModalResult;
function ChangePersistentClass(ADesigner: TIDesigner;
  APersistent: TPersistent; NewClass: TClass): TModalResult;

implementation

{$R *.lfm}

function ShowChangeClassDialog(ADesigner: TIDesigner;
  APersistent: TPersistent): TModalResult;
var
  ChangeClassDlg: TChangeClassDlg;
begin
  Result:=mrCancel;
  ChangeClassDlg:=TChangeClassDlg.Create(nil);
  try
    ChangeClassDlg.ThePersistent:=APersistent;
    ChangeClassDlg.FillNewClassComboBox;
    if ChangeClassDlg.ShowModal=mrOk then begin
      Result:=ChangePersistentClass(ADesigner,APersistent,ChangeClassDlg.NewClass);
    end;
  finally
    ChangeClassDlg.Free;
  end;
end;

function ChangePersistentClass(ADesigner: TIDesigner;
  APersistent: TPersistent; NewClass: TClass): TModalResult;
var
  ComponentStream: TMemoryStream;
  PersistentName: String;
  UnitCode: TCodeBuffer;
  LFMBuffer: TCodeBuffer;
  LFMTree: TLFMTree;
  UnitInfo: TUnitInfo;
  OldParents: TStrings; // Name=OldParent pairs

  procedure ShowAbortMessage(const Msg: string);
  begin
    IDEMessageDialog('Error',
      Format(lisUnableToChangeClassOfTo, [Msg, LineEnding, PersistentName,
        NewClass.ClassName]),
      mtError,[mbCancel]);
  end;

  function StreamSelection: boolean;
  begin
    Result:=false;
    // select only this persistent
    GlobalDesignHook.SelectOnlyThis(APersistent);
    if (APersistent is TControl)
    and (TControl(APersistent).Parent<>nil) then begin
      if OldParents=nil then
        OldParents:=TStringList.Create;
      OldParents.Values[TControl(APersistent).Name]:=TControl(APersistent).Parent.Name;
    end;

    // stream selection
    ComponentStream:=TMemoryStream.Create;
    if (not FormEditingHook.SaveSelectionToStream(ComponentStream))
    or (ComponentStream.Size=0) then begin
      ShowAbortMessage(lisUnableToStreamSelectedComponents2);
      exit;
    end;
    Result:=true;
  end;

  function ParseLFMStream: boolean;
  var
    SrcEdit: TSourceEditorInterface;
    Msg: String;
  begin
    Result:=false;
    if not CodeToolBoss.GatherExternalChanges then begin
      ShowAbortMessage(lisUnableToGatherEditorChanges);
      exit;
    end;
    MainIDEInterface.GetUnitInfoForDesigner(ADesigner,SrcEdit,UnitInfo);
    if UnitInfo=nil then begin
      ShowAbortMessage(lisUnableToGetSourceForDesigner);
      exit;
    end;
    UnitCode:=UnitInfo.Source;
    LFMBuffer:=CodeToolBoss.CreateTempFile('changeclass.lfm');
    if (LFMBuffer=nil) or (ComponentStream.Size=0) then begin
      ShowAbortMessage(lisUnableToCreateTemporaryLfmBuffer);
      exit;
    end;
    ComponentStream.Position:=0;
    LFMBuffer.LoadFromStream(ComponentStream);
    //debugln('ChangePersistentClass-Before-Checking--------------------------------------------');
    //debugln(LFMBuffer.Source);
    //debugln('ChangePersistentClass-Before-Checking-------------------------------------------');
    if not CodeToolBoss.CheckLFM(UnitCode,LFMBuffer,LFMTree,false,false,false) then
    begin
      debugln('ChangePersistentClass-Before--------------------------------------------');
      debugln(LFMBuffer.Source);
      debugln('ChangePersistentClass-Before--------------------------------------------');
      if CodeToolBoss.ErrorMessage<>'' then
        MainIDEInterface.DoJumpToCodeToolBossError
      else begin
        Msg:=lisErrorParsingLfmComponentStream;
        if LFMTree<>nil then
          Msg:=Msg+LineEnding+LineEnding+LFMTree.FirstErrorAsString+LineEnding;
        ShowAbortMessage(Msg);
      end;
      exit;
    end;
    Result:=true;
  end;

  function ChangeClassName: boolean;
  var
    CurNode: TLFMTreeNode;
    ObjectNode: TLFMObjectNode;
  begin
    Result:=false;
    // find classname position
    CurNode:=LFMTree.Root;
    while CurNode<>nil do begin
      if (CurNode is TLFMObjectNode) then begin
        ObjectNode:=TLFMObjectNode(CurNode);
        if (CompareText(ObjectNode.Name,(APersistent as TComponent).Name)=0)
        and (CompareText(ObjectNode.TypeName,APersistent.ClassName)=0) then begin
          // replace classname
          LFMBuffer.Replace(ObjectNode.TypeNamePosition,length(ObjectNode.TypeName),
            NewClass.ClassName);
          Result:=true;
          exit;
        end;
      end;
      CurNode:=CurNode.NextSibling;
    end;
    ShowAbortMessage(Format(lisUnableToFindInLFMStream, [PersistentName]));
  end;

  function CheckProperties: boolean;
  begin
    Result:=RepairLFMBuffer(UnitCode,LFMBuffer,false,false,false)=mrOk;
    if not Result and (CodeToolBoss.ErrorMessage<>'') then
      MainIDEInterface.DoJumpToCodeToolBossError;
  end;

  function InsertStreamedSelection: boolean;
  var
    MemStream: TMemoryStream;
    LFMType, LFMComponentName, LFMClassName: string;
    AComponent: TComponent;
    NewParent: TWinControl;
    NewParentName: string;
  begin
    Result:=false;
    if LFMBuffer.SourceLength=0 then exit;
    MemStream:=TMemoryStream.Create;
    try
      debugln('ChangePersistentClass-After--------------------------------------------');
      debugln(LFMBuffer.Source);
      debugln('ChangePersistentClass-After--------------------------------------------');
      LFMBuffer.SaveToStream(MemStream);
      MemStream.Position:=0;
      NewParent:=nil;
      if OldParents<>nil then begin
        ReadLFMHeader(MemStream,LFMType,LFMComponentName,LFMClassName);
        if (LFMType='') or (LFMClassName='') then ;
        MemStream.Position:=0;
        if LFMComponentName<>'' then begin
          NewParentName:=OldParents.Values[LFMComponentName];
          if NewParentName<>'' then begin
            AComponent:=GlobalDesignHook.GetComponent(NewParentName);
            if AComponent is TWinControl then
              NewParent:=TWinControl(AComponent);
          end;
        end;
      end;
      Result:=FormEditingHook.InsertFromStream(MemStream,NewParent,
                                               [cpsfReplace]);
      if not Result then
        ShowAbortMessage(lisReplacingSelectionFailed);
    finally
      MemStream.Free;
    end;
  end;

begin
  Result:=mrCancel;
  if NewClass = nil then
    exit;
  if CompareText(APersistent.ClassName,NewClass.ClassName)=0 then begin
    Result:=mrOk;
    exit;
  end;
  PersistentName:=APersistent.ClassName;
  if APersistent is TComponent then begin
    PersistentName:=TComponent(APersistent).Name+': '+PersistentName;
  end else begin
    ShowAbortMessage(lisCanOnlyChangeTheClassOfTComponents);
    exit;
  end;
  ComponentStream:=nil;
  LFMTree:=nil;
  OldParents:=nil;
  try
    if not StreamSelection then exit;
    if not ParseLFMStream then exit;
    if not ChangeClassName then exit;
    if not CheckProperties then exit;
    if not InsertStreamedSelection then exit;
  finally
    ComponentStream.Free;
    OldParents.Free;
    // Note: do not free LFMTree, it is cached by the codetools
  end;
  Result:=mrOk;
end;

{ TChangeClassDlg }

procedure TChangeClassDlg.ChangeClassDlgCreate(Sender: TObject);
begin
  OldGroupBox.Caption:=lisOldClass;
  NewGroupBox.Caption:=lisNewClass;
  NewClassComboBox.DropDownCount:=EnvironmentOptions.DropDownCount;
end;

procedure TChangeClassDlg.NewClassComboBoxEditingDone(Sender: TObject);
begin
  UpdateNewInfo;
end;

procedure TChangeClassDlg.NewClassComboBoxKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_RETURN then
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
      OldClassLabel.Caption:=TComponent(ThePersistent).Name+': '+ThePersistent.ClassName
    else
      OldClassLabel.Caption:=ThePersistent.ClassName;
    Caption:=Format(lisCCDChangeClassOf, [OldClassLabel.Caption]);
  end else begin
    OldClassLabel.Caption:=lisCCDNoClass;
    Caption:=lisChangeClass;
  end;
end;

procedure TChangeClassDlg.UpdateNewInfo;
var
  ANode: TAvlTreeNode;
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
  if NewClass<>nil then begin
    NewClassComboBox.Text:=NewClass.ClassName;
    BtnPanel.OKButton.Enabled:=true;
  end
  else begin
    NewClassComboBox.Text:='';
    BtnPanel.OKButton.Enabled:=false;
  end;
end;

procedure TChangeClassDlg.FillAncestorListBox(AClass: TClass; AListBox: TListBox);
var
  List: TStringList;
  
  procedure AddAncestor(CurClass: TClass);
  begin
    if CurClass=nil then exit;
    List.Add(CurClass.ClassName);
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

function TChangeClassDlg.CompareClasses(Tree: TAvlTree; Class1,Class2: TClass): integer;
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
  ANode: TAvlTreeNode;
  List: TStringList;
begin
  // create/clear tree
  if FClasses=nil then
    FClasses:=TAvlTree.CreateObjectCompare(TObjectSortCompare(@CompareClasses))
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
  try
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
  finally
    List.Free;
  end;
end;

end.

