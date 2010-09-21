{***************************************************************************
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

  Abstract:
    Frame to edit build macros and conditionals of compiler options
    (project+packages).

}
unit Compiler_BuildMacro_Options;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LCLProc, FileUtil, Controls, Forms, StdCtrls, Grids,
  Buttons, ExtCtrls, Dialogs, ComCtrls, Menus, AvgLvlTree, IDEImagesIntf,
  ProjectIntf, PackageIntf, CompilerOptions, IDEOptionsIntf,
  LazarusIDEStrConsts, CompOptsModes, PackageDefs, SynEdit, SynHighlighterPas;

type
  TCBMNodeType = (
    cbmntNone,
    cbmntBuildMacro,
    cbmntValue
    );

  { TCompOptBuildMacrosFrame }

  TCompOptBuildMacrosFrame = class(TAbstractIDEOptionsEditor)
    BuildMacroDescriptionEdit: TEdit;
    BuildMacroSelectedGroupBox: TGroupBox;
    BuildMacrosTreeView: TTreeView;
    BuildMacrosTVPopupMenu: TPopupMenu;
    BuildMacroDefaultLabel: TLabel;
    BuildMacroDescriptionLabel: TLabel;
    ConditionalsGroupBox: TGroupBox;
    ConditionalsSynPasSyn: TSynPasSyn;
    CondSynEdit: TSynEdit;
    MacrosGroupBox: TGroupBox;
    MacrosSplitter: TSplitter;
    Splitter1: TSplitter;
    procedure BuildMacrosTreeViewEdited(Sender: TObject; Node: TTreeNode;
      var S: string);
    procedure BuildMacrosTreeViewEditing(Sender: TObject; Node: TTreeNode;
      var AllowEdit: Boolean);
    procedure BuildMacrosTreeViewSelectionChanged(Sender: TObject);
    procedure BuildMacrosTVPopupMenuPopup(Sender: TObject);
    procedure DeleteBuildMacroClick(Sender: TObject);
    procedure NewBuildMacroClick(Sender: TObject);
    procedure NewValueClick(Sender: TObject);
    procedure DeleteValueClick(Sender: TObject);
  private
    FBuildMacros: TIDEBuildMacros;
    fVarImgID: LongInt;
    fValueImgID: LongInt;
    fDefValueImgID: LongInt;
    procedure SaveItemProperties;
    procedure SetBuildMacros(const AValue: TIDEBuildMacros);
    procedure RebuildTreeView;
    function TreeViewAddBuildMacro(aBuildMacro: TLazBuildMacro): TTreeNode;
    procedure TreeViewAddValue(ValuesTVNode: TTreeNode; aValue: string);
    function GetNodeInfo(Node: TTreeNode; out BuildMacro: TLazBuildMacro): TCBMNodeType;
    function GetSelectedNode(out aBuildMacro: TLazBuildMacro;
                             out NodeType: TCBMNodeType): TTreeNode;
    function GetBuildMacroTVNode(aBuildMacro: TLazBuildMacro): TTreeNode;
    function GetMacroNamePrefix: string;
    procedure UpdateItemPropertyControls;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    function GetTitle: String; override;
    procedure Setup(ADialog: TAbstractOptionsEditorDialog); override;
    class function SupportedOptionsClass: TAbstractIDEOptionsClass; override;
    procedure ReadSettings(AOptions: TAbstractIDEOptions); override;
    procedure WriteSettings(AOptions: TAbstractIDEOptions); override;
    property BuildMacros: TIDEBuildMacros read FBuildMacros write SetBuildMacros; // local copy
    procedure LoadFromOptions(Options: TBaseCompilerOptions);
    procedure SaveToOptions(Options: TBaseCompilerOptions);
  end;

implementation

{$R *.lfm}

{ TCompOptBuildMacrosFrame }

procedure TCompOptBuildMacrosFrame.NewBuildMacroClick(Sender: TObject);
var
  NewIdentifier: String;
  NewBuildMacro: TLazBuildMacro;
  i: Integer;
  TVNode: TTreeNode;
begin
  i:=1;
  repeat
    NewIdentifier:=GetMacroNamePrefix+IntToStr(BuildMacros.Count+1);
    if BuildMacros.IndexOfIdentifier(NewIdentifier)<0 then break;
    inc(i);
  until false;
  NewBuildMacro:=BuildMacros.Add(NewIdentifier);
  // add to TreeView
  BuildMacrosTreeView.BeginUpdate;
  TVNode:=TreeViewAddBuildMacro(NewBuildMacro);
  BuildMacrosTreeView.Selected:=TVNode;
  BuildMacrosTreeView.EndUpdate;
  UpdateItemPropertyControls;
end;

procedure TCompOptBuildMacrosFrame.NewValueClick(Sender: TObject);
var
  BuildMacro: TLazBuildMacro;
  NodeType: TCBMNodeType;
  i: Integer;
  NewValueStr: String;
  ValuesTVNode: TTreeNode;
begin
  GetSelectedNode(BuildMacro,NodeType);
  if BuildMacro=nil then exit;
  i:=1;
  repeat
    NewValueStr:=Format(lisValue2, [IntToStr(i)]);
    if BuildMacro.Values.IndexOf(NewValueStr)<0 then break;
    inc(i);
  until false;
  BuildMacro.Values.Add(NewValueStr);
  BuildMacrosTreeView.BeginUpdate;
  ValuesTVNode:=GetBuildMacroTVNode(BuildMacro);
  TreeViewAddValue(ValuesTVNode,NewValueStr);
  ValuesTVNode.Expand(true);
  BuildMacrosTreeView.EndUpdate;
end;

procedure TCompOptBuildMacrosFrame.DeleteValueClick(Sender: TObject);
var
  BuildProperty: TLazBuildMacro;
  NodeType: TCBMNodeType;
  SelTVNode: TTreeNode;
  aValue: String;
  i: LongInt;
begin
  SelTVNode:=GetSelectedNode(BuildProperty,NodeType);
  if NodeType<>cbmntValue then exit;
  aValue:=SelTVNode.Text;
  if MessageDlg(lisConfirmDelete,
    Format(lisDeleteValue, ['"', aValue, '"']),
    mtConfirmation,[mbYes,mbCancel],0)<>mrYes
  then exit;
  i:=BuildProperty.Values.IndexOf(aValue);
  if i>=0 then BuildProperty.Values.Delete(i);
  BuildMacrosTreeView.BeginUpdate;
  SelTVNode.Delete;
  BuildMacrosTreeView.EndUpdate;
end;

procedure TCompOptBuildMacrosFrame.DeleteBuildMacroClick(Sender: TObject);
var
  aBuildMacro: TIDEBuildMacro;
  SelTVNode: TTreeNode;
  NodeType: TCBMNodeType;
  i: LongInt;
begin
  SelTVNode:=GetSelectedNode(TLazBuildMacro(aBuildMacro),NodeType);
  if aBuildMacro=nil then exit;
  if MessageDlg(lisConfirmDelete,
    Format(lisDeleteBuildMacro, ['"', aBuildMacro.Identifier, '"']),
    mtConfirmation,[mbYes,mbCancel],0)<>mrYes
  then exit;
  i:=BuildMacros.IndexOfIdentifier(aBuildMacro.Identifier);
  BuildMacros.Delete(i);
  BuildMacrosTreeView.BeginUpdate;
  SelTVNode.Delete;
  BuildMacrosTreeView.EndUpdate;
end;

procedure TCompOptBuildMacrosFrame.BuildMacrosTVPopupMenuPopup(Sender: TObject);
var
  aBuildMacro: TLazBuildMacro;
  NodeType: TCBMNodeType;

  function Add(const aCaption: string; const OnClickEvent: TNotifyEvent): TMenuItem;
  begin
    Result:=TMenuItem.Create(Self);
    Result.Caption:=aCaption;
    Result.OnClick:=OnClickEvent;
    BuildMacrosTVPopupMenu.Items.Add(Result);
  end;

  function AddSeparator: TMenuItem;
  begin
    Result:=nil;
    if BuildMacrosTVPopupMenu.Items.Count=0 then exit;
    Result:=TMenuItem.Create(Self);
    Result.Caption:='-';
    BuildMacrosTVPopupMenu.Items.Add(Result);
  end;

begin
  BuildMacrosTVPopupMenu.Items.Clear;
  GetSelectedNode(aBuildMacro,NodeType);

  if NodeType in [cbmntBuildMacro,cbmntValue] then
    Add('New value',@NewValueClick);
  if NodeType in [cbmntValue] then
    Add('Delete value ...',@DeleteValueClick);
  AddSeparator;
  Add('New build macro',@NewBuildMacroClick);
  if NodeType in [cbmntBuildMacro] then
    Add('Delete build macro ...',@DeleteBuildMacroClick);
end;

procedure TCompOptBuildMacrosFrame.BuildMacrosTreeViewEditing(Sender: TObject;
  Node: TTreeNode; var AllowEdit: Boolean);
var
  BuildProperty: TLazBuildMacro;
  NodeType: TCBMNodeType;
begin
  NodeType:=GetNodeInfo(Node,BuildProperty);
  AllowEdit:=NodeType in [cbmntBuildMacro,cbmntValue];
end;

procedure TCompOptBuildMacrosFrame.BuildMacrosTreeViewSelectionChanged(
  Sender: TObject);
begin
  UpdateItemPropertyControls;
end;

procedure TCompOptBuildMacrosFrame.BuildMacrosTreeViewEdited(Sender: TObject;
  Node: TTreeNode; var S: string);
var
  BuildMacro: TLazBuildMacro;
  NodeType: TCBMNodeType;
  ConflictBuildProperty: TIDEBuildMacro;
  Index: LongInt;
begin
  NodeType:=GetNodeInfo(Node,BuildMacro);
  case NodeType of

  cbmntBuildMacro:
    if S<>BuildMacro.Identifier then begin
      // rename build macro
      if (S='') or (not IsValidIdent(S)) then begin
        MessageDlg(lisCCOErrorCaption,
          Format(lisInvalidBuildMacroTheBuildMacroMustBeAPascalIdentifie, ['"',
            S, '"']),
          mtError,[mbCancel],0);
        S:=BuildMacro.Identifier;
        exit;
      end;
      ConflictBuildProperty:=BuildMacros.VarWithIdentifier(S);
      if (ConflictBuildProperty<>nil) and (ConflictBuildProperty<>BuildMacro) then
      begin
        MessageDlg(lisCCOErrorCaption,
          Format(lisThereIsAlreadyABuildMacroWithTheName, ['"', S, '"']),
          mtError,[mbCancel],0);
        S:=BuildMacro.Identifier;
        exit;
      end;
      BuildMacro.Identifier:=S;
    end;

  cbmntValue:
    begin
      Index:=Node.Index;
      Index:=BuildMacro.Values.IndexOf(S);
      if (Index>=0) and (Index<>Node.Index) then begin
        MessageDlg(lisCCOErrorCaption,
          Format(lisDuplicateFoundOfValue, ['"', S, '"']),
          mtError,[mbCancel],0);
        S:=BuildMacro.Values[Node.Index];
        exit;
      end;
      BuildMacro.Values[Node.Index]:=S;
    end;

  end;
end;

procedure TCompOptBuildMacrosFrame.SetBuildMacros(
  const AValue: TIDEBuildMacros);
begin
  if FBuildMacros=AValue then exit;
  BuildMacros.Assign(AValue);
  RebuildTreeView;
  UpdateItemPropertyControls;
end;

procedure TCompOptBuildMacrosFrame.RebuildTreeView;
var
  i: Integer;
begin
  BuildMacrosTreeView.BeginUpdate;
  BuildMacrosTreeView.Items.Clear;
  if BuildMacros<>nil then begin
    // first level: build macros
    for i:=0 to BuildMacros.Count-1 do
      TreeViewAddBuildMacro(BuildMacros.Items[i]);
  end;
  BuildMacrosTreeView.EndUpdate;
end;

function TCompOptBuildMacrosFrame.TreeViewAddBuildMacro(
  aBuildMacro: TLazBuildMacro): TTreeNode;
var
  Values: TStrings;
  i: Integer;
begin
  // create node for the build macro
  Result:=BuildMacrosTreeView.Items.AddObject(nil,aBuildMacro.Identifier,aBuildMacro);
  Result.ImageIndex:=fVarImgID;
  Result.SelectedIndex:=Result.ImageIndex;
  // second level
  begin
    // a node for each value
    Values:=aBuildMacro.Values;
    for i:=0 to Values.Count-1 do
      TreeViewAddValue(Result,Values[i]);
  end;
  //DebugLn(['TCompOptBuildMacrosFrame.TreeViewAddBuildMacro ',TVNode.Text]);
  Result.Expand(true);
end;

procedure TCompOptBuildMacrosFrame.TreeViewAddValue(ValuesTVNode: TTreeNode;
  aValue: string);
var
  ValueTVNode: TTreeNode;
begin
  ValueTVNode:=BuildMacrosTreeView.Items.AddChild(ValuesTVNode,aValue);
  ValueTVNode.ImageIndex:=fValueImgID;
  ValueTVNode.SelectedIndex:=ValueTVNode.ImageIndex;
end;

function TCompOptBuildMacrosFrame.GetNodeInfo(Node: TTreeNode; out
  BuildMacro: TLazBuildMacro): TCBMNodeType;

  function GetNodeType(CurNode: TTreeNode): TCBMNodeType;
  var
    ParentType: TCBMNodeType;
  begin
    if CurNode=nil then
      Result:=cbmntNone
    else if TObject(CurNode.Data) is TLazBuildMacro then begin
      BuildMacro:=TLazBuildMacro(CurNode.Data);
      Result:=cbmntBuildMacro;
    end else begin
      ParentType:=GetNodeType(CurNode.Parent);
      case ParentType of
      cbmntBuildMacro:
        Result:=cbmntValue;
      end;
    end;
  end;

begin
  BuildMacro:=nil;
  Result:=GetNodeType(Node);
end;

function TCompOptBuildMacrosFrame.GetSelectedNode(out
  aBuildMacro: TLazBuildMacro; out NodeType: TCBMNodeType): TTreeNode;
begin
  Result:=BuildMacrosTreeView.Selected;
  NodeType:=GetNodeInfo(Result,aBuildMacro);
end;

function TCompOptBuildMacrosFrame.GetBuildMacroTVNode(aBuildMacro: TLazBuildMacro
  ): TTreeNode;
begin
  Result:=BuildMacrosTreeView.Items.GetFirstNode;
  while (Result<>nil) and (TObject(Result.Data)<>aBuildMacro) do
    Result:=Result.GetNextSibling;
end;

function TCompOptBuildMacrosFrame.GetMacroNamePrefix: string;
begin
  Result:='BuildMacro';
  if (BuildMacros=nil) or (BuildMacros.Owner=nil) then exit;
  if BuildMacros.Owner is TPkgCompilerOptions then
    Result:=TPkgCompilerOptions(BuildMacros.Owner).LazPackage.Name+'_macro';
end;

procedure TCompOptBuildMacrosFrame.UpdateItemPropertyControls;
var
  aBuildMacro: TLazBuildMacro;
  NodeType: TCBMNodeType;
begin
  GetSelectedNode(aBuildMacro,NodeType);
  if aBuildMacro<>nil then begin
    BuildMacroSelectedGroupBox.Caption:='Macro '+aBuildMacro.Identifier;
    BuildMacroSelectedGroupBox.Enabled:=true;
    BuildMacroDescriptionEdit.Enabled:=true;
    BuildMacroDescriptionEdit.Text:=aBuildMacro.Description;
  end else begin
    BuildMacroSelectedGroupBox.Caption:='No macro selected';
    BuildMacroSelectedGroupBox.Enabled:=false;
    BuildMacroDescriptionEdit.Enabled:=false;
    BuildMacroDescriptionEdit.Text:='';
  end;
end;

procedure TCompOptBuildMacrosFrame.SaveItemProperties;
var
  BuildMacro: TLazBuildMacro;
  NodeType: TCBMNodeType;
begin
  GetSelectedNode(BuildMacro,NodeType);
  if BuildMacro=nil then exit;
  BuildMacro.Description:=BuildMacroDescriptionEdit.Text;
end;

constructor TCompOptBuildMacrosFrame.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  FBuildMacros:=TIDEBuildMacros.Create(nil);

  MacrosGroupBox.Caption:='Build macros:';
  BuildMacrosTreeView.Images := IDEImages.Images_24;
  fVarImgID:=IDEImages.LoadImage(24,'da_define');
  fValueImgID:=IDEImages.LoadImage(24,'da_define');
  fDefValueImgID:=IDEImages.LoadImage(24,'da_define');

  BuildMacroDefaultLabel.Caption:='Hint: A default value can be defined in the conditionals.';
  BuildMacroDescriptionLabel.Caption:='Description:';

  ConditionalsGroupBox.Caption:='Conditionals:';
end;

destructor TCompOptBuildMacrosFrame.Destroy;
begin
  FreeAndNil(FBuildMacros);
  inherited Destroy;
end;

function TCompOptBuildMacrosFrame.GetTitle: String;
begin
  Result:='Build macros';
end;

procedure TCompOptBuildMacrosFrame.ReadSettings(AOptions: TAbstractIDEOptions);
begin
  if AOptions is TBaseCompilerOptions then
    LoadFromOptions(TBaseCompilerOptions(AOptions));
end;

procedure TCompOptBuildMacrosFrame.Setup(ADialog: TAbstractOptionsEditorDialog
  );
begin

end;

class function TCompOptBuildMacrosFrame.SupportedOptionsClass: TAbstractIDEOptionsClass;
begin
  Result := TBaseCompilerOptions;
end;

procedure TCompOptBuildMacrosFrame.WriteSettings(AOptions: TAbstractIDEOptions
  );
begin
  if AOptions is TBaseCompilerOptions then
    SaveToOptions(TBaseCompilerOptions(AOptions));
end;

procedure TCompOptBuildMacrosFrame.LoadFromOptions(Options: TBaseCompilerOptions
  );
begin
  BuildMacros:=Options.BuildMacros as TIDEBuildMacros;
  CondSynEdit.Lines.Text:=Options.Conditionals;
end;

procedure TCompOptBuildMacrosFrame.SaveToOptions(Options: TBaseCompilerOptions
  );
begin
  SaveItemProperties;
  (Options.BuildMacros as TIDEBuildMacros).Assign(BuildMacros);
  Options.Conditionals:=CondSynEdit.Lines.Text;
end;

{$IFDEF EnableBuildModes}
initialization
  RegisterIDEOptionsEditor(GroupCompiler, TCompOptBuildMacrosFrame,
    CompilerOptionsConditional);
{$ENDIF}

end.

