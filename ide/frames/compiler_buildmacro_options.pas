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
 *   Inc., 51 Franklin Street - Fifth Floor, Boston, MA 02110-1335, USA.   *
 *                                                                         *
 ***************************************************************************

  Abstract:
    Frame to edit build macros of package compiler options.
}
unit Compiler_BuildMacro_Options;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  // LCL
  Controls, Forms, StdCtrls, Buttons, ExtCtrls, Dialogs, ComCtrls,
  // CodeTools
  KeywordFuncLists, CodeToolsCfgScript,
  // IdeIntf
  IDEOptionsIntf, IDEOptEditorIntf, CompOptsIntf, MacroIntf, IDEImagesIntf, IDEDialogs,
  // IDE
  CompilerOptions, LazarusIDEStrConsts, PackageDefs;

type
  TCBMNodeType = (
    cbmntNone,
    cbmntBuildMacro,
    cbmntValue
    );
  TCBMPrefixType = (
    cbmpShort,
    cbmpMedium,
    cbmpLong
    );

  { TCompOptBuildMacrosFrame }

  TCompOptBuildMacrosFrame = class(TAbstractIDEOptionsEditor)
    BMAddMacroSpeedButton: TSpeedButton;
    BMAddMacroValueSpeedButton: TSpeedButton;
    BMDeleteMacroSpeedButton: TSpeedButton;
    BuildMacroDescriptionEdit: TEdit;
    BuildMacroSelectedGroupBox: TGroupBox;
    BuildMacrosTreeView: TTreeView;
    BuildMacroDefaultLabel: TLabel;
    BuildMacroDescriptionLabel: TLabel;
    MacrosGroupBox: TGroupBox;
    MacrosSplitter: TSplitter;
    Splitter1: TSplitter;
    procedure BMAddMacroSpeedButtonClick(Sender: TObject);
    procedure BMAddMacroValueSpeedButtonClick(Sender: TObject);
    procedure BMDeleteMacroSpeedButtonClick(Sender: TObject);
    procedure BuildMacroDescriptionEditExit(Sender: TObject);
    procedure BuildMacrosTreeViewEdited(Sender: TObject; Node: TTreeNode;
      var S: string);
    procedure BuildMacrosTreeViewEditing(Sender: TObject; Node: TTreeNode;
      var AllowEdit: Boolean);
    procedure BuildMacrosTreeViewSelectionChanged(Sender: TObject);
  private
    FBuildMacros: TIDEBuildMacros;
    FMacrosOwner: TObject;
    fVarImgID: LongInt;
    fValueImgID: LongInt;
    fDefValueImgID: LongInt;
    procedure SetBuildMacros(const AValue: TIDEBuildMacros);
    procedure RebuildTreeView;
    function TreeViewAddBuildMacro(aBuildMacro: TLazBuildMacro): TTreeNode;
    procedure TreeViewAddValue(ValuesTVNode: TTreeNode; aValue: string);
    function GetNodeInfo(Node: TTreeNode; out BuildMacro: TLazBuildMacro): TCBMNodeType;
    function GetSelectedNode(out aBuildMacro: TLazBuildMacro;
                             out NodeType: TCBMNodeType): TTreeNode;
    function GetBuildMacroTVNode(aBuildMacro: TLazBuildMacro): TTreeNode;
    function GetMacroNamePrefix(PrefixType: TCBMPrefixType): string;
    procedure UpdateItemPropertyControls;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    function GetTitle: String; override;
    procedure Setup({%H-}ADialog: TAbstractOptionsEditorDialog); override;
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

procedure TCompOptBuildMacrosFrame.BuildMacrosTreeViewEditing(Sender: TObject;
  Node: TTreeNode; var AllowEdit: Boolean);
var
  BuildProperty: TLazBuildMacro;
  NodeType: TCBMNodeType;
begin
  NodeType:=GetNodeInfo(Node,BuildProperty);
  AllowEdit:=NodeType in [cbmntBuildMacro,cbmntValue];
end;

procedure TCompOptBuildMacrosFrame.BuildMacrosTreeViewSelectionChanged(Sender: TObject);
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
  Prefix: String;
  BetterName: String;
  DlgResult: TModalResult;
  Vars: TCTCfgScriptVariables;
  ok: Boolean;
begin
  NodeType:=GetNodeInfo(Node,BuildMacro);
  case NodeType of

  cbmntBuildMacro:
    if S<>BuildMacro.Identifier then begin
      // rename build macro
      ok:=false;
      try
        // check syntax
        if not IsValidIdent(S) then begin
          IDEMessageDialog(lisCCOErrorCaption,
            Format(lisInvalidMacroTheMacroMustBeAPascalIdentifie, [S]),
            mtError,[mbCancel]);
          exit;
        end;

        // check for prefix
        Prefix:=GetMacroNamePrefix(cbmpShort);
        if (Prefix<>'') and (SysUtils.CompareText(Prefix,copy(S,1,length(Prefix)))<>0)
        then  begin
          BetterName:=GetMacroNamePrefix(cbmpMedium)+S;
          DlgResult:=IDEQuestionDialog(lisCCOWarningCaption,
              Format(lisTheMacroDoesNotBeginWith, [S, Prefix]),
              mtWarning, [mrCancel,
                          mrYes, Format(lisRenameTo, [BetterName]),
                          mrIgnore]);
          if DlgResult=mrIgnore then begin
          end else if DlgResult=mrYes then
            S:=BetterName
          else begin
            exit;
          end;
        end;

        // check for keyword
        if WordIsKeyWord.DoItCaseInsensitive(S) then begin
          IDEMessageDialog(lisCCOErrorCaption,
            Format(lisInvalidMacroTheNameIsAKeyword, [S]),
            mtError,[mbCancel]);
          exit;
        end;

        // check for conflicts with built-in macros
        if IDEMacros.IsMacro(s) then begin
          IDEMessageDialog(lisCCOErrorCaption,
            Format(lisThereIsAlreadyAnIDEMacroWithTheName, [S]),
            mtError,[mbCancel]);
          exit;
        end;

        // check for duplicates
        ConflictBuildProperty:=BuildMacros.VarWithIdentifier(S);
        if ((ConflictBuildProperty<>nil) and (ConflictBuildProperty<>BuildMacro))
        or (SysUtils.CompareText('TargetOS',S)=0)
        or (SysUtils.CompareText('SrcOS',S)=0)
        or (SysUtils.CompareText('SrcOS2',S)=0)
        or (SysUtils.CompareText('TargetCPU',S)=0)
        or (SysUtils.CompareText('LCLWidgetType',S)=0)
        then begin
          IDEMessageDialog(lisCCOErrorCaption,
            Format(lisThereIsAlreadyAMacroWithTheName, [S]),
            mtError,[mbCancel]);
          exit;
        end;

        // check for duplicates with used packages
        if (BuildMacros<>nil) and (BuildMacros.Owner is TBaseCompilerOptions) then
        begin
          Vars:=GetBuildMacroValues(TBaseCompilerOptions(BuildMacros.Owner),false);
          if (Vars<>nil) and Vars.IsDefined(PChar(S)) then begin
            DlgResult:=MessageDlg(lisCCOWarningCaption,
              Format(lisThereIsAlreadyAMacroWithTheName, [S]),
              mtWarning,[mbCancel,mbIgnore],0);
            if DlgResult<>mrIgnore then
              exit;
          end;
        end;

        ok:=true;
      finally
        if not ok then
          S:=BuildMacro.Identifier;
      end;

      // rename build macro
      BuildMacro.Identifier:=S;
    end;

  cbmntValue:
    begin
      Index:=Node.Index;
      Index:=BuildMacro.Values.IndexOf(S);
      if (Index>=0) and (Index<>Node.Index) then begin
        IDEMessageDialog(lisCCOErrorCaption, Format(lisDuplicateFoundOfValue, [S]),
          mtError,[mbCancel]);
        S:=BuildMacro.Values[Node.Index];
        exit;
      end;
      BuildMacro.Values[Node.Index]:=S;
    end;

  end;
end;

procedure TCompOptBuildMacrosFrame.BMAddMacroSpeedButtonClick(Sender: TObject);
var
  NewIdentifier: String;
  NewBuildMacro: TLazBuildMacro;
  i: Integer;
  TVNode: TTreeNode;
begin
  //debugln(['TCompOptBuildMacrosFrame.BMAddMacroSpeedButtonClick ',GetMacroNamePrefix(cbmpLong)]);
  i:=1;
  repeat
    NewIdentifier:=GetMacroNamePrefix(cbmpLong)+IntToStr(BuildMacros.Count+1);
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

procedure TCompOptBuildMacrosFrame.BMAddMacroValueSpeedButtonClick(
  Sender: TObject);
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

procedure TCompOptBuildMacrosFrame.BMDeleteMacroSpeedButtonClick(Sender: TObject);
var
  aBuildMacro: TIDEBuildMacro;
  SelTVNode: TTreeNode;
  NodeType: TCBMNodeType;
  i: LongInt;
  aValue: String;
begin
  SelTVNode:=GetSelectedNode(TLazBuildMacro(aBuildMacro),NodeType);
  if aBuildMacro=nil then exit;
  if NodeType=cbmntValue then
  begin
    aValue:=SelTVNode.Text;
    if IDEMessageDialog(lisConfirmDelete, Format(lisDeleteValue, [aValue]),
      mtConfirmation,[mbYes,mbCancel])<>mrYes
    then exit;
    i:=aBuildMacro.Values.IndexOf(aValue);
    if i>=0 then aBuildMacro.Values.Delete(i);
  end else begin
    if IDEMessageDialog(lisConfirmDelete,
      Format(lisDeleteMacro, [aBuildMacro.Identifier]),
      mtConfirmation,[mbYes,mbCancel])<>mrYes
    then exit;
    i:=BuildMacros.IndexOfIdentifier(aBuildMacro.Identifier);
    BuildMacros.Delete(i);
  end;
  BuildMacrosTreeView.BeginUpdate;
  SelTVNode.Delete;
  BuildMacrosTreeView.EndUpdate;
end;

procedure TCompOptBuildMacrosFrame.BuildMacroDescriptionEditExit(Sender: TObject);
var
  BuildMacro: TLazBuildMacro;
  NodeType: TCBMNodeType;
begin
  GetSelectedNode(BuildMacro,NodeType);
  if BuildMacro<>nil then
    BuildMacro.Description:=BuildMacroDescriptionEdit.Text;
end;

procedure TCompOptBuildMacrosFrame.SetBuildMacros(const AValue: TIDEBuildMacros);
begin
  if FBuildMacros=AValue then exit;
  if AValue<>nil then
  begin
    BuildMacros.Assign(AValue);
    FMacrosOwner:=AValue.Owner;
  end else begin
    FMacrosOwner:=nil;
  end;
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

function TCompOptBuildMacrosFrame.GetNodeInfo(Node: TTreeNode;
  out BuildMacro: TLazBuildMacro): TCBMNodeType;

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
      else
        Result:=cbmntNone;
      end;
    end;
  end;

begin
  BuildMacro:=nil;
  Result:=GetNodeType(Node);
end;

function TCompOptBuildMacrosFrame.GetSelectedNode(out aBuildMacro: TLazBuildMacro;
  out NodeType: TCBMNodeType): TTreeNode;
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

function TCompOptBuildMacrosFrame.GetMacroNamePrefix(PrefixType: TCBMPrefixType
  ): string;
begin
  if PrefixType=cbmpShort then
    Result:=''
  else
    Result:='BuildMacro';
  if (FMacrosOwner=nil) then
    exit;
  if FMacrosOwner is TPkgCompilerOptions then
  begin
    Result:=TPkgCompilerOptions(FMacrosOwner).LazPackage.Name;
    if ord(PrefixType)>=ord(cbmpMedium) then
      Result:=Result+'_';
    if PrefixType=cbmpLong then
      Result:=Result+'macro';
  end;
end;

procedure TCompOptBuildMacrosFrame.UpdateItemPropertyControls;
var
  aBuildMacro: TLazBuildMacro;
  NodeType: TCBMNodeType;
begin
  GetSelectedNode(aBuildMacro,NodeType);
  if aBuildMacro<>nil then begin
    BuildMacroSelectedGroupBox.Caption:=Format(lisMacro, [aBuildMacro.Identifier]);
    BuildMacroSelectedGroupBox.Enabled:=true;
    BuildMacroDescriptionEdit.Enabled:=true;
    BuildMacroDescriptionEdit.Text:=aBuildMacro.Description;
    BMAddMacroValueSpeedButton.Hint:=Format(lisAddValueToMacro, [aBuildMacro.Identifier]);
    if NodeType=cbmntBuildMacro then
      BMDeleteMacroSpeedButton.Hint:=Format(lisDeleteMacro, [aBuildMacro.Identifier])
    else
      BMDeleteMacroSpeedButton.Hint:=Format(lisDeleteValue2, [BuildMacrosTreeView.Selected.Text]);
  end else begin
    BuildMacroSelectedGroupBox.Caption:=lisNoMacroSelected;
    BuildMacroSelectedGroupBox.Enabled:=false;
    BuildMacroDescriptionEdit.Enabled:=false;
    BuildMacroDescriptionEdit.Text:='';
    BMAddMacroValueSpeedButton.Hint:='';
    BMDeleteMacroSpeedButton.Hint:='';
  end;
  BMAddMacroSpeedButton.Hint:=lisAddNewMacro;
  BMAddMacroValueSpeedButton.Enabled:=NodeType in [cbmntBuildMacro,cbmntValue];
  BMDeleteMacroSpeedButton.Enabled:=NodeType in [cbmntBuildMacro,cbmntValue];
end;

constructor TCompOptBuildMacrosFrame.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  FBuildMacros:=TIDEBuildMacros.Create(nil);

  MacrosGroupBox.Caption:=lisIDEMacros;
  BuildMacrosTreeView.Images := IDEImages.Images_24;
  fVarImgID:=IDEImages.LoadImage('da_define', 24);
  fValueImgID:=IDEImages.LoadImage('da_define', 24);
  fDefValueImgID:=IDEImages.LoadImage('da_define', 24);

  BuildMacroDefaultLabel.Caption:=
    lisHintADefaultValueCanBeDefinedInTheConditionals;
  BuildMacroDescriptionLabel.Caption:=lisCodeToolsDefsDescription;

  IDEImages.AssignImage(BMAddMacroSpeedButton, 'laz_add');
  IDEImages.AssignImage(BMAddMacroValueSpeedButton, 'laz_add');
  IDEImages.AssignImage(BMDeleteMacroSpeedButton, 'laz_delete');
end;

destructor TCompOptBuildMacrosFrame.Destroy;
begin
  FreeAndNil(FBuildMacros);
  inherited Destroy;
end;

function TCompOptBuildMacrosFrame.GetTitle: String;
begin
  Result:=lisIDEMacros;
end;

procedure TCompOptBuildMacrosFrame.ReadSettings(AOptions: TAbstractIDEOptions);
begin
  if AOptions is TBaseCompilerOptions then
    LoadFromOptions(TBaseCompilerOptions(AOptions));
end;

procedure TCompOptBuildMacrosFrame.Setup(ADialog: TAbstractOptionsEditorDialog);
begin
end;

class function TCompOptBuildMacrosFrame.SupportedOptionsClass: TAbstractIDEOptionsClass;
begin
  Result := TPkgCompilerOptions;
end;

procedure TCompOptBuildMacrosFrame.WriteSettings(AOptions: TAbstractIDEOptions);
begin
  if AOptions is TBaseCompilerOptions then
    SaveToOptions(TBaseCompilerOptions(AOptions));
end;

procedure TCompOptBuildMacrosFrame.LoadFromOptions(Options: TBaseCompilerOptions);
begin
  BuildMacros:=Options.BuildMacros as TIDEBuildMacros;
end;

procedure TCompOptBuildMacrosFrame.SaveToOptions(Options: TBaseCompilerOptions);
begin
  (Options.BuildMacros as TIDEBuildMacros).Assign(BuildMacros);
end;

initialization
  RegisterIDEOptionsEditor(GroupPkgCompiler, TCompOptBuildMacrosFrame,
    CompilerOptionsConditional);

end.

