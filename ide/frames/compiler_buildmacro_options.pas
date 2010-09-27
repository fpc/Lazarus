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
  Classes, SysUtils, LCLProc, FileUtil, Controls, Forms, StdCtrls, Grids, LCLType,
  Buttons, ExtCtrls, Dialogs, ComCtrls, Menus, AvgLvlTree, IDEImagesIntf,
  KeywordFuncLists, CodeToolsCfgScript,
  SynEdit, SynHighlighterPas, SynEditKeyCmds,
  ProjectIntf, PackageIntf, CompilerOptions, IDEOptionsIntf, MacroIntf,
  EditorOptions, LazarusIDEStrConsts, CompOptsModes, SourceSynEditor,
  PackageDefs;

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
    BMDeleteSpeedButton: TSpeedButton;
    BuildMacroDescriptionEdit: TEdit;
    BuildMacroSelectedGroupBox: TGroupBox;
    BuildMacrosTreeView: TTreeView;
    BuildMacroDefaultLabel: TLabel;
    BuildMacroDescriptionLabel: TLabel;
    ConditionalsGroupBox: TGroupBox;
    CondStatusbar: TStatusBar;
    CondSynEdit: TSynEdit;
    MacrosGroupBox: TGroupBox;
    MacrosSplitter: TSplitter;
    Splitter1: TSplitter;
    procedure BMAddMacroSpeedButtonClick(Sender: TObject);
    procedure BMAddMacroValueSpeedButtonClick(Sender: TObject);
    procedure BMDeleteSpeedButtonClick(Sender: TObject);
    procedure BuildMacrosTreeViewEdited(Sender: TObject; Node: TTreeNode;
      var S: string);
    procedure BuildMacrosTreeViewEditing(Sender: TObject; Node: TTreeNode;
      var AllowEdit: Boolean);
    procedure BuildMacrosTreeViewSelectionChanged(Sender: TObject);
    procedure CondSynEditChange(Sender: TObject);
    procedure CondSynEditStatusChange(Sender: TObject;
      Changes: TSynStatusChanges);
    procedure OnIdle(Sender: TObject; var Done: Boolean);
  private
    FHighlighter: TIDESynFreePasSyn;
    FBuildMacros: TIDEBuildMacros;
    FIdleConnected: Boolean;
    FStatusMessage: string;
    fVarImgID: LongInt;
    fValueImgID: LongInt;
    fDefValueImgID: LongInt;
    fEngine: TCTConfigScriptEngine;
    procedure SaveItemProperties;
    procedure SetBuildMacros(const AValue: TIDEBuildMacros);
    procedure RebuildTreeView;
    procedure SetIdleConnected(const AValue: Boolean);
    procedure SetStatusMessage(const AValue: string);
    function TreeViewAddBuildMacro(aBuildMacro: TLazBuildMacro): TTreeNode;
    procedure TreeViewAddValue(ValuesTVNode: TTreeNode; aValue: string);
    function GetNodeInfo(Node: TTreeNode; out BuildMacro: TLazBuildMacro): TCBMNodeType;
    function GetSelectedNode(out aBuildMacro: TLazBuildMacro;
                             out NodeType: TCBMNodeType): TTreeNode;
    function GetBuildMacroTVNode(aBuildMacro: TLazBuildMacro): TTreeNode;
    function GetMacroNamePrefix(PrefixType: TCBMPrefixType): string;
    procedure UpdateItemPropertyControls;
    procedure UpdateMessages;
    procedure UpdateStatusBar;
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
    property IdleConnected: Boolean read FIdleConnected write SetIdleConnected;
    property StatusMessage: string read FStatusMessage write SetStatusMessage;
  end;

implementation

{$R *.lfm}

{ TCompOptBuildMacrosFrame }

procedure TCompOptBuildMacrosFrame.OnIdle(Sender: TObject; var Done: Boolean);
begin
  IdleConnected:=false;
  UpdateMessages;
end;

procedure TCompOptBuildMacrosFrame.CondSynEditChange(Sender: TObject);
begin
  UpdateStatusBar;
  IdleConnected:=true;
end;

procedure TCompOptBuildMacrosFrame.CondSynEditStatusChange(Sender: TObject;
  Changes: TSynStatusChanges);
begin
  UpdateStatusBar;
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
        if (S='') or (not IsValidIdent(S)) then begin
          MessageDlg(lisCCOErrorCaption,
            Format(lisInvalidBuildMacroTheBuildMacroMustBeAPascalIdentifie, ['"',
              S, '"']),
            mtError,[mbCancel],0);
          exit;
        end;

        // check for prefix
        Prefix:=GetMacroNamePrefix(cbmpShort);
        if (Prefix<>'') and (SysUtils.CompareText(Prefix,copy(S,1,length(Prefix)))<>0)
        then  begin
          BetterName:=GetMacroNamePrefix(cbmpMedium)+S;
          DlgResult:=QuestionDlg('Warning',
            'The build macro "'+S+'" does not begin with "'+Prefix+'".',
            mtWarning,[mrCancel,mrYes,'Rename to '+BetterName,mrIgnore],0);
          if DlgResult=mrIgnore then begin
          end else if DlgResult=mrYes then
            S:=BetterName
          else begin
            exit;
          end;
        end;

        // check for keyword
        if WordIsKeyWord.DoItCaseInsensitive(S) then begin
          MessageDlg(lisCCOErrorCaption,
            Format(lisInvalidBuildMacroTheNameIsAKeyword, [S]),
            mtError,[mbCancel],0);
          exit;
        end;

        // check for conflicts with built-in macros
        if IDEMacros.IsMacro(s) then begin
          MessageDlg(lisCCOErrorCaption,
            Format(lisThereIsAlreadyAnIDEMacroWithTheName, [S]),
            mtError,[mbCancel],0);
          exit;
        end;

        // check for duplicates
        ConflictBuildProperty:=BuildMacros.VarWithIdentifier(S);
        if ((ConflictBuildProperty<>nil) and (ConflictBuildProperty<>BuildMacro))
        or (SysUtils.CompareText('TargetOS',S)=0)
        or (SysUtils.CompareText('TargetCPU',S)=0)
        or (SysUtils.CompareText('LCLWidgetType',S)=0)
        then begin
          MessageDlg(lisCCOErrorCaption,
            Format(lisThereIsAlreadyABuildMacroWithTheName, ['"', S, '"']),
            mtError,[mbCancel],0);
          exit;
        end;

        // check for duplicates with used packages
        if (BuildMacros<>nil) and (BuildMacros.Owner is TBaseCompilerOptions) then
        begin
          Vars:=GetBuildMacroValues(TBaseCompilerOptions(BuildMacros.Owner),false);
          if (Vars<>nil) and Vars.IsDefined(PChar(S)) then begin
            DlgResult:=MessageDlg('Warning',
              Format(lisThereIsAlreadyABuildMacroWithTheName, ['"', S, '"']),
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

procedure TCompOptBuildMacrosFrame.BMAddMacroSpeedButtonClick(Sender: TObject);
var
  NewIdentifier: String;
  NewBuildMacro: TLazBuildMacro;
  i: Integer;
  TVNode: TTreeNode;
begin
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

procedure TCompOptBuildMacrosFrame.BMDeleteSpeedButtonClick(Sender: TObject);
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
    if MessageDlg(lisConfirmDelete,
      Format(lisDeleteValue, ['"', aValue, '"']),
      mtConfirmation,[mbYes,mbCancel],0)<>mrYes
    then exit;
    i:=aBuildMacro.Values.IndexOf(aValue);
    if i>=0 then aBuildMacro.Values.Delete(i);
  end else begin
    if MessageDlg(lisConfirmDelete,
      Format(lisDeleteBuildMacro, ['"', aBuildMacro.Identifier, '"']),
      mtConfirmation,[mbYes,mbCancel],0)<>mrYes
    then exit;
    i:=BuildMacros.IndexOfIdentifier(aBuildMacro.Identifier);
    BuildMacros.Delete(i);
  end;
  BuildMacrosTreeView.BeginUpdate;
  SelTVNode.Delete;
  BuildMacrosTreeView.EndUpdate;
end;

procedure TCompOptBuildMacrosFrame.SetBuildMacros(
  const AValue: TIDEBuildMacros);
begin
  if FBuildMacros=AValue then exit;
  BuildMacros.Assign(AValue);
  RebuildTreeView;
  UpdateItemPropertyControls;
  IdleConnected:=true;
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

procedure TCompOptBuildMacrosFrame.SetIdleConnected(const AValue: Boolean);
begin
  if FIdleConnected=AValue then exit;
  FIdleConnected:=AValue;
  if FIdleConnected then
    Application.AddOnIdleHandler(@OnIdle)
  else
    Application.RemoveOnIdleHandler(@OnIdle);
end;

procedure TCompOptBuildMacrosFrame.SetStatusMessage(const AValue: string);
begin
  if FStatusMessage=AValue then exit;
  FStatusMessage:=AValue;
  CondStatusbar.Panels[2].Text := FStatusMessage;
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

function TCompOptBuildMacrosFrame.GetMacroNamePrefix(PrefixType: TCBMPrefixType
  ): string;
begin
  if PrefixType=cbmpShort then
    Result:=''
  else
    Result:='BuildMacro';
  if (BuildMacros=nil) or (BuildMacros.Owner=nil) then exit;
  if BuildMacros.Owner is TPkgCompilerOptions then
  begin
    Result:=TPkgCompilerOptions(BuildMacros.Owner).LazPackage.Name;
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
    BuildMacroSelectedGroupBox.Caption:='Macro '+aBuildMacro.Identifier;
    BuildMacroSelectedGroupBox.Enabled:=true;
    BuildMacroDescriptionEdit.Enabled:=true;
    BuildMacroDescriptionEdit.Text:=aBuildMacro.Description;
    BMAddMacroValueSpeedButton.Hint:='Add value to macro '+aBuildMacro.Identifier;
    BMDeleteSpeedButton.Hint:='Delete macro '+aBuildMacro.Identifier;
  end else begin
    BuildMacroSelectedGroupBox.Caption:='No macro selected';
    BuildMacroSelectedGroupBox.Enabled:=false;
    BuildMacroDescriptionEdit.Enabled:=false;
    BuildMacroDescriptionEdit.Text:='';
    BMAddMacroValueSpeedButton.Hint:='';
    BMDeleteSpeedButton.Hint:='';
  end;
  BMAddMacroSpeedButton.Hint:='Add new macro';
  BMAddMacroValueSpeedButton.Enabled:=NodeType=cbmntBuildMacro;
  BMDeleteSpeedButton.Enabled:=NodeType in [cbmntBuildMacro,cbmntValue];
end;

procedure TCompOptBuildMacrosFrame.UpdateMessages;
begin
  fEngine.Execute(CondSynEdit.Lines.Text,1);
  if fEngine.ErrorCount>0 then begin
    StatusMessage:=fEngine.GetErrorStr(0);
  end else begin
    StatusMessage:='No errors';
  end;
end;

procedure TCompOptBuildMacrosFrame.UpdateStatusBar;
var
  PanelCharMode: String;
  PanelXY: String;
begin
  PanelXY := Format(' %6d:%4d',[CondSynEdit.CaretY,CondSynEdit.CaretX]);
  if CondSynEdit.InsertMode then
    PanelCharMode := uepIns
  else
    PanelCharMode := uepOvr;

  CondStatusbar.Panels[0].Text := PanelXY;
  CondStatusbar.Panels[1].Text := PanelCharMode;
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
  fEngine:=TCTConfigScriptEngine.Create;

  MacrosGroupBox.Caption:='Build macros:';
  BuildMacrosTreeView.Images := IDEImages.Images_24;
  fVarImgID:=IDEImages.LoadImage(24,'da_define');
  fValueImgID:=IDEImages.LoadImage(24,'da_define');
  fDefValueImgID:=IDEImages.LoadImage(24,'da_define');

  BuildMacroDefaultLabel.Caption:='Hint: A default value can be defined in the conditionals.';
  BuildMacroDescriptionLabel.Caption:='Description:';

  ConditionalsGroupBox.Caption:='Conditionals:';

  CondSynEdit.OnStatusChange:=@CondSynEditStatusChange;
end;

destructor TCompOptBuildMacrosFrame.Destroy;
begin
  FreeAndNil(fEngine);
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
  BMAddMacroSpeedButton.LoadGlyphFromLazarusResource('laz_add');
  BMAddMacroValueSpeedButton.LoadGlyphFromLazarusResource('laz_add');
  BMDeleteSpeedButton.LoadGlyphFromLazarusResource('laz_delete');
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
  EditorOpts.GetSynEditPreviewSettings(CondSynEdit);
  CondSynEdit.ReadOnly:=false;
  if FHighlighter=nil then
  begin
    FHighlighter := TPreviewPasSyn.Create(Self);
    CondSynEdit.Highlighter:=FHighlighter;
  end;
  EditorOpts.ReadHighlighterSettings(FHighlighter, '');
  UpdateStatusBar;
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

