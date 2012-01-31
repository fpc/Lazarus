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
  Classes, SysUtils, Math, types, AVL_Tree, LCLProc, FileUtil, Controls, Forms,
  StdCtrls, Grids, LCLType, Buttons, ExtCtrls, Dialogs, ComCtrls, Menus,
  AvgLvlTree, SynEdit, SynHighlighterPas, SynEditKeyCmds, SynCompletion,
  KeywordFuncLists, CodeToolsCfgScript, IDEImagesIntf, IDECommands, ProjectIntf,
  PackageIntf, IDEOptionsIntf, MacroIntf, CompilerOptions, EditorOptions, CompOptsIntf,
  LazarusIDEStrConsts, CompOptsModes, SourceSynEditor, PackageDefs;

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
    ConditionalsGroupBox: TGroupBox;
    CondStatusbar: TStatusBar;
    CondSynEdit: TSynEdit;
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
    procedure CondSynEditChange(Sender: TObject);
    procedure CondSynEditKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure CondSynEditKeyPress(Sender: TObject; var Key: char);
    procedure CondSynEditProcessCommand(Sender: TObject;
      var Command: TSynEditorCommand; var AChar: TUTF8Char; Data: pointer);
    procedure CondSynEditProcessUserCommand(Sender: TObject;
      var Command: TSynEditorCommand; var AChar: TUTF8Char; Data: pointer);
    procedure CondSynEditStatusChange(Sender: TObject;
      Changes: TSynStatusChanges);
    procedure fSynCompletionCancel(Sender: TObject);
    procedure fSynCompletionExecute(Sender: TObject);
    procedure fSynCompletionKeyCompletePrefix(Sender: TObject);
    procedure fSynCompletionKeyDelete(Sender: TObject);
    procedure fSynCompletionKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure fSynCompletionKeyNextChar(Sender: TObject);
    procedure fSynCompletionKeyPrevChar(Sender: TObject);
    procedure fSynCompletionSearchPosition(var Position: integer);
    procedure fSynCompletionUTF8KeyPress(Sender: TObject; var UTF8Key: TUTF8Char);
    procedure fSynCompletionValidate(Sender: TObject; KeyChar: TUTF8Char;
      Shift: TShiftState);
    procedure OnIdle(Sender: TObject; var Done: Boolean);
  private
    FCompletionHistory: TStrings;
    FCompletionValues: TStrings;
    FDefaultVariables: TCTCfgScriptVariables;
    FHighlighter: TIDESynFreePasSyn;
    FBuildMacros: TIDEBuildMacros;
    FIdleConnected: Boolean;
    FIsPackage: boolean;
    FMacrosOwner: TObject;
    FStatusMessage: string;
    fVarImgID: LongInt;
    fValueImgID: LongInt;
    fDefValueImgID: LongInt;
    fEngine: TCTConfigScriptEngine;
    fSynCompletion: TSynCompletion;
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
    procedure StartCompletion;
    procedure UpdateCompletionValues;
    function GetCondCursorWord: string;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    function GetTitle: String; override;
    procedure Setup(ADialog: TAbstractOptionsEditorDialog); override;
    class function SupportedOptionsClass: TAbstractIDEOptionsClass; override;
    procedure ReadSettings(AOptions: TAbstractIDEOptions); override;
    procedure WriteSettings(AOptions: TAbstractIDEOptions); override;
    property BuildMacros: TIDEBuildMacros read FBuildMacros write SetBuildMacros; // local copy
    property MacrosOwner: TObject read FMacrosOwner;
    procedure LoadFromOptions(Options: TBaseCompilerOptions);
    procedure SaveToOptions(Options: TBaseCompilerOptions);
    property IdleConnected: Boolean read FIdleConnected write SetIdleConnected;
    property StatusMessage: string read FStatusMessage write SetStatusMessage;
    property DefaultVariables: TCTCfgScriptVariables read FDefaultVariables;
    property IsPackage: boolean read FIsPackage;
    property CompletionValues: TStrings read FCompletionValues;
    property CompletionHistory: TStrings read FCompletionHistory;
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

procedure TCompOptBuildMacrosFrame.CondSynEditKeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);
begin
end;

procedure TCompOptBuildMacrosFrame.CondSynEditKeyPress(Sender: TObject;
  var Key: char);
begin
  //debugln(['TCompOptBuildMacrosFrame.CondSynEditKeyPress ',ord(Key)]);
end;

procedure TCompOptBuildMacrosFrame.CondSynEditProcessCommand(Sender: TObject;
  var Command: TSynEditorCommand; var AChar: TUTF8Char; Data: pointer);
begin

end;

procedure TCompOptBuildMacrosFrame.CondSynEditProcessUserCommand(
  Sender: TObject; var Command: TSynEditorCommand; var AChar: TUTF8Char;
  Data: pointer);
begin
  if (Command=ecWordCompletion) or (Command=ecIdentCompletion) then
    StartCompletion;
end;

procedure TCompOptBuildMacrosFrame.CondSynEditStatusChange(Sender: TObject;
  Changes: TSynStatusChanges);
begin
  if fSynCompletion.TheForm.Visible then
  begin
    //debugln(['TCompOptBuildMacrosFrame.CondSynEditStatusChange ']);
    fSynCompletion.CurrentString := GetCondCursorWord;
  end;
  UpdateStatusBar;
end;

procedure TCompOptBuildMacrosFrame.fSynCompletionCancel(Sender: TObject);
begin
  //debugln(['TCompOptBuildMacrosFrame.fSynCompletionCancel ',fSynCompletion.TheForm.Visible]);
  if fSynCompletion.TheForm.Visible then
    fSynCompletion.Deactivate;
  fSynCompletion.RemoveEditor(CondSynEdit);
end;

procedure TCompOptBuildMacrosFrame.fSynCompletionExecute(Sender: TObject);
begin
  //debugln(['TCompOptBuildMacrosFrame.fSynCompletionExecute ']);
end;

procedure TCompOptBuildMacrosFrame.fSynCompletionKeyCompletePrefix(
  Sender: TObject);
begin
  //debugln(['TCompOptBuildMacrosFrame.fSynCompletionKeyCompletePrefix ToDo']);
end;

procedure TCompOptBuildMacrosFrame.fSynCompletionKeyDelete(Sender: TObject);
begin
  //debugln(['TCompOptBuildMacrosFrame.fSynCompletionKeyDelete']);
end;

procedure TCompOptBuildMacrosFrame.fSynCompletionKeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);
begin
  //debugln(['TCompOptBuildMacrosFrame.fSynCompletionKeyDown ']);
  if Key=VK_BACK then
  begin
    Key:=VK_UNKNOWN;
    if fSynCompletion.CurrentString<>'' then
      CondSynEdit.CommandProcessor(ecDeleteLastChar,#0,nil);
  end;
end;

procedure TCompOptBuildMacrosFrame.fSynCompletionKeyNextChar(Sender: TObject);
var
  XY: TPoint;
  StartX: integer;
  EndX: integer;
  Line: string;
begin
  XY:=CondSynEdit.LogicalCaretXY;
  if XY.Y>CondSynEdit.Lines.Count then exit;
  CondSynEdit.GetWordBoundsAtRowCol(XY,StartX,EndX);
  if EndX<=XY.X then exit;
  Line := CondSynEdit.Lines[XY.Y - 1];
  inc(XY.X,UTF8CharacterLength(@Line[XY.X-1]));
  CondSynEdit.LogicalCaretXY:=XY;
end;

procedure TCompOptBuildMacrosFrame.fSynCompletionKeyPrevChar(Sender: TObject);
var
  XY: TPoint;
  StartX: integer;
  EndX: integer;
  Line: string;
begin
  XY:=CondSynEdit.LogicalCaretXY;
  if XY.Y>CondSynEdit.Lines.Count then exit;
  CondSynEdit.GetWordBoundsAtRowCol(XY,StartX,EndX);
  if StartX>=XY.X then exit;
  Line := CondSynEdit.Lines[XY.Y - 1];
  XY.X:=UTF8FindNearestCharStart(PChar(Line),length(Line),XY.X-2)+1;
  CondSynEdit.LogicalCaretXY:=XY;
end;

procedure TCompOptBuildMacrosFrame.fSynCompletionSearchPosition(
  var Position: integer);
var
  sl: TStringList;
  Prefix: String;
  s: string;
  i: Integer;
begin
  //debugln(['TCompOptBuildMacrosFrame.fSynCompletionSearchPosition "',fSynCompletion.CurrentString,'"']);
  Prefix:=fSynCompletion.CurrentString;
  sl:=TStringList.Create;
  try
    Position:=-1;
    for i:=0 to CompletionValues.Count-1 do
    begin
      s:=CompletionValues[i];
      if SysUtils.CompareText(Prefix,copy(s,1,length(Prefix)))<>0 then continue;
      if (Position<0) or (length(Prefix)=length(s)) then
        Position:=sl.Count;
      sl.AddObject(s,TObject({%H-}Pointer(i)));
    end;
    fSynCompletion.ItemList.Assign(sl);
  finally
    sl.Free;
  end;
end;

procedure TCompOptBuildMacrosFrame.fSynCompletionUTF8KeyPress(Sender: TObject;
  var UTF8Key: TUTF8Char);
begin
  //debugln(['TCompOptBuildMacrosFrame.fSynCompletionUTF8KeyPress ']);
end;

procedure TCompOptBuildMacrosFrame.fSynCompletionValidate(Sender: TObject;
  KeyChar: TUTF8Char; Shift: TShiftState);
var
  i: LongInt;
  s: string;
  p: LongInt;
  TxtXY: TPoint;
  TxtStartX: integer;
  TxtEndX: integer;
begin
  //debugln(['TCompOptBuildMacrosFrame.fSynCompletionValidate ']);
  i:=fSynCompletion.Position;
  if (i>=0) and (i<fSynCompletion.ItemList.Count) then begin
    i:=PtrUInt(fSynCompletion.ItemList.Objects[i]);
    if (i>=0) and (i<CompletionValues.Count) then begin
      s:=CompletionValues[i];
      p:=System.Pos(#9,s);
      if p>0 then s:=copy(s,1,p-1);
      TxtXY:=CondSynEdit.LogicalCaretXY;
      CondSynEdit.GetWordBoundsAtRowCol(TxtXY,TxtStartX,TxtEndX);
      CondSynEdit.BeginUndoBlock{$IFDEF SynUndoDebugBeginEnd}('TCompOptBuildMacrosFrame.fSynCompletionValidate'){$ENDIF};
      try
        CondSynEdit.BlockBegin:=Point(TxtStartX,TxtXY.Y);
        CondSynEdit.BlockEnd:=Point(TxtEndX,TxtXY.Y);
        CondSynEdit.SelText:=s;
      finally
        CondSynEdit.EndUndoBlock{$IFDEF SynUndoDebugBeginEnd}('TCompOptBuildMacrosFrame.fSynCompletionValidate'){$ENDIF};
      end;
      FCompletionHistory.Insert(0,s);
      if FCompletionHistory.Count>100 then
        FCompletionHistory.Delete(FCompletionHistory.Count-1);
    end;
  end;

  fSynCompletion.Deactivate;
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
        if (S='') or (not IsValidIdent(S)) then begin
          MessageDlg(lisCCOErrorCaption,
            Format(lisInvalidMacroTheMacroMustBeAPascalIdentifie, ['"', S, '"']),
            mtError,[mbCancel],0);
          exit;
        end;

        // check for prefix
        Prefix:=GetMacroNamePrefix(cbmpShort);
        if (Prefix<>'') and (SysUtils.CompareText(Prefix,copy(S,1,length(Prefix)))<>0)
        then  begin
          BetterName:=GetMacroNamePrefix(cbmpMedium)+S;
          DlgResult:=QuestionDlg(lisCCOWarningCaption,
            Format(lisTheMacroDoesNotBeginWith, [S, Prefix]),
            mtWarning, [mrCancel, mrYes, Format(lisRenameTo, [BetterName]),
              mrIgnore], 0);
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
            Format(lisInvalidMacroTheNameIsAKeyword, [S]),
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
        or (SysUtils.CompareText('SrcOS',S)=0)
        or (SysUtils.CompareText('SrcOS2',S)=0)
        or (SysUtils.CompareText('TargetCPU',S)=0)
        or (SysUtils.CompareText('LCLWidgetType',S)=0)
        then begin
          MessageDlg(lisCCOErrorCaption,
            Format(lisThereIsAlreadyAMacroWithTheName, ['"', S, '"']),
            mtError,[mbCancel],0);
          exit;
        end;

        // check for duplicates with used packages
        if (BuildMacros<>nil) and (BuildMacros.Owner is TBaseCompilerOptions) then
        begin
          Vars:=GetBuildMacroValues(TBaseCompilerOptions(BuildMacros.Owner),false);
          if (Vars<>nil) and Vars.IsDefined(PChar(S)) then begin
            DlgResult:=MessageDlg(lisCCOWarningCaption,
              Format(lisThereIsAlreadyAMacroWithTheName, ['"', S, '"']),
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
    if MessageDlg(lisConfirmDelete,
      Format(lisDeleteValue, ['"', aValue, '"']),
      mtConfirmation,[mbYes,mbCancel],0)<>mrYes
    then exit;
    i:=aBuildMacro.Values.IndexOf(aValue);
    if i>=0 then aBuildMacro.Values.Delete(i);
  end else begin
    if MessageDlg(lisConfirmDelete,
      Format(lisDeleteMacro, ['"', aBuildMacro.Identifier, '"']),
      mtConfirmation,[mbYes,mbCancel],0)<>mrYes
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

procedure TCompOptBuildMacrosFrame.SetBuildMacros(
  const AValue: TIDEBuildMacros);
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
  if (MacrosOwner=nil) then
    exit;
  if MacrosOwner is TPkgCompilerOptions then
  begin
    Result:=TPkgCompilerOptions(MacrosOwner).LazPackage.Name;
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
      BMDeleteMacroSpeedButton.Hint:=Format(lisDeleteMacro, ['"',aBuildMacro.Identifier,'"'])
    else
      BMDeleteMacroSpeedButton.Hint:=Format(lisDeleteValue2, [BuildMacrosTreeView.
        Selected.Text]);
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

procedure TCompOptBuildMacrosFrame.UpdateMessages;
begin
  fEngine.Variables.Assign(DefaultVariables);
  fEngine.Execute(CondSynEdit.Lines.Text,1);
  if fEngine.ErrorCount>0 then begin
    StatusMessage:=fEngine.GetErrorStr(0);
  end else begin
    StatusMessage:=lisNoErrors;
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

procedure TCompOptBuildMacrosFrame.StartCompletion;

  function EditorRowColumnToCompletionXY(ScreenRowCol: TPoint;
    AboveRow: boolean): TPoint;
  begin
    if not AboveRow then
      inc(ScreenRowCol.Y,1);
    Result:=CondSynEdit.RowColumnToPixels(ScreenRowCol);
    Result:=CondSynEdit.ClientToScreen(Result);
    if fSynCompletion.TheForm.Parent<>nil then
      Result:=fSynCompletion.TheForm.Parent.ScreenToClient(Result);
  end;

var
  LogStartX: integer;
  LogEndX: integer;
  LogXY: TPoint;
  ScreenXY: TPoint;
  XY: TPoint;
  Line: String;
begin
  //debugln(['TCompOptBuildMacrosFrame.StartCompletion ']);
  UpdateCompletionValues;
  fSynCompletion.ItemList.Assign(CompletionValues);

  // get row and column of word start at cursor
  LogXY:=CondSynEdit.LogicalCaretXY;
  CondSynEdit.GetWordBoundsAtRowCol(LogXY,LogStartX,LogEndX);
  LogEndX:=Min(LogEndX,LogXY.X);
  // convert text row,column to screen row,column
  ScreenXY:=CondSynEdit.PhysicalToLogicalPos(Point(LogStartX,LogXY.Y));
  // convert screen row,column to coordinates for the completion form
  XY:=EditorRowColumnToCompletionXY(ScreenXY,false);

  if XY.Y+fSynCompletion.TheForm.Height>fSynCompletion.TheForm.Parent.ClientHeight
  then begin
    // place completion above text
    XY:=EditorRowColumnToCompletionXY(ScreenXY,true);
    dec(XY.Y,fSynCompletion.TheForm.Height);
  end;

  // show completion box
  fSynCompletion.AddEditor(CondSynEdit);
  Line:=CondSynEdit.LineText;
  fSynCompletion.Execute(copy(Line,LogStartX,LogEndX-LogStartX),XY.X,XY.Y);
end;

procedure TCompOptBuildMacrosFrame.UpdateCompletionValues;

  function HasWord(const aName: string): Boolean;
  var
    i: Integer;
    s: string;
    p: LongInt;
  begin
    for i:=0 to CompletionValues.Count-1 do begin
      s:=CompletionValues[i];
      p:=System.Pos(#9,s);
      if p>0 then
        s:=copy(s,1,p-1);
      if SysUtils.CompareText(s,aName)=0 then exit(true);
    end;
    Result:=false;
  end;

  procedure AddVar(aName, aValue: string);
  var
    s: String;
  begin
    s:=dbgstr(aValue);
    if length(s)>50 then s:=copy(s,1,50)+'...';
    s:=aName+#9+aValue;
    CompletionValues.Add(s);
  end;

  procedure AddKeyword(aName: string);
  begin
    CompletionValues.Add(aName);
  end;

  procedure AddWord(aName: string);
  begin
    aName:=dbgstr(aName);
    if aName='' then exit;
    if HasWord(aName) then exit;
    CompletionValues.Add(aName);
  end;

var
  Node: TAVLTreeNode;
  V: PCTCfgScriptVariable;
  s: String;
  p: PChar;
  AtomStart: PChar;
  Macro: TLazBuildMacro;
  pcov: TParsedCompilerOptString;
  pcouv: TParsedCompilerOptString;
  i: Integer;
  j: Integer;
begin
  CompletionValues.Clear;

  // add default variables with values
  Node:=DefaultVariables.Tree.FindLowest;
  while Node<>nil do begin
    V:=PCTCfgScriptVariable(Node.Data);
    AddVar(V^.Name,GetCTCSVariableAsString(V));
    Node:=DefaultVariables.Tree.FindSuccessor(Node);
  end;

  // add keywords and operands
  AddKeyword('if');
  AddKeyword('then');
  AddKeyword('else');
  AddKeyword('begin');
  AddKeyword('end');
  AddKeyword('not');
  AddKeyword('and');
  AddKeyword('or');
  AddKeyword('xor');
  AddKeyword('undefine');
  AddKeyword('defined');
  AddKeyword('undefined');
  AddKeyword('integer');
  AddKeyword('int64');
  AddKeyword('string');
  AddKeyword('true');
  AddKeyword('false');

  // add result variables
  for pcov:=low(ParsedCompilerOptsVars) to high(ParsedCompilerOptsVars) do
    AddWord(ParsedCompilerOptsVars[pcov]);
  if FIsPackage then
    for pcouv:=low(ParsedCompilerOptsUsageVars) to high(ParsedCompilerOptsUsageVars) do
      AddWord(ParsedCompilerOptsUsageVars[pcouv]);

  // add build macros and values
  if BuildMacros<>nil then begin
    for i:=0 to BuildMacros.Count-1 do
    begin
      Macro:=BuildMacros[i];
      AddWord(Macro.Identifier);
      for j:=0 to Macro.Values.Count-1 do
        AddWord(Macro.Values[j]);
    end;
  end;

  // add words in text
  s:=CondSynEdit.Lines.Text;
  if s<>'' then begin
    p:=PChar(s);
    repeat
      AtomStart:=p;
      while (AtomStart^<>#0) and not IsIdentStartChar[AtomStart^] do
        inc(AtomStart);
      if (AtomStart^=#0) then break;
      p:=AtomStart;
      while IsIdentChar[p^] do inc(p);
      AddWord(copy(s,AtomStart-PChar(s)+1,p-AtomStart));
    until false;
  end;

  // sort alphabetically
  TStringList(FCompletionValues).Sort;

  // push recently used words upwards
  for i:=CompletionHistory.Count-1 downto 0 do begin
    j:=CompletionValues.IndexOf(CompletionHistory[i]);
    if j>0 then
      CompletionValues.Move(j,0);
  end;

  // set index
  for i:=0 to CompletionValues.Count-1 do
    CompletionValues.Objects[i]:=TObject({%H-}Pointer(i));

  //debugln(['TCompOptBuildMacrosFrame.UpdateCompletionValues ',CompletionValues.Text]);
end;

function TCompOptBuildMacrosFrame.GetCondCursorWord: string;
var
  XY: TPoint;
  StartX: integer;
  EndX: integer;
  Line: string;
begin
  XY:=CondSynEdit.LogicalCaretXY;
  if (XY.Y>=1) and (XY.Y<=CondSynEdit.Lines.Count) then
  begin
    CondSynEdit.GetWordBoundsAtRowCol(XY,StartX,EndX);
    //debugln(['TCompOptBuildMacrosFrame.GetCondCursorWord ',StartX,' ',EndX,' ',XY.X]);
    EndX:=Min(EndX,XY.X);
    Line := CondSynEdit.Lines[XY.Y - 1];
    Result:= Copy(Line, StartX, EndX - StartX);
  end else
    Result:='';
  //debugln(['TCompOptBuildMacrosFrame.GetCondCursorWord "',Result,'"']);
end;

constructor TCompOptBuildMacrosFrame.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  FCompletionValues:=TStringList.Create;
  FCompletionHistory:=TStringList.Create;
  fDefaultVariables:=TCTCfgScriptVariables.Create;
  FBuildMacros:=TIDEBuildMacros.Create(nil);
  fEngine:=TCTConfigScriptEngine.Create;

  MacrosGroupBox.Caption:=lisIDEMacros;
  BuildMacrosTreeView.Images := IDEImages.Images_24;
  fVarImgID:=IDEImages.LoadImage(24,'da_define');
  fValueImgID:=IDEImages.LoadImage(24,'da_define');
  fDefValueImgID:=IDEImages.LoadImage(24,'da_define');

  BuildMacroDefaultLabel.Caption:=
    lisHintADefaultValueCanBeDefinedInTheConditionals;
  BuildMacroDescriptionLabel.Caption:=lisCodeToolsDefsDescription;

  ConditionalsGroupBox.Caption:=lisConditionals;

  CondSynEdit.OnStatusChange:=@CondSynEditStatusChange;

  BMAddMacroSpeedButton.LoadGlyphFromLazarusResource('laz_add');
  BMAddMacroValueSpeedButton.LoadGlyphFromLazarusResource('laz_add');
  BMDeleteMacroSpeedButton.LoadGlyphFromLazarusResource('laz_delete');

  fSynCompletion:=TSynCompletion.Create(Self);
  fSynCompletion.TheForm.Parent:=Self;
  fSynCompletion.OnExecute:=@fSynCompletionExecute;
  fSynCompletion.OnCancel:=@fSynCompletionCancel;
  fSynCompletion.OnValidate:=@fSynCompletionValidate;
  fSynCompletion.OnSearchPosition:=@fSynCompletionSearchPosition;
  fSynCompletion.OnKeyCompletePrefix:=@fSynCompletionKeyCompletePrefix;
  fSynCompletion.OnUTF8KeyPress:=@fSynCompletionUTF8KeyPress;
  fSynCompletion.OnKeyNextChar:=@fSynCompletionKeyNextChar;
  fSynCompletion.OnKeyPrevChar:=@fSynCompletionKeyPrevChar;
  fSynCompletion.OnKeyDelete:=@fSynCompletionKeyDelete;
  fSynCompletion.OnKeyDown:=@fSynCompletionKeyDown;
end;

destructor TCompOptBuildMacrosFrame.Destroy;
begin
  FreeAndNil(FCompletionHistory);
  FreeAndNil(FCompletionValues);
  FreeAndNil(fDefaultVariables);
  FreeAndNil(fEngine);
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
  Result := TBaseCompilerOptions;
end;

procedure TCompOptBuildMacrosFrame.WriteSettings(AOptions: TAbstractIDEOptions);
begin
  if AOptions is TBaseCompilerOptions then
    SaveToOptions(TBaseCompilerOptions(AOptions));
end;

procedure TCompOptBuildMacrosFrame.LoadFromOptions(Options: TBaseCompilerOptions);
var
  Vars: TCTCfgScriptVariables;
begin
  FIsPackage:=Options is TPkgCompilerOptions;

  BuildMacros:=Options.BuildMacros as TIDEBuildMacros;
  Vars:=GetBuildMacroValues(Options,false);
  if Vars<>nil then
    DefaultVariables.Assign(Vars)
  else
    DefaultVariables.Clear;

  CondSynEdit.Lines.Text:=Options.Conditionals;
  EditorOpts.GetSynEditSettings(CondSynEdit);
  if FHighlighter=nil then
  begin
    FHighlighter := TPreviewPasSyn.Create(Self);
    CondSynEdit.Highlighter:=FHighlighter;
  end;
  EditorOpts.ReadHighlighterSettings(FHighlighter, '');
  UpdateStatusBar;
end;

procedure TCompOptBuildMacrosFrame.SaveToOptions(Options: TBaseCompilerOptions);
begin
  (Options.BuildMacros as TIDEBuildMacros).Assign(BuildMacros);
  Options.Conditionals:=CondSynEdit.Lines.Text;
end;

initialization
  RegisterIDEOptionsEditor(GroupCompiler, TCompOptBuildMacrosFrame,
    CompilerOptionsConditional);
  RegisterIDEOptionsEditor(GroupPkgCompiler, TCompOptBuildMacrosFrame,
    CompilerOptionsConditional);

end.

