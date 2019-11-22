{
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
    Dialog to explore IDE internal codetools structures.
}
unit CodeToolsDefPreview;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Math, Laz_AVL_Tree,
  // LCL
  LCLProc, Forms, Controls, Graphics, Dialogs, ButtonPanel, Buttons,
  StdCtrls, ComCtrls, ExtCtrls,
  // LazUtils
  LazFileUtils,
  // Codetools
  DefineTemplates, ExprEval,
  // SynEdit
  SynEdit,
  // IdeIntf
  IDEWindowIntf, IDEHelpIntf,
  // IDE
  EditorOptions, LazarusIDEStrConsts, InputHistory, CodeToolsOptions, IDEProcs, EnvironmentOpts;

type
  TCodeToolsDefinesNodeValues = class
  public
    Node: TDefineTemplate;
    ValueParsed: boolean;
    ParsedValue: string;
    ExpressionCalculated: boolean;
    ExpressionResult: string;
    Execute: boolean;
  end;

  { TCodeToolsDefinesDialog }

  TCodeToolsDefinesDialog = class(TForm)
    ButtonPanel: TButtonPanel;
    DirectoryBrowseButton: TButton;
    DirectoryCombobox: TComboBox;
    DirectoryGroupbox: TGroupBox;
    PageControl1: TPageControl;
    ReportMemo: TMemo;
    Splitter1: TSplitter;
    ValuesTabSheet: TTabSheet;
    ReportTabSheet: TTabSheet;
    TemplatesMemo: TMemo;
    TemplatesSplitter: TSplitter;
    TemplatesGroupBox: TGroupBox;
    MainSplitter: TSplitter;
    ParsedTemplatesTreeView: TTreeView;
    ValueSynedit: TSynEdit;
    ValueGroupbox: TGroupBox;
    ValuesListview: TListView;
    procedure CodeToolsDefinesDialogCLOSE(Sender: TObject;
      var {%H-}CloseAction: TCloseAction);
    procedure CodeToolsDefinesDialogCREATE(Sender: TObject);
    procedure DirectoryBrowseButtonCLICK(Sender: TObject);
    procedure DirectoryComboboxCHANGE(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure HelpButtonClick(Sender: TObject);
    procedure ParsedTemplatesTreeViewSelectionChanged(Sender: TObject);
    procedure ValuesListviewSELECTITEM(Sender: TObject; {%H-}Item: TListItem;
      {%H-}Selected: Boolean);
  private
    FDefineTree: TDefineTree;
    FNodeValues: TAvlTree;
    fReport: TStringList;
    procedure SetDefineTree(const AValue: TDefineTree);
    procedure UpdateValues;
    procedure UpdateValue;
    procedure UpdateTemplateValues;
    procedure ClearValues;
    procedure FillTemplateTree;
    procedure SetComboBox(AComboBox: TComboBox; const NewText: string);
    procedure DefineTreeCalculate({%H-}DefineTree: TDefineTree; Node: TDefineTemplate;
                  ValueParsed: boolean; const ParsedValue: string;
                  ExpressionCalculated: boolean; const ExpressionResult: string;
                  Execute: boolean);
  public
    property DefineTree: TDefineTree read FDefineTree write SetDefineTree;
  end;


function ShowCodeToolsDefinesValuesDialog(ADefineTree: TDefineTree;
  const InitialDirectory: string): TModalresult;
  
procedure RebuildDefineTreeView(ATreeView: TTreeView;
  RootTemplate: TDefineTemplate);
procedure AddDefineNodes(ATreeView: TTreeView; ANode: TDefineTemplate;
  AParent: TTreeNode; WithChilds,WithNextSiblings: boolean);
procedure SetNodeImages(ANode: TTreeNode; WithSubNodes: boolean);

function CompareNodeValues(Data1, Data2: Pointer): Integer;
function CompareNodeAndNodeValues(Node, NodeValues: Pointer): Integer;

implementation

{$R *.lfm}

function ShowCodeToolsDefinesValuesDialog(ADefineTree: TDefineTree;
  const InitialDirectory: string): TModalresult;
var
  CodeToolsDefinesDialog: TCodeToolsDefinesDialog;
begin
  CodeToolsDefinesDialog:=TCodeToolsDefinesDialog.Create(nil);
  if InitialDirectory<>'' then
    CodeToolsDefinesDialog.SetComboBox(CodeToolsDefinesDialog.DirectoryCombobox,
                                       InitialDirectory);
  CodeToolsDefinesDialog.DefineTree:=ADefineTree;
  Result:=CodeToolsDefinesDialog.ShowModal;
  CodeToolsDefinesDialog.Free;
end;

procedure RebuildDefineTreeView(ATreeView: TTreeView;
  RootTemplate: TDefineTemplate);
begin
  ATreeView.Items.BeginUpdate;
  ATreeView.Items.Clear;
  AddDefineNodes(ATreeView,RootTemplate,nil,true,true);
  ATreeView.Items.EndUpdate;
end;

procedure AddDefineNodes(
  ATreeView: TTreeView; ANode: TDefineTemplate; AParent: TTreeNode;
  WithChilds, WithNextSiblings: boolean);
var NewTreeNode: TTreeNode;
begin
  if ANode=nil then exit;
  ATreeView.Items.BeginUpdate;
  NewTreeNode:=ATreeView.Items.AddChildObject(AParent,ANode.Name,ANode);
  SetNodeImages(NewTreeNode,false);
  if WithChilds and (ANode.FirstChild<>nil) then begin
    AddDefineNodes(ATreeView,ANode.FirstChild,NewTreeNode,true,true);
  end;
  if WithNextSiblings and (ANode.Next<>nil) then begin
    AddDefineNodes(ATreeView,ANode.Next,AParent,WithChilds,true);
  end;
  ATreeView.Items.EndUpdate;
end;

procedure SetNodeImages(ANode: TTreeNode;
  WithSubNodes: boolean);
var
  ADefineTemplate: TDefineTemplate;
begin
  ADefineTemplate := TDefineTemplate(ANode.Data);

  ANode.ImageIndex := DefineActionImages[ADefineTemplate.Action];
  ANode.SelectedIndex := ANode.ImageIndex;

  if ADefineTemplate.IsAutoGenerated then
    ANode.StateIndex := AutogeneratedImage
  else
    ANode.StateIndex := -1;

  if WithSubNodes then
  begin
    ANode:=ANode.GetFirstChild;
    while ANode<>nil do
    begin
      SetNodeImages(ANode,true);
      ANode:=ANode.GetNextSibling;
    end;
  end;
end;

function CompareNodeValues(Data1, Data2: Pointer): Integer;
begin
  Result:=ComparePointers(TCodeToolsDefinesNodeValues(Data1).Node,
                          TCodeToolsDefinesNodeValues(Data2).Node);
end;

function CompareNodeAndNodeValues(Node, NodeValues: Pointer): Integer;
begin
  Result:=ComparePointers(TCodeToolsDefinesNodeValues(Node),
                          TCodeToolsDefinesNodeValues(NodeValues).Node);
end;

{ TCodeToolsDefinesDialog }

procedure TCodeToolsDefinesDialog.FormDestroy(Sender: TObject);
begin
  if FNodeValues<>nil then begin
    FNodeValues.FreeAndClear;
    FreeAndNil(FNodeValues);
  end;
end;

procedure TCodeToolsDefinesDialog.HelpButtonClick(Sender: TObject);
begin
  LazarusHelp.ShowHelpForIDEControl(Self);
end;

procedure TCodeToolsDefinesDialog.ParsedTemplatesTreeViewSelectionChanged(
  Sender: TObject);
begin
  UpdateTemplateValues;
end;

procedure TCodeToolsDefinesDialog.ValuesListviewSELECTITEM(Sender: TObject;
  Item: TListItem; Selected: Boolean);
begin
  UpdateValue;
end;

procedure TCodeToolsDefinesDialog.SetDefineTree(const AValue: TDefineTree);
begin
  if FDefineTree=AValue then exit;
  FDefineTree:=AValue;
  UpdateValues;
  FillTemplateTree;
end;

procedure TCodeToolsDefinesDialog.UpdateValues;
// let the codetools calculate the defines for the directory

  procedure AddNodeReport(Prefix: string; DefTempl: TDefineTemplate);
  var
    AVLNode: TAvlTreeNode;
    NodeValues: TCodeToolsDefinesNodeValues;
    s: String;
  begin
    while DefTempl<>nil do begin
      s:=Prefix+'Name="'+DefTempl.Name+'"';
      s:=s+' Description="'+DefTempl.Description+'"';
      s:=s+' Action="'+DefineActionNames[DefTempl.Action]+'"';
      s:=s+' Variable="'+DefTempl.Variable+'"';
      s:=s+' Value="'+dbgstr(DefTempl.Value)+'"';
      if FNodeValues<>nil then begin
        AVLNode:=FNodeValues.FindKey(DefTempl,@CompareNodeAndNodeValues);
        if AVLNode<>nil then begin
          NodeValues:=TCodeToolsDefinesNodeValues(AVLNode.Data);
          if NodeValues.ValueParsed then
            s:=s+' ParsedValue="'+dbgstr(NodeValues.ParsedValue)+'"';
          if NodeValues.ExpressionCalculated then
            s:=s+' ExpressionResult="'+dbgstr(NodeValues.ExpressionResult)+'"';
          s:=s+' Executed="'+dbgs(NodeValues.Execute)+'"';
        end;
      end;
      fReport.Add(s);
      if DefTempl.FirstChild<>nil then
        AddNodeReport(Prefix+'    ',DefTempl.FirstChild);
      DefTempl:=DefTempl.Next;
    end;
  end;

var
  Dir: String;
  Defines: TExpressionEvaluator;
  i: Integer;
  ListItem: TListItem;
  Value: String;
  OldOnCalculate: TDefTreeCalculate;
begin
  Dir:=TrimFilename(DirectoryCombobox.Text);
  if (DefineTree=nil) or (not FilenameIsAbsolute(Dir))
  or (not DirPathExists(Dir)) then begin
    ClearValues;
    exit;
  end;

  // set our debug function, clear codetools cache and calculate the values
  if FNodeValues<>nil then
    FNodeValues.FreeAndClear;
  DefineTree.ClearCache;// make sure the defines are reparsed

  fReport:=TStringList.Create;
  try
    fReport.Add('Directory: '+Dir);

    OldOnCalculate:=DefineTree.OnCalculate;
    DefineTree.OnCalculate:=@DefineTreeCalculate;
    try
      Defines:=DefineTree.GetDefinesForDirectory(Dir,false);
    finally
      DefineTree.OnCalculate:=OldOnCalculate;
    end;

    // fill the ValuesListview
    ValuesListview.BeginUpdate;
    if Defines<>nil then begin
      fReport.Add('Defines:');
      for i:=0 to Defines.Count-1 do begin
        if ValuesListview.Items.Count<=i then
          ListItem:=ValuesListview.Items.Add
        else
          ListItem:=ValuesListview.Items[i];
        fReport.Add(Defines.Names(i)+'='+dbgstr(Defines.Values(i)));
        ListItem.Caption:=Defines.Names(i);
        Value:=Defines.Values(i);
        if length(Value)>100 then
          Value:=copy(Value,1,100)+' ...';
        if ListItem.SubItems.Count<1 then
          ListItem.SubItems.Add(Value)
        else
          ListItem.SubItems[0]:=Value;
      end;
      fReport.Add('');
      while ValuesListview.Items.Count>Defines.Count do
        ValuesListview.Items.Delete(ValuesListview.Items.Count-1);
    end else begin
      ValuesListview.Items.Clear;
    end;
    ValuesListview.EndUpdate;
    UpdateValue;

    // add all nodes to report
    fReport.Add('Tree:');
    AddNodeReport('  ',DefineTree.RootTemplate);

    ReportMemo.Lines.Assign(fReport);
  finally
    FreeAndNil(fReport);
  end;
end;

procedure TCodeToolsDefinesDialog.UpdateValue;
var
  VariableName: String;
  Dir: String;
  Defines: TExpressionEvaluator;
  Value: string;
begin
  Dir:=TrimFilename(DirectoryCombobox.Text);
  if (ValuesListview.Selected=nil) or (DefineTree=nil)
  or (not FilenameIsAbsolute(Dir)) then begin
    ValueGroupbox.Caption:=lisCTDefnoVariableSelected;
    ValueSynedit.Lines.Text:='';
  end else begin
    VariableName:=ValuesListview.Selected.Caption;
    ValueGroupbox.Caption:=Format(lisCTDefVariable, [VariableName]);
    Defines:=DefineTree.GetDefinesForDirectory(Dir,false);
    Value:=Defines.Variables[VariableName];
    ValueSynedit.Lines.Text:=Value;
  end;
end;

procedure TCodeToolsDefinesDialog.UpdateTemplateValues;
var
  SelTreeNode: TTreeNode;
  SelDefNode: TDefineTemplate;
  s: string;
  AVLNode: TAvlTreeNode;
  NodeValues: TCodeToolsDefinesNodeValues;
begin
  SelTreeNode:=ParsedTemplatesTreeView.Selected;
  if SelTreeNode=nil then begin
    TemplatesMemo.Text:='No node selected';
  end else begin
    SelDefNode:=TDefineTemplate(SelTreeNode.Data);
    s:='Name="'+SelDefNode.Name+'"'+LineEnding;
    s:=s+'Description="'+SelDefNode.Description+'"'+LineEnding;
    s:=s+'Action="'+DefineActionNames[SelDefNode.Action]+'"'+LineEnding;
    s:=s+'Variable="'+SelDefNode.Variable+'"'+LineEnding;
    s:=s+'Value="'+SelDefNode.Value+'"'+LineEnding;

    if FNodeValues<>nil then begin
      AVLNode:=FNodeValues.FindKey(SelDefNode,@CompareNodeAndNodeValues);
      if AVLNode<>nil then begin
        NodeValues:=TCodeToolsDefinesNodeValues(AVLNode.Data);
        if NodeValues.ValueParsed then
          s:=s+'Parsed Value="'+NodeValues.ParsedValue+'"'+LineEnding;
        if NodeValues.ExpressionCalculated then
          s:=s+'Expression Result="'+NodeValues.ExpressionResult+'"'+LineEnding;
        s:=s+'Executed="'+dbgs(NodeValues.Execute)+'"'+LineEnding;
      end;
    end;
    TemplatesMemo.Text:=s;
  end;
end;

procedure TCodeToolsDefinesDialog.ClearValues;
begin
  ValuesListview.Items.Clear;
  if FNodeValues<>nil then
    FNodeValues.FreeAndClear;
end;

procedure TCodeToolsDefinesDialog.FillTemplateTree;
begin
  RebuildDefineTreeView(ParsedTemplatesTreeView,DefineTree.RootTemplate);
  UpdateTemplateValues;
end;

procedure TCodeToolsDefinesDialog.SetComboBox(AComboBox: TComboBox;
  const NewText: string);
var
  i: Integer;
begin
  i:=AComboBox.Items.IndexOf(NewText);
  if i<0 then
    AComboBox.Items.Add(NewText)
  else
    AComboBox.ItemIndex:=i;
  AComboBox.Text:=NewText;
  //debugln('TCodeToolsDefinesDialog.SetComboBox Text=',AComboBox.Text,' NewText=',NewText);
end;

procedure TCodeToolsDefinesDialog.DefineTreeCalculate(DefineTree: TDefineTree;
  Node: TDefineTemplate; ValueParsed: boolean; const ParsedValue: string;
  ExpressionCalculated: boolean; const ExpressionResult: string;
  Execute: boolean);
var
  NewNodeValues: TCodeToolsDefinesNodeValues;
begin
  NewNodeValues:=TCodeToolsDefinesNodeValues.Create;
  NewNodeValues.Node:=Node;
  NewNodeValues.ValueParsed:=ValueParsed;
  NewNodeValues.ParsedValue:=ParsedValue;
  NewNodeValues.ExpressionCalculated:=ExpressionCalculated;
  NewNodeValues.ExpressionResult:=ExpressionResult;
  NewNodeValues.Execute:=Execute;
  if FNodeValues=nil then
    FNodeValues:=TAvlTree.Create(@CompareNodeValues);
  FNodeValues.Add(NewNodeValues);
end;

procedure TCodeToolsDefinesDialog.CodeToolsDefinesDialogCREATE(Sender: TObject);
var
  ListColumn: TListColumn;
begin
  IDEDialogLayoutList.ApplyLayout(Self,485,450);

  Caption:=lisCTDefCodeToolsDirectoryValues;

  ValuesTabSheet.Caption:=lisValues;

  ListColumn:=ValuesListview.Columns.Add;
  ListColumn.Caption:=lisCTDefVariableName;
  ListColumn.Width:=150;
  ListColumn:=ValuesListview.Columns.Add;
  ListColumn.Caption:=lisValue;
  
  DirectoryGroupbox.Caption:=lisCodeToolsDefsInsertBehindDirectory;
  ButtonPanel.HelpButton.OnClick := @HelpButtonClick;
  DirectoryCombobox.Items.Assign(
    InputHistories.HistoryLists.GetList(hlCodeToolsDirectories,true,rltFile));
  if DirectoryCombobox.Items.Count>0 then
    DirectoryCombobox.ItemIndex:=0
  else
    DirectoryCombobox.Text:='';
    
  TemplatesGroupBox.Caption:=lisCTDefDefineTemplates;
    
  MainSplitter.SetSplitterPosition(
         Max(20,Min(ClientWidth-100,CodeToolsOpts.DefinesPreviewMainSplitterPos)));
  TemplatesSplitter.SetSplitterPosition(
         Max(20,Min(TemplatesGroupBox.ClientHeight-50,
                    CodeToolsOpts.DefinesPreviewTemplSplitterPos)));

  EditorOpts.GetSynEditSettings(ValueSynedit);
  ValueSynedit.Gutter.Visible:=false;

  ReportTabSheet.Caption:=dlgReport;
  PageControl1.PageIndex:=0;
  DirectoryCombobox.DropDownCount:=EnvironmentOptions.DropDownCount;
end;

procedure TCodeToolsDefinesDialog.CodeToolsDefinesDialogCLOSE(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  IDEDialogLayoutList.SaveLayout(Self);
  InputHistories.HistoryLists.GetList(hlCodeToolsDirectories,true,rltFile).Assign(
    DirectoryCombobox.Items);
  CodeToolsOpts.DefinesPreviewMainSplitterPos:=MainSplitter.GetSplitterPosition;
  CodeToolsOpts.DefinesPreviewTemplSplitterPos:=TemplatesSplitter.GetSplitterPosition;
  CodeToolsOpts.Save;
end;

procedure TCodeToolsDefinesDialog.DirectoryBrowseButtonCLICK(Sender: TObject);
var
  OpenDialog: TOpenDialog;
  Filename: string;
begin
  OpenDialog:=TSelectDirectoryDialog.Create(nil);
  try
    InputHistories.ApplyFileDialogSettings(OpenDialog);
    OpenDialog.Title:=lisCTDefChooseDirectory;
    OpenDialog.Options:=OpenDialog.Options+[ofPathMustExist];
    if OpenDialog.Execute then begin
      Filename:=CleanAndExpandFilename(OpenDialog.Filename);
      SetComboBox(DirectoryCombobox,Filename);
      UpdateValues;
    end;
    InputHistories.StoreFileDialogSettings(OpenDialog);
  finally
    OpenDialog.Free;
  end;
end;

procedure TCodeToolsDefinesDialog.DirectoryComboboxCHANGE(Sender: TObject);
begin
  UpdateValues;
end;

end.

