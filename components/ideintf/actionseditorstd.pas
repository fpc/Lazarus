{ Copyright (C) 2005

 *****************************************************************************
 *                                                                           *
 *  See the file COPYING.modifiedLGPL.txt, included in this distribution,    *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************


  part of the ActionList Editor

  author:
     Pawel Piwowar, alfapawel@tlen.pl
     
  version:
    0.1 - 10.03.2005 - added to ActionList Editor
    0.2 - 14.03.2005 - headline, hint and shortcut descriptions
    
  ToDo: - multiselect for actions
        - standard icon
}
unit ActionsEditorStd;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ComCtrls, Buttons, ActnList, StdActns, DBActns, LCLType, Contnrs,
  LCLProc, ActionsEditor, ObjInspStrConsts, ExtCtrls, ButtonPanel;

type

  { TFormActStandard }

  TFormActStandard = class(TForm)
    LabelHeadLine: TLabel;
    tvActStdList: TTreeView;
    BtnPanel: TButtonPanel;
    procedure FormActStandardClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure btnOKClick(Sender: TObject);
    procedure tvActStdListDblClick(Sender: TObject);
  private
    FResultActionProc: TResultActProc;
    FActStdProperty: TActStdProp;
    procedure EnumAct;
    procedure ResultActionProc;
    procedure AddStdActProperties;
  public
    constructor Create(AOwner: TComponent); override;
    constructor CreateEx(AOwner: TComponent; ResultActProc: TResultActProc);
    destructor Destroy; override;
  end;

implementation

{$R *.lfm}

{ TFormActStandard }

procedure TFormActStandard.FormActStandardClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  CloseAction := caFree;
end;

procedure TFormActStandard.btnOKClick(Sender: TObject);
begin
  if Assigned(FResultActionProc) and not tvActStdList.Selected.HasChildren then
    ResultActionProc;
end;

procedure TFormActStandard.tvActStdListDblClick(Sender: TObject);
var
  node: TTreeNode;
  MyHitTest : THitTests;
  mousePoint: TPoint;
begin
  mousePoint := TTreeView(Sender).ScreenToClient(Mouse.CursorPos);
  node := TTreeView(Sender).Selected;
  if Assigned(node) then
  begin
    MyHitTest := TTreeView(Sender).GetHitTestInfoAt(mousePoint.X, mousePoint.Y );
    if (htOnItem in MyHitTest) or (htOnLabel in MyHitTest) then
    begin
      if (not node.HasChildren) and (node.Parent is TTreeNode) then
        BtnPanel.OKButton.Click
{      else begin
        if node.Expanded
        then node.Collapse(False)
        else node.Expand(False);
      end;  }
    end;
  end;
end;

procedure TFormActStandard.EnumAct;
var
  outer, inner: Integer;
  NodeCategory: TTreeNode;
begin
  tvActStdList.BeginUpdate;
  for outer := 0 to RegisteredActions.Count-1 do
  begin
    with tvActStdList.Items do
    begin
      NodeCategory := Add(nil, RegisteredActions.Items[outer].Name);
      for inner := 0 to RegisteredActions.Items[outer].Count-1 do
        AddChild(NodeCategory, RegisteredActions.Items[outer].Items[inner].ActionClass.ClassName);
    end;  // with
  end;  // for outer
  tvActStdList.EndUpdate;

  tvActStdList.Selected := tvActStdList.Items[0].Items[0];
end;

procedure TFormActStandard.ResultActionProc;
var
  Category: String;
  lastItem: Boolean;  // for multiselect, but now the multiselect property is not implemented in the TTreeView
  fClass: TBasicActionClass;
begin
  Category := tvActStdList.Selected.Parent.Text;
  lastItem := True;
  fClass := RegisteredActions.Items[RegisteredActions.IndexOfCategory(Category)].Items[tvActStdList.Selected.Index].ActionClass;
  FResultActionProc(Category, fClass, FActStdProperty.IndexOfClass(fClass.ClassName), lastItem);
end;

procedure TFormActStandard.AddStdActProperties;
begin
//ActStdResource.Add(TEditCutResource);
  FActStdProperty.Add(TEditCut, oiStdActEditCutHeadLine, oiStdActEditCutShortCut, oiStdActEditCutShortHint);
  FActStdProperty.Add(TEditCopy, oiStdActEditCopyHeadLine, oiStdActEditCopyShortCut, oiStdActEditCopyShortHint);
  FActStdProperty.Add(TEditPaste, oiStdActEditPasteHeadLine, oiStdActEditPasteShortCut, oiStdActEditPasteShortHint);
  FActStdProperty.Add(TEditSelectAll, oiStdActEditSelectAllHeadLine, oiStdActEditSelectAllShortCut, oiStdActEditSelectAllShortHint);
  FActStdProperty.Add(TEditUndo, oiStdActEditUndoHeadLine, oiStdActEditUndoShortCut, oiStdActEditUndoShortHint);
  FActStdProperty.Add(TEditDelete, oiStdActEditDeleteHeadLine, oiStdActEditDeleteShortCut, oiStdActEditDeleteShortHint);

  FActStdProperty.Add(TSearchFind, oiStdActSearchFindHeadLine, oiStdActSearchFindShortCut, '');
  FActStdProperty.Add(TSearchFindFirst, oiStdActSearchFindFirstHeadLine, '', '');
  FActStdProperty.Add(TSearchFindNext, oiStdActSearchFindNextHeadLine, oiStdActSearchFindNextShortCut, '');
  FActStdProperty.Add(TSearchReplace, oiStdActSearchReplaceHeadLine, '', '');
  
  FActStdProperty.Add(THelpContents, oiStdActHelpContentsHeadLine, '', oiStdActHelpContentsHint);
  FActStdProperty.Add(THelpTopicSearch, oiStdActHelpTopicSearchHeadLine, '', oiStdActHelpTopicSearchHint);
  FActStdProperty.Add(THelpOnHelp, oiStdActHelpHelpHelpHeadLine, '', oiStdActHelpHelpHelpHint);

  FActStdProperty.Add(TFileOpen, oiStdActFileOpenHeadLine, oiStdActFileOpenShortCut, oiStdActFileOpenHint);
  FActStdProperty.Add(TFileOpenWith, oiStdActFileOpenWithHeadLine, '', oiStdActFileOpenHint);
  FActStdProperty.Add(TFileSaveAs, oiStdActFileSaveAsHeadLine, '', oiStdActFileSaveAsHint);
  FActStdProperty.Add(TFileExit, oiStdActFileExitHeadLine, '', oiStdActFileExitHint);

  FActStdProperty.Add(TColorSelect, oiStdActColorSelect1HeadLine, '', oiStdActColorSelectHint);
  FActStdProperty.Add(TFontEdit, oiStdActFontEditHeadLine, '', oiStdActFontEditHint);

  FActStdProperty.Add(TDataSetFirst, oiStdActDataSetFirstHeadLine, '', oiStdActDataSetFirstHint);
  FActStdProperty.Add(TDataSetPrior, oiStdActDataSetPriorHeadLine, '', oiStdActDataSetPriorHint);
  FActStdProperty.Add(TDataSetNext, oiStdActDataSetNextHeadLine, '', oiStdActDataSetNextHint);
  FActStdProperty.Add(TDataSetLast, oiStdActDataSetLastHeadLine, '', oiStdActDataSetLastHint);
  FActStdProperty.Add(TDataSetInsert, oiStdActDataSetInsertHeadLine, '', oiStdActDataSetInsertHint);
  FActStdProperty.Add(TDataSetDelete, oiStdActDataSetDeleteHeadLine, '', oiStdActDataSetDeleteHint);
  FActStdProperty.Add(TDataSetEdit, oiStdActDataSetEditHeadLine, '', oiStdActDataSetEditHint);
  FActStdProperty.Add(TDataSetPost, oiStdActDataSetPostHeadLine, '', oiStdActDataSetPostHint);
  FActStdProperty.Add(TDataSetCancel, oiStdActDataSetCancelHeadLine, '', oiStdActDataSetCancel1Hint);
  FActStdProperty.Add(TDataSetRefresh, oiStdActDataSetRefreshHeadLine, '', oiStdActDataSetRefreshHint);
end;

constructor TFormActStandard.Create(AOwner: TComponent);
begin
  CreateEx(AOwner, nil);
end;

constructor TFormActStandard.CreateEx(AOwner: TComponent; ResultActProc: TResultActProc);
begin
  inherited Create(AOwner);
  FResultActionProc := ResultActProc;
  Caption := OisStdActionListEditor;
  LabelHeadLine.Caption := oisStdActionListEditorClass;
  BtnPanel.OKButton.OnClick := @btnOKClick;
  EnumAct;
  FActStdProperty := TActStdProp.Create;
  AddStdActProperties;
end;

destructor TFormActStandard.Destroy;
begin
  FActStdProperty.Free;
  inherited Destroy;
end;

procedure CreateFormActStandard(AOwner: TComponent;
  ResultActProc: TResultActProc; out Form: TForm);
begin
  Form := TFormActStandard.CreateEx(AOwner, ResultActProc);
end;

initialization
  CreateDlgStdActions:=@CreateFormActStandard;

end.

