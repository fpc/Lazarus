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
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ComCtrls, Buttons, ActnList, StdActns, DBActns, LCLType, Contnrs,
  LCLProc, ActionsEditor, ObjInspStrConsts, ExtCtrls;

type

  { TFormActStandard }

  TFormActStandard = class(TForm)
    btnCancel: TBitBtn;
    btnOK: TBitBtn;
    LabelHeadLine: TLabel;
    BtnPanel: TPanel;
    tvActStdList: TTreeView;
    procedure FormActStandardClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormActStandardKeyPress(Sender: TObject; var Key: char);
    procedure btnOKClick(Sender: TObject);
    procedure tvActStdListDblClick(Sender: TObject);
  private
    FResultActionProc: TResultActProc;
    fActStdProperty: TActStdProp;
    procedure EnumAct;
    procedure ResultActionProc;
    procedure AddStdActProperties;
  public
    constructor Create(AOwner: TComponent); override;
    constructor CreateEx(AOwner: TComponent; ResultActProc: TResultActProc);
    destructor Destroy; override;
  end;

implementation

{ TFormActStandard }

procedure TFormActStandard.FormActStandardClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  CloseAction := caFree;
end;

procedure TFormActStandard.FormActStandardKeyPress(Sender: TObject; var Key: char);
begin
  if Ord(Key) = VK_ESCAPE then Self.Close;
end;

procedure TFormActStandard.btnOKClick(Sender: TObject);
begin
  if Assigned(FResultActionProc)
     and not tvActStdList.Selected.HasChildren
  then ResultActionProc;
end;

procedure TFormActStandard.tvActStdListDblClick(Sender: TObject);
var
  node: TTreeNode;
  MyHitTest : THitTests;
  mousePoint: TPoint;
begin
  mousePoint := TTreeView(Sender).ScreenToClient(Mouse.CursorPos);
  node := TTreeView(Sender).Selected;
  if Assigned(node) then begin
    MyHitTest := TTreeView(Sender).GetHitTestInfoAt(mousePoint.X, mousePoint.Y );
    if (htOnItem in MyHitTest)
       or (htOnLabel in MyHitTest) then begin
      if (not node.HasChildren) and (node.Parent is TTreeNode)
      then btnOK.Click
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
  for outer := 0 to RegisteredActions.Count-1 do begin
    with tvActStdList.Items do begin
      NodeCategory := Add(nil, RegisteredActions.Items[outer].Name);
      for inner := 0 to RegisteredActions.Items[outer].Count-1
      do AddChild(NodeCategory, RegisteredActions.Items[outer].Items[inner].ActionClass.ClassName);
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
  FResultActionProc(Category, fClass, fActStdProperty.IndexOfClass(fClass.ClassName), lastItem);
end;

procedure TFormActStandard.AddStdActProperties;
begin
//ActStdResource.Add(TEditCutResource);
  fActStdProperty.Add(TEditCut, oiStdActEditCutHeadLine, oiStdActEditCutShortCut, oiStdActEditCutShortHint);
  fActStdProperty.Add(TEditCopy, oiStdActEditCopyHeadLine, oiStdActEditCopyShortCut, oiStdActEditCopyShortHint);
  fActStdProperty.Add(TEditPaste, oiStdActEditPasteHeadLine, oiStdActEditPasteShortCut, oiStdActEditPasteShortHint);
  fActStdProperty.Add(TEditSelectAll, oiStdActEditSelectAllHeadLine, oiStdActEditSelectAllShortCut, oiStdActEditSelectAllShortHint);
  fActStdProperty.Add(TEditUndo, oiStdActEditUndoHeadLine, oiStdActEditUndoShortCut, oiStdActEditUndoShortHint);
  fActStdProperty.Add(TEditDelete, oiStdActEditDeleteHeadLine, oiStdActEditDeleteShortCut, oiStdActEditDeleteShortHint);

  fActStdProperty.Add(TSearchFind, oiStdActSearchFindHeadLine, oiStdActSearchFindShortCut, '');
  fActStdProperty.Add(TSearchFindFirst, oiStdActSearchFindFirstHeadLine, '', '');
  fActStdProperty.Add(TSearchFindNext, oiStdActSearchFindNextHeadLine, oiStdActSearchFindNextShortCut, '');
  fActStdProperty.Add(TSearchReplace, oiStdActSearchReplaceHeadLine, '', '');
  
  fActStdProperty.Add(THelpContents, oiStdActHelpContentsHeadLine, '', oiStdActHelpContentsHint);
  fActStdProperty.Add(THelpTopicSearch, oiStdActHelpTopicSearchHeadLine, '', oiStdActHelpTopicSearchHint);
  fActStdProperty.Add(THelpOnHelp, oiStdActHelpHelpHelpHeadLine, '', oiStdActHelpHelpHelpHint);

  fActStdProperty.Add(TFileOpen, oiStdActFileOpenHeadLine, oiStdActFileOpenShortCut, oiStdActFileOpenHint);
  fActStdProperty.Add(TFileOpenWith, oiStdActFileOpenWithHeadLine, '', oiStdActFileOpenHint);
  fActStdProperty.Add(TFileSaveAs, oiStdActFileSaveAsHeadLine, '', oiStdActFileSaveAsHint);
  fActStdProperty.Add(TFileExit, oiStdActFileExitHeadLine, '', oiStdActFileExitHint);

  fActStdProperty.Add(TColorSelect, oiStdActColorSelect1HeadLine, '', oiStdActColorSelectHint);
  fActStdProperty.Add(TFontEdit, oiStdActFontEditHeadLine, '', oiStdActFontEditHint);

  fActStdProperty.Add(TDataSetFirst, oiStdActDataSetFirstHeadLine, '', oiStdActDataSetFirstHint);
  fActStdProperty.Add(TDataSetPrior, oiStdActDataSetPriorHeadLine, '', oiStdActDataSetPriorHint);
  fActStdProperty.Add(TDataSetNext, oiStdActDataSetNextHeadLine, '', oiStdActDataSetNextHint);
  fActStdProperty.Add(TDataSetLast, oiStdActDataSetLastHeadLine, '', oiStdActDataSetLastHint);
  fActStdProperty.Add(TDataSetInsert, oiStdActDataSetInsertHeadLine, '', oiStdActDataSetInsertHint);
  fActStdProperty.Add(TDataSetDelete, oiStdActDataSetDeleteHeadLine, '', oiStdActDataSetDeleteHint);
  fActStdProperty.Add(TDataSetEdit, oiStdActDataSetEditHeadLine, '', oiStdActDataSetEditHint);
  fActStdProperty.Add(TDataSetPost, oiStdActDataSetPostHeadLine, '', oiStdActDataSetPostHint);
  fActStdProperty.Add(TDataSetCancel, oiStdActDataSetCancelHeadLine, '', oiStdActDataSetCancel1Hint);
  fActStdProperty.Add(TDataSetRefresh, oiStdActDataSetRefreshHeadLine, '', oiStdActDataSetRefreshHint);
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
  EnumAct;
  fActStdProperty := TActStdProp.Create;
  AddStdActProperties;
end;

destructor TFormActStandard.Destroy;
begin
  fActStdProperty.Free;
  inherited Destroy;
end;

procedure CreateFormActStandard(AOwner: TComponent;
  ResultActProc: TResultActProc; out Form: TForm);
begin
  Form:=TFormActStandard.CreateEx(AOwner,ResultActProc);
end;

initialization
  {$I actionseditorstd.lrs}
  CreateDlgStdActions:=@CreateFormActStandard;

end.

