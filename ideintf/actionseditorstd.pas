{ Copyright (C) 2005

 *****************************************************************************
 *                                                                           *
 *  See the file COPYING.modifiedLGPL, included in this distribution,        *
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
unit actionseditorstd;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ComCtrls, Buttons, ActnList, StdActns, DBActns, LCLType, Contnrs,
  LCLProc;

type
  TActStdPropItem = class;
  TActStdProp = class;
  TResultActProc = procedure (const Category: string; ActionClass: TBasicActionClass; ActionProperty: TActStdPropItem; LastItem: Boolean) of object;

  { TFormActStandard }

  TFormActStandard = class(TForm)
    btnCancel: TBitBtn;
    btnOK: TBitBtn;
    LabelHeadLine: TLabel;
    tvActStdList: TTreeView;
    procedure FormActStandardClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormActStandardKeyPress(Sender: TObject; var Key: char);
    procedure btnOKClick(Sender: TObject);
    procedure tvActStdListDblClick(Sender: TObject);
  private
    { private declarations }
    FResultActionProc: TResultActProc;
    fActStdProperty: TActStdProp;
    procedure EnumAct;
    procedure ResultActionProc;
    procedure AddStdActProperties;
  public
    { public declarations }
    constructor Create(AOwner: TComponent); override;
    constructor CreateEx(AOwner: TComponent; ResultActProc: TResultActProc);
    destructor Destroy; override;
  end;


  TRecActStdProp = packed record
    Caption: String;
    ShortCut: TShortCut;
    Hint: String;
  end;

  { TActStdPropItem }

  TActStdPropItem = class
  private
    FActProperties: TRecActStdProp;
    FClassName: String;
    procedure SetActClassName(const AValue: String);
    procedure SetActProperties(const AValue: TRecActStdProp);
  public
    property ActClassName: String read FClassName write SetActClassName;
    property ActionProperty: TRecActStdProp read FActProperties write FActProperties;
  end;
   
  { TActStdProp }

  TActStdProp = class
  private
    fPropList: TObjectList;
    procedure Add(ActClassType: TClass; HeadLine, ShortCut, Hint: String);
  public
    constructor Create;
    destructor Destroy; override;
    function IndexOfClass(ActClassName: String): TActStdPropItem;
  end;

implementation

uses actionseditor, ObjInspStrConsts;


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
  
  fActStdProperty.Add(THelpContents, oiStdActHelpContentsHeadLine, '', oiStdActHelpContentsHint);
  fActStdProperty.Add(THelpTopicSearch, oiStdActHelpTopicSearchHeadLine, '', oiStdActHelpTopicSearchHint);
  fActStdProperty.Add(THelpOnHelp, oiStdActHelpHelpHelpHeadLine, '', oiStdActHelpHelpHelpHint);

  fActStdProperty.Add(TFileOpen, oiStdActFileOpenHeadLine, oiStdActFileOpenShortCut, oiStdActFileOpenHint);
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

{ TActStdProp }

procedure TActStdProp.Add(ActClassType: TClass; HeadLine, ShortCut, Hint: String);
var
  ActItem: TActStdPropItem;
begin
  if Assigned(IndexOfClass(ActClassType.ClassName)) then Exit;
  ActItem := TActStdPropItem.Create;
  ActItem.ActClassName := ActClassType.ClassName;
  ActItem.ActionProperty.Caption := HeadLine;
  ActItem.ActionProperty.ShortCut := TextToShortCut(ShortCut);
  ActItem.ActionProperty.Hint := Hint;
  fPropList.Add(ActItem);
end;

constructor TActStdProp.Create;
begin
  fPropList := TObjectList.Create;
end;

destructor TActStdProp.Destroy;
begin
  fPropList.Free;
  inherited Destroy;
end;

function TActStdProp.IndexOfClass(ActClassName: String): TActStdPropItem;
var
  i: Integer;
begin
  Result := nil;
  for i:= 0 to fPropList.Count-1 do begin
    if TActStdPropItem(fPropList[i]).ActClassName = ActClassName then begin
      Result := TActStdPropItem(fPropList[i]);
      Break;
    end;
  end;
end;

{ TActStdPropItem }

procedure TActStdPropItem.SetActClassName(const AValue: String);
begin
  if FClassName = AValue then Exit;
  FClassName := AValue;
end;

procedure TActStdPropItem.SetActProperties(const AValue: TRecActStdProp);
begin
  FActProperties := AValue;
end;

initialization
  {$I actionseditorstd.lrs}

end.

