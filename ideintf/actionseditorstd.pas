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
    
  ToDo: - multiselect for actions
}
unit ActionsEditorStd;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ComCtrls, Buttons, ActnList, StdActns, LCLType;

type
  TResultActProc = procedure (const Category: string;
                   ActionClass: TBasicActionClass; LastItem: Boolean) of object;
  
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
    procedure EnumAct;
    procedure ResultActionProc;
  public
    { public declarations }
    constructor Create(AOwner: TComponent); override;
    constructor CreateEx(AOwner: TComponent; ResultActProc: TResultActProc);
    destructor Destroy; override;
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
      else begin
        if node.Expanded
        then node.Collapse(False)
        else node.Expand(False);
      end;
    end;
  end;
end;

procedure TFormActStandard.EnumAct;
var
  outer, inner: Integer;
  NodeCategory: TTreeNode;
begin
  tvActStdList.BeginUpdate;
  with tvActStdList.Items do begin
    NodeCategory := tvActStdList.Items.Add(nil, cActionListEditorUnknownCategory);
    AddChild(NodeCategory, TAction.ClassName);
  end;
  
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
  outer: Integer;
  lastItem: Boolean;  // for multiselect, but now the multiselect property is not implemented in the TTreeView
begin
  Category := tvActStdList.Selected.Parent.Text;

  lastItem := True;
  for outer := 0 to RegisteredActions.Count-1 do begin
    if RegisteredActions.Items[outer].Name = Category then begin
      FResultActionProc(Category, RegisteredActions.Items[outer].
                      Items[tvActStdList.Selected.Index].ActionClass, lastItem);
      Break;
    end;
  end;
end;

constructor TFormActStandard.Create(AOwner: TComponent);
begin
  CreateEx(AOwner, nil);
end;

constructor TFormActStandard.CreateEx(AOwner: TComponent; ResultActProc: TResultActProc);
begin
  inherited Create(AOwner);
  FResultActionProc := ResultActProc;
  Caption := 'Standard Action Classes';
  LabelHeadLine.Caption := 'Available Action Classes:';
  EnumAct;
end;

destructor TFormActStandard.Destroy;
begin
  inherited Destroy;
end;

initialization
  {$I actionseditorstd.lrs}

end.

