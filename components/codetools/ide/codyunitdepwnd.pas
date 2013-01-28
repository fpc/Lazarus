unit CodyUnitDepWnd;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, AVL_Tree, FileUtil, lazutf8classes, LazLogger,
  TreeFilterEdit, CTUnitGraph, CodeToolManager, DefineTemplates, Forms,
  Controls, Graphics, Dialogs, ExtCtrls, Buttons, ComCtrls, LazIDEIntf,
  ProjectIntf, IDEWindowIntf;

type

  { TUnitDependenciesDialog }

  TUnitDependenciesDialog = class(TForm)
    BtnPanel: TPanel;
    CloseBitBtn: TBitBtn;
    CurUnitPanel: TPanel;
    CurUnitSplitter: TSplitter;
    CurUnitTreeView: TTreeView;
    ProgressBar1: TProgressBar;
    Timer1: TTimer;
    CurUnitTreeFilterEdit: TTreeFilterEdit;
    procedure CloseBitBtnClick(Sender: TObject);
    procedure CurUnitTreeViewSelectionChanged(Sender: TObject);
    procedure FormClose(Sender: TObject; var {%H-}CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure OnIdle(Sender: TObject; var {%H-}Done: Boolean);
    procedure Timer1Timer(Sender: TObject);
  private
    FCurrentUnit: TUGUnit;
    FIdleConnected: boolean;
    FUsesGraph: TUsesGraph;
    procedure SetCurrentUnit(AValue: TUGUnit);
    procedure SetIdleConnected(AValue: boolean);
    procedure AddStartAndTargetUnits;
    procedure UpdateAll;
    procedure UpdateCurUnitTreeView;
    function NodeTextToUnit(NodeText: string): TUGUnit;
    function UGUnitToNodeText(UGUnit: TUGUnit): string;
  public
    property IdleConnected: boolean read FIdleConnected write SetIdleConnected;
    property UsesGraph: TUsesGraph read FUsesGraph;
    property CurrentUnit: TUGUnit read FCurrentUnit write SetCurrentUnit;
  end;

var
  UnitDependenciesDialog: TUnitDependenciesDialog;

procedure ShowUnitDependenciesDialog(Sender: TObject);

implementation

procedure ShowUnitDependenciesDialog(Sender: TObject);
var
  Dlg: TUnitDependenciesDialog;
begin
  Dlg:=TUnitDependenciesDialog.Create(nil);
  try
    Dlg.ShowModal;
  finally
    Dlg.Free;
  end;
end;

{ TUnitDependenciesDialog }

procedure TUnitDependenciesDialog.CloseBitBtnClick(Sender: TObject);
begin
  ModalResult:=mrCancel;
end;

procedure TUnitDependenciesDialog.CurUnitTreeViewSelectionChanged(
  Sender: TObject);
var
  CurUnit: TUGUnit;
begin
  if CurUnitTreeView.Selected=nil then exit;
  CurUnit:=NodeTextToUnit(CurUnitTreeView.Selected.Text);
  if CurUnit=nil then exit;
  CurrentUnit:=CurUnit;
end;

procedure TUnitDependenciesDialog.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  IDEDialogLayoutList.SaveLayout(Self);
end;

procedure TUnitDependenciesDialog.FormCreate(Sender: TObject);
begin
  FUsesGraph:=CodeToolBoss.CreateUsesGraph;
  ProgressBar1.Style:=pbstMarquee;
  AddStartAndTargetUnits;

  IDEDialogLayoutList.ApplyLayout(Self,600,400);

  IdleConnected:=true;
end;

procedure TUnitDependenciesDialog.FormDestroy(Sender: TObject);
begin
  IdleConnected:=false;
  FreeAndNil(FUsesGraph);
end;

procedure TUnitDependenciesDialog.OnIdle(Sender: TObject; var Done: Boolean);
var
  Completed: boolean;
begin
  UsesGraph.Parse(true,Completed,200);
  if Completed then begin
    IdleConnected:=false;
    ProgressBar1.Visible:=false;
    ProgressBar1.Style:=pbstNormal;
    Timer1.Enabled:=false;
    UpdateAll;
  end;
end;

procedure TUnitDependenciesDialog.Timer1Timer(Sender: TObject);
begin
  UpdateAll;
end;

procedure TUnitDependenciesDialog.SetIdleConnected(AValue: boolean);
begin
  if FIdleConnected=AValue then Exit;
  FIdleConnected:=AValue;
  if IdleConnected then
    Application.AddOnIdleHandler(@OnIdle)
  else
    Application.RemoveOnIdleHandler(@OnIdle);
end;

procedure TUnitDependenciesDialog.SetCurrentUnit(AValue: TUGUnit);
begin
  if FCurrentUnit=AValue then Exit;
  FCurrentUnit:=AValue;

end;

procedure TUnitDependenciesDialog.AddStartAndTargetUnits;
var
  aProject: TLazProject;
begin
  UsesGraph.TargetAll:=true;

  // project lpr
  aProject:=LazarusIDE.ActiveProject;
  if (aProject<>nil) and (aProject.MainFile<>nil) then
    UsesGraph.AddStartUnit(aProject.MainFile.Filename);

  // ToDo: add all open packages

end;

procedure TUnitDependenciesDialog.UpdateAll;
begin
  UpdateCurUnitTreeView;
end;

procedure TUnitDependenciesDialog.UpdateCurUnitTreeView;
var
  AVLNode: TAVLTreeNode;
  CurUnit: TUGUnit;
  sl: TStringListUTF8;
  i: Integer;
begin
  CurUnitTreeView.BeginUpdate;
  sl:=TStringListUTF8.Create;
  try
    CurUnitTreeView.Items.Clear;

    AVLNode:=UsesGraph.FilesTree.FindLowest;
    while AVLNode<>nil do begin
      CurUnit:=TUGUnit(AVLNode.Data);
      sl.Add(UGUnitToNodeText(CurUnit));
      AVLNode:=UsesGraph.FilesTree.FindSuccessor(AVLNode);
    end;

    sl.CustomSort(@CompareStringListItemsUTF8LowerCase);
    for i:=0 to sl.Count-1 do begin
      CurUnitTreeView.Items.Add(nil,sl[i]);
    end;
  finally
    sl.Free;
    CurUnitTreeView.EndUpdate;
  end;
end;

function TUnitDependenciesDialog.NodeTextToUnit(NodeText: string): TUGUnit;
var
  AVLNode: TAVLTreeNode;
begin
  AVLNode:=UsesGraph.FilesTree.FindLowest;
  while AVLNode<>nil do begin
    Result:=TUGUnit(AVLNode.Data);
    if NodeText=UGUnitToNodeText(Result) then exit;
    AVLNode:=UsesGraph.FilesTree.FindSuccessor(AVLNode);
  end;
  Result:=nil;
end;

function TUnitDependenciesDialog.UGUnitToNodeText(UGUnit: TUGUnit): string;
begin
  Result:=ExtractFileName(UGUnit.Filename);
end;

{$R *.lfm}

end.

