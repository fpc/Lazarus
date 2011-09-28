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
 *   Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.        *
 *                                                                         *
 ***************************************************************************

  Author: Mattias Gaertner

  Abstract:
    Editor dialog to edit nodes of Help for IDE windows (controls).
}
{%RunCommand bb$MakeExe($(EdFile))}
unit IDEContextHelpEdit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LCLProc, Forms, Controls, Graphics, Dialogs,
  Buttons, ButtonPanel, StdCtrls, ComCtrls,
  // codetools
  CodeToolManager, CodeCache,
  // IDEIntf
  PropEdits, LazIDEIntf, IDEWindowIntf,
  // IDE
  IDEWindowHelp, LazarusIDEStrConsts, ExtCtrls;

type

  { TOpenIDEFileOnIdle }

  TOpenIDEFileOnIdle = class(TComponent)
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation);
      override;
  public
    Filename: string;
    X, Y: integer;
    CloseDialogs: TFPList; // list of modal TCustomForm to cancel
    CancelDialogs: TFPList; // list of modal TCustomForm tried to cancel
    Ending: boolean;
    constructor Create(AOwner: TComponent); override;
    procedure Run;
    destructor Destroy; override;
    procedure OnIdle(Sender: TObject; var Done: Boolean);
  end;


  { TContextHelpEditorDlg }

  TContextHelpEditorDlg = class(TForm)
    ButtonPanel: TButtonPanel;
    FullPathEdit: TEdit;
    NodeIsRootCheckBox: TCheckBox;
    OpenDeclarationBitBtn: TBitBtn;
    TestButton: TButton;
    CreateHelpNodeForControlButton: TButton;
    NodeNameEdit: TEdit;
    NodePathEdit: TEdit;
    NodeHasHelpCheckBox: TCheckBox;
    HelpNodePropertiesGroupBox: TGroupBox;
    NodePathLabel: TLabel;
    NodeNameLabel: TLabel;
    NodesGroupBox: TGroupBox;
    ControlsTreeView: TTreeView;
    HelpNodesTreeView: TTreeView;
    Splitter1: TSplitter;
    WindowControlsGroupBox: TGroupBox;
    procedure ControlsTreeViewShowHint(Sender: TObject; HintInfo: PHintInfo);
    procedure CreateHelpNodeForControlButtonClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure HelpNodesTreeViewSelectionChanged(Sender: TObject);
    procedure NodeHasHelpCheckBoxEditingDone(Sender: TObject);
    procedure NodeIsRootCheckBoxEditingDone(Sender: TObject);
    procedure NodeNameEditEditingDone(Sender: TObject);
    procedure NodePathEditEditingDone(Sender: TObject);
    procedure OkBitBtnClick(Sender: TObject);
    procedure OpenDeclarationBitBtnClick(Sender: TObject);
    procedure OpenDeclarationBitBtnShowHint(Sender: TObject; HintInfo: PHintInfo
      );
    procedure TestButtonClick(Sender: TObject);
  private
    FIDEWindow: TCustomForm;
    FInvoker: TObject;
    FWorkingHelpNodes: TIWHelpTree;
    fLoading: boolean;
    procedure SetIDEWindow(const AValue: TCustomForm);
    procedure SetInvoker(const AValue: TObject);
    procedure UpdateWindowControlsGroupBoxCaption;
    procedure FillControlsTreeView;
    procedure FillHelpNodesTreeView;
    procedure UpdateHelpNodePropertiesGroupBox;
    procedure SelectHelpNode(AControl: TControl);
    procedure SelectControlNode(AControl: TControl);
    function FindHelpTreeNode(HelpNode: TIWHelpNode): TTreeNode;
    function FindControlTreeNode(AControl: TControl): TTreeNode;
    function GetCurrentControl: TControl;
    function GetCurrentHelpNode: TIWHelpNode;
    procedure SaveHelpNodeProperties;
    function GetHintForControl(AControl: TControl): string;
  public
    property Invoker: TObject read FInvoker write SetInvoker;
    property IDEWindow: TCustomForm read FIDEWindow write SetIDEWindow;
    property WorkingHelpNodes: TIWHelpTree read FWorkingHelpNodes;
  end;

var
  ContextHelpEditorDlg: TContextHelpEditorDlg = nil;

function ShowContextHelpEditor(Sender: TObject): TModalResult;
procedure ShowContextHelpForIDE(Sender: TObject);

function FindDeclarationOfIDEControl(AControl: TControl; out Filename: string;
                                     out X, Y: integer): boolean;

implementation

{$R *.lfm}

function ShowContextHelpEditor(Sender: TObject): TModalResult;
begin
  // make sure there is only one editor at a time
  if ContextHelpEditorDlg<>nil then exit;
  
  ContextHelpEditorDlg:=TContextHelpEditorDlg.Create(nil);
  try
    ContextHelpEditorDlg.Invoker:=Sender;
    Result:=ContextHelpEditorDlg.ShowModal;
  finally
    ContextHelpEditorDlg.Free;
    ContextHelpEditorDlg:=nil;
  end;
end;

procedure ShowContextHelpForIDE(Sender: TObject);
begin
  if Sender is TControl then begin
    LoadIDEWindowHelp;
    IDEWindowHelpNodes.InvokeHelp(TControl(Sender));
  end else begin
    DebugLn('ShowContextHelpForIDE Sender=',dbgsName(Sender));
  end;
end;

function FindDeclarationOfIDEControl(AControl: TControl; out Filename: string;
  out X, Y: integer): boolean;
var
  UnitControl: TControl;
  FormFilename: String;
  Code: TCodeBuffer;
  TopLine: integer;
  NewCode: TCodeBuffer;
  Path: String;
  AComponent: TComponent;
begin
  Result:=false;
  Filename:='';
  Y:=0;
  X:=0;
  if AControl=nil then exit;
  UnitControl:=AControl;
  while (UnitControl<>nil) do begin
    if (UnitControl is TFrame) or (UnitControl is TCustomForm) then break;
    UnitControl:=UnitControl.Parent;
  end;
  if UnitControl=nil then begin
    debugln(['FindDeclarationOfIDEControl control '+DbgSName(AControl)+' is not on a form/frame']);
    exit;
  end;
  debugln(['FindDeclarationOfIDEControl UnitControl=',DbgSName(UnitControl),' Unitname=',UnitControl.UnitName]);
  FormFilename:=LazarusIDE.FindUnitFile(GetClassUnitName(UnitControl.ClassType),LazarusIDE);
  debugln(['FindDeclarationOfIDEControl FormFilename=',FormFilename]);
  if FormFilename='' then begin
    debugln(['FindDeclarationOfIDEControl UnitControl=',DbgSName(UnitControl),' Unitname=',GetClassUnitName(UnitControl.ClassType),': unit source not found']);
    exit;
  end;
  Code:=CodeToolBoss.LoadFile(FormFilename,true,false);
  if Code=nil then begin
    debugln(['FindDeclarationOfIDEControl unable to open file: '+FormFilename]);
    exit;
  end;

  Path:='';
  AComponent:=AControl;
  while (AComponent<>nil) and (AComponent<>UnitControl) do begin
    Path:='.'+AComponent.Name+Path;
    AComponent:=AComponent.Owner;
  end;
  Path:=UnitControl.ClassName+Path;
  if not CodeToolBoss.FindDeclarationOfPropertyPath(Code,Path,NewCode,X,Y,TopLine)
  then begin
    debugln(['FindDeclarationOfIDEControl path ',Path,' not found in unit ',Code.Filename]);
    exit;
  end;
  Filename:=NewCode.Filename;
  // success
  Result:=true;
end;

{ TContextHelpEditorDlg }

procedure TContextHelpEditorDlg.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  IDEDialogLayoutList.SaveLayout(Self);
end;

procedure TContextHelpEditorDlg.CreateHelpNodeForControlButtonClick(
  Sender: TObject);
var
  AControl: TControl;
begin
  AControl:=GetCurrentControl;
  if AControl=nil then exit;
  WorkingHelpNodes.FindNodeForControl(AControl,true);
  FillHelpNodesTreeView;
  SelectHelpNode(AControl);
  SelectControlNode(AControl);
end;

procedure TContextHelpEditorDlg.ControlsTreeViewShowHint(Sender: TObject;
  HintInfo: PHintInfo);
var
  Node: TTreeNode;
begin
  Node:=ControlsTreeView.GetNodeAt(HintInfo^.CursorPos.X,HintInfo^.CursorPos.Y);
  if Node=nil then exit;
  HintInfo^.HintStr:=GetHintForControl(TControl(Node.Data));
end;

procedure TContextHelpEditorDlg.FormCreate(Sender: TObject);
begin
  Caption:=lisEditContextHelp;
  
  TestButton.Caption:=dlgCCOTest;
  CreateHelpNodeForControlButton.Caption:=lisCreateHelpNode;
  OpenDeclarationBitBtn.Caption:=lisOpen;
  OpenDeclarationBitBtn.OnShowHint:=@OpenDeclarationBitBtnShowHint;
  NodeHasHelpCheckBox.Caption:=lisHasHelp;
  NodeIsRootCheckBox.Caption:=lisCEIsARootControl;
  NodePathLabel.Caption:=lisPath;
  NodeNameLabel.Caption:=lisDebugOptionsFrmName;
  NodesGroupBox.Caption:=lisHelpEntries;

  ButtonPanel.OKButton.OnClick:=@OkBitBtnClick;

  IDEDialogLayoutList.ApplyLayout(Self, 600, 450);

  LoadIDEWindowHelp;
  FWorkingHelpNodes:=TIWHelpTree.Create;
  FWorkingHelpNodes.Assign(IDEWindowHelpNodes);
  FillHelpNodesTreeView;
end;

procedure TContextHelpEditorDlg.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FWorkingHelpNodes);
end;

procedure TContextHelpEditorDlg.HelpNodesTreeViewSelectionChanged(
  Sender: TObject);
begin
  UpdateHelpNodePropertiesGroupBox;
end;

procedure TContextHelpEditorDlg.NodeHasHelpCheckBoxEditingDone(Sender: TObject);
begin
  SaveHelpNodeProperties;
end;

procedure TContextHelpEditorDlg.NodeIsRootCheckBoxEditingDone(Sender: TObject);
begin
  SaveHelpNodeProperties;
end;

procedure TContextHelpEditorDlg.NodeNameEditEditingDone(Sender: TObject);
begin
  SaveHelpNodeProperties;
end;

procedure TContextHelpEditorDlg.NodePathEditEditingDone(Sender: TObject);
begin
  SaveHelpNodeProperties;
end;

procedure TContextHelpEditorDlg.OkBitBtnClick(Sender: TObject);
begin
  WorkingHelpNodes.DeleteLeavesWithoutHelp;
  IDEWindowHelpNodes.Assign(WorkingHelpNodes);
  SaveIDEWindowHelp;
  ModalResult:=mrOk;
end;

procedure TContextHelpEditorDlg.OpenDeclarationBitBtnClick(Sender: TObject);
var
  AControl: TControl;
  Closer: TOpenIDEFileOnIdle;
  Filename: string;
  X: integer;
  Y: integer;
begin
  AControl:=GetCurrentControl;
  if AControl=nil then begin
    MessageDlg('Error','Please select a control first',mtError,[mbCancel],0);
    exit;
  end;
  if not FindDeclarationOfIDEControl(AControl,Filename,X,Y) then begin
    MessageDlg('Error','No declaration found for '+DbgSName(AControl),mtError,[mbCancel],0);
    exit;
  end;
  if MessageDlg('Close dialogs?','This will close all currently open modal forms and open the file '+Filename+' in the editor.',
    mtConfirmation,[mbOk,mbCancel],0)<>mrOk then exit;
  Closer:=TOpenIDEFileOnIdle.Create(LazarusIDE.OwningComponent);
  Closer.Filename:=Filename;
  Closer.X:=X;
  Closer.Y:=Y;
  Closer.Run;
end;

procedure TContextHelpEditorDlg.OpenDeclarationBitBtnShowHint(Sender: TObject;
  HintInfo: PHintInfo);
begin
  HintInfo^.HintStr:='Open declaration '+GetHintForControl(GetCurrentControl);
end;

procedure TContextHelpEditorDlg.TestButtonClick(Sender: TObject);
var
  AControl: TControl;
begin
  AControl:=GetCurrentControl;
  if AControl=nil then exit;
  WorkingHelpNodes.InvokeHelp(AControl);
end;

procedure TContextHelpEditorDlg.SetInvoker(const AValue: TObject);
var
  AControl: TControl;
begin
  if FInvoker=AValue then exit;
  FInvoker:=AValue;
  //DebugLn('TContextHelpEditorDlg.SetInvoker Invoker=',dbgsName(Invoker));
  if Invoker is TControl then begin
    AControl:=TControl(Invoker);
    IDEWindow:=GetParentForm(AControl);
    //DebugLn('TContextHelpEditorDlg.SetInvoker IDEWindow=',dbgsName(IDEWindow));
    WorkingHelpNodes.FindNodeForControl(AControl,true);
    FillHelpNodesTreeView;
    SelectHelpNode(AControl);
    SelectControlNode(AControl);
  end;
end;

procedure TContextHelpEditorDlg.UpdateWindowControlsGroupBoxCaption;
begin
  if IDEWindow<>nil then begin
    WindowControlsGroupBox.Caption:=dbgsName(IDEWindow);
    WindowControlsGroupBox.Enabled:=true;
  end else begin
    WindowControlsGroupBox.Caption:=lisNoIDEWindowSelected;
    WindowControlsGroupBox.Enabled:=false;
  end;
end;

procedure TContextHelpEditorDlg.FillControlsTreeView;

  procedure Add(AControl: TControl; ParentNode: TTreeNode);
  var
    i: Integer;
    NewNode: TTreeNode;
  begin
    NewNode:=ControlsTreeView.Items.AddChildObject(ParentNode,
                                                   dbgsName(AControl),AControl);
    if AControl is TWinControl then begin
      for i:=0 to TWinControl(AControl).ControlCount-1 do
        Add(TWinControl(AControl).Controls[i],NewNode);
    end;
    NewNode.Expanded:=true;
  end;

begin
  ControlsTreeView.BeginUpdate;
  ControlsTreeView.Items.Clear;
  if IDEWindow<>nil then
    Add(IDEWindow,nil);
  ControlsTreeView.EndUpdate;
end;

procedure TContextHelpEditorDlg.FillHelpNodesTreeView;

  procedure Add(HelpNode: TIWHelpNode; ParentNode: TTreeNode);
  var
    i: Integer;
    NewNode: TTreeNode;
  begin
    NewNode:=HelpNodesTreeView.Items.AddChildObject(ParentNode,
                                                   HelpNode.Name,HelpNode);
    for i:=0 to HelpNode.Count-1 do
      Add(HelpNode[i],NewNode);
    NewNode.Expanded:=true;
  end;

begin
  HelpNodesTreeView.BeginUpdate;
  HelpNodesTreeView.Items.Clear;
  Add(WorkingHelpNodes.Root,nil);
  HelpNodesTreeView.EndUpdate;
end;

procedure TContextHelpEditorDlg.UpdateHelpNodePropertiesGroupBox;
var
  HelpNode: TIWHelpNode;
begin
  if (csDestroying in ComponentState) then exit;
  HelpNode:=GetCurrentHelpNode;
  fLoading:=true;
  try
    if HelpNode<>nil then begin
      HelpNodePropertiesGroupBox.Caption:=HelpNode.Name;
      NodeNameEdit.Text:=HelpNode.Name;
      NodePathEdit.Text:=HelpNode.Path;
      NodeHasHelpCheckBox.Checked:=HelpNode.HasHelp;
      NodeIsRootCheckBox.Checked:=HelpNode.IsRoot;
      HelpNodePropertiesGroupBox.Enabled:=true;
      FullPathEdit.Text:=HelpNode.GetFullPath;
    end else begin
      HelpNodePropertiesGroupBox.Caption:=lisNoNodeSelected;
      HelpNodePropertiesGroupBox.Enabled:=false;
    end;
  finally
    fLoading:=false;
  end;
end;

procedure TContextHelpEditorDlg.SelectHelpNode(AControl: TControl);
var
  Node: TTreeNode;
begin
  Node:=FindHelpTreeNode(WorkingHelpNodes.FindNodeForControl(AControl));
  HelpNodesTreeView.Selected:=Node;
  //DebugLn('TContextHelpEditorDlg.SelectHelpNode Node=',dbgs(Node),' AControl=',dbgsName(AControl),' ',dbgs(HelpNodesTreeView.Selected));
end;

procedure TContextHelpEditorDlg.SelectControlNode(AControl: TControl);
var
  Node: TTreeNode;
begin
  Node:=FindControlTreeNode(AControl);
  ControlsTreeView.Selected:=Node;
end;

function TContextHelpEditorDlg.FindHelpTreeNode(HelpNode: TIWHelpNode
  ): TTreeNode;
  
  function Find(HNode: TIWHelpNode): TTreeNode;
  var
    ParentTreeNode: TTreeNode;
  begin
    if HNode=nil then exit(nil);
    if HNode.Parent=nil then begin
      Result:=HelpNodesTreeView.Items.FindTopLvlNode(HNode.Name);
    end else begin
      ParentTreeNode:=Find(HNode.Parent);
      if ParentTreeNode=nil then
        Result:=nil
      else
        Result:=ParentTreeNode.FindNode(HNode.Name);
    end;
  end;
  
begin
  Result:=Find(HelpNode);
end;

function TContextHelpEditorDlg.FindControlTreeNode(AControl: TControl
  ): TTreeNode;

  function Find(TheControl: TControl): TTreeNode;
  var
    ParentTreeNode: TTreeNode;
  begin
    if TheControl=nil then exit(nil);
    if TheControl.Parent=nil then begin
      Result:=ControlsTreeView.Items.FindTopLvlNode(dbgsName(TheControl));
    end else begin
      ParentTreeNode:=Find(TheControl.Parent);
      if ParentTreeNode=nil then
        Result:=nil
      else
        Result:=ParentTreeNode.FindNode(dbgsName(TheControl));
    end;
  end;

begin
  Result:=Find(AControl);
end;

function TContextHelpEditorDlg.GetCurrentControl: TControl;
var
  Node: TTreeNode;
begin
  Node:=ControlsTreeView.Selected;
  if Node=nil then exit(nil);
  Result:=TControl(Node.Data);
end;

function TContextHelpEditorDlg.GetCurrentHelpNode: TIWHelpNode;
var
  Node: TTreeNode;
begin
  Node:=HelpNodesTreeView.Selected;
  if Node=nil then exit(nil);
  Result:=TIWHelpNode(Node.Data);
end;

procedure TContextHelpEditorDlg.SaveHelpNodeProperties;
var
  HelpNode: TIWHelpNode;
begin
  if fLoading then exit;
  HelpNode:=GetCurrentHelpNode;
  if HelpNode=nil then exit;
  HelpNode.Name:=NodeNameEdit.Text;
  HelpNode.Path:=NodePathEdit.Text;
  HelpNode.HasHelp:=NodeHasHelpCheckBox.Checked;
  HelpNode.IsRoot:=NodeIsRootCheckBox.Checked;
  FullPathEdit.Text:=HelpNode.GetFullPath;
end;

function TContextHelpEditorDlg.GetHintForControl(AControl: TControl): string;
var
  X: integer;
  Y: integer;
  Filename: string;
begin
  if not FindDeclarationOfIDEControl(AControl,Filename,X,Y) then
    Result:='';
  Result:=DbgSName(AControl)+': '+Filename+'('+IntToStr(Y)+','+IntToStr(X)+')';
end;

procedure TContextHelpEditorDlg.SetIDEWindow(const AValue: TCustomForm);
begin
  if FIDEWindow=AValue then exit;
  FIDEWindow:=AValue;
  UpdateWindowControlsGroupBoxCaption;
  FillControlsTreeView;
end;

{ TOpenIDEFileOnIdle }

procedure TOpenIDEFileOnIdle.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if Operation=opRemove then begin
    CloseDialogs.Remove(AComponent);
    CancelDialogs.Remove(AComponent);
  end;
end;

constructor TOpenIDEFileOnIdle.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  CloseDialogs:=TFPList.Create;
  CancelDialogs:=TFPList.Create;
end;

procedure TOpenIDEFileOnIdle.Run;
var
  i: Integer;
  Form: TCustomForm;
begin
  for i:=0 to Screen.CustomFormCount-1 do begin
    Form:=Screen.CustomForms[i];
    if (CloseDialogs.IndexOf(Form)<0) and (fsModal in Form.FormState) then
    begin
      FreeNotification(Form);
      CloseDialogs.Add(Form);
    end;
  end;
  Application.AddOnIdleHandler(@OnIdle);
end;

destructor TOpenIDEFileOnIdle.Destroy;
begin
  Application.RemoveOnIdleHandler(@OnIdle);
  inherited Destroy;
  FreeAndNil(CloseDialogs);
  FreeAndNil(CancelDialogs);
end;

procedure TOpenIDEFileOnIdle.OnIdle(Sender: TObject; var Done: Boolean);
var
  i: Integer;
  Form: TCustomForm;
begin
  if Ending then exit;

  { For example:
    User press ok:
    - cancel the context help dialog
    - cancel the options dialog
    - a question is asked
  }

  // remove pending forms that are no longer modal
  for i:=CloseDialogs.Count-1 downto 0 do begin
    Form:=TCustomForm(CloseDialogs[i]);
    if not (fsModal in Form.FormState) then
      CloseDialogs.Delete(i);
  end;
  Form:=Screen.GetCurrentModalForm;
  debugln(['TOpenIDEFileOnIdle.OnIdle Modal=',DbgSName(Form)]);
  // check if complete
  if CloseDialogs.Count=0 then begin
    debugln(['TOpenIDEFileOnIdle.OnIdle no more closing ...']);
    Ending:=true;
    if Form=nil then begin
      // no more modal forms open
      debugln(['TOpenIDEFileOnIdle.OnIdle opening ...']);
      LazarusIDE.DoOpenFileAndJumpToPos(Filename,Point(X,Y),-1,-1,-1,[ofDoNotLoadResource]);
    end;
    Free;
  end
  else begin
    // close a modal dialog
    if CancelDialogs.IndexOf(Form)>=0 then begin
      // this form was already cancelled, but is still there on idle
      // => user cancelled or something went wrong
      debugln(['TOpenIDEFileOnIdle.OnIdle closing failed']);
      Ending:=true;
      Free;
    end else if CloseDialogs.IndexOf(Form)>=0 then begin
      // close modal dialog
      debugln(['TOpenIDEFileOnIdle.OnIdle closing ',DbgSName(Form)]);
      CancelDialogs.Add(Form);
      CloseDialogs.Remove(Form);
      Form.ModalResult:=mrCancel;
    end;
  end;
end;

end.

