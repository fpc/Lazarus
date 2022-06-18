{
  A
}
unit frmHTMLActionsEditor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, stub.htmlactions, p2jselementactions,
  // LCL
  LCLType, LCLProc, Forms, Controls, Dialogs, ExtCtrls, StdCtrls,
  Graphics, Menus, ComCtrls, ActnList,
  // IDEIntf
  PropEdits, PropEditUtils, IDEWindowIntf,
  IDEImagesIntf, ComponentEditors;

type

  { TActionMenuItem }

  TActionMenuItem = Class(TMenuItem)
  private
    Faction: TRegisteredHTMLAction;
  Public
    Property Action : TRegisteredHTMLAction Read Faction Write FAction;
  end;

  { THTMLActionListEditorForm }

  THTMLActionListEditorForm = class(TForm)
    ActDelete: TAction;
    actAddMissing: TAction;
    actAddMissingUsingDB: TAction;
    ActPanelToolBar: TAction;
    ActPanelDescr: TAction;
    ActMoveUp: TAction;
    ActMoveDown: TAction;
    alHTMLActions: TActionList;
    ActNew: TAction;
    lblName: TLabel;
    lstActionName: TListBox;
    MenuItem1: TMenuItem;
    mnuPopupShowToolbar: TMenuItem;
    mnuTBAddMissingDB: TMenuItem;
    mnuAddStdAction: TMenuItem;
    mnuPopupShowDescr: TMenuItem;
    mnuTBAddMissing: TMenuItem;
    mnuTBAddAction: TMenuItem;
    mnuPopupNewAction: TMenuItem;
    mnuPopupAddStdAction: TMenuItem;
    mnuPopupMoveUpAction: TMenuItem;
    mnuPopupMoveDownAction: TMenuItem;
    MenuItem6: TMenuItem;
    mnuPopupDeleteAction: TMenuItem;
    MenuItem8: TMenuItem;
    PanelDescr: TPanel;
    PopMenuActions: TPopupMenu;
    PopMenuToolBarActions: TPopupMenu;
    tbActions: TToolBar;
    btnAdd: TToolButton;
    btnDelete: TToolButton;
    ToolButton4: TToolButton;
    btnUp: TToolButton;
    btnDown: TToolButton;
    procedure actAddMissingExecute(Sender: TObject);
    procedure actAddMissingDBExecute(Sender: TObject);
    procedure ActDeleteExecute(Sender: TObject);
    procedure ActDeleteUpdate(Sender: TObject);
    procedure ActMoveUpDownExecute(Sender: TObject);
    procedure ActMoveDownUpdate(Sender: TObject);
    procedure ActMoveUpUpdate(Sender: TObject);
    procedure ActNewExecute(Sender: TObject);
    procedure ActPanelDescrExecute(Sender: TObject);
    procedure ActPanelToolBarExecute(Sender: TObject);
    procedure ActionListEditorClose(Sender: TObject;
      var CloseAction: TCloseAction);
    procedure ActionListEditorKeyDown(Sender: TObject; var Key: Word; {%H-}Shift: TShiftState);
    procedure ActionListEditorKeyPress(Sender: TObject; var Key: char);
    procedure FormCreate(Sender: TObject);
    procedure FormHide(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure lstActionNameKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure lstActionNameMouseDown(Sender: TOBject; Button: TMouseButton; {%H-}Shift: TShiftState; {%H-}X, Y: Integer);
    procedure lstActionNameClick(Sender: TObject);
    procedure lstActionNameDblClick(Sender: TObject);
  private
    FActionList: THTMLCustomElementActionList;
    FCompDesigner: TComponentEditorDesigner;
    FCompEditor: TComponentEditor;
    procedure FillActions;
    function AddActionComponent(aClass: THTMLCustomElementActionClass): THTMLCustomElementAction;
    procedure AddStandardAction(Sender: TObject);
    procedure AddStandardActionsToMenu;
    procedure SetHTMLActionList(AActionList: THTMLCustomElementActionList);
  protected
    procedure OnComponentRenamed(AComponent: TComponent);
    procedure OnComponentSelection(const NewSelection: TPersistentSelectionList);
    procedure OnComponentDelete(APersistent: TPersistent);
    procedure OnRefreshPropertyValues;
    function GetSelectedAction: THTMLCustomElementAction;
    procedure Notification(AComponent: TComponent; Operation: TOperation);
      override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    Property HTMLActionList: THTMLCustomElementActionList Read FActionList Write SetHTMLActionList;
    Property ComponentEditor : TComponentEditor Read FCompEditor Write FCompEditor;
    Property ComponentDesigner : TComponentEditorDesigner Read FCompDesigner Write FCompDesigner;
  end;

  { TActionListComponentEditor }

Function CreateMissingActions(aEditor : TComponentEditor; aList : THTMLCustomElementActionList; PreferDB : Boolean = False) : Integer;
Function FindActionEditor(AList: THTMLCustomElementActionList): THTMLActionListEditorForm;

implementation

uses strpas2jscomponents, Types, idehtml2class, idehtmltools, frmselecthtmlactions;

{$R *.lfm}

var
  EditorForms : TFPList = nil;
  
procedure InitFormsList;
begin
  EditorForms:=TFPList.Create;
end;

procedure ReleaseFormsList;
begin
  EditorForms.Free;
  EditorForms:=nil;
end;

procedure AddActionEditor(Editor: THTMLActionListEditorForm);
begin
  if Assigned(EditorForms) and (EditorForms.IndexOf(Editor)<0) then 
    EditorForms.Add(Editor);
end;

procedure ReleaseActionEditor(Editor: THTMLActionListEditorForm);
var
  i : Integer;
begin
  if not Assigned(EditorForms) then Exit;
  i:=EditorForms.IndexOf(Editor);
  if i>=0 then EditorForms.Delete(i);
end;


function CreateMissingActions(aEditor: TComponentEditor;
  aList: THTMLCustomElementActionList; PreferDB : Boolean = False): Integer;

Var
  aName, FN : String;
  I : Integer;
  Tags : TElementInfoList;
  aEl : TElementInfo;
  aAction : THTMLCustomElementAction;

begin
  Result:=-1;
  FN:=HTMLTools.GetHTMLFileForComponent(aList);
  if (FN='') then
    begin
    ShowMessage(Format(rsErrNoHTMLFileNameForComponent,[aList.Name]));
    exit;
    end;
  Tags:=TElementInfoList.Create;
  try
    HTMLTools.GetTagIDs(FN,Tags,[eoExtraInfo]);
    // Remove existing
    for I:=Tags.Count-1 downto 0 do
      if aList.FindActionByElementID(Tags[i].ElementID)<>Nil then
        Tags.Delete(I);
    if Tags.Count=0 then
      begin
      ShowMessage(rsAllTagsHaveAction);
      exit;
      end;
    // Now select
    if SelectHTMLActionClasses(Tags,PreferDB) then
      begin
      Result:=0;
      For I:=0 to Tags.Count-1 do
        begin
        aEl:=Tags[i];
        aAction:=aList.NewAction(aList.Owner,aEl.ActionClass);
        aName:='act'+HTMLTools.TagToIdentifier(aEl.ElementID);
        if aList.Owner.FindComponent(aName)<>Nil then
          aName:=aEditor.Designer.CreateUniqueComponentName(aName);
        aAction.Name:=aName;
        aAction.ElementID:=aEl.ElementID;
        aEditor.Designer.ClearSelection;
        aEditor.Designer.PropertyEditorHook.PersistentAdded(aAction,True);
        Inc(Result);
        end;
      end;
  finally
    Tags.Free;
  end;
  aEditor.Designer.Modified;
end;

function FindActionEditor(AList: THTMLCustomElementActionList): THTMLActionListEditorForm;
var
  i : Integer;
begin
  if AList<>nil then
    for i:=0 to EditorForms.Count-1 do begin
      if THTMLActionListEditorForm(EditorForms[i]).HTMLActionList=AList then
        Exit(THTMLActionListEditorForm(EditorForms[i]));
    end;
  Result:=nil
end;


{ THTMLActionListEditorForm }

constructor THTMLActionListEditorForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Caption := rsActionListEditor;
  lblName.Caption := rsElementAction;
  ActNew.Hint := rsActionListEditorNewAction;
  ActDelete.Hint := rsActionListEditorDeleteActionHint;
  ActMoveUp.Hint := rsActionListEditorMoveUpAction;
  ActMoveDown.Hint := rsActionListEditorMoveDownAction;
  ActPanelDescr.Caption := rsActionListEditorPanelDescrriptions;
  ActPanelToolBar.Caption := rsActionListEditorPanelToolBar;
  btnAdd.Hint := rsActionListEditorNewAction;
  mnuTBAddAction.Caption := rsActionListEditorNewAction;
  mnuPopupNewAction.Caption := rsActionListEditorNewAction;
  mnuPopupMoveDownAction.Caption := rsActionListEditorMoveDownAction;
  mnuPopupMoveUpAction.Caption := rsActionListEditorMoveUpAction;
  mnuPopupDeleteAction.Caption := rsActionListEditorDeleteAction;
  AddActionEditor(Self);
end;

destructor THTMLActionListEditorForm.Destroy;
begin
  if Assigned(GlobalDesignHook) then
    GlobalDesignHook.RemoveAllHandlersForObject(Self);
  ReleaseActionEditor(Self);
  inherited Destroy;
end;



procedure THTMLActionListEditorForm.FormCreate(Sender: TObject);

begin
  tbActions.Images := IDEImages.Images_16;
  btnAdd.ImageIndex := IDEImages.GetImageIndex('laz_add');
  btnDelete.ImageIndex := IDEImages.GetImageIndex('laz_delete');
  btnUp.ImageIndex := IDEImages.GetImageIndex('arrow_up');
  btnDown.ImageIndex := IDEImages.GetImageIndex('arrow_down');
  IDEDialogLayoutList.ApplyLayout(Self);
  AddStandardActionsToMenu;
end;

procedure THTMLActionListEditorForm.AddStandardActionsToMenu;

  Procedure AddMenuItem(aParent : TMenuItem; aAction : TRegisteredHTMLAction);

  var
    Mnu : TActionMenuItem;

  begin
    Mnu:=TActionMenuItem.Create(Self);
    Mnu.Action:=aAction;
    Mnu.OnClick:=@AddStandardAction;
    Mnu.Caption:=aAction.ActionClass.ClassName+' ('+aAction.Description+')';
    aParent.Add(Mnu);
  end;

Var
  I : Integer;

begin
  For I:=0 to Pas2JSActionRegistry.ActionCount-1 do
    begin
    AddMenuItem(mnuAddStdAction,Pas2JSActionRegistry.Actions[I]);
    AddMenuItem(mnuPopupAddStdAction,Pas2JSActionRegistry.Actions[I]);
    end;
end;

procedure THTMLActionListEditorForm.ActionListEditorClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  IDEDialogLayoutList.SaveLayout(Self);
  CloseAction := caFree;
end;

procedure THTMLActionListEditorForm.FormShow(Sender: TObject);
begin
  Assert(Assigned(GlobalDesignHook), 'TActionListEditor.FormShow: GlobalDesignHook not assigned.');
  GlobalDesignHook.AddHandlerComponentRenamed(@OnComponentRenamed);
  GlobalDesignHook.AddHandlerSetSelection(@OnComponentSelection);
  GlobalDesignHook.AddHandlerRefreshPropertyValues(@OnRefreshPropertyValues);
  GlobalDesignHook.AddHandlerPersistentDeleting(@OnComponentDelete);
end;

procedure THTMLActionListEditorForm.FormHide(Sender: TObject);
begin
  GlobalDesignHook.RemoveHandlerComponentRenamed(@OnComponentRenamed);
  GlobalDesignHook.RemoveHandlerSetSelection(@OnComponentSelection);
  GlobalDesignHook.RemoveHandlerRefreshPropertyValues(@OnRefreshPropertyValues);
  GlobalDesignHook.RemoveHandlerPersistentDeleting(@OnComponentDelete);
end;


procedure THTMLActionListEditorForm.OnComponentRenamed(AComponent: TComponent);
var
  i: Integer;
begin
  if not (AComponent is THTMLCustomElementAction) then Exit;
  i := lstActionName.Items.IndexOfObject(AComponent);
  if i >= 0 then
    lstActionName.Items[i] := AComponent.Name;
end;

procedure THTMLActionListEditorForm.OnComponentSelection(
  const NewSelection: TPersistentSelectionList);
var
  CurAct: THTMLCustomElementAction;
  idx: Integer;
begin
  // TODO: multiselect
  if Not (Assigned(NewSelection) and (NewSelection.Count > 0)) then
    exit;
  if (NewSelection.Items[0] is THTMLCustomElementAction) then
    begin
    CurAct := THTMLCustomElementAction(NewSelection.Items[0]);
    if (CurAct.ActionList = FActionList) then
      begin
      if GetSelectedAction = NewSelection.Items[0] then Exit;
      idx:=lstActionName.Items.IndexOf(CurAct.Name);
      if Idx=-1 then
        begin
        FillActions;
        idx:=lstActionName.Items.IndexOf(CurAct.Name);
        end;
      lstActionName.ItemIndex := Idx;

      lstActionName.Click;
      end
    end
  else
    lstActionName.ItemIndex := -1;
end;

procedure THTMLActionListEditorForm.OnRefreshPropertyValues;
var
  ASelections: TPersistentSelectionList;
  curSel: TPersistent;
  curAct: THTMLCustomElementAction;
  tmpIndex : Integer;

begin
  ASelections := TPersistentSelectionList.Create;
  try
    Assert(Assigned(GlobalDesignHook));
    GlobalDesignHook.GetSelection(ASelections);
    if ASelections.Count = 0 then Exit;
    curSel := ASelections.Items[0];
    if not (curSel is THTMLCustomElementAction) then Exit;
    curAct := THTMLCustomElementAction(curSel);
    if curAct.ActionList <> FActionList then Exit;

    tmpIndex := lstActionName.items.IndexOf(curAct.Name);
    if lstActionName.ItemIndex <> tmpIndex then
    begin
      lstActionName.ItemIndex := tmpIndex;
      lstActionName.Click;
    end;
  finally
    ASelections.Free;
  end;
end;

function THTMLActionListEditorForm.GetSelectedAction: THTMLCustomElementAction;
begin
  if (lstActionName.ItemIndex >= 0) and (FActionList <> nil) then
    Result := FActionList.ActionByName(lstActionName.Items[lstActionName.ItemIndex])
  else
    Result := nil;
end;

procedure THTMLActionListEditorForm.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if Operation=opRemove then
    begin
    if AComponent=FActionList then
      begin
      FActionList:=nil;
      FillActions;
      Close;
      end;
    end;
end;



procedure THTMLActionListEditorForm.lstActionNameKeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);
begin
  if (ssCtrl in Shift) then begin
     case key of
       VK_UP: if ActMoveUp.Enabled then begin
           ActMoveUp.OnExecute(ActMoveUp);
           Key := 0;
         end;
         
       VK_DOWN: if ActMoveDown.Enabled then begin
           ActMoveDown.OnExecute(ActMoveDown);
           Key := 0;
         end;
      end;
  end;
end;

procedure THTMLActionListEditorForm.lstActionNameMouseDown(Sender: TOBject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  oldIndex, index: Integer;
begin
  if Button = mbRight then begin
    oldIndex := TListBox(Sender).ItemIndex;
    index := TListBox(Sender).GetIndexAtY(Y);
    if (index >= 0) and (oldIndex <> index) then begin
      TListBox(Sender).ItemIndex := index;
      TListBox(Sender).Click;
    end;
  end;
end;

procedure THTMLActionListEditorForm.ActDeleteUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := lstActionName.SelCount > 0;
end;

procedure THTMLActionListEditorForm.ActMoveUpDownExecute(Sender: TObject);

var
  fact0,fAct1: THTMLCustomElementAction;
  lboxIndex: Integer;
  direction: Integer;

begin
  if FActionList=nil then exit;
  if TComponent(Sender).Name = 'ActMoveUp'
  then direction := -1
  else direction := 1;

  lboxIndex := lstActionName.ItemIndex;
  
  fact0 := FActionList.ActionByName(lstActionName.Items[lboxIndex]);
  fact1 := FActionList.ActionByName(lstActionName.Items[lboxIndex+direction]);
  fact1.Index := fact0.Index;

  lstActionName.Items.Move(lboxIndex, lboxIndex+direction);
  lstActionName.ItemIndex := lboxIndex+direction;
  FCompDesigner.PropertyEditorHook.Modified(FCompEditor);
end;

procedure THTMLActionListEditorForm.ActMoveDownUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := (lstActionName.Items.Count > 1)
                         and (lstActionName.ItemIndex >= 0)
                         and (lstActionName.ItemIndex < lstActionName.Items.Count-1);
end;

procedure THTMLActionListEditorForm.ActMoveUpUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := (lstActionName.Items.Count > 1)
                         and (lstActionName.ItemIndex > 0);
end;

procedure THTMLActionListEditorForm.ActNewExecute(Sender: TObject);


begin
  if FActionList=nil then exit;
  AddActionComponent(THTMLElementAction);
end;

procedure THTMLActionListEditorForm.ActPanelDescrExecute(Sender: TObject);
begin
  PanelDescr.Visible := TAction(Sender).Checked;
end;

procedure THTMLActionListEditorForm.ActPanelToolBarExecute(Sender: TObject);
begin
  tbActions.Visible := TAction(Sender).Checked;
end;

procedure THTMLActionListEditorForm.ActionListEditorKeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);
var
  MousePoint: TPoint;
begin
  MousePoint := Self.ClientToScreen(Point(0,0));
  if (Key=VK_APPS) then
    PopMenuActions.PopUp(MousePoint.X, MousePoint.Y);
end;

procedure THTMLActionListEditorForm.ActionListEditorKeyPress(Sender: TObject;
  var Key: char);
begin
  if Ord(Key) = VK_ESCAPE then
    Close;
end;


procedure THTMLActionListEditorForm.OnComponentDelete(APersistent: TPersistent);
var
  i: Integer;
begin
  if not (APersistent is THTMLCustomElementAction) then Exit;
  i := lstActionName.Items.IndexOfObject(APersistent);
  if i >= 0 then
    lstActionName.Items.Delete(i);
end;

procedure THTMLActionListEditorForm.ActDeleteExecute(Sender: TObject);

var
  iNameIndex: Integer;
  OldName: String;
  OldAction: THTMLCustomElementAction;

begin
  if FActionList=nil then exit;
  iNameIndex := lstActionName.ItemIndex;
  if iNameIndex < 0 then Exit;
  OldName := lstActionName.Items[iNameIndex];
  lstActionName.Items.Delete(iNameIndex);

  OldAction := FActionList.ActionByName(OldName);
  if Assigned(OldAction) then
  begin
    try
      FCompDesigner.PropertyEditorHook.DeletePersistent(TPersistent(OldAction));
      OldAction:=nil;
    except
      on E: Exception do begin
        MessageDlg(rsErrorDeletingAction,
          Format(rsErrorWhileDeletingAction, [LineEnding, E.Message]), mtError,
          [mbOk], 0);
      end;
    end;
  end;

  if iNameIndex >= lstActionName.Items.Count then
    lstActionName.ItemIndex := lstActionName.Items.Count -1
  else
    lstActionName.ItemIndex := iNameIndex;

  FCompDesigner.SelectOnlyThisComponent(
      FActionList.ActionByName(lstActionName.Items[lstActionName.ItemIndex]));

  if lstActionName.ItemIndex < 0 then
  FCompDesigner.SelectOnlyThisComponent(FActionList);
end;

procedure THTMLActionListEditorForm.actAddMissingDBExecute(Sender: TObject);

Var
  aCount : Integer;
  CurAction: THTMLCustomElementAction;

begin
  aCount:=CreateMissingActions(Self.ComponentEditor,FActionList,True);
  ShowMessage(Format(rsHTMLActionsCreated,[aCount]));
  FillActions;
  if FActionList.ActionCount>0 then
    begin
    CurAction:=FActionList.Actions[FActionList.ActionCount-1];
    FCompDesigner.SelectOnlyThisComponent(CurAction);
    end;
end;

procedure THTMLActionListEditorForm.actAddMissingExecute(Sender: TObject);

Var
  aCount : Integer;
  CurAction: THTMLCustomElementAction;

begin
  aCount:=CreateMissingActions(Self.ComponentEditor,FActionList, False);
  ShowMessage(Format(rsHTMLActionsCreated,[aCount]));
  FillActions;
  if FActionList.ActionCount>0 then
    begin
    CurAction:=FActionList.Actions[FActionList.ActionCount-1];
    FCompDesigner.SelectOnlyThisComponent(CurAction);
    end;
end;


procedure THTMLActionListEditorForm.lstActionNameClick(Sender: TObject);
var
  CurAction: THTMLCustomElementAction;
begin
  // TODO: multiselect
  if lstActionName.ItemIndex < 0 then Exit;
  CurAction := GetSelectedAction;
  if CurAction = nil then Exit;

  FCompDesigner.SelectOnlyThisComponent(CurAction);
end;

procedure THTMLActionListEditorForm.lstActionNameDblClick(Sender: TObject);
var
  CurAction: THTMLCustomElementAction;
begin
  if lstActionName.GetIndexAtY(lstActionName.ScreenToClient(Mouse.CursorPos).Y) < 0
  then Exit;
  CurAction := GetSelectedAction;
  if CurAction = nil then Exit;
  // Add OnExecute for this action
  CreateComponentEvent(CurAction,'OnExecute');
end;

procedure THTMLActionListEditorForm.SetHTMLActionList( AActionList: THTMLCustomElementActionList);
begin
  if FActionList = AActionList then
    exit;
  if FActionList<>nil then
     RemoveFreeNotification(FActionList);
  FActionList := AActionList;
  if FActionList<>nil then
    FreeNotification(FActionList);
  FillActions;
end;

procedure THTMLActionListEditorForm.FillActions;
var
  i: Integer;
  IndexedActionName: String;
begin
  if FActionList=nil then
  begin
    lstActionName.Clear;
    exit;
  end;
  lstActionName.Items.BeginUpdate;
  try
    if lstActionName.ItemIndex > -1 then
      IndexedActionName := lstActionName.Items[lstActionName.ItemIndex];
    lstActionName.Clear;
    // handle all

    for i := 0 to FActionList.ActionCount-1 do
      lstActionName.Items.AddObject(FActionList.Actions[i].Name, FActionList.Actions[i]);
  finally
    lstActionName.Items.EndUpdate;
    i := -1;
    if IndexedActionName <> '' then
      i := lstActionName.Items.IndexOf(IndexedActionName);
    if i > -1 then
      lstActionName.ItemIndex := i
    else if lstActionName.ItemIndex = -1 then
      FCompDesigner.SelectOnlyThisComponent(FActionList);
  end;
end;

Function THTMLActionListEditorForm.AddActionComponent(aClass : THTMLCustomElementActionClass) : THTMLCustomElementAction;

begin
  Result := FActionList.NewAction(FActionList.Owner,aClass);
  Result.Name := FCompDesigner.CreateUniqueComponentName(Result.ClassName);

  // Selection updates correctly when we first clear the selection in Designer
  //  and in Object Inspector, then add a new item. Otherwise there is
  //  a loop of back-and-forth selection updates and the new item does not show.
  FCompDesigner.ClearSelection;
  FCompDesigner.PropertyEditorHook.PersistentAdded(Result,True);
  FCompDesigner.Modified;
end;

procedure THTMLActionListEditorForm.AddStandardAction(Sender: TObject);

Var
  aClass : THTMLCustomElementActionClass;

begin
  aClass:=(Sender as TActionMenuItem).Action.ActionClass;
  AddActionComponent(aClass);
end;



initialization
  InitFormsList;

finalization
  ReleaseFormsList;
end.

