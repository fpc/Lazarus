unit MainInspector;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls, Buttons,
  ComCtrls, Menus, StdCtrls, MenuIntf, ObjectInspector, types, LazIDEIntf;

type

  { THistoryEntry }

  THistoryEntry = class
  private
    FBaseDisplay: string;
  public
    Comp: TComponent;
    Display: string;
    TheUnitName: string;
    FileName: string;
    procedure Assign(Other: THistoryEntry);
    procedure UpdateDisplayName;
  end;

  { TIdeInspectForm }

  TIdeInspectForm = class(TForm)
    ComboHistory: TComboBox;
    EditUnit: TEdit;
    EditFile: TEdit;
    ImageList1: TImageList;
    menuFollowForm: TMenuItem;
    menuFollowFrame: TMenuItem;
    Panel1: TPanel;
    popComponent: TPopupMenu;
    popSubComponent: TPopupMenu;
    popControls: TPopupMenu;
    popFollowType: TPopupMenu;
    btnOpenFile: TSpeedButton;
    Splitter1: TSplitter;
    ToolBar1: TToolBar;
    btnComponent: TToolButton;
    btnSubComponent: TToolButton;
    ToolButton1: TToolButton;
    btnRemoveSelected: TToolButton;
    ToolButton2: TToolButton;
    btnControls: TToolButton;
    ToolButton3: TToolButton;
    ToolButtonActiveType: TToolButton;
    ToolButtonFollowActive: TToolButton;
    TreeView1: TTreeView;
    procedure btnControlsClick(Sender: TObject);
    procedure btnOpenFileClick(Sender: TObject);
    procedure btnSubComponentClick(Sender: TObject);
    procedure ComboHistoryChange(Sender: TObject);
    procedure menuFollowFormClick(Sender: TObject);
    procedure menuFollowFrameClick(Sender: TObject);
    procedure MenuItem1Click(Sender: TObject);
    procedure btnComponentClick(Sender: TObject);
    procedure popComponentPopup(Sender: TObject);
    procedure popControlsPopup(Sender: TObject);
    procedure popSubComponentPopup(Sender: TObject);
    procedure btnRemoveSelectedClick(Sender: TObject);
    procedure ToolButtonActiveTypeClick(Sender: TObject);
    procedure ToolButtonFollowActiveClick(Sender: TObject);
    procedure TreeView1Change(Sender: TObject; Node: TTreeNode);
    procedure TreeView1Click(Sender: TObject);
  private
    { private declarations }
    FSelected: TComponent;
    FFollowFrames: Boolean;
    FHistoryList: TList;
    FCurEntry: THistoryEntry;
    FIsUpdatingHistory: Boolean;
  protected
    FPropertiesGrid: TCustomPropertiesGrid;
    procedure SetSelected(AComp: TComponent);
    procedure UpdateTree;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure DoActiveFormChanged(Sender: TObject; Form: TCustomForm);
    procedure DoActiveControChanged(Sender: TObject; LastControl: TControl);
    procedure UpdateHistory;
    procedure UpdateCurrent;
  public
    { public declarations }
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  end;

var
  IdeInspectForm: TIdeInspectForm;

resourcestring
  ideinspInspectIDE = 'Inspect IDE';
  ideinspApplcicationComponents = 'Applcication.Components';
  ideinspScreenForms = 'Screen.Forms';
  ideinspQuickLinks = 'Quick links';
  ideinspComponentsOwned = 'Components (Owned)';
  ideinspRemoveSelectedItemSFromTree = 'Remove selected item(s) from tree';
  ideinspControlsChildren = 'Controls (Children)';
  ideinspInspectingNameClass = 'Inspecting %s';
  ideinspIdeInspector = 'Ide Inspector';

procedure Register;

implementation

{$R *.lfm}

const
  MAX_HIST_CNT = 25;

type

  { TExtMenuItem }

  TExtMenuItem = class(TMenuItem)
  private
    FTheObject: TPersistent;
  public
    property TheObject: TPersistent read FTheObject write FTheObject;
  end;

{ THistoryEntry }

procedure THistoryEntry.Assign(Other: THistoryEntry);
begin
  Comp := Other.Comp;
  FBaseDisplay := Other.FBaseDisplay;
  Display := Other.Display;
  TheUnitName := Other.TheUnitName;
  FileName := Other.FileName;
end;

procedure THistoryEntry.UpdateDisplayName;
begin
  if Comp <> nil then begin
    FBaseDisplay := Comp.Name + ': ' + Comp.ClassName;
    Display := FBaseDisplay + ' (' + IntToHex(PtrUInt(Comp), SizeOf(Pointer)*2) + ')';
  end else
    Display := FBaseDisplay;
end;

{ TIdeInspectForm }

procedure TIdeInspectForm.MenuItem1Click(Sender: TObject);
begin
  SetSelected(TComponent(TExtMenuItem(Sender).TheObject));
end;

procedure TIdeInspectForm.btnSubComponentClick(Sender: TObject);
begin
  btnSubComponent.CheckMenuDropdown;
end;

procedure TIdeInspectForm.ComboHistoryChange(Sender: TObject);
var
  i: LongInt;
begin
  if FIsUpdatingHistory then exit;
  i := ComboHistory.ItemIndex;
  // ignore current entry re-select
  if i <= 0 then
    exit;
  if ComboHistory.Items.Count > FHistoryList.Count then begin
    ComboHistory.Items.Delete(0); // the none-history curent entry
    dec(i);
  end;
  FCurEntry.Assign(THistoryEntry(FHistoryList[i]));
  if i > 0 then begin
    FHistoryList.Move(i, 0);
    ComboHistory.Items.Move(i, 0);
  end;
  if FCurEntry.Comp <> nil then
    SetSelected(FCurEntry.Comp);
  UpdateCurrent;
end;

procedure TIdeInspectForm.menuFollowFormClick(Sender: TObject);
begin
  FFollowFrames := False;
  ToolButtonActiveType.Caption := menuFollowForm.Caption;
  ToolButtonFollowActive.Down := True;
end;

procedure TIdeInspectForm.menuFollowFrameClick(Sender: TObject);
begin
  FFollowFrames := True;
  ToolButtonActiveType.Caption := menuFollowFrame.Caption;
  ToolButtonFollowActive.Down := True;
end;

procedure TIdeInspectForm.btnControlsClick(Sender: TObject);
begin
  btnControls.CheckMenuDropdown;
end;

procedure TIdeInspectForm.btnOpenFileClick(Sender: TObject);
begin
  LazarusIDE.DoOpenEditorFile(EditFile.Text, -1, -1, [ofOnlyIfExists, ofRegularFile]);
end;

procedure TIdeInspectForm.btnComponentClick(Sender: TObject);
begin
  btnComponent.CheckMenuDropdown;
end;

procedure TIdeInspectForm.popComponentPopup(Sender: TObject);
var
  m: TExtMenuItem;
  i: Integer;
begin
  popComponent.Items.Clear;

  m := TExtMenuItem.Create(Self);
  m.Caption := ideinspApplcicationComponents + ' (' + IntToStr(Application.ComponentCount) + ')' ;
  m.Enabled := False;
  popComponent.Items.Add(m);
  for i := 0 to Application.ComponentCount - 1 do begin
    m := TExtMenuItem.Create(Self);
    m.Caption := Application.Components[i].Name +
                 ' ['+Application.Components[i].ClassName+']'+
                 ' ('+IntToStr(Application.Components[i].ComponentCount)+')';
    m.TheObject := Application.Components[i];
    m.OnClick := @MenuItem1Click;
    popComponent.Items.Add(m);
  end;

  m := TExtMenuItem.Create(Self);
  m.Caption := ideinspScreenForms + ' (' + IntToStr(Screen.FormCount) + ')' ;
  m.Enabled := False;
  popComponent.Items.Add(m);
  for i := 0 to Screen.FormCount - 1 do begin
    m := TExtMenuItem.Create(Self);
    m.Caption := Screen.Forms[i].Name +
                 ' ['+Screen.Forms[i].ClassName+']'+
                 ' ('+IntToStr(Screen.Forms[i].ComponentCount)+')';
    m.TheObject := Screen.Forms[i];
    m.OnClick := @MenuItem1Click;
    popComponent.Items.Add(m);
 end;
end;

procedure TIdeInspectForm.popControlsPopup(Sender: TObject);
var
  i: Integer;
  m: TExtMenuItem;
begin
  popControls.Items.Clear;
  if (FSelected = nil) or not(FSelected is TWinControl) then
    exit;

  for i := 0 to TWinControl(FSelected).ControlCount - 1 do begin
    m := TExtMenuItem.Create(Self);
    m.Caption := TWinControl(FSelected).Controls[i].Name +
                 ' ['+TWinControl(FSelected).Controls[i].ClassName+']'+
                 ' ('+IntToStr(TWinControl(FSelected).Controls[i].ComponentCount)+')';
    m.TheObject := TWinControl(FSelected).Controls[i];
    m.OnClick := @MenuItem1Click;
    popControls.Items.Add(m);
  end;
end;

procedure TIdeInspectForm.popSubComponentPopup(Sender: TObject);
var
  i: Integer;
  m: TExtMenuItem;
begin
  popSubComponent.Items.Clear;

  for i := 0 to FSelected.ComponentCount - 1 do begin
    m := TExtMenuItem.Create(Self);
    m.Caption := FSelected.Components[i].Name +
                 ' ['+FSelected.Components[i].ClassName+']'+
                 ' ('+IntToStr(FSelected.Components[i].ComponentCount)+')';
    m.TheObject := FSelected.Components[i];
    m.OnClick := @MenuItem1Click;
    popSubComponent.Items.Add(m);
  end;
end;

procedure TIdeInspectForm.btnRemoveSelectedClick(Sender: TObject);
begin
  if TreeView1.Selected = nil then
    exit;
  if TreeView1.Selected.Parent <> nil then
    SetSelected(TComponent(TreeView1.Selected.Parent.Data))
  else
    SetSelected(nil);
  TreeView1.Selected.Delete;
  UpdateTree;
end;

procedure TIdeInspectForm.ToolButtonActiveTypeClick(Sender: TObject);
begin
  ToolButtonActiveType.CheckMenuDropdown;
end;

procedure TIdeInspectForm.ToolButtonFollowActiveClick(Sender: TObject);
begin
  if ToolButtonFollowActive.Down then
    SetSelected(Self);
end;

procedure TIdeInspectForm.TreeView1Change(Sender: TObject; Node: TTreeNode);
begin
  TreeView1Click(nil);
end;

procedure TIdeInspectForm.TreeView1Click(Sender: TObject);
begin
  if (TreeView1.Selected = nil) or (TreeView1.Selected.Data = nil) then
    exit;
  SetSelected(TComponent(TreeView1.Selected.Data));
end;

procedure TIdeInspectForm.SetSelected(AComp: TComponent);
begin
  FSelected := AComp;
  FPropertiesGrid.TIObject := FSelected;
  btnSubComponent.Enabled := (FSelected <> nil) and (FSelected.ComponentCount > 0);
  btnControls.Enabled := (FSelected <> nil) and
                         (FSelected is TWinControl) and (TWinControl(FSelected).ControlCount > 0);
  UpdateTree;

  // keep date, if the component gets destroyed
  if FCurEntry.Comp <> FSelected then begin
    FCurEntry.Comp := FSelected;
    FCurEntry.UpdateDisplayName;
    FCurEntry.TheUnitName := FSelected.UnitName;
    FCurEntry.FileName := LazarusIDE.FindUnitFile(FCurEntry.TheUnitName);
    UpdateHistory;
  end;
end;

procedure TIdeInspectForm.UpdateTree;
  function FindNode(AComp: TComponent): TTreeNode;
  var
    AParent: TTreeNode;
  begin
    Result := TreeView1.Items.FindNodeWithData(AComp);
    if Result = nil then begin
      if AComp.Owner <> nil then begin
        AParent := FindNode(AComp.Owner);
        Result := AParent.TreeNodes.AddChildObject
          (AParent,
           AComp.Name + ': ' + AComp.ClassName
           + ' ('+IntToStr(AComp.ComponentCount)+')'
             + ' ' + IntToHex(PtrUInt(AComp), 8),
           AComp);
      end else begin
        Result := TreeView1.Items.AddObject
          (nil,
           AComp.Name + ': ' + AComp.ClassName
           + ' ('+IntToStr(AComp.ComponentCount)+')'
           + ' ' + IntToHex(PtrUInt(AComp), 8),
           AComp);
      end;
      AComp.FreeNotification(Self);
    end;
  end;
var
  ANode: TTreeNode;
begin
  if FSelected = nil then begin
    TreeView1.Selected := nil;
    exit;
  end;

  ANode := FindNode(FSelected);
  ANode.Expanded := True;
  ANode.Selected := True;
end;

procedure TIdeInspectForm.Notification(AComponent: TComponent; Operation: TOperation);
var
  ANode: TTreeNode;
  i: Integer;
  f: Boolean;
begin
  if (Operation = opRemove) and (FCurEntry <> nil) and (FCurEntry.Comp = AComponent) then begin
    FCurEntry.Comp := nil;
    FCurEntry.UpdateDisplayName;
    UpdateCurrent;
  end;
  if (Operation = opRemove) and (FHistoryList <> nil) then begin
    f := False;
    for i := 0 to FHistoryList.Count - 1 do
      if THistoryEntry(FHistoryList[i]).Comp = AComponent then begin
        THistoryEntry(FHistoryList[i]).Comp := nil;
        THistoryEntry(FHistoryList[i]).UpdateDisplayName;
        f := True;
      end;
    if f then
      UpdateHistory;
  end;
  if (Operation = opRemove) and (TreeView1 <> nil) then begin
    ANode := TreeView1.Items.FindNodeWithData(AComponent);
    if ANode <> nil then begin
      ANode.DeleteChildren;
      if (AComponent = FSelected) or (TreeView1.Items.FindNodeWithData(FSelected) = nil) then begin
        if ANode.Parent <> nil then
          SetSelected(TComponent(ANode.Parent.Data))
        else
          SetSelected(nil);
      end;
      ANode.Delete;
      UpdateTree;
    end
    else
    if AComponent = FSelected then
      SetSelected(nil);
  end;
  inherited Notification(AComponent, Operation);
end;

procedure TIdeInspectForm.DoActiveFormChanged(Sender: TObject; Form: TCustomForm);
begin
  If not ToolButtonFollowActive.Down then
    exit;

  if Form <> Self then
    SetSelected(Form);
end;

procedure TIdeInspectForm.DoActiveControChanged(Sender: TObject; LastControl: TControl);
begin
  If (not ToolButtonFollowActive.Down) or (not FFollowFrames) then
    exit;
  if Screen.ActiveForm = Self then
    exit;

  if (Screen.ActiveControl <> nil) and (Screen.ActiveControl.Owner <> nil) then
    SetSelected(Screen.ActiveControl.Owner);
end;

procedure TIdeInspectForm.UpdateHistory;
var
  i: Integer;
  FNewHist: THistoryEntry;
begin
  FIsUpdatingHistory := True;

  i := -1;
  if (FCurEntry.Comp <> nil) and
     ( (FCurEntry.Comp is TCustomForm) or (FCurEntry.Comp is TCustomFrame) )
  then begin
    i := FHistoryList.Count - 1;
    while (i >= 0) and (THistoryEntry(FHistoryList[i]).Comp <> FCurEntry.Comp) do
      dec(i);
    if i < 0 then begin
      FNewHist := THistoryEntry.Create;
      FNewHist.Assign(FCurEntry);
      FHistoryList.Insert(0, FNewHist);
      while FHistoryList.Count > MAX_HIST_CNT do begin
        THistoryEntry(FHistoryList[FHistoryList.Count - 1]).Free;
        FHistoryList.Delete(FHistoryList.Count - 1);
      end;
    end
    else begin
      if i > 0 then
        FHistoryList.Move(i, 0);
    end;
  end;

  ComboHistory.Clear;
  if (FHistoryList.Count = 0) or
     (THistoryEntry(FHistoryList[0]).Display <> FCurEntry.Display)
  then
    ComboHistory.Items.Add(FCurEntry.Display);
  for i := 0 to FHistoryList.Count - 1 do
    ComboHistory.Items.Add(THistoryEntry(FHistoryList[i]).Display);

  FIsUpdatingHistory := False;
  UpdateCurrent;
end;

procedure TIdeInspectForm.UpdateCurrent;
begin
  FIsUpdatingHistory := True;
  ComboHistory.Text :=  FCurEntry.Display;
  EditUnit.Text := FCurEntry.TheUnitName;
  EditFile.Text := FCurEntry.FileName;
  if FCurEntry.Comp<> nil then
    Caption := Format(ideinspInspectingNameClass, [FCurEntry.Display])
  else
    Caption := ideinspIdeInspector;
  btnOpenFile.Enabled := EditFile.Text <> '';
  FIsUpdatingHistory := False;
end;

constructor TIdeInspectForm.Create(TheOwner: TComponent);
begin
  FHistoryList := TList.Create;
  FCurEntry := THistoryEntry.Create;
  Screen.AddHandlerActiveFormChanged(@DoActiveFormChanged);
  Screen.AddHandlerActiveControlChanged(@DoActiveControChanged);
  inherited Create(TheOwner);
  FPropertiesGrid := TCustomPropertiesGrid.Create(Self);
  with FPropertiesGrid do
  begin
    Name := 'FPropertiesGrid';
    Parent := self;
    Align := alClient;
    BorderSpacing.Around := 6;
  end;
  btnComponent.Caption := ideinspQuickLinks;
  btnSubComponent.Caption := ideinspComponentsOwned;
  btnControls.Caption := ideinspControlsChildren;
  btnRemoveSelected.Hint := ideinspRemoveSelectedItemSFromTree;

  FFollowFrames := True;
  ToolButtonActiveType.Caption := menuFollowFrame.Caption;
  SetSelected(Application);
end;

destructor TIdeInspectForm.Destroy;
var
  i: Integer;
begin
  Screen.RemoveHandlerActiveControlChanged(@DoActiveControChanged);
  Screen.RemoveHandlerActiveFormChanged(@DoActiveFormChanged);
  for i := 0 to FHistoryList.Count - 1 do
    THistoryEntry(FHistoryList[i]).Free;
  FreeAndNil(FHistoryList);
  FreeAndNil(FCurEntry);
  FreeAndNil(TreeView1);
  inherited Destroy;
end;

procedure IDEMenuClicked(Sender: TObject);
begin
  if not Assigned(IdeInspectForm) then begin
    IdeInspectForm := TIdeInspectForm.Create(Application);
  end;
  IdeInspectForm.Show;
end;

procedure Register;
begin
  RegisterIDEMenuCommand(itmViewIDEInternalsWindows, 'mnuIdeInspector', ideinspInspectIDE, nil,
    @IDEMenuClicked);
end;

end.

