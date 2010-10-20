unit MainInspector;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls, Buttons,
  ComCtrls, Menus, MenuIntf, ObjectInspector, types;

type

  { TIdeInspectForm }

  TIdeInspectForm = class(TForm)
    ImageList1: TImageList;
    popComponent: TPopupMenu;
    popSubComponent: TPopupMenu;
    Splitter1: TSplitter;
    ToolBar1: TToolBar;
    btnComponent: TToolButton;
    btnSubComponent: TToolButton;
    ToolButton1: TToolButton;
    btnRemoveSelected: TToolButton;
    TreeView1: TTreeView;
    procedure btnSubComponentClick(Sender: TObject);
    procedure MenuItem1Click(Sender: TObject);
    procedure btnComponentClick(Sender: TObject);
    procedure popComponentPopup(Sender: TObject);
    procedure popSubComponentPopup(Sender: TObject);
    procedure btnRemoveSelectedClick(Sender: TObject);
    procedure TreeView1Change(Sender: TObject; Node: TTreeNode);
    procedure TreeView1Click(Sender: TObject);
  private
    { private declarations }
  protected
    FPropertiesGrid: TCustomPropertiesGrid;
    FSelected: TComponent;
    procedure SetSelected(AComp: TComponent);
    procedure UpdateTree;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
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
  ideinspChildren = 'Children';
  ideinspRemoveSelectedItemSFromTree = 'Remove selected item(s) from tree';

procedure Register;

implementation

{$R *.lfm}

type

  { TExtMenuItem }

  TExtMenuItem = class(TMenuItem)
  private
    FTheObject: TPersistent;
  public
    property TheObject: TPersistent read FTheObject write FTheObject;
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
  UpdateTree;
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
  if FSelected = nil then exit;

  ANode := FindNode(FSelected);
  ANode.Expanded := True;
  ANode.Selected := True;
end;

procedure TIdeInspectForm.Notification(AComponent: TComponent; Operation: TOperation);
var
  ANode: TTreeNode;
begin
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
    end;
  end;
  inherited Notification(AComponent, Operation);
end;

constructor TIdeInspectForm.Create(TheOwner: TComponent);
begin
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
  btnSubComponent.Caption := ideinspChildren;
  btnRemoveSelected.Hint := ideinspRemoveSelectedItemSFromTree;

  SetSelected(Application);
end;

destructor TIdeInspectForm.Destroy;
begin
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

