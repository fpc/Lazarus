unit CHMSiteMapEditor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, chmsitemap,
  ComCtrls, ExtCtrls, StdCtrls, Buttons;

type

  { TSitemapEditForm }

  TSitemapEditForm = class(TForm)
    BeforeBtn: TButton;
    AfterBtn: TButton;
    DeleteBtn: TButton;
    CancelBtn: TButton;
    SaveBtn: TButton;
    FolderViewCheck: TCheckBox;
    ForegroundClrBtn: TColorButton;
    BackgroundClrBtn: TColorButton;
    Label3: TLabel;
    FontEdit: TLabeledEdit;
    Label4: TLabel;
    Label5: TLabel;
    SubItemBtn: TButton;
    Label2: TLabel;
    URLEdit: TLabeledEdit;
    LocalCombo: TComboBox;
    DescFromTitleBtn: TButton;
    GroupBox1: TGroupBox;
    Label1: TLabel;
    DescriptionEdit: TLabeledEdit;
    SitemapTree: TTreeView;
    procedure AfterBtnClick(Sender: TObject);
    procedure BeforeBtnClick(Sender: TObject);
    procedure CancelBtnClick(Sender: TObject);
    procedure DeleteBtnClick(Sender: TObject);
    procedure DescriptionEditChange(Sender: TObject);
    procedure LocalComboChange(Sender: TObject);
    procedure LocalComboKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState
      );
    procedure SaveBtnClick(Sender: TObject);
    procedure SitemapTreeCustomCreateItem(Sender: TCustomTreeView;
      var ATreeNode: TTreenode);
    procedure SitemapTreeSelectionChanged(Sender: TObject);
    procedure SubItemBtnClick(Sender: TObject);
    procedure URLEditChange(Sender: TObject);
  private
    { private declarations }
    FStream: TStream;
    FSiteMapType: TSiteMapType;
    procedure InitControls;
  public
    { public declarations }
    procedure LoadFromStream(AStream: TStream);
    function Execute(AStream: TStream; SiteType: TSiteMapType; AvailableLinks: TStrings): Boolean;
  end; 

var
  SitemapEditForm: TSitemapEditForm;

implementation
uses
  LCLType;

type

  { TChmTreeNode }

  TChmTreeNode = class(TTreeNode)
  private
    FLocal: String;
    FURL: String;
  public
    property URL: String read FURL write FURL;
    property Local: String read FLocal write FLocal;
  end;

{ TSitemapEditForm }

procedure TSitemapEditForm.CancelBtnClick(Sender: TObject);
begin

end;

procedure TSitemapEditForm.DeleteBtnClick(Sender: TObject);
begin
  SitemapTree.Selected.DeleteChildren;
  SitemapTree.Selected.Delete;
end;

procedure TSitemapEditForm.DescriptionEditChange(Sender: TObject);
begin
  if SitemapTree.Selected = nil then Exit;
  TChmTreeNode(SitemapTree.Selected).Text := DescriptionEdit.Text;
end;

procedure TSitemapEditForm.LocalComboChange(Sender: TObject);
begin
  if SitemapTree.Selected = nil then Exit;
  TChmTreeNode(SitemapTree.Selected).Local := LocalCombo.Text;
end;

procedure TSitemapEditForm.LocalComboKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var
  I: Integer;
  FLine: String;
  FLength: Integer;
begin
  case Key of
    VK_BACK,
    VK_DELETE,
    VK_LEFT,
    VK_RIGHT,
    VK_SHIFT,
    VK_CONTROL,
    VK_UP,
    VK_DOWN,
    VK_END,
    VK_HOME: Exit;
  end;
  FLength := Length(LocalCombo.Text);
  for I := 0 to LocalCombo.Items.Count-1 do begin
    FLine := Copy(LocalCombo.Items.Strings[I],1, FLength);
    if CompareStr(FLine, LocalCombo.Text) = 0 then
    begin
      LocalCombo.Text := LocalCombo.Items.Strings[I];
      LocalCombo.SelStart := FLength;
      Application.ProcessMessages;
      LocalCombo.SelLength := Length(LocalCombo.Text) - FLength;
      TChmTreeNode(SitemapTree.Selected).Local := LocalCombo.Text;
    end;

  end;
end;

procedure TSitemapEditForm.SaveBtnClick(Sender: TObject);
  procedure AddItem(TreeNode: TChmTreeNode; ChmItems: TChmSiteMapItems);
  var
    ChmItem: TChmSiteMapItem;
    I: Integer;
  begin
    ChmItem := ChmItems.NewItem;
    ChmItem.Text := TreeNode.Text;
    ChmItem.URL := TreeNode.URL;
    ChmItem.Local := TreeNode.Local;
    for I := 0 to TreeNode.Count-1 do begin
      AddItem(TChmTreeNode(TreeNode.Items[I]), ChmItem.Children);
    end;
  end;
var
  ChmSitemap: TChmSiteMap;
  I: Integer;
begin
  ChmSitemap := TChmSiteMap.Create(FSiteMapType);

  for I := 0 to SitemapTree.Items.Count-1 do
    if SitemapTree.Items.Item[I].Parent = nil then
      AddItem(TChmTreeNode(SitemapTree.Items.Item[I]), ChmSitemap.Items);
    
  ChmSiteMap.ForegroundColor := LongInt(ForegroundClrBtn.ButtonColor);
  ChmSiteMap.BackgroundColor := LongInt(BackgroundClrBtn.ButtonColor);
  ChmSiteMap.UseFolderImages := FolderViewCheck.Checked;
  ChmSiteMap.Font            := FontEdit.Text;
  FStream.Position := 0;
  ChmSitemap.SaveToStream(FStream);
  
  ChmSitemap.Free;
end;

procedure TSitemapEditForm.BeforeBtnClick(Sender: TObject);
var
  Item: TTreeNode;
begin
  Item := SitemapTree.Items.Insert(SitemapTree.Selected, 'Untitled');
  Item.Selected := True;
  SitemapTreeSelectionChanged(Sender);
end;

procedure TSitemapEditForm.AfterBtnClick(Sender: TObject);
var
  Item: TTreeNode;
begin
  Item := SitemapTree.Items.Add(SitemapTree.Selected, 'Untitled');
  Item.Selected := True;
  SitemapTreeSelectionChanged(Sender);
end;

procedure TSitemapEditForm.SitemapTreeCustomCreateItem(Sender: TCustomTreeView;
  var ATreeNode: TTreenode);
begin
  ATreeNode := TChmTreeNode.Create(TTreeView(Sender).Items);
end;

procedure TSitemapEditForm.SitemapTreeSelectionChanged(Sender: TObject);
var
  Value: Boolean;
  Item: TChmTreeNode;
begin
  Value :=  SitemapTree.Selected <> nil;
  
  DeleteBtn.Enabled := Value;
  
  BeforeBtn.Enabled := Value and (SitemapTree.Selected.Parent = nil) and (SitemapTree.Selected.Index <> 0);
  SubItemBtn.Enabled := Value and (FSiteMapType = stTOC);
  Label1.Enabled := Value;
  DescFromTitleBtn.Enabled := Value;
  DescriptionEdit.Enabled := Value;
  LocalCombo.Enabled := Value;
  URLEdit.Enabled := Value;

  if Value then begin
    Item := TChmTreeNode(SitemapTree.Selected);
    DescriptionEdit.Text := Item.Text;
    LocalCombo.Text := Item.Local;
    URLEdit.Text := Item.URL;
  end
  else begin
    DescriptionEdit.Text := '';
    LocalCombo.Text := '';
    URLEdit.Text := '';
  end;
end;

procedure TSitemapEditForm.SubItemBtnClick(Sender: TObject);
var
  Item : TTreeNode;
begin
  Item := SitemapTree.Items.AddChild(SitemapTree.Selected, 'Untitled');
  Item.Selected := True;
  SitemapTreeSelectionChanged(Sender);
end;

procedure TSitemapEditForm.URLEditChange(Sender: TObject);
begin
  if SitemapTree.Selected = nil then Exit;
  TChmTreeNode(SitemapTree.Selected).URL := URLEdit.Text;
end;

procedure TSitemapEditForm.InitControls;
var
  Value: Boolean;
begin
  Value := (FSiteMapType = stTOC);
  
  SubItemBtn.Enabled := Value;
  FolderViewCheck.Enabled := Value;
  ForegroundClrBtn.Enabled := Value;
  BackgroundClrBtn.Enabled := Value;
  Label4.Enabled := Value;
  Label5.Enabled := Value;
end;

procedure TSitemapEditForm.LoadFromStream(AStream: TStream);
   procedure AddItems(Items: TChmSiteMapItems; ParentItem: TTreeNode);
   var
     TreeNode: TChmTreeNode;
     ChmItem: TChmSiteMapItem;
     I: Integer;
   begin
     for I := 0 to Items.Count-1 do begin
       ChmItem := Items.Item[I];
       TreeNode := TChmTreeNode(SitemapTree.Items.AddChild(ParentItem, ChmItem.Text));
       TreeNode.Local := ChmItem.Local;
       TreeNode.URL := ChmItem.URL;
       AddItems(ChmItem.Children, TreeNode);
     end;
   end;
var
  ChmSiteMap: TChmSiteMap;
begin
  ChmSiteMap := TChmSiteMap.Create(FSiteMapType);
  AStream. Position := 0;
  
  if AStream.Size > 0 then;
    ChmSiteMap.LoadFromStream(AStream);
  
  SitemapTree.Items.Clear;
  AddItems(ChmSiteMap.Items, nil);
  
  ForegroundClrBtn.ButtonColor := TColor(ChmSiteMap.ForegroundColor);
  BackgroundClrBtn.ButtonColor := TColor(ChmSiteMap.BackgroundColor);
  FolderViewCheck.Checked := ChmSiteMap.UseFolderImages;
  FontEdit.Text := ChmSiteMap.Font;
  
  
  //ChmSiteMap.Free;
end;

function TSitemapEditForm.Execute(AStream: TStream; SiteType: TSiteMapType;
  AvailableLinks: TStrings): Boolean;
begin
  FStream := AStream;
  AStream.Position := 0;
  FSiteMapType := SiteType;
  InitControls;
  LoadFromStream(AStream);
  
  LocalCombo.Items.Assign(AvailableLinks);
  
  ShowModal;
  
  while ModalResult = mrNone do
    Application.HandleMessage;
    
  Result := ModalResult = mrOK;
end;

initialization
  {$I chmsitemapeditor.lrs}

end.

