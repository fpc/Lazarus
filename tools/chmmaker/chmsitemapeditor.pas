unit CHMSiteMapEditor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, chmsitemap,
  RegExpr,
  Forms, Controls, Graphics, Dialogs, ComCtrls, ExtCtrls, StdCtrls;

type

  { TSitemapEditForm }

  TSitemapEditForm = class(TForm)
    BeforeBtn: TButton;
    AfterBtn: TButton;
    Bevel1: TBevel;
    DeleteBtn: TButton;
    CancelBtn: TButton;
    DescriptionEdit: TEdit;
    FontEdit: TEdit;
    GlobalPropertiesGroupbox: TGroupBox;
    FontLabel: TLabel;
    DescriptionLabel: TLabel;
    Panel3: TPanel;
    URLEdit: TEdit;
    URLLabel: TLabel;
    Panel1: TPanel;
    Panel2: TPanel;
    SaveBtn: TButton;
    FolderViewCheck: TCheckBox;
    ForegroundClrBtn: TColorButton;
    BackgroundClrBtn: TColorButton;
    ForegroundColorLabel: TLabel;
    BackgroundColorLabel: TLabel;
    SubItemBtn: TButton;
    AddItemLabel: TLabel;
    LocalCombo: TComboBox;
    DescFromTitleBtn: TButton;
    SitemapGroupBox: TGroupBox;
    LocalLinkLabel: TLabel;
    SitemapTree: TTreeView;
    procedure AfterBtnClick(Sender: TObject);
    procedure BeforeBtnClick(Sender: TObject);
    procedure CancelBtnClick(Sender: TObject);
    procedure DeleteBtnClick(Sender: TObject);
    procedure DescFromTitleBtnClick(Sender: TObject);
    procedure DescriptionEditChange(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure LocalComboChange(Sender: TObject);
    procedure LocalComboKeyUp(Sender: TObject; var Key: Word; {%H-}Shift: TShiftState);
    procedure SaveBtnClick(Sender: TObject);
    procedure SitemapTreeCustomCreateItem(Sender: TCustomTreeView;
      var ATreeNode: TTreenode);
    procedure SitemapTreeSelectionChanged(Sender: TObject);
    procedure SubItemBtnClick(Sender: TObject);
    procedure URLEditChange(Sender: TObject);
  private
    FStream: TStream;
    FSiteMapType: TSiteMapType;
    FActivated: Boolean;
    procedure InitControls;
  public
    procedure LoadFromStream(AStream: TStream);
    function Execute(AStream: TStream; SiteType: TSiteMapType; AvailableLinks: TStrings): Boolean;
    procedure UpdateLanguage;
  end; 

var
  SitemapEditForm: TSitemapEditForm;

implementation

{$R *.lfm}

uses
  LCLType, CHMMain, CHMStrConsts;

type

  { TChmTreeNode }

  TChmTreeNode = class(TTreeNode) // make pairs
  private
    FName: String;
    FLocal: String;
  public
    property Name: String read FName write FName;
    property Local: String read FLocal write FLocal;
  end;

{ TSitemapEditForm }

procedure TSitemapEditForm.CancelBtnClick(Sender: TObject);
begin
  //
end;

procedure TSitemapEditForm.DeleteBtnClick(Sender: TObject);
begin
  SitemapTree.Selected.DeleteChildren;
  SitemapTree.Selected.Delete;
end;

procedure TSitemapEditForm.DescFromTitleBtnClick(Sender: TObject);
var
  iSub: Integer;
  sFilename: String;
  oLines: TStringList;
  oRE: TRegExpr;
begin
  if (LocalCombo.ItemIndex = -1) then
  begin
    MessageDlg(rsSelectLocalFile, mtError, [mbCancel], 0);
    Exit;
  end;

  // Load to StringList and Search for <title>
  sFilename := CHMForm.CreateAbsoluteProjectFile(LocalCombo.Items[LocalCombo.ItemIndex]);
  oLines := TStringList.Create();
  oRE := TRegExpr.Create('(?i)<TITLE>(.*)</TITLE>');
  try
    oLines.LoadFromFile(sFilename);
    for iSub := 0 to oLines.Count - 1 do
    begin
      if (oRE.Exec(oLines.Strings[iSub])) then
      begin
        DescriptionEdit.Text := oRE.Match[1];
        Break;
      end;
    end;
  finally
    oRE.Free();
    oLines.Free();
  end;
end;

procedure TSitemapEditForm.DescriptionEditChange(Sender: TObject);
begin
  if SitemapTree.Selected = nil then Exit;
  TChmTreeNode(SitemapTree.Selected).Text := DescriptionEdit.Text;
end;

procedure TSitemapEditForm.FormActivate(Sender: TObject);
begin
  if not FActivated then
  begin
    FActivated := true;
    Constraints.MinWidth := Panel3.Width + 2*SitemapGroupBox.BorderSpacing.Around +
      Panel1.Width + Panel1.BorderSpacing.Left + Panel1.BorderSpacing.Right;
    Constraints.MinHeight := Panel3.Height + Panel3.BorderSpacing.Top + Panel3.BorderSpacing.Bottom +
      Panel2.Height + 2*BorderSpacing.Around +
      SiteMapTree.Constraints.MinHeight + 2*SiteMapTree.BorderSpacing.Around +
      2*SitemapGroupBox.BorderSpacing.Around;
    if Width < Constraints.MinWidth then Width := Constraints.MinWidth;
    if Height < Constraints.MinHeight then Height := Constraints.MinHeight;
  end;
end;

procedure TSitemapEditForm.FormCreate(Sender: TObject);
begin
  UpdateLanguage;
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
    // indexes typically have 3 keys per item. A name, and nested within a name-local pair.
    // the name parts are often the same. For now we fake it by issuing name twice.
    ChmItem.Name := TreeNode.Text;    // Sets name in item.
    if FSiteMapType=stIndex then
      ChmItem.AddName(TreeNode.Name);   // sets name in first subitem
    ChmItem.AddLocal(TreeNode.Local); // sets local in first subitem

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
  Item := SitemapTree.Items.Insert(SitemapTree.Selected, rsUntitled);
  Item.Selected := True;
  SitemapTreeSelectionChanged(Sender);
end;

procedure TSitemapEditForm.AfterBtnClick(Sender: TObject);
var
  Item: TTreeNode;
begin
  Item := SitemapTree.Items.Add(SitemapTree.Selected, rsUntitled);
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
  LocalLinkLabel.Enabled := Value;
  DescFromTitleBtn.Enabled := Value;
  DescriptionLabel.Enabled := Value;
  DescriptionEdit.Enabled := Value;
  LocalCombo.Enabled := Value;
  URLLabel.Enabled := Value;
  URLEdit.Enabled := Value;

  if Value then begin
    Item := TChmTreeNode(SitemapTree.Selected);
    DescriptionEdit.Text := Item.Text;
    LocalCombo.Text := Item.Local;
    URLEdit.Text := ''; // Item.url; // hardly used
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
  Item := SitemapTree.Items.AddChild(SitemapTree.Selected, rsUntitled);
  Item.Selected := True;
  SitemapTreeSelectionChanged(Sender);
end;

procedure TSitemapEditForm.URLEditChange(Sender: TObject);
begin
  if SitemapTree.Selected = nil then Exit;
//  TChmTreeNode(SitemapTree.Selected). url := URLEdit.Text;
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
  ForegroundColorLabel.Enabled := Value;
  BackgroundColorLabel.Enabled := Value;
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
       TreeNode.Name := ChmItem.Name;
       if ChmItem.SubItemCount > 0 then
         TreeNode.Local := ChmItem.Local;
       AddItems(ChmItem.Children, TreeNode);
     end;
   end;
var
  ChmSiteMap: TChmSiteMap;
begin
  ChmSiteMap := TChmSiteMap.Create(FSiteMapType);
  AStream. Position := 0;
  
  if AStream.Size > 0 then
    ChmSiteMap.LoadFromStream(AStream);
  
  SitemapTree.Items.Clear;
  AddItems(ChmSiteMap.Items, nil);
  
  ForegroundClrBtn.ButtonColor := TColor(ChmSiteMap.ForegroundColor);
  BackgroundClrBtn.ButtonColor := TColor(ChmSiteMap.BackgroundColor);
  FolderViewCheck.Checked := ChmSiteMap.UseFolderImages;
  FontEdit.Text := ChmSiteMap.Font;

  ChmSiteMap.Free;
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

  Result := ShowModal = mrOK;
end;

procedure TSitemapEditForm.UpdateLanguage;
begin
  Caption := rsSiteMapEditor;
  SitemapGroupBox.Caption := rsSiteMap;
  DescriptionLabel.Caption := rsDescription;
  DescFromTitleBtn.Caption := rsFromTitle;
  LocalLinkLabel.Caption := rsLocalLink;
  URLLabel.Caption := rsURL;
  AddItemLabel.Caption := rsAddItem;
  BeforeBtn.Caption := rsBefore;
  AfterBtn.Caption := rsAfter;
  DeleteBtn.Caption := rsDelete;
  SubItemBtn.Caption := rsSubItem;
  GlobalPropertiesGroupbox.Caption := rsGlobalProperties;
  FontLabel.Caption := rsFont;
  BackgroundColorLabel.Caption := rsBackgroundColor;
  ForegroundColorLabel.Caption := rsForegroundColor;
  FolderViewCheck.Caption := rsUseFolderIcons;
  SaveBtn.Caption := rsSave;
  CancelBtn.Caption := rsCancel;
end;

end.

