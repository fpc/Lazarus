unit OptionsEditorBase;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Buttons, ComCtrls, ExtCtrls;

type

  { TStringIndexItem }

  TStringIndexItem = class
    Name, Value: String;
    Next: TStringIndexItem;
    
    constructor Create;
    constructor Create(AName, AValue: string);
  end;
  
  { TStringIndex }
  TStringIndex = class
    Last, First: TStringIndexItem;
    
    constructor Create;
    destructor Destroy;override;
    
    procedure AddItem(ItemName, ItemValue: String);
    procedure RemoveItem(ItemName: String);
    function ItemHasValue(ItemName: String; SubStringValue: String): Boolean;
    function ItemHasValues(Item: String; SubStringValues: TStrings): Boolean;

    procedure DebugOutFileWords(FileName: String);
  end;
  
  { TOptionsEditorForm }

  TOptionsEditorForm = class(TForm)
    btnCancel: TBitBtn;
    btnOK: TBitBtn;
    ClearFilterButton: TBitBtn;
    FilterEdit: TEdit;
    HelpButton: TBitBtn;
    Label1: TLabel;
    OptionsAreaPanel: TPanel;
    SectionList: TListBox;
    SectionsTreePanel: TPanel;
    SectionTitleLabel: TLabel;
    BottomPanel: TPanel;
    OKCancelPanel: TPanel;
    CategoriesPanel: TPanel;
    MainSplitter: TSplitter;
    procedure ClearFilterButtonClick(Sender: TObject);
    procedure FilterEditChange(Sender: TObject);
    procedure FilterEditEnter(Sender: TObject);
    procedure FilterEditExit(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure HelpButtonClick(Sender: TObject);
    procedure SearchLabelClick(Sender: TObject);
    procedure SectionListSelectionChange(Sender: TObject; User: boolean);
  private
    { private declarations }
    FilterText: String;
    
    ItemList: TStringList;
    IndexTexts: TStringIndex;
    FilterWords: TStringList;
    
    FilterResult: TStringList;
    
    LastControl: TWinControl;
    
    UpdatePageFromSectionList: Boolean;
    
    procedure DoFilter;

  protected
    procedure ScanControlTextsForIndex(ItemName: String; Item: TWinControl);
  public
    { public declarations }
    function AddCategoryItem(ItemName: String; ParentItem: String;
        Item: TPanel): Boolean;
        
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure RecusiveControlsIndex(const Item: TWinControl;
        const ItemName: String);
  end;

implementation

uses
  IDEContextHelpEdit;

type
  TCustomNotebookAccess = class(TCustomNotebook)
  end;

{ TOptionsEditorForm }

procedure TOptionsEditorForm.RecusiveControlsIndex(const Item: TWinControl; const ItemName: String
  );
var
  NewItem: TControl;
  i: Integer;
begin
  for i:= 0 to item.ControlCount-1 do
  begin
    NewItem:= item.Controls[i];
    if NewItem <> Nil then
    begin
      if NewItem is TButton then
      begin
        IndexTexts.AddItem(ItemName, (NewItem as TButton).Caption);
      end
      else
      if NewItem is TCheckBox then
      begin
        IndexTexts.AddItem(ItemName, (NewItem as TCheckBox).Caption);
      end
      else
      if NewItem is TRadioButton then
      begin
        IndexTexts.AddItem(ItemName, (NewItem as TRadioButton).Caption);
      end
      else
      if NewItem is TLabel then
      begin
        IndexTexts.AddItem(ItemName, (NewItem as TLabel).Caption);
      end
      else
      if NewItem is TEdit then
      begin
        IndexTexts.AddItem(ItemName, (NewItem as TEdit).Text);
      end
      else
      if (NewItem is TPanel)
      or (NewItem is TGroupBox)
      or (NewItem is TRadioGroup)
      then
        RecusiveControlsIndex(NewItem as TWinControl, ItemName);
    end;
  end;
end;

procedure TOptionsEditorForm.FilterEditChange(Sender: TObject);
begin
  if FilterEdit.Text = 'text to filter...' then
  begin
    FilterText:= '';
    ClearFilterButton.Visible:= False;
  end
  else
  begin
    FilterText:= FilterEdit.Text;
    FilterEdit.Font.Color:= clText;
    ClearFilterButton.Visible:= (FilterText <> '');
  end;
  DoFilter;
end;

procedure TOptionsEditorForm.ClearFilterButtonClick(Sender: TObject);
begin
  FilterEdit.Text:= '';
end;

procedure TOptionsEditorForm.FilterEditEnter(Sender: TObject);
begin
  FilterEdit.Text:= FilterText;
end;

procedure TOptionsEditorForm.FilterEditExit(Sender: TObject);
begin
  if FilterText = '' then
  begin
     FilterEdit.Font.Color:= clGrayText;
     FilterEdit.Text:= 'text to filter...';
  end;
end;

procedure TOptionsEditorForm.FormActivate(Sender: TObject);
begin
  DoFilter;
end;

procedure TOptionsEditorForm.HelpButtonClick(Sender: TObject);
begin
  ShowContextHelpForIDE(Self);
end;

procedure TOptionsEditorForm.SearchLabelClick(Sender: TObject);
begin
  FilterEdit.SetFocus;
end;

procedure TOptionsEditorForm.SectionListSelectionChange(Sender: TObject;
  User: boolean);
var
  selIndex: Integer;
begin
  if not UpdatePageFromSectionList then
  begin
    Exit;
  end;

  selIndex:= SectionList.ItemIndex;
  if selIndex = -1 then
    Exit;
  if LastControl <> Nil then
  begin
    LastControl.Visible:= false;
  end;
  LastControl:= SectionList.Items.Objects[selIndex] as TWinControl;
  SectionTitleLabel.Caption:= SectionList.Items[selIndex];
  if (LastControl.Parent is TCustomNotebook) and (LastControl is TCustomPage) then
    TCustomNotebookAccess(LastControl.Parent).ActivePageComponent := TCustomPage(LastControl)
  else
    LastControl.Visible:=True;
end;

procedure TOptionsEditorForm.DoFilter;
var
  FilterTextCopy: String;
  FirstWord: String;
  I: Integer;
  LastIndex, NewIndex: Integer;
  LastText: String;
begin
  //Create the list of filter words
  FilterWords.Clear;
  FilterTextCopy:= FilterText;
  while FilterTextCopy <> '' do
  begin
    if Pos(' ', FilterTextCopy) = 0 then
    begin
      FilterWords.Add(FilterTextCopy);
      FilterTextCopy:='';
    end
    else
    begin
      FirstWord:= Copy(FilterTextCopy, 1, Pos(' ', FilterTextCopy) - 1);
      if FirstWord <> '' then
         FilterWords.Add(FirstWord);
      Delete(FilterTextCopy, 1, Length(FirstWord) + 1);
    end;
  end;
  LastIndex:= SectionList.ItemIndex;
  if LastIndex <> -1 then
  begin
    LastText:= SectionList.Items[LastIndex];
  end
  else
  begin
    LastText:= '';
  end;
  SectionList.Items.Clear;
  
  for I:= 0 to ItemList.Count-1 do
  begin
    if IndexTexts.ItemHasValues(ItemList[i], FilterWords) then
       SectionList.Items.AddObject(ItemList[i], ItemList.Objects[i]);
  end;
  UpdatePageFromSectionList:= False;
  NewIndex :=SectionList.Items.IndexOf(LastText);
  if NewIndex = -1 then
  begin
    if SectionList.Items.Count>0 then
    begin
       SectionList.ItemIndex:= 0;
    end;
  end;
  UpdatePageFromSectionList:= True;
end;

procedure TOptionsEditorForm.ScanControlTextsForIndex(ItemName: String;
  Item: TWinControl);
begin
  ItemList.AddObject(ItemName, Item);
  
  if ItemList.Count = 1 then
  begin
    Item.Visible:= True;
    SectionTitleLabel.Caption:= ItemName;
  end
  else
  begin
    Item.Visible:= False;
  end;

  //clear all texts for the current item name
  IndexTexts.RemoveItem(ItemName);
  
  RecusiveControlsIndex(Item, ItemName);
  IndexTexts.DebugOutFileWords('indexdata.txt');
  DoFilter;
end;

function TOptionsEditorForm.AddCategoryItem(ItemName: String;
  ParentItem: String; Item: TPanel): Boolean;
begin
  Item.Parent:= OptionsAreaPanel;
  Item.Align:= alClient;
  Result:= True;
end;

constructor TOptionsEditorForm.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  LastControl:= nil;
  IndexTexts:= TStringIndex.Create;

  ItemList:= TStringList.Create;
  FilterResult:= TStringList.Create;
  FilterWords:= TStringList.Create;
end;

destructor TOptionsEditorForm.Destroy;
begin
  IndexTexts.Free;
  ItemList.Free;
  FilterResult.Free;
  FilterWords.Free;
  inherited Destroy;
end;

{ TStringIndex }

constructor TStringIndex.Create;
begin
  First:= nil;
  Last:= nil;
end;

destructor TStringIndex.Destroy;
var
  Item, Next: TStringIndexItem;
begin
  inherited;
  Item:= First;
  while Item <> Nil do
  begin
    Next:= Item.Next;
    Item.Free;
    Item:= Next;
  end;
end;

procedure TStringIndex.AddItem(ItemName, ItemValue: String);
begin
  ItemValue:= UpperCase(ItemValue);
  if First = Nil then
  begin
    Last:= TStringIndexItem.Create(ItemName, ItemValue);
    First:= Last;
    Exit;
  end;
  Last.Next:= TStringIndexItem.Create(ItemName, ItemValue);
  Last:= Last.Next;
end;

procedure TStringIndex.RemoveItem(ItemName: String);
var
  ItemToRemove, AuxNode: TStringIndexItem;
begin
  ItemToRemove:= First;
  while ItemToRemove <> Nil do
  begin
    if ItemToRemove.Name = ItemName then
    begin
      AuxNode:= ItemToRemove.Next;
      ItemToRemove.Free;
      if ItemToRemove = First then
      begin
        First:= AuxNode;
      end;
      ItemToRemove:= AuxNode;
    end
    else
    begin
     ItemToRemove:= ItemToRemove.Next;
    end;
  end;
end;

function TStringIndex.ItemHasValue(ItemName: String; SubStringValue: String
  ): Boolean;
var
  ItemToScan: TStringIndexItem;
begin
  ItemToScan:= First;
  SubStringValue:= UpperCase(SubStringValue);
  while ItemToScan <> nil do
  begin
    if (ItemName = ItemToScan.Name) and (Pos(SubStringValue, ItemToScan.Value)<> 0) then
    begin
      Result:= True;
      Exit;
    end;
    ItemToScan:= ItemToScan.Next;
  end;
  Result:= False;
end;

function TStringIndex.ItemHasValues(Item: String; SubStringValues: TStrings
  ): Boolean;
var
  I: Integer;
begin
  if SubStringValues.Count = 0 then
  begin
    Result:= True;
    Exit;
  end;
  for I:= 0 to SubStringValues.Count-1 do
  begin
    if not ItemHasValue(Item, SubStringValues[i]) then
    begin
      result:= false;
      Exit;
    end;
  end;
  Result:= True;
end;

procedure TStringIndex.DebugOutFileWords(FileName: String);
var
  F: TextFile;
  ItemToScan: TStringIndexItem;
begin
  AssignFile(F, FileName); Rewrite(F);
  ItemToScan:= First;
  while ItemToScan <> Nil do
  begin
    Writeln(F, 'Item: ', ItemToScan.Name, ' Value: ', ItemToScan.Value);
    ItemToScan:= ItemToScan.Next;
  end;
  CloseFile(F);
end;

{ TStringIndexItem }

constructor TStringIndexItem.Create;
begin
  inherited;
  Next:= nil;
end;

constructor TStringIndexItem.Create(Aname, AValue: string);
begin
  Create;
  Name:= Aname;
  Value:= AValue;
end;

initialization
  {$I optionseditorbase.lrs}
end.


