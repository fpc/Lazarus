{
 /***************************************************************************
                          ViewUnit_dlg.pp
                          ---------------
   TViewUnit is the application dialog for displaying all units in a project.
   It gets used for the "View Units", "View Forms" and "Remove from Project"
   menu items.


   Initial Revision  : Sat Feb 19 17:42 CST 1999


 ***************************************************************************/

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
 *   Inc., 51 Franklin Street - Fifth Floor, Boston, MA 02110-1335, USA.   *
 *                                                                         *
 ***************************************************************************
}
unit ViewUnit_Dlg;

{$mode objfpc}{$H+}

{$I ide.inc}

interface

uses
  SysUtils, Classes, Laz_AVL_Tree,
  // LCL
  LCLType, LCLIntf,
  Controls, Forms, Buttons, StdCtrls, ExtCtrls, ButtonPanel, Menus, ComCtrls,
  // LazUtils
  LazSysUtils, LazFileUtils, LazFileCache, AvgLvlTree,
  // Codetools
  CodeToolManager, FileProcs,
  // LazControls
  ListFilterEdit,
  // IdeIntf
  IDEWindowIntf, IDEHelpIntf, IDEImagesIntf,
  // IDE
  LazarusIdeStrConsts, IDEProcs, CustomFormEditor, PackageDefs;

type
  TIDEProjectItem = (
    piNone,
    piUnit,
    piComponent,
    piFrame
  );

  { TViewUnitsEntry }

  TViewUnitsEntry = class
  public
    Name: string;
    ID: integer;
    Selected: boolean;
    Filename: string;
    constructor Create(const AName, AFilename: string; AnID: integer; ASelected: boolean);
  end;

  { TViewUnitsEntryEnumerator }

  TViewUnitsEntryEnumerator = class
  private
    FTree: TAVLTree;
    FCurrent: TAVLTreeNode;
    function GetCurrent: TViewUnitsEntry;
  public
    constructor Create(Tree: TAVLTree);
    function MoveNext: boolean;
    property Current: TViewUnitsEntry read GetCurrent;
  end;

  { TViewUnitEntries }

  TViewUnitEntries = class
  private
    fItems: TStringToPointerTree; // tree of TViewUnitsEntry
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    function Add(AName, AFilename: string; AnID: integer; ASelected: boolean): TViewUnitsEntry;
    function Find(const aName: string): TViewUnitsEntry; inline;
    function Count: integer; inline;
    function GetFiles: TStringList;
    function GetNames: TStringList;
    function GetEntries: TFPList;
    function GetEnumerator: TViewUnitsEntryEnumerator;
  end;

  { TViewUnitDialog }

  TViewUnitDialog = class(TForm)
    BtnPanel: TPanel;
    ButtonPanel: TButtonPanel;
    DummySpeedButton: TSpeedButton;
    FilterEdit: TListFilterEdit;
    ListBox: TListBox;
    mniMultiSelect: TMenuItem;
    OptionsBitBtn: TSpeedButton;
    popListBox: TPopupMenu;
    ProgressBar1: TProgressBar;
    RemoveBitBtn: TSpeedButton;
    SortAlphabeticallySpeedButton: TSpeedButton;
    procedure FormClose(Sender: TObject; var {%H-}CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ListboxDrawItem({%H-}Control: TWinControl; Index: Integer;
      ARect: TRect; {%H-}State: TOwnerDrawState);
    procedure ListboxKeyPress(Sender: TObject; var Key: char);
    procedure ListboxMeasureItem({%H-}Control: TWinControl; {%H-}Index: Integer;
      var AHeight: Integer);
    procedure OnIdle(Sender: TObject; var {%H-}Done: Boolean);
    procedure SortAlphabeticallySpeedButtonClick(Sender: TObject);
    procedure OKButtonClick(Sender :TObject);
    procedure HelpButtonClick(Sender: TObject);
    procedure CancelButtonClick(Sender :TObject);
    procedure MultiselectCheckBoxClick(Sender :TObject);
  private
    FIdleConnected: boolean;
    FItemType: TIDEProjectItem;
    FSortAlphabetically: boolean;
    FImageIndex: Integer;
    fStartFilename: string;
    fSearchDirectories: TFilenameToStringTree; // queued directories to search
    fSearchFiles: TFilenameToStringTree; // queued files to search
    fFoundFiles: TFilenameToStringTree; // filename to caption
    fEntries: TViewUnitEntries;
    procedure SetIdleConnected(AValue: boolean);
    procedure SetItemType(AValue: TIDEProjectItem);
    procedure SetSortAlphabetically(const AValue: boolean);
    procedure ShowEntries;
    procedure UpdateEntries;
  public
    procedure Init(const aCaption: string;
      AllowMultiSelect, EnableMultiSelect: Boolean; aItemType: TIDEProjectItem;
      TheEntries: TViewUnitEntries; aStartFilename: string = '');
    property SortAlphabetically: boolean read FSortAlphabetically write SetSortAlphabetically;
    property ItemType: TIDEProjectItem read FItemType write SetItemType;
    property IdleConnected: boolean read FIdleConnected write SetIdleConnected;
  end;

// Entries is a list of TViewUnitsEntry(s)
function ShowViewUnitsDlg(Entries: TViewUnitEntries; AllowMultiSelect: boolean;
  var CheckMultiSelect: Boolean; const aCaption: string; ItemType: TIDEProjectItem;
  StartFilename: string = '' // if StartFilename is given the Entries are automatically updated
  ): TModalResult;

implementation

{$R *.lfm}

function ShowViewUnitsDlg(Entries: TViewUnitEntries; AllowMultiSelect: boolean;
  var CheckMultiSelect: Boolean; const aCaption: string;
  ItemType: TIDEProjectItem; StartFilename: string): TModalResult;
var
  ViewUnitDialog: TViewUnitDialog;
begin
  ViewUnitDialog:=TViewUnitDialog.Create(nil);
  try
    ViewUnitDialog.Init(aCaption,AllowMultiSelect,CheckMultiSelect,ItemType,Entries,
         StartFilename);
    // Show the dialog
    Result:=ViewUnitDialog.ShowModal;
    if Result=mrOk then begin
      CheckMultiSelect := ViewUnitDialog.mniMultiselect.Checked;
    end;
  finally
    ViewUnitDialog.Free;
  end;
end;

{ TViewUnitsEntryEnumerator }

function TViewUnitsEntryEnumerator.GetCurrent: TViewUnitsEntry;
begin
  if (FCurrent<>nil) and (FCurrent.Data<>nil) then
    Result:=TViewUnitsEntry(PStringToPointerTreeItem(FCurrent.Data)^.Value)
  else
    Result:=nil;
end;

constructor TViewUnitsEntryEnumerator.Create(Tree: TAVLTree);
begin
  FTree:=Tree;
end;

function TViewUnitsEntryEnumerator.MoveNext: boolean;
begin
  if FCurrent=nil then
    FCurrent:=FTree.FindLowest
  else
    FCurrent:=FTree.FindSuccessor(FCurrent);
  Result:=FCurrent<>nil;
end;

{ TViewUnitEntries }

// inline
function TViewUnitEntries.Count: integer;
begin
  Result:=fItems.Count;
end;

// inline
function TViewUnitEntries.Find(const aName: string): TViewUnitsEntry;
begin
  Result:=TViewUnitsEntry(fItems[aName]);
end;

function TViewUnitEntries.GetFiles: TStringList;
var
  S2PItem: PStringToPointerTreeItem;
begin
  Result:=TStringList.Create;
  for S2PItem in fItems do
    Result.Add(TViewUnitsEntry(S2PItem^.Value).Filename);
end;

function TViewUnitEntries.GetNames: TStringList;
var
  S2PItem: PStringToPointerTreeItem;
begin
  Result:=TStringList.Create;
  for S2PItem in fItems do
    Result.Add(TViewUnitsEntry(S2PItem^.Value).Name);
end;

function TViewUnitEntries.GetEntries: TFPList;
var
  S2PItem: PStringToPointerTreeItem;
begin
  Result:=TFPList.Create;
  for S2PItem in fItems do
    Result.Add(TViewUnitsEntry(S2PItem^.Value));
end;

function TViewUnitEntries.GetEnumerator: TViewUnitsEntryEnumerator;
begin
  Result:=TViewUnitsEntryEnumerator.Create(fItems.Tree);
end;

constructor TViewUnitEntries.Create;
begin
  fItems:=TStringToPointerTree.create(false);
end;

destructor TViewUnitEntries.Destroy;
begin
  Clear;
  FreeAndNil(fItems);
  inherited Destroy;
end;

procedure TViewUnitEntries.Clear;
var
  S2PItem: PStringToPointerTreeItem;
begin
  for S2PItem in fItems do
  begin
    TViewUnitsEntry(S2PItem^.Value).Free;
    S2PItem^.Value:=nil;
  end;
  fItems.Clear;
end;

function TViewUnitEntries.Add(AName, AFilename: string; AnID: integer;
  ASelected: boolean): TViewUnitsEntry;
var
  i: Integer;
begin
  if Find(AName)<>nil then begin
    i:=2;
    while Find(AName+'('+IntToStr(i)+')')<>nil do
      inc(i);
    AName:=AName+'('+IntToStr(i)+')';
  end;
  Result:=TViewUnitsEntry.Create(AName,AFilename,AnID,ASelected);
  fItems[AName]:=Result;
end;

{ TViewUnitsEntry }

constructor TViewUnitsEntry.Create(const AName, AFilename: string;
  AnID: integer; ASelected: boolean);
begin
  inherited Create;
  Name := AName;
  ID := AnID;
  Selected := ASelected;
  Filename := AFilename;
end;

{ TViewUnitDialog }

procedure TViewUnitDialog.FormCreate(Sender: TObject);
begin
  IDEDialogLayoutList.ApplyLayout(Self,450,300);
  fSearchDirectories:=TFilenameToStringTree.Create(false);
  fSearchFiles:=TFilenameToStringTree.Create(false);
  fFoundFiles:=TFilenameToStringTree.Create(false);

  mniMultiSelect.Caption := dlgMultiSelect;
  ButtonPanel.OKButton.Caption:=lisMenuOk;
  ButtonPanel.HelpButton.Caption:=lisMenuHelp;
  ButtonPanel.CancelButton.Caption:=lisCancel;
  SortAlphabeticallySpeedButton.Hint:=lisPESortFilesAlphabetically;
  IDEImages.AssignImage(SortAlphabeticallySpeedButton, 'pkg_sortalphabetically');
end;

procedure TViewUnitDialog.FormDestroy(Sender: TObject);
begin
  FreeAndNil(fSearchDirectories);
  FreeAndNil(fSearchFiles);
  FreeAndNil(fFoundFiles);
  IdleConnected:=false;
end;

procedure TViewUnitDialog.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  IDEDialogLayoutList.SaveLayout(Self);
end;

procedure TViewUnitDialog.Init(const aCaption: string; AllowMultiSelect,
  EnableMultiSelect: Boolean; aItemType: TIDEProjectItem;
  TheEntries: TViewUnitEntries; aStartFilename: string);
var
  SearchPath: String;
  p: Integer;
  Dir: String;
begin
  Caption:=aCaption;
  ItemType:=aItemType;
  fEntries:=TheEntries;
  mniMultiselect.Enabled := AllowMultiSelect;
  mniMultiselect.Checked := EnableMultiSelect;
  ListBox.MultiSelect := mniMultiselect.Enabled;
  ShowEntries;

  if aStartFilename<>'' then begin
    // init search for units
    // -> get unit search path and fill fSearchDirectories
    fStartFilename:=TrimFilename(aStartFilename);
    SearchPath:=CodeToolBoss.GetCompleteSrcPathForDirectory(ExtractFilePath(fStartFilename));
    p:=1;
    while p<=length(SearchPath) do begin
      Dir:=GetNextDirectoryInSearchPath(SearchPath,p);
      if Dir<>'' then
        fSearchDirectories[Dir]:='';
    end;
    IdleConnected:=fSearchDirectories.Count>0;
  end;
end;

procedure TViewUnitDialog.SortAlphabeticallySpeedButtonClick(Sender: TObject);
begin
  SortAlphabetically:=SortAlphabeticallySpeedButton.Down;
end;

procedure TViewUnitDialog.ListboxDrawItem(Control: TWinControl; Index: Integer;
  ARect: TRect; State: TOwnerDrawState);
begin
  if Index < 0 then Exit;
  with ListBox do
  begin
    Canvas.FillRect(ARect);
    IDEImages.Images_16.Draw(Canvas, 1, ARect.Top, FImageIndex);
    Canvas.TextRect(ARect, ARect.Left + IDEImages.Images_16.Width + Scale96ToFont(4), ARect.Top, Items[Index]);
  end;
end;

procedure TViewUnitDialog.OnIdle(Sender: TObject; var Done: Boolean);

  procedure CheckFile(aFilename: string);
  var
    CompClass: TPFComponentBaseClass;
  begin
    //debugln(['CheckFile ',aFilename]);
    case ItemType of
    piUnit:
      begin
      end;
    piComponent:
      begin
        CompClass:=FindLFMBaseClass(aFilename);
        if CompClass=pfcbcNone then exit;
      end;
    piFrame:
      begin
        CompClass:=FindLFMBaseClass(aFilename);
        if CompClass<>pfcbcFrame then exit;
      end;
    end;
    fFoundFiles[aFilename]:=ExtractFileName(aFilename);
  end;

  procedure CheckDirectory(aDirectory: string);
  var
    Files: TStrings;
    i: Integer;
    aFilename: String;
  begin
    if not FilenameIsAbsolute(aDirectory) then exit;
    aDirectory:=AppendPathDelim(aDirectory);
    //DebugLn(['CheckDirectory ',aDirectory]);
    Files:=nil;
    try
      CodeToolBoss.DirectoryCachePool.GetListing(aDirectory,Files,false);
      if Files=nil then exit;
      for i:=0 to Files.Count-1 do begin
        aFilename:=Files[i];
        if not FilenameIsPascalUnit(aFilename) then continue;
        aFilename:=aDirectory+aFilename;
        if (ItemType in [piComponent,piFrame])
        and (not FileExistsCached(ChangeFileExt(aFilename,'.lfm'))) then
          continue;
        fSearchFiles[aFilename]:='';
      end;
    finally
      Files.Free;
    end;
  end;

var
  AVLNode: TAVLTreeNode;
  StartTime: int64;
  aFilename: String;
begin
  StartTime:=int64(GetTickCount64);
  while Abs(StartTime-int64(GetTickCount64))<100 do begin
    AVLNode:=fSearchFiles.Tree.FindLowest;
    if AVLNode<>nil then begin
      aFilename:=fSearchFiles.GetNodeData(AVLNode)^.Name;
      fSearchFiles.Remove(aFilename);
      CheckFile(aFilename);
    end else begin
      AVLNode:=fSearchDirectories.Tree.FindLowest;
      if AVLNode<>nil then begin
        aFilename:=fSearchDirectories.GetNodeData(AVLNode)^.Name;
        fSearchDirectories.Remove(aFilename);
        CheckDirectory(aFilename);
      end else begin
        // update entries from fFoundFiles
        UpdateEntries;
        IdleConnected:=false;
        exit;
      end;
    end;
  end;
end;

procedure TViewUnitDialog.OKButtonClick(Sender: TObject);
var
  S2PItem: PStringToPointerTreeItem;
  Entry: TViewUnitsEntry;
Begin
  FilterEdit.StoreSelection;
  for S2PItem in fEntries.fItems do begin
    Entry:=TViewUnitsEntry(S2PItem^.Value);
    Entry.Selected:=FilterEdit.SelectionList.IndexOf(Entry.Name)>-1;
    if Entry.Selected then
      ModalResult := mrOK;
  end;
End;

procedure TViewUnitDialog.HelpButtonClick(Sender: TObject);
begin
  LazarusHelp.ShowHelpForIDEControl(Self);
end;

procedure TViewUnitDialog.CancelButtonClick(Sender: TObject);
Begin
  ModalResult := mrCancel;
end;

procedure TViewUnitDialog.ListboxKeyPress(Sender: TObject; var Key: char);
begin
  if Key = Char(VK_RETURN) then
    OKButtonClick(nil);
end;

procedure TViewUnitDialog.ListboxMeasureItem(Control: TWinControl;
  Index: Integer; var AHeight: Integer);
begin
  if AHeight <= IDEImages.Images_16.Height then
    AHeight := IDEImages.Images_16.Height + 2;
end;

procedure TViewUnitDialog.MultiselectCheckBoxClick(Sender :TObject);
begin
  ListBox.Multiselect := mniMultiSelect.Checked;
end;

procedure TViewUnitDialog.SetSortAlphabetically(const AValue: boolean);
begin
  if FSortAlphabetically=AValue then exit;
  FSortAlphabetically:=AValue;
  SortAlphabeticallySpeedButton.Down:=SortAlphabetically;
  FilterEdit.SortData:=SortAlphabetically;
  FilterEdit.InvalidateFilter;
end;

procedure TViewUnitDialog.ShowEntries;
var
  UEntry: TViewUnitsEntry;
begin
  DisableAutoSizing{$IFDEF DebugDisableAutoSizing}('TViewUnitDialog.ShowEntries'){$ENDIF};
  try
    // Data items
    FilterEdit.Items.Clear;
    for UEntry in fEntries do
      FilterEdit.Items.Add(UEntry.Name);
    FilterEdit.InvalidateFilter;
  finally
    EnableAutoSizing{$IFDEF DebugDisableAutoSizing}('TViewUnitDialog.ShowEntries'){$ENDIF};
  end;
end;

procedure TViewUnitDialog.UpdateEntries;
var
  F2SItem: PStringToStringItem;
begin
  fEntries.Clear;
  for F2SItem in fFoundFiles do
    fEntries.Add(F2SItem^.Value,F2SItem^.Name,-1,false);
  ShowEntries;
end;

procedure TViewUnitDialog.SetItemType(AValue: TIDEProjectItem);
begin
  if FItemType=AValue then Exit;
  FItemType:=AValue;
  case ItemType of
    piComponent: FImageIndex := IDEImages.LoadImage('item_form');
    piFrame:     FImageIndex := IDEImages.LoadImage('tpanel');
    else         FImageIndex := IDEImages.LoadImage('item_unit');
  end;
  if FImageIndex<0 then FImageIndex:=0;
end;

procedure TViewUnitDialog.SetIdleConnected(AValue: boolean);
begin
  if FIdleConnected=AValue then Exit;
  FIdleConnected:=AValue;
  if IdleConnected then begin
    Application.AddOnIdleHandler(@OnIdle);
    ProgressBar1.Visible:=true;
    ProgressBar1.Style:=pbstMarquee;
  end
  else begin
    Application.RemoveOnIdleHandler(@OnIdle);
    ProgressBar1.Visible:=false;
  end;
end;

end.

