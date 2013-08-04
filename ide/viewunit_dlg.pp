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
 *   Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.        *
 *                                                                         *
 ***************************************************************************
}
unit ViewUnit_Dlg;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, Math, Controls, Forms, Dialogs, Buttons, StdCtrls,
  LazarusIdeStrConsts, IDEProcs, CustomFormEditor, LCLType, LCLIntf, LMessages,
  ExtCtrls, ButtonPanel, Menus, StrUtils, AVL_Tree, ImgList, ComCtrls,
  PackageDefs, IDEWindowIntf, IDEHelpIntf, IDEImagesIntf, ListFilterEdit,
  CodeToolsStructs, CodeToolManager, lazutf8sysutils, LazFileUtils, LazLogger;

type
  TIDEProjectItem = (
    piUnit,
    piComponent,
    piFrame
  );

  TViewUnitsEntry = class
  public
    Name: string;
    ID: integer;
    Selected: boolean;
    constructor Create(const AName: string; AnID: integer; ASelected: boolean);
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
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ListboxDrawItem(Control: TWinControl; Index: Integer;
      ARect: TRect; State: TOwnerDrawState);
    procedure OnIdle(Sender: TObject; var Done: Boolean);
    procedure SortAlphabeticallySpeedButtonClick(Sender: TObject);
    procedure OKButtonClick(Sender :TObject);
    procedure HelpButtonClick(Sender: TObject);
    procedure CancelButtonClick(Sender :TObject);
    procedure ListboxKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
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
    procedure SetIdleConnected(AValue: boolean);
    procedure SetItemType(AValue: TIDEProjectItem);
    procedure SetSortAlphabetically(const AValue: boolean);
  public
    constructor Create(TheOwner: TComponent); override;
    procedure Init(const aCaption: string;
      AllowMultiSelect, EnableMultiSelect: Boolean; aItemType: TIDEProjectItem;
      Entries: TStringList; aStartFilename: string = '');
    property SortAlphabetically: boolean read FSortAlphabetically write SetSortAlphabetically;
    property ItemType: TIDEProjectItem read FItemType write SetItemType;
    property IdleConnected: boolean read FIdleConnected write SetIdleConnected;
  end;

// Entries is a list of TViewUnitsEntry(s)
function ShowViewUnitsDlg(Entries: TStringList; AllowMultiSelect: boolean;
  var CheckMultiSelect: Boolean; const aCaption: string; ItemType: TIDEProjectItem;
  StartFilename: string = ''): TModalResult;

implementation

{$R *.lfm}

function ShowViewUnitsDlg(Entries: TStringList; AllowMultiSelect: boolean;
  var CheckMultiSelect: Boolean; const aCaption: string;
  ItemType: TIDEProjectItem; StartFilename: string): TModalResult;
var
  ViewUnitDialog: TViewUnitDialog;
  UEntry: TViewUnitsEntry;
  i: integer;
begin
  ViewUnitDialog:=TViewUnitDialog.Create(nil);
  with ViewUnitDialog do
  try
    Init(aCaption,AllowMultiSelect,CheckMultiSelect,ItemType,Entries,
         StartFilename);
    // Show the dialog
    Result:=ShowModal;
    if Result=mrOk then begin
      // Return new selections from the dialog
      FilterEdit.StoreSelection;
      for i:=0 to Entries.Count-1 do begin
        UEntry:=TViewUnitsEntry(Entries.Objects[i]);
        UEntry.Selected:=FilterEdit.SelectionList.IndexOf(UEntry.Name)>-1;
      end;
      CheckMultiSelect := mniMultiselect.Checked;
    end;
  finally
    Free;
  end;
end;

{ TViewUnitsEntry }

constructor TViewUnitsEntry.Create(const AName: string; AnID: integer;
  ASelected: boolean);
begin
  inherited Create;
  Name := AName;
  ID := AnID;
  Selected := ASelected;
end;

{ TViewUnitDialog }

constructor TViewUnitDialog.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  IDEDialogLayoutList.ApplyLayout(Self,450,300);
end;

procedure TViewUnitDialog.Init(const aCaption: string; AllowMultiSelect,
  EnableMultiSelect: Boolean; aItemType: TIDEProjectItem; Entries: TStringList;
  aStartFilename: string);
var
  i: Integer;
  UEntry: TViewUnitsEntry;
  SearchPath: String;
  p: Integer;
  Dir: String;
begin
  Caption:=aCaption;
  ItemType:=aItemType;
  mniMultiselect.Enabled := AllowMultiSelect;
  mniMultiselect.Checked := EnableMultiSelect;
  ListBox.MultiSelect := mniMultiselect.Enabled;
  // Data items
  for i:=0 to Entries.Count-1 do begin
    UEntry:=TViewUnitsEntry(Entries.Objects[i]);
    FilterEdit.Items.Add(UEntry.Name);
  end;
  FilterEdit.InvalidateFilter;
  // Initial selection
  for i:=0 to Entries.Count-1 do begin
    UEntry:=TViewUnitsEntry(Entries.Objects[i]);
    if UEntry.Selected then
      FilterEdit.SelectionList.Add(UEntry.Name);
  end;

  if aStartFilename<>'' then begin
    // init search for units
    // -> get unit search path
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
    Canvas.TextRect(ARect, ARect.Left + 20, ARect.Top, Items[Index]);
  end;
end;

procedure TViewUnitDialog.OnIdle(Sender: TObject; var Done: Boolean);

  procedure CheckFile(aFilename: string);
  var
    CompClass: TPFComponentBaseClass;
  begin
    case ItemType of
    piUnit:
      begin
        fFoundFiles[aFilename]:=ExtractFileName(aFilename);
      end;
    piComponent:
      begin
        CompClass:=FindLFMBaseClass(aFilename);
        if CompClass<>pfcbcNone then begin
          fFoundFiles[aFilename]:=ExtractFileName(aFilename);
        end;
      end;
    piFrame:
      begin
        CompClass:=FindLFMBaseClass(aFilename);
        if CompClass<>pfcbcFrame then begin
          fFoundFiles[aFilename]:=ExtractFileName(aFilename);
        end;
      end;
    end;
  end;

  procedure CheckDirectory(aDirectory: string);
  begin
    DebugLn(['CheckDirectory ',aDirectory]);
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
      CheckFile(aFilename);
      fSearchFiles.Remove(aFilename);
    end else begin
      AVLNode:=fSearchDirectories.Tree.FindLowest;
      if AVLNode<>nil then begin
        aFilename:=fSearchDirectories.GetNodeData(AVLNode)^.Name;
        CheckDirectory(aFilename);
        fSearchDirectories.Remove(aFilename);
      end else
        break;
    end;
  end;
  // ToDo: update entries from fFoundFiles
  IdleConnected:=false;
end;

procedure TViewUnitDialog.FormDestroy(Sender: TObject);
begin
  FreeAndNil(fSearchDirectories);
  FreeAndNil(fSearchFiles);
  FreeAndNil(fFoundFiles);
  IdleConnected:=false;
end;

procedure TViewUnitDialog.FormCreate(Sender: TObject);
begin
  fSearchDirectories:=TFilenameToStringTree.Create(false);
  fSearchFiles:=TFilenameToStringTree.Create(false);
  fFoundFiles:=TFilenameToStringTree.Create(false);

  //ActiveControl:=FilterEdit;
  mniMultiSelect.Caption := dlgMultiSelect;
  ButtonPanel.OKButton.Caption:=lisMenuOk;
  ButtonPanel.HelpButton.Caption:=lisMenuHelp;
  ButtonPanel.CancelButton.Caption:=lisCancel;
  SortAlphabeticallySpeedButton.Hint:=lisPESortFilesAlphabetically;
  SortAlphabeticallySpeedButton.LoadGlyphFromLazarusResource('pkg_sortalphabetically');
end;

procedure TViewUnitDialog.OKButtonClick(Sender: TObject);
Begin
  IDEDialogLayoutList.SaveLayout(Self);
  ModalResult := mrOK;
End;

procedure TViewUnitDialog.HelpButtonClick(Sender: TObject);
begin
  LazarusHelp.ShowHelpForIDEControl(Self);
end;

procedure TViewUnitDialog.CancelButtonClick(Sender: TObject);
Begin
  IDEDialogLayoutList.SaveLayout(Self);
  ModalResult := mrCancel;
end;

procedure TViewUnitDialog.ListboxKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_RETURN then
    OKButtonClick(nil)
  // A hack to prevent 'O' working as shortcut for OK-button.
  // Should be removed when issue #20599 is resolved.
  else if (Key = VK_O) and (Shift = []) then
    Key:=VK_UNKNOWN;
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

procedure TViewUnitDialog.SetItemType(AValue: TIDEProjectItem);
begin
  if FItemType=AValue then Exit;
  FItemType:=AValue;
  case ItemType of
    piComponent: FImageIndex := IDEImages.LoadImage(16, 'item_form');
    piFrame:    FImageIndex := IDEImages.LoadImage(16, 'tpanel');
  else FImageIndex:=IDEImages.LoadImage(16, 'item_unit');
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

