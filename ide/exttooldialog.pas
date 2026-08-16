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
 *   Inc., 51 Franklin Street - Fifth Floor, Boston, MA 02110-1335, USA.   *
 *                                                                         *
 ***************************************************************************

  Author: Mattias Gaertner

  Abstract:
    Defines the TExternalToolList which stores the settings of all external
    tools. (= Programfilename and parameters)
    And provides TExternalToolDlg which is a dialog for editing this list.
}
unit ExtToolDialog;

{$mode objfpc}
{$H+}

{$I ide.inc}

interface

uses
  {$IFDEF IDE_MEM_CHECK}
  MemCheck,
  {$ENDIF}
  Classes, SysUtils,
  // LCL
  Controls, Forms, StdCtrls, ComCtrls, Dialogs, ButtonPanel, Menus, LCLStrConsts, LCLType,
  // LazUtils
  FileUtil,
  // IdeIntf
  IDEImagesIntf, IDEExternToolIntf, IDEDialogs, IDECommands, IdeIntfStrConsts,
  // IdeConfig
  TransferMacros, IDEOptionDefs,
  // IDE
  ExtToolEditDlg, LazarusIDEStrConsts;

const
  MaxExtTools = ecExtToolLast-ecExtToolFirst+1;

type
  { TExternalToolDialog -
    the dialog to edit all external tools }

  TExternalToolDialog = class(TForm)
    ButtonPanel: TButtonPanel;
    lvTools: TListView;
    MenuItemImport: TMenuItem;
    MenuItemExport: TMenuItem;
    MenuItemSeparator: TMenuItem;
    MenuItemClone: TMenuItem;
    OpenDialog1: TOpenDialog;
    PopupDropdownMenu: TPopupMenu;
    SaveDialog1: TSaveDialog;
    ToolBar: TToolBar;
    AddButton: TToolButton;
    RemoveButton: TToolButton;
    EditButton: TToolButton;
    tbSeparator: TToolButton;
    MoveUpButton: TToolButton;
    MoveDownButton: TToolButton;
    tbSeparator2: TToolButton;
    ExtraButton: TToolButton;
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure AddButtonClick(Sender: TObject);
    procedure MenuItemCloneClick(Sender: TObject);
    procedure MenuItemExportClick(Sender: TObject);
    procedure MenuItemImportClick(Sender: TObject);
    procedure RemoveButtonClick(Sender: TObject);
    procedure EditButtonClick(Sender: TObject);
    procedure Move(aOld, aNew: integer);
    procedure MoveUpButtonClick(Sender: TObject);
    procedure MoveDownButtonClick(Sender: TObject);
    procedure lvToolsSelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
    procedure lvToolsDblClick(Sender: TObject);
  private
    fExtToolList: TExternalUserTools;
    procedure SelectItem(i: integer);
    procedure AddTool(aTool: TExternalUserTool; aIndex: integer = -1);
    procedure Load;
    procedure SetExtToolList(NewExtToolList: TExternalUserTools);
    procedure EnableButtons;
  public
    constructor Create(AnOwner: TComponent); override;
    destructor Destroy; override;
    property ExtToolList: TExternalUserTools read fExtToolList write SetExtToolList;
  end;
  
function ShowExtToolDialog: TModalResult;

implementation

{$R *.lfm}

function ShowExtToolDialog: TModalResult;
var
  ExternalToolDialog: TExternalToolDialog;
begin
  Result:=mrCancel;
  ExternalToolDialog:=TExternalToolDialog.Create(nil);
  try
    ExternalToolDialog.ExtToolList:=ExternalUserTools;
    Result:=ExternalToolDialog.ShowModal;
    if Result=mrOk then
      ExternalUserTools.Assign(ExternalToolDialog.ExtToolList);
  finally
    ExternalToolDialog.Free;
  end;
end;

{ TExternalToolDialog }

constructor TExternalToolDialog.Create(AnOwner: TComponent);
begin
  inherited Create(AnOwner);
  Name:='ExternalToolDialog';
  Caption:=lisExtToolExternalTools;
  ToolBar.Images := IDEImages.Images_16;

  AddButton.Caption:=lisAdd;
  RemoveButton.Caption:=lisRemove;
  EditButton.Caption:=lisEdit;
  MoveUpButton.Caption:=lisUp;
  MoveDownButton.Caption:=lisDown;

  ExtraButton.Caption:=lisMoreSub;
  ExtraButton.Style:=tbsButtonDrop;
  MenuItemClone.Caption:=lisClone;
  MenuItemExport.Caption:=lisDlgExport;
  MenuItemImport.Caption:=lisDlgImport;

  AddButton.ImageIndex := IDEImages.LoadImage('laz_add');
  RemoveButton.ImageIndex := IDEImages.LoadImage('laz_delete');
  EditButton.ImageIndex := IDEImages.LoadImage('laz_edit');
  MoveUpButton.ImageIndex := IDEImages.LoadImage('arrow_up');
  MoveDownButton.ImageIndex := IDEImages.LoadImage('arrow_down');

  fExtToolList:=TExternalUserTools.Create;

  OpenDialog1.Filter:= dlgFilterXML+'|*.xml|'+dlgFilterAll+'|'+GetAllFilesMask;
  SaveDialog1.Filter:= OpenDialog1.Filter;
end;

destructor TExternalToolDialog.Destroy;
begin
  FreeAndNil(fExtToolList);
  inherited Destroy;
end;

procedure TExternalToolDialog.SetExtToolList(NewExtToolList: TExternalUserTools);
begin
  if fExtToolList=NewExtToolList then exit;
  fExtToolList.Assign(NewExtToolList);
  Load;
end;

procedure TExternalToolDialog.Load;
var
  i: integer;
begin
  lvTools.Items.BeginUpdate;
  lvTools.Items.Clear;
  for i:=0 to fExtToolList.Count-1 do 
    lvTools.Items.Add.Caption:=fExtToolList[i].Title;
  lvTools.Items.EndUpdate;
  EnableButtons;
end;

procedure TExternalToolDialog.SelectItem(i: integer);
begin
  lvTools.ItemIndex:=i;
  lvTools.ItemFocused:=lvTools.Selected;
  lvTools.Selected.MakeVisible(false);
end;

procedure TExternalToolDialog.AddTool(aTool: TExternalUserTool; aIndex: integer = -1);
var
  lItem: TListItem;
begin
  // add
  fExtToolList.Add(aTool);
  lItem:=lvTools.Items.Add;
  // caption
  lItem.Caption:=aTool.Title;
  // move next to original
  if aIndex>=0 then
    Move(lvTools.Items.Count-1,aIndex);
  // select
  SelectItem(lItem.Index);
end;

procedure TExternalToolDialog.AddButtonClick(Sender: TObject);
var
  MsgResult: TModalResult;
  NewTool: TExternalUserTool;
begin
  if fExtToolList.Count>=MaxExtTools then begin
    IDEMessageDialog(lisExtToolMaximumToolsReached,
                  Format(lisExtToolThereIsAMaximumOfTools, [IntToStr(MaxExtTools)]),
                  mtInformation,[mbCancel]);
    exit;
  end;
  NewTool:=TExternalUserTool.Create(nil);
  NewTool.HasParser[SubToolDefault]:=True;
  MsgResult:=ShowExtToolOptionDlg(NewTool);
  if MsgResult=mrOk then
    AddTool(NewTool)
  else
    NewTool.Free;
  EnableButtons;
end;

procedure TExternalToolDialog.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  // dialog
  if (Key = VK_ESCAPE) and (Shift = []) then
  begin
    ModalResult := mrCancel;
    Key := 0;
  end
  else if (Key = VK_LCL_ENTER) and (Shift = [ssCtrl]) then
  begin
    ModalResult := mrOK;
    Key := 0;
  end
  // add/remove
  else if (Key = VK_N) and (Shift = [ssCtrl]) then
  begin
    AddButtonClick(nil);
    Key := 0;
  end
  else if (Key = VK_C) and (Shift = [ssCtrl, ssAlt]) then
  begin
    MenuItemCloneClick(nil);
    Key := 0;
  end
  else if (Key = VK_DELETE) and (Shift = []) then
  begin
    RemoveButtonClick(nil);
    Key := 0;
  end
  // edit
  else if (Key = VK_LCL_ENTER) and (Shift = []) then
  begin
    EditButtonClick(nil);
    Key := 0;
  end
  // move
  else if (Key = VK_DOWN) and (Shift = [ssCtrl, ssShift]) then
  begin
    MoveDownButtonClick(nil);
    Key := 0;
  end
  else if (Key = VK_UP) and (Shift = [ssCtrl, ssShift]) then
  begin
    MoveUpButtonClick(nil);
    Key := 0;
  end
end;

procedure TExternalToolDialog.MenuItemCloneClick(Sender: TObject);
var
  NewTool, OldTool: TExternalUserTool;
begin
  If lvTools.ItemIndex <> -1 Then Begin
    if fExtToolList.Count>=MaxExtTools then begin
      IDEMessageDialog(lisExtToolMaximumToolsReached,Format(lisExtToolThereIsAMaximumOfTools,
        [IntToStr(MaxExtTools)]),mtInformation,[mbCancel]);
      exit;
    end;
    OldTool := fExtToolList.Items[lvTools.ItemIndex];
    If Assigned(OldTool) Then Begin
      NewTool:=TExternalUserTool.Create(nil);
      NewTool.Assign(OldTool);
      AddTool(NewTool,lvTools.ItemIndex+1); // paste next to original
    end;
  end;
  EnableButtons;
end;

procedure TExternalToolDialog.MenuItemExportClick(Sender: TObject);
Var
  FileConfig : TXMLOptionsStorage;
  AFileName : String;
begin
  If SaveDialog1.Execute Then Begin
    AFileName := SaveDialog1.FileName;
    Case SaveDialog1.FilterIndex Of
      1 : AFileName := ChangeFileExt(AFileName, '.xml');
    end;
    FileConfig := TXMLOptionsStorage.Create(AFileName, False);
    fExtToolList.Save(FileConfig);
    FileConfig.WriteToDisk;
    FreeAndNil(FileConfig);
  end;
end;

procedure TExternalToolDialog.MenuItemImportClick(Sender: TObject);
Var
  FileConfig: TXMLOptionsStorage;
  NewToolList: TExternalUserTools;
begin
  If OpenDialog1.Execute Then Begin
    NewToolList := TExternalUserTools.Create;
    FileConfig := TXMLOptionsStorage.Create(OpenDialog1.FileName, True);
    NewToolList.Load(FileConfig);
    SetExtToolList(NewToolList);
    FreeAndNil(FileConfig);
    FreeAndNil(NewToolList);
  end;
end;

procedure TExternalToolDialog.RemoveButtonClick(Sender: TObject);
var
  i: integer;
begin
  i := lvTools.ItemIndex;
  if i < 0 then exit;
  // confirm
  if IDEMessageDialog(rsMtConfirmation, Format(lisExtToolConfirmRemoving, [lvTools.Items[i].Caption]),
    mtConfirmation, mbYesNoCancel) <> mrYes
  then
    exit;
  // delete
  fExtToolList.Delete(i);
  lvTools.Items.Delete(i);
  // select
  if i < lvTools.Items.Count then
    SelectItem(i)
  else if lvTools.Items.Count > 0 then
    SelectItem(lvTools.Items.Count - 1);
  // update buttons
  EnableButtons;
end;

procedure TExternalToolDialog.EditButtonClick(Sender: TObject);
var
  i: LongInt;
begin
  i:=lvTools.ItemIndex;
  if i<0 then exit;
  if ShowExtToolOptionDlg(fExtToolList[i])=mrOk
  then begin
    lvTools.Items[i].Caption:=fExtToolList[i].Title;
    EnableButtons;
  end;
end;

procedure TExternalToolDialog.Move(aOld, aNew: integer);
begin
  if aOld < 0 then exit;
  if aNew < 0 then exit;
  if aNew >= lvTools.Items.Count then exit;
  fExtToolList.Move(aOld, aNew);
  lvTools.Items.Move(aOld, aNew);
  lvTools.ItemIndex := aNew;
  EnableButtons;
end;

procedure TExternalToolDialog.MoveUpButtonClick(Sender: TObject);
begin
  Move(lvTools.ItemIndex, lvTools.ItemIndex - 1);
end;

procedure TExternalToolDialog.MoveDownButtonClick(Sender: TObject);
begin
  Move(lvTools.ItemIndex, lvTools.ItemIndex + 1);
end;

procedure TExternalToolDialog.EnableButtons;
var
  i: integer;
begin
  i:=lvTools.ItemIndex;
  AddButton.Enabled:=fExtToolList.Count<MaxExtTools;
  MenuItemClone.Enabled:=(i>=0) and (fExtToolList.Count<MaxExtTools);
  RemoveButton.Enabled:=(i>=0);
  EditButton.Enabled:=(i>=0);
  MoveUpButton.Enabled:=(i>0);
  MoveDownButton.Enabled:=(i>=0) and (i<fExtToolList.Count-1);
  MenuItemExport.Enabled:=(fExtToolList.Count>0);
end;

procedure TExternalToolDialog.lvToolsSelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
begin
  EnableButtons;
end;

procedure TExternalToolDialog.lvToolsDblClick(Sender: TObject);
begin
  EditButtonClick(Sender);
end;

end.
