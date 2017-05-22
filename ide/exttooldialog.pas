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
  Classes, SysUtils, LCLType, LCLProc, Controls, Forms,
  Buttons, StdCtrls, ComCtrls, Dialogs, ExtCtrls, ButtonPanel, Menus,
  FileProcs, FileUtil,
  IDEExternToolIntf, IDEImagesIntf, IDEDialogs, IDEHelpIntf, IDECommands,
  ProjectIntf,
  EnvironmentOpts,
  ExtToolEditDlg, KeyMapping, TransferMacros, IDEProcs, LazFileUtils,
  CompilerOptions,
  LazarusIDEStrConsts, IDEOptionDefs, EditorOptions;

const
  MaxExtTools = ecExtToolLast-ecExtToolFirst+1;

type
  { TExternalToolDialog -
    the dialog to edit all external tools }

  TExternalToolDialog = class(TForm)
    ButtonPanel: TButtonPanel;
    ListBox: TListBox;
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
    procedure AddButtonClick(Sender: TObject);
    procedure HelpButtonClick(Sender: TObject);
    procedure MenuItemCloneClick(Sender: TObject);
    procedure MenuItemExportClick(Sender: TObject);
    procedure MenuItemImportClick(Sender: TObject);
    procedure RemoveButtonClick(Sender: TObject);
    procedure EditButtonClick(Sender: TObject);
    procedure MoveUpButtonClick(Sender: TObject);
    procedure MoveDownButtonClick(Sender: TObject);
    procedure ListboxClick(Sender: TObject);
  private
    fExtToolList: TExternalUserTools;
    fTransferMacros: TTransferMacroList;
    procedure Load;
    procedure SetExtToolList(NewExtToolList: TExternalUserTools);
    procedure SetTransferMacros(NewMacros: TTransferMacroList);
    function ToolDescription(Index: integer): string;
    procedure EnableButtons;
  public
    constructor Create(AnOwner: TComponent); override;
    destructor Destroy; override;
    property ExtToolList: TExternalUserTools read fExtToolList write SetExtToolList;
    property TransferMacros: TTransferMacroList
                                   read fTransferMacros write SetTransferMacros;
  end;
  
function ShowExtToolDialog(ExtToolList: TExternalUserTools;
  TransferMacros: TTransferMacroList):TModalResult;

implementation

{$R *.lfm}

function ShowExtToolDialog(ExtToolList: TExternalUserTools;
  TransferMacros: TTransferMacroList):TModalResult;
var
  ExternalToolDialog: TExternalToolDialog;
begin
  Result:=mrCancel;
  ExternalToolDialog:=TExternalToolDialog.Create(nil);
  try
    ExternalToolDialog.TransferMacros:=TransferMacros;
    ExternalToolDialog.ExtToolList:=ExtToolList;
    Result:=ExternalToolDialog.ShowModal;
    if Result=mrOk then
      ExtToolList.Assign(ExternalToolDialog.ExtToolList);
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

  ButtonPanel.HelpButton.OnClick := @HelpButtonClick;

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

procedure TExternalToolDialog.SetTransferMacros(NewMacros: TTransferMacroList);
begin
  if fTransferMacros=NewMacros then exit;
  fTransferMacros:=NewMacros;
end;

function TExternalToolDialog.ToolDescription(Index: integer): string;
begin
  Result:=fExtToolList[Index].Title;
  if Result='' then
    Result:=ExtractFilename(fExtToolList[Index].Filename);
  //DebugLn(['TExternalToolDialog.ToolDescription Index=',Index,' Result=',Result,' Cmd="',fExtToolList[Index].Filename,' ',fExtToolList[Index].CmdLineParams,'"']);
end;

procedure TExternalToolDialog.Load;
var
  i: integer;
begin
  Listbox.Items.BeginUpdate;
  Listbox.Items.Clear;
  for i:=0 to fExtToolList.Count-1 do 
    Listbox.Items.Add(ToolDescription(i));
  Listbox.Items.EndUpdate;
  EnableButtons;
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
  MsgResult:=ShowExtToolOptionDlg(fTransferMacros, NewTool, EditorOpts.KeyMap);
  if MsgResult=mrOk then
  begin
    fExtToolList.Add(NewTool);
    Listbox.Items.Add(ToolDescription(fExtToolList.Count-1));
  end else begin
    NewTool.Free;
  end;
  EnableButtons;
end;

procedure TExternalToolDialog.HelpButtonClick(Sender: TObject);
begin
  LazarusHelp.ShowHelpForIDEControl(Self);
end;

procedure TExternalToolDialog.MenuItemCloneClick(Sender: TObject);
var
  NewTool, OldTool: TExternalUserTool;
begin
  If Listbox.ItemIndex <> -1 Then Begin
    OldTool := fExtToolList.Items[Listbox.ItemIndex];
    If Assigned(OldTool) Then Begin
      NewTool:=TExternalUserTool.Create(nil);
      NewTool.Assign(OldTool);
      fExtToolList.Add(NewTool);
      Listbox.Items.Add(ToolDescription(fExtToolList.Count-1));
    end;
  end;
end;

procedure TExternalToolDialog.MenuItemExportClick(Sender: TObject);
Var
  FileConfig : TXMLOptionsStorage;
  AFileName : String;
begin
  If SaveDialog1.Execute Then Begin
    AFileName := SaveDialog1.FileName;
    Case SaveDialog1.FilterIndex Of
      1 : Begin
            AFileName := ChangeFileExt(AFileName, '.xml');
          end;
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
begin
  if Listbox.ItemIndex<0 then exit;
  fExtToolList.Delete(Listbox.ItemIndex);
  ListBox.Items.Delete(Listbox.ItemIndex);
  EnableButtons;
end;

procedure TExternalToolDialog.EditButtonClick(Sender: TObject);
var
  i: LongInt;
begin
  i:=Listbox.ItemIndex;
  if i<0 then exit;
  if ShowExtToolOptionDlg(fTransferMacros,fExtToolList[i],EditorOpts.KeyMap)=mrOk
  then begin
    Listbox.Items[i]:=ToolDescription(i);
    EnableButtons;
  end;
end;

procedure TExternalToolDialog.MoveUpButtonClick(Sender: TObject);
var
  i: integer;
begin
  i:=Listbox.ItemIndex;
  if i<1 then exit;
  fExtToolList.Move(i,i-1);
  Listbox.Items.Move(i,i-1);
  Listbox.ItemIndex:=i-1;
  EnableButtons;
end;

procedure TExternalToolDialog.MoveDownButtonClick(Sender: TObject);
var
  i: integer;
begin
  i:=Listbox.ItemIndex;
  if (i<0) or (i>=Listbox.Items.Count-1) then exit;
  fExtToolList.Move(i,i+1);
  Listbox.Items.Move(i,i+1);
  Listbox.ItemIndex:=i+1;
  EnableButtons;
end;

procedure TExternalToolDialog.EnableButtons;
var
  i: integer;
begin
  i:=Listbox.ItemIndex;
  AddButton.Enabled:=fExtToolList.Count<MaxExtTools;
  RemoveButton.Enabled:=(i>=0);
  EditButton.Enabled:=(i>=0);
  MoveUpButton.Enabled:=(i>0);
  MoveDownButton.Enabled:=(i>=0) and (i<fExtToolList.Count-1);
end;

procedure TExternalToolDialog.ListboxClick(Sender: TObject);
begin
  EnableButtons;
end;

end.
