{
 *****************************************************************************
 *                             WinCEWSDialogs.pp                             *
 *                             -----------------                             *
 *                                                                           *
 *                                                                           *
 *****************************************************************************

 *****************************************************************************
 *                                                                           *
 *  This file is part of the Lazarus Component Library (LCL)                 *
 *                                                                           *
 *  See the file COPYING.modifiedLGPL.txt, included in this distribution,    *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************
}
unit WinCEWSDialogs;

{$mode delphi}

interface

uses
  // RTL, FCL
  SysUtils, Classes,
  Windows,
  // LCL
  ShellCtrls, Forms, Dialogs, FileCtrl, Controls, ComCtrls,
  LResources, ExtCtrls, Buttons, Graphics, StdCtrls,
  LCLStrConsts, FileUtil,
  // Widgetset
  WSDialogs, WSLCLClasses, InterfaceBase;

type
  { TWinCEWSCommonDialog }

  TWinCEWSCommonDialog = class(TWSCommonDialog)
  published
//    class function CreateHandle(const ACommonDialog: TCommonDialog): THandle; override;
//    class procedure DestroyHandle(const ACommonDialog: TCommonDialog); override;
  end;

  { TWinCEWSFileDialog }

  TWinCEWSFileDialog = class(TWSFileDialog)
  published
    class function CreateHandle(const ACommonDialog: TCommonDialog): THandle; override;
    class procedure DestroyHandle(const ACommonDialog: TCommonDialog); override;
    class procedure ShowModal(const ACommonDialog: TCommonDialog); override;
  end;

  { TWinCEWSOpenDialog }

  TWinCEWSOpenDialog = class(TWSOpenDialog)
  published
//    class function CreateHandle(const ACommonDialog: TCommonDialog): THandle; override;
//    class procedure DestroyHandle(const ACommonDialog: TCommonDialog); override;
//    class procedure ShowModal(const ACommonDialog: TCommonDialog); override;
  end;

  { TWinCEWSSaveDialog }

  TWinCEWSSaveDialog = class(TWSSaveDialog)
  published
//    class procedure ShowModal(const ACommonDialog: TCommonDialog); override;
  end;

  { TWinCEWSSelectDirectoryDialog }

  TWinCEWSSelectDirectoryDialog = class(TWSSelectDirectoryDialog)
  published
//    class function CreateHandle(const ACommonDialog: TCommonDialog): THandle; override;
  end;

  { TWinCEWSColorDialog }

  TWinCEWSColorDialog = class(TWSColorDialog)
  published
//    class function CreateHandle(const ACommonDialog: TCommonDialog): THandle; override;
//    class procedure ShowModal(const ACommonDialog: TCommonDialog); override;
//    class procedure DestroyHandle(const ACommonDialog: TCommonDialog); override;
  end;

  { TWinCEWSColorButton }

  TWinCEWSColorButton = class(TWSColorButton)
  published
  end;

  { TWinCEWSFontDialog }

  TWinCEWSFontDialog = class(TWSFontDialog)
  published
//    class function CreateHandle(const ACommonDialog: TCommonDialog): THandle; override;
  end;

implementation

type

  { TWinCEFileDialogForm }

  TWinCEFileDialogForm = class(TForm)
  private
    procedure SetFilter();
  public
    // User interface
    ShellTreeView: TShellTreeView;
    ShellListView: TShellListView;
    Panel: TPanel;
    OkButton: TBitBtn;
    CancelButton: TBitBtn;
    SaveEdit: TEdit;
    FilterComboBox: TFilterComboBox;
    // Communication fields
    LCLDialog: TFileDialog;
    constructor Create(AOwner: TComponent; ALCLDialog: TFileDialog);
    procedure HandleOkClick(ASender: TObject);
    procedure HandleCancelClick(ASender: TObject);
    procedure HandleCloseQuery(Sender : TObject; var CanClose : boolean);
    procedure HandleEditChange(ASender: TObject);
    procedure HandleSelectItem(Sender: TObject;
     Item: TListItem; Selected: Boolean);
  end;

{ TWinCEFileDialogForm }

procedure TWinCEFileDialogForm.SetFilter();
begin
  if LCLDialog.Filter = '' then
    FilterComboBox.Filter := Format(rsAllFiles,[GetAllFilesMask, GetAllFilesMask,''])
  else
    FilterComboBox.Filter := LCLDialog.Filter;
end;

{
  The size of the window is determined only when creating the
  handle, so any reference to TForm.Width and TForm.Height
  here doesnt correspond to the final value.
}
constructor TWinCEFileDialogForm.Create(AOwner: TComponent; ALCLDialog: TFileDialog);
var
  AImage: TPortableNetworkGraphic;
begin
  inherited Create(AOwner);

  LCLDialog := ALCLDialog;

  {$ifdef VerboseWinCE}
    DebugLn(':>TWinCEFileDialogForm.Create Width=', Width,
     ' Height=', Height);
  {$endif}

  // Add the Panel to the dialog (Toolbar didn't work well)
  Panel := TPanel.Create(Self);
  Panel.Parent := Self;
  Panel.Left := 0;
  Panel.Height := 20;
  Panel.Top := Height - Panel.Height;
  Panel.Width := Width;
  Panel.Align := alBottom;

  AImage := TPortableNetworkGraphic.Create;

  // ok button
  OkButton := TBitBtn.Create(Panel);
  OkButton.Parent := Panel;
  OkButton.Height := 17;
  OkButton.Width := 17;
  AImage.LoadFromLazarusResource('wincedialog_ok');
  OkButton.Glyph.Assign(AImage);
  OkButton.OnClick := HandleOkClick;
  OkButton.Left := 0;

  // cancel button
  CancelButton := TBitBtn.Create(Panel);
  CancelButton.Parent := Panel;
  CancelButton.Height := 17;
  CancelButton.Width := 17;
  AImage.LoadFromLazarusResource('wincedialog_cancel');
  CancelButton.Glyph.Assign(AImage);
  CancelButton.OnClick := HandleCancelClick;
  CancelButton.Left := 20;

  // dialog images
  // the wincedialogs.lrs image is compiled with the script at
  // lcl/images/wince/build.bat
  //ToolBar.Images := TImageList.Create(Self);
  //ToolBar.Images.AddLazarusResource('wincedialog_ok');
  //ToolBar.Images.AddLazarusResource('wincedialog_cancel');
  AImage.Free;

  // Add the ShellTreeView to the dialog
  ShellTreeView := TShellTreeView.Create(Self);
  ShellTreeView.Parent := Self;
  ShellTreeView.Left := 0;
  ShellTreeView.Top := 0;
  ShellTreeView.Width := Width;
  ShellTreeView.Height := 100;
  ShellTreeView.Align := alTop;

  // Add the ShellListView to the dialog
  ShellListView := TShellListView.Create(Self);
  ShellListView.Parent := Self;
  ShellListView.Left := 0;
  ShellListView.Top := ShellTreeView.Height;
  ShellListView.Width := Width;
  ShellListView.Height := Height - ShellTreeView.Height - Panel.Height;
  ShellListView.Align := alClient;
  ShellListView.ShellTreeView := ShellTreeView;
  ShellListView.ScrollBars := ssVertical;
  ShellListView.OnSelectItem := HandleSelectItem;

  // TEdit for save dialog
  if LCLDialog is TSaveDialog then
  begin
    SaveEdit := TEdit.Create(Self);
    SaveEdit.Parent := Self;
    SaveEdit.Left := 0;
    SaveEdit.Height := 20;
    SaveEdit.Top := Height - Panel.Height - SaveEdit.Height;
    SaveEdit.Width := Width;
    SaveEdit.Align := alBottom;
    SaveEdit.Text := SysUtils.ExtractFileName(LCLDialog.FileName);
    SaveEdit.OnChange := HandleEditChange;
  end;

  // TFilterComboBox
  FilterComboBox := TFilterComboBox.Create(Self);
  FilterComboBox.Parent := Self;
  FilterComboBox.Left := 0;
  FilterComboBox.Height := 20;
  FilterComboBox.Top := Height - Panel.Height - FilterComboBox.Height;
  if SaveEdit <> nil then
    FilterComboBox.Top := FilterComboBox.Top - SaveEdit.Height;
  FilterComboBox.Width := Width;
  FilterComboBox.Align := alBottom;
  SetFilter();
  FilterComboBox.ShellListView := ShellListView;

  // In the save dialog it is enabled when there is a text in the TEdit
  if (LCLDialog is TSaveDialog) then
    OkButton.Enabled := SaveEdit.Text <> ''
  // In a TOpenDialog the Ok button is only enabled when a file is selected
  else
    OkButton.Enabled := False;

  // Form events
  OnCloseQuery := HandleCloseQuery;
end;

// The Ok button code should be only a simple mrOk,
// because there is the dialog Ok button, which will
// always be active and will set the ModalResult to mrOk
// so the code needs to affect it too, and this can be
// done in CloseQuery
procedure TWinCEFileDialogForm.HandleOkClick(ASender: TObject);
begin
  ModalResult := mrOk;
end;

procedure TWinCEFileDialogForm.HandleCancelClick(ASender: TObject);
begin
  ModalResult := mrCancel;
end;

procedure TWinCEFileDialogForm.HandleCloseQuery(Sender: TObject;
  var CanClose: boolean);
begin
  if ModalResult = mrCancel then
  begin
    CanClose := True;
    Exit;
  end;

  CanClose := False;

  if (LCLDialog is TSaveDialog) then
  begin
    if SaveEdit.Text = '' then Exit;

    LCLDialog.FileName := ShellTreeView.GetPathFromNode(ShellTreeView.Selected);
    LCLDialog.FileName := IncludeTrailingPathDelimiter(LCLDialog.FileName);
    LCLDialog.FileName := LCLDialog.FileName + SaveEdit.Text;
    CanClose := True;
  end
  else
  begin
    if ShellListView.Selected = nil then Exit;

    LCLDialog.FileName := ShellListView.GetPathFromItem(ShellListView.Selected);
    CanClose := True;
  end;
end;

procedure TWinCEFileDialogForm.HandleEditChange(ASender: TObject);
begin
  OkButton.Enabled := SaveEdit.Text <> '';
end;

procedure TWinCEFileDialogForm.HandleSelectItem(Sender: TObject;
  Item: TListItem; Selected: Boolean);
begin
  // Selecting an item changes the filename in the TEdit
  // in save dialogs
  if (LCLDialog is TSaveDialog) and Selected then
  begin
    SaveEdit.Text := Item.Caption;
  end
  // In the OpenDialog the state of the Ok button is dependent
  // on the selection of an item
  else
  begin
    OkButton.Enabled := Selected;
  end;
end;

{ TWinCEWSFileDialog }

class function TWinCEWSFileDialog.CreateHandle(const ACommonDialog: TCommonDialog): THandle;
var
  ResultForm: TWinCEFileDialogForm absolute Result;
begin
  Result := THandle(TWinCEFileDialogForm.Create(Application,
    TFileDialog(ACommonDialog)));
end;

class procedure TWinCEWSFileDialog.DestroyHandle(const ACommonDialog: TCommonDialog);
var
  ResultForm: TWinCEFileDialogForm;
begin
  ResultForm := TWinCEFileDialogForm(ACommonDialog.Handle);

  ResultForm.Free;
end;

class procedure TWinCEWSFileDialog.ShowModal(const ACommonDialog: TCommonDialog);
var
  ResultForm: TWinCEFileDialogForm;
begin
  ResultForm := TWinCEFileDialogForm(ACommonDialog.Handle);

  ResultForm.ShowModal;

  // Without setting UserChoice the app will be locked
  ACommonDialog.UserChoice := ResultForm.ModalResult;
end;

initialization
  {$I wincedialogs.lrs}
end.
