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

{$mode objfpc}{$H+}

interface

uses
  // RTL, FCL
  SysUtils, Classes,
  Windows,
  // LCL
  ShellCtrls, Forms, Dialogs, FileCtrl, Controls, ComCtrls,
  LResources, ExtCtrls, Buttons, Graphics,
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
  public
    // User interface
    ShellTreeView: TShellTreeView;
    ShellListView: TShellListView;
    Panel: TPanel;
    // Communication fields
    LCLDialog: TFileDialog;
    procedure HandleOkClick(ASender: TObject);
    procedure HandleCancelClick(ASender: TObject);
  end;

{ TWinCEFileDialogForm }

procedure TWinCEFileDialogForm.HandleOkClick(ASender: TObject);
begin
  LCLDialog.FileName := ShellListView.GetPathFromItem(ShellListView.Selected);
  ModalResult := mrOk;
end;

procedure TWinCEFileDialogForm.HandleCancelClick(ASender: TObject);
begin
  ModalResult := mrCancel;
end;

{ TWinCEWSFileDialog }

class function TWinCEWSFileDialog.CreateHandle(const ACommonDialog: TCommonDialog): THandle;
var
  ResultForm: TWinCEFileDialogForm absolute Result;
  AButton: TBitBtn;
  AImage: TPortableNetworkGraphic;
begin
  Result := THandle(TWinCEFileDialogForm.Create(Application));

  ResultForm.LCLDialog := TFileDialog(ACommonDialog);

  // Add the Panel to the dialog (Toolbar didn't work well)
  ResultForm.Panel := TPanel.Create(ResultForm);
  ResultForm.Panel.Parent := ResultForm;
  ResultForm.Panel.Left := 0;
  ResultForm.Panel.Height := 20;
  ResultForm.Panel.Top := ResultForm.Height -
    ResultForm.Panel.Height;
  ResultForm.Panel.Width := ResultForm.Width;
  ResultForm.Panel.Align := alBottom;

  AImage := TPortableNetworkGraphic.Create;

  // ok button
  AButton := TBitBtn.Create(ResultForm);
  AButton.Parent := ResultForm.Panel;
  AButton.Height := 17;
  AButton.Width := 17;
  AImage.LoadFromLazarusResource('wincedialog_ok');
  AButton.Glyph.Assign(AImage);
  AButton.OnClick := @ResultForm.HandleOkClick;
  AButton.Left := 0;

  // cancel button
  AButton := TBitBtn.Create(ResultForm);
  AButton.Parent := ResultForm.Panel;
  AButton.Height := 17;
  AButton.Width := 17;
  AImage.LoadFromLazarusResource('wincedialog_cancel');
  AButton.Glyph.Assign(AImage);
  AButton.OnClick := @ResultForm.HandleCancelClick;
  AButton.Left := 20;

  // dialog images
  // the wincedialogs.lrs image is compiled with the script at
  // lcl/images/wince/build.bat
  //ResultForm.ToolBar.Images := TImageList.Create(ResultForm);
  //ResultForm.ToolBar.Images.AddLazarusResource('wincedialog_ok');
  //ResultForm.ToolBar.Images.AddLazarusResource('wincedialog_cancel');
  AImage.Free;

  // Add the ShellTreeView to the dialog
  ResultForm.ShellTreeView := TShellTreeView.Create(ResultForm);
  ResultForm.ShellTreeView.Parent := ResultForm;
  ResultForm.ShellTreeView.Left := 0;
  ResultForm.ShellTreeView.Top := 0;
  ResultForm.ShellTreeView.Width := ResultForm.Width;
  ResultForm.ShellTreeView.Height := 100;
  ResultForm.ShellTreeView.Align := alTop;

  // Add the ShellListView to the dialog
  ResultForm.ShellListView := TShellListView.Create(ResultForm);
  ResultForm.ShellListView.Parent := ResultForm;
  ResultForm.ShellListView.Left := 0;
  ResultForm.ShellListView.Top := ResultForm.ShellTreeView.Height;
  ResultForm.ShellListView.Width := ResultForm.Width;
  ResultForm.ShellListView.Height :=
    ResultForm.Height - ResultForm.ShellTreeView.Height
    - ResultForm.Panel.Height;
  ResultForm.ShellListView.ShellTreeView := ResultForm.ShellTreeView;
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
end;

initialization
  {$I wincedialogs.lrs}
end.
