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
  LResources, ExtCtrls, Buttons, Graphics, StdCtrls,
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
    constructor Create(AOwner: TComponent); override;
    procedure HandleOkClick(ASender: TObject);
    procedure HandleCancelClick(ASender: TObject);
  end;

{ TWinCEFileDialogForm }

constructor TWinCEFileDialogForm.Create(AOwner: TComponent);
var
  AButton: TBitBtn;
  AImage: TPortableNetworkGraphic;
begin
  inherited Create(AOwner);

  {$ifdef VerboseWinCE}
    WriteLn(':>TWinCEFileDialogForm.Create Width=', Width,
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
  AButton := TBitBtn.Create(Panel);
  AButton.Parent := Panel;
  AButton.Height := 17;
  AButton.Width := 17;
  AImage.LoadFromLazarusResource('wincedialog_ok');
  AButton.Glyph.Assign(AImage);
  AButton.OnClick := @HandleOkClick;
  AButton.Left := 0;

  // cancel button
  AButton := TBitBtn.Create(Panel);
  AButton.Parent := Panel;
  AButton.Height := 17;
  AButton.Width := 17;
  AImage.LoadFromLazarusResource('wincedialog_cancel');
  AButton.Glyph.Assign(AImage);
  AButton.OnClick := @HandleCancelClick;
  AButton.Left := 20;

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
  ShellListView.ShellTreeView := ShellTreeView;
  ShellListView.ScrollBars := ssNone;
end;

procedure TWinCEFileDialogForm.HandleOkClick(ASender: TObject);
begin
  if ShellListView.Selected = nil then Exit;

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
begin
  Result := THandle(TWinCEFileDialogForm.Create(Application));

  ResultForm.LCLDialog := TFileDialog(ACommonDialog);
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
