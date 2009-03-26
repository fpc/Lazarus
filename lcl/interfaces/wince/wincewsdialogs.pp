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
  // RTL, FCL, LCL
  SysUtils, Classes,
  Windows,
  // Widgetset
  ShellCtrls, Forms, Dialogs, FileCtrl, Controls,
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
  TWinCEFileDialogForm = class(TForm)
    ShellTreeView: TShellTreeView;
    FileListBox: TFileListBox;
  end;

{ TWinCEWSFileDialog }

class function TWinCEWSFileDialog.CreateHandle(const ACommonDialog: TCommonDialog): THandle;
var
  ResultForm: TWinCEFileDialogForm absolute Result;
  ShellTreeView: TShellTreeView;
  FileListBox: TFileListBox;
begin
  Result := THandle(TWinCEFileDialogForm.Create(Application));

//  ResultForm.

  // Add the ShellTreeView to the dialog
  ShellTreeView := TShellTreeView.Create(ResultForm);
  ResultForm.ShellTreeView := ShellTreeView;
  ShellTreeView.Parent := ResultForm;
  ShellTreeView.Left := 0;
  ShellTreeView.Top := 0;
  ShellTreeView.Width := ResultForm.Width;
  ShellTreeView.Height := 100;
  ShellTreeView.Align := alTop;

  // Add the FileListView to the dialog
  FileListBox := TFileListBox.Create(ResultForm);
  ResultForm.FileListBox := FileListBox;
  FileListBox.Parent := ResultForm;
  FileListBox.Left := 0;
  FileListBox.Top := ShellTreeView.Height;
  FileListBox.Width := ResultForm.Width;
  FileListBox.Height := 100;
end;

class procedure TWinCEWSFileDialog.DestroyHandle(const ACommonDialog: TCommonDialog);
var
  ResultForm: TWinCEFileDialogForm absolute ACommonDialog.Handle;
begin
  ResultForm.ShellTreeView.Free;
  ResultForm.FileListBox.Free;
  ResultForm.Free;
end;

class procedure TWinCEWSFileDialog.ShowModal(const ACommonDialog: TCommonDialog);
var
  ResultForm: TWinCEFileDialogForm absolute ACommonDialog.Handle;
begin
  ResultForm.ShowModal;
end;

initialization

////////////////////////////////////////////////////
// I M P O R T A N T
////////////////////////////////////////////////////
// To improve speed, register only classes
// which actually implement something
////////////////////////////////////////////////////
//  RegisterWSComponent(TCommonDialog, TWinCEWSCommonDialog);
  RegisterWSComponent(TFileDialog, TWinCEWSFileDialog);
//  RegisterWSComponent(TOpenDialog, TWinCEWSOpenDialog);
//  RegisterWSComponent(TSaveDialog, TWinCEWSSaveDialog);
//  RegisterWSComponent(TSelectDirectoryDialog, TWinCEWSSelectDirectoryDialog);
//  RegisterWSComponent(TColorDialog, TWinCEWSColorDialog);
//  RegisterWSComponent(TColorButton, TWinCEWSColorButton);
//  RegisterWSComponent(TFontDialog, TWinCEWSFontDialog);
////////////////////////////////////////////////////

end.
