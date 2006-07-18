{ Copyright (C) 2006 Mattias Gaertner

  This source is free software; you can redistribute it and/or modify it under
  the terms of the GNU General Public License as published by the Free
  Software Foundation; either version 2 of the License, or (at your option)
  any later version.

  This code is distributed in the hope that it will be useful, but WITHOUT ANY
  WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
  FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
  details.

  A copy of the GNU General Public License is available on the World Wide Web
  at <http://www.gnu.org/copyleft/gpl.html>. You can also obtain it by writing
  to the Free Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
  MA 02111-1307, USA.
}
unit H2PasDlg;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LCLProc, LCLType, LResources, Forms, Controls, Graphics,
  Dialogs, ComCtrls, Buttons, H2PasStrConsts,
  TextTools, MenuIntf, IDECommands, StdCtrls, ExtCtrls, CheckLst, SynEdit;

type

  { TH2PasDialog }

  TH2PasDialog = class(TForm)
    AddCHeaderFilesButton: TButton;
    CHeaderFilesSplitter1: TSplitter;
    UnselectAllCHeaderFilesButton: TButton;
    SelectAllCHeaderFilesButton: TButton;
    DeleteCHeaderFilesButton: TButton;
    CHeaderFilesCheckListBox: TCheckListBox;
    MainPageControl: TPageControl;
    FilesTabSheet: TTabSheet;
    procedure FormCreate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  H2PasDialog: TH2PasDialog = nil;
  CmdH2PasTool: TIDECommand = nil;

procedure ExecuteH2PasTool(Sender: TObject);
  
procedure Register;

implementation

procedure ExecuteH2PasTool(Sender: TObject);
begin
  if H2PasDialog<>nil then exit;
  H2PasDialog:=TH2PasDialog.Create(nil);
  try
    H2PasDialog.ShowModal;
  finally
    FreeAndNil(H2PasDialog);
  end;
end;

procedure Register;
var
  Key: TIDEShortCut;
  Cat: TIDECommandCategory;
begin
  Key := IDEShortCut(VK_UNKNOWN,[],VK_UNKNOWN,[]);

  Cat:=IDECommandList.FindCategoryByName(CommandCategoryToolMenuName);
  CmdH2PasTool := RegisterIDECommand(Cat                 ,
         h2pH2Pas, h2pCreateUnitsFromCHeaderFiles, Key, nil, @ExecuteH2PasTool);
  RegisterIDEMenuCommand(itmSecondaryTools, h2pH2PasTool, h2pH2Pas, nil, nil,
                         CmdH2PasTool);
end;

{ TH2PasDialog }

procedure TH2PasDialog.FormCreate(Sender: TObject);
begin
  Caption:=h2pCHeaderFileConverter;
  FilesTabSheet.Caption:='C header files';
end;

procedure TH2PasDialog.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (Key=VK_ESCAPE) and (Shift=[]) then ModalResult:=mrCancel;
end;

initialization
  {$I h2pasdlg.lrs}

end.

