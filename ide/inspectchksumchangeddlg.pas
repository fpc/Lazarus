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
 *   Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.        *
 *                                                                         *
 ***************************************************************************

  Author: Mattias Gaertner

  Abstract:
    Dialog to show information about the message
      "recompiling unit1, checksum changed for unit2"

  ToDo:
    - show the location(s) of the first unit
    - show the location(s) of the second unit
    - actions:
      - open a source file
      - open a package
      - delete a ppu+o file
      - recompile a package clean (remove the .compiled file)
}
unit InspectChksumChangedDlg;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LCLProc, FileUtil, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ExtCtrls, ComCtrls, ButtonPanel,
  // codetools
  CodeCache,
  // IDEIntf
  LazIDEIntf, TextTools, IDEMsgIntf, PackageIntf,
  LazarusIDEStrConsts;

type
  TInspectChksumChgDialog = class;

  { TICCAction }

  TICCAction = class
  public
    Dlg: TInspectChksumChgDialog;
    Caption: string;
    constructor Create(aDlg: TInspectChksumChgDialog; aCaption: string);
  end;

  { TInspectChksumChgDialog }

  TInspectChksumChgDialog = class(TForm)
    ButtonPanel1: TButtonPanel;
    InfoGroupBox: TGroupBox;
    ProgressBar1: TProgressBar;
    ActionsRadioGroup: TRadioGroup;
    Splitter1: TSplitter;
    InfoTreeView: TTreeView;
    procedure CancelClick(Sender: TObject);
    procedure OkClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FMsg: string;
    FUnit1: string;
    FUnit2: string;
  public
    procedure InitWithMsg(aMsg: TIDEMessageLine);
    property Msg: string read FMsg;
    property Unit1: string read FUnit1;
    property Unit2: string read FUnit2;
  end;

  { TQuickFixRecompilingChecksumChanged }

  TQuickFixRecompilingChecksumChanged = class(TIDEMsgQuickFixItem)
  public
    constructor Create;
    function IsApplicable(Line: TIDEMessageLine): boolean; override;
    procedure Execute(const Msg: TIDEMessageLine; Step: TIMQuickFixStep); override;
  end;

procedure InitInspectChecksumChangedQuickFixItems;

implementation

procedure InitInspectChecksumChangedQuickFixItems;
begin
  RegisterIDEMsgQuickFix(TQuickFixRecompilingChecksumChanged.Create);
end;

{ TICCAction }

constructor TICCAction.Create(aDlg: TInspectChksumChgDialog; aCaption: string);
begin
  Dlg:=aDlg;
  Caption:=aCaption;
end;

{$R *.lfm}

{ TInspectChksumChgDialog }

procedure TInspectChksumChgDialog.FormCreate(Sender: TObject);
begin
  Caption:='Inspect checksum changed message';
  InfoGroupBox.Caption:='Hints:';
  ActionsRadioGroup.Caption:='Actions';

  ButtonPanel1.OKButton.OnClick:=@OkClick;
  ButtonPanel1.CancelButton.OnClick:=@CancelClick;
end;

procedure TInspectChksumChgDialog.OkClick(Sender: TObject);
begin
  ModalResult:=mrOK;
end;

procedure TInspectChksumChgDialog.CancelClick(Sender: TObject);
begin
  ModalResult:=mrCancel;
end;

procedure TInspectChksumChgDialog.FormDestroy(Sender: TObject);
begin

end;

procedure TInspectChksumChgDialog.InitWithMsg(aMsg: TIDEMessageLine);
begin
  FMsg:=aMsg.Msg;
  REMatches(Msg,'Recompiling ([a-z_][a-z_0-9]*), checksum changed for ([a-z_][a-z_0-9]*)','i');
  FUnit1:=REVar(1);
  FUnit2:=REVar(2);

  InfoTreeView.BeginUpdate;
  InfoTreeView.Items.Clear;

  InfoTreeView.Items.Add(nil,'Message: '+dbgstr(Msg));
  InfoTreeView.Items.Add(nil,'ToDo: search for unit '+Unit1);
  InfoTreeView.Items.Add(nil,'ToDo: search for unit '+Unit2);
  InfoTreeView.Items.Add(nil,'ToDo: search for ppu files of '+Unit2);

  InfoTreeView.EndUpdate;
end;

{ TQuickFixRecompilingChecksumChanged }

constructor TQuickFixRecompilingChecksumChanged.Create;
begin
  Name:='Show dialog for message Recompiling Unit1, checksum changed for Unit1';
  Caption:='Explore message "checksum changed"';
  Steps:=[imqfoMenuItem];
end;

function TQuickFixRecompilingChecksumChanged.IsApplicable(Line: TIDEMessageLine
  ): boolean;
begin
  Result:=false;
  if not REMatches(Line.Msg,'Recompiling ([a-z_][a-z_0-9]*), checksum changed for ([a-z_][a-z_0-9]*)','i')
  then exit;
  Result:=true;
end;

procedure TQuickFixRecompilingChecksumChanged.Execute(
  const Msg: TIDEMessageLine; Step: TIMQuickFixStep);
var
  Dlg: TInspectChksumChgDialog;
begin
  if Step=imqfoMenuItem then begin
    debugln(['TQuickFixRecompilingChecksumChanged.Execute  ']);
    if not REMatches(Msg.Msg,'Recompiling ([a-z_][a-z_0-9]*), checksum changed for ([a-z_][a-z_0-9]*)','i')
    then exit;
    debugln(['TQuickFixRecompilingChecksumChanged.Execute Unit1=',REVar(1),', checksum changed for Unit2=',REVar(2)]);
    Dlg:=TInspectChksumChgDialog.Create(nil);
    try
      Dlg.InitWithMsg(Msg);
      Dlg.ShowModal;
    finally
      Dlg.Free;
    end;
  end;
end;

end.

