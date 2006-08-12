{  $Id: ldocktree.pas 8153 2005-11-14 21:53:06Z mattias $  }
{
 /***************************************************************************
                             LDockCtrlEdit.pas
                             -----------------

 ***************************************************************************/

 *****************************************************************************
 *                                                                           *
 *  This file is part of the Lazarus Component Library (LCL)                 *
 *                                                                           *
 *  See the file COPYING.modifiedLGPL, included in this distribution,        *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************

  Author: Mattias Gaertner

  Abstract:
    This unit contains a dialog to dock or undock a control to another.
}
unit LDockCtrlEdit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, Buttons,
  StdCtrls;

type
  TLazDockControlEditorDlgResult = (
    ldcedrNone,
    ldcedrUndock,
    ldcedrDockLeft,
    ldcedrDockRight,
    ldcedrDockTop,
    ldcedrDockBottom,
    ldcedrDockPage
    );

  { TLazDockControlEditorDlg }

  TLazDockControlEditorDlg = class(TForm)
    CancelButton: TButton;
    DockControlComboBox: TComboBox;
    DockPageButton: TButton;
    DockBottomButton: TButton;
    DockTopButton: TButton;
    DockRightButton: TButton;
    DockLeftButton: TButton;
    DockGroupBox: TGroupBox;
    DockControlLabel: TLabel;
    UndockButton: TButton;
    UndockGroupBox: TGroupBox;
    procedure DockBottomButtonClick(Sender: TObject);
    procedure DockControlComboBoxEditingDone(Sender: TObject);
    procedure DockLeftButtonClick(Sender: TObject);
    procedure DockPageButtonClick(Sender: TObject);
    procedure DockRightButtonClick(Sender: TObject);
    procedure DockTopButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure UndockButtonClick(Sender: TObject);
  private
    FCurrentControlName: string;
    FDlgResult: TLazDockControlEditorDlgResult;
    procedure CheckSetDlgResult(NewDlgResult: TLazDockControlEditorDlgResult);
    procedure SetCurrentControlName(const AValue: string);
    procedure UpdateButtonEnabled;
  public
    property DlgResult: TLazDockControlEditorDlgResult read FDlgResult write FDlgResult;
    property CurrentControlName: string read FCurrentControlName write SetCurrentControlName;
  end;

implementation

{ TLazDockControlEditorDlg }

procedure TLazDockControlEditorDlg.FormCreate(Sender: TObject);
begin
  Caption:='Docking';
  
  UndockGroupBox.Caption:='Undock';
  UndockButton.Caption:='Undock (make it a single, normal window)';

  DockPageButton.Caption:='Dock as page';
  DockBottomButton.Caption:='Dock to bottom';
  DockTopButton.Caption:='Dock to top';
  DockRightButton.Caption:='Dock to right';
  DockLeftButton.Caption:='Dock to left';
  DockGroupBox.Caption:='Dock to control';
  DockControlLabel.Caption:='To control';
  
  CancelButton.Caption:='Cancel';
  
  UpdateButtonEnabled;
end;

procedure TLazDockControlEditorDlg.DockLeftButtonClick(Sender: TObject);
begin
  CheckSetDlgResult(ldcedrDockLeft);
end;

procedure TLazDockControlEditorDlg.DockPageButtonClick(Sender: TObject);
begin
  CheckSetDlgResult(ldcedrDockPage);
end;

procedure TLazDockControlEditorDlg.DockBottomButtonClick(Sender: TObject);
begin
  CheckSetDlgResult(ldcedrDockBottom);
end;

procedure TLazDockControlEditorDlg.DockControlComboBoxEditingDone(
  Sender: TObject);
begin
  UpdateButtonEnabled;
end;

procedure TLazDockControlEditorDlg.DockRightButtonClick(Sender: TObject);
begin
  CheckSetDlgResult(ldcedrDockRight);
end;

procedure TLazDockControlEditorDlg.DockTopButtonClick(Sender: TObject);
begin
  CheckSetDlgResult(ldcedrDockTop);
end;

procedure TLazDockControlEditorDlg.UndockButtonClick(Sender: TObject);
begin
  CheckSetDlgResult(ldcedrUndock);
end;

procedure TLazDockControlEditorDlg.CheckSetDlgResult(
  NewDlgResult: TLazDockControlEditorDlgResult);
begin
  if NewDlgResult in [ldcedrDockLeft,ldcedrDockRight,ldcedrDockTop,
    ldcedrDockBottom,ldcedrDockPage] then
  begin
    if DockControlComboBox.Items.IndexOf(DockControlComboBox.Text)<0 then
    begin
      MessageDlg('Incomplete','Please select first a control,'
        +' to which '+CurrentControlName+' should be docked.',mtError,
        [mbCancel],0);
      exit;
    end;
  end;
  DlgResult:=NewDlgResult;
  ModalResult:=mrOk;
end;

procedure TLazDockControlEditorDlg.SetCurrentControlName(const AValue: string);
begin
  if FCurrentControlName=AValue then exit;
  FCurrentControlName:=AValue;
end;

procedure TLazDockControlEditorDlg.UpdateButtonEnabled;
var
  SelectionValid: Boolean;
begin
  SelectionValid:=DockControlComboBox.Items.IndexOf(DockControlComboBox.Text)>=0;
  DockPageButton.Enabled:=SelectionValid;
  DockBottomButton.Enabled:=SelectionValid;
  DockTopButton.Enabled:=SelectionValid;
  DockRightButton.Enabled:=SelectionValid;
  DockLeftButton.Enabled:=SelectionValid;
end;

initialization
  {$I ldockctrledit.lrs}

end.
