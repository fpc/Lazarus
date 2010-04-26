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
 *  See the file COPYING.modifiedLGPL.txt, included in this distribution,    *
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
    ldcedrDockPage,
    ldcedrEnlargeLeft,
    ldcedrEnlargeTop,
    ldcedrEnlargeRight,
    ldcedrEnlargeBottom
    );

  { TLazDockControlEditorDlg }

  TLazDockControlEditorDlg = class(TForm)
    CancelButton: TButton;
    DockControlComboBox: TComboBox;
    DockPageButton: TSpeedButton;
    DockBottomButton: TSpeedButton;
    DockTopButton: TSpeedButton;
    DockRightButton: TSpeedButton;
    DockLeftButton: TSpeedButton;
    DockGroupBox: TGroupBox;
    DockControlLabel: TLabel;
    EnlargeGroupBox: TGroupBox;
    EnlargeLeftSpeedButton: TSpeedButton;
    EnlargeRightSpeedButton: TSpeedButton;
    EnlargeTopSpeedButton: TSpeedButton;
    EnlargeBottomSpeedButton: TSpeedButton;
    UndockButton: TButton;
    UndockGroupBox: TGroupBox;
    procedure DockBottomButtonClick(Sender: TObject);
    procedure DockControlComboBoxEditingDone(Sender: TObject);
    procedure DockLeftButtonClick(Sender: TObject);
    procedure DockPageButtonClick(Sender: TObject);
    procedure DockRightButtonClick(Sender: TObject);
    procedure DockTopButtonClick(Sender: TObject);
    procedure EnlargeBottomSpeedButtonClick(Sender: TObject);
    procedure EnlargeLeftSpeedButtonClick(Sender: TObject);
    procedure EnlargeRightSpeedButtonClick(Sender: TObject);
    procedure EnlargeTopSpeedButtonClick(Sender: TObject);
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

{$ifndef ver2_2}
{$R *.lfm}
{$ENDIF}

{ TLazDockControlEditorDlg }

procedure TLazDockControlEditorDlg.FormCreate(Sender: TObject);
begin
  Caption := 'Docking';
  
  UndockGroupBox.Caption := 'Undock';
  UndockButton.Caption := 'Undock (make it a single, normal window)';

  DockPageButton.Hint := 'Dock as page';
  DockBottomButton.Hint := 'Dock to bottom';
  DockTopButton.Hint := 'Dock to top';
  DockRightButton.Hint := 'Dock to right';
  DockLeftButton.Hint := 'Dock to left';

  DockPageButton.LoadGlyphFromLazarusResource('lcl_dock_to_page');
  DockBottomButton.LoadGlyphFromLazarusResource('lcl_dock_to_bottom');
  DockTopButton.LoadGlyphFromLazarusResource('lcl_dock_to_top');
  DockRightButton.LoadGlyphFromLazarusResource('lcl_dock_to_right');
  DockLeftButton.LoadGlyphFromLazarusResource('lcl_dock_to_left');

  DockGroupBox.Caption := 'Dock to control';
  DockControlLabel.Caption := 'To control';

  EnlargeGroupBox.Caption := 'Enlarge one side';
  EnlargeLeftSpeedButton.Hint := 'Left';
  EnlargeTopSpeedButton.Hint := 'Top';
  EnlargeRightSpeedButton.Hint := 'Right';
  EnlargeBottomSpeedButton.Hint := 'Bottom';

  CancelButton.Caption := 'Cancel';
  
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

procedure TLazDockControlEditorDlg.EnlargeBottomSpeedButtonClick(Sender: TObject
  );
begin
  CheckSetDlgResult(ldcedrEnlargeBottom);
end;

procedure TLazDockControlEditorDlg.EnlargeLeftSpeedButtonClick(Sender: TObject);
begin
  CheckSetDlgResult(ldcedrEnlargeLeft);
end;

procedure TLazDockControlEditorDlg.EnlargeRightSpeedButtonClick(Sender: TObject
  );
begin
  CheckSetDlgResult(ldcedrEnlargeRight);
end;

procedure TLazDockControlEditorDlg.EnlargeTopSpeedButtonClick(Sender: TObject);
begin
  CheckSetDlgResult(ldcedrEnlargeTop);
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
  {$I lcl_dock_to_images.lrs}

end.
