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
}
unit editor_multiwindow_options;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  // LCL
  LCLType, StdCtrls, ExtCtrls, CheckLst, ComCtrls,
  // IdeIntf
  IDEOptionsIntf, IDEOptEditorIntf,
  // IDE
  EditorOptions, LazarusIDEStrConsts, SourceEditor;

type

  { TEditorMultiWindowOptionsFrame }

  TEditorMultiWindowOptionsFrame = class(TAbstractIDEOptionsEditor)
    Bevel1: TBevel;
    Bevel1a: TBevel;
    Bevel2a: TBevel;
    Bevel2: TBevel;
    CenterLabel: TLabel;
    chkShowFileNameInCaption: TCheckBox;
    chkMultiLine: TCheckBox;
    chkCtrlMiddleCloseOthers: TCheckBox;
    chkUseTabHistory: TCheckBox;
    chkShowCloseBtn: TCheckBox;
    chkShowNumbers: TCheckBox;
    chkHideSingleTab: TCheckBox;
    EditorTabPositionCheckBox: TComboBox;
    EditorTabPositionLabel: TLabel;
    lblAccessTypeDesc: TLabel;
    lblMultiWinTabSection: TLabel;
    listAccessType: TCheckListBox;
    AccessTypePanel: TPanel;
    lblEditActivationOrderSection: TLabel;
    lblAccessOrder: TLabel;
    lblAccessType: TLabel;
    Panel1: TPanel;
    pnlNBTabs: TPanel;
    radioAccessOrderEdit: TRadioButton;
    radioAccessOrderWin: TRadioButton;
    Panel2: TPanel;
    Splitter1: TSplitter;
    procedure listAccessTypeClickCheck(Sender: TObject);
    procedure listAccessTypeKeyUp(Sender: TObject; var {%H-}Key: Word; {%H-}Shift: TShiftState);
    procedure radioAccessOrderEditChange(Sender: TObject);
  private
    FMultiWinEditAccessOrder: TEditorOptionsEditAccessOrderList;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function GetTitle: String; override;
    procedure Setup({%H-}ADialog: TAbstractOptionsEditorDialog); override;
    procedure ReadSettings(AOptions: TAbstractIDEOptions); override;
    procedure WriteSettings(AOptions: TAbstractIDEOptions); override;
    class function SupportedOptionsClass: TAbstractIDEOptionsClass; override;
  end;

implementation

{$R *.lfm}

{ TEditorMultiWindowOptionsFrame }

procedure TEditorMultiWindowOptionsFrame.listAccessTypeClickCheck(Sender: TObject);
var
  i: Integer;
begin
  i := listAccessType.ItemIndex;
  lblAccessTypeDesc.Caption := FMultiWinEditAccessOrder[i].Desc;
  for i := 0 to FMultiWinEditAccessOrder.Count - 1 do begin
    if FMultiWinEditAccessOrder[i].IsFallback then
      listAccessType.Checked[i] := True;
    FMultiWinEditAccessOrder[i].Enabled := listAccessType.Checked[i];
  end;
end;

procedure TEditorMultiWindowOptionsFrame.listAccessTypeKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  listAccessTypeClickCheck(Sender);
end;

procedure TEditorMultiWindowOptionsFrame.radioAccessOrderEditChange(Sender: TObject);
begin
  if radioAccessOrderEdit.Checked then
    FMultiWinEditAccessOrder.SearchOrder := eoeaOrderByEditFocus;
  if radioAccessOrderWin.Checked then
    FMultiWinEditAccessOrder.SearchOrder := eoeaOrderByWindowFocus;
end;

constructor TEditorMultiWindowOptionsFrame.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FMultiWinEditAccessOrder := TEditorOptionsEditAccessOrderList.Create;
end;

destructor TEditorMultiWindowOptionsFrame.Destroy;
begin
  FreeAndNil(FMultiWinEditAccessOrder);
  inherited Destroy;
end;

function TEditorMultiWindowOptionsFrame.GetTitle: String;
begin
  Result := dlgMultiWinOptions;
end;

procedure TEditorMultiWindowOptionsFrame.Setup(ADialog: TAbstractOptionsEditorDialog);
var
  TmpNB: TSourceNotebook;
begin
  TmpNB := TSourceNotebook.Create(nil, -1);
  chkShowCloseBtn.Enabled := nbcShowCloseButtons in TmpNB.GetCapabilities;
  chkMultiLine.Enabled := nbcMultiLine in TmpNB.GetCapabilities;
  TmpNB.Free;

  lblMultiWinTabSection.Caption := dlgMultiWinTabGroup;
  lblEditActivationOrderSection.Caption := dlgMultiWinAccessGroup;
  lblAccessOrder.Caption := dlgMultiWinAccessOrder;
  radioAccessOrderEdit.Caption := dlgMultiWinAccessOrderEdit;
  radioAccessOrderWin.Caption := dlgMultiWinAccessOrderWin;
  lblAccessType.Caption := dlgMultiWinAccessType;
  chkHideSingleTab.Caption := dlgHideSingleTabInNotebook;
  chkShowNumbers.Caption := dlgTabNumbersNotebook;
  chkShowCloseBtn.Caption := dlgCloseButtonsNotebook;
  chkUseTabHistory.Caption := dlgUseTabsHistory;
  chkCtrlMiddleCloseOthers.Caption := dlgCtrlMiddleTabCloseOtherPages;
  chkShowFileNameInCaption.Caption := dlgShowFileNameInCaption;
  chkMultiLine.Caption := dlgSourceEditTabMultiLine;
  EditorTabPositionCheckBox.Items.Add(lisNotebookTabPosTop);
  EditorTabPositionCheckBox.Items.Add(lisNotebookTabPosBottom);
  EditorTabPositionCheckBox.Items.Add(lisNotebookTabPosLeft);
  EditorTabPositionCheckBox.Items.Add(lisNotebookTabPosRight);
  EditorTabPositionLabel.Caption := dlgNotebookTabPos;

end;

procedure TEditorMultiWindowOptionsFrame.ReadSettings(AOptions: TAbstractIDEOptions);
const
  TabPosToIndex : Array [TTabPosition] of Integer = (0, 1, 2, 3);
var
  i: Integer;
begin
  with TEditorOptions(AOptions) do begin
    chkHideSingleTab.Checked := HideSingleTabInWindow;
    chkShowNumbers.Checked := ShowTabNumbers;
    chkShowCloseBtn.Checked := ShowTabCloseButtons and chkShowCloseBtn.Enabled;
    chkUseTabHistory.Checked := UseTabHistory;
    chkCtrlMiddleCloseOthers.Checked := CtrlMiddleTabClickClosesOthers;
    chkShowFileNameInCaption.Checked := ShowFileNameInCaption;
    chkMultiLine.Checked := MultiLineTab;
    EditorTabPositionCheckBox.ItemIndex := TabPosToIndex[TabPosition];
  end;
  FMultiWinEditAccessOrder.Assign(TEditorOptions(AOptions).MultiWinEditAccessOrder);

  radioAccessOrderEdit.Checked := FMultiWinEditAccessOrder.SearchOrder = eoeaOrderByEditFocus;
  radioAccessOrderWin.Checked := FMultiWinEditAccessOrder.SearchOrder = eoeaOrderByWindowFocus;

  listAccessType.Clear;
  for i := 0 to FMultiWinEditAccessOrder.Count - 1 do begin
    listAccessType.Items.Add(FMultiWinEditAccessOrder[i].Caption);
    listAccessType.Checked[i] := FMultiWinEditAccessOrder[i].Enabled;
  end;
  listAccessType.ItemIndex := 0;
  listAccessTypeClickCheck(nil);
end;

procedure TEditorMultiWindowOptionsFrame.WriteSettings(AOptions: TAbstractIDEOptions);
const
  TabIndexToPos : Array [0..3] of TTabPosition = (tpTop, tpBottom, tpLeft, tpRight);
begin
  TEditorOptions(AOptions).MultiWinEditAccessOrder.Assign(FMultiWinEditAccessOrder);
  with TEditorOptions(AOptions) do begin
    HideSingleTabInWindow := chkHideSingleTab.Checked;
    ShowTabNumbers := chkShowNumbers.Checked;
    ShowTabCloseButtons := chkShowCloseBtn.Checked;
    UseTabHistory := chkUseTabHistory.Checked;
    CtrlMiddleTabClickClosesOthers := chkCtrlMiddleCloseOthers.Checked;
    ShowFileNameInCaption := chkShowFileNameInCaption.Checked;
    MultiLineTab := chkMultiLine.Checked;
    TabPosition := TabIndexToPos[EditorTabPositionCheckBox.ItemIndex];
  end;
end;

class function TEditorMultiWindowOptionsFrame.SupportedOptionsClass: TAbstractIDEOptionsClass;
begin
  Result := TEditorOptions;
end;

initialization
  RegisterIDEOptionsEditor(GroupEditor, TEditorMultiWindowOptionsFrame, EdtOptionsMultiWindow);
end.

