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
}
unit options_formed;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, StdCtrls, Dialogs,
  EnvironmentOpts, LazarusIDEStrConsts, IDEProcs;

type

  { TFormEditorOptionsFrame }

  TFormEditorOptionsFrame = class(TAbstractOptionsFrame)
    AutoCreateFormsOnOpenCheckBox: TCheckBox;
    DesignerPaintLazyCheckBox: TCheckBox;
    FormEditMiscGroupBox: TGroupBox;
    GrabberColorButton: TColorButton;
    GrabberColorLabel: TLabel;
    GridColorButton: TColorButton;
    GridColorLabel: TLabel;
    GridGroupBox: TGroupBox;
    GridSizeXComboBox: TComboBox;
    GridSizeXLabel: TLabel;
    GridSizeYComboBox: TComboBox;
    GridSizeYLabel: TLabel;
    GuideLineColorLeftTopButton: TColorButton;
    GuideLineColorLeftTopLabel: TLabel;
    GuideLineColorRightBottomButton: TColorButton;
    GuideLineColorRightBottomLabel: TLabel;
    GuideLinesGroupBox: TGroupBox;
    MarkerColorButton: TColorButton;
    MarkerColorLabel: TLabel;
    RightClickSelectsCheckBox: TCheckBox;
    RubberbandCreateColorButton: TColorButton;
    RubberbandCreateColorLabel: TLabel;
    RubberbandGroupBox: TGroupBox;
    RubberbandSelectColorButton: TColorButton;
    RubberbandSelectColorLabel: TLabel;
    RubberbandSelectsGrandChildsCheckBox: TCheckBox;
    ShowBorderSpaceCheckBox: TCheckBox;
    ShowComponentCaptionsCheckBox: TCheckBox;
    ShowEditorHintsCheckBox: TCheckBox;
    ShowGridCheckBox: TCheckBox;
    ShowGuideLinesCheckBox: TCheckBox;
    SnapToGridCheckBox: TCheckBox;
    SnapToGuideLinesCheckBox: TCheckBox;
    procedure FrameResize(Sender: TObject);
  private
  public
    function Check: Boolean; override;
    function GetTitle: String; override;
    procedure Setup; override;
    procedure ReadSettings(AOptions: TEnvironmentOptions); override;
    procedure WriteSettings(AOptions: TEnvironmentOptions); override;
  end;

implementation

{ TFormEditorOptionsFrame }

function TFormEditorOptionsFrame.Check: Boolean;
begin
  Result := True;
end;

function TFormEditorOptionsFrame.GetTitle: String;
begin
  Result := dlgFrmEditor;
end;

procedure TFormEditorOptionsFrame.Setup;
  procedure SetupGridGroupBox;
  begin
    ShowBorderSpaceCheckBox.Caption:=dlgQShowBorderSpacing;
    ShowGridCheckBox.Caption:=dlgQShowGrid;
    GridColorLabel.Caption:=dlgGridColor;
    SnapToGridCheckBox.Caption:=dlgQSnapToGrid;
    GridSizeXComboBox.Hint:=dlgGridXHint;
    GridSizeXLabel.Caption:=dlgGridX;
    GridSizeYComboBox.Hint:=dlgGridYHint;
    GridSizeYLabel.Caption:=dlgGridY;
  end;

  procedure SetupGuideLinesGroupBox;
  begin
    ShowGuideLinesCheckBox.Caption:=dlgGuideLines;
    SnapToGuideLinesCheckBox.Caption:=dlgSnapGuideLines;
    GuideLineColorLeftTopLabel.Caption:=dlgLeftTopClr;
    GuideLineColorRightBottomLabel.Caption:=dlgRightBottomClr;
  end;

  procedure SetupMiscGroupBox;
  begin
    ShowComponentCaptionsCheckBox.Caption:=dlgShowCaps;
    ShowEditorHintsCheckBox.Caption:=dlgShowEdrHints;
    AutoCreateFormsOnOpenCheckBox.Caption:=dlgAutoForm;
    RightClickSelectsCheckBox.Caption:=dlgRightClickSelects;
    GrabberColorLabel.Caption:=dlgGrabberColor;
    MarkerColorLabel.Caption:=dlgMarkerColor;

    with DesignerPaintLazyCheckBox do begin
      Caption:=lisFEPaintDesignerItemsOnIdle;
      Hint:=lisFEPaintDesignerItemsOnIdleReduceOverheadForSlowCompu;
    end;
  end;

  procedure SetupRubberbandBox;
  begin
    RubberbandSelectColorLabel.Caption:=dlgRuberbandSelectionColor;
    RubberbandCreateColorLabel.Caption:=dlgRuberbandCreationColor;
    RubberbandSelectsGrandChildsCheckBox.Caption:=dlgRubberbandSelectsGrandChilds;
  end;
begin
  GridGroupBox.Caption := dlgEnvGrid;
  SetupGridGroupBox;
  GuideLinesGroupBox.Caption := dlgEnvLGuideLines;
  SetupGuideLinesGroupBox;
  RubberbandGroupBox.Caption := dlgRubberBandGroup;
  SetupRubberbandBox;
  FormEditMiscGroupBox.Caption := dlgEnvMisc;
  SetupMiscGroupBox;
end;

procedure TFormEditorOptionsFrame.ReadSettings(AOptions: TEnvironmentOptions);
begin
  with AOptions do
  begin
    ShowBorderSpaceCheckBox.Checked:=ShowBorderSpacing;
    ShowGridCheckBox.Checked:=ShowGrid;
    GridColorButton.ButtonColor:=GridColor;
    SnapToGridCheckBox.Checked:=SnapToGrid;
    SetComboBoxText(GridSizeXComboBox,IntToStr(GridSizeX));
    SetComboBoxText(GridSizeYComboBox,IntToStr(GridSizeY));
    ShowGuideLinesCheckBox.Checked:=ShowGuideLines;
    SnapToGuideLinesCheckBox.Checked:=SnapToGuideLines;
    GuideLineColorLeftTopButton.ButtonColor:=GuideLineColorLeftTop;
    GuideLineColorRightBottomButton.ButtonColor:=GuideLineColorRightBottom;
    ShowComponentCaptionsCheckBox.Checked:=ShowComponentCaptions;
    ShowEditorHintsCheckBox.Checked:=ShowEditorHints;
    AutoCreateFormsOnOpenCheckBox.Checked:=AutoCreateFormsOnOpen;
    RightClickSelectsCheckBox.Checked:=RightClickSelects;
    GrabberColorButton.ButtonColor:=GrabberColor;
    MarkerColorButton.ButtonColor:=MarkerColor;
    RubberbandSelectColorButton.ButtonColor:=RubberbandSelectionColor;
    RubberbandCreateColorButton.ButtonColor:=RubberbandCreationColor;
    RubberbandSelectsGrandChildsCheckBox.Checked:=RubberbandSelectsGrandChilds;
    DesignerPaintLazyCheckBox.Checked:=DesignerPaintLazy;
  end;
end;

procedure TFormEditorOptionsFrame.WriteSettings(AOptions: TEnvironmentOptions);
begin
  with AOptions do
  begin
    ShowBorderSpacing:=ShowBorderSpaceCheckBox.Checked;
    ShowGrid:=ShowGridCheckBox.Checked;
    GridColor:=GridColorButton.ButtonColor;
    SnapToGrid:=SnapToGridCheckBox.Checked;
    GridSizeX:=StrToIntDef(GridSizeXComboBox.Text,GridSizeX);
    GridSizeY:=StrToIntDef(GridSizeYComboBox.Text,GridSizeY);
    ShowGuideLines:=ShowGuideLinesCheckBox.Checked;
    SnapToGuideLines:=SnapToGuideLinesCheckBox.Checked;
    GuideLineColorLeftTop:=GuideLineColorLeftTopButton.ButtonColor;
    GuideLineColorRightBottom:=GuideLineColorRightBottomButton.ButtonColor;
    ShowComponentCaptions:=ShowComponentCaptionsCheckBox.Checked;
    ShowEditorHints:=ShowEditorHintsCheckBox.Checked;
    AutoCreateFormsOnOpen:=AutoCreateFormsOnOpenCheckBox.Checked;
    RightClickSelects:=RightClickSelectsCheckBox.Checked;
    GrabberColor:=GrabberColorButton.ButtonColor;
    MarkerColor:=MarkerColorButton.ButtonColor;
    RubberbandSelectionColor:=RubberbandSelectColorButton.ButtonColor;
    RubberbandCreationColor:=RubberbandCreateColorButton.ButtonColor;
    RubberbandSelectsGrandChilds:=RubberbandSelectsGrandChildsCheckBox.Checked;
    DesignerPaintLazy:=DesignerPaintLazyCheckBox.Checked;
  end;
end;

procedure TFormEditorOptionsFrame.FrameResize(Sender: TObject);
var
  w: Integer;
begin
  w := ((ClientWidth - 3 * 5) * 5) div 10;
  GridGroupBox.Width := w;
  FormEditMiscGroupBox.Width := GridGroupBox.Width;
end;

initialization
  {$I options_formed.lrs}
  RegisterEnvironmentOptionsEditor(TFormEditorOptionsFrame);

end.

