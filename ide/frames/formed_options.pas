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
unit formed_options;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Graphics, Forms, StdCtrls, Dialogs, Spin,
  ColorBox, EnvironmentOpts, LazarusIDEStrConsts, IDEProcs, IDEOptionsIntf;

type
  TDesignerColor = (
    dcGrid,
    dcGridLinesLeftTop,
    dcGridLinesRightBottom,
    dcGrabber,
    dcMarker,
    dcRuberbandSelection,
    dcRuberbandCreation
  );

  { TFormEditorOptionsFrame }

  TFormEditorOptionsFrame = class(TAbstractIDEOptionsEditor)
    SwitchToFavoritesOITabCheckBox:TCheckBox;
    CheckPackagesOnFormCreateCheckBox: TCheckBox;
    OpenDesignerOnOpenUnitCheckBox: TCheckBox;
    ColorBox: TColorBox;
    ColorsListBox: TColorListBox;
    CreateCompFocusNameCheckBox: TCheckBox;
    DesignerPaintLazyCheckBox: TCheckBox;
    FormEditMiscGroupBox: TGroupBox;
    GridGroupBox: TGroupBox;
    GridSizeXSpinEdit: TSpinEdit;
    GridSizeXLabel: TLabel;
    GridSizeYSpinEdit: TSpinEdit;
    GridSizeYLabel: TLabel;
    GuideLinesGroupBox: TGroupBox;
    DesignerColorsGroupBox: TGroupBox;
    RightClickSelectsCheckBox: TCheckBox;
    RubberbandSelectsGrandChildsCheckBox: TCheckBox;
    ShowBorderSpaceCheckBox: TCheckBox;
    ShowComponentCaptionsCheckBox: TCheckBox;
    ShowEditorHintsCheckBox: TCheckBox;
    ShowGridCheckBox: TCheckBox;
    ShowGuideLinesCheckBox: TCheckBox;
    SnapToGridCheckBox: TCheckBox;
    SnapToGuideLinesCheckBox: TCheckBox;
    procedure ColorBoxChange(Sender: TObject);
    procedure ColorsListBoxGetColors(Sender: TCustomColorListBox;
      Items: TStrings);
    procedure ColorsListBoxSelectionChange(Sender: TObject; User: boolean);
    procedure CreateCompFocusNameCheckBoxChange(Sender:TObject);
    procedure FrameResize(Sender: TObject);
  private
    FLoaded: Boolean;
    FSaved: Boolean;
    procedure ChangeColor(AIndex: Integer; NewColor: TColor);
  public
    function GetTitle: String; override;
    procedure Setup(ADialog: TAbstractOptionsEditorDialog); override;
    procedure ReadSettings(AOptions: TAbstractIDEOptions); override;
    procedure WriteSettings(AOptions: TAbstractIDEOptions); override;
    class function SupportedOptionsClass: TAbstractIDEOptionsClass; override;
  end;

implementation

{$R *.lfm}

{ TFormEditorOptionsFrame }

function TFormEditorOptionsFrame.GetTitle: String;
begin
  Result := dlgFrmEditor;
end;

procedure TFormEditorOptionsFrame.Setup(ADialog: TAbstractOptionsEditorDialog);
  procedure SetupGridGroupBox;
  begin
    ShowBorderSpaceCheckBox.Caption:=dlgQShowBorderSpacing;
    ShowGridCheckBox.Caption:=dlgQShowGrid;
    SnapToGridCheckBox.Caption:=dlgQSnapToGrid;
    GridSizeXSpinEdit.Hint:=dlgGridXHint;
    GridSizeXLabel.Caption:=dlgGridX;
    GridSizeYSpinEdit.Hint:=dlgGridYHint;
    GridSizeYLabel.Caption:=dlgGridY;
  end;

  procedure SetupGuideLinesGroupBox;
  begin
    ShowGuideLinesCheckBox.Caption:=dlgGuideLines;
    SnapToGuideLinesCheckBox.Caption:=dlgSnapGuideLines;
  end;

  procedure SetupMiscGroupBox;
  begin
    ShowComponentCaptionsCheckBox.Caption:=dlgShowCaps;
    ShowEditorHintsCheckBox.Caption:=dlgShowEdrHints;
    OpenDesignerOnOpenUnitCheckBox.Caption:=lisOpenDesignerOnOpenUnit;
    RightClickSelectsCheckBox.Caption:=dlgRightClickSelects;
    CheckPackagesOnFormCreateCheckBox.Caption:=dlgCheckPackagesOnFormCreate;

    with DesignerPaintLazyCheckBox do
    begin
      Caption:=lisFEPaintDesignerItemsOnIdle;
      Hint:=lisFEPaintDesignerItemsOnIdleReduceOverheadForSlowCompu;
    end;
    with CreateCompFocusNameCheckBox do
    begin
      Caption:=lisAskNameOnCreate;
      Hint:=lisAskForComponentNameAfterPuttingItOnForm;
    end;
    with SwitchToFavoritesOITabCheckBox do
    begin
      Caption:=lisOFESwitchToObjectInspectorFavoritesTab;
      Hint:=lisOFESwitchToObjectInspectorFavoritesTabAfterAsking;
    end;
    RubberbandSelectsGrandChildsCheckBox.Caption:=dlgRubberbandSelectsGrandChildren;
  end;
begin
  GridGroupBox.Caption := dlgEnvGrid;
  GuideLinesGroupBox.Caption := dlgEnvLGuideLines;
  FormEditMiscGroupBox.Caption := dlgEnvMisc;
  DesignerColorsGroupBox.Caption := dlgColors;
  SetupGridGroupBox;
  SetupGuideLinesGroupBox;
  SetupMiscGroupBox;
  FLoaded := False;
end;

procedure TFormEditorOptionsFrame.ReadSettings(AOptions: TAbstractIDEOptions);
begin
  if fLoaded then exit;
  with AOptions as TEnvironmentOptions do
  begin
    // read colors
    ColorsListBox.Items.Objects[Ord(dcGrid)] := TObject(PtrInt(GridColor));
    ColorsListBox.Items.Objects[Ord(dcGridLinesLeftTop)] := TObject(PtrInt(GuideLineColorLeftTop));
    ColorsListBox.Items.Objects[Ord(dcGridLinesRightBottom)] := TObject(PtrInt(GuideLineColorRightBottom));
    ColorsListBox.Items.Objects[Ord(dcGrabber)] := TObject(PtrInt(GrabberColor));
    ColorsListBox.Items.Objects[Ord(dcMarker)] := TObject(PtrInt(MarkerColor));
    ColorsListBox.Items.Objects[Ord(dcRuberbandSelection)] := TObject(PtrInt(RubberbandSelectionColor));
    ColorsListBox.Items.Objects[Ord(dcRuberbandCreation)] := TObject(PtrInt(RubberbandCreationColor));

    ShowBorderSpaceCheckBox.Checked := ShowBorderSpacing;
    ShowGridCheckBox.Checked := ShowGrid;
    SnapToGridCheckBox.Checked := SnapToGrid;
    GridSizeXSpinEdit.Value := GridSizeX;
    GridSizeYSpinEdit.Value := GridSizeY;
    ShowGuideLinesCheckBox.Checked := ShowGuideLines;
    SnapToGuideLinesCheckBox.Checked := SnapToGuideLines;
    ShowComponentCaptionsCheckBox.Checked := ShowComponentCaptions;
    ShowEditorHintsCheckBox.Checked := ShowEditorHints;
    OpenDesignerOnOpenUnitCheckBox.Checked := AutoCreateFormsOnOpen;
    CheckPackagesOnFormCreateCheckBox.Checked := CheckPackagesOnFormCreate;
    RightClickSelectsCheckBox.Checked := RightClickSelects;
    RubberbandSelectsGrandChildsCheckBox.Checked := RubberbandSelectsGrandChilds;
    DesignerPaintLazyCheckBox.Checked := DesignerPaintLazy;
    CreateCompFocusNameCheckBox.Checked := CreateComponentFocusNameProperty;
    SwitchToFavoritesOITabCheckBox.Checked := SwitchToFavoritesOITab;
    SwitchToFavoritesOITabCheckBox.Enabled := CreateCompFocusNameCheckBox.Checked;
  end;
  FLoaded := True;
end;

procedure TFormEditorOptionsFrame.WriteSettings(AOptions: TAbstractIDEOptions);
begin
  if FSaved then exit;
  FSaved:=true;
  with AOptions as TEnvironmentOptions do
  begin
    // write colors
    GridColor := ColorsListBox.Colors[Ord(dcGrid)];
    GuideLineColorLeftTop := ColorsListBox.Colors[Ord(dcGridLinesLeftTop)];
    GuideLineColorRightBottom := ColorsListBox.Colors[Ord(dcGridLinesRightBottom)];
    GrabberColor := ColorsListBox.Colors[Ord(dcGrabber)];
    MarkerColor := ColorsListBox.Colors[Ord(dcMarker)];
    RubberbandSelectionColor := ColorsListBox.Colors[Ord(dcRuberbandSelection)];
    RubberbandCreationColor := ColorsListBox.Colors[Ord(dcRuberbandCreation)];

    ShowBorderSpacing := ShowBorderSpaceCheckBox.Checked;
    ShowGrid := ShowGridCheckBox.Checked;
    SnapToGrid := SnapToGridCheckBox.Checked;
    GridSizeX := GridSizeXSpinEdit.Value;
    GridSizeY := GridSizeYSpinEdit.Value;
    ShowGuideLines := ShowGuideLinesCheckBox.Checked;
    SnapToGuideLines := SnapToGuideLinesCheckBox.Checked;
    ShowComponentCaptions := ShowComponentCaptionsCheckBox.Checked;
    ShowEditorHints := ShowEditorHintsCheckBox.Checked;
    AutoCreateFormsOnOpen := OpenDesignerOnOpenUnitCheckBox.Checked;
    CheckPackagesOnFormCreate := CheckPackagesOnFormCreateCheckBox.Checked;
    RightClickSelects := RightClickSelectsCheckBox.Checked;
    RubberbandSelectsGrandChilds := RubberbandSelectsGrandChildsCheckBox.Checked;
    DesignerPaintLazy := DesignerPaintLazyCheckBox.Checked;
    CreateComponentFocusNameProperty := CreateCompFocusNameCheckBox.Checked;
    SwitchToFavoritesOITab := SwitchToFavoritesOITabCheckBox.Checked;
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

procedure TFormEditorOptionsFrame.ChangeColor(AIndex: Integer; NewColor: TColor);
begin
  ColorsListBox.Items.Objects[AIndex] := TObject(PtrInt(NewColor));
end;

procedure TFormEditorOptionsFrame.ColorsListBoxGetColors(
  Sender: TCustomColorListBox; Items: TStrings);
begin
  Items.Add(dlgGridColor);
  Items.Add(dlgLeftTopClr);
  Items.Add(dlgRightBottomClr);
  Items.Add(dlgGrabberColor);
  Items.Add(dlgMarkerColor);
  Items.Add(dlgRuberbandSelectionColor);
  Items.Add(dlgRuberbandCreationColor);
end;

procedure TFormEditorOptionsFrame.ColorBoxChange(Sender: TObject);
begin
  if not FLoaded or (ColorsListBox.ItemIndex < 0) then
    Exit;
  ChangeColor(ColorsListBox.ItemIndex, ColorBox.Selected);
  ColorsListBox.Invalidate;
end;

procedure TFormEditorOptionsFrame.ColorsListBoxSelectionChange(Sender: TObject;
  User: boolean);
begin
  if not (FLoaded and User) then
    Exit;
  ColorBox.Selected := ColorsListBox.Selected;
end;

procedure TFormEditorOptionsFrame.CreateCompFocusNameCheckBoxChange(Sender:
  TObject);
begin
  SwitchToFavoritesOITabCheckBox.Enabled := CreateCompFocusNameCheckBox.Checked;
end;

class function TFormEditorOptionsFrame.SupportedOptionsClass: TAbstractIDEOptionsClass;
begin
  Result := TEnvironmentOptions;
end;

initialization
  RegisterIDEOptionsEditor(GroupEnvironment, TFormEditorOptionsFrame, EnvOptionsFormEd);
end.

