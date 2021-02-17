{
 *****************************************************************************
  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************

  Author: Michael W. Vogel
}
unit DockedOptionsFrame;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  // LCL
  Forms, StdCtrls, Dialogs, ComCtrls, ColorBox, Controls, Graphics, Spin,
  ExtCtrls,
  // LazUtils
  LazFileCache, LazFileUtils, LazLoggerBase,
  // IdeIntf
  IDEOptionsIntf, IDEOptEditorIntf, LazIDEIntf, DividerBevel,
  // DockedFormEditor
  DockedStrConsts, DockedOptionsIDE, DockedMainIDE;

type

  { TFrameDockedOptions }

  TFrameDockedOptions = class(TAbstractIDEOptionsEditor)
    CheckBoxAnchorTabVisible: TCheckBox;
    CheckBoxForceRefreshing: TCheckBox;
    CheckBoxTreatAlign: TCheckBox;
    CheckBoxTreatBorder: TCheckBox;
    CheckBoxAllowSizing: TCheckBox;
    ColorBoxResizer: TColorBox;
    ComboBoxTabPosition: TComboBox;
    DividerBevelAnchors: TDividerBevel;
    LabelColors: TLabel;
    LabelCaptureDistance: TLabel;
    LabelMouseBorderFactor: TLabel;
    LabelResizerColor: TLabel;
    LabelTabPosition: TLabel;
    AnchorsColorBox: TColorBox;
    AnchorsColorListBox: TColorListBox;
    SpinEditCaptureDistance: TSpinEdit;
    SpinEditMouseBorderFactor: TSpinEdit;
    procedure CheckBoxAnchorTabVisibleChange(Sender: TObject);
    procedure AnchorsColorBoxChange(Sender: TObject);
    procedure AnchorsColorListBoxGetColors(Sender: TCustomColorListBox; Items: TStrings);
    procedure AnchorsColorListBoxSelectionChange(Sender: TObject; User: Boolean);
  private
    FLastAllowSizing: Boolean;
    FLastAnchorBorderColor: TColor;
    FLastAnchorControlColor: TColor;
    FLastAnchorTabsVisible: Boolean;
    FLastAnchorTargetColor: TColor;
    FLastAnchorTopColor: TColor;
    FLastAnchorLeftColor: TColor;
    FLastAnchorRightColor: TColor;
    FLastAnchorBottomColor: TColor;
    FLastCaptureDistance: Integer;
    FLastForceRefreshing: Boolean;
    FLastMouseBorderFactor: Integer;
    FLastResizerColor: TColor;
    FLastTabPosition: TTabPosition;
    FLastTreatAlign: Boolean;
    FLastTreatBorder: Boolean;
    FReady: Boolean;
  public
    function GetTitle: String; override;
    procedure Setup({%H-}ADialog: TAbstractOptionsEditorDialog); override;
    procedure ReadSettings({%H-}AOptions: TAbstractIDEOptions); override;
    procedure WriteSettings({%H-}AOptions: TAbstractIDEOptions); override;
    procedure RestoreSettings({%H-}AOptions: TAbstractIDEOptions); override;
    class function SupportedOptionsClass: TAbstractIDEOptionsClass; override;
  end;

implementation

{$R *.lfm}

{ TFrameDockedOptions }

procedure TFrameDockedOptions.CheckBoxAnchorTabVisibleChange(Sender: TObject);
begin
  CheckBoxAllowSizing.Enabled       := CheckBoxAnchorTabVisible.Checked;
  CheckBoxTreatAlign.Enabled        := CheckBoxAnchorTabVisible.Checked;
  CheckBoxTreatBorder.Enabled       := CheckBoxAnchorTabVisible.Checked;
  LabelColors.Enabled               := CheckBoxAnchorTabVisible.Checked;
  LabelCaptureDistance.Enabled      := CheckBoxAnchorTabVisible.Checked;
  LabelMouseBorderFactor.Enabled    := CheckBoxAnchorTabVisible.Checked;
  SpinEditCaptureDistance.Enabled   := CheckBoxAnchorTabVisible.Checked;
  SpinEditMouseBorderFactor.Enabled := CheckBoxAnchorTabVisible.Checked;
end;

procedure TFrameDockedOptions.AnchorsColorBoxChange(Sender: TObject);
var
  i: Integer;
begin
  i := AnchorsColorListBox.ItemIndex;
  if not FReady or (i < 0) then Exit;
  AnchorsColorListBox.Colors[i] := AnchorsColorBox.Selected;
end;

procedure TFrameDockedOptions.AnchorsColorListBoxGetColors(Sender: TCustomColorListBox; Items: TStrings);
var
  LStr: String;
begin
  for LStr in AnchorColorStr do
    Items.Add(LStr);
end;

procedure TFrameDockedOptions.AnchorsColorListBoxSelectionChange(Sender: TObject; User: Boolean);
begin
  if not (FReady and User) then
    Exit;
  AnchorsColorBox.Selected := AnchorsColorListBox.Selected;
end;

function TFrameDockedOptions.GetTitle: String;
begin
  Result := SCaptionDockedFormEditor;
end;

procedure TFrameDockedOptions.Setup(ADialog: TAbstractOptionsEditorDialog);
var
  ATabPosition: TTabPosition;
begin
  FReady := false;
  ComboBoxTabPosition.Items.Clear;
  for ATabPosition := Low(TTabPosition) to High(TTabPosition) do
    ComboBoxTabPosition.Items.Add(STabPosition[ATabPosition]);

  CheckBoxAllowSizing.Caption      := SAllowSizingCaption;
  CheckBoxAnchorTabVisible.Caption := SAnchorTabVisibleCaption;
  CheckBoxForceRefreshing.Caption  := SForceRefreshingCaption;
  CheckBoxTreatAlign.Caption       := STreatAlignCaption;
  CheckBoxTreatBorder.Caption      := STreatBorderCaption;
  DividerBevelAnchors.Caption      := SAnchors;
  LabelColors.Caption              := SColorsCaption;
  LabelResizerColor.Caption        := SResizerColorCaption;
  LabelTabPosition.Caption         := STabPositionCaption;
  LabelCaptureDistance.Caption     := SCaptureDistanceCaption;
  LabelMouseBorderFactor.Caption   := SMouseBorderFactorCaption;

  CheckBoxAllowSizing.Hint       := SAllowSizingHint;
  CheckBoxAnchorTabVisible.Hint  := SAnchorTabVisibleHint;
  CheckBoxTreatAlign.Hint        := STreatAlignHint;
  CheckBoxTreatBorder.Hint       := StreatBorderHint;
  CheckBoxForceRefreshing.Hint   := SForceRefreshingHint;
  ColorBoxResizer.Hint           := SResizerColorHint;
  ComboBoxTabPosition.Hint       := STabPositionHint;
  SpinEditCaptureDistance.Hint   := SCaptureDistanceHint;
  SpinEditMouseBorderFactor.Hint := SMouseBorderFactorHint;
end;

procedure TFrameDockedOptions.ReadSettings(AOptions: TAbstractIDEOptions);
begin
  FLastAllowSizing        := DockedOptions.AllowSizing;
  FLastAnchorBorderColor  := DockedOptions.AnchorBorderColor;
  FLastAnchorControlColor := DockedOptions.AnchorControlColor;
  FLastAnchorTabsVisible  := DockedOptions.AnchorTabVisible;
  FLastAnchorTargetColor  := DockedOptions.AnchorTargetColor;
  FLastAnchorTopColor     := DockedOptions.AnchorTopColor;
  FLastAnchorLeftColor    := DockedOptions.AnchorLeftColor;
  FLastAnchorRightColor   := DockedOptions.AnchorRightColor;
  FLastAnchorBottomColor  := DockedOptions.AnchorBottomColor;
  FLastCaptureDistance    := DockedOptions.CaptureDistance;
  FLastForceRefreshing    := DockedOptions.ForceRefreshing;
  FLastMouseBorderFactor  := DockedOptions.MouseBorderFactor;
  FLastResizerColor       := DockedOptions.ResizerColor;
  FLastTabPosition        := DockedOptions.TabPosition;
  FLastTreatAlign         := DockedOptions.TreatAlign;
  FLastTreatBorder        := DockedOptions.TreatBorder;
  RestoreSettings(AOptions);
  FReady := true;
end;

procedure TFrameDockedOptions.WriteSettings(AOptions: TAbstractIDEOptions);
begin
  DockedOptions.AllowSizing        := CheckBoxAllowSizing.Checked;
  DockedOptions.AnchorBorderColor  := AnchorsColorListBox.Colors[ord(acControlBorder)];
  DockedOptions.AnchorControlColor := AnchorsColorListBox.Colors[ord(acControl)];
  DockedOptions.AnchorTopColor     := AnchorsColorListBox.Colors[ord(acAnchorTop)];
  DockedOptions.AnchorLeftColor    := AnchorsColorListBox.Colors[ord(acAnchorLeft)];
  DockedOptions.AnchorRightColor   := AnchorsColorListBox.Colors[ord(acAnchorRight)];
  DockedOptions.AnchorBottomColor  := AnchorsColorListBox.Colors[ord(acAnchorBottom)];
  DockedOptions.AnchorTargetColor  := AnchorsColorListBox.Colors[ord(acAnchorTarget)];
  DockedOptions.AnchorTabVisible   := CheckBoxAnchorTabVisible.Checked;
  DockedOptions.CaptureDistance    := SpinEditCaptureDistance.Value;
  DockedOptions.ForceRefreshing    := CheckBoxForceRefreshing.Checked;
  DockedOptions.MouseBorderFactor  := SpinEditMouseBorderFactor.Value;
  DockedOptions.ResizerColor       := ColorBoxResizer.Selected;
  DockedOptions.TabPosition        := TTabPosition(ComboBoxTabPosition.ItemIndex);
  DockedOptions.TreatAlign         := CheckBoxTreatAlign.Checked;
  DockedOptions.TreatBorder        := CheckBoxTreatBorder.Checked;

  if DockedOptions.Modified then
  begin
    DockedOptions.SaveSafe;
    if Assigned(DockedTabMaster) then
      DockedTabMaster.OptionsModified;
  end;
end;

procedure TFrameDockedOptions.RestoreSettings(AOptions: TAbstractIDEOptions);
begin
  AnchorsColorListBox.Colors[ord(acControlBorder)] := DockedOptions.AnchorBorderColor;
  AnchorsColorListBox.Colors[ord(acControl)]       := DockedOptions.AnchorControlColor;
  AnchorsColorListBox.Colors[ord(acAnchorTop)]     := DockedOptions.AnchorTopColor;
  AnchorsColorListBox.Colors[ord(acAnchorLeft)]    := DockedOptions.AnchorLeftColor;
  AnchorsColorListBox.Colors[ord(acAnchorRight)]   := DockedOptions.AnchorRightColor;
  AnchorsColorListBox.Colors[ord(acAnchorBottom)]  := DockedOptions.AnchorBottomColor;
  AnchorsColorListBox.Colors[ord(acAnchorTarget)]  := DockedOptions.AnchorTargetColor;
  CheckBoxAnchorTabVisible.Checked := FLastAnchorTabsVisible;
  CheckBoxAnchorTabVisible.OnChange(nil);
  CheckBoxAllowSizing.Checked      := FLastAllowSizing;
  CheckBoxForceRefreshing.Checked  := FLastForceRefreshing;
  CheckBoxTreatAlign.Checked       := FLastTreatAlign;
  CheckBoxTreatBorder.Checked      := FLastTreatBorder;
  ColorBoxResizer.Selected         := FLastResizerColor;
  ComboBoxTabPosition.ItemIndex    := Integer(FLastTabPosition);
  SpinEditCaptureDistance.Value    := FLastCaptureDistance;
  SpinEditMouseBorderFactor.Value  := FLastMouseBorderFactor;
end;

class function TFrameDockedOptions.SupportedOptionsClass: TAbstractIDEOptionsClass;
begin
  Result := IDEEditorGroups.GetByIndex(GroupEnvironment)^.GroupClass;
end;

end.

