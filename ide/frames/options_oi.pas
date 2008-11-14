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
unit Options_OI;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, StdCtrls, Dialogs, Spin, LCLProc,
  ObjectInspector, LazarusIDEStrConsts, EnvironmentOpts, IDEOptionsIntf,
  ColorBox, Graphics;

type
  TColorRec = record
    ColorName: String;
    ColorValue: TColor;
  end;

  { TOIOptionsFrame }

  TOIOptionsFrame = class(TAbstractIDEOptionsEditor)
    BtnUseDefaultLazarusColors: TButton;
    BtnUseDefaultDelphiColors: TButton;
    ColorBox: TColorBox;
    ColorsListBox: TColorListBox;
    ObjectInspectorColorsGroupBox: TGroupBox;
    OIAutoShowCheckBox: TCheckBox;
    OIBoldNonDefaultCheckBox: TCheckBox;
    OIDefaultItemHeightLabel: TLabel;
    OIDefaultItemHeightSpinEdit: TSpinEdit;
    OIDrawGridLinesCheckBox: TCheckBox;
    OIMiscGroupBox: TGroupBox;
    OIShowHintCheckBox: TCheckBox;
    procedure BtnUseDefaultDelphiColorsClick(Sender: TObject);
    procedure BtnUseDefaultLazarusColorsClick(Sender: TObject);
    procedure ColorBoxChange(Sender: TObject);
    procedure ColorsListBoxGetColors(Sender: TCustomColorListBox;
      Items: TStrings);
    procedure ColorsListBoxSelectionChange(Sender: TObject; User: boolean);
  private
    FStoredColors: array of TColorRec;
    FLoaded: Boolean;
    procedure ChangeColor(AIndex: Integer; NewColor: TColor);
  public
    function GetTitle: String; override;
    procedure Setup(ADialog: TAbstractOptionsEditorDialog); override;
    procedure ReadSettings(AOptions: TAbstractIDEOptions); override;
    procedure WriteSettings(AOptions: TAbstractIDEOptions); override;
    class function SupportedOptionsClass: TAbstractIDEOptionsClass; override;
  end; 

implementation

type
  THackColorListBox = class(TColorListBox);


{ TOIOptionsFrame }

procedure TOIOptionsFrame.Setup(ADialog: TAbstractOptionsEditorDialog);
begin
  ObjectInspectorColorsGroupBox.Caption := dlgEnvColors;
  OIMiscGroupBox.Caption := dlgOIMiscellaneous;
  OIDefaultItemHeightLabel.Caption := dlgOIItemHeight;
  OIShowHintCheckBox.Caption := lisShowHintsInObjectInspector;
  OIAutoShowCheckBox.Caption := lisAutoShowObjectInspector;
  OIBoldNonDefaultCheckBox.Caption := lisBoldNonDefaultObjectInspector;
  OIDrawGridLinesCheckBox.Caption := lisDrawGridLinesObjectInspector;

  SetLength(FStoredColors, 10);
  FStoredColors[0].ColorName := dlgBackColor;
  FStoredColors[1].ColorName := dlgSubPropColor;
  FStoredColors[2].ColorName := dlgReferenceColor;
  FStoredColors[3].ColorName := dlgHighlightColor;
  FStoredColors[4].ColorName := dlgHighlightFontColor;
  FStoredColors[5].ColorName := dlgValueColor;
  FStoredColors[6].ColorName := dlgDefValueColor;
  FStoredColors[7].ColorName := dlgPropNameColor;
  FStoredColors[8].ColorName := dlgPropGutterColor;
  FStoredColors[9].ColorName := dlgPropGutterEdgeColor;

  BtnUseDefaultLazarusColors.Caption := dlgOIUseDefaultLazarusColors;
  BtnUseDefaultDelphiColors.Caption := dlgOIUseDefaultDelphiColors;

  FLoaded := False;
end;

procedure TOIOptionsFrame.ColorsListBoxGetColors(Sender: TCustomColorListBox;
  Items: TStrings);
var
  i: integer;
begin
  if not FLoaded then
    Exit;
  for i := Low(FStoredColors) to High(FStoredColors) do
    Items.AddObject(FStoredColors[i].ColorName, TObject(PtrInt(FStoredColors[i].ColorValue)));
end;

procedure TOIOptionsFrame.ChangeColor(AIndex: Integer; NewColor: TColor);
begin
  ColorsListBox.Items.Objects[AIndex] := TObject(PtrInt(NewColor));
end;

procedure TOIOptionsFrame.ColorBoxChange(Sender: TObject);
begin
  if not FLoaded or (ColorsListBox.ItemIndex < 0) then
    Exit;
  ChangeColor(ColorsListBox.ItemIndex, ColorBox.Selected);
  ColorsListBox.Invalidate;
end;

procedure TOIOptionsFrame.BtnUseDefaultLazarusColorsClick(Sender: TObject);
begin
  ChangeColor(0, DefBackgroundColor);
  ChangeColor(1, DefSubPropertiesColor);
  ChangeColor(2, DefReferencesColor);
  ChangeColor(3, DefHighlightColor);
  ChangeColor(4, DefHighlightFontColor);
  ChangeColor(5, DefValueColor);
  ChangeColor(6, DefDefaultValueColor);
  ChangeColor(7, DefNameColor);
  ChangeColor(8, DefGutterColor);
  ChangeColor(9, DefGutterEdgeColor);
  ColorsListBox.Invalidate;
end;

procedure TOIOptionsFrame.BtnUseDefaultDelphiColorsClick(Sender: TObject);
begin
  ChangeColor(0, clWindow);
  ChangeColor(1, clGreen);
  ChangeColor(2, clMaroon);
  ChangeColor(3, $E0E0E0);
  ChangeColor(4, clBlack);
  ChangeColor(5, clNavy);
  ChangeColor(6, clNavy);
  ChangeColor(7, clBtnText);
  ChangeColor(8, clCream);
  ChangeColor(9, clGray);
  ColorsListBox.Invalidate;
end;

procedure TOIOptionsFrame.ColorsListBoxSelectionChange(Sender: TObject;
  User: boolean);
begin
  if not (FLoaded and User) then
    Exit;
  ColorBox.Selected := ColorsListBox.Selected;
end;

function TOIOptionsFrame.GetTitle: String;
begin
  Result := dlgObjInsp;
end;

procedure TOIOptionsFrame.ReadSettings(AOptions: TAbstractIDEOptions);
begin
  with AOptions as TEnvironmentOptions do
  begin
    FStoredColors[0].ColorValue := ObjectInspectorOptions.GridBackgroundColor;
    FStoredColors[1].ColorValue := ObjectInspectorOptions.SubPropertiesColor;
    FStoredColors[2].ColorValue := ObjectInspectorOptions.ReferencesColor;
    FStoredColors[3].ColorValue := ObjectInspectorOptions.HighlightColor;
    FStoredColors[4].ColorValue := ObjectInspectorOptions.HighlightFontColor;
    FStoredColors[5].ColorValue := ObjectInspectorOptions.ValueColor;
    FStoredColors[6].ColorValue := ObjectInspectorOptions.DefaultValueColor;
    FStoredColors[7].ColorValue := ObjectInspectorOptions.PropertyNameColor;
    FStoredColors[8].ColorValue := ObjectInspectorOptions.GutterColor;
    FStoredColors[9].ColorValue := ObjectInspectorOptions.GutterEdgeColor;

    OIDefaultItemHeightSpinEdit.Value:=ObjectInspectorOptions.DefaultItemHeight;
    OIShowHintCheckBox.Checked := ObjectInspectorOptions.ShowHints;
    OIAutoShowCheckBox.Checked := ObjectInspectorOptions.AutoShow;
    OIBoldNonDefaultCheckBox.Checked := ObjectInspectorOptions.BoldNonDefaultValues;
    OIDrawGridLinesCheckBox.Checked := ObjectInspectorOptions.DrawGridLines;
  end;
  FLoaded := True;
  THackColorListBox(ColorsListBox).SetColorList;
end;

procedure TOIOptionsFrame.WriteSettings(AOptions: TAbstractIDEOptions);
begin
  with AOptions as TEnvironmentOptions do
  begin
    ObjectInspectorOptions.GridBackgroundColor := ColorsListBox.Colors[0];
    ObjectInspectorOptions.SubPropertiesColor := ColorsListBox.Colors[1];
    ObjectInspectorOptions.ReferencesColor := ColorsListBox.Colors[2];
    ObjectInspectorOptions.HighlightColor := ColorsListBox.Colors[3];
    ObjectInspectorOptions.HighlightFontColor := ColorsListBox.Colors[4];
    ObjectInspectorOptions.ValueColor := ColorsListBox.Colors[5];
    ObjectInspectorOptions.DefaultValueColor := ColorsListBox.Colors[6];
    ObjectInspectorOptions.PropertyNameColor := ColorsListBox.Colors[7];
    ObjectInspectorOptions.GutterColor := ColorsListBox.Colors[8];
    ObjectInspectorOptions.GutterEdgeColor := ColorsListBox.Colors[9];

    ObjectInspectorOptions.DefaultItemHeight:=
       RoundToInt(OIDefaultItemHeightSpinEdit.Value);
    ObjectInspectorOptions.ShowHints := OIShowHintCheckBox.Checked;
    ObjectInspectorOptions.AutoShow := OIAutoShowCheckBox.Checked;
    ObjectInspectorOptions.BoldNonDefaultValues := OIBoldNonDefaultCheckBox.Checked;
    ObjectInspectorOptions.DrawGridLines := OIDrawGridLinesCheckBox.Checked;
  end;
end;

class function TOIOptionsFrame.SupportedOptionsClass: TAbstractIDEOptionsClass;
begin
  Result := TEnvironmentOptions;
end;

initialization
  {$I options_oi.lrs}
  RegisterIDEOptionsEditor(GroupEnvironment, TOIOptionsFrame, EnvOptionsOI);
end.

