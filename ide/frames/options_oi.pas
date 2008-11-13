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
    procedure ColorBoxChange(Sender: TObject);
    procedure ColorsListBoxGetColors(Sender: TCustomColorListBox;
      Items: TStrings);
    procedure ColorsListBoxSelectionChange(Sender: TObject; User: boolean);
  private
    FStoredColors: array of TColorRec;
    FLoaded: Boolean;
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

  SetLength(FStoredColors, 6);
  FStoredColors[0].ColorName := dlgBackColor;
  FStoredColors[1].ColorName := dlgSubPropColor;
  FStoredColors[2].ColorName := dlgReferenceColor;
  FStoredColors[3].ColorName := dlgValueColor;
  FStoredColors[4].ColorName := dlgDefValueColor;
  FStoredColors[5].ColorName := dlgPropNameColor;
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

procedure TOIOptionsFrame.ColorBoxChange(Sender: TObject);
begin
  if not FLoaded or (ColorsListBox.ItemIndex < 0) then
    Exit;
  ColorsListBox.Items.Objects[ColorsListBox.ItemIndex] := TObject(PtrInt(ColorBox.Selected));
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
    FStoredColors[3].ColorValue := ObjectInspectorOptions.ValueColor;
    FStoredColors[4].ColorValue := ObjectInspectorOptions.DefaultValueColor;
    FStoredColors[5].ColorValue := ObjectInspectorOptions.PropertyNameColor;

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
    ObjectInspectorOptions.ValueColor := ColorsListBox.Colors[3];
    ObjectInspectorOptions.DefaultValueColor := ColorsListBox.Colors[4];
    ObjectInspectorOptions.PropertyNameColor := ColorsListBox.Colors[5];

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

