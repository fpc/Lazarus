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
  ObjectInspector, LazarusIDEStrConsts, EnvironmentOpts;

type

  { TOIOptionsFrame }

  TOIOptionsFrame = class(TAbstractOptionsFrame)
    ObjectInspectorColorsGroupBox: TGroupBox;
    OIAutoShowCheckBox: TCheckBox;
    OIBackgroundColorButton: TColorButton;
    OIBackgroundColorLabel: TLabel;
    OIBoldNonDefaultCheckBox: TCheckBox;
    OIDefaultItemHeightLabel: TLabel;
    OIDefaultItemHeightSpinEdit: TSpinEdit;
    OIDefaultValueColorButton: TColorButton;
    OIDefaultValueColorLabel: TLabel;
    OIDrawGridLinesCheckBox: TCheckBox;
    OIMiscGroupBox: TGroupBox;
    OIPropNameColorButton: TColorButton;
    OIPropNameColorLabel: TLabel;
    OIReferencesColorButton: TColorButton;
    OIReferencesColorLabel: TLabel;
    OIShowHintCheckBox: TCheckBox;
    OISubPropsColorButton: TColorButton;
    OISubPropsColorLabel: TLabel;
    OIValueColorButton: TColorButton;
    OIValueColorLabel: TLabel;
  private
  public
    function Check: Boolean; override;
    function GetTitle: String; override;
    procedure Setup; override;
    procedure ReadSettings(AOptions: TEnvironmentOptions); override;
    procedure WriteSettings(AOptions: TEnvironmentOptions); override;
  end; 

implementation

{ TOIOptionsFrame }

procedure TOIOptionsFrame.Setup;
begin
  ObjectInspectorColorsGroupBox.Caption := dlgEnvColors;
  OIBackgroundColorLabel.Caption := dlgBackColor;
  OISubPropsColorLabel.Caption := dlgSubPropColor;
  OIReferencesColorLabel.Caption := dlgReferenceColor;
  OIValueColorLabel.Caption := dlgValueColor;
  OIDefaultValueColorLabel.Caption := dlgDefValueColor;
  OIPropNameColorLabel.Caption := dlgPropNameColor;
  OIMiscGroupBox.Caption := dlgOIMiscellaneous;
  OIDefaultItemHeightLabel.Caption := dlgOIItemHeight;
  OIShowHintCheckBox.Caption := lisShowHintsInObjectInspector;
  OIAutoShowCheckBox.Caption := lisAutoShowObjectInspector;
  OIBoldNonDefaultCheckBox.Caption := lisBoldNonDefaultObjectInspector;
  OIDrawGridLinesCheckBox.Caption := lisDrawGridLinesObjectInspector;
end;

function TOIOptionsFrame.Check: Boolean;
begin
  Result := True;
end;

function TOIOptionsFrame.GetTitle: String;
begin
  Result := dlgObjInsp;
end;

procedure TOIOptionsFrame.ReadSettings(AOptions: TEnvironmentOptions);
begin
  with AOptions do
  begin
    OIBackgroundColorButton.ButtonColor:=
       ObjectInspectorOptions.GridBackgroundColor;
    OISubPropsColorButton.ButtonColor:=
       ObjectInspectorOptions.SubPropertiesColor;
    OIReferencesColorButton.ButtonColor:=
       ObjectInspectorOptions.ReferencesColor;
    OIValueColorButton.ButtonColor:=
       ObjectInspectorOptions.ValueColor;
    OIDefaultValueColorButton.ButtonColor:=
       ObjectInspectorOptions.DefaultValueColor;
    OIPropNameColorButton.ButtonColor:=
       ObjectInspectorOptions.PropertyNameColor;

    OIDefaultItemHeightSpinEdit.Value:=ObjectInspectorOptions.DefaultItemHeight;
    OIShowHintCheckBox.Checked := ObjectInspectorOptions.ShowHints;
    OIAutoShowCheckBox.Checked := ObjectInspectorOptions.AutoShow;
    OIBoldNonDefaultCheckBox.Checked := ObjectInspectorOptions.BoldNonDefaultValues;
    OIDrawGridLinesCheckBox.Checked := ObjectInspectorOptions.DrawGridLines;
  end;
end;

procedure TOIOptionsFrame.WriteSettings(AOptions: TEnvironmentOptions);
begin
  with AOptions do
  begin
    ObjectInspectorOptions.GridBackgroundColor:=
       OIBackgroundColorButton.ButtonColor;
    ObjectInspectorOptions.SubPropertiesColor:=
       OISubPropsColorButton.ButtonColor;
    ObjectInspectorOptions.ReferencesColor:=
       OIReferencesColorButton.ButtonColor;
    ObjectInspectorOptions.ValueColor:=
       OIValueColorButton.ButtonColor;
    ObjectInspectorOptions.DefaultValueColor:=
       OIDefaultValueColorButton.ButtonColor;
    ObjectInspectorOptions.PropertyNameColor:=
       OIPropNameColorButton.ButtonColor;

    ObjectInspectorOptions.DefaultItemHeight:=
       RoundToInt(OIDefaultItemHeightSpinEdit.Value);
    ObjectInspectorOptions.ShowHints := OIShowHintCheckBox.Checked;
    ObjectInspectorOptions.AutoShow := OIAutoShowCheckBox.Checked;
    ObjectInspectorOptions.BoldNonDefaultValues := OIBoldNonDefaultCheckBox.Checked;
    ObjectInspectorOptions.DrawGridLines := OIDrawGridLinesCheckBox.Checked;
  end;
end;

initialization
  {$I options_oi.lrs}

end.

