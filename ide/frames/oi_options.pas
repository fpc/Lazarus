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
unit OI_options;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, StdCtrls, Dialogs, Spin, LCLProc,
  ObjectInspector, LazarusIDEStrConsts, EnvironmentOpts, IDEOptionsIntf,
  ColorBox, Graphics;

type
  TOIColor = (
    ocBackground,
    ocGutter,
    ocGutterEdge,
    ocHighlight,
    ocHighlightFont,
    ocPropName,
    ocValue,
    ocDefValue,
    ocSubProp,
    ocReference,
    ocReadOnly
  );

  TOIOption = (
    ooShowHints,
    ooAutoShow,
    ooBoldNonDefault,
    ooDrawGridLines,
    ooShowGutter,
    ooShowStatusBar,
    ooShowInfoBox
  );

  TSpeedOISettings = record
    Name: String;
    Colors: array[TOIColor] of TColor;
    Options: array[TOIOption] of Boolean;
  end;

  { TOIOptionsFrame }

  TOIOptionsFrame = class(TAbstractIDEOptionsEditor)
    BtnUseDefaultDelphiSettings: TButton;
    BtnUseDefaultLazarusSettings: TButton;
    OIMiscGroupBox: TGroupBox;
    ObjectInspectorSpeedSettingsGroupBox: TGroupBox;
    OIDefaultItemHeightLabel: TLabel;
    OIDefaultItemHeightSpinEdit: TSpinEdit;
    OIShowGutterCheckBox: TCheckBox;
    ColorBox: TColorBox;
    ColorsListBox: TColorListBox;
    ObjectInspectorColorsGroupBox: TGroupBox;
    OIAutoShowCheckBox: TCheckBox;
    OIBoldNonDefaultCheckBox: TCheckBox;
    OIDrawGridLinesCheckBox: TCheckBox;
    OIOptionsGroupBox: TGroupBox;
    OIShowStatusBarCheckBox: TCheckBox;
    OIShowHintCheckBox: TCheckBox;
    OIShowInfoBoxCheckBox: TCheckBox;
    procedure BtnUseDefaultDelphiSettingsClick(Sender: TObject);
    procedure BtnUseDefaultLazarusSettingsClick(Sender: TObject);
    procedure ColorBoxChange(Sender: TObject);
    procedure ColorsListBoxGetColors(Sender: TCustomColorListBox;
      Items: TStrings);
    procedure ColorsListBoxSelectionChange(Sender: TObject; User: boolean);
  private
    FLoaded: Boolean;
    procedure ChangeColor(AIndex: Integer; NewColor: TColor);
    procedure ApplyOISettings(ASettings: TSpeedOISettings);
  public
    function GetTitle: String; override;
    procedure Setup(ADialog: TAbstractOptionsEditorDialog); override;
    procedure ReadSettings(AOptions: TAbstractIDEOptions); override;
    procedure WriteSettings(AOptions: TAbstractIDEOptions); override;
    class function SupportedOptionsClass: TAbstractIDEOptionsClass; override;
  end; 

implementation

{$R *.lfm}

const
  DefaultOISettings: TSpeedOISettings = (
    Name: 'Default';
    Colors: (
      { ocBackground    } DefBackgroundColor,
      { ocGutter        } DefGutterColor,
      { ocGutterEdge    } DefGutterEdgeColor,
      { ocHighlight     } DefHighlightColor,
      { ocHighlightFont } DefHighlightFontColor,
      { ocPropName      } DefNameColor,
      { ocValue         } DefValueColor,
      { ocDefValue      } DefDefaultValueColor,
      { ocSubProp       } DefSubPropertiesColor,
      { ocReference     } DefReferencesColor,
      { ocReadOnly      } DefReadOnlyColor
      );
    Options: (
      { ooShowHints      } False,
      { ooAutoShow       } True,
      { ooBoldNonDefault } True,
      { ooDrawGridLines  } True,
      { ooShowGutter     } True,
      { ooShowStatusBar  } True,
      { ooShowInfoBox    } True
    );
  );

  DelphiOISettings: TSpeedOISettings = (
    Name: 'Delphi';
    Colors: (
      { ocBackground    } clWindow,
      { ocGutter        } clCream,
      { ocGutterEdge    } clGray,
      { ocHighlight     } $E0E0E0,
      { ocHighlightFont } clBlack,
      { ocPropName      } clBtnText,
      { ocValue         } clNavy,
      { ocDefValue      } clNavy,
      { ocSubProp       } clGreen,
      { ocReference     } clMaroon,
      { ocReadOnly      } clGrayText
      );
    Options: (
      { ooShowHints      } False,
      { ooAutoShow       } True,
      { ooBoldNonDefault } True,
      { ooDrawGridLines  } False,
      { ooShowGutter     } True,
      { ooShowStatusBar  } True,
      { ooShowInfoBox    } False
    );
  );

{ TOIOptionsFrame }

procedure TOIOptionsFrame.Setup(ADialog: TAbstractOptionsEditorDialog);
begin
  ObjectInspectorColorsGroupBox.Caption := dlgColors;
  OIMiscGroupBox.Caption := dlgOIMiscellaneous;
  OIOptionsGroupBox.Caption := dlgOIOptions;
  ObjectInspectorSpeedSettingsGroupBox.Caption := dlgOISpeedSettings;
  OIDefaultItemHeightLabel.Caption := dlgOIItemHeight;
  OIShowHintCheckBox.Caption := lisShowHintsInObjectInspector;
  OIAutoShowCheckBox.Caption := lisAutoShowObjectInspector;
  OIBoldNonDefaultCheckBox.Caption := lisBoldNonDefaultObjectInspector;
  OIDrawGridLinesCheckBox.Caption := lisDrawGridLinesObjectInspector;
  OIShowGutterCheckBox.Caption := lisShowGutterInObjectInspector;
  OIShowStatusBarCheckBox.Caption := lisShowStatusBarInObjectInspector;
  OIShowInfoBoxCheckBox.Caption := lisShowInfoBoxInObjectInspector;

  BtnUseDefaultLazarusSettings.Caption := dlgOIUseDefaultLazarusSettings;
  BtnUseDefaultDelphiSettings.Caption := dlgOIUseDefaultDelphiSettings;

  FLoaded := False;
end;

procedure TOIOptionsFrame.ColorsListBoxGetColors(Sender: TCustomColorListBox;
  Items: TStrings);
begin
  Items.Add(dlgBackColor);
  Items.Add(dlgGutterColor);
  Items.Add(dlgGutterEdgeColor);
  Items.Add(dlgHighlightColor);
  Items.Add(dlgHighlightFontColor);
  Items.Add(dlgPropNameColor);
  Items.Add(dlgValueColor);
  Items.Add(dlgDefValueColor);
  Items.Add(dlgSubPropColor);
  Items.Add(dlgReferenceColor);
  Items.Add(dlfReadOnlyColor)
end;

procedure TOIOptionsFrame.ChangeColor(AIndex: Integer; NewColor: TColor);
begin
  ColorsListBox.Items.Objects[AIndex] := TObject(PtrInt(NewColor));
end;

procedure TOIOptionsFrame.ApplyOISettings(ASettings: TSpeedOISettings);
var
  OIColor: TOIColor;
begin
  for OIColor := Low(TOIColor) to High(TOIColor) do
    ColorsListBox.Items.Objects[Ord(OIColor)] := TObject(PtrInt(ASettings.Colors[OIColor]));
  ColorsListBox.Invalidate;

  OIShowHintCheckBox.Checked := ASettings.Options[ooShowHints];
  OIAutoShowCheckBox.Checked := ASettings.Options[ooAutoShow];
  OIBoldNonDefaultCheckBox.Checked := ASettings.Options[ooBoldNonDefault];
  OIDrawGridLinesCheckBox.Checked := ASettings.Options[ooDrawGridLines];
  OIShowGutterCheckBox.Checked := ASettings.Options[ooShowGutter];
  OIShowStatusBarCheckBox.Checked := ASettings.Options[ooShowStatusBar];
  OIShowInfoBoxCheckBox.Checked := ASettings.Options[ooShowInfoBox];
end;

procedure TOIOptionsFrame.ColorBoxChange(Sender: TObject);
begin
  if not FLoaded or (ColorsListBox.ItemIndex < 0) then
    Exit;
  ChangeColor(ColorsListBox.ItemIndex, ColorBox.Selected);
  ColorsListBox.Invalidate;
end;

procedure TOIOptionsFrame.BtnUseDefaultLazarusSettingsClick(Sender: TObject);
begin
  ApplyOISettings(DefaultOISettings);
end;

procedure TOIOptionsFrame.BtnUseDefaultDelphiSettingsClick(Sender: TObject);
begin
  ApplyOISettings(DelphiOISettings);
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
var
  ASettings: TSpeedOISettings;
begin
  with AOptions as TEnvironmentOptions do
  begin
    ASettings.Colors[ocBackground] := ObjectInspectorOptions.GridBackgroundColor;
    ASettings.Colors[ocGutter] := ObjectInspectorOptions.GutterColor;
    ASettings.Colors[ocGutterEdge] := ObjectInspectorOptions.GutterEdgeColor;
    ASettings.Colors[ocHighlight] := ObjectInspectorOptions.HighlightColor;
    ASettings.Colors[ocHighlightFont] := ObjectInspectorOptions.HighlightFontColor;
    ASettings.Colors[ocPropName] := ObjectInspectorOptions.PropertyNameColor;
    ASettings.Colors[ocValue] := ObjectInspectorOptions.ValueColor;
    ASettings.Colors[ocDefValue] := ObjectInspectorOptions.DefaultValueColor;
    ASettings.Colors[ocSubProp] := ObjectInspectorOptions.SubPropertiesColor;
    ASettings.Colors[ocReference] := ObjectInspectorOptions.ReferencesColor;
    ASettings.Colors[ocReadOnly] := ObjectInspectorOptions.ReadOnlyColor;

    ASettings.Options[ooShowHints] := ObjectInspectorOptions.ShowHints;
    ASettings.Options[ooAutoShow] := ObjectInspectorOptions.AutoShow;
    ASettings.Options[ooBoldNonDefault] := ObjectInspectorOptions.BoldNonDefaultValues;
    ASettings.Options[ooDrawGridLines] := ObjectInspectorOptions.DrawGridLines;
    ASettings.Options[ooShowGutter] := ObjectInspectorOptions.ShowGutter;
    ASettings.Options[ooShowStatusBar] := ObjectInspectorOptions.ShowStatusBar;
    ASettings.Options[ooShowInfoBox] := ObjectInspectorOptions.ShowInfoBox;
    ApplyOISettings(ASettings);
    OIDefaultItemHeightSpinEdit.Value := ObjectInspectorOptions.DefaultItemHeight;
  end;
  FLoaded := True;
end;

procedure TOIOptionsFrame.WriteSettings(AOptions: TAbstractIDEOptions);
begin
  with AOptions as TEnvironmentOptions do
  begin
    ObjectInspectorOptions.GridBackgroundColor := ColorsListBox.Colors[Ord(ocBackground)];
    ObjectInspectorOptions.GutterColor := ColorsListBox.Colors[Ord(ocGutter)];
    ObjectInspectorOptions.GutterEdgeColor := ColorsListBox.Colors[Ord(ocGutterEdge)];
    ObjectInspectorOptions.HighlightColor := ColorsListBox.Colors[Ord(ocHighlight)];
    ObjectInspectorOptions.HighlightFontColor := ColorsListBox.Colors[Ord(ocHighlightFont)];
    ObjectInspectorOptions.PropertyNameColor := ColorsListBox.Colors[Ord(ocPropName)];
    ObjectInspectorOptions.ValueColor := ColorsListBox.Colors[Ord(ocValue)];
    ObjectInspectorOptions.DefaultValueColor := ColorsListBox.Colors[Ord(ocDefValue)];
    ObjectInspectorOptions.SubPropertiesColor := ColorsListBox.Colors[Ord(ocSubProp)];
    ObjectInspectorOptions.ReferencesColor := ColorsListBox.Colors[Ord(ocReference)];
    ObjectInspectorOptions.ReadOnlyColor := ColorsListBox.Colors[Ord(ocReadOnly)];

    ObjectInspectorOptions.ShowHints := OIShowHintCheckBox.Checked;
    ObjectInspectorOptions.AutoShow := OIAutoShowCheckBox.Checked;
    ObjectInspectorOptions.BoldNonDefaultValues := OIBoldNonDefaultCheckBox.Checked;
    ObjectInspectorOptions.DrawGridLines := OIDrawGridLinesCheckBox.Checked;
    ObjectInspectorOptions.ShowGutter := OIShowGutterCheckBox.Checked;
    ObjectInspectorOptions.ShowStatusBar := OIShowStatusBarCheckBox.Checked;
    ObjectInspectorOptions.ShowInfoBox := OIShowInfoBoxCheckBox.Checked;
    ObjectInspectorOptions.DefaultItemHeight := RoundToInt(OIDefaultItemHeightSpinEdit.Value);
  end;
end;

class function TOIOptionsFrame.SupportedOptionsClass: TAbstractIDEOptionsClass;
begin
  Result := TEnvironmentOptions;
end;

initialization
  RegisterIDEOptionsEditor(GroupEnvironment, TOIOptionsFrame, EnvOptionsOI);
end.

