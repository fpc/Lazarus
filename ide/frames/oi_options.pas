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
  o: TOIOptions;
begin
  o:=(AOptions as TEnvironmentOptions).ObjectInspectorOptions;
  ASettings.Colors[ocBackground] := o.GridBackgroundColor;
  ASettings.Colors[ocGutter] := o.GutterColor;
  ASettings.Colors[ocGutterEdge] := o.GutterEdgeColor;
  ASettings.Colors[ocHighlight] := o.HighlightColor;
  ASettings.Colors[ocHighlightFont] := o.HighlightFontColor;
  ASettings.Colors[ocPropName] := o.PropertyNameColor;
  ASettings.Colors[ocValue] := o.ValueColor;
  ASettings.Colors[ocDefValue] := o.DefaultValueColor;
  ASettings.Colors[ocSubProp] := o.SubPropertiesColor;
  ASettings.Colors[ocReference] := o.ReferencesColor;
  ASettings.Colors[ocReadOnly] := o.ReadOnlyColor;

  ASettings.Options[ooShowHints] := o.ShowHints;
  ASettings.Options[ooAutoShow] := o.AutoShow;
  ASettings.Options[ooBoldNonDefault] := o.BoldNonDefaultValues;
  ASettings.Options[ooDrawGridLines] := o.DrawGridLines;
  ASettings.Options[ooShowGutter] := o.ShowGutter;
  ASettings.Options[ooShowStatusBar] := o.ShowStatusBar;
  ASettings.Options[ooShowInfoBox] := o.ShowInfoBox;
  ApplyOISettings(ASettings);
  OIDefaultItemHeightSpinEdit.Value := o.DefaultItemHeight;
  FLoaded := True;
end;

procedure TOIOptionsFrame.WriteSettings(AOptions: TAbstractIDEOptions);
var
  o: TOIOptions;
begin
  o:=(AOptions as TEnvironmentOptions).ObjectInspectorOptions;
  o.GridBackgroundColor := ColorsListBox.Colors[Ord(ocBackground)];
  o.GutterColor := ColorsListBox.Colors[Ord(ocGutter)];
  o.GutterEdgeColor := ColorsListBox.Colors[Ord(ocGutterEdge)];
  o.HighlightColor := ColorsListBox.Colors[Ord(ocHighlight)];
  o.HighlightFontColor := ColorsListBox.Colors[Ord(ocHighlightFont)];
  o.PropertyNameColor := ColorsListBox.Colors[Ord(ocPropName)];
  o.ValueColor := ColorsListBox.Colors[Ord(ocValue)];
  o.DefaultValueColor := ColorsListBox.Colors[Ord(ocDefValue)];
  o.SubPropertiesColor := ColorsListBox.Colors[Ord(ocSubProp)];
  o.ReferencesColor := ColorsListBox.Colors[Ord(ocReference)];
  o.ReadOnlyColor := ColorsListBox.Colors[Ord(ocReadOnly)];

  o.ShowHints := OIShowHintCheckBox.Checked;
  o.AutoShow := OIAutoShowCheckBox.Checked;
  o.BoldNonDefaultValues := OIBoldNonDefaultCheckBox.Checked;
  o.DrawGridLines := OIDrawGridLinesCheckBox.Checked;
  o.ShowGutter := OIShowGutterCheckBox.Checked;
  o.ShowStatusBar := OIShowStatusBarCheckBox.Checked;
  o.ShowInfoBox := OIShowInfoBoxCheckBox.Checked;
  o.DefaultItemHeight := RoundToInt(OIDefaultItemHeightSpinEdit.Value);
end;

class function TOIOptionsFrame.SupportedOptionsClass: TAbstractIDEOptionsClass;
begin
  Result := TEnvironmentOptions;
end;

initialization
  RegisterIDEOptionsEditor(GroupEnvironment, TOIOptionsFrame, EnvOptionsOI);
end.

