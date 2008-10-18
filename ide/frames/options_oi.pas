unit options_oi;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, StdCtrls, Dialogs, Spin, LCLProc,
  ObjectInspector, lazarusidestrconsts, environmentopts;

type

  { TOIOptionsFrame }

  TOIOptionsFrame = class(TFrame)
    ObjectInspectorColorsGroupBox: TGroupBox;
    OIAutoShowCheckBox: TCheckBox;
    OIBackgroundColorButton: TColorButton;
    OIBackgroundColorLabel: TLabel;
    OIBoldNonDefaultCheckBox: TCheckBox;
    OIDefaultItemHeightLabel: TLabel;
    OIDefaultItemHeightSpinEdit: TSpinEdit;
    OIDefaultValueColorButton: TColorButton;
    OIDefaultValueColorLabel: TLabel;
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
    { private declarations }
  public
    procedure Setup;
    function GetTitle: String;
    procedure ReadSettings(AOptions: TEnvironmentOptions);
    procedure WriteSettings(AOptions: TEnvironmentOptions);
  end; 

var
  OIOptionsFrame: TOIOptionsFrame;

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
  end;
end;

initialization
  {$I options_oi.lrs}

end.

