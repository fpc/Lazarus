unit options_oi;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, StdCtrls,
  ObjectInspector, lazarusidestrconsts;

type

  { TOIOptionsFrame }

  TOIOptionsFrame = class(TFrame)
    ObjectInspectorColorsGroupBox: TGroupBox;
    OIAutoShowCheckBox: TCheckBox;
    OIBackgroundColorButton: TColorButton;
    OIBackgroundColorLabel: TLabel;
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
end;

function TOIOptionsFrame.GetTitle: String;
begin
  Result := dlgObjInsp;
end;

initialization
  {$I options_oi.lrs}

end.

