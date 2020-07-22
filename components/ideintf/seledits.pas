{
 *****************************************************************************
  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************

  Author: Sven Barth

  Abstract:
    This unit contains selection editors for various LCL components.
}
unit SelEdits;

{$mode objfpc}{$H+}

interface

uses
  // RTL / FCL
  SysUtils, TypInfo,
  // LCL
  Controls, ExtCtrls,
  // IdeIntf
  PropEdits;

type

  { TFlowPanelControlIndexEditor }

  TFlowPanelControlIndexEditor = class(TSelectionEditor)
  public
    function GetAttributes: TSelectionEditorAttributes; override;
    procedure FilterProperties(ASelection: TPersistentSelectionList;
      AProperties: TPropertyEditorList); override;
  end;

implementation

type

  { TFlowPanelControlIndexProperty }

  TFlowPanelControlIndexProperty = class(TPropertyEditor)
  private
    FPanel: TCustomFlowPanel;
    FControl: TControl;
    FPropInfo: TPropInfo;
    FPropType: PTypeInfo;
  public
    constructor Create(APanel: TCustomFlowPanel; AControl: TControl; AHook: TPropertyEditorHook); reintroduce;
    function GetAttributes: TPropertyAttributes; override;
    function GetValue: ansistring; override;
    procedure SetValue(const NewValue: ansistring); override;
  end;

{ TFlowPanelControlIndexProperty }

constructor TFlowPanelControlIndexProperty.Create(APanel: TCustomFlowPanel;
  AControl: TControl; AHook: TPropertyEditorHook);
begin
  inherited Create(AHook, 1);
  FPanel := APanel;
  FControl := AControl;
  FPropType := TypeInfo(Integer);
{$if FPC_FULLVERSION<30101}
  FPropInfo.PropType := FPropType;
{$else}
  FPropInfo.PropTypeRef := @FPropType;
{$endif}
  FPropInfo.Name := 'ControlIndex';
  SetPropEntry(0, Nil, @FPropInfo);
end;

function TFlowPanelControlIndexProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [];
end;

function TFlowPanelControlIndexProperty.GetValue: ansistring;
begin
  Result := IntToStr(FPanel.GetControlIndex(FControl));
end;

procedure TFlowPanelControlIndexProperty.SetValue(const NewValue: ansistring);
var
  idx: Integer;
begin
  if not TryStrToInt(NewValue, idx) then
    Exit;
  FPanel.SetControlIndex(FControl, idx);
end;

{ TFlowPanelControlIndexEditor }

function TFlowPanelControlIndexEditor.GetAttributes: TSelectionEditorAttributes;
begin
  Result := [seaFilterProperties];
end;

procedure TFlowPanelControlIndexEditor.FilterProperties(
  ASelection: TPersistentSelectionList; AProperties: TPropertyEditorList);
var
  ctrl: TControl;
  i: LongInt;
  todelete: array[0..1] of LongInt;
  propname: ShortString;
begin
  if not Assigned(ASelection) or not Assigned(AProperties) then
    Exit;
  if ASelection.Count <> 1 then
    Exit;
  todelete[0] := -1;
  todelete[1] := -1;
  for i := 0 to AProperties.Count - 1 do begin
    if AProperties[i] is TFlowPanelControlIndexProperty then
      Exit;
    propname := UpperCase(AProperties[i].GetName);
    if propname = 'CONTROLINDEX' then
      Exit
    else if propname = 'LEFT' then
      todelete[0] := i
    else if propname = 'TOP' then
      todelete[1] := i;
  end;

  if not (ASelection[0] is TControl) then
    Exit;
  ctrl := TControl(ASelection[0]);
  if not (ctrl.Parent is TCustomFlowPanel) then
    Exit;

  if todelete[0] < todelete[1] then begin
    i := todelete[0];
    todelete[0] := todelete[1];
    todelete[1] := i;
  end;
  for i := Low(todelete) to High(todelete) do
    if todelete[i] >= 0 then
      AProperties.Delete(todelete[i]);

  AProperties.Add(TFlowPanelControlIndexProperty.Create(TCustomFlowPanel(ctrl.Parent), ctrl, Hook));
end;

initialization
  { we need to register this for TControl as this is applied to each control
    that is placed on a TFlowPanel }
  RegisterSelectionEditor(TControl, TFlowPanelControlIndexEditor);
end.

