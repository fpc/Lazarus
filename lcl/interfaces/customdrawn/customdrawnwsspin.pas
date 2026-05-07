{
 *****************************************************************************
 *                                QtWSSpin.pp                                * 
 *                                -----------                                * 
 *                                                                           *
 *                                                                           *
 *****************************************************************************

 *****************************************************************************
  This file is part of the Lazarus Component Library (LCL)

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************
}
unit customdrawnwsspin;

{$mode objfpc}{$H+}

interface

{$I customdrawndefines.inc}

uses
  // RTL
  Classes,
  // LCL
  Spin, SysUtils, Controls, LCLType, LCLProc, LCLIntf, Forms,
  customdrawncontrols,
  // Widgetset
  WSProc, WSSpin, WSLCLClasses, CustomDrawnWsControls, customdrawnproc,
  customdrawnprivate;

type

  { TCDWSCustomFloatSpinEdit }

  TCDWSCustomFloatSpinEdit = class(TWSCustomFloatSpinEdit)
  public
    class procedure InjectCDControl(const AWinControl: TWinControl; var ACDControlField: TCDControl);
  published
    class function  CreateHandle(const AWinControl: TWinControl;
      const AParams: TCreateParams): TLCLHandle; override;
    class procedure DestroyHandle(const AWinControl: TWinControl); override;
    class procedure ShowHide(const AWinControl: TWinControl); override;
    class procedure GetPreferredSize(const AWinControl: TWinControl;
      var PreferredWidth, PreferredHeight: integer;
      WithThemeSpace: Boolean); override;

    class procedure UpdateControl(const ACustomFloatSpinEdit: TCustomFloatSpinEdit); override;

    class function GetValue(const ACustomFloatSpinEdit: TCustomFloatSpinEdit): Double; override;

  (*TODO: seperation into properties instead of bulk update
    class procedure SetIncrement(const ACustomFloatSpinEdit: TCustomFloatSpinEdit; NewIncrement: Double); virtual;
    class procedure SetMinValue(const ACustomFloatSpinEdit: TCustomFloatSpinEdit; NewValue: Double); virtual;
    class procedure SetMaxValue(const ACustomFloatSpinEdit: TCustomFloatSpinEdit; NewValue: Double); virtual;
    class procedure SetValueEmpty(const ACustomFloatSpinEdit: TCustomFloatSpinEdit; NewEmpty: boolean); virtual;
    *)
  end;

implementation

class procedure TCDWSCustomFloatSpinEdit.InjectCDControl(
  const AWinControl: TWinControl; var ACDControlField: TCDControl);
begin
  TCDIntfSpinEdit(ACDControlField).LCLControl := TCustomFloatSpinEdit(AWinControl);
  ACDControlField.Caption := AWinControl.Caption;
  ACDControlField.Parent := AWinControl;
  ACDControlField.Align := alClient;
end;

{------------------------------------------------------------------------------
  Method: TCDWSCustomFloatSpinEdit.CreateHandle
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
class function TCDWSCustomFloatSpinEdit.CreateHandle(const AWinControl: TWinControl;
  const AParams: TCreateParams): TLCLHandle;
var
  lCDWinControl: TCDWinControl;
begin
  Result := TCDWSWinControl.CreateHandle(AWinControl, AParams);
  lCDWinControl := TCDWinControl(Result);
  lCDWinControl.CDControl := TCDIntfSpinEdit.Create(AWinControl);
end;

class procedure TCDWSCustomFloatSpinEdit.DestroyHandle(
  const AWinControl: TWinControl);
var
  lCDWinControl: TCDWinControl;
begin
  lCDWinControl := TCDWinControl(AWinControl.Handle);
  lCDWinControl.CDControl.Free;
  lCDWinControl.Free;
end;

class procedure TCDWSCustomFloatSpinEdit.ShowHide(const AWinControl: TWinControl);
var
  lCDWinControl: TCDWinControl;
begin
  lCDWinControl := TCDWinControl(AWinControl.Handle);

  TCDWSWinControl.ShowHide(AWinControl);

  if not lCDWinControl.CDControlInjected then
  begin
    InjectCDControl(AWinControl, lCDWinControl.CDControl);
    lCDWinControl.CDControlInjected := True;
  end;
end;

{ Lazarus's WSClass dispatch (wslclclasses.pp:347 CreateVClass) walks the
  *LCL component* class hierarchy and patches the spin's VClass VMT with
  TCDWSCustomEdit's overrides for any slot the spin's WS class doesn't
  itself override -- because TCustomFloatSpinEdit IS-A TCustomEdit and
  TCDWSCustomEdit is the WS for TCustomEdit. So virtual dispatch on
  GetPreferredSize for the spin would land in TCDWSCustomEdit.GetPreferredSize,
  which calls InjectCDControl statically resolved to TCDWSCustomEdit.InjectCDControl,
  which casts the spin's TCDIntfSpinEdit as TCDIntfEdit and writes LCLControl
  -- a sibling-class field at a different offset -- corrupting TCDSpinEdit's
  FDecimalPlaces. Override here so the patched slot is replaced with our
  spin-aware version that calls our own InjectCDControl with the right cast. }
class procedure TCDWSCustomFloatSpinEdit.GetPreferredSize(
  const AWinControl: TWinControl; var PreferredWidth, PreferredHeight: integer;
  WithThemeSpace: Boolean);
var
  lCDWinControl: TCDWinControl;
begin
  lCDWinControl := TCDWinControl(AWinControl.Handle);
  if (lCDWinControl = nil) or (lCDWinControl.CDControl = nil) then Exit;
  if not lCDWinControl.CDControlInjected then
  begin
    InjectCDControl(AWinControl, lCDWinControl.CDControl);
    lCDWinControl.CDControlInjected := True;
  end;
  lCDWinControl.CDControl.LCLWSCalculatePreferredSize(
    PreferredWidth, PreferredHeight, WithThemeSpace, AWinControl.AutoSize, False);
end;

class function  TCDWSCustomFloatSpinEdit.GetValue(const ACustomFloatSpinEdit: TCustomFloatSpinEdit): Double;
var
  lCDWinControl: TCDWinControl;
begin
  lCDWinControl := TCDWinControl(ACustomFloatSpinEdit.Handle);
  Result := TCDIntfSpinEdit(lCDWinControl.CDControl).Value;
end;

{ Push the LCL spin's full state (Value, DecimalPlaces, range,
  Increment) into the injected control. Called by
  TCustomFloatSpinEdit.SetValue / SetMinValue / SetMaxValue /
  SetIncrement / SetDecimalPlaces, by RealSetText when the assigned
  text is a parseable number, and indirectly by Ctrl+V of a numeric
  string (Paste -> SetSelText -> Text := -> RealSetText -> SetValue).
  Without this every one of those paths updates the LCL state but
  leaves the injected control's text and arrow range stale, so
  Spin1.Value := 5, MinValue := 10, paste, cut etc. all appear to do
  nothing visible. }
class procedure TCDWSCustomFloatSpinEdit.UpdateControl(const ACustomFloatSpinEdit: TCustomFloatSpinEdit);
var
  lCDWinControl: TCDWinControl;
  lIntf: TCDIntfSpinEdit;
begin
  if not WSCheckHandleAllocated(ACustomFloatSpinEdit, 'UpdateControl') then
    Exit;
  lCDWinControl := TCDWinControl(ACustomFloatSpinEdit.Handle);
  if (lCDWinControl = nil) or (lCDWinControl.CDControl = nil) then Exit;
  lIntf := TCDIntfSpinEdit(lCDWinControl.CDControl);
  lIntf.DecimalPlaces := ACustomFloatSpinEdit.DecimalPlaces;
  lIntf.MinValue := ACustomFloatSpinEdit.MinValue;
  lIntf.MaxValue := ACustomFloatSpinEdit.MaxValue;
  lIntf.Increment := ACustomFloatSpinEdit.Increment;
  lIntf.Value := ACustomFloatSpinEdit.Value;  { also refreshes Text via DoUpdateText }
  lIntf.Invalidate();
end;

end.
