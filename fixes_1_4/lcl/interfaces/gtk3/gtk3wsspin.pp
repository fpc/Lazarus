{
 *****************************************************************************
 *                               Gtk3WSSpin.pp                               *
 *                               -------------                               * 
 *                                                                           *
 *                                                                           *
 *****************************************************************************

 *****************************************************************************
  This file is part of the Lazarus Component Library (LCL)

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************
}
unit Gtk3WSSpin;

{$mode objfpc}{$H+}
{$I gtk3defines.inc}

interface

uses
  // RTL, FCL, LCL
  SysUtils, Math, Controls, LCLType, LCLProc, Spin, StdCtrls,
  // Widgetset
  gtk3widgets, Gtk3WSStdCtrls, WSLCLClasses, WSProc, WSSpin;

type

  { TGtk3WSCustomFloatSpinEdit }

  TGtk3WSCustomFloatSpinEdit = class(TWSCustomFloatSpinEdit)
  published
    class function CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): TLCLIntfHandle; override;
    class procedure GetPreferredSize(const AWinControl: TWinControl;
                        var PreferredWidth, PreferredHeight: integer;
                        WithThemeSpace: Boolean); override;
    class function GetSelStart(const ACustomEdit: TCustomEdit): integer; override;
    class function GetSelLength(const ACustomEdit: TCustomEdit): integer; override;
    class function GetValue(const ACustomFloatSpinEdit: TCustomFloatSpinEdit): Double; override;

    class procedure SetSelStart(const ACustomEdit: TCustomEdit; NewStart: integer); override;
    class procedure SetSelLength(const ACustomEdit: TCustomEdit; NewLength: integer); override;
    class procedure SetReadOnly(const ACustomEdit: TCustomEdit; ReadOnly: boolean); override;
    class procedure UpdateControl(const ACustomFloatSpinEdit: TCustomFloatSpinEdit); override;
  end;

implementation


{ TGtk3WSCustomFloatSpinEdit }

class function TGtk3WSCustomFloatSpinEdit.CreateHandle(
  const AWinControl: TWinControl; const AParams: TCreateParams): TLCLIntfHandle;
var
  ASpin: TGtk3SpinEdit;
begin
  ASpin := TGtk3SpinEdit.Create(AWinControl, AParams);
  Result := TLCLIntfHandle(ASpin);
end;

class procedure TGtk3WSCustomFloatSpinEdit.GetPreferredSize(
  const AWinControl: TWinControl; var PreferredWidth, PreferredHeight: integer;
  WithThemeSpace: Boolean);
begin
  if not WSCheckHandleAllocated(AWinControl, 'GetPreferredSize') then
    Exit;
  TGtk3SpinEdit(AWinControl.Handle).preferredSize(PreferredWidth, PreferredHeight, WithThemeSpace);
end;

class function TGtk3WSCustomFloatSpinEdit.GetSelStart(
  const ACustomEdit: TCustomEdit): integer;
begin
  Result := -1;
  if not WSCheckHandleAllocated(ACustomEdit, 'GetSelStart') then
    Exit;
  Result := TGtk3Editable(ACustomEdit.Handle).getSelStart;
end;

class function TGtk3WSCustomFloatSpinEdit.GetSelLength(
  const ACustomEdit: TCustomEdit): integer;
begin
  Result := 0;
  if not WSCheckHandleAllocated(ACustomEdit, 'GetSelLength') then
    Exit;
  Result := TGtk3Editable(ACustomEdit.Handle).getSelLength;
end;

class function TGtk3WSCustomFloatSpinEdit.GetValue(
  const ACustomFloatSpinEdit: TCustomFloatSpinEdit): Double;
begin
  Result := 0;
  if not WSCheckHandleAllocated(ACustomFloatSpinEdit, 'GetValue') then
    Exit;
  Result := TGtk3SpinEdit(ACustomFloatSpinEdit.Handle).Value;
end;

class procedure TGtk3WSCustomFloatSpinEdit.SetSelStart(
  const ACustomEdit: TCustomEdit; NewStart: integer);
begin
  if not WSCheckHandleAllocated(ACustomEdit, 'SetSelStart') then
    Exit;
  TGtk3Editable(ACustomEdit.Handle).BeginUpdate;
  TGtk3Editable(ACustomEdit.Handle).SetSelStart(NewStart);
  TGtk3Editable(ACustomEdit.Handle).EndUpdate;
end;

class procedure TGtk3WSCustomFloatSpinEdit.SetSelLength(
  const ACustomEdit: TCustomEdit; NewLength: integer);
begin
  if not WSCheckHandleAllocated(ACustomEdit, 'SetSelLength') then
    Exit;
  TGtk3Editable(ACustomEdit.Handle).BeginUpdate;
  TGtk3Editable(ACustomEdit.Handle).SetSelLength(NewLength);
  TGtk3Editable(ACustomEdit.Handle).EndUpdate;
end;


class procedure TGtk3WSCustomFloatSpinEdit.SetReadOnly(
  const ACustomEdit: TCustomEdit; ReadOnly: boolean);
begin
  if not WSCheckHandleAllocated(ACustomEdit, 'SetReadOnly') then
    Exit;
  TGtk3Editable(ACustomEdit.Handle).BeginUpdate;
  TGtk3Editable(ACustomEdit.Handle).ReadOnly := ReadOnly;
  TGtk3Editable(ACustomEdit.Handle).EndUpdate;
end;

class procedure TGtk3WSCustomFloatSpinEdit.UpdateControl(
  const ACustomFloatSpinEdit: TCustomFloatSpinEdit);
var
  ASpin: TGtk3SpinEdit;
  AMin: Double;
  AMax: Double;
begin
  if not WSCheckHandleAllocated(ACustomFloatSpinEdit, 'UpdateControl') then
    Exit;
  ASpin := TGtk3SpinEdit(ACustomFloatSpinEdit.Handle);

  if ACustomFloatSpinEdit.MaxValue >= ACustomFloatSpinEdit.MinValue then
  begin
    AMin := ACustomFloatSpinEdit.MinValue;
    AMax := ACustomFloatSpinEdit.MaxValue;
  end else
  begin
    AMin := -MaxDouble;
    AMax := MaxDouble;
  end;
  ASpin.BeginUpdate;
  try
    ASpin.SetRange(AMin, AMax);
    ASpin.Numeric := ACustomFloatSpinEdit.DecimalPlaces > 0;
    ASpin.NumDigits := ACustomFloatSpinEdit.DecimalPlaces;
    ASpin.Step := ACustomFloatSpinEdit.Increment;
    ASpin.Value := ACustomFloatSpinEdit.Value;

    ASpin.ReadOnly := ACustomFloatSpinEdit.ReadOnly;
  finally
    ASpin.EndUpdate;
  end;
end;

end.
