{
 *****************************************************************************
 *                               MUIWSSpin.pp                                *
 *                               ------------                                *
 *                                                                           *
 *                                                                           *
 *****************************************************************************

 *****************************************************************************
  This file is part of the Lazarus Component Library (LCL)

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************
}
unit MUIWSSpin;

{$mode objfpc}{$H+}

interface

uses
  // RTL
  SysUtils, Math, Classes,
  // AROS
  Mui, tagsparamshelper,
  // LCL
  Controls, LCLType, LCLProc, Spin, StdCtrls,
  // Widgetset
  MUIBaseUnit,MUIstdctrls, WSLCLClasses, WSProc, WSSpin;

type

  { TMUIWSCustomFloatSpinEdit }

  TMUIWSCustomFloatSpinEdit = class(TWSCustomFloatSpinEdit)
  published
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
    class function CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): TLCLIntfHandle; override;
  end;

implementation


{ TMUIWSCustomFloatSpinEdit }


class procedure TMUIWSCustomFloatSpinEdit.GetPreferredSize(const AWinControl: TWinControl; var PreferredWidth, PreferredHeight: integer; WithThemeSpace: Boolean);
begin
  PreferredHeight := 25;
  PreferredWidth := AWinControl.Width;
end;

class function TMUIWSCustomFloatSpinEdit.GetSelStart(const ACustomEdit: TCustomEdit): integer;
begin
  Result := 0;
end;

class function TMUIWSCustomFloatSpinEdit.GetSelLength(const ACustomEdit: TCustomEdit): integer;
//var
//  AStart, AEnd: Integer;
begin
  Result := 0;
end;

class function TMUIWSCustomFloatSpinEdit.GetValue(
  const ACustomFloatSpinEdit: TCustomFloatSpinEdit): Double;
var
  Edit: TMuiSpinEdit;
begin
  if (ACustomFloatSpinEdit.Handle <> 0) and (TMUIObject(ACustomFloatSpinEdit.Handle) is TMUISpinEdit) then
  begin
    Edit := TMUISpinEdit(ACustomFloatSpinEdit.Handle);
    Result := Edit.CurValue;
  end;
end;

class procedure TMUIWSCustomFloatSpinEdit.SetSelStart(const ACustomEdit: TCustomEdit;
  NewStart: integer);
begin
  //
end;

class procedure TMUIWSCustomFloatSpinEdit.SetSelLength(const ACustomEdit: TCustomEdit;
  NewLength: integer);
begin
  //
end;

class procedure TMUIWSCustomFloatSpinEdit.SetReadOnly(const ACustomEdit: TCustomEdit; ReadOnly: boolean);
begin
  //
end;

class procedure TMUIWSCustomFloatSpinEdit.UpdateControl(
  const ACustomFloatSpinEdit: TCustomFloatSpinEdit);
var
  Edit: TMuiSpinEdit;
begin
  if (ACustomFloatSpinEdit.Handle <> 0) and (TMUIObject(ACustomFloatSpinEdit.Handle) is TMUISpinEdit) then
  begin
    Edit := TMUISpinEdit(ACustomFloatSpinEdit.Handle);
    Edit.CurValue := ACustomFloatSpinEdit.Value;
    Edit.MinValue := ACustomFloatSpinEdit.MinValue;
    Edit.MaxValue := ACustomFloatSpinEdit.MaxValue;
    Edit.Increment := ACustomFloatSpinEdit.Increment;
    Edit.Decimals := ACustomFloatSpinEdit.DecimalPlaces;
  end;
end;

class function TMUIWSCustomFloatSpinEdit.CreateHandle(
  const AWinControl: TWinControl; const AParams: TCreateParams
  ): TLCLIntfHandle;
var
  MuiEdit: TMuiSpinEdit;
  Tags: TATagList;
  CEdit: TCustomFloatSpinEdit absolute AWinControl;
begin
  Tags.Clear;
  MuiEdit := TMuiSpinEdit.Create(Tags);
  With MuiEdit do
  begin
    Left := AParams.X;
    Top := AParams.Y;
    Width := AParams.Width;
    Height := AParams.Height;
    PasObject := AWinControl;
    TabStop := AWinControl.TabStop;
    Decimals := CEdit.DecimalPlaces;
    CurValue := CEdit.Value;
    MinValue := CEdit.MinValue;
    MaxValue := CEdit.MaxValue;
    Increment := CEdit.Increment;
  end;
  if AWinControl.Parent <> nil then
  begin
    MuiEdit.Parent := TMuiObject(AWinControl.Parent.Handle);
  end;
  //
  Result := TLCLIntfHandle(MuiEdit);
  //
end;

end.
