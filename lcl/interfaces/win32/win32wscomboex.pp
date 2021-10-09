unit Win32WSComboEx;

{$mode objfpc}{$H+}
{$I win32defines.inc}

interface

uses
////////////////////////////////////////////////////
// I M P O R T A N T
////////////////////////////////////////////////////
// To get as little as posible circles,
// uncomment only when needed for registration
////////////////////////////////////////////////////
  Classes, SysUtils, CommCtrl,
  StdCtrls, Controls, Graphics, Forms, Themes,
////////////////////////////////////////////////////
  ComboEx, WSComboEx;

type
  { TWin32WSCustomComboBoxEx }

  TWin32WSCustomComboBoxEx = class(TWSCustomComboBoxEx)
  published
    class function GetFocusedEditableMainItemNoDD(const ACombo: TCustomComboboxEx;
      ADroppedDown, AMainItem: Boolean): boolean; override;
  end;
  
  { TWin32WSCustomCheckCombo }
  
  TWin32WSCustomCheckCombo = class(TWSCustomCheckCombo)
  published
    class function GetFocusedEditableMainItemNoDD(const ACombo: TCustomCheckCombo;
      ADroppedDown: Boolean; ALeft: Integer): boolean; override;
    class function GetRejectDropdown(const ACombo: TCustomCheckCombo;
      ALeft, ARight: Integer): Boolean; override;
    class procedure GetRejectToggleOnSelect(const ACombo: TCustomCheckCombo;
      var AResult: Boolean); override;
  end;
  
  
implementation

{ TWin32WSCustomComboBoxEx }

class function TWin32WSCustomComboBoxEx.GetFocusedEditableMainItemNoDD(
  const ACombo: TCustomComboBoxEx; ADroppedDown, AMainItem: Boolean): boolean;
begin
  Result := ACombo.Focused and aMainItem and not ADroppedDown;
end;

{ TWin32WSCustomCheckCombo }

class function TWin32WSCustomCheckCombo.GetFocusedEditableMainItemNoDD(
  const ACombo: TCustomCheckCombo; ADroppedDown: Boolean; ALeft: Integer): boolean;
begin
  Result := ACombo.Focused and (ALeft > 0) and not ADroppedDown;
end;

class function TWin32WSCustomCheckCombo.GetRejectDropdown(
  const ACombo: TCustomCheckCombo; ALeft, ARight: Integer): Boolean;
begin
  Result := false;
end;

class procedure TWin32WSCustomCheckCombo.GetRejectToggleOnSelect(
  const ACombo: TCustomCheckCombo; var AResult: Boolean);
 begin
   if ACombo.DroppedDown then AResult := true;
 end;

end.
