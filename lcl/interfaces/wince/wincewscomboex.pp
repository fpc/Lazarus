unit WinCEWSComboEx;

{$mode objfpc}{$H+}

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
  { TWinCEWSCustomComboBoxEx }

  TWinCEWSCustomComboBoxEx = class(TWSCustomComboBoxEx)
  published
    class function GetFocusedEditableMainItemNoDD(const ACombo: TCustomComboboxEx;
      ADroppedDown, AMainItem: Boolean): boolean; override;
  end;
  
  { TWinCEWSCustomCheckCombo }
  
  TWinCEWSCustomCheckCombo = class(TWSCustomCheckCombo)
  published
    class function GetFocusedEditableMainItemNoDD(const ACombo: TCustomCheckCombo;
      ADroppedDown: Boolean; ALeft: Integer): boolean; override;
    class function GetRejectDropdown(const ACombo: TCustomCheckCombo;
      ALeft, ARight: Integer): Boolean; override;
    class procedure GetRejectToggleOnSelect(
      const ACombo: TCustomCheckCombo; var AResult: Boolean); override;
  end;
  
  
implementation

{ TWinCEWSCustomComboBoxEx }

class function TWinCEWSCustomComboBoxEx.GetFocusedEditableMainItemNoDD(
  const ACombo: TCustomComboBoxEx; ADroppedDown, AMainItem: Boolean): boolean;
begin
  Result := ACombo.Focused and aMainItem and not aDropped;
end;

{ TWinCEWSCustomCheckCombo }

class function TWinCEWSCustomCheckCombo.GetFocusedEditableMainItemNoDD(
  const ACombo: TCustomCheckCombo; ADroppedDown: Boolean; ALeft: Integer): boolean;
begin
  Result := ACombo.Focused and (ALeft > 0) and not ADroppedDown;
end;

class function TWinCEWSCustomCheckCombo.GetRejectDropdown(
  const ACombo: TCustomCheckCombo; ALeft, ARight: Integer): Boolean;
begin
  Result := false;
end;

class procedure TWinCEWSCustomCheckCombo.GetRejectToggleOnSelect(
  const ACombo: TCustomCheckCombo; var AResult: Boolean);
 begin
   if ACombo.DroppedDown then AResult := true;
 end;

end.
