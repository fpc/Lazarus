unit CocoaWSComboEx;

{$mode objfpc}{$H+}
{$WARN 5024 off : Parameter "$1" not used}

interface

uses
////////////////////////////////////////////////////
// I M P O R T A N T
////////////////////////////////////////////////////
// To get as little as possible circles,
// uncomment only when needed for registration
////////////////////////////////////////////////////
  Classes, SysUtils, 
  StdCtrls, Controls, Graphics, Forms, Themes,
////////////////////////////////////////////////////
  ComboEx, WSComboEx;

type
  { TCocoaCustomComboBoxEx }

  TCocoaCustomComboBoxEx = class(TWSCustomComboBoxEx)
  published
    class function GetFocusedEditableMainItemNoDD(const ACombo: TCustomComboboxEx;
      ADroppedDown, AMainItem: Boolean): boolean; override;
  end;
  
  { TCocoaWSCustomCheckCombo }
  
  TCocoaWSCustomCheckCombo = class(TWSCustomCheckCombo)
  published
    class function GetFocusedEditableMainItemNoDD(const ACombo: TCustomCheckCombo;
      ADroppedDown: Boolean; ALeft: Integer): boolean; override;
    class function GetRejectDropdown(const {%H-}ACombo: TCustomCheckCombo;
      ALeft, {%H-}ARight: Integer): Boolean; override;
  end;
  
  
implementation

{ TCocoaWSCustomComboBoxEx }

class function TCocoaWSCustomComboBoxEx.GetFocusedEditableMainItemNoDD(
  const ACombo: TCustomComboBoxEx; ADroppedDown, AMainItem: Boolean): boolean;
begin
  Result := ACombo.Focused and aMainItem and not ADroppedDown;
end;

{ TCocoaWSCustomCheckCombo }

class function TCocoaWSCustomCheckCombo.GetFocusedEditableMainItemNoDD(
  const ACombo: TCustomCheckCombo; ADroppedDown: Boolean; ALeft: Integer): boolean;
begin
  Result := ACombo.Focused and (ALeft > 0) and not ADroppedDown;
end;

class procedure TWin32WSCustomCheckCombo.GetRejectToggleOnSelect(
  const ACombo: TCustomCheckCombo; var AResult: Boolean);
 begin
   if ACombo.DroppedDown then AResult := true;
//   AResult := false;
 end;

end.
