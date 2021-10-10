unit WSComboEx;

{$mode objfpc}{$H+}
//{$I lcl_defines.inc}

interface
////////////////////////////////////////////////////
// I M P O R T A N T
////////////////////////////////////////////////////
// 1) Only class methods allowed
// 2) Class methods have to be published and virtual
// 3) To get as little as posible circles, the uses
//    clause should contain only those LCL units 
//    needed for registration. WSxxx units are OK
// 4) To improve speed, register only classes in the 
//    initialization section which actually 
//    implement something
// 5) To enable your XXX widgetset units, look at
//    the uses clause of the XXXintf.pp
////////////////////////////////////////////////////
uses
  Classes,
////////////////////////////////////////////////////
// To get as little as possible circles,
// uncomment only when needed for registration
////////////////////////////////////////////////////
  Graphics, Controls, StdCtrls,
  Types, ComboEx,
////////////////////////////////////////////////////
  WSControls, WSFactory, WSLCLClasses, WSStdCtrls;

type
  { TWSCustomComboBoxEx }
  
  TWSCustomComboBoxEx = class(TWSCustomComboBox)
  published
    class function GetFocusedEditableMainItemNoDD(const ACombo: TCustomComboBoxEx;
      ADroppedDown, AMainItem: Boolean): boolean; virtual;
  end;
  TWSCustomComboBoxExClass = class of TWSCustomComboBoxEx;
  
  { TWSCustomCheckCombo }
  
  TWSCustomCheckCombo = class(TWSCustomComboBox)
  published
    class function GetFocusedEditableMainItemNoDD(const ACombo: TCustomCheckCombo;
      ADroppedDown: Boolean; ALeft: Integer): boolean; virtual;
    class function GetRejectDropdown(const ACombo: TCustomCheckCombo;
      ALeft, ARight: Integer): Boolean; virtual;
    class procedure GetRejectToggleOnSelect(const ACombo: TCustomCheckCombo;
      var AResult: Boolean); virtual;
  end;
  TWSCustomCheckComboClass = class of TWSCustomCheckCombo;

  { WidgetSetRegistration }

  procedure RegisterCustomComboBoxEx;
  procedure RegisterCustomCheckCombo;

implementation

{ TWSCustomComboBoxEx }

class function TWSCustomComboBoxEx.GetFocusedEditableMainItemNoDD(
  const ACombo: TCustomComboBoxEx; ADroppedDown, AMainItem: Boolean): boolean;
begin
  Result := false;
end;

{ TWSCustomCheckCombo }

class function TWSCustomCheckCombo.GetFocusedEditableMainItemNoDD(
  const ACombo: TCustomCheckCombo; ADroppedDown: Boolean; ALeft: Integer): boolean;
begin
  Result := false;
end;
  
class function TWSCustomCheckCombo.GetRejectDropdown(
  const ACombo: TCustomCheckCombo; ALeft, ARight: Integer): Boolean;
var
  aCursorPos: TPoint;
  aRect: TRect;
begin
  aCursorPos := ACombo.ScreenToControl(Mouse.CursorPos);
  aRect := Rect(ALeft, 0, ARight, ACombo.Height);
  Result := PtInRect(aRect, aCursorPos);
end;

class procedure TWSCustomCheckCombo.GetRejectToggleOnSelect(
  const ACombo: TCustomCheckCombo; var AResult: Boolean);
begin
end;

{ WidgetSetRegistration }

procedure RegisterCustomComboBoxEx;
const
  Done: Boolean = False;
begin
  if Done then exit;
  WSRegisterCustomComboBoxEx;
//  if not WSRegisterCustomComboBoxEs then
//    RegisterWSComponent(TCustomComboBoxEx, TWSCustomComboBoxEx);
  Done := True;
end;

procedure RegisterCustomCheckCombo;
const
  Done: Boolean = False;
begin
  if Done then exit;
  WSRegisterCustomCheckCombo;
//  if not WSRegisterCustomCheckCombo then
//    RegisterWSComponent(TCustomCheckCombo, TWSCustomCheckCombo);
  Done := True;
end;

end.


  
