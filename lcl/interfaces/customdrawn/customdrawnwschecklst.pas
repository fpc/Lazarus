{
 *****************************************************************************
  This file is part of the Lazarus Component Library (LCL)

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************
}
unit customdrawnwschecklst;

{$mode objfpc}{$H+}

interface

uses
  // LCL
  Classes, Types, Controls, StdCtrls, CheckLst, LCLType,
  // Widgetset
  WSCheckLst, WSLCLClasses,
  CustomDrawnWSStdCtrls, CustomDrawnWsControls,
  customdrawncontrols, customdrawnproc, customdrawnprivate;

type

  { TCDWSCustomCheckListBox -- the customdrawn host class for
    TCustomCheckListBox. Inherits the listbox handle / strings /
    selection plumbing from TCDWSCustomListBox; adds the check-state,
    enabled-state and header-row methods on top. The injected
    control's class is TCDIntfCheckListBox, NOT TCDIntfListBox. }

  TCDWSCustomCheckListBox = class(TWSCustomCheckListBox)
  public
    class procedure InjectCDControl(const AWinControl: TWinControl;
      var ACDControlField: TCDControl);
  published
    // TWSWinControl
    class function CreateHandle(const AWinControl: TWinControl;
      const AParams: TCreateParams): TLCLHandle; override;
    class procedure DestroyHandle(const AWinControl: TWinControl); override;
    class procedure ShowHide(const AWinControl: TWinControl); override;
    class procedure GetPreferredSize(const AWinControl: TWinControl;
      var PreferredWidth, PreferredHeight: integer;
      WithThemeSpace: Boolean); override;

    // TWSCustomListBox (inherited via TWSCustomCheckListBox -> TWSCustomListBox)
    class function GetIndexAtXY(const ACustomListBox: TCustomListBox; X, Y: integer): integer; override;
    class function GetItemIndex(const ACustomListBox: TCustomListBox): integer; override;
    class function GetItemRect(const ACustomListBox: TCustomListBox; Index: integer; var ARect: TRect): boolean; override;
    class function GetSelCount(const ACustomListBox: TCustomListBox): integer; override;
    class function GetSelected(const ACustomListBox: TCustomListBox; const AIndex: integer): boolean; override;
    class function GetStrings(const ACustomListBox: TCustomListBox): TStrings; override;
    class procedure FreeStrings(var AStrings: TStrings); override;
    class function GetTopIndex(const ACustomListBox: TCustomListBox): integer; override;
    class procedure SelectItem(const ACustomListBox: TCustomListBox; AIndex: integer; ASelected: boolean); override;
    class procedure SelectRange(const ACustomListBox: TCustomListBox; ALow, AHigh: integer; ASelected: boolean); override;
    class procedure SetItemIndex(const ACustomListBox: TCustomListBox; const AIndex: integer); override;
    class procedure SetSelectionMode(const ACustomListBox: TCustomListBox;
      const AExtendedSelect, AMultiSelect: boolean); override;
    class procedure SetSorted(const ACustomListBox: TCustomListBox; AList: TStrings; ASorted: boolean); override;
    class procedure SetTopIndex(const ACustomListBox: TCustomListBox; const NewTopIndex: integer); override;

    // TWSCustomCheckListBox
    class function GetCheckWidth(const ACheckListBox: TCustomCheckListBox): integer; override;
    class function GetItemEnabled(const ACheckListBox: TCustomCheckListBox; const AIndex: integer): Boolean; override;
    class function GetHeader(const ACheckListBox: TCustomCheckListBox; const AIndex: integer): Boolean; override;
    class function GetState(const ACheckListBox: TCustomCheckListBox; const AIndex: integer): TCheckBoxState; override;
    class procedure SetItemEnabled(const ACheckListBox: TCustomCheckListBox; const AIndex: integer; const AEnabled: Boolean); override;
    class procedure SetHeader(const ACheckListBox: TCustomCheckListBox; const AIndex: integer; const AHeader: Boolean); override;
    class procedure SetState(const ACheckListBox: TCustomCheckListBox; const AIndex: integer; const AState: TCheckBoxState); override;
  end;

implementation

{ Helper -- pull the injected TCDCheckListBox out of the LCL TCustomCheckListBox.
  Returns nil before the handle exists (e.g. property defaults loading). }
function CDCheckListBoxFromLCL(const AControl: TCustomListBox): TCDCheckListBox;
var
  WSCtrl: TCDWinControl;
begin
  Result := nil;
  if (AControl = nil) or (AControl.Handle = 0) then Exit;
  WSCtrl := TCDWinControl(AControl.Handle);
  if (WSCtrl = nil) or (WSCtrl.CDControl = nil) then Exit;
  if WSCtrl.CDControl is TCDCheckListBox then
    Result := TCDCheckListBox(WSCtrl.CDControl);
end;

class procedure TCDWSCustomCheckListBox.InjectCDControl(
  const AWinControl: TWinControl; var ACDControlField: TCDControl);
var
  LCLLB: TCustomCheckListBox;
  Inj:   TCDIntfCheckListBox;
begin
  LCLLB := TCustomCheckListBox(AWinControl);
  Inj := TCDIntfCheckListBox(ACDControlField);
  Inj.LCLControl := LCLLB;
  Inj.Parent := AWinControl;
  Inj.Align := alClient;
  { Same as plain TCustomListBox: TCustomListBox.InitializeWnd does not
    call SetSelectionMode, and the LCL TCheckListBox-only properties
    (AllowGrayed, header colours) are not pushed via setters either,
    because the setters early-out before HandleAllocated. Copy them
    over here. }
  Inj.MultiSelect           := LCLLB.MultiSelect;
  Inj.ExtendedSelect        := LCLLB.ExtendedSelect;
  Inj.Sorted                := LCLLB.Sorted;
  Inj.AllowGrayed           := LCLLB.AllowGrayed;
  Inj.HeaderColor           := LCLLB.HeaderColor;
  Inj.HeaderBackgroundColor := LCLLB.HeaderBackgroundColor;
  if LCLLB.ItemIndex >= 0 then Inj.ItemIndex := LCLLB.ItemIndex;
  {$ifdef VerboseCDInjectedControlNames}
  Inj.Name := 'CustomDrawnInternal_' + AWinControl.Name;
  {$endif}
end;

class function TCDWSCustomCheckListBox.CreateHandle(
  const AWinControl: TWinControl; const AParams: TCreateParams): TLCLHandle;
var
  lCDWinControl: TCDWinControl;
begin
  Result := TCDWSWinControl.CreateHandle(AWinControl, AParams);
  lCDWinControl := TCDWinControl(Result);
  lCDWinControl.CDControl := TCDIntfCheckListBox.Create(AWinControl);
end;

class procedure TCDWSCustomCheckListBox.DestroyHandle(const AWinControl: TWinControl);
var
  lCDWinControl: TCDWinControl;
begin
  lCDWinControl := TCDWinControl(AWinControl.Handle);
  lCDWinControl.CDControl.Free;
  lCDWinControl.Free;
end;

class procedure TCDWSCustomCheckListBox.ShowHide(const AWinControl: TWinControl);
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

class procedure TCDWSCustomCheckListBox.GetPreferredSize(
  const AWinControl: TWinControl;
  var PreferredWidth, PreferredHeight: integer; WithThemeSpace: Boolean);
begin
  PreferredWidth := 0;
  PreferredHeight := 0;
end;

{ TWSCustomListBox overrides -- delegate to the injected TCDCheckListBox.
  These mirror TCDWSCustomListBox; we can't simply inherit because that
  class's helper grabs TCDListBox specifically. }

class function TCDWSCustomCheckListBox.GetIndexAtXY(
  const ACustomListBox: TCustomListBox; X, Y: integer): integer;
var L: TCDCheckListBox;
begin
  Result := -1;
  L := CDCheckListBoxFromLCL(ACustomListBox);
  if L <> nil then Result := L.ItemAtPos(X, Y);
end;

class function TCDWSCustomCheckListBox.GetItemIndex(
  const ACustomListBox: TCustomListBox): integer;
var L: TCDCheckListBox;
begin
  Result := -1;
  L := CDCheckListBoxFromLCL(ACustomListBox);
  if L <> nil then Result := L.ItemIndex;
end;

class function TCDWSCustomCheckListBox.GetItemRect(
  const ACustomListBox: TCustomListBox; Index: integer; var ARect: TRect): boolean;
var L: TCDCheckListBox;
begin
  Result := False;
  L := CDCheckListBoxFromLCL(ACustomListBox);
  if L = nil then Exit;
  ARect := L.ItemRect(Index);
  Result := (ARect.Right - ARect.Left) > 0;
end;

class function TCDWSCustomCheckListBox.GetSelCount(
  const ACustomListBox: TCustomListBox): integer;
var L: TCDCheckListBox;
begin
  Result := 0;
  L := CDCheckListBoxFromLCL(ACustomListBox);
  if L <> nil then Result := L.GetSelCount;
end;

class function TCDWSCustomCheckListBox.GetSelected(
  const ACustomListBox: TCustomListBox; const AIndex: integer): boolean;
var L: TCDCheckListBox;
begin
  Result := False;
  L := CDCheckListBoxFromLCL(ACustomListBox);
  if L <> nil then Result := L.GetSelected(AIndex);
end;

class function TCDWSCustomCheckListBox.GetStrings(
  const ACustomListBox: TCustomListBox): TStrings;
var L: TCDCheckListBox;
begin
  Result := nil;
  L := CDCheckListBoxFromLCL(ACustomListBox);
  if L <> nil then Result := L.Items;
end;

class procedure TCDWSCustomCheckListBox.FreeStrings(var AStrings: TStrings);
begin
  AStrings := nil;
end;

class function TCDWSCustomCheckListBox.GetTopIndex(
  const ACustomListBox: TCustomListBox): integer;
var L: TCDCheckListBox;
begin
  Result := 0;
  L := CDCheckListBoxFromLCL(ACustomListBox);
  if L <> nil then Result := L.TopIndex;
end;

class procedure TCDWSCustomCheckListBox.SelectItem(
  const ACustomListBox: TCustomListBox; AIndex: integer; ASelected: boolean);
var L: TCDCheckListBox;
begin
  L := CDCheckListBoxFromLCL(ACustomListBox);
  if L <> nil then L.SetSelected(AIndex, ASelected);
end;

class procedure TCDWSCustomCheckListBox.SelectRange(
  const ACustomListBox: TCustomListBox; ALow, AHigh: integer; ASelected: boolean);
var L: TCDCheckListBox;
begin
  L := CDCheckListBoxFromLCL(ACustomListBox);
  if L <> nil then L.SelectRange(ALow, AHigh, ASelected);
end;

class procedure TCDWSCustomCheckListBox.SetItemIndex(
  const ACustomListBox: TCustomListBox; const AIndex: integer);
var L: TCDCheckListBox;
begin
  L := CDCheckListBoxFromLCL(ACustomListBox);
  if L <> nil then L.ItemIndex := AIndex;
end;

class procedure TCDWSCustomCheckListBox.SetSelectionMode(
  const ACustomListBox: TCustomListBox; const AExtendedSelect, AMultiSelect: boolean);
var L: TCDCheckListBox;
begin
  L := CDCheckListBoxFromLCL(ACustomListBox);
  if L = nil then Exit;
  L.MultiSelect    := AMultiSelect;
  L.ExtendedSelect := AExtendedSelect;
end;

class procedure TCDWSCustomCheckListBox.SetSorted(
  const ACustomListBox: TCustomListBox; AList: TStrings; ASorted: boolean);
var L: TCDCheckListBox;
begin
  L := CDCheckListBoxFromLCL(ACustomListBox);
  if L <> nil then L.Sorted := ASorted;
end;

class procedure TCDWSCustomCheckListBox.SetTopIndex(
  const ACustomListBox: TCustomListBox; const NewTopIndex: integer);
var L: TCDCheckListBox;
begin
  L := CDCheckListBoxFromLCL(ACustomListBox);
  if L <> nil then L.TopIndex := NewTopIndex;
end;

{ TWSCustomCheckListBox overrides }

class function TCDWSCustomCheckListBox.GetCheckWidth(
  const ACheckListBox: TCustomCheckListBox): integer;
var L: TCDCheckListBox;
begin
  Result := 0;
  L := CDCheckListBoxFromLCL(ACheckListBox);
  if L <> nil then Result := L.GetCheckWidth;
end;

class function TCDWSCustomCheckListBox.GetItemEnabled(
  const ACheckListBox: TCustomCheckListBox; const AIndex: integer): Boolean;
var L: TCDCheckListBox;
begin
  Result := True;
  L := CDCheckListBoxFromLCL(ACheckListBox);
  if L <> nil then Result := L.GetItemEnabled(AIndex);
end;

class function TCDWSCustomCheckListBox.GetHeader(
  const ACheckListBox: TCustomCheckListBox; const AIndex: integer): Boolean;
var L: TCDCheckListBox;
begin
  Result := False;
  L := CDCheckListBoxFromLCL(ACheckListBox);
  if L <> nil then Result := L.GetItemHeader(AIndex);
end;

class function TCDWSCustomCheckListBox.GetState(
  const ACheckListBox: TCustomCheckListBox; const AIndex: integer): TCheckBoxState;
var L: TCDCheckListBox;
begin
  Result := cbUnchecked;
  L := CDCheckListBoxFromLCL(ACheckListBox);
  if L <> nil then Result := L.GetItemState(AIndex);
end;

class procedure TCDWSCustomCheckListBox.SetItemEnabled(
  const ACheckListBox: TCustomCheckListBox; const AIndex: integer;
  const AEnabled: Boolean);
var L: TCDCheckListBox;
begin
  L := CDCheckListBoxFromLCL(ACheckListBox);
  if L <> nil then L.SetItemEnabled(AIndex, AEnabled);
end;

class procedure TCDWSCustomCheckListBox.SetHeader(
  const ACheckListBox: TCustomCheckListBox; const AIndex: integer;
  const AHeader: Boolean);
var L: TCDCheckListBox;
begin
  L := CDCheckListBoxFromLCL(ACheckListBox);
  if L <> nil then L.SetItemHeader(AIndex, AHeader);
end;

class procedure TCDWSCustomCheckListBox.SetState(
  const ACheckListBox: TCustomCheckListBox; const AIndex: integer;
  const AState: TCheckBoxState);
var L: TCDCheckListBox;
begin
  L := CDCheckListBoxFromLCL(ACheckListBox);
  if L <> nil then L.SetItemState(AIndex, AState);
end;

end.
