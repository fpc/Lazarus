{
 *****************************************************************************
  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************

  Author: Maciej Izak

  DaThoX 2004-2015
  FreeSparta.com
}

unit sparta_FakeFrame;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, sparta_FakeCustom, Controls, Forms;

type
  TFakeFrame = class(TFakeCustomFrame)
  end;

  { THookFrame - temporary name need refactoring }

  THookFrame = class(TFrame)
  private
    FHackAlign: TAlign;
    FHackAnchors: TAnchors;

    function IsAnchorsStored: Boolean;
    function GetAlign: TAlign;
    procedure SetAlign(Value: TAlign);
    function GetAnchors: TAnchors;
    procedure SetAnchors(const AValue: TAnchors);
  public
    constructor Create(TheOwner: TComponent); override;
  published
    property Align: TAlign read GetAlign write SetAlign default alNone;
    property Anchors: TAnchors read GetAnchors write SetAnchors stored IsAnchorsStored default [akLeft, akTop];
  end;

implementation



{ THookFrame }

function THookFrame.IsAnchorsStored: Boolean;
begin
  Result:=(Anchors<>AnchorAlign[Align]);
end;

function THookFrame.GetAlign: TAlign;
begin
  if not (csDesignInstance in ComponentState) then
    Result := inherited Align
  else
    Result := FHackAlign;
end;

procedure THookFrame.SetAlign(Value: TAlign);
var
  OldAlign: TAlign;
  a: TAnchorKind;
begin
  if not (csDesignInstance in ComponentState) then
    inherited Align := Value
  else begin
    if FHackAlign = Value then exit;
    OldAlign := FHackAlign;
    FHackAlign := Value;
    if (not (csLoading in ComponentState))
    and (Align in [alLeft,alTop,alRight,alBottom,alClient]) then begin
      // Align for alLeft,alTop,alRight,alBottom,alClient takes precedence
      // over AnchorSides => clean up
      for a:=low(TAnchorKind) to High(TAnchorKind) do
      begin
        if not (a in AnchorAlign[FHackAlign]) then continue;
        AnchorSide[a].Control:=nil;
        AnchorSide[a].Side:=asrTop;
      end;
    end;

    // Notes:
    // - if anchors had default values then change them to new default values
    //   This is done for Delphi compatibility.
    // - Anchors are not stored if they are AnchorAlign[Align]
    if (Anchors = AnchorAlign[OldAlign]) and (Anchors <> AnchorAlign[FHackAlign]) then
      Anchors := AnchorAlign[FHackAlign];
  end;
end;

function THookFrame.GetAnchors: TAnchors;
begin
  if not (csDesignInstance in ComponentState) then
    Result := inherited Anchors
  else
    Result := FHackAnchors;
end;

procedure THookFrame.SetAnchors(const AValue: TAnchors);
var
  NewAnchors: TAnchors;
  a: TAnchorKind;
begin
  if not (csDesignInstance in ComponentState) then
    inherited Anchors := AValue
  else begin
    if Anchors = AValue then Exit;
    NewAnchors:=AValue-FHackAnchors;
    FHackAnchors := AValue;
    for a:=Low(TAnchorKind) to high(TAnchorKind) do
      if (a in NewAnchors) and (AnchorSide[a].Side=asrCenter) then
        AnchorSide[a].FixCenterAnchoring;
  end;
end;

constructor THookFrame.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  FHackAnchors := [akLeft,akTop];
  FHackAlign := alNone;
end;

end.

