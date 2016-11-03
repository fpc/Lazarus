{   Component(s):
    tcyBaseLed

    Description:
    A base led component with Group feature
    Led states : ON/OFF/DISABLE

    * ***** BEGIN LICENSE BLOCK *****
    *
    * Version: MPL 1.1
    *
    * The contents of this file are subject to the Mozilla Public License Version
    * 1.1 (the "License"); you may not use this file except in compliance with the
    * License. You may obtain a copy of the License at http://www.mozilla.org/MPL/
    *
    * Software distributed under the License is distributed on an "AS IS" basis,
    * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
    * the specific language governing rights and limitations under the License.
    *
    * The Initial Developer of the Original Code is Mauricio
    * (https://sourceforge.net/projects/tcycomponents/).
    *
    * Alternatively, the contents of this file may be used under the terms of
    * either the GNU General Public License Version 2 or later (the "GPL"), or the
    * GNU Lesser General Public License Version 2.1 or later (the "LGPL"), in which
    * case the provisions of the GPL or the LGPL are applicable instead of those
    * above. If you wish to allow use of your version of this file only under the
    * terms of either the GPL or the LGPL, and not to allow others to use your
    * version of this file under the terms of the MPL, indicate your decision by
    * deleting the provisions above and replace them with the notice and other
    * provisions required by the LGPL or the GPL. If you do not delete the
    * provisions above, a recipient may use your version of this file under the
    * terms of any one of the MPL, the GPL or the LGPL.
    *
    * ***** END LICENSE BLOCK *****}
    
unit indcyBaseLed;

{$mode objfpc}{$H+}

interface

uses LCLIntf, LCLType, LMessages, Messages, Classes, Types, Controls, Graphics;

type
  TLedStatus = (lsOn, lsOff, lsDisabled);

  TcyBaseLed = class(TGraphicControl)
  private
    FGroupIndex: Integer;
    FAllowAllOff: Boolean;
    FLedValue: Boolean;
    FReadOnly: Boolean;
    procedure SetAllowAllOff(Value: Boolean);
    procedure SetGroupIndex(Value: Integer);
    procedure UpdateExclusive;
  protected
    procedure Click; override;
    procedure Loaded; override;
    procedure SetEnabled(Value: Boolean); override;
    procedure CMButtonPressed(var Message: TLMessage); message CM_BUTTONPRESSED;  // Called in UpdateExclusive procedure ...
    function  TransparentColorAtPos(Point: TPoint): boolean; virtual;
    procedure LedStatusChanged; virtual;
    procedure SetInternalLedValue(Value: Boolean);
    function  GetLedStatus: TLedStatus; virtual;
    procedure SetLedvalue(Value: Boolean); virtual;
    procedure SetReadOnly(AValue: Boolean); virtual;
    property AllowAllOff: Boolean read FAllowAllOff write SetAllowAllOff default false;
    property GroupIndex: Integer read FGroupIndex write SetGroupIndex default 0;
    property LedValue: Boolean read FLedvalue write SetLedvalue;
    property ReadOnly: Boolean read FReadOnly write SetReadOnly default false;
  public
    property Canvas;
    constructor Create(AOwner: TComponent); override;
    property LedStatus: TLedStatus read GetLedStatus;
    procedure Switch;
  published
  end;

implementation

constructor TcyBaseLed.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FAllowAllOff := false;
  FGroupIndex := 0;
  FLedvalue:= false;
  FReadOnly := false;
end;

procedure TcyBaseLed.LedStatusChanged;
begin
  Invalidate;
end;

procedure TcyBaseLed.Loaded;
begin
  Inherited;
  ControlStyle := ControlStyle - [csDoubleClicks];
end;

procedure TcyBaseLed.SetReadOnly(AValue: Boolean);
begin
  if AValue <> FReadOnly
  then FReadOnly := AValue;
end;

procedure TcyBaseLed.SetEnabled(Value: Boolean);
begin
  Inherited;
  LedStatusChanged;
end;

function TcyBaseLed.TransparentColorAtPos(Point: TPoint): boolean;
begin
  RESULT := false;
end;

procedure TcyBaseLed.Click;
var aPt: TPoint;
begin
  if not FReadOnly
  then begin
    GetCursorPos(aPt);
    aPt := Self.ScreenToClient(aPt);
    
    if Not TransparentColorAtPos(aPt)
    then LedValue := not FLedValue;
  end;

  Inherited;
end;

function TcyBaseLed.GetLedStatus: TLedStatus;
begin
  if not Enabled
  then
    RESULT := lsDisabled
  else
    if FLedValue
    then RESULT := lsOn
    else RESULT := lsOff;
end;

// Procedure to force changing value :
procedure TcyBaseLed.SetInternalLedValue(Value: Boolean);
begin
  if FLedValue <> Value
  then begin
    FLedValue := Value;
    LedStatusChanged;
  end;
end;

procedure TcyBaseLed.Switch;
begin
  LedValue := not FLedValue;
end;

procedure TcyBaseLed.SetLedvalue(Value: Boolean);
begin
  if Value <> FLedvalue
  then begin
    if (not Value) and (not FAllowAllOff) and (FGroupIndex <> 0)
    then Exit;              // Can't turn off all leds of the same group ...

    FLedvalue := Value;
    LedStatusChanged;

    if Value
    then UpdateExclusive;   // Send message to turn off the other one ...
  end;
end;

procedure TcyBaseLed.SetAllowAllOff(Value: Boolean);
begin
  if FAllowAllOff <> Value
  then begin
    FAllowAllOff := Value;
    UpdateExclusive;        // Inform FAllowAllOff value to the others from the same group 
  end;
end;

procedure TcyBaseLed.SetGroupIndex(Value: Integer);
begin
  if FGroupIndex <> Value
  then begin
    FGroupIndex := Value;
    UpdateExclusive;
  end;
end;

procedure TcyBaseLed.UpdateExclusive;
var
  Msg: TMessage;
begin
  if (FGroupIndex <> 0) and (Parent <> nil)
  then begin
    Msg.Msg := CM_BUTTONPRESSED;
    Msg.WParam := FGroupIndex;
    Msg.LParam := PtrInt(Self);
    Msg.Result := 0;
    Parent.Broadcast(Msg);
  end;
end;

procedure TcyBaseLed.CMButtonPressed(var Message: TLMessage);
var Sender: TcyBaseLed;
begin
 if (csLoading in ComponentState) then exit;

  if Message.WParam = FGroupIndex        // Same group?
  then begin
    Sender := TcyBaseLed(Message.LParam);
    if Sender <> Self
    then begin
      if Sender.LedValue and FLedValue   // Only one can be turn on on group mode ...
      then begin;
        FLedValue := false;
        LedStatusChanged;
      end;

      FAllowAllOff := Sender.AllowAllOff;
    end;
  end;
end;

end.
