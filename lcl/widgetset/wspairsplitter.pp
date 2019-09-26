{ $Id$}
{
 *****************************************************************************
 *                             WSPairSplitter.pp                             * 
 *                             -----------------                             * 
 *                                                                           *
 *                                                                           *
 *****************************************************************************

 *****************************************************************************
  This file is part of the Lazarus Component Library (LCL)

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************
}
unit WSPairSplitter;

{$mode objfpc}{$H+}
{$I lcl_defines.inc}

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
////////////////////////////////////////////////////
// To get as little as posible circles,
// uncomment only when needed for registration
  Controls, ExtCtrls, PairSplitter, WSLCLClasses, WSControls, WSFactory;

type
  { TWSPairSplitterSide }

  TWSPairSplitterSide = class(TWSWinControl)
  published
  end;

  { TWSCustomPairSplitter }

  TWSCustomPairSplitter = class(TWSWinControl)
  published
    class function AddSide(ASplitter: TCustomPairSplitter; ASide: TPairSplitterSide; Side: integer): Boolean; virtual;
    class function RemoveSide(ASplitter: TCustomPairSplitter; ASide: TPairSplitterSide; Side: integer): Boolean; virtual;
    class function GetPosition(ASplitter: TCustomPairSplitter): Integer; virtual;
    class function SetPosition(ASplitter: TCustomPairSplitter; var NewPosition: integer): Boolean; virtual;

    // special cursor handling
    class function GetSplitterCursor(ASplitter: TCustomPairSplitter; var ACursor: TCursor): Boolean; virtual;
    class function SetSplitterCursor(ASplitter: TCustomPairSplitter; ACursor: TCursor): Boolean; virtual;
  end;
  TWSCustomPairSplitterClass = class of TWSCustomPairSplitter;

  { WidgetSetRegistration }

  procedure RegisterPairSplitterSide;
  procedure RegisterCustomPairSplitter;

implementation
uses
  WSProc;
  
function GetInternalSplitter(ASplitter: TCustomPairSplitter): TSplitter;
var
  i: integer;
begin
  Result := nil;
  for i := 0 to ASplitter.ControlCount - 1 do
    if ASplitter.Controls[i] is TSplitter then
    begin
      Result := TSplitter(ASplitter.Controls[i]);
      break;
    end;
end;

{ TWSCustomPairSplitter }

class function TWSCustomPairSplitter.AddSide(ASplitter: TCustomPairSplitter;
  ASide: TPairSplitterSide; Side: integer): Boolean;
var
  InternalSplitter: TSplitter;
begin
  // this implementation can be common for all widgetsets and should be
  // overrided only if widgetset support such controls itself
  
  Result := False;
  if not (WSCheckHandleAllocated(ASplitter, 'AddSide - splitter') and
          WSCheckHandleAllocated(ASide, 'AddSide - side'))
  then Exit;

  if (Side < 0) or (Side > 1) then exit;

  if Side = 0 then
  begin
    if ASplitter.SplitterType = pstHorizontal then
      ASide.Align := alLeft
    else
      ASide.Align := alTop;
  end else
  begin
    InternalSplitter := GetInternalSplitter(ASplitter);
    if InternalSplitter = nil then
    begin
      InternalSplitter := TSplitter.Create(ASplitter);
      InternalSplitter.AutoSnap := False;
      InternalSplitter.MinSize := 1;
      InternalSplitter.Parent := ASplitter;
    end;
    InternalSplitter.Align := ASplitter.Sides[0].Align;
    if ASplitter.SplitterType = pstHorizontal then
      InternalSplitter.Left := ASplitter.Sides[0].Width + 1
    else
      InternalSplitter.Top := ASplitter.Sides[0].Height + 1;
    ASide.Align := alClient;
  end;

  Result := True;
end;

class function TWSCustomPairSplitter.RemoveSide(ASplitter: TCustomPairSplitter;
  ASide: TPairSplitterSide; Side: integer): Boolean;
begin
  Result := False;
end;

class function TWSCustomPairSplitter.GetPosition(ASplitter: TCustomPairSplitter): Integer;
begin
  if WSCheckHandleAllocated(ASplitter, 'GetPosition') then
  begin
    if ASplitter.SplitterType = pstHorizontal then
      Result := ASplitter.Sides[0].Width
    else
      Result := ASplitter.Sides[0].Height;
  end else
    Result := ASplitter.Position;
end;

class function TWSCustomPairSplitter.SetPosition(
  ASplitter: TCustomPairSplitter; var NewPosition: integer): Boolean;
var
  InternalSplitter: TSplitter;
begin
  Result := False;
  if not WSCheckHandleAllocated(ASplitter, 'SetPosition')
  then Exit;

  if NewPosition >= 0 then
  begin
    InternalSplitter := GetInternalSplitter(ASplitter);
    if ASplitter.SplitterType = pstHorizontal then
    begin
      ASplitter.Sides[0].Width := NewPosition;
      if InternalSplitter <> nil then
        InternalSplitter.Left := NewPosition + 1;
    end else
    begin
      ASplitter.Sides[0].Height := NewPosition;
      if InternalSplitter <> nil then
        InternalSplitter.Top := NewPosition + 1;
    end;
  end;
  if ASplitter.SplitterType = pstHorizontal then
    NewPosition := ASplitter.Sides[0].Width
  else
    NewPosition := ASplitter.Sides[0].Height;

  Result := True;
end;

class function TWSCustomPairSplitter.GetSplitterCursor(ASplitter: TCustomPairSplitter; var ACursor: TCursor): Boolean;
var
  InternalSplitter: TSplitter;
begin
  Result := True;
  InternalSplitter := GetInternalSplitter(ASplitter);
  if InternalSplitter <> nil then
    ACursor := InternalSplitter.Cursor
  else
    ACursor := crDefault;
end;

class function TWSCustomPairSplitter.SetSplitterCursor(ASplitter: TCustomPairSplitter; ACursor: TCursor): Boolean;
var
  InternalSplitter: TSplitter;
begin
  Result := True;
  InternalSplitter := GetInternalSplitter(ASplitter);
  if InternalSplitter <> nil then
  begin
    InternalSplitter.Cursor := ACursor;
    ASplitter.Sides[0].Cursor := crArrow;
    ASplitter.Sides[1].Cursor := crArrow;
  end;
end;

  { WidgetSetRegistration }

procedure RegisterPairSplitterSide;
const
  Done: Boolean = False;
begin
  if Done then exit;
  WSRegisterPairSplitterSide;
//  if not WSRegisterPairSplitterSide then
//    RegisterWSComponent(TPairSplitterSide, TWSPairSplitterSide);
  Done := True;
end;

procedure RegisterCustomPairSplitter;
const
  Done: Boolean = False;
begin
  if Done then exit;
  if not WSRegisterCustomPairSplitter then
    RegisterWSComponent(TCustomPairSplitter, TWSCustomPairSplitter);
  Done := True;
end;

end.
