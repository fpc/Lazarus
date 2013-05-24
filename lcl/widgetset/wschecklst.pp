{
 *****************************************************************************
 *                               WSCheckLst.pp                               * 
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
unit WSCheckLst;

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
////////////////////////////////////////////////////
  StdCtrls, CheckLst,
////////////////////////////////////////////////////
  WSLCLClasses, WSStdCtrls, Classes, WSFactory;

type
  { TWSCustomCheckListBox }

  TWSCustomCheckListBox = class(TWSCustomListBox)
  published
    class function GetCheckWidth(const ACheckListBox: TCustomCheckListBox):
      integer; virtual;
    class function GetItemEnabled(const ACheckListBox: TCustomCheckListBox;
      const AIndex: integer): Boolean; virtual;
    class function GetHeader(const ACheckListBox: TCustomCheckListBox;
      const AIndex: integer): Boolean; virtual;
    class function GetState(const ACheckListBox: TCustomCheckListBox;
      const AIndex: integer): TCheckBoxState; virtual;
    class procedure SetItemEnabled(const ACheckListBox: TCustomCheckListBox;
      const AIndex: integer; const AEnabled: Boolean); virtual;
    class procedure SetHeader(const ACheckListBox: TCustomCheckListBox;
      const AIndex: integer; const AHeader: Boolean); virtual;
    class procedure SetState(const ACheckListBox: TCustomCheckListBox;
      const AIndex: integer; const AState: TCheckBoxState); virtual;
  end;
  TWSCustomCheckListBoxClass = class of TWSCustomCheckListBox;

  { WidgetSetRegistration }

  procedure RegisterCustomCheckListBox;

implementation

class function TWSCustomCheckListBox.GetCheckWidth(
  const ACheckListBox: TCustomCheckListBox): Integer;
begin
  Result := 0;
end;

class function TWSCustomCheckListBox.GetHeader(
  const ACheckListBox: TCustomCheckListBox; const AIndex: integer): Boolean;
begin
  Result := False;
end;

class function TWSCustomCheckListBox.GetItemEnabled(
  const ACheckListBox: TCustomCheckListBox; const AIndex: integer): Boolean;
begin
  Result := True;
end;

class function TWSCustomCheckListBox.GetState(
  const ACheckListBox: TCustomCheckListBox; const AIndex: integer
  ): TCheckBoxState;
begin
  Result := cbUnchecked;
end;

class procedure TWSCustomCheckListBox.SetHeader(
  const ACheckListBox: TCustomCheckListBox; const AIndex: integer;
  const AHeader: Boolean);
begin
end;

class procedure TWSCustomCheckListBox.SetItemEnabled(
  const ACheckListBox: TCustomCheckListBox; const AIndex: integer;
  const AEnabled: Boolean);
begin
end;

class procedure TWSCustomCheckListBox.SetState(
  const ACheckListBox: TCustomCheckListBox; const AIndex: integer;
  const AState: TCheckBoxState);
begin
end;

{ WidgetSetRegistration }

procedure RegisterCustomCheckListBox;
const
  Done: Boolean = False;
begin
  if Done then exit;
  WSRegisterCustomCheckListBox;
//  if not WSRegisterCustomCheckListBox then
//    RegisterWSComponent(TCustomCheckListBox, TWSCustomCheckListBox);
  Done := True;
end;

end.
