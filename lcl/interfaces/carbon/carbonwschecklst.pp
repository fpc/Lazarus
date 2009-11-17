{ $Id$}
{
 *****************************************************************************
 *                              CarbonWSCheckLst.pp                          * 
 *                              ---------------                              * 
 *                                                                           *
 *                                                                           *
 *****************************************************************************

 *****************************************************************************
 *                                                                           *
 *  This file is part of the Lazarus Component Library (LCL)                 *
 *                                                                           *
 *  See the file COPYING.modifiedLGPL.txt, included in this distribution,    *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************
}
unit CarbonWSCheckLst;

{$mode objfpc}{$H+}

interface

// defines
{$I carbondefines.inc}

uses
  //FCL+LCL
  Classes, Controls, StdCtrls, CheckLst, LCLType,
  //widgetset
  WSCheckLst, WSLCLClasses,
  //LCL Carbon
  CarbonDef, CarbonListViews, CarbonStrings;

type

  { TCarbonWSCustomCheckListBox }

  TCarbonWSCustomCheckListBox = class(TWSCustomCheckListBox)
  published
    class function  CreateHandle(const AWinControl: TWinControl;
                    const AParams: TCreateParams): TLCLIntfHandle; override;
    class function  GetState(const ACheckListBox: TCustomCheckListBox;
      const AIndex: integer): TCheckBoxState; override;
    class procedure SetState(const ACheckListBox: TCustomCheckListBox;
      const AIndex: integer; const AState: TCheckBoxState); override;
  end;


implementation

{ TCarbonWSCustomCheckListBox }

{------------------------------------------------------------------------------
  Method:  TCarbonWSCustomCheckListBox.CreateHandle
  Params:  AWinControl - LCL control
           AParams     - Creation parameters
  Returns: Handle to the control in Carbon interface

  Creates new check list box in Carbon interface with the specified parameters
 ------------------------------------------------------------------------------}
class function TCarbonWSCustomCheckListBox.CreateHandle(
  const AWinControl: TWinControl; const AParams: TCreateParams
  ): TLCLIntfHandle;
begin
  Result := TLCLIntfHandle(TCarbonCheckListBox.Create(AWinControl, AParams));
end;

{------------------------------------------------------------------------------
  Method:  TCarbonWSCustomCheckListBox.GetState
  Params:  ACustomCheckListBox - LCL custom check list box
           AIndex              - Item index
  Returns: If the specified item in check list box in Carbon interface is
           checked, grayed or unchecked
 ------------------------------------------------------------------------------}
class function TCarbonWSCustomCheckListBox.GetState(
  const ACheckListBox: TCustomCheckListBox; const AIndex: integer
  ): TCheckBoxState;
begin
  Result := cbUnchecked;
  if not CheckHandle(ACheckListBox, Self, 'GetState') then Exit;

  // TODO: grayed state
  if TCarbonCheckListBox(ACheckListBox.Handle).GetItemChecked(AIndex) then
    Result := cbChecked
  else
    Result := cbUnchecked;
end;

{------------------------------------------------------------------------------
  Method:  TCarbonWSCustomCheckListBox.SetState
  Params:  ACustomCheckListBox - LCL custom check list box
           AIndex              - Item index to change checked value
           AChecked            - New checked value

  Changes checked value of item with the specified index of check list box in
  Carbon interface
 ------------------------------------------------------------------------------}
class procedure TCarbonWSCustomCheckListBox.SetState(
  const ACheckListBox: TCustomCheckListBox; const AIndex: integer;
  const AState: TCheckBoxState);
begin
  if not CheckHandle(ACheckListBox, Self, 'SetState') then Exit;
  
  // TODO: grayed state
  TCarbonCheckListBox(ACheckListBox.Handle).SetItemChecked(AIndex, AState <> cbUnchecked);
end;

end.
