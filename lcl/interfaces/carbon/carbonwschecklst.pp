{ $Id$}
{
 *****************************************************************************
 *                              CarbonWSCheckLst.pp                              * 
 *                              ---------------                              * 
 *                                                                           *
 *                                                                           *
 *****************************************************************************

 *****************************************************************************
 *                                                                           *
 *  This file is part of the Lazarus Component Library (LCL)                 *
 *                                                                           *
 *  See the file COPYING.modifiedLGPL, included in this distribution,        *
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

// debugging defines
{$I carbondebug.inc}

uses
  //FCL+LCL
  Classes, Controls, StdCtrls, CheckLst, LCLType,
  //widgetset
  WSCheckLst, WSLCLClasses,
  //LCL Carbon
  CarbonDef, CarbonLists, CarbonStrings;

type

  { TCarbonWSCustomCheckListBox }

  TCarbonWSCustomCheckListBox = class(TWSCustomCheckListBox)
  private
  protected
  public
    class function  CreateHandle(const AWinControl: TWinControl;
                    const AParams: TCreateParams): TLCLIntfHandle; override;
    class function  GetStrings(const ACustomListBox: TCustomListBox): TStrings; override;
    class function  GetChecked(const ACheckListBox: TCustomCheckListBox;
      const AIndex: integer): boolean; override;
    class procedure SetChecked(const ACheckListBox: TCustomCheckListBox;
      const AIndex: integer; const AChecked: boolean); override;
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
  Method:  TCarbonWSCustomCheckListBox.GetStrings
  Params:  ACustomListBox - LCL custom list box
  Returns: Items of check list box in Carbon interface
 ------------------------------------------------------------------------------}
class function TCarbonWSCustomCheckListBox.GetStrings(
  const ACustomListBox: TCustomListBox): TStrings;
begin
  Result := nil;
  if not CheckHandle(ACustomListBox, Self, 'GetStrings') then Exit;

  Result := TCarbonCheckListBoxStrings.Create(TCarbonCheckListBox(ACustomListBox.Handle));
end;

{------------------------------------------------------------------------------
  Method:  TCarbonWSCustomCheckListBox.GetChecked
  Params:  ACustomCheckListBox - LCL custom check list box
           AIndex              - Item index
  Returns: If the specified item in check list box in Carbon interface is
           checked
 ------------------------------------------------------------------------------}
class function TCarbonWSCustomCheckListBox.GetChecked(
  const ACheckListBox: TCustomCheckListBox; const AIndex: integer): boolean;
begin
  Result := false;
  if not CheckHandle(ACheckListBox, Self, 'GetChecked') then Exit;

  Result:=TCarbonCheckListBox(ACheckListBox.Handle).GetChecked(AIndex);
end;

{------------------------------------------------------------------------------
  Method:  TCarbonWSCustomCheckListBox.SetChecked
  Params:  ACustomCheckListBox - LCL custom check list box
           AIndex              - Item index to change checked value
           AChecked            - New checked value

  Changes checked value of item with the specified index of check list box in
  Carbon interface
 ------------------------------------------------------------------------------}
class procedure TCarbonWSCustomCheckListBox.SetChecked(
  const ACheckListBox: TCustomCheckListBox; const AIndex: integer;
  const AChecked: boolean);
begin
  if not CheckHandle(ACheckListBox, Self, 'SetChecked') then Exit;
  
  TCarbonCheckListBox(ACheckListBox.Handle).SetChecked(AIndex,AChecked);
end;

initialization

////////////////////////////////////////////////////
// I M P O R T A N T
////////////////////////////////////////////////////
// To improve speed, register only classes
// which actually implement something
////////////////////////////////////////////////////
  RegisterWSComponent(TCustomCheckListBox, TCarbonWSCustomCheckListBox);
////////////////////////////////////////////////////
end.
