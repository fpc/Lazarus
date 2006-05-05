{ $Id$}
{
 *****************************************************************************
 *                            Win32WSCListBox.pp                             * 
 *                            ------------------                             * 
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
unit Win32WSCListBox;

{$mode objfpc}{$H+}

interface

uses
////////////////////////////////////////////////////
// I M P O R T A N T                                
////////////////////////////////////////////////////
// To get as little as posible circles,
// uncomment only when needed for registration
////////////////////////////////////////////////////
//  CListBox,
////////////////////////////////////////////////////
  WSCListBox, WSLCLClasses;

type

  { TWin32WSCListBox }

  TWin32WSCListBox = class(TWSCListBox)
  private
  protected
  public
    class function  CreateHandle(const AWinControl: TWinControl;
          const AParams: TCreateParams): HWND; override;
    class function  GetStrings(const ACustomListBox: TCustomListBox): TStrings; override;
    class procedure SetSorted(const ACustomListBox: TCustomListBox; AList: TStrings; ASorted: boolean); override;
  end;


implementation

{ TWin32WSCListBox }

function TWin32WSCListBox.CreateHandle(const AWinControl: TWinControl;
  const AParams: TCreateParams): HWND;
var
  Params: TCreateWindowExParams;
begin
  // general initialization of Params
  PrepareCreateWindow(AWinControl, Params);
  // customization of Params
  with Params do
  begin
    with TCustomListBox(AWinControl) do
    begin
      if Sorted then
        Flags := Flags or LBS_SORT;
      if MultiSelect then
        if ExtendedSelect then
          Flags := Flags or LBS_EXTENDEDSEL
        else
          Flags := Flags or LBS_MULTIPLESEL;
    end;
    FlagsEx := FlagsEx or WS_EX_CLIENTEDGE;
    pClassName := 'LISTBOX';
    Flags := Flags or LBS_MULTICOLUMN or WS_HSCROLL;
  end;
  // create window
  FinishCreateWindow(AWinControl, Params, false);
  Windows.SendMessage(Params.Window, LB_SETCOLUMNWIDTH, Windows.WPARAM(
    TCListBox(AWinControl).Width div (TCListBox(AWinControl).ListColumns)), 0);
  Result := Params.Window;
end;

function  TWin32WSCustomCListBox.GetStrings(const ACustomListBox: TCustomListBox): TStrings;
var
  Handle: HWND;
begin
  Handle := ACustomListBox.Handle;
  Result := TWin32CListStringList.Create(Handle, ACustomListBox)
  GetWindowInfo(Handle)^.List := Result;
end;

procedure TWin32WSCustomCListBox.SetSorted(const ACustomListBox: TCustomListBox; AList: TStrings; ASorted: boolean);
begin
  TWin32CListStringList(AList).Sorted := ASorted;
end;

initialization

////////////////////////////////////////////////////
// I M P O R T A N T
////////////////////////////////////////////////////
// To improve speed, register only classes
// which actually implement something
////////////////////////////////////////////////////
  RegisterWSComponent(TCListBox, TWin32WSCListBox);
////////////////////////////////////////////////////
end.