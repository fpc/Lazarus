{
 *****************************************************************************
 *                              CarbonWSCheckLst.pp                          * 
 *                              ---------------                              * 
 *                                                                           *
 *                                                                           *
 *****************************************************************************

 *****************************************************************************
  This file is part of the Lazarus Component Library (LCL)

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************
}
unit cocoawschecklst;

{$mode objfpc}{$H+}
{$modeswitch objectivec1}
{$modeswitch objectivec2}

interface

uses
  // Libs
  MacOSAll, CocoaAll, Classes, sysutils,
  // LCL
  Controls, StdCtrls, CheckLst, LCLType,
  // Widgetset
  WSCheckLst, WSLCLClasses,
  // LCL Cocoa
  CocoaWSCommon, CocoaPrivate, CocoaUtils, CocoaWSStdCtrls, CocoaTables,
  CocoaScrollers;

type

  { TCocoaWSCustomCheckListBox }

  TCocoaWSCustomCheckListBox = class(TWSCustomCheckListBox)
  published
    class function  CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): TLCLIntfHandle; override;
    class function  GetState(const ACheckListBox: TCustomCheckListBox; const AIndex: integer): TCheckBoxState; override;
    class procedure SetState(const ACheckListBox: TCustomCheckListBox; const AIndex: integer; const AState: TCheckBoxState); override;
  end;

implementation

{ TCocoaWSCustomCheckListBox }

{------------------------------------------------------------------------------
  Method:  TCocoaWSCustomCheckListBox.CreateHandle
  Params:  AWinControl - LCL control
           AParams     - Creation parameters
  Returns: Handle to the control in Carbon interface

  Creates new check list box in Carbon interface with the specified parameters
 ------------------------------------------------------------------------------}
class function TCocoaWSCustomCheckListBox.CreateHandle(
  const AWinControl: TWinControl; const AParams: TCreateParams): TLCLIntfHandle;
var
  list: TCocoaCheckListBox;
  scroll: TCocoaScrollView;
begin
  list := NSView(TCocoaCheckListBox.alloc).lclInitWithCreateParams(AParams);
  if not Assigned(list) then
  begin
    Result := 0;
    Exit;
  end;
  list.callback := TLCLListBoxCallback.Create(list, AWinControl);
  list.list := TCocoaStringList.Create(list);
  list.setHeaderView(nil);
  list.setDataSource(list);
  list.setDelegate(list);
  list.AllowMixedState := TCustomCheckListBox(AWinControl).AllowGrayed;
  ListBoxSetStyle(list, TCustomCheckListBox(AWinControl).Style);

  scroll := EmbedInScrollView(list);
  if not Assigned(scroll) then
  begin
    list.dealloc;
    Result := 0;
    Exit;
  end;
  scroll.callback := list.callback;
  scroll.setHasVerticalScroller(true);
  scroll.setAutohidesScrollers(true);
  Result := TLCLIntfHandle(scroll);
end;

{------------------------------------------------------------------------------
  Method:  TCocoaWSCustomCheckListBox.GetState
  Params:  ACustomCheckListBox - LCL custom check list box
           AIndex              - Item index
  Returns: If the specified item in check list box in Cocoa interface is
           checked, grayed or unchecked
 ------------------------------------------------------------------------------}
class function TCocoaWSCustomCheckListBox.GetState(
  const ACheckListBox: TCustomCheckListBox; const AIndex: integer): TCheckBoxState;
var
  lListBox: TCocoaCheckListBox;
begin
  Result := cbUnchecked;
  lListBox := TCocoaCheckListBox(GetListBox(ACheckListBox));
  if lListBox = nil then Exit;

  Result := lListBox.GetState(AIndex);
end;

{------------------------------------------------------------------------------
  Method:  TCocoaWSCustomCheckListBox.SetState
  Params:  ACustomCheckListBox - LCL custom check list box
           AIndex              - Item index to change checked value
           AChecked            - New checked value

  Changes checked value of item with the specified index of check list box in
  Cocoa interface
 ------------------------------------------------------------------------------}
class procedure TCocoaWSCustomCheckListBox.SetState(
  const ACheckListBox: TCustomCheckListBox; const AIndex: integer;
  const AState: TCheckBoxState);
var
  lListBox: TCocoaCheckListBox;
begin
  lListBox := TCocoaCheckListBox(GetListBox(ACheckListBox));
  if lListBox = nil then Exit;

  lListBox.SetState(AIndex, AState);
end;

end.
