{ $Id$}
{
 *****************************************************************************
 *                                WSGrids.pp                                 * 
 *                                ----------                                 * 
 *                                                                           *
 *                                                                           *
 *****************************************************************************

 *****************************************************************************
  This file is part of the Lazarus Component Library (LCL)

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************
}
unit WSGrids;

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
  LCLType, Types, Controls, StdCtrls, Grids, LazUTF8, Graphics,
////////////////////////////////////////////////////
  WSLCLClasses, WSControls, WSFactory;

type
  { TWSCustomGrid }

  TWSCustomGrid = class(TWSCustomControl)
  published
    class procedure SendCharToEditor(AEditor:TWinControl; Ch: TUTF8Char); virtual;
    class function InvalidateStartY(const FixedHeight, RowOffset: Integer): integer; virtual;
    class procedure Invalidate(sender: TCustomGrid); virtual;
    class function GetEditorBoundsFromCellRect(ACanvas: TCanvas;
      const ACellRect: TRect; const AColumnLayout: TTextLayout): TRect; virtual;
  end;
  TWSCustomGridClass = class of TWSCustomgrid;

  { WidgetSetRegistration }

  function RegisterCustomGrid: Boolean;

implementation
uses
  LCLIntf;

type
  TCustomGridAccess=class(TCustomGrid)
  end;

{ TWSCustomGrid }

class procedure TWSCustomGrid.SendCharToEditor(AEditor:TWinControl;
  Ch: TUTF8Char);
var
  GMsg: TGridMessage;
  GridEditor: boolean;
begin
  GMsg.Grid := nil;
  GMsg.Options:= 0;
  GMsg.LclMsg.Msg:=GM_GETGRID;
  AEditor.Dispatch(GMsg);
  GridEditor := (GMsg.Options and EO_IMPLEMENTED<>0) and (GMsg.Grid<>nil);

  GMsg.LclMsg.Msg:=GM_SETVALUE;
  if Ch=#8 then // backspace
    GMsg.Value:=''
  else
    GMsg.Value:=Ch;

  if GridEditor then
    AEditor.Dispatch(GMsg)
  else begin
    // TODO: Find a generic way ...
    if AEditor is TCustomEdit then begin
      TCustomEdit(AEditor).Text:=GMsg.Value;
      TCustomEdit(AEditor).SelStart:=UTF8Length(GMsg.Value);
    end else
    if AEditor is TCustomCombobox then begin
      TCustomCombobox(AEditor).Text:=GMsg.Value;
      TCustomCombobox(AEditor).SelStart:=UTF8Length(GMsg.Value);
    end;
  end;

  // make sure the grid is notified that some text is changed, some
  // widgets do not notify when they are modified programmatically.
  if GMsg.Grid<>nil then
    with TCustomGridAccess(GMsg.Grid) do
      EditorTextChanged(Col, Row, GMsg.Value);
end;

class function TWSCustomGrid.GetEditorBoundsFromCellRect(ACanvas: TCanvas;
  const ACellRect: TRect; const AColumnLayout: TTextLayout): TRect;
begin
  Result := ACellRect;
  Dec(Result.Right);
  Dec(Result.Bottom);
end;

class function TWSCustomGrid.InvalidateStartY(const FixedHeight,
  RowOffset: Integer): integer;
begin
  result := FixedHeight;
end;

class procedure TWSCustomGrid.Invalidate(sender: TCustomGrid);
begin
  // override in widgetset level if needed
end;

{ WidgetSetRegistration }

function RegisterCustomGrid: Boolean;
const
  Done: Boolean = False;
begin
  Result := False;
  if Done then exit;
  if not WSRegisterCustomGrid then
    RegisterWSComponent(TCustomGrid, TWSCustomGrid);
  Done := True;
  Result := True;
end;

end.
