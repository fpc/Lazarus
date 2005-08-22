{%MainUnit win32int.pp}
{ $Id$ }
{******************************************************************************
                 All GTK interface communication implementations.
                   Initial Revision  : Sun Nov 23 23:53:53 2003


  !! Keep alphabetical !!

  Support routines go to gtkproc.pp

 ******************************************************************************
 Implementation
 ******************************************************************************

 *****************************************************************************
 *                                                                           *
 *  This file is part of the Lazarus Component Library (LCL)                 *
 *                                                                           *
 *  See the file COPYING.LCL, included in this distribution,                 *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************
}

//##apiwiz##sps##   // Do not remove

procedure TWin32WidgetSet.DrawArrow(Arrow: TComponent; Canvas: TPersistent);
const
    { up, down, left, right }
  ArrowTypeToState: array[TArrowType] of dword = (DFCS_SCROLLUP, DFCS_SCROLLDOWN,
    DFCS_SCROLLLEFT, DFCS_SCROLLRIGHT);
var
  drawRect: Windows.RECT;
  canvasHandle: HDC;
begin
  drawRect := TControl(Arrow).ClientRect;
  canvasHandle := TCanvas(Canvas).Handle;
  Windows.FillRect(canvasHandle, drawRect, GetSysColorBrush(COLOR_BTNFACE));
  dec(drawRect.Left, 2);
  dec(drawRect.Top, 2);
  inc(drawRect.Right, 2);
  inc(drawRect.Bottom, 2);
  Windows.DrawFrameControl(TCanvas(Canvas).Handle, drawRect,
	DFC_SCROLL, ArrowTypeToState[TArrow(Arrow).ArrowType]);
end;

{------------------------------------------------------------------------------
  Function: GetAcceleratorString
  Params: AVKey:
          AShiftState:
  Returns:

 ------------------------------------------------------------------------------}
function TWin32WidgetSet.GetAcceleratorString(const AVKey: Byte; const AShiftState: TShiftState): String;
begin
  //TODO: Implement
  Result := '';
end;

{------------------------------------------------------------------------------
  Function: GetControlConstraints
  Params: Constraints: TObject
  Returns: true on success

  Updates the constraints object (e.g. TSizeConstraints) with interface specific
  bounds.
 ------------------------------------------------------------------------------}
function TWin32WidgetSet.GetControlConstraints(Constraints: TObject): boolean;
var
  SizeConstraints: TSizeConstraints;
  SizeRect: TRect;
  Height, Width: Integer;
  FixedHeight, FixedWidth: boolean;
begin
  Result:=true;
  if Constraints is TSizeConstraints then begin
    SizeConstraints:=TSizeConstraints(Constraints);

    if (SizeConstraints.Control=nil) then exit;

    FixedHeight := false;
    FixedWidth := false;
    if SizeConstraints.Control is TCustomComboBox then
      FixedHeight := true;
    if SizeConstraints.Control is TCustomCalendar then
    begin
      FixedHeight := true;
      FixedWidth := true;
    end;
    
    if (FixedHeight or FixedWidth)
      and TWinControl(SizeConstraints.Control).HandleAllocated then 
    begin
      Windows.GetWindowRect(TWinControl(SizeConstraints.Control).Handle, @SizeRect);
      if FixedHeight then
        Height := SizeRect.Bottom - SizeRect.Top
      else
        Height := 0;
      if FixedWidth then
        Width := SizeRect.Right - SizeRect.Left
      else
        Width := 0;
      SizeConstraints.SetInterfaceConstraints(Width, Height, Width, Height);
    end;
  end;
end;

{------------------------------------------------------------------------------
  Function: GetListBoxIndexAtY
  Params: ListBox:
          y:
  Returns:

 ------------------------------------------------------------------------------}
function TWin32WidgetSet.GetListBoxIndexAtY(ListBox: TComponent; y: integer): integer;
begin
  Result := -1;
  if ListBox is TCustomListBox then begin
    Result := Windows.SendMessage(TCustomListBox(ListBox).Handle, LB_ITEMFROMPOINT, 0, MakeLParam(0,y));
    if hi(Result)=0 then
      Result := lo(Result)
    else Result := -1;
  end;
end;

//##apiwiz##eps##   // Do not remove, no wizard declaration after this line

{
  $Log$
  Revision 1.25  2005/07/01 12:07:50  mattias
  moved some menu intf methods to the TXXXWSMenuItem class  from Martin Smat

  Revision 1.24  2005/05/21 15:58:44  mattias
  implemented right justification for menuitems for winapi intf  from Martin Smat

  Revision 1.23  2005/05/05 20:53:37  micha
  hack: avoid drawing border of scrollbar arrow, by drawing it outside of clipped region

  Revision 1.22  2005/05/05 14:51:05  micha
  better looking arrow, not ideal, but better anyway (fixes 760)

  Revision 1.21  2005/02/23 01:12:47  marc
  + Added RemoveProp winapi call
  * Some maintenace on winapi/lclintf files

  Revision 1.20  2005/02/07 18:53:35  micha
  implement size constraints for calendar (it is not resizeable)

  Revision 1.19  2004/10/16 10:15:45  micha
  cleanup statusbar methods in lcl interfaceobject
  fix statusbar recursive call issue

  Revision 1.18  2004/04/15 08:03:07  micha
  fix radiogroup menuitem, uncheck others in same group (from jreyes)

  Revision 1.17  2004/03/19 00:53:34  marc
  * Removed all ComponentCreateHandle routines

  Revision 1.16  2004/03/05 01:04:21  marc
  * Renamed TWin32Object to TWin32WidgetSet

  Revision 1.15  2004/02/27 00:42:41  marc
  * Interface CreateComponent splitup
  * Implemented CreateButtonHandle on GTK interface
    on win32 interface it still needs to be done
  * Changed ApiWizz to support multilines and more interfaces

  Revision 1.14  2004/02/23 08:19:04  micha
  revert intf split

  Revision 1.12  2004/02/22 15:47:58  micha
  fp 1.0 compatiblity

  Revision 1.11  2004/02/20 19:52:18  micha
  fixed: tarrow crash in win32
  added: interface function DrawArrow to draw themed arrow

  Revision 1.10  2004/02/05 13:53:38  mattias
  fixed GetConstraints for win32 intf

  Revision 1.9  2004/02/02 16:56:43  micha
  implement GetControlConstraints for combobox

  Revision 1.8  2004/01/12 08:36:34  micha
  statusbar interface dependent reimplementation (from vincent)

  Revision 1.7  2004/01/11 16:38:29  marc
  * renamed (Check|Enable)MenuItem to MenuItemSet(Check|Enable)
  + Started with accelerator nameing routines
  * precheckin for createwidget splitup

  Revision 1.6  2004/01/09 20:03:13  mattias
  implemented new statusbar methods in gtk intf

  Revision 1.5  2004/01/03 11:57:48  mattias
  applied implementation for LM_LB_GETINDEXAT  from Vincent

  Revision 1.4  2003/12/29 14:22:22  micha
  fix a lot of range check errors win32

  Revision 1.3  2003/11/27 23:02:30  mattias
  removed menutype.pas

  Revision 1.2  2003/11/26 21:55:15  mattias
  fixed win32 TBaseMenuitem

  Revision 1.1  2003/11/26 00:23:47  marc
  * implemented new LCL(check|enable)Menuitem functions
  * introduced the lclintf inc files to win32


}
