{ $Id$}
{
 *****************************************************************************
 *                            Win32WSControls.pp                             * 
 *                            ------------------                             * 
 *                                                                           *
 *                                                                           *
 *****************************************************************************

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
unit Win32WSControls;

{$mode objfpc}{$H+}

interface

uses
////////////////////////////////////////////////////
// I M P O R T A N T                                
////////////////////////////////////////////////////
// To get as little as posible circles,
// uncomment only when needed for registration
////////////////////////////////////////////////////
  Controls,
////////////////////////////////////////////////////
  WSControls, WSLCLClasses, SysUtils,
  { TODO: needs to move }
  Buttons, StdCtrls, ExtCtrls, GraphMath, GraphType, InterfaceBase, LCLIntf, LCLType;

type

  { TWin32WSDragImageList }

  TWin32WSDragImageList = class(TWSDragImageList)
  private
  protected
  public
  end;

  { TWin32WSControl }

  TWin32WSControl = class(TWSControl)
  private
  protected
  public
    class procedure SetCursor(const AControl: TControl; const ACursor: TCursor); override;
  end;

  { TWin32WSWinControl }

  TWin32WSWinControl = class(TWSWinControl)
  private
  protected
  public
    class procedure SetBorderStyle(const AWinControl: TWinControl; const ABorderStyle: TBorderStyle); override;
    class procedure SetColor(const AWinControl: TWinControl); override;
    class procedure SetText(const AWinControl: TWinControl; const AText: string); override;

    class procedure ShowHide(const AWinControl: TWinControl); override;
  end;

  { TWin32WSGraphicControl }

  TWin32WSGraphicControl = class(TWSGraphicControl)
  private
  protected
  public
  end;

  { TWin32WSCustomControl }

  TWin32WSCustomControl = class(TWSCustomControl)
  private
  protected
  public
  end;

  { TWin32WSImageList }

  TWin32WSImageList = class(TWSImageList)
  private
  protected
  public
  end;


implementation

uses
  Windows, Win32Int, Win32WSButtons;

procedure TWin32WSControl.SetCursor(const AControl: TControl; const ACursor: TCursor);
begin
  Windows.SetCursor(Windows.LoadCursor(0, LclCursorToWin32CursorMap[ACursor]));
end;

procedure TWin32WSWinControl.SetBorderStyle(const AWinControl: TWinControl; const ABorderStyle: TBorderStyle);
begin
  TWin32WidgetSet(InterfaceObject).RecreateWnd(AWinControl);
end;

procedure TWin32WSWinControl.SetColor(const AWinControl: TWinControl);
begin
  // TODO: to be implemented, had no implementation in LM_SETCOLOR message
end;

procedure TWin32WSWinControl.SetText(const AWinControl: TWinControl; const AText: string);

  procedure SetPageCaption(const Page:TCustomPage);
  var
    TCI: TC_ITEM;
    PageIndex: integer;
    NotebookHandle: HWND;
  begin
    Assert(False, 'Trace: TWin32WidgetSet.SetLabel - Got csPage');
    PageIndex := Page.PageIndex;
    NotebookHandle := Page.Parent.Handle;
    // We can't set label of a page not yet added,
    // Check for valid page index
    if (PageIndex>=0) and
      (PageIndex < Windows.SendMessage(NotebookHandle, TCM_GETITEMCOUNT,0,0)) then
    begin
      // retrieve page handle from tab as extra check (in case page isn't added yet).
      TCI.mask := TCIF_PARAM;
      Windows.SendMessage(NotebookHandle, TCM_GETITEM, PageIndex, LPARAM(@TCI));
      if dword(TCI.lParam)=Page.Handle then
      begin
        Assert(False, Format('Trace:TWin32WidgetSet.SetLabel - label --> %S', [AText]));
        TCI.mask := TCIF_TEXT;
        TCI.pszText := PChar(AText);
        Windows.SendMessage(NotebookHandle, TCM_SETITEM, PageIndex, LPARAM(@TCI));
      end;
    end;
  end;

Var
  Handle: HWnd;
{  TCI: TC_ITEM; }
  TempText: string;
Const
  TermChar: PChar = #0#0;
Begin
  Handle := AWinControl.Handle;
  Assert(Handle<>0,'Trace:WARNING: [TWin32WidgetSet.SetLabel] --> Got NULL handle');
  Assert(False, 'Trace:Setting the label in TWin32WidgetSet.SetLabel');

  Case AWinControl.FCompStyle Of
    csBitBtn:
      DrawBitBtnImage(TCustomBitBtn(AWinControl), PChar(AText));
      
      
{ TODO: CHECK !! Code was never reached in SetLabel ? }
{
    csFileDialog, csOpenFileDialog, csSaveFileDialog, csSelectDirectoryDialog,
    csColorDialog, csFontDialog:
    Begin
      Assert(False, Format('Trace:TWin32WidgetSet.SetLabel - Got %S', [CS_To_String(AWinControl.FCompStyle)]));
      Assert(False, 'Trace:TWin32WidgetSet.SetLabel - I''m not sure if this''ll work');
      Assert(False, Format('Trace:Is Sender a TCommonDialog - %S', [BOOL_RESULT[AWinControl Is TCommonDialog]]));
      If AWinControl Is TCommonDialog Then
        TCommonDialog(AWinControl).Title := AText 
      Else
        AWinControl.Caption := AText;
      Assert(False, Format('Trace:TWin32WidgetSet.SetLabel - Leaving %S', [CS_To_String(AWinControl.FCompStyle)]));
    End;
}
  
    csComboBox:
    Begin
      Assert(False, Format('Trace:TWin32WidgetSet.SetLabel - Got %S', [CS_To_String(AWinControl.FCompStyle)]));
      Assert(False, Format('Trace:TWin32WidgetSet.SetLabel - label --> %S', [AText]));
      if TCustomComboBox(AWinControl).Style = csDropDownList then
        Windows.SendMessage(Handle, CB_SELECTSTRING, -1, LPARAM(PChar(AText)))
      else
        Windows.SendMessage(Handle, WM_SETTEXT, 0, LPARAM(PChar(AText)));
    End;
    csMemo:
    Begin
      SendMessage(Handle, WM_SETTEXT, 0, LPARAM(PChar(AText)));
    End;
  {
    csNotebook:
    Begin
      Assert(False, 'Trace: TWin32WidgetSet.SetLabel - Got csNotebook');
      with TLMNotebookEvent(Data^) do
      if Parent=Sender then
      begin
        TCI.mask := TCIF_TEXT;
        Assert(False, Format('Trace:TWin32WidgetSet.SetLabel - label --> %S', [Str]));
        TCI.pszText := PChar(Str);
        Windows.SendMessage(TCustomNotebook(Sender).Handle, TCM_SETITEM, Page, LPARAM(@TCI));
      end
    End;
  }
    csPage:
      SetPageCaption(TCustomPage(AWinControl));
    csToolButton:
    Begin
      TempText := AText + TermChar;
      SendMessage(AWinControl.Parent.Handle, TB_ADDSTRING, 0, LPARAM(PChar(TempText)));
    End;
  Else
    Windows.SetWindowText(Handle, PChar(AText));
  End;
  Assert(False, Format('Trace:[TWin32WidgetSet.SetLabel] %S --> END', [AWinControl.ClassName]));
End;


procedure TWin32WSWinControl.ShowHide(const AWinControl: TWinControl);
begin
  // other methods also use ShowHide, can't move code
  TWin32WidgetSet(InterfaceObject).ShowHide(AWinControl);
end;

initialization

////////////////////////////////////////////////////
// I M P O R T A N T
////////////////////////////////////////////////////
// To improve speed, register only classes
// which actually implement something
////////////////////////////////////////////////////
//  RegisterWSComponent(TDragImageList, TWin32WSDragImageList);
  RegisterWSComponent(TControl, TWin32WSControl);
  RegisterWSComponent(TWinControl, TWin32WSWinControl);
//  RegisterWSComponent(TGraphicControl, TWin32WSGraphicControl);
//  RegisterWSComponent(TCustomControl, TWin32WSCustomControl);
//  RegisterWSComponent(TImageList, TWin32WSImageList);
////////////////////////////////////////////////////
end.
