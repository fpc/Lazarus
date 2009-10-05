{ $Id$}
{
 *****************************************************************************
 *                            Win32WSCheckLst.pp                             * 
 *                            ------------------                             * 
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
unit Win32WSCheckLst;

{$mode objfpc}{$H+}

interface

uses
////////////////////////////////////////////////////
// I M P O R T A N T                                
////////////////////////////////////////////////////
// To get as little as posible circles,
// uncomment only when needed for registration
////////////////////////////////////////////////////
  Windows, Classes, Controls, CheckLst, StdCtrls, LCLType, LMessages, LCLMessageGlue,
////////////////////////////////////////////////////
  WSCheckLst, WSLCLClasses, Win32Int, Win32Proc, Win32WSControls, Win32WSStdCtrls;

type

  { TWin32WSCustomCheckListBox }

  TWin32WSCustomCheckListBox = class(TWSCustomCheckListBox)
  published
    class function CreateHandle(const AWinControl: TWinControl;
       const AParams: TCreateParams): TLCLIntfHandle; override;
    class function GetStrings(const ACustomListBox: TCustomListBox): TStrings; override;
    class function GetItemEnabled(const ACheckListBox: TCustomCheckListBox;
      const AIndex: integer): Boolean; override;
    class function GetState(const ACheckListBox: TCustomCheckListBox;
      const AIndex: integer): TCheckBoxState; override;
    class procedure SetItemEnabled(const ACheckListBox: TCustomCheckListBox;
      const AIndex: integer; const AEnabled: Boolean); override;
    class procedure SetState(const ACheckListBox: TCustomCheckListBox;
      const AIndex: integer; const AState: TCheckBoxState); override;
  end;


implementation

function CheckListBoxWndProc(Window: HWnd; Msg: UInt; WParam: Windows.WParam;
    LParam: Windows.LParam): LResult; stdcall;
var
  WindowInfo: PWin32WindowInfo;

  procedure CheckListBoxLButtonDown;
  var
    I: Integer;
    ItemRect: Windows.Rect;
    MousePos: Windows.Point;
    Message: TLMessage;
  begin
    MousePos.X := GET_X_LPARAM(LParam);
    MousePos.Y := GET_Y_LPARAM(LParam);
    for I := 0 to Windows.SendMessage(Window, LB_GETCOUNT, 0, 0) - 1 do
    begin
      Windows.SendMessage(Window, LB_GETITEMRECT, I, PtrInt(@ItemRect));
      ItemRect.Right := ItemRect.Left + ItemRect.Bottom - ItemRect.Top;
      if Windows.PtInRect(ItemRect, MousePos) then
      begin
        // item clicked: toggle
        if I < TCheckListBox(WindowInfo^.WinControl).Items.Count then
        begin
          if TCheckListBox(WindowInfo^.WinControl).ItemEnabled[I] then
          begin
            TCheckListBox(WindowInfo^.WinControl).Toggle(I);
            Message.Msg := LM_CHANGED;
            Message.WParam := I;
            DeliverMessage(WindowInfo^.WinControl, Message);
          end;
        end;
        // can only click one item
        Exit;
      end;
    end;
  end;

begin
  Result := WindowProc(Window, Msg, WParam, LParam);
  // move groupbox specific code here
  case Msg of
    WM_LBUTTONDOWN, WM_LBUTTONDBLCLK:
    begin
      WindowInfo := GetWin32WindowInfo(Window);
      if (WindowInfo <> nil) and (WindowInfo^.WinControl <> nil) then
        CheckListBoxLButtonDown;
    end;
  end;
end;

class function TWin32WSCustomCheckListBox.CreateHandle(
  const AWinControl: TWinControl; const AParams: TCreateParams): TLCLIntfHandle;
var
  Params: TCreateWindowExParams;
begin
  Params := GetListBoxParams(TCustomListBox(AWinControl), AParams, True);
  Params.SubClassWndProc := @CheckListBoxWndProc;
  // create window
  FinishCreateWindow(AWinControl, Params, False);
  // listbox is not a transparent control -> no need for parentpainting
  Params.WindowInfo^.needParentPaint := False;
  Result := Params.Window;
end;

class function  TWin32WSCustomCheckListBox.GetStrings(const ACustomListBox: TCustomListBox): TStrings;
var
  Handle: HWND;
begin
  Handle := ACustomListBox.Handle;
  Result := TWin32CheckListBoxStrings.Create(Handle, ACustomListBox);
  GetWin32WindowInfo(Handle)^.List := Result;
end;

class function TWin32WSCustomCheckListBox.GetItemEnabled(
  const ACheckListBox: TCustomCheckListBox; const AIndex: integer): Boolean;
begin
  Result := TWin32CheckListBoxStrings(ACheckListBox.Items).Enabled[AIndex];
end;

class function TWin32WSCustomCheckListBox.GetState(
  const ACheckListBox: TCustomCheckListBox; const AIndex: integer
  ): TCheckBoxState;
begin
  Result := TWin32CheckListBoxStrings(ACheckListBox.Items).State[AIndex];
end;

class procedure TWin32WSCustomCheckListBox.SetItemEnabled(
  const ACheckListBox: TCustomCheckListBox; const AIndex: integer;
  const AEnabled: Boolean);
var
  SizeRect: Windows.RECT;
  Handle: HWND;
begin
  TWin32CheckListBoxStrings(ACheckListBox.Items).Enabled[AIndex] := AEnabled;

  // redraw control
  Handle := ACheckListBox.Handle;
  Windows.SendMessage(Handle, LB_GETITEMRECT, AIndex, LPARAM(@SizeRect));
  Windows.InvalidateRect(Handle, @SizeRect, False);
end;

class procedure TWin32WSCustomCheckListBox.SetState(
  const ACheckListBox: TCustomCheckListBox; const AIndex: integer;
  const AState: TCheckBoxState);
var
  SizeRect: Windows.RECT;
  Handle: HWND;
begin
  TWin32CheckListBoxStrings(ACheckListBox.Items).State[AIndex] := AState;

  // redraw control
  Handle := ACheckListBox.Handle;
  Windows.SendMessage(Handle, LB_GETITEMRECT, AIndex, LPARAM(@SizeRect));
  Windows.InvalidateRect(Handle, @SizeRect, False);
end;

end.
