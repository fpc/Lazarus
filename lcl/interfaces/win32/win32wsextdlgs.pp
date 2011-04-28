{ $Id$}
{
 *****************************************************************************
 *                             Win32WSExtDlgs.pp                             * 
 *                             -----------------                             * 
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
unit Win32WSExtDlgs;

{$mode objfpc}{$H+}
{$I win32defines.inc}

interface

uses
////////////////////////////////////////////////////
// I M P O R T A N T                                
////////////////////////////////////////////////////
// To get as little as posible circles,
// uncomment only when needed for registration
////////////////////////////////////////////////////
  Windows,
////////////////////////////////////////////////////
  WSExtDlgs, WSLCLClasses, Win32WSDialogs, Win32WSControls, Win32Int, Win32Proc,
  Types, Controls, Dialogs, ExtDlgs, LCLType, Graphics, Themes, Win32Extra, ShlObj;

type

  { TWin32WSPreviewFileControl }

  TWin32WSPreviewFileControl = class(TWSPreviewFileControl)
  published
    class function CreateHandle(const AWinControl: TWinControl;
          const AParams: TCreateParams): HWND; override;
  end;

  { TWin32WSPreviewFileDialog }

  TWin32WSPreviewFileDialog = class(TWSPreviewFileDialog)
  published
  end;

  { TWin32WSOpenPictureDialog }

  TWin32WSOpenPictureDialog = class(TWin32WSOpenDialog)
  published
    class function CreateHandle(const ACommonDialog: TCommonDialog): THandle; override;
  end;

  { TWin32WSSavePictureDialog }

  TWin32WSSavePictureDialog = class(TWin32WSSaveDialog)
  published
    class function CreateHandle(const ACommonDialog: TCommonDialog): THandle; override;
  end;

  { TWin32WSCalculatorDialog }

  TWin32WSCalculatorDialog = class(TWSCalculatorDialog)
  published
  end;

  { TWin32WSCalculatorForm }

  TWin32WSCalculatorForm = class(TWSCalculatorForm)
  published
  end;

  { TWin32WSCalendarDialogForm }

  TWin32WSCalendarDialogForm = class(TWSCalendarDialogForm)
  published
  end;

  { TWin32WSCalendarDialog }

  TWin32WSCalendarDialog = class(TWSCalendarDialog)
  published
  end;


implementation

{$R *.res}

function OpenPictureDialogCallBack(hWnd: Handle; uMsg: UINT; wParam: WPARAM;
  lParam: LPARAM): UINT_PTR; stdcall;
var
  OpenFileName: Windows.POPENFILENAME;
  DialogRec: POpenFileDialogRec;
  AControl: TPreviewFileControl;
  stc32Handle: Handle;
  ARect, ADialogRect: TRect;
begin
  Result := OpenFileDialogCallBack(hWnd, uMsg, wParam, lparam);
  if uMsg = WM_INITDIALOG then
  begin
    OpenFileName := Windows.POPENFILENAME(lParam);
    // Our dialog template contains a special control with ID stc32 which
    // tells it how our template will be positioned. We need to place our
    // control at the end of tempate
    stc32Handle := GetDlgItem(hWnd, 1119);
    if stc32Handle <> 0 then
    begin
      DialogRec := POpenFileDialogRec(OpenFileName^.lCustData);
      AControl := TPreviewFileDialog(DialogRec^.Dialog).PreviewFileControl;
      // attach our child to the template window
      AControl.ParentWindow := hWnd;

      GetWindowRect(stc32Handle, ARect);
      ScreenToClient(hWnd, ARect.TopLeft);
      ScreenToClient(hWnd, ARect.BottomRight);
      GetClientRect(hWnd, ADialogRect);

      with ARect do
      begin
        Left := Right;
        Top := 30; // do not know how to get relative coord
        Right := ADialogRect.Right - 4;
        Bottom := ADialogRect.Bottom;
      end;

      AControl.BoundsRect := ARect;
      AControl.Color := clBtnFace;
    end;
  end;
end;

procedure AddPreviewControl(const ACommonDialog: TCommonDialog; OFN: LPOPENFILENAME);
const
  ResName: WideString = 'LAZ_PIC_DIALOG_TEMPLATE';
begin
  if (TPreviewFileDialog(ACommonDialog).PreviewFileControl <> nil) and
     not (ofOldStyleDialog in TPreviewFileDialog(ACommonDialog).Options) then
    with OFN^ do
    begin
    {$ifdef WindowsUnicodeSupport}
      if UnicodeEnabledOS then
      begin
        lpTemplateName := AllocMem(Length(ResName) * 2 + 2);
        Move(PChar(ResName)^, lpTemplateName^, Length(ResName) * 2);
      end
      else
    {$endif}
      begin
        lpTemplateName := AllocMem(Length(ResName) + 1);
        Move(PChar(AnsiString(ResName))^, lpTemplateName^, Length(ResName));
      end;
      Flags := Flags or OFN_ENABLETEMPLATE;
      lpfnHook := LPOFNHOOKPROC(@OpenPictureDialogCallBack);
    end;
end;

{ TWin32WSOpenPictureDialog }

class function TWin32WSOpenPictureDialog.CreateHandle(const ACommonDialog: TCommonDialog): THandle;
{$ifdef UseVistaDialogs}
var
  Dialog: IFileOpenDialog;
  fos: FILEOPENDIALOGOPTIONS;
{$endif}
begin
  Result := inherited CreateHandle(ACommonDialog);
  {$ifdef UseVistaDialogs}
  if (WindowsVersion >= wvVista) and ThemeServices.ThemesEnabled then
  begin
    Dialog := IFileOpenDialog(Result);
    if Succeeded(Dialog.GetOptions(@fos)) then
    begin
      fos := fos or FOS_FORCEPREVIEWPANEON;
      Dialog.SetOptions(fos);
    end;
  end
  else
  {$endif}
    AddPreviewControl(ACommonDialog, LPOPENFILENAME(Result));
end;

{ TWin32WSPreviewFileControl }

class function TWin32WSPreviewFileControl.CreateHandle(
  const AWinControl: TWinControl; const AParams: TCreateParams): HWND;
var
  Params: TCreateWindowExParams;
begin
  // general initialization of Params
  PrepareCreateWindow(AWinControl, AParams, Params);
  // customization of Params
  with Params do
  begin
    pClassName := @ClsName[0];
    SubClassWndProc := nil;
  end;
  // create window
  FinishCreateWindow(AWinControl, Params, false);
  Result := Params.Window;
end;

{ TWin32WSSavePictureDialog }

class function TWin32WSSavePictureDialog.CreateHandle(
  const ACommonDialog: TCommonDialog): THandle;
{$ifdef UseVistaDialogs}
var
  Dialog: IFileSaveDialog;
  fos: FILEOPENDIALOGOPTIONS;
{$endif}
begin
  Result := inherited CreateHandle(ACommonDialog);
  {$ifdef UseVistaDialogs}
  if (WindowsVersion >= wvVista) and ThemeServices.ThemesEnabled then
  begin
    Dialog := IFileSaveDialog(Result);
    if Succeeded(Dialog.GetOptions(@fos)) then
    begin
      fos := fos or FOS_FORCEPREVIEWPANEON;
      Dialog.SetOptions(fos);
    end;
  end
  else
  {$endif}
    AddPreviewControl(ACommonDialog, LPOPENFILENAME(Result));
end;

end.
