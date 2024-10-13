{ $Id$}
{
 *****************************************************************************
 *                            TestMockWSControls.pp                          *
 *                            ------------------                             *
 *                                                                           *
 *                                                                           *
 *****************************************************************************

 *****************************************************************************
  This file is part of the Lazarus Component Library (LCL)

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************
}
unit TestMockWSControls;

{$mode objfpc}{$H+}
{$OPTIMIZATION NOREMOVEEMPTYPROCS}

interface

uses
  WSControls, WSLCLClasses, LCLType, Controls, TestMockMiscClasses,
  SysUtils;
  //{ LCL }
  //InterfaceBase;

type

  { TTestMockWSControl }

  TTestMockWSControl = class(TWSControl)
  published
  end;

  { TTestMockWSWinControl }

  TTestMockWSWinControl = class(TWSWinControl)
  published
    class function  CreateHandle(const AWinControl: TWinControl;
          const AParams: TCreateParams): HWND; override;
    class procedure DestroyHandle(const AWinControl: TWinControl); override;
  end;

  { TTestMockWSGraphicControl }

  TTestMockWSGraphicControl = class(TWSGraphicControl)
  published
  end;

  { TTestMockWSCustomControl }

  TTestMockWSCustomControl = class(TWSCustomControl)
  published
  end;



implementation


{ TTestMockWSWinControl }

class function TTestMockWSWinControl.CreateHandle(const AWinControl: TWinControl;
  const AParams: TCreateParams): HWND;
begin
  Result := HWND(TTestMockWindowHandle.Create(AWinControl));
end;

class procedure TTestMockWSWinControl.DestroyHandle(const AWinControl: TWinControl);
begin
  if not AWinControl.HandleAllocated then
    exit;
  assert(TObject(AWinControl.Handle) is TTestMockWindowHandle, 'TTestMockWSWinControl.DestroyHandle: TObject(AWinControl.Handle) is TTestMockWindowHandle');
  TTestMockWindowHandle(AWinControl.Handle).Free;
end;

end.
