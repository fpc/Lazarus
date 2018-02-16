{%MainUnit win32wscomctrls.pp}
{
 *****************************************************************************
  This file is part of the Lazarus Component Library (LCL)

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************
}

{ TWin32WSCustomTreeView }

class function TWin32WSCustomTreeView.GetDoubleBuffered(
  const AWinControl: TWinControl): Boolean;
begin
  Result := GetWin32ThemedDoubleBuffered(AWinControl);
end;
