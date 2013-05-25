{%MainUnit ../stdctrls.pp}

{
 *****************************************************************************
  This file is part of the Lazarus Component Library (LCL)

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************
}

{ TMemoScrollbar }

function TMemoScrollbar.GetHorzScrollBar: TControlScrollBar;
begin
  Result:=TCustomMemo(FControl).HorzScrollBar;
end;

function TMemoScrollbar.GetVertScrollBar: TControlScrollBar;
begin
  Result:=TCustomMemo(FControl).VertScrollBar;
end;


// included by stdctrls.pp
