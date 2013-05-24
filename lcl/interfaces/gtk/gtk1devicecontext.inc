{%MainUnit gtk1def.pp}

{******************************************************************************
                                   TGtk1DeviceContext
 ******************************************************************************

 *****************************************************************************
  This file is part of the Lazarus Component Library (LCL)

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************
}

{$IFOPT C-}
// Uncomment for local trace
//  {$C+}
//  {$DEFINE ASSERT_IS_ON}
{$ENDIF}

{ TGtk1DeviceContext }

function TGtk1DeviceContext.GetFunction: TGdkFunction;
begin
  Result := GCValues.thefunction;
end;



