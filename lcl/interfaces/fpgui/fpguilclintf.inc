{%MainUnit fpguiint.pp}

{******************************************************************************
                 All FPGUI interface communication implementations.
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
 *  See the file COPYING.modifiedLGPL, included in this distribution,        *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************
}

//##apiwiz##sps##   // Do not remove

{------------------------------------------------------------------------------
  function FontCanUTF8(Font: HFont): boolean;

  True if font recognizes Unicode UTF8 encoding.
  
  FPGUI supports only Unicode
 ------------------------------------------------------------------------------}
function TFpGuiWidgetSet.FontCanUTF8(Font: HFont): boolean;
begin
  Result := True;
end;

//##apiwiz##eps##   // Do not remove, no wizard declaration after this line

