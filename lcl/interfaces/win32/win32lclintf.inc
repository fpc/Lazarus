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

{------------------------------------------------------------------------------
  Function: LCLCheckMenuItem
  Params:  AMenuItem
  Returns: Nothing

  Checks or unchecks the specified menu item.
 ------------------------------------------------------------------------------}
Function TWin32Object.LclCheckMenuItem(AMenuItem: TMenuItem): Boolean;
var
  CheckFlag: Integer;
Begin
  if AMenuItem.Checked then CheckFlag := MF_CHECKED
  else CheckFlag := MF_UNCHECKED;
  CheckFlag := CheckFlag or MF_BYCOMMAND;
  Result := Windows.CheckMenuItem(AMenuItem.Parent.Handle, AMenuItem.Command, CheckFlag) <> $FFFFFFFF;
End;

{------------------------------------------------------------------------------
  Function: LCLEnableMenuItem
  Params: AMenuItem: 
  Returns:

  Enables, disables, or grays the specified menu item.
 ------------------------------------------------------------------------------}
Function TWin32Object.LclEnableMenuItem(AMenuItem: TMenuItem): Boolean;
Var
  EnableFlag: Integer;
Begin
  if AMenuItem.Enabled then EnableFlag := MF_ENABLED
  else EnableFlag := MF_GRAYED;
  EnableFlag := EnableFlag or MF_BYCOMMAND;
  Result := Windows.EnableMenuItem(AMenuItem.Parent.Handle, AMenuItem.Command, EnableFlag);
End;



//##apiwiz##eps##   // Do not remove, no wizard declaration after this line

{ =============================================================================

  $Log$
  Revision 1.1  2003/11/26 00:23:47  marc
  * implemented new LCL(check|enable)Menuitem functions
  * introduced the lclintf inc files to win32


}
