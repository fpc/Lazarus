{ $Id$ }
{******************************************************************************
                 All GTK interface communication implementations.
                   Initial Revision  : Sun Nov 23 23:53:53 2003


  !! Keep alphabetical !!

  Support routines go to win32proc.inc

 ******************************************************************************
 Interface
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
procedure DrawArrow(Arrow: TComponent; Canvas: TPersistent); override;

function GetAcceleratorString(const AVKey: Byte; const AShiftState: TShiftState): String; override;
function GetControlConstraints(Constraints: TObject): boolean; override;
function GetListBoxIndexAtY(ListBox: TComponent; y: integer): integer; override;

function MenuItemSetCheck(BaseMenuItem: TComponent): Boolean; override;
function MenuItemSetEnable(BaseMenuItem: TComponent): Boolean; override;

procedure StatusBarPanelUpdate(StatusBar: TObject; PanelIndex: integer); override;
procedure StatusBarSetText(StatusBar: TObject; PanelIndex: integer); override;
procedure StatusBarUpdate(StatusBar: TObject); override;
//##apiwiz##eps##   // Do not remove, no wizard declaration after this line

{ =============================================================================

  $Log$
  Revision 1.10  2004/02/20 19:52:18  micha
  fixed: tarrow crash in win32
  added: interface function DrawArrow to draw themed arrow

  Revision 1.9  2004/02/05 13:53:38  mattias
  fixed GetConstraints for win32 intf

  Revision 1.8  2004/02/02 16:56:44  micha
  implement GetControlConstraints for combobox

  Revision 1.7  2004/01/12 08:36:34  micha
  statusbar interface dependent reimplementation (from vincent)

  Revision 1.6  2004/01/11 16:38:29  marc
  * renamed (Check|Enable)MenuItem to MenuItemSet(Check|Enable)
  + Started with accelerator nameing routines
  * precheckin for createwidget splitup

  Revision 1.5  2004/01/09 20:03:13  mattias
  implemented new statusbar methods in gtk intf

  Revision 1.4  2004/01/03 11:57:48  mattias
  applied implementation for LM_LB_GETINDEXAT  from Vincent

  Revision 1.3  2003/11/27 23:02:30  mattias
  removed menutype.pas

  Revision 1.2  2003/11/26 21:55:15  mattias
  fixed win32 TBaseMenuitem

  Revision 1.1  2003/11/26 00:23:47  marc
  * implemented new LCL(check|enable)Menuitem functions
  * introduced the lclintf inc files to win32

}
