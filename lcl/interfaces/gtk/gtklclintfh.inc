{ $Id$ }

// included by gtkint.pp

{******************************************************************************
                 All GTK interface communication implementations.
                   Initial Revision  : Sun Nov 23 23:53:53 2003


  !! Keep alphabetical !!

  Support routines go to gtkproc.pp

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
function GetAcceleratorString(const AVKey: Byte; const AShiftState: TShiftState): String; override;
function GetControlConstraints(Constraints: TObject): boolean; override;
function GetListBoxIndexAtY(ListBox: TComponent; y: integer): integer; override;
function GetListBoxItemRect(ListBox: TComponent; Index: integer; var ARect: TRect): boolean; override;

function MenuItemSetCheck(BaseMenuItem: TComponent): Boolean; override;
function MenuItemSetEnable(BaseMenuItem: TComponent): Boolean; override;

procedure StatusBarPanelUpdate(StatusBar: TObject; Index: integer); override;
procedure StatusBarSetText(StatusBar: TObject; PanelIndex: integer); override;
procedure StatusBarUpdate(StatusBar: TObject); override;
//##apiwiz##eps##   // Do not remove, no wizard declaration after this line

{ =============================================================================

  $Log$
  Revision 1.10  2004/02/02 12:44:45  mattias
  implemented interface constraints

  Revision 1.9  2004/01/12 08:36:34  micha
  statusbar interface dependent reimplementation (from vincent)

  Revision 1.8  2004/01/11 16:38:29  marc
  * renamed (Check|Enable)MenuItem to MenuItemSet(Check|Enable)
  + Started with accelerator nameing routines
  * precheckin for createwidget splitup

  Revision 1.7  2004/01/11 11:57:54  mattias
  implemented TCustomListBox.ItemRect for gtk1 intf

  Revision 1.6  2004/01/09 20:03:13  mattias
  implemented new statusbar methods in gtk intf

  Revision 1.5  2004/01/03 11:57:48  mattias
  applied implementation for LM_LB_GETINDEXAT  from Vincent

  Revision 1.4  2003/11/27 23:02:30  mattias
  removed menutype.pas

  Revision 1.3  2003/11/26 21:30:19  mattias
  reduced unit circles, fixed fpImage streaming

  Revision 1.2  2003/11/26 00:23:47  marc
  * implemented new LCL(check|enable)Menuitem functions
  * introduced the lclintf inc files to win32

  Revision 1.1  2003/11/24 11:03:07  marc
  * Splitted winapi*.inc into a winapi and a lcl interface communication part

}
