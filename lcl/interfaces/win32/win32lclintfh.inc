{ $Id$ }
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
function GetListBoxIndexAtY(ListBox: TComponent; y: integer): integer; override;

function LclCheckMenuItem(BaseMenuItem: TComponent): Boolean; override;
function LclEnableMenuItem(BaseMenuItem: TComponent): Boolean; override;

procedure StatusBarInsertPanel(StatusBar: TObject; index: integer); override;
procedure StatusBarPanelUpdate(StatusBar: TObject; index: integer); override;
procedure StatusBarRemovePanel(StatusBar: TObject; index: integer); override;
procedure StatusBarSetSimpleText(StatusBar: TObject); override;
procedure StatusBarSetStyle(StatusBar: TObject); override;
//##apiwiz##eps##   // Do not remove, no wizard declaration after this line

{ =============================================================================

  $Log$
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
