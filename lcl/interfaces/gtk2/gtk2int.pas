{
 /***************************************************************************
                       gtk2int.pas  -  GTK2 Interface Object
                       -------------------------------------


 ***************************************************************************/

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

unit Gtk2Int;

{$mode objfpc}
{$LONGSTRINGS ON}

interface

{$ifdef Trace}
{$ASSERTIONS ON}
{$endif}

uses
  Classes, SysUtils, InterfaceBase, gdk2pixbuf, gtk2, gdk2, glib2, LMessages,
  Controls, Forms, VclGlobals, LCLLinux, LCLType, gtkDef, DynHashArray,
  LazQueue, GraphType, GraphMath, gtkInt;

type
  TGtk2Object = class(TGtkObject)
  end;


implementation

uses
  Graphics, Buttons, Menus, GTKWinApiWindow, StdCtrls, ComCtrls, CListBox,
  KeyMap, Calendar, Arrow, Spin, CommCtrl, ExtCtrls, Dialogs, FileCtrl,
  LResources, Math, gtkglobals, gtkproc, LCLStrConsts;


end.

{
  $Log$
  Revision 1.1  2002/12/15 11:52:28  mattias
  started gtk2 interface

  Revision 1.15  2002/02/09 01:48:23  mattias
  renamed TinterfaceObject.Init to AppInit and TWinControls can now contain childs in gtk

  Revision 1.14  2002/11/03 22:40:00  lazarus
  MG: fixed ControlAtPos

  Revision 1.13  2002/10/30 18:45:52  lazarus
  AJ: fixed compiling & removed '_' from custom stock items

  Revision 1.12  2002/10/26 15:15:50  lazarus
  MG: broke LCL<->interface circles

  Revision 1.11  2002/10/25 15:27:02  lazarus
  AJ: Moved form contents creation to gtkproc for code
      reuse between GNOME and GTK, and to make GNOME MDI
      programming easier later on.

  Revision 1.10  2002/10/24 22:10:39  lazarus
  AJ: More changes for better code reuse between gnome & gtk interfaces

  Revision 1.9  2002/10/23 20:47:27  lazarus
  AJ: Started Form Scrolling
      Started StaticText FocusControl
      Fixed Misc Dialog Problems
      Added TApplication.Title

  Revision 1.8  2002/10/21 13:15:24  lazarus
  AJ:Try and fall back on default style if nil(aka default theme)

  Revision 1.7  2002/10/21 03:23:34  lazarus
  AJ: rearranged GTK init stuff for proper GNOME init & less duplication between interfaces

  Revision 1.6  2002/10/15 22:28:04  lazarus
  AJ: added forcelinebreaks

  Revision 1.5  2002/10/14 14:29:50  lazarus
  AJ: Improvements to TUpDown; Added TStaticText & GNOME DrawText

  Revision 1.4  2002/10/12 16:36:40  lazarus
  AJ: added new QueryUser/NotifyUser

  Revision 1.3  2002/10/10 13:29:08  lazarus
  AJ: added LoadStockPixmap routine & minor fixes to/for GNOMEInt


}
