{
                  ----------------------------------
                  gtk2proc.pp  - gtk 2 interface procs
                  ----------------------------------

 This unit contains procedures/functions needed for the gtk 2 <-> LCL interface
}
{
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
unit gtk2proc;

{$mode objfpc}{$H+}

interface

{$I gtkdefines.inc}

uses
  {$IFDEF windows}
    // use windows unit first,
    // if not, Rect and Point are taken from the windows unit instead of classes.
    Windows, // needed for keyboard handling
  {$endif}
  {$IFDEF Unix}
    baseunix, unix,
  {$ENDIF}
  SysUtils, Classes, FPCAdds,
  {$IFDEF HasX}
    XAtom, X, XLib, XUtil, //Font retrieval and Keyboard handling
  {$ENDIF}
  InterfaceBase,
  // gtk2
    glib2, gdk2pixbuf, gdk2, gtk2, Pango,
    {$ifdef HasGdk2X}
    gdk2x,
    {$endif}
  // Other units
  Math, // Math after gtk to get the correct Float type
  LMessages, LCLProc, LCLStrConsts, LCLIntf, LCLType, DynHashArray, Maps, Masks,
  GraphType, GraphMath, Graphics, GTKWinApiWindow, LResources, Controls, Forms,
  Buttons, Menus, StdCtrls, ComCtrls, ExtCtrls, Dialogs, ExtDlgs,
  FileUtil, ImgList, GtkFontCache, GTKGlobals, gtkDef, GtkExtra, GtkDebug;

{
  Callbacks for events

  gtk2callback.inc headers
}

function GTKWindowStateEventCB(widget: PGtkWidget;
                               state: PGdkEventWindowState;
                               data: gpointer): gboolean; cdecl;

function gtkMouseWheelCB(widget: PGtkWidget; event: PGdkEventScroll;
                         data: gPointer): GBoolean; cdecl;

implementation

uses gtkproc; // Remove when separation is complete

{$include gtk2callback.inc}

end.

