{
 /***************************************************************************
                         GTK1INT.pp  -  GTKInterface Object
                             -------------------

                  Initial Revision  : Thu November 15th CST 2007


 ***************************************************************************/

 *****************************************************************************
 *                                                                           *
 *  This file is part of the Lazarus Component Library (LCL)                 *
 *                                                                           *
 *  See the file COPYING.modifiedLGPL.txt, included in this distribution,    *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************
 }

unit Gtk1Int;

{$mode objfpc}{$H+}

interface

{$ifdef Trace}
{$ASSERTIONS ON}
{$endif}


{$I gtkdefines.inc}

uses
  {$IFDEF UNIX}
  // use unix units first,
  // if not, TSize is taken from the unix unit instead of types.
  ctypes, baseunix, unix,
  {$ENDIF}
  {$IFDEF TraceGdiCalls}
  LineInfo,
  {$ENDIF}
  // rtl+fcl
  Types, Classes, SysUtils, FPCAdds,
  // interfacebase
  InterfaceBase,
  // gtk
    glib, gdk, gtk, gdkpixbuf,
  // Target OS specific
  {$ifdef HasX}
  x, xlib,
  {$endif}
  Math, // after gtk to get the correct Float type
  // LCL
  Translations, ExtDlgs, Dialogs, Controls, Forms, LCLStrConsts, LMessages,
  LCLProc, LCLIntf, LCLType, DynHashArray, GraphType, GraphMath,
  Graphics, Menus, Maps, Themes,
  // widgetset
  GtkInt,
  GtkDebug,
  GtkFontCache, GtkDef, Gtk1Def, GtkProc, gtkMsgQueue, GtkExtra, gtkWSPrivate,
  WSLCLClasses;

type

  { TGTK1WidgetSet }

  TGTK1WidgetSet = class(TGTKWidgetSet)
  protected
    function GetDeviceContextClass: TGtkDeviceContextClass; override;
  public
    procedure SetWidgetFont(const AWidget: PGtkWidget; const AFont: TFont); override;
    procedure SetLabelCaption(const ALabel: PGtkLabel; const ACaption: String;
                              const AComponent: TComponent = nil;
                              const ASignalWidget: PGTKWidget = nil;
                              const ASignal: PChar = nil); override;
  end;
  
var
  GTK1WidgetSet: TGTK1WidgetSet absolute GtkWidgetSet;

implementation
  
uses
////////////////////////////////////////////////////
// I M P O R T A N T
////////////////////////////////////////////////////
// To get as little as possible circles,
// uncomment only those units with implementation
////////////////////////////////////////////////////
// Gtk1WSActnList,
// Gtk1WSArrow,
// Gtk1WSButtons,
// Gtk1WSCalendar,
// Gtk1WSCheckLst,
// Gtk1WSComCtrls,
// Gtk1WSControls,
// Gtk1WSDbCtrls,
// Gtk1WSDBGrids,
// Gtk1WSDialogs,
// Gtk1WSEditBtn,
// Gtk1WSExtCtrls,
// Gtk1WSExtDlgs,
// Gtk1WSFileCtrl,
// Gtk1WSForms,
// Gtk1WSGrids,
// Gtk1WSImgList,
// Gtk1WSMaskEdit,
// Gtk1WSMenus,
// Gtk1WSPairSplitter,
// Gtk1WSSpin,
// Gtk1WSStdCtrls,
// Gtk1WSToolwin,
// Gtk1Themes,
////////////////////////////////////////////////////
  StdCtrls,
  GTKWinApiWindow,
  GtkWSFactory;

{$include gtk1widgetset.inc}

end.
  
