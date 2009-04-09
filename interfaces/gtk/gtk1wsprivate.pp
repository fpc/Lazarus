{ $Id$ }
{
                  ----------------------------------------
                  gtk1private.pp  -  Gtk1 internal classes
                  ----------------------------------------

 @created(Thu Feb 1st WET 2007)
 @lastmod($Date$)
 @author(Marc Weustink <marc@@lazarus.dommelstein.net>)

 This unit contains the private classhierarchy for the gtk implemetations
 This hierarchy reflects (more or less) the gtk widget hierarchy

 *****************************************************************************
 *                                                                           *
 *  This file is part of the Lazarus Component Library (LCL)                 *
 *                                                                           *
 *  See the file COPYING.modifiedLGPL.txt, included in this distribution,        *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************
}

unit Gtk1WSPrivate;
{$mode objfpc}{$H+}

interface

uses
  // libs
  Gtk, Glib, Gdk,
  // LCL
  LCLType, LMessages, LCLProc, Controls, Classes, SysUtils, Forms,
  // widgetset
  WSControls, WSLCLClasses, WSProc,
  // interface
  GtkDef, GtkProc, GtkWSPrivate, GtkWsControls, GtkInt;


type
  { TGtk1PrivateWidget }
  { Private class for gtkwidgets }

  TGtk1PrivateWidget = class(TGtkPrivateWidget)
  private
  protected
  public
  end;
  
  
  { TGtk1PrivateContainer }
  { Private class for gtkcontainers }

  TGtk1PrivateContainer = class(TGtkPrivateContainer)
  private
  protected
  public
  end;
  
  
  { TGtk1PrivateBin }
  { Private class for gtkbins }

  TGtk1PrivateBin = class(TGtkPrivateBin)
  private
  protected
  public
  end;


  { TGtk1PrivateWindow }
  { Private class for gtkwindows }
  
  TGtk1PrivateWindow = class(TGtkPrivateWindow)
  private
  protected
  public
  end;

  
  { TGtk1PrivateDialog }
  { Private class for gtkdialogs }

  TGtk1PrivateDialog = class(TGtkPrivateDialog)
  private
  protected
  public
  end;


  { TGtk1PrivateButton }
  { Private class for gtkbuttons }
  
  TGtk1PrivateButton = class(TGtkPrivateButton)
  private
  protected
  public
  end;


  { TGtk1PrivateList }
  { Private class for gtklists }

  TGtk1PrivateList = class(TGtkPrivateList)
  private
  protected
  public
    class procedure SetCallbacks(const AGtkWidget: PGtkWidget; const AWidgetInfo: PWidgetInfo); override;
  end;

  { TGtk1PrivateNotebook }
  { Private class for gtknotebooks }

  TGtk1PrivateNotebook = class(TGtkPrivateNotebook)
  private
  protected
  public
  end;
  
implementation
// {$I Gtk1PrivateWidget.inc}
 {$I Gtk1PrivateList.inc}

end.
  
