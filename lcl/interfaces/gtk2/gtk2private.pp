{ $Id: $ }
{
                  ----------------------------------------
                  gtk2private.pp  -  Gtk2 internal classes
                  ----------------------------------------

 @created(Thu Feb 1st WET 2007)
 @lastmod($Date: $)
 @author(Marc Weustink <marc@@lazarus.dommelstein.net>)

 This unit contains the private classhierarchy for the gtk implemetations
 This hierarchy reflects (more or less) the gtk widget hierarchy

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

unit Gtk2Private;
{$mode objfpc}{$H+}

interface

uses
  // libs
  Gtk2, Glib2, Gdk2,
  // LCL
  LCLType, LMessages, LCLProc, Controls, Classes, SysUtils, Forms,
  // widgetset
  WSControls, WSLCLClasses, WSProc,
  // interface
  GtkDef, GtkProc, GtkPrivate;


type
  { TGtk2PrivateWidget }
  { Private class for gtkwidgets }

  TGtk2PrivateWidget = class(TGtkPrivateWidget)
  private
  protected
  public
  end;
  
  
  { TGtk2PrivateContainer }
  { Private class for gtkcontainers }

  TGtk2PrivateContainer = class(TGtkPrivateContainer)
  private
  protected
  public
  end;


  { TGtk2PrivateBin }
  { Private class for gtkbins }

  TGtk2PrivateBin = class(TGtkPrivateBin)
  private
  protected
  public
  end;


  { TGtk2PrivateWindow }
  { Private class for gtkwindows }

  TGtk2PrivateWindow = class(TGtkPrivateWindow)
  private
  protected
  public
  end;


  { TGtk2PrivateDialog }
  { Private class for gtkdialogs }

  TGtk2PrivateDialog = class(TGtkPrivateDialog)
  private
  protected
  public
  end;


  { TGtk2PrivateButton }
  { Private class for gtkbuttons }

  TGtk2PrivateButton = class(TGtkPrivateButton)
  private
  protected
  public
    class procedure UpdateCursor(AInfo: PWidgetInfo); override;
  end;
  
  TGtk2PrivateNotebook = class(TGtkPrivateNotebook)
  private
  protected
  public
    class procedure UpdateCursor(AInfo: PWidgetInfo); override;
  end;


implementation
{$I Gtk2PrivateWidget.inc}
end.
  
