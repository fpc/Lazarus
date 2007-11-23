{ $Id$ 
                         -------------------------------
                         gtk2def.pp  -  Type definitions
                         ------------------------------- 
 
 @created(Tue Nov 20st WET 2007)
 @lastmod($Date$)
 @author(Marc Weustink <marc@@dommelstein.net>)                       

 This unit contains type definitions needed in the GTK2 <-> LCL interface
 
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


unit Gtk2Def;
 
{$mode objfpc} {$H+}

interface

uses
  glib2, gdk2pixbuf, pango, gdk2, gtk2,
//  Classes, SysUtils, LCLIntf, LCLProc, LCLType, DynHashArray,
//  GraphType, GtkExtra,
  GtkDef;


type

  { TGtk2DeviceContext }

  TGtk2DeviceContext = class(TGtkDeviceContext)
  private
  protected
    function GetFunction: TGdkFunction; override;
  public
  end;

implementation

{$i gtk2devicecontext.inc}

end.
