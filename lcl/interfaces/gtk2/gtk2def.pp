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
  GtkExtra, GtkDef, SysUtils;

type
  { TGtk2DeviceContext }

  TGtk2DeviceContext = class(TGtkDeviceContext)
  protected
    function GetFunction: TGdkFunction; override;
  public
    procedure DrawTextWithColors(AText: PChar; ALength: LongInt; X, Y: Integer; FGColor, BGColor: PGdkColor);
  end;
  
  procedure SetLayoutText(ALayout: PPangoLayout; AText: PChar; ALength: PtrInt);

implementation
{$i gtk2devicecontext.inc}

end.
