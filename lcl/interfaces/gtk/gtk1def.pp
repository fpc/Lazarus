{ $Id$ 
                         -------------------------------
                         gtk1def.pp  -  Type definitions
                         ------------------------------- 
 
 @created(Tue Nov 20st WET 2007)
 @lastmod($Date$)
 @author(Marc Weustink <marc@@dommelstein.net>)                       

 This unit contains type definitions needed in the GTK1 <-> LCL interface
 
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


unit Gtk1Def;
 
{$mode objfpc} {$H+}

interface

uses
  glib, gdk, gtk, gdkpixbuf,
//  Classes, SysUtils, LCLIntf, LCLProc, LCLType, DynHashArray,
//  GraphType, GtkExtra,
  GtkDef;


type

  { TGtk1DeviceContext }

  TGtk1DeviceContext = class(TGtkDeviceContext)
  public
    function GetFunction: TGdkFunction; override;
  end;

implementation

{$i gtk1devicecontext.inc}

end.
