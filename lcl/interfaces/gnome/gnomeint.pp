{
 /***************************************************************************
                         GNOMEINT.pp  -  GNOMEInterface Object
                             -------------------

                   Initial Revision  : Thu Oct 3rd EST 2002


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

unit GNOMEInt;

{$mode objfpc}
{$LONGSTRINGS ON}

interface

{$ifdef Trace}
{$ASSERTIONS ON}
{$endif}

{off $DEFINE NoGdkPixbufLib}

uses
  InterfaceBase, {$Ifndef NoGdkPixbufLib}gdkpixbuf,{$EndIf} gtk, gdk,
  glib, SysUtils, LMessages, Classes, Controls, Forms, VclGlobals,
  LCLLinux, LCLType, gtkDef, DynHashArray, LazQueue, GraphType,
  GraphicsMath, gtkInt;

type
  TGnomeObject = class(TGtkObject)
  public
    {$I gnomewinapih.inc}
  end;


implementation

 {$I gnomewinapi.inc}

initialization

finalization

end.
