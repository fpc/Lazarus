{
 *****************************************************************************
 *                                                                           *
 *  See the file COPYING.modifiedLGPL, included in this distribution,        *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************

  Author: Mattias Gaertner

}
unit gdk2x;

{$IFDEF FPC}
  {$mode objfpc}
{$ENDIF}
{$IFDEF VER140}
  {$DEFINE KYLIX}
{$ENDIF}

interface

uses
  Classes, SysUtils, libc, glib2, gdk2, xlib, x, xrender;

{$ifdef FREEBSD}
  {$linklib c}
  {$linklib pthread}
{$endif}

{$IFNDEF KYLIX}
  {$PACKRECORDS C}
{$ELSE}
  {$ALIGN 4}
  {$WEAKPACKAGEUNIT}
  {$WARNINGS OFF}
{$ENDIF}

{$DEFINE read_forward_definitions}
type
{$I gdk2x11includes.inc}
{$UNDEF read_forward_definitions}

{$DEFINE read_interface_rest}
{$I gdk2x11includes.inc}
{$UNDEF read_interface_rest}

implementation

{$IFNDEF KYLIX}
{ There is a bug in the compiler. If an external variable is not used, it will
  create code, that can't be relocated by the linker.
  So, use them in this hidden dummy procedure.
}
procedure CheckUnusedVariable; [Public];
begin
  //_gdk_x11_drawable_class
  //_gdk_use_xshm
  //_gdk_nenvent_masks
  //_gdk_event_mask_table
  //_gdk_selection_property
  //_gdk_synchronize
  
  //gdk_display
end;
{$ENDIF}

{*****************************************************************************
 * macro functions
 *
 *****************************************************************************}

// call implementation parts of header files
{$DEFINE read_implementation}
{$I gdk2x11includes.inc}
{$UNDEF read_implementation}

end.

