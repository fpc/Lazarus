{
 ***************************************************************************
                                osprinters.pas
                                ------------
                               Printer object
                     Initial Revision  : 09 Mars 2005

 ***************************************************************************

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

  Author: Olivier

  Abstract :
    Unit to centralize the implementation of Printer according to the target OS

  history
    09/03/2005 OG - Create
-----------------------------------------------------------------------------}
unit OSPrinters;

{$mode objfpc}{$H+}

interface


{$IFDEF UNIX}
  {$IFDEF LCLCarbon}
    {$IFNDEF NativePrint}
      {$I cupsprinters_h.inc}
    {$ELSE}
      {$I carbonprinters_h.inc}
    {$ENDIF}
  {$ELSE}
    {$IFDEF LCLQt}
      {$I qtprinters_h.inc}
    {$ELSE}
      {$I cupsprinters_h.inc}
    {$ENDIF}
  {$ENDIF}
{$ENDIF}

{$IFDEF MSWindows}
  {$IFDEF LCLQt}
    {$I qtprinters_h.inc}
  {$ELSE}
    {$I winprinters_h.inc}
  {$ENDIF}
{$ENDIF}

implementation

{$IFDEF UNIX}
  {$IFDEF LCLCarbon}
    {$IFNDEF NativePrint}
      {$I cupsprinters.inc}
    {$ELSE}
      {$I carbonprinters.inc}
    {$ENDIF}
  {$ELSE}
    {$IFDEF LCLQt}
      {$I qtprinters.inc}
    {$ELSE}
      {$I cupsprinters.inc}
    {$ENDIF}
  {$ENDIF}
{$ENDIF}

{$IFDEF MSWindows}
  {$IFDEF LCLQt}
    {$I qtprinters.inc}
  {$ELSE}
    {$I winprinters.inc}
  {$ENDIF}
{$ENDIF}

end.
