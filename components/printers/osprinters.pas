{
 ***************************************************************************
                                osprinters.pas
                                ------------
                               Printer object
                     Initial Revision  : 09 Mars 2005

 ***************************************************************************

 *****************************************************************************
  This file is part of the Lazarus Component Library (LCL)

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
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

{$IFDEF WinCE}
{$FATAL This unit (and therefore the Printers4Lazarus package) cannot be built for WinCE}
{$ENDIF}

{$IFDEF UNIX}
  {$IFDEF DARWIN}
    {$IFDEF LCLCarbon}
      {$IFNDEF NativePrint}
        {$I cupsprinters_h.inc}
      {$ELSE}
        {$I carbonprinters_h.inc}
      {$ENDIF}
    {$ENDIF}
    {$IFDEF LCLCocoa}
      {$I cocoaprinters_h.inc}
    {$ENDIF}
    {$IF DEFINED(LCLQt) OR DEFINED(LCLQt5)}
      {$I qtprinters_h.inc}
    {$ENDIF}
    {$IFDEF LCLGtk2}
      {$I cupsprinters_h.inc}
    {$ENDIF}
  {$ELSE}
    {$IF DEFINED(LCLQt) OR DEFINED(LCLQt5)}
      {$I qtprinters_h.inc}
    {$ELSE}
      {$I cupsprinters_h.inc}
    {$ENDIF}
  {$ENDIF}
{$ENDIF}

{$IFDEF MSWindows}
  {$IF DEFINED(LCLQt) OR DEFINED(LCLQt5)}
    {$I qtprinters_h.inc}
  {$ELSE}
    {$I winprinters_h.inc}
  {$ENDIF}
{$ENDIF}

implementation

{$IFDEF UNIX}
  {$IFDEF DARWIN}
    {$IFDEF LCLCarbon}
      {$IFNDEF NativePrint}
        {$I cupsprinters.inc}
      {$ELSE}
        {$I carbonprinters.inc}
      {$ENDIF}
    {$ENDIF}
    {$IFDEF LCLCocoa}
      {$I cocoaprinters.inc}
    {$ENDIF}
    {$IF DEFINED(LCLQt) OR DEFINED(LCLQt5)}
      {$I qtprinters.inc}
    {$ENDIF}
    {$IFDEF LCLGtk2}
      {$I cupsprinters.inc}
    {$ENDIF}
  {$ELSE}
    {$IF DEFINED(LCLQt) OR DEFINED(LCLQt5)}
      {$I qtprinters.inc}
    {$ELSE}
      {$I cupsprinters.inc}
    {$ENDIF}
  {$ENDIF}
{$ENDIF}

{$IFDEF MSWindows}
  {$IF DEFINED(LCLQt) OR DEFINED(LCLQt5)}
    {$I qtprinters.inc}
  {$ELSE}
    {$I winprinters.inc}
  {$ENDIF}
{$ENDIF}

end.
