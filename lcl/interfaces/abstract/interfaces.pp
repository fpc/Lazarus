{ $Id$ }
{
                 ----------------------------------------------
                 Interfaces.pp  -  determines the placeholder
                                   interface to compile the LCL
                 ---------------------------------------------- 
 
 @created(Tue Mar 27st WET 2001)
 @lastmod($Date$)
 @author(Marc Weustink <marc@@lazarus.dommelstein.net>)                       

 This unit contains the definition for the LCL interface

 
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
unit Interfaces;
 
{$mode objfpc} {$H+}

interface

uses 
  InterfaceBase;

var
  InterfaceObject : TInterfaceBase;


implementation


initialization
  InterfaceObject := TInterfaceBase.Create;

finalization
  InterfaceObject.Free;

end.

