{ $Id$ }
{                        ----------------------------------------------  
                         Interfaces.pp  -  determines the placeholder
                                           interface to compile the LCL
                         ---------------------------------------------- 
 
 @created(Tue Mar 27st WET 2001)
 @lastmod($Date$)
 @author(Marc Weustink <marc@@lazarus.dommelstein.net>)                       

 This unit contains the definition for the LCL interface

 
/*************************************************************************** 
 *                                                                         * 
 *   This program is free software; you can redistribute it and/or modify  * 
 *   it under the terms of the GNU General Public License as published by  * 
 *   the Free Software Foundation; either version 2 of the License, or     * 
 *   (at your option) any later version.                                   * 
 *                                                                         * 
 ***************************************************************************/ 
 } 


unit interfaces;
 
{$mode objfpc} 

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
