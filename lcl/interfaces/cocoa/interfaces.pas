{ 
 /*************************************************************************** 
                 Interfaces.pp  -  determines what interface to use
                 --------------------------------------------------
 
                   Initial Revision  : Mon August 6st CST 2004


 ***************************************************************************/
 
 *****************************************************************************
 *                                                                           *
 *  This file is part of the Lazarus Component Library (LCL)                 *
 *                                                                           *
 *  See the file COPYING.modifiedLGPL.txt, included in this distribution,    *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************
}
unit Interfaces;
 
{$mode objfpc}{$H+} 

{$IFNDEF COCOA}
{$ERROR wrong interfaces source}
{$ENDIF}

interface

uses 
  {$IFNDEF DisableCWString}cwstring,{$ENDIF}
  InterfaceBase;

implementation

uses 
  CocoaInt, Forms;

initialization
  CreateWidgetset(TCocoaWidgetSet);

finalization
  FreeWidgetSet;

end.
