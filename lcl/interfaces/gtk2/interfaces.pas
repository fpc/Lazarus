{ 
 /*************************************************************************** 
              Interfaces.pp  -  determines what interface to use
              --------------------------------------------------
 
                   Initial Revision  : Thu July 1st CST 1999 
 
 
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

interface

uses 
  {$IFDEF UNIX}{$IFNDEF DisableCWString}cwstring,{$ENDIF}{$ENDIF}
  InterfaceBase;

implementation

uses 
  Gtk2Int, Forms;

initialization
  CreateWidgetset(TGtk2WidgetSet);

finalization
  FreeWidgetSet;

end.
