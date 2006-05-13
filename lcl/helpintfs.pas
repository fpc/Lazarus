{
 /***************************************************************************
                                 helpintfs.pas
                                 -------------
                             Component Library Code


 ***************************************************************************/

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
 
  Author: Mattias Gaertner
  
  Abstract:
    Interfaces to define the abstract HelpSystem.
    You can create your own HelpSystem based on these interfaces
    or use the LCL help system in lazhelpsystem.pas.
    The THTMLHelpDatabase and THTMLBrowserHelpViewer in helphtml.pas use the
    LCL help system.
}
unit HelpIntfs;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;
  
type
  THelpManager = class(TInterfacedObject)
  
  end;

var
  HelpManager: THelpManager = nil;

implementation

end.

