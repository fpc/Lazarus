{ 
 /*************************************************************************** 
                 Interfaces.pp  -  determines what interface to use
                 --------------------------------------------------
 
                   Initial Revision  : Mon August 6st CST 2004


 ***************************************************************************/
 
 *****************************************************************************
  This file is part of the Lazarus Component Library (LCL)

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************
}
unit Interfaces;
 
{$mode objfpc}{$H+} 

{$IFNDEF CARBON}
{$ERROR wrong interfaces source}
{$ENDIF}
{$IFNDEF i386}
{$ERROR only i386 is supported}
{$ENDIF}

interface

uses 
  {$IFNDEF DisableCWString}cwstring,{$ENDIF}
  InterfaceBase;

implementation

uses 
  CarbonInt, Forms;

initialization
  CreateWidgetset(TCarbonWidgetSet);

finalization
  FreeWidgetSet;

end.
