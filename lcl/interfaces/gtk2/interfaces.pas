{ 
 /*************************************************************************** 
              Interfaces.pp  -  determines what interface to use
              --------------------------------------------------
 
                   Initial Revision  : Thu July 1st CST 1999 
 
 
 ***************************************************************************/ 
 
 *****************************************************************************
  This file is part of the Lazarus Component Library (LCL)

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************
}
unit Interfaces;
 
{$mode objfpc}{$H+} 

interface

{$IFDEF UNIX}{$IFNDEF DisableCWString}uses cwstring;{$ENDIF}{$ENDIF}

implementation

uses
  {$IFNDEF EnableLibOverlay}
  gtk2DisableLibOverlay,
  {$ENDIF}
  Gtk2Int, Forms;

initialization
  CreateWidgetset(TGtk2WidgetSet);

finalization
  FreeWidgetSet;

end.
