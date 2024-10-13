{ 
 /*************************************************************************** 
                         Interfaces.pp  -  determines what interface to use
                             -------------------

 ******************** *******************************************************/

 *****************************************************************************
  This file is part of the Lazarus Component Library (LCL)

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************
}

unit Interfaces;

{$mode objfpc}{$H+}

interface 

uses
  //{$IFnDEF DisableUTF8RTL}
  //LazUTF8,
  //{$ENDIF}
  InterfaceBase;

implementation

uses
  TestMockInt, Forms;

initialization
  CreateWidgetset(TTestMockWidgetSet);

finalization
  FreeWidgetSet;

end.
