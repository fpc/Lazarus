{******************************************************************************
                                  TNotebook
 ******************************************************************************

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
{------------------------------------------------------------------------------
  TNotebook Constructor
 ------------------------------------------------------------------------------}
constructor TNotebook.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  TabPosition := tpTop;
  ShowTabs := True;
  Setbounds(1,1,200,200);
end;

{------------------------------------------------------------------------------
  TNotebook Destructor
 ------------------------------------------------------------------------------}
destructor TNotebook.Destroy;
begin
  inherited Destroy;
end;

{
  $Log$
  Revision 1.2  2002/05/10 06:05:55  lazarus
  MG: changed license to LGPL

  Revision 1.1  2000/07/13 10:28:27  michael
  + Initial import

  Revision 1.2  2000/06/16 19:11:00  lazarus
  Added TSpinedit and TNotebooK
  Shane

  Revision 1.1  2000/04/02 20:49:56  lazarus
  MWE:
    Moved lazarus/lcl/*.inc files to lazarus/lcl/include

  Revision 1.7  2000/01/04 19:19:56  lazarus
  Modified notebook.inc so it works.  Don't need tabnotbk.pp anymore...

  Shane

  Revision 1.6  1999/09/30 21:59:03  lazarus
  MWE: Fixed TNoteBook problems
       Modifications: A few
       - Removed some debug messages
       + Added some others
       * changed fixed widged of TPage. Code is still broken.
       + TWinControls are also added to the Controls collection
       + Added TControl.Controls[] property

  Revision 1.5  1999/09/22 19:09:18  lazarus
  Added some trace info for the TNotebook problem.

  Revision 1.4  1999/08/04 05:22:23  lazarus
  Moved TCustomNotebook code to customnotebook.inc unit. Kept methods
  specific to TNotebook in this unit.          CAW

}

