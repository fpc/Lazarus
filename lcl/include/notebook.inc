{%MainUnit ../extctrls.pp}
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

function TNotebook.GetActiveNotebookPageComponent: TPage;
begin
  Result:=TPage(GetActiveNotebookPageComponent);
end;

function TNotebook.GetNoteBookPage(Index: Integer): TPage;
begin
  Result:=TPage(GetPage(Index));
end;

procedure TNotebook.SetActiveNotebookPageComponent(const AValue: TPage);
begin
  SetActivePageComponent(AValue);
end;

{------------------------------------------------------------------------------
  TNotebook Constructor
 ------------------------------------------------------------------------------}
constructor TNotebook.Create(TheOwner: TComponent);
begin
  PageClass:=TPage;
  inherited Create(TheOwner);
end;

{
  $Log$
  Revision 1.6  2004/04/10 17:58:57  mattias
  implemented mainunit hints for include files

  Revision 1.5  2003/09/20 13:27:49  mattias
  varois improvements for ParentColor from Micha

  Revision 1.4  2003/08/26 14:33:40  mattias
  implemented component tree for OI

  Revision 1.3  2003/08/20 15:06:57  mattias
  implemented Build+Run File

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

