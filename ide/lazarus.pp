{ $Id$ }
{
 /***************************************************************************
                          Lazarus.pp  -  Main application unit
                             -------------------
                   This unit does nothing more than launch the
                   TConfig Object which is responsible for the
                   creation of everything else.


                   Initial Revision  : Sun Mar 28 23:15:32 CST 1999


 ***************************************************************************/

/***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 ***************************************************************************/
 }

program lazarus;

{$mode objfpc}

uses 
  forms,
  splash,
  main,
  testform,
  compileroptions,
  IdeEditor,
  viewunit_dlg,  //dialog used to list the units in a project
  viewform_dlg;  //dialog to display the forms in the project


var
  SplashForm: TSplashForm;

begin
   Application.Initialize; { calls InitProcedure which starts up GTK }
   
   //Show splashform
   SplashForm := TSplashForm.Create(nil);
   with SplashForm do
   begin
     Show;
     Paint;
   end;

   Application.CreateForm(TForm1, Form1);
   Application.CreateForm(TfrmCompilerOptions, frmCompilerOptions);
   Application.CreateForm(TTestForm, TestForm1);
   Application.CreateForm(TIDEEditor, IdeEditor1);
   Application.CreateForm(TViewUnits1, ViewUnits1);
   Application.CreateForm(TViewForms1, ViewForms1);

   SplashForm.StartTimer;
   Application.Run;
end.


{
  $Log$
  Revision 1.3  2000/08/09 18:32:10  lazarus
  Added more code for the find function.
  Shane

  Revision 1.2  2000/08/08 18:52:14  lazarus
  Started a FIND dialog box.
  Shane

  Revision 1.1  2000/07/13 10:27:47  michael
  + Initial import

  Revision 1.15  2000/03/19 23:01:41  lazarus
  MWE:
    = Changed splashscreen loading/colordepth
    = Chenged Save/RestoreDC to platform  dependent, since they are
      relative to a DC

  Revision 1.14  2000/03/14 05:53:18  lazarus
  Changed the name of the compiler options form.         CAW

  Revision 1.13  2000/02/21 17:38:04  lazarus
  Added modalresult to TCustomForm
  Added a View Units dialog box
  Added a View Forms dialog box
  Added a New Unit menu selection
  Added a New Form menu selection
  Shane

  Revision 1.12  1999/12/08 00:56:06  lazarus
  MWE:
    Fixed menus. Events aren't enabled yet (dumps --> invalid typecast ??)

  Revision 1.11  1999/11/24 18:54:12  lazarus
  Added a unit called ideeditor.pp
  Shane

  Revision 1.10  1999/11/17 01:12:52  lazarus
  MWE:
    Added a TestForm and moved mwEdit to that form. The form popsup after
    pressing the testform buttomn

  Revision 1.9  1999/11/01 01:28:28  lazarus
  MWE: Implemented HandleNeeded/CreateHandle/CreateWND
       Now controls are created on demand. A call to CreateComponent shouldn't
       be needed. It is now part of CreateWnd

  Revision 1.8  1999/09/30 21:59:00  lazarus
  MWE: Fixed TNoteBook problems
       Modifications: A few
       - Removed some debug messages
       + Added some others
       * changed fixed widged of TPage. Code is still broken.
       + TWinControls are also added to the Controls collection
       + Added TControl.Controls[] property

  Revision 1.7  1999/07/10 21:20:21  lazarus

       cleaned up the forms to make them more readable

  Revision 1.6  1999/07/09 13:54:41  lazarus
  Changed to use Dispatch instead of DispatchStr for messaging.
  You pass it LM_Message which is an integer value and therefore you
  can now use Dispatch to send the integer value back to the class.
  There is currently a problem with having multiple "message" procedures
  in one class so I commented them out for now.

  Shane

  Revision 1.5  1999/05/24 21:20:19  lazarus
  *** empty log message ***

  Revision 1.4  1999/05/15 21:15:05  lazarus
  *** empty log message ***

  Revision 1.3  1999/05/14 18:44:13  lazarus
  *** empty log message ***

  Revision 1.2  1999/04/18 05:42:10  lazarus
  *** empty log message ***

  Revision 1.1  1999/04/14 07:31:44  michael
  + Initial implementation

}


