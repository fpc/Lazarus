{ $Id$ }
{
 /***************************************************************************
                                 Lazarus.pp
                             -------------------
                   This is the lazarus editor program.

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

{$mode objfpc}{$H+}

{$I ide.inc}

{$IFDEF SUPPORTS_RESOURCES}
  {$R *.res}
{$ENDIF}

uses 
{$IFDEF IDE_MEM_CHECK}
  MemCheck,
{$ENDIF}
  Forms,
  Splash,
  Main,
  MsgView,
  FindReplaceDialog;

begin
  Application.Initialize;
    // calls InitProcedure which starts up the interface (e.g. GTK)
   
  // Show splashform
  SplashForm := TSplashForm.Create(nil);
  with SplashForm do begin
    Show;
    Paint;
  end;

  Application.CreateForm(TMainIDE, MainIDE);
{$IFDEF IDE_MEM_CHECK}
CheckHeap('TMainIDE created');
{$ENDIF}
  Application.CreateForm(TMessagesView, MessagesView);
  Application.CreateForm(TLazFindReplaceDialog, FindReplaceDlg);
  SplashForm.StartTimer;
  Application.Run;
  SplashForm.Free;

writeln('LAZARUS END');
end.


{
  $Log$
  Revision 1.22  2001/10/15 17:41:30  lazarus
  MG: fixed splashform showing

  Revision 1.20  2001/08/02 12:58:35  lazarus
  MG: win32 interface patch from Keith Bowes

  Revision 1.19  2001/06/04 07:50:42  lazarus
  MG: close application object in gtkint.pp

  Revision 1.18  2001/03/19 14:00:46  lazarus
  MG: fixed many unreleased DC and GDIObj bugs

  Revision 1.17  2001/03/12 09:34:52  lazarus
  MG: added transfermacros, renamed dlgmessage.pp to msgview.pp

  Revision 1.15  2001/02/22 17:04:57  lazarus
  added environment options + killed ide unit circles

  Revision 1.14  2001/02/21 22:55:24  lazarus
  small bugfixes + added TOIOptions

  Revision 1.13  2001/02/20 16:53:24  lazarus
  Changes for wordcompletion and many other things from Mattias.
  Shane

  Revision 1.12  2001/01/31 13:03:33  lazarus
  Commitng source with new editor.
  Shane

  Revision 1.11  2001/01/16 23:30:45  lazarus
  trying to determine what's crashing LAzarus on load.
  Shane

  Revision 1.9  2001/01/15 18:25:51  lazarus
  Fixed a stupid error I caused by using a variable as an index in main.pp and this variable sometimes caused an exception because the index was out of range.
  Shane

  Revision 1.8  2001/01/13 06:11:06  lazarus
  Minor fixes
  Shane

  Revision 1.7  2001/01/08 23:48:33  lazarus
  MWE:
    ~ Changed makefiles
    ~ Removed testform from lararus and changed it into program
    * some formatting

  Revision 1.6  2001/01/04 20:33:53  lazarus
  Moved lresources.
  Moved CreateLFM to Main.pp
  Changed Form1 and TFOrm1 to MainIDE and TMainIDE
  Shane

  Revision 1.5  2000/12/19 18:43:12  lazarus
  Removed IDEEDITOR.  This causes the PROJECT class to not function.
  Saving projects no longer works.

  I added TSourceNotebook and TSourceEditor.  They do all the work for saving/closing/opening units.  Somethings work but they are in early development.
  Shane

  Revision 1.4  2000/09/10 23:08:29  lazarus
  MWE:
    + Added CreateCompatibeleBitamp function
    + Updated TWinControl.WMPaint
    + Added some checks to avoid gtk/gdk errors
    - Removed no fixed warning from GetDC
    - Removed some output

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


