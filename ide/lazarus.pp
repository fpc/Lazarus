{ $Id$ }
{
 /***************************************************************************
                                 Lazarus.pp
                             -------------------
                   This is the lazarus editor program.

                   Initial Revision  : Sun Mar 28 23:15:32 CST 1999


 ***************************************************************************/

 ***************************************************************************
 *                                                                         *
 *   This source is free software; you can redistribute it and/or modify   *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 *   This code is distributed in the hope that it will be useful, but      *
 *   WITHOUT ANY WARRANTY; without even the implied warranty of            *
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU     *
 *   General Public License for more details.                              *
 *                                                                         *
 *   A copy of the GNU General Public License is available on the World    *
 *   Wide Web at <http://www.gnu.org/copyleft/gpl.html>. You can also      *
 *   obtain it by writing to the Free Software Foundation,                 *
 *   Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.        *
 *                                                                         *
 ***************************************************************************
}

program Lazarus;

{$mode objfpc}{$H+}

{$I ide.inc}

{$IFDEF WIN32}
  {$R *.res}
{$ENDIF}

{ $DEFINE IDE_MEM_CHECK}

uses
  //cmem,
  {$IFDEF IDE_MEM_CHECK}
  MemCheck,
  {$ENDIF}
  Interfaces,
  Forms,
  Splash,
  Main,
  // use the custom IDE static packages AFTER 'main'
  {$IFDEF AddStaticPkgs}
  {$I staticpackages.inc}
  {$ENDIF}
  MainBase, compileroptionsdlg;
  
begin
  Application.Initialize;
  TMainIDE.ParseCmdLineOptions;

  // Show splashform
  if ShowSplashScreen then begin
    SplashForm := TSplashForm.Create(nil);
    with SplashForm do begin
      Show;
      Paint;
    end;
    Application.ProcessMessages; // process splash paint message
  end;

  MainIDE:=TMainIDE.Create(Application);
  MainIDE.CreateOftenUsedForms;
  {$IFDEF IDE_MEM_CHECK}
  CheckHeapWrtMemCnt('lazarus.pp: TMainIDE created');
  {$ENDIF}

  if (SplashForm<>nil) then
    SplashForm.StartTimer;
  try
    Application.Run;
  except
    writeln('lazarus.pp - unhandled exception');
  end;
  if (SplashForm<>nil) then begin
    SplashForm.Free;
    SplashForm:=nil;
  end;

  writeln('LAZARUS END - cleaning up ...');

  // free the IDE, so everything is freed before the finalization sections
  MainIDE.Free;
  MainIDE:=nil;
end.


{
  $Log$
  Revision 1.60  2004/09/04 21:54:08  marc
  + Added option to skip compiler step on compile, build or run
  * Fixed adding of runtime watches
  * Fixed runnerror reporting (correct number and location is shown)

  Revision 1.59  2004/08/08 18:02:44  mattias
  splitted TMainIDE (main control instance) and TMainIDEBar (IDE menu and palette), added mainbase.pas and mainintf.pas

  Revision 1.58  2004/04/24 21:00:41  mattias
  clean up

  Revision 1.57  2004/04/24 20:57:18  mattias
  fixed gtk2 for drawing list item  from Ladislav

  Revision 1.56  2004/04/09 10:32:02  mattias
  FindInFilesDialog size is now stored

  Revision 1.55  2004/04/08 18:27:51  mattias
  fixed memleak in TDefaultComponentEditor.Edit

  Revision 1.54  2004/02/27 00:42:41  marc
  * Interface CreateComponent splitup
  * Implemented CreateButtonHandle on GTK interface
    on win32 interface it still needs to be done
  * Changed ApiWizz to support multilines and more interfaces

  Revision 1.53  2004/01/04 03:53:35  marc
  * Changed TComponentSelectionList to TPersistentSelectionList
  + Added SSHdebugger property

  Revision 1.52  2003/08/18 14:40:29  mattias
  deactivated memcheck

  Revision 1.51  2003/08/18 13:21:23  mattias
  renamed lazqueue to lazlinkedlist, patch from Jeroen

  Revision 1.50  2003/08/08 07:52:33  mattias
  deactivated memcheck

  Revision 1.49  2003/08/08 07:49:56  mattias
  fixed mem leaks in debugger

  Revision 1.48  2003/07/24 08:52:46  marc
  + Added SSHGDB debugger

  Revision 1.47  2003/07/24 08:47:36  marc
  + Added SSHGDB debugger

  Revision 1.46  2003/06/23 09:42:09  mattias
  fixes for debugging lazarus

  Revision 1.45  2003/05/30 12:41:46  mattias
  added checks and texts for mixing gtk1 and gtk2

  Revision 1.44  2003/05/30 08:10:51  mattias
  added try except to Application.Run, message on changing debugger items during compile

  Revision 1.43  2003/04/26 07:34:54  mattias
  implemented custom package initialization

  Revision 1.42  2003/04/21 16:21:28  mattias
  implemented default package for custom IDE components

  Revision 1.41  2003/04/18 15:32:51  mattias
  implemented file reference list

  Revision 1.40  2003/04/08 09:04:07  mattias
  fixed registration for fpc 1.0.x

  Revision 1.39  2002/10/26 15:15:42  lazarus
  MG: broke LCL<->interface circles

  Revision 1.38  2002/10/10 20:34:32  lazarus
  MG: fixed freeing Glyph in TBitBtn.SetKind

  Revision 1.37  2002/09/13 08:11:45  lazarus
  MG: fixed memcheck output

  Revision 1.36  2002/07/30 06:24:04  lazarus
  MG: added a faster version of TXMLConfig

  Revision 1.35  2002/06/09 07:08:41  lazarus
  MG: fixed window jumping

  Revision 1.34  2002/05/27 17:58:40  lazarus
  MG: added command line help

  Revision 1.33  2002/05/15 05:58:16  lazarus
  MG: added TMainMenu.Parent

  Revision 1.32  2002/05/10 06:57:42  lazarus
  MG: updated licenses

  Revision 1.31  2002/05/08 14:45:55  lazarus

     New About Dialog Window added; Splash screen modified to stay visible
     longer.  MAH

  Revision 1.30  2002/03/30 07:29:15  lazarus
  MG: fixed splash screen, fixed parser of resource strings

  Revision 1.29  2002/03/27 09:18:11  lazarus
  MG: splitted main.pp: TMainIDE has now an ancestor TMainIDEBar

  Revision 1.28  2002/02/25 16:48:10  lazarus
  MG: new IDE window layout system

  Revision 1.27  2002/01/29 18:55:27  lazarus
  Keith: Fixed duplicate compiler options on Win32

  Revision 1.26  2001/12/11 16:51:36  lazarus
  Modified the Watches dialog
  Shane

  Revision 1.25  2001/12/07 20:12:13  lazarus
  Added a watch dialog.
  Shane

  Revision 1.24  2001/11/06 16:42:23  lazarus
  MG: added facade for find in files

  Revision 1.23  2001/10/26 20:36:48  lazarus
  Added an OnSelectionChanged event in Main.pp fired by MSgView dialog.  This fires when the ListBox gets clicked on.
  This allows the editor to highlight different lines when you click on different error messages.
  Shane

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


