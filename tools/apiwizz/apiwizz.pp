{  $Id$  }
{
 /***************************************************************************
                                 APIWiZZ.pp 
                             -------------------
                   APIWiZZ is an API wizard to generate WINAPI
                   Templates for GTK.

                   Initial Revision  : 05-02-2000


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
{
@author(M. Weustink <weus@quicknet.nl>)                       
@created(02-May-2000)
}
program APIWizz;

uses
  Interfaces,
  Forms,
  APIWizard;

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
{ =============================================================================

  $Log$
  Revision 1.2  2003/01/17 16:28:42  mattias
  updated translation files

  Revision 1.1  2000/07/13 10:28:31  michael
  + Initial import

  Revision 1.2  2000/05/03 21:48:45  lazarus
  MWE:
    * Fixed wizard  typo
    + added phony entries to makefile

  Revision 1.1  2000/05/03 00:27:06  lazarus
  MWE:
    + First rollout of the API wizard.

}
