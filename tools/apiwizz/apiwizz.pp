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
@author(M. Weustink <marc@dommelstein.net>)                       
@created(02-May-2000)
}
program APIWizz;

uses
  Interfaces,
  Forms,
  APIWizard;

begin
  Application.Initialize;
  Application.CreateForm(TApiWizForm, ApiWizForm);
  Application.Run;
end.
