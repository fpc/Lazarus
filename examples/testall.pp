{
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
program TestAll;

{$mode objfpc}{$H+}

uses Interfaces, Forms, TestAllForm;

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
{
  $Log$
  Revision 1.3  2002/10/29 08:22:32  lazarus
  MG: added interfaces unit

  Revision 1.2  2002/05/10 06:57:50  lazarus
  MG: updated licenses

  Revision 1.1  2000/07/31 20:33:33  lazarus
  + added "testall" demo provided by <christer.t.johansson@se.abb.com>
  stoppok

} 
