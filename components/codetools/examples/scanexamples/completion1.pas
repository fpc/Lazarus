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

  Author: Mattias Gaertner
  
  This is an example unit to demonstrate some features of the code completion.
}
unit Completion1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils; 

implementation

procedure DoSomething;
begin
  // put the cursor at the beginning of this comment and code completion will
  // add DoSomething to the interface
  
  Str:='Path'+PathDelim; // put the cursor on 'Str' and code completion will
            // insert a local variable var Str: String in front of the 'begin'

  NewProcedure(12345,LineEnding,PathDelim); // put the cursor on 'NewProcedure' and code completion
            // will create a new procedure
end;

end.

