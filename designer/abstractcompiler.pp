{
 /***************************************************************************
                               AbstractCompiler.pp
                             -------------------

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
unit AbstractCompiler;

{$mode objfpc}{$H+}

interface

uses
  classes, Controls, forms,buttons,sysutils, Graphics,Extctrls;

type

  TAbstractCompiler = class
   public
     procedure Compile; abstract; virtual; //Starts the compiler
     property Flags: TCompilerFlags; abstract; virtual; //All avalable flags of the compiler
     property OnOutput: TCompilerOutputEvent; abstract; virtual; //Event procedure to get responces back from the compiler
     property FileName: TString; abstract; virtual; //Name of the file to compile
  end;


implementation

end.
