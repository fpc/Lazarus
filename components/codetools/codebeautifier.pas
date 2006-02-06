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

  Abstract:
    Functions to beautify code.
    Goals:
      - Highly customizable.
      - Beautification of whole sources. For example a unit, or several
        sources.
      - Beautification of parts of sources. For example selections.
      - Beautification of insertion source. For example beautifying code, that
        will be inserted in another source.
      - Working with syntax errors. The beautification will try its best to
        work, even if the source contains errors.
      - Does not ignore comments and directives
      - Contexts: statements, declarations

  Examples for beautification styles:

    if expr then
    begin
      ;
    end;

    if expr then
      ...
    else
      ...;
}
unit CodeBeautifier;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils; 
  
type
  TBeautifySplit =(
    bsNone,
    bsInsertSpace, // insert space before
    bsNewLine,     // break line, no indent
    bsEmptyLine,   // insert empty line, no indent
    bsNewLineAndIndent, // break line, indent
    bsEmptyLineAndIndent, // insert empty line, indent
    bsNewLineUnindent,
    bsEmptyLineUnindent,
    bsNoSplit   // do not break line here when line too long
    );
    
  TWordPolicy = (
    wpNone,
    wpLowerCase,
    wpUpperCase,
    wpLowerCaseFirstLetterUp
    );


implementation

end.

