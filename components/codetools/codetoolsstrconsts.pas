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
    This unit contains all resource strings for the codetools.

}
unit CodeToolsStrConsts;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils; 
  
ResourceString
  ctsUnknownSubDescriptor = '(unknown subdescriptor %s)';
  ctsForward = 'Forward';
  ctsUnparsed = 'Unparsed';
  
  ctsIdentExpectedButAtomFound = 'identifier expected, but %s found';
  ctsIdentExpectedButKeyWordFound = 'identifier expected, but keyword %s found';
  ctsCharExpectedButAtomFound = '%s expected, but %s found';
  
  ctsIdentExpectedButEOFFound = 'unexpected end of file (identifier expected)';
  ctsBracketOpenExpectedButAtomFound = 'bracket open expected, but %s found';
  ctsBracketCloseExpectedButAtomFound = 'bracket close expected, but %s found';
  ctsBracketNotFound = 'bracket %s not found';
  ctsNoNodeFoundAtCursor = 'no node found at cursor';
  ctsUnknownMainFilename = '(unknown mainfilename)';
  
  ctsPropertySpecifierAlreadyDefined = 'property specifier already defined: %s';
  ctsErrorInParamList = 'error in paramlist';
  ctsPropertTypeExpectedButAtomFound = 'property type expected, but %s found';
  ctsIndexSpecifierRedefined = 'index specifier redefined';
  ctsIndexParameterExpectedButAtomFound = 'index parameter expected, but %s found';
  ctsDefaultSpecifierRedefined = 'default specifier redefined';
  ctsDefaultParameterExpectedButAtomFound = 'default parameter expected, but %s found';
  ctsNodefaultSpecifierDefinedTwice = 'nodefault specifier defined twice';
  

implementation

end.

