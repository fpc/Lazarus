{  $Id$  }
{
 /***************************************************************************
                             intfstrconsts.pas
                          -----------------------
           This unit contains all resource strings of the interface


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
{
  Note: All resource strings should be prefixed with 'ifs' (InterFace String)

}
unit IntfStrConsts;

{$mode objfpc}{$H+}

interface

resourcestring

  // I'm not sure if in all languages the Dialog texts for a button
  // have the same meaning as a key
  // So every VK gets its own constant
  ifsVK_UNKNOWN    = 'Unknown';
  ifsVK_LBUTTON    = 'Mouse Button Left';
  ifsVK_RBUTTON    = 'Mouse Button Right';
  ifsVK_CANCEL     = 'Cancel'; //= dlgCancel
  ifsVK_MBUTTON    = 'Mouse Button Middle';
  ifsVK_BACK       = 'Backspace';
  ifsVK_TAB        = 'Tab';
  ifsVK_CLEAR      = 'Clear';
  ifsVK_RETURN     = 'Return';
  ifsVK_SHIFT      = 'Shift';
  ifsVK_CONTROL    = 'Control';
  ifsVK_MENU       = 'Menu';
  ifsVK_PAUSE      = 'Pause key';
  ifsVK_CAPITAL    = 'Capital';
  ifsVK_KANA       = 'Kana';
  ifsVK_JUNJA      = 'Junja';
  ifsVK_FINAL      = 'Final';
  ifsVK_HANJA      = 'Hanja';
  ifsVK_ESCAPE     = 'Escape';
  ifsVK_CONVERT    = 'Convert';
  ifsVK_NONCONVERT = 'Nonconvert';
  ifsVK_ACCEPT     = 'Accept';
  ifsVK_MODECHANGE = 'Mode Change';
  ifsVK_SPACE      = 'Space key';
  ifsVK_PRIOR      = 'Prior';
  ifsVK_NEXT       = 'Next';
  ifsVK_END        = 'End';
  ifsVK_HOME       = 'Home';
  ifsVK_LEFT       = 'Left';
  ifsVK_UP         = 'Up';
  ifsVK_RIGHT      = 'Right';
  ifsVK_DOWN       = 'Down'; //= dlgdownword
  ifsVK_SELECT     = 'Select'; //= lismenuselect
  ifsVK_PRINT      = 'Print';
  ifsVK_EXECUTE    = 'Execute';
  ifsVK_SNAPSHOT   = 'Snapshot';
  ifsVK_INSERT     = 'Insert';
  ifsVK_DELETE     = 'Delete'; //= dlgeddelete
  ifsVK_HELP       = 'Help';
  ifsVK_LWIN       = 'left windows key';
  ifsVK_RWIN       = 'right windows key';
  ifsVK_APPS       = 'application key';
  ifsVK_NUMPAD     = 'Numpad %d';
  ifsVK_NUMLOCK    = 'Numlock';
  ifsVK_SCROLL     = 'Scroll';

implementation

end.

