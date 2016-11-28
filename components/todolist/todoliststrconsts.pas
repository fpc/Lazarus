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
unit ToDoListStrConsts;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

resourcestring
  lisTodolistRefresh='Refresh todo items';
  lisTodoListGotoLine='Goto selected source line';
  lisCTInsertMacro = 'Insert Macro';
  lisToDoLDone = 'Done';
  lisToDoLDescription = 'Description';
  lisToDoLPriority = 'Priority';
  lisToDoLFile  = 'Module';
  lisToDoLLine  = 'Line';
  lisToDoLOwner = 'Owner';
  listToDoLCategory = 'Category';
  lisToDoGoto = 'Goto';
  lisToDoExport = 'Export';
  lisOptions = 'Options';
  lisToDoListed = 'Listed';
  lisToDoListedHint = 'Add units listed in project inspector/package editor';
  lisToDoUsed = 'Used';
  lisToDoUsedHint = 'Add units used by main source file';
  lisPackages = 'Packages';
  lisPackagesHint = 'Extends "%s" and "%s" options by units from used packages';
  lisSourceEditor = 'Editor';
  lisSourceEditorHint = 'Add units in source editor';
  dlgUnitDepRefresh = 'Refresh';
  lisTDDInsertToDo = 'Insert ToDo';
  lisViewToDoList = 'View ToDo List';
  lisToDoList = 'ToDo List';
  lisPkgFileTypeText = 'Text';
  dlgFilterCsv = 'CSV files';

implementation

end.

