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
 *   Inc., 51 Franklin Street - Fifth Floor, Boston, MA 02110-1335, USA.   *
 *                                                                         *
 ***************************************************************************
}
unit ToDoListStrConsts;

{$mode objfpc}{$H+}

interface

const
  csvHeader = 'Done,Description,Priority,Module,Line,Owner,Category';

resourcestring
  lisTodolistRefresh='Refresh todo items';
  lisTodoListGotoLine='Goto selected source line';
  lisCTInsertMacro = 'Insert Macro';
  lisToDoLDone = 'Done';
  lisToDoLType = 'Type';
  lisToDoLDescription = 'Description';
  lisToDoLPriority = 'Priority';
  lisToDoLFile  = 'Module';
  lisToDoLLine  = 'Line';
  lisToDoLOwner = 'Owner';
  listToDoLCategory = 'Category';
  lisToDoGoto = 'Goto';
  lisToDoExport = 'Export';
  lisOptions = 'Options';
  lisHelp = 'Help';
  lisToDoListed = 'Listed';
  lisToDoListedHint = 'Add units listed in project inspector/package editor';
  lisToDoUsed = 'Used';
  lisToDoUsedHint = 'Add units used by main source file';
  lisPackages = 'Packages';
  lisPackagesHint = 'Extends "%s" and "%s" options by units from used packages';
  lisSourceEditor = 'Editor';
  lisSourceEditorHint = 'Add units in source editor';
  dlgUnitDepRefresh = 'Refresh';
  lisTDDInsertToDo = 'Insert a ToDo/Done/Note';
  lisViewToDoList = 'View ToDo List';
  lisToDoList = 'ToDo List';
  lisPkgFileTypeText = 'Text';
  dlgFilterCsv = 'CSV files';

  lisFilterItem0 = 'All';
  lisFilterItem1 = 'ToDo Only';
  lisFilterItem2 = 'Done Only';
  lisFilterItem3 = 'Note Only';
  lisFilterItem4 = 'ToDo & Done';
  lisFilterItem5 = 'ToDo & Note';
  lisFilterItem6 = 'Done & Note';

  lisShowWhat = 'Show';
  lisShowWhatHint = 'Select which ToDo types to list';
  lisToDoToDoType = 'ToDo type';
  lisAlternateTokens = 'Use Alternate Tokens';
  lisAlternateTokensHint = 'Standard token is #todo etc. Alternate is without the #';

  excInvalidParseState = 'Invalid parse state.';

  errScanfileFailed = 'Finding ToDo items failed in file %0:s.' + LineEnding +
                       'Is it a valid source file?' + LineEnding +
                       'The rest of the file has been skipped.';
  rsExportTodoIt = 'Export ToDo items to CSV';

implementation

end.

