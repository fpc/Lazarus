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
  csvHeader = 'Type,Description,Priority,Module,Line,Owner,Category';

  lisToDo  = 'ToDo';
  lisFixMe = 'FixMe';
  lisDone  = 'Done';
  lisNote  = 'Note';

resourcestring
  lisTodolistEdit='Edit selected item';
  lisTodolistRefresh='Refresh ToDo items';
  lisTodoListGotoLine='Goto selected source line';
  lisColorOptions = 'Color Options';
  lisCTInsertMacro = 'Insert Macro';
  lisToDoLType = 'Type';
  lisToDoLDescription = 'Description';
  lisToDoLPriority = 'Priority';
  lisToDoLFile  = 'Module';
  lisToDoLLine  = 'Line';
  lisToDoLOwner = 'Owner';
  listToDoLCategory = 'Category';
  lisEdit = 'Edit';
  lisToDoGoto = 'Goto';
  lisToDoExport = 'Export';
  lisColors = 'Colors';
  lisHelp = 'Help';
  lisToDoListed = 'Listed files';
  //lisToDoListedHint = 'Add files listed in project inspector/package editor';
  lisToDoUsedUnits = 'Used units';
  //lisToDoUsedUnitsHint = 'Add units used by main source file';
  lisSourceEditor = 'Source editor files';
  //lisSourceEditorHint = 'Add files in source editor';
  lisPackages = 'Used packages';
  //lisPackagesHint = 'Extends "%s" and "%s" options by units from used packages';
  dlgUnitDepRefresh = 'Refresh';
  lisTDDInsertToDo = 'Insert a ToDo item';
  lisTDDEditToDo = 'Edit the ToDo item';
  lisTDDInsertToDoDlgCaption = 'Insert a ToDo item';
  lisTDDEditToDoDlgCaption = 'Edit the ToDo item';
  lisViewToDoList = 'View ToDo List';
  lisToDoList = 'ToDo List';
  lisPkgFileTypeText = 'Text';
  dlgFilterCsv = 'CSV files';
  lisShowTypes = 'Show Types';
  lisShowTypesHint = 'Select which ToDo types to list';
  lisShowFiles = 'Show Files';
  lisShowFilesHint = 'Select which Pascal units and .todo files to list';
  lisToDoItems = '%d items';
  lisToDoToDoType = 'ToDo type';
  lisAlternateTokens = 'Use Alternate Tokens';
  lisAlternateTokensHint = 'Standard token is #todo etc. Alternate is without the #';

  errScanfileFailed = 'Finding ToDo items failed in file %0:s.' + LineEnding +
                       'Is it a valid source file?' + LineEnding +
                       'The rest of the file has been skipped.';
  rsExportTodoIt = 'Export ToDo items to CSV';

  AttribGroupName = 'ToDo comments';
  AttribNameTodo  = 'ToDo comment';
  AttribNameFixMe = 'FixMe comment';
  AttribNameDone  = 'Done comment';
  AttribNameNote  = 'Note comment';

  lisToDoColorsAreAtTheEndOfColorList = 'ToDo colors are at the end of color list';

implementation

end.

