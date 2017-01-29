{
 /***************************************************************************
                            DebuggerStrConst.pp
                          -----------------------
   This unit contains resource strings for the generic parts of the debugger


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
 *   Inc., 51 Franklin Street - Fifth Floor, Boston, MA 02110-1335, USA.   *
 *                                                                         *
 ***************************************************************************
}
{
  Note: All resource strings should be prefixed with 'drs' (Debugger Resource String)

}
unit DebuggerStrConst;

{$mode objfpc}{$H+}

interface

resourcestring

  drsColWidthName        = 'Name column';
  drsColWidthExpression  = 'Expression column';
  drsColWidthValue       = 'Value column';
  drsColWidthState       = 'State column';
  drsColWidthIndex       = 'Index column';
  drsColWidthSource      = 'Source column';
  drsColWidthLine        = 'Line column';
  drsColWidthFunc        = 'Function name column';
  drsColWidthBrkPointImg = 'Break indication column';

  drsWatchSplitterInspect = 'Inspect pane';

  drsBreakPointColWidthFile      = 'File/address column';
  drsBreakPointColWidthLine      = 'Line column';
  drsBreakPointColWidthCondition = 'Condition column';
  drsBreakPointColWidthAction    = 'Action column';
  drsBreakPointColWidthPassCount = 'Pass-count column';
  drsBreakPointColWidthGroup     = 'Group column';

  drsHistoryColWidthCurrent  = 'Current column';
  drsHistoryColWidthTime     = 'Time column';
  drsHistoryColWidthLocation = 'Location column';

  drsInspectColWidthDataName = 'Data name column';
  drsInspectColWidthDataType = 'Data type column';
  drsInspectColWidthDataValue = 'Data value column';
  drsInspectColWidthDataClass = 'Data class column';
  drsInspectColWidthDataVisibility = 'Data visibility column';
  drsInspectColWidthMethName = 'Method name column';
  drsInspectColWidthMethType = 'Method type column';
  drsInspectColWidthMethReturns = 'Method returns column';
  drsInspectColWidthMethAddress = 'Method address column';
  drsEvalHistoryNone      = 'No history kept';
  dsrEvalHistoryUp        = 'Insert result at top of history';
  dsrEvalHistoryDown      = 'Append result at bottom of history';

  drsUseInstanceClassType = 'Use Instance class type';
  drsLen = 'Len=%d: ';
  synfNewValueIsEmpty = '"New value" is empty.';
  synfTheDebuggerWasNotAbleToModifyTheValue = 'The debugger was not able to modify the value.';


implementation

end.

