{
 *****************************************************************************
 *                                                                           *
 *  See the file COPYING.modifiedLGPL, included in this distribution,        *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************

  Author: Mattias Gaertner
  
  Abstract:
    Interface to various IDE tools manipulating text.
}
unit TextTools;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms;

type
  TSortDirection = (sdAscending, sdDescending);
  TSortDomain = (sdWords, sdLines, sdParagraphs);

  TShowSortSelectionDialogFunc = function(const TheText: string;
    Highlighter: TObject; var SortedText: string): TModalResult;
  TSortTextFunc = function(const TheText: string; Direction: TSortDirection;
    Domain: TSortDomain; CaseSensitive, IgnoreSpace: boolean): string;

var
  ShowSortSelectionDialogFunc: TShowSortSelectionDialogFunc;
  SortTextFunc: TSortTextFunc;

implementation

end.

