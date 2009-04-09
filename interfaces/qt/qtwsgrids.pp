{ $Id$}
{
 *****************************************************************************
 *                               QtWSGrids.pp                                * 
 *                               ------------                                * 
 *                                                                           *
 *                                                                           *
 *****************************************************************************

 *****************************************************************************
 *                                                                           *
 *  This file is part of the Lazarus Component Library (LCL)                 *
 *                                                                           *
 *  See the file COPYING.modifiedLGPL.txt, included in this distribution,    * 
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************
}
unit QtWSGrids;

{$mode objfpc}{$H+}

interface

{$I qtdefines.inc}

uses
  // Bindings
  qt4,
  qtwidgets,
  // LCL
  LCLType, LCLProc, Controls, Grids,
  // Widgetset
  WSGrids, WSLCLClasses;

type

  { TQtWSStringCellEditor }

  TQtWSStringCellEditor = class(TWSStringCellEditor)
  published
  end;

  { TQtWSCustomGrid }

  TQtWSCustomGrid = class(TWSCustomGrid)
  published
  end;

  { TQtWSDrawGrid }

  TQtWSDrawGrid = class(TWSDrawGrid)
  published
  end;

  { TQtWSStringGrid }

  TQtWSStringGrid = class(TWSStringGrid)
  published
  end;


implementation

initialization

////////////////////////////////////////////////////
// I M P O R T A N T
////////////////////////////////////////////////////
// To improve speed, register only classes
// which actually implement something
////////////////////////////////////////////////////
//  RegisterWSComponent(TStringCellEditor, TQtWSStringCellEditor);
//  RegisterWSComponent(TCustomGrid, TQtWSCustomGrid);
//  RegisterWSComponent(TDrawGrid, TQtWSDrawGrid);
//  RegisterWSComponent(TStringGrid, TQtWSStringGrid);
////////////////////////////////////////////////////
end.
