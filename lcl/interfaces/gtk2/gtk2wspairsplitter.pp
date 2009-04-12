{ $Id$}
{
 *****************************************************************************
 *                           Gtk2WSPairSplitter.pp                           * 
 *                           ---------------------                           * 
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
unit Gtk2WSPairSplitter;

{$mode objfpc}{$H+}

interface

uses
////////////////////////////////////////////////////
// I M P O R T A N T                                
////////////////////////////////////////////////////
// To get as little as posible circles,
// uncomment only when needed for registration
////////////////////////////////////////////////////
//  PairSplitter,
////////////////////////////////////////////////////
  WSPairSplitter, WSLCLClasses;

type

  { TGtk2WSPairSplitterSide }

  TGtk2WSPairSplitterSide = class(TWSPairSplitterSide)
  private
  protected
  public
  end;

  { TGtk2WSCustomPairSplitter }

  TGtk2WSCustomPairSplitter = class(TWSCustomPairSplitter)
  private
  protected
  public
  end;


implementation

end.
