{ $Id$}
{
 *****************************************************************************
 *                           gtk2wspairsplitter.pp                           * 
 *                           ---------------------                           * 
 *                                                                           *
 *                                                                           *
 *****************************************************************************

 *****************************************************************************
 *                                                                           *
 *  This file is part of the Lazarus Component Library (LCL)                 *
 *                                                                           *
 *  See the file COPYING.LCL, included in this distribution,                 *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************
}
unit gtk2wspairsplitter;

{$mode objfpc}{H+}

interface

uses
////////////////////////////////////////////////////
// I M P O R T A N T                                
////////////////////////////////////////////////////
// To get as litle as posible circles,
// Uncomment only when needed for registration
////////////////////////////////////////////////////
//  pairsplitter,
////////////////////////////////////////////////////
  wspairsplitter, wslclclasses;

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

  { TGtk2WSPairSplitter }

  TGtk2WSPairSplitter = class(TWSPairSplitter)
  private
  protected
  public
  end;


implementation

initialization

////////////////////////////////////////////////////
// I M P O R T A N T
////////////////////////////////////////////////////
// To improve speed, register only classes
// which actually implement something
////////////////////////////////////////////////////
//  RegisterWSComponent(TPairSplitterSide, TGtk2WSPairSplitterSide);
//  RegisterWSComponent(TCustomPairSplitter, TGtk2WSCustomPairSplitter);
//  RegisterWSComponent(TPairSplitter, TGtk2WSPairSplitter);
////////////////////////////////////////////////////
end.
