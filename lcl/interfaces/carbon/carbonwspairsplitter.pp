{ $Id$}
{
 *****************************************************************************
 *                            CarbonWSPairSplitter.pp                            * 
 *                            -------------------                            * 
 *                                                                           *
 *                                                                           *
 *****************************************************************************

 *****************************************************************************
 *                                                                           *
 *  This file is part of the Lazarus Component Library (LCL)                 *
 *                                                                           *
 *  See the file COPYING.modifiedLGPL, included in this distribution,        *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************
}
unit CarbonWSPairSplitter;

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

  { TCarbonWSPairSplitterSide }

  TCarbonWSPairSplitterSide = class(TWSPairSplitterSide)
  private
  protected
  public
  end;

  { TCarbonWSCustomPairSplitter }

  TCarbonWSCustomPairSplitter = class(TWSCustomPairSplitter)
  private
  protected
  public
  end;

  { TCarbonWSPairSplitter }

  TCarbonWSPairSplitter = class(TWSPairSplitter)
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
//  RegisterWSComponent(TPairSplitterSide, TCarbonWSPairSplitterSide);
//  RegisterWSComponent(TCustomPairSplitter, TCarbonWSCustomPairSplitter);
//  RegisterWSComponent(TPairSplitter, TCarbonWSPairSplitter);
////////////////////////////////////////////////////
end.