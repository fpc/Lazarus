{ $Id$}
{
 *****************************************************************************
 *                          win32wspairsplitter.pp                           * 
 *                          ----------------------                           * 
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
unit win32wspairsplitter;

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

  { TWin32WSPairSplitterSide }

  TWin32WSPairSplitterSide = class(TWSPairSplitterSide)
  private
  protected
  public
  end;

  { TWin32WSCustomPairSplitter }

  TWin32WSCustomPairSplitter = class(TWSCustomPairSplitter)
  private
  protected
  public
  end;

  { TWin32WSPairSplitter }

  TWin32WSPairSplitter = class(TWSPairSplitter)
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
//  RegisterWSComponent(TPairSplitterSide, TWin32WSPairSplitterSide);
//  RegisterWSComponent(TCustomPairSplitter, TWin32WSCustomPairSplitter);
//  RegisterWSComponent(TPairSplitter, TWin32WSPairSplitter);
////////////////////////////////////////////////////
end.
