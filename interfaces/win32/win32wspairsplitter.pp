{ $Id$}
{
 *****************************************************************************
 *                          Win32WSPairSplitter.pp                           * 
 *                          ----------------------                           * 
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
unit Win32WSPairSplitter;

{$mode objfpc}{$H+}

interface

uses
////////////////////////////////////////////////////
// I M P O R T A N T                                
////////////////////////////////////////////////////
// To get as little as posible circles,
// uncomment only when needed for registration
////////////////////////////////////////////////////
  WSPairSplitter, WSLCLClasses;

type

  { TWin32WSPairSplitterSide }

  TWin32WSPairSplitterSide = class(TWSPairSplitterSide)
  published
  end;

  { TWin32WSCustomPairSplitter }

  TWin32WSCustomPairSplitter = class(TWSCustomPairSplitter)
  published
  end;

implementation

end.
