{ $Id$}
{
 *****************************************************************************
 *                            CarbonWSPairSplitter.pp                        * 
 *                            -------------------                            * 
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
  Controls, LCLType, PairSplitter,
////////////////////////////////////////////////////
  WSPairSplitter, WSLCLClasses;

type

  { TCarbonWSPairSplitterSide }

  TCarbonWSPairSplitterSide = class(TWSPairSplitterSide)
  published
    class function  CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): TLCLIntfHandle; override;
  end;

  { TCarbonWSCustomPairSplitter }

  TCarbonWSCustomPairSplitter = class(TWSCustomPairSplitter)
  published
    class function  CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): TLCLIntfHandle; override;
  end;


implementation

uses
  CarbonPrivate;
  
{ TCarbonWSPairSplitterSide }

{------------------------------------------------------------------------------
  Method:  TCarbonWSPairSplitterSide.CreateHandle
  Params:  AWinControl - LCL control
           AParams     - Creation parameters
  Returns: Handle to the control in Carbon interface

  Creates new pair splitter side in Carbon interface with the specified
  parameters
 ------------------------------------------------------------------------------}
class function TCarbonWSPairSplitterSide.CreateHandle(const AWinControl: TWinControl;
  const AParams: TCreateParams): TLCLIntfHandle;
begin
  Result := TLCLIntfHandle(TCarbonCustomControl.Create(AWinControl, AParams));;
end;

{ TCarbonWSCustomPairSplitter }

{------------------------------------------------------------------------------
  Method:  TCarbonWSCustomPairSplitter.CreateHandle
  Params:  AWinControl - LCL control
           AParams     - Creation parameters
  Returns: Handle to the control in Carbon interface

  Creates new pair splitter in Carbon interface with the specified parameters
 ------------------------------------------------------------------------------}
class function TCarbonWSCustomPairSplitter.CreateHandle(const AWinControl: TWinControl;
  const AParams: TCreateParams): TLCLIntfHandle;
begin
  Result := TLCLIntfHandle(TCarbonCustomControl.Create(AWinControl, AParams));;
end;

end.
