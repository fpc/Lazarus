{
 /***************************************************************************
                                  rubberband.pas
                                  ----------
                Component Library TCustomRubberBand, TRubberBand Controls
                   Initial Revision  : Wed Aug 5 09:27:00 GMT+07 2007


 ***************************************************************************/

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

unit RubberBand;

{$mode objfpc}{$H+}

interface

uses
  Classes, Controls;

type
  TRubberBandShape =
  (
    rbsLine,
    rbsRectangle
  );
  
  { TCustomRubberBand }

  TCustomRubberBand = class(TWinControl)
  private
    FShape: TRubberBandShape;
    function GetShape: TRubberBandShape;
    procedure SetShape(const AValue: TRubberBandShape);
  protected
    class procedure WSRegisterClass; override;
  public
    constructor Create(AOwner: TComponent); override;
    property Shape: TRubberBandShape read GetShape write SetShape default rbsLine;
  end;
  
  TRubberBand = class(TCustomRubberBand)
  end;

implementation

uses
  WSDesigner;

{$I rubberband.inc}

end.

