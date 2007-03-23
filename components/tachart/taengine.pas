{
 /***************************************************************************
                               TAEngine.pp
                               ----------
          Component Library Standard Graph Helper Functions / Classes


 ***************************************************************************/

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
unit TAEngine;

interface

uses Classes;

type
     //not completetly implemented (only TPieSeries - not all)
     TSeriesMarksStyle=( smsValue,          { 1234 }
                      smsPercent,        { 12 % }
                      smsLabel,          { Cars }
                      smsLabelPercent,   { Cars 12 % }
                      smsLabelValue,     { Cars 1234 }
                      smsLegend,         { ? }
                      smsPercentTotal,   { 12 % of 1234 }
                      smsLabelPercentTotal, { Cars 12 % of 1234 }
                      smsXValue);        { 21/6/1996 }


    TSeriesList = class(TList)

    private

//        function Add(Item: TASeries): Integer; override;

    end;

implementation
 
end.

