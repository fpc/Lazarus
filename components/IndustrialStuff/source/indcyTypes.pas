{   Unit indcyTypes from cyTypes

    Description:
    Unit with Types declarations.

    * ***** BEGIN LICENSE BLOCK *****
    *
    * Version: MPL 1.1
    *
    * The contents of this file are subject to the Mozilla Public License Version
    * 1.1 (the "License"); you may not use this file except in compliance with the
    * License. You may obtain a copy of the License at http://www.mozilla.org/MPL/
    *
    * Software distributed under the License is distributed on an "AS IS" basis,
    * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
    * the specific language governing rights and limitations under the License.
    *
    * The Initial Developer of the Original Code is Mauricio
    * (https://sourceforge.net/projects/tcycomponents/).
    *
    * No contributors for now ...
    *
    * Alternatively, the contents of this file may be used under the terms of
    * either the GNU General Public License Version 2 or later (the "GPL"), or the
    * GNU Lesser General Public License Version 2.1 or later (the "LGPL"), in which
    * case the provisions of the GPL or the LGPL are applicable instead of those
    * above. If you wish to allow use of your version of this file only under the
    * terms of either the GPL or the LGPL, and not to allow others to use your
    * version of this file under the terms of the MPL, indicate your decision by
    * deleting the provisions above and replace them with the notice and other
    * provisions required by the LGPL or the GPL. If you do not delete the
    * provisions above, a recipient may use your version of this file under the
    * terms of any one of the MPL, the GPL or the LGPL.
    *
    * ***** END LICENSE BLOCK *****}
    
unit indcyTypes;

{$mode objfpc}{$H+}

interface

uses
  LCLIntf, LCLType, Graphics, Classes, types;

type
  // Graphic:
  TCaptionRender = (crNormal, crPathEllipsis, crEndEllipsis, crWordEllipsis);
  TCaptionOrientation = (coHorizontal, coHorizontalReversed, coVertical, coVerticalReversed);

  TBgPosition = (bgCentered, bgTopLeft, bgTopCenter, bgTopRight, bgCenterRight, bgBottomRight, bgBottomCenter, bgBottomLeft, bgCenterLeft);
  TBgStyle = (bgNone, bgNormal, bgMosaic, bgStretch, bgStretchProportional);

  TcyBevelCut = (bcLowered, bcRaised, bcNone, bcTransparent);

  TDgradOrientation = (dgdVertical, dgdHorizontal, dgdAngle, dgdRadial, dgdRectangle);
  TDgradOrientationShape = (osRadial, osRectangle);
  TDgradBalanceMode = (bmNormal, bmMirror, bmReverse, bmReverseFromColor, bmInvertReverse, bmInvertReverseFromColor);
  
  // Cindy components:
  TRunTimeDesignJob = (rjNothing, rjMove, rjResizeTop, rjResizeBottom, rjResizeLeft, rjResizeTopLeft,
                        rjResizeBottomLeft, rjResizeRight, rjResizeTopRight, rjResizeBottomRight);

  TLineCoord = record
    BottomCoord, TopCoord: TPoint;
  end;

//var
//  CaptionOrientationWarning: Boolean = true;

const
  DT_PATH_ELLIPSIS = $8000;
  DT_WORD_ELLIPSIS = $8000;

  cCaptionOrientationWarning = 'Note that text orientation doesn''t work with all fonts!';

  Alignments: array[TAlignment] of Word = (DT_LEFT, DT_RIGHT, DT_CENTER);
  WordWraps: array[Boolean] of Word = (DT_SINGLELINE, DT_WORDBREAK);
  TextLayouts: array[TTextLayout] of Longint = (DT_TOP, DT_VCENTER, DT_BOTTOM);
  CaptionOrientations: array[TCaptionOrientation] of word = (0, 1800, 900, 2700);
  CaptionRenders: array[TCaptionRender] of Integer = (0, DT_PATH_ELLIPSIS, DT_END_ELLIPSIS, DT_WORD_ELLIPSIS);

implementation


end.
