{
 /***************************************************************************
                               Toolwin.pp
                               ----------
                     Component Library ToolWindow Controls
                   Initial Revision  : THU Dec 9th 11:00am CST


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

{
@author(TToolWindow - Author Name <smiller@lakefield.net>)                       
@created(08-DEC-1999)
@lastmod(08-DEC-1999)

Detailed description of the Unit.
}

unit Toolwin;

{$mode objfpc}{$H+}

interface

{$ifdef Trace}
  {$ASSERTIONS ON}
{$endif}

uses
  Classes, LCLProc, Controls, SysUtils, GraphType, Graphics, LCLType, LCLIntf,
  StdCtrls;

type

  { TToolWindow }

  TEdgeBorder = (ebLeft, ebTop, ebRight, ebBottom);
  TEdgeBorders = set of TEdgeBorder;

  TEdgeStyle = (esNone, esRaised, esLowered);

  TToolWindow = class(TCustomControl)
  private
    FEdgeBorders: TEdgeBorders;
    FEdgeInner: TEdgeStyle;
    FEdgeOuter: TEdgeStyle;
    procedure SetEdgeBorders(Value: TEdgeBorders);
    procedure SetEdgeInner(Value: TEdgeStyle);
    procedure SetEdgeOuter(Value: TEdgeStyle);
  protected
    procedure AdjustClientRect(var ARect: TRect); override;
    procedure Paint; override;
  public
    constructor Create(TheOwner: TComponent); override;
  public
    property EdgeBorders: TEdgeBorders read FEdgeBorders write SetEdgeBorders default [ebLeft, ebTop, ebRight, ebBottom];
    property EdgeInner: TEdgeStyle read FEdgeInner write SetEdgeInner default esRaised;
    property EdgeOuter: TEdgeStyle read FEdgeOuter write SetEdgeOuter default esLowered;
  end;

implementation

{$I toolwindow.inc}

end.
