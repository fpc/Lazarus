{
 *****************************************************************************
  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************

 Authors: Michael W. Vogel

}

unit DockedBasicAnchorDesigner;

{$mode objfpc}{$H+}

interface

uses
  // RTL, FCL
  Classes, SysUtils,
  // LCL
  LCLProc, Forms, Controls,
  // IDEIntf
  ComponentEditors;

type

  TBoolFunc = function: Boolean of object;

  { TBasicAnchorDesigner }

  TBasicAnchorDesigner = class
  private
    FOnDesignerSetFocus: TProcedureOfObject;
    FOnMouseWheel: TMouseWheelEvent;
  public
    procedure Abort; virtual; abstract;
    procedure BeginUpdate; virtual; abstract;
    procedure EndUpdate; virtual; abstract;
    procedure Invalidate; virtual; abstract;
    procedure Refresh; virtual; abstract;
  public
    property OnDesignerSetFocus: TProcedureOfObject read FOnDesignerSetFocus write FOnDesignerSetFocus;
    property OnMouseWheel: TMouseWheelEvent read FOnMouseWheel write FOnMouseWheel;
  end;

implementation

end.

