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
    FIsFocusedFunc: TBoolFunc;
    FOnDesignerSetFocus: TProcedureOfObject;
  public
    constructor Create;
    function IsFocused: Boolean; virtual; abstract;
    procedure Abort; virtual; abstract;
    procedure BeginUpdate; virtual; abstract;
    procedure EndUpdate; virtual; abstract;
    procedure Invalidate; virtual; abstract;
    procedure Refresh; virtual; abstract;
  public
    property IsFocusedFunc: TBoolFunc read FIsFocusedFunc write FIsFocusedFunc;
    property OnDesignerSetFocus: TProcedureOfObject read FOnDesignerSetFocus write FOnDesignerSetFocus;
  end;

implementation

{ TBasicAnchorDesigner }

constructor TBasicAnchorDesigner.Create;
begin
  FIsFocusedFunc := nil;
end;

end.

