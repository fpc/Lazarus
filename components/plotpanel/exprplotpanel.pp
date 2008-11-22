{ Unit to plot an expression on a canvas

  Copyright (C) 2008 Michael Van Canneyt michael@freepascal.org

  This library is free software; you can redistribute it and/or modify it
  under the terms of the GNU Library General Public License as published by
  the Free Software Foundation; either version 2 of the License, or (at your
  option) any later version with the following modification:

  As a special exception, the copyright holders of this library give you
  permission to link this library with independent modules to produce an
  executable, regardless of the license terms of these independent modules,and
  to copy and distribute the resulting executable under terms of your choice,
  provided that you also meet, for each linked independent module, the terms
  and conditions of the license of that module. An independent module is a
  module which is not derived from or based on this library. If you modify
  this library, you may extend this exception to your version of the library,
  but you are not obligated to do so. If you do not wish to do so, delete this
  exception statement from your version.

  This program is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public License
  for more details.

  You should have received a copy of the GNU Library General Public License
  along with this library; if not, write to the Free Software Foundation,
  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
}

unit exprplotpanel;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, plotpanel, fpexprpars;

Type

  { TExpressionControlPlotter }

  TExpressionControlPlotter = Class(TCanvasPlotter)
  private
    FParser : TFPExpressionParser;
    FX : TFPExprIdentifierDef;
    function GetExpression: String;
    function GetIdentifiers: TFPExprIdentifierDefs;
    procedure SetExpression(const AValue: String);
    procedure SetIdentifiers(const AValue: TFPExprIdentifierDefs);
  Protected
    Function CalcFunction(X : TPlotFloat) : TPlotFloat; override;
    Property Parser : TFPExpressionParser Read FParser;
  Public
    Constructor Create(AOwner : TComponent); override;
    Destructor Destroy; override;
    Property Expression : String Read GetExpression Write SetExpression;
    Property Identifiers : TFPExprIdentifierDefs Read GetIdentifiers Write SetIdentifiers;
  end;

  { TPlotFunctionPanel }

  TPlotExpressionPanel = Class(TCustomPlotFunctionPanel)
  private
    function GetExpression: String;
    function GetIdentifiers: TFPExprIdentifierDefs;
    procedure SetExpression(const AValue: String);
    procedure SetIdentifiers(const AValue: TFPExprIdentifierDefs);
  Protected
    Function CreatePlotter : TCanvasPlotter; override;
  Published
    Property Expression : String Read GetExpression Write SetExpression;
    Property Identifiers : TFPExprIdentifierDefs Read GetIdentifiers Write SetIdentifiers;
    Property Anchors;
    Property Align;
    Property XAxis;
    Property YAxis;
    Property Active;
    Property PlotColor;
  end;

implementation

resourcestring
   SErrInvalidResultType = 'Expression result type must be integer or float. Got %s"';

{ TExpressionControlPlotter }

function TExpressionControlPlotter.GetIdentifiers: TFPExprIdentifierDefs;
begin
  Result:=FParser.Identifiers;
end;

function TExpressionControlPlotter.GetExpression: String;
begin
  Result:=FParser.Expression;
end;

procedure TExpressionControlPlotter.SetExpression(const AValue: String);
begin
  FParser.Expression:=AValue;
  If Not (FParser.ResultType in [rtInteger,rtFLoat]) then
    Raise EExprParser.CreateFmt(SErrInvalidResultType,[ResultTypeName(FParser.ResultType)]);
  Changed;
end;

procedure TExpressionControlPlotter.SetIdentifiers(
  const AValue: TFPExprIdentifierDefs);
begin
  FParser.Identifiers.Assign(AValue);
  // Reassign...
  FX:=FParser.IdentifierByName('X');
end;

function TExpressionControlPlotter.CalcFunction(X: TPlotFloat): TPlotFloat;

Var
  E : TFPExpressionResult;

begin
  FX.AsFloat:=X;
  E:=FParser.Evaluate;
  If E.ResultType=rtFloat then
    result:=E.ResFloat
  else If E.ResultType=rtInteger then
    result:=E.ResInteger;
end;

constructor TExpressionControlPlotter.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FParser:=TFPExpressionParser.Create(Self);
  FX:=FParser.Identifiers.AddFloatVariable('X',0.0);
end;

destructor TExpressionControlPlotter.Destroy;
begin
  FX:=Nil;
  FreeAndNil(FParser);
  inherited Destroy;
end;

{ TPlotExpressionPanel }

function TPlotExpressionPanel.GetExpression: String;
begin
  Result:=TExpressionControlPlotter(Plotter).Expression;
end;

function TPlotExpressionPanel.GetIdentifiers: TFPExprIdentifierDefs;
begin
  Result:=TExpressionControlPlotter(Plotter).Identifiers;
end;

procedure TPlotExpressionPanel.SetExpression(const AValue: String);
begin
  TExpressionControlPlotter(Plotter).Expression:=AValue;
end;

procedure TPlotExpressionPanel.SetIdentifiers(const AValue: TFPExprIdentifierDefs
  );
begin
  TExpressionControlPlotter(Plotter).Identifiers:=AValue;
end;

function TPlotExpressionPanel.CreatePlotter: TCanvasPlotter;
begin
  Result:=TExpressionControlPlotter.Create(Self);
end;

end.

