{
 *****************************************************************************
  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************

  Authors: Alexander Klenin

}
unit TADiagramLayout;

{$H+}

interface

uses
  TADiagram;

function MakeBoxSideConnector(
  ABox: TDiaBox; ASide: TDiaBoxSide): TDiaBoxConnector;
function MakeLink(
  ADia: TDiagram; AConn1, AConn2: TDiaBoxConnector): TDiaLink;

implementation

function MakeBoxSideConnector(
  ABox: TDiaBox; ASide: TDiaBoxSide): TDiaBoxConnector;
begin
  Result := TDiaBoxConnector.Create;
  Result.Position := DiaPos(50, duPixels, false, true);
  Result.Side := ASide;
  ABox.Add(Result);
end;

function MakeLink(
  ADia: TDiagram; AConn1, AConn2: TDiaBoxConnector): TDiaLink;
begin
  Result := TDiaLink.Create;
  Result.Start.Connector := AConn1;
  Result.Finish.Connector := AConn2;
  ADia.Add(Result);
end;

end.

