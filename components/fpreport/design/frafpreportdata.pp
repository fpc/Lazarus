{
    This file is part of the Free Component Library.
    Copyright (c) 2017 Michael Van Canneyt, member of the Free Pascal development team

    Frame to display the data in a report.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit frafpreportdata;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, ComCtrls, StdCtrls, fpreport,
  fpreportdesignobjectlist;

type

  { TReportDataDisplay }

  TReportDataDisplay = class(TFrame)
    LBVariables: TListBox;
    PCData: TPageControl;
    TSVariables: TTabSheet;
    TVData: TTreeView;
    TSData: TTabSheet;
    procedure LBVariablesStartDrag(Sender: TObject; var DragObject: TDragObject);
    procedure TVDataStartDrag(Sender: TObject; var DragObject: TDragObject);
  private
    FReport: TFPReport;
    FReportData: TFPReportDataCollection;
    procedure SetReport(AValue: TFPReport);
  public
    Constructor Create(AOwner : TComponent); override;
    Destructor Destroy; override;
    Procedure RefreshData;
    Procedure RefreshVariables;
    Procedure RefreshDisplay;
    Property Report : TFPReport Read FReport Write SetReport;
  end;

implementation

{$R *.lfm}

resourcestring
  SUnNamedData = 'Unnamed data %d';

{ TReportDataDisplay }

procedure TReportDataDisplay.TVDataStartDrag(Sender: TObject;
  var DragObject: TDragObject);

Var
  S : String;
  M : TMemoDragDrop;
begin
  if (TVData.Selected<>Nil) then
    begin
    S:=TVData.Selected.Text;
    if Assigned(TVData.Selected.Data) and (TObject(TVData.Selected.Data).InheritsFrom(TFPReportData)) then
      S:=TFPReportData(TVData.Selected.Data).Name+'.'+S;
    M:=TMemoDragDrop.Create(TVData);
    M.Content:='['+S+']';
    DragObject:=M;
    end;
end;

procedure TReportDataDisplay.LBVariablesStartDrag(Sender: TObject;
  var DragObject: TDragObject);

Var
  S : String;
  M : TMemoDragDrop;

begin
  if (LBVariables.ItemIndex<>-1) then
    begin
    S:=LBVariables.Items[LBVariables.Itemindex];
    M:=TMemoDragDrop.Create(LBVariables);
    M.Content:='['+S+']';
    DragObject:=M;
    end;
end;

procedure TReportDataDisplay.SetReport(AValue: TFPReport);
begin
  if FReport=AValue then Exit;
  FReport:=AValue;
  RefreshVariables;
  RefreshData;
end;

constructor TReportDataDisplay.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FReportData:=TFPReportDataCollection.Create(TFPReportDataItem);
end;

destructor TReportDataDisplay.Destroy;
begin
  FreeAndNil(FReportData);
  inherited Destroy;
end;

procedure TReportDataDisplay.RefreshData;

Var
  DN,N : TTreeNode;
  D : TFPReportData;
  FN,RDN : String;
  I,J : integer;


begin
  With TVData.Items do
    try
      BeginUpdate;
      Clear;
      if not Assigned(Report) then
        exit;
      For I:=0 to Report.ReportData.Count-1 do
        begin
        D:=Report.ReportData[i].Data;
        if Assigned(D) then
          begin
          RDN:=D.Name;
          if RDN='' then
            RDN:=Format(SUnNamedData,[i+1]);
          DN:=AddChild(Nil,RDN);
          DN.Data:=D;
          For J:=0 to D.FieldCount-1 do
            begin
            FN:=D.FieldNames[J];
            N:=AddChild(DN,FN);
            N.Data:=D;
            end;
          DN.Expand(True);
          end;
        end;
    finally
      EndUpdate;
    end;
end;

procedure TReportDataDisplay.RefreshVariables;

Var
  V : TFPReportVariable;
  I : Integer;

begin
  With LBVariables.Items do
    try
      BeginUpdate;
      Clear;
      If Not Assigned(FReport) then
        Exit;
      For I:=0 to FReport.Variables.Count-1 do
        begin
        V:=FReport.Variables[I];
        AddObject(V.Name,V);
        end;
    finally
      BeginUpdate;
    end;

end;

procedure TReportDataDisplay.RefreshDisplay;
begin
  RefreshVariables;
  RefreshData;
end;

end.

