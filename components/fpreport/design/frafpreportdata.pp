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
{$ifdef VER3_0}
  fprepexprpars,
{$ELSE}
  fpexprpars,
{$endif}
  fpreportdesignobjectlist;

type

  { TReportDataDisplay }

  TReportDataDisplay = class(TFrame)
    PCData: TPageControl;
    TabSheet1: TTabSheet;
    TVVariables: TTreeView;
    TSVariables: TTabSheet;
    TVData: TTreeView;
    TSData: TTabSheet;
    TVFunctions: TTreeView;
    procedure LBVariablesStartDrag(Sender: TObject; var DragObject: TDragObject);
    procedure TVDataMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure TVDataStartDrag(Sender: TObject; var DragObject: TDragObject);
    procedure TVFunctionsStartDrag(Sender: TObject; var DragObject: TDragObject);
    procedure TVVariablesMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
  private
    FIdentifiers: TFPExprIdentifierDefs;
    FUserVariables : TTreeNode;
    FBuiltinVariables : TTreeNode;
    FDataLastDown : TPoint;
    FVariablesLastDown : TPoint;
    FReport: TFPReport;
    FReportData: TFPReportDataCollection;

    procedure AddBuiltInVariables(aParent: TTreeNode);
    procedure AddUserVariables(aParent: TTreeNode; Vars: TFPReportVariables);
    procedure SetReport(AValue: TFPReport);
  public
    Constructor Create(AOwner : TComponent); override;
    Destructor Destroy; override;
    Procedure RefreshData;
    Procedure RefreshVariables;
    Procedure RefreshDisplay;
    procedure RefreshFunctions;
    Property Report : TFPReport Read FReport Write SetReport;
  end;

implementation

{$R *.lfm}

resourcestring
  SUnNamedData = 'Unnamed data %d';
  SBuiltIn = 'Built in';
  SUserDefined = 'User-defined';

{ TReportDataDisplay }

procedure TReportDataDisplay.TVDataStartDrag(Sender: TObject;
  var DragObject: TDragObject);

Var
  S : String;
  M : TMemoDragDrop;
  N : TTreeNode;

begin
  DragObject:=Nil;
  if (FDataLastDown.Y<>0) then
    begin
    N:=TVData.GetNodeAt(FDataLastDown.X,FDataLastDown.Y);
    if (N<>Nil) and (TVData.Selected<>N) then
      TVData.Selected:=N;
    end;
  if (TVData.Selected<>Nil) then
    begin
    S:=TVData.Selected.Text;
    if Assigned(TVData.Selected.Data) and (TObject(TVData.Selected.Data).InheritsFrom(TFPReportData)) then
      S:=TFPReportData(TVData.Selected.Data).Name+'.'+S;
    S:='['+S+']';
    M:=TMemoDragDrop.Create(TVData,S,[]);
    DragObject:=M;
    end;
  FDataLastDown.X:=0;
  FDataLastDown.Y:=0;
end;

procedure TReportDataDisplay.TVFunctionsStartDrag(Sender: TObject; var DragObject: TDragObject);

  Function TypeName (R : TResultType) : String;
  begin
    Result:=ResultTypeName(R);
    Delete(Result,1,2);
  end;


Var
  A,S : String;
  J : Integer;
  M : TMemoDragDrop;
  N : TTreeNode;
  D : TFPBuiltInExprIdentifierDef;

begin
  DragObject:=Nil;
  N:=TVFunctions.Selected;
  if (N<>Nil) then
    begin
    S:=TVFunctions.Selected.Text;
    if Assigned(N.Data) and (TObject(N.Data).InheritsFrom(TFPBuiltInExprIdentifierDef)) then
      begin
      D:=TFPBuiltInExprIdentifierDef(N.Data);
      S:=D.Name;
      A:='';
      if D.ArgumentCount<>0 then
        For J:=1 to D.ArgumentCount do
          begin
          if J>1 then
            A:=A+',';
          A:=A+TypeName(CharToResultType(D.ParameterTypes[J]));
          end;
        If (A<>'') then
          S:=S+'('+A+')';
        M:=TMemoDragDrop.Create(TVData,S,[mddShowEditor]);
        DragObject:=M;
        end;
    end;
end;

procedure TReportDataDisplay.TVVariablesMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  FVariablesLastDown:=Point(X,Y);
end;

procedure TReportDataDisplay.LBVariablesStartDrag(Sender: TObject;
  var DragObject: TDragObject);

Var
  S : String;
  M : TMemoDragDrop;
  O : TObject;
  N : TTreeNode;

begin
  if (FVariablesLastDown.Y<>0) then
    begin
    N:=TVData.GetNodeAt(FVariablesLastDown.X,FVariablesLastDown.Y);
    if (N<>Nil) and (TVVariables.Selected<>N) then
      begin
      TVVariables.Selected:=N;
      end;
    end;
  if (TVVariables.Selected<>Nil) then
    begin
    O:=TObject(TVVariables.Selected.Data);
    if Assigned(o) then
      if (O.InheritsFrom(TFPReportVariable)) then
        S:=TFPReportVariable(O).Name
      else if (O).InheritsFrom(TFPExprIdentifierDef) then
        S:=TFPExprIdentifierDef(O).Name;
    if (S<>'') then
      begin
      S:='['+S+']';
      M:=TMemoDragDrop.Create(TVVariables,S,[]);
      DragObject:=M;
      end;
    end;
  FVariablesLastDown.X:=0;
  FVariablesLastDown.Y:=0;
end;

procedure TReportDataDisplay.TVDataMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  FDataLastDown:=Point(X,Y);
end;

procedure TReportDataDisplay.RefreshFunctions;

Const
  CatNames : Array[TBuiltInCategory] of string =
    ('Strings','DateTime','Math','Boolean','Conversion','Data','Varia','User','Aggregate');

  Function TypeName (R : TResultType) : String;
  begin
    Result:=ResultTypeName(R);
    Delete(Result,1,2);
  end;

Var
  N : TTreeNode;
  S,A : String;
  I,J : integer;
  ID : TFPBuiltInExprIdentifierDef;
  CatNodes : Array[TBuiltInCategory] of TTreeNode;
  C : TBuiltInCategory;
  L : TStringList;

begin
  L:=Nil;
  With TVFunctions.Items do
    try
      BeginUpdate;
      Clear;
      if not Assigned(Report) then
        exit;
      L:=TStringList.Create;
      For I:=0 to BuiltinIdentifiers.IdentifierCount-1 do
        L.AddObject(BuiltinIdentifiers.Identifiers[i].Name,BuiltinIdentifiers.Identifiers[i]);
      L.Sort;
      For C in TBuiltInCategory do
        CatNodes[C]:=AddChild(Nil,CatNames[C]);
      For I:=0 to L.Count-1 do
        begin
        ID:=TFPBuiltInExprIdentifierDef(L.Objects[i]);
        S:=ID.Name;
        A:='';
        For J:=1 to ID.ArgumentCount do
          begin
          if J>1 then
            A:=A+',';
          A:=A+TypeName(CharToResultType(ID.ParameterTypes[J]));
          end;
        If (A<>'') then
          S:=S+'('+A+')';
        S:=S+':'+TypeName(ID.ResultType);
        N:=AddChild(CatNodes[ID.Category],S);
        N.Data:=ID;
        end;
      For C in TBuiltInCategory do
        if CatNodes[C].Count=0 then
          FreeAndNil(CatNodes[C])
        else
          CatNodes[C].Expand(True);
    finally
      EndUpdate;
      FreeAndNil(L);
    end;
end;

procedure TReportDataDisplay.SetReport(AValue: TFPReport);
begin
  if FReport=AValue then Exit;
  FReport:=AValue;
  RefreshVariables;
  RefreshData;
  RefreshFunctions;
end;

constructor TReportDataDisplay.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FReportData:=TFPReportDataCollection.Create(TFPReportDataItem);
  FIdentifiers:=TFPExprIdentifierDefs.Create(TFPExprIdentifierDef);
end;

destructor TReportDataDisplay.Destroy;
begin
  FreeAndNil(FIdentifiers);
  FreeAndNil(FReportData);
  inherited Destroy;
end;

procedure TReportDataDisplay.RefreshData;

Var
  DN,N : TTreeNode;
  D : TFPReportData;
  FN,RDN : String;
  I,J : integer;
  L : TStringList;

begin
  L:=Nil;
  With TVData.Items do
    try
      BeginUpdate;
      Clear;
      L:=TStringList.Create;
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
          L.Clear;
          For J:=0 to D.FieldCount-1 do
            L.Add(D.FieldNames[J]);
          L.Sort;
          For J:=0 to L.Count-1 do
            begin
            FN:=L[J];
            N:=AddChild(DN,FN);
            N.Data:=D;
            end;
          DN.Expand(True);
          end;
        end;
    finally
      L.Free;
      EndUpdate;
    end;
end;

procedure TReportDataDisplay.AddUserVariables(aParent : TTreeNode; Vars : TFPReportVariables);

Var
  V : TFPReportVariable;
  I : Integer;
  N : TTreeNode;
  L : TStringList;

begin
  L:=TStringList.Create;
  try
    For I:=0 to Vars.Count-1 do
      L.AddObject(Vars[i].Name,Vars[i]);
    L.Sort;
    For I:=0 to FReport.Variables.Count-1 do
      begin
      V:=TFPReportVariable(L.Objects[i]);
      N:=TVVariables.Items.AddChild(aParent,V.Name);
      N.Data:=V;
      end;
  finally
    L.Free;
  end;
  AParent.Expand(false);
end;

procedure TReportDataDisplay.AddBuiltInVariables(aParent : TTreeNode);

Var
  V : TFPExprIdentifierDef;
  I : Integer;
  N : TTreeNode;
  L : TStringList;

begin

  if Assigned(Fidentifiers) then
    FIdentifiers.Clear
  else
    FIdentifiers:=TFPExprIdentifierDefs.Create(TFPExprIdentifierDef);
  if Assigned(FReport) then
    FReport.AddBuiltinsToExpressionIdentifiers(FIdentifiers);
  if FIdentifiers.FindIdentifier('PageCount')=Nil then
    FIdentifiers.AddIntegerVariable('PageCount',-1);
  L:=TStringList.Create;
  try
    For I:=0 to Fidentifiers.Count-1 do
      L.AddObject(Fidentifiers[i].Name,Fidentifiers[i]);
    L.Sort;
    For I:=0 to L.Count-1 do
      begin
      V:=TFPExprIdentifierDef(L.Objects[i]);
      N:=TVVariables.Items.AddChild(aParent,V.Name);
      N.Data:=V;
      end;
  finally
    L.Free;
  end;
  AParent.Expand(false);
end;

procedure TReportDataDisplay.RefreshVariables;


begin
  With TVVariables.Items do
    try
      BeginUpdate;
      Clear;
      If Not Assigned(FReport) then
        Exit;
      FUserVariables:=AddChild(Nil,SUserDefined);
      FBuiltinVariables:=AddChild(Nil,SBuiltIn);
      AddBuiltInVariables(FBuiltinVariables);
      AddUservariables(FUserVariables,FReport.Variables);
    finally
      EndUpdate;
    end;

end;

procedure TReportDataDisplay.RefreshDisplay;
begin
  RefreshVariables;
  RefreshData;
end;

end.

