{
    This file is part of the Free Component Library.
    Copyright (c) 2017 Michael Van Canneyt, member of the Free Pascal development team

    Frame to display the object inspector.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit fraReportObjectInspector;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, RTTIGrids, Forms, Controls, ComCtrls, ExtCtrls,
  StdCtrls, fpreport, fpreportdesignobjectlist;

type
  { TObjectInspectorFrame }
  TSelectElementEvent = Procedure (Sender : TObject; Selected : TComponent) of Object;

  TObjectInspectorFrame = class(TFrame)
    ILTree: TImageList;
    LPropertyGrid: TLabel;
    Panel1: TPanel;
    Panel2: TPanel;
    PGReport: TTIPropertyGrid;
    Splitter1: TSplitter;
    TVReport: TTreeView;
    procedure PGReportModified(Sender: TObject);
    procedure TVReportSelectionChanged(Sender: TObject);
  private
    FISC: Boolean;
    FList: TReportObjectList;
    FOnModified: TNotifyEvent;
    FOnSelectElement: TSelectElementEvent;
    FReport: TFPReport;
    procedure AddElementChildren(AParentNode: TTreeNode; AParent: TFPReportElementWithChildren);
    function AddElementNode(AIndex: Integer; AParentNode: TTreeNode; AElement: TFPReportElement): TTreeNode;
    function GetNodeNameForElement(E: TComponent): String;
    function GetSRT: Boolean;
    procedure SetNodeProps(N: TTreeNode; E: TComponent);
    procedure SetReport(AValue: TFPReport);
    procedure SetSRT(AValue: Boolean);
  Protected
    Property IgnoreSelectionChange : Boolean Read FISC Write FISC;
  public
    Procedure UpdateSelection;
    procedure RefreshReportTree;
    Procedure SelectControls(AList : TReportObjectList);
    Property Objectlist : TReportObjectList Read FList;
    Property ShowReportTree : Boolean Read GetSRT Write SetSRT;
    Property Report : TFPReport Read FReport Write SetReport;
    Property OnSelectElement  : TSelectElementEvent Read FOnSelectElement Write FOnSelectElement;
    Property OnModified : TNotifyEvent Read FOnModified Write FOnModified;
  end;

implementation

uses propeditutils;

{$R *.lfm}

Const
  iiReport   = 0;
  iiPage     = 1;
  iiband     = 2;
  iiElement  = 3;

Resourcestring
  SPage   = 'Page %d';
  SReport = 'Report';
  SNoNameBand = '%d: Unnamed band %s';
  SNoNameElement = '%d: Unnamed element %s';
  SPGCaption = 'Properties of %s';
  Smultiple = 'multiple (%d) elements';
  SNoSelection = 'No selection';

{ TObjectInspectorFrame }

function TObjectInspectorFrame.GetSRT: Boolean;
begin
  Result:=TVReport.Visible;
end;

function TObjectInspectorFrame.GetNodeNameForElement(E: TComponent): String;

begin
  Result:=E.Name;
  if (Result<>'') then
    exit;
  if e is TFPCustomReport then
    Result:=SReport
  else if E is TFPReportCustomPage then
    Result:=Format(SPage,[(E as TFPReportCustomPage).PageIndex+1])
  else if E is TFPReportCustomBand then
    Result:=Format(SNoNameBand,[E.ComponentIndex,E.ClassName])
  else
    Result:=Format(SNoNameElement,[E.ComponentIndex,E.ClassName])

end;


procedure TObjectInspectorFrame.SetNodeProps(N : TTreeNode; E : TComponent);

begin
  if e is TFPCustomReport then
    N.ImageIndex:=iiReport
  else if E is TFPReportCustomPage then
    N.ImageIndex:=iiPage
  else if E is TFPReportCustomBand then
    N.ImageIndex:=iiBand
  else
    N.ImageIndex:=iiElement;
  N.Text:=GetNodeNameForElement(E);
  N.SelectedIndex:=N.ImageIndex;
  N.Data:=E;
end;

function TObjectInspectorFrame.AddElementNode(AIndex: Integer;
  AParentNode: TTreeNode; AElement: TFPReportElement): TTreeNode;

Var
  N : TTreeNode;

begin
  N:=TVReport.Items.AddChild(AParentNode,'');
  SetNodeProps(N,AElement);
  if AElement is TFPReportElementWithChildren then
    AddElementChildren(N,AElement as TFPReportElementWithChildren);
  Result:=N;
end;

procedure TObjectInspectorFrame.TVReportSelectionChanged(Sender: TObject);

Var
  N : TTreeNode;
  C : TComponent;

begin
  if IgnoreSelectionChange then
    exit;
  N:=TVReport.Selected;
  if Assigned(N) then
    C:=TComponent(N.Data)
  else
    C:=Nil;
  if Assigned(OnSelectElement) then
    OnSelectElement(Self,C);
end;

procedure TObjectInspectorFrame.PGReportModified(Sender: TObject);
begin
  If Assigned(FOnModified) then
    FOnModified(Self);
end;

procedure TObjectInspectorFrame.AddElementChildren(AParentNode : TTreeNode; AParent : TFPReportElementWithChildren);

Var
  I : Integer;

begin
  For I:=0 to AParent.ChildCount-1 do
    AddElementNode(I,AparentNode,AParent.Child[i]);
end;

procedure TObjectInspectorFrame.RefreshReportTree;

Var
  PN,N : TTreeNode;
  S : String;
  I : Integer;


begin
  With TVReport.Items do
    try
      IgnoreSelectionChange:=True;
      BeginUpdate;
      Clear;
      if Assigned(FReport) then
        begin
        PN:=AddChild(Nil,'Report');
        SetNodeProps(PN,FReport);
        For I:=0 to FReport.PageCount-1 do
          begin
          S:=Format(SPage,[I+1]);
          if (FReport.Pages[i].Name<>'') then
            S:=S+': '+FReport.Pages[i].Name;
          N:=AddChild(PN,S);
          SetNodeProps(N,FReport.Pages[i]);
          AddElementChildren(N,FReport.Pages[i]);
          end;
        PN.Expand(True);
        end;
    finally
      EndUpdate;
      IgnoreSelectionChange:=False;
    end;
end;

procedure TObjectInspectorFrame.SetReport(AValue: TFPReport);
begin
  if FReport=AValue then Exit;
  FReport:=AValue;
  if ShowReportTree then
    RefreshReportTree;
end;

procedure TObjectInspectorFrame.SetSRT(AValue: Boolean);
begin
  if AValue=GetSRT then exit;
  TVReport.Visible:=AValue;
  if AValue then
    RefreshReportTree;
end;

procedure TObjectInspectorFrame.UpdateSelection;

Var
  I : integer;
  L : TPersistentSelectionList;
  S : String;

begin
  // Update grid
  L:=TPersistentSelectionList.Create;
  try
    L.BeginUpdate;
    try
      if Assigned(ObjectList) then
        For I:=0 to ObjectList.Count-1 do
          if ObjectList[i].Selected then
            L.Add(ObjectList[i].Element);
      if (L.Count=0) and  Assigned(FReport) then
        L.Add(FReport);
      PGReport.Selection:=L;
    finally
      L.EndUpdate;
    end;
    // Update tree, select first element
    if L.Count>0 then
      begin
      IgnoreSelectionChange:=True;
      try
        TVReport.Selected:=TVReport.Items.FindNodeWithData(L.Items[0]);
      finally
        IgnoreSelectionChange:=False;
      end;
      end;
    // Update caption
    if L.Count>1 then
      S:=Format(SPGCaption,[Format(Smultiple,[L.Count])])
    else if L.Count=1 then
      S:=Format(SPGCaption,[GetNodeNameForElement(TComponent(L.Items[0]))])
    else
      S:=SNoSelection;
    LPropertyGrid.Caption:=S;
  Finally
    L.Free;
  end;
end;

procedure TObjectInspectorFrame.SelectControls(AList: TReportObjectList);
begin
  FList:=AList;
  UpdateSelection;
end;

end.

