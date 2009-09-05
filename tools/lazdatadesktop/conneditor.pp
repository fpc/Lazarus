{
 ***************************************************************************
 *                                                                         *
 *   This source is free software; you can redistribute it and/or modify   *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 *   This code is distributed in the hope that it will be useful, but      *
 *   WITHOUT ANY WARRANTY; without even the implied warranty of            *
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU     *
 *   General Public License for more details.                              *
 *                                                                         *
 *   A copy of the GNU General Public License is available on the World    *
 *   Wide Web at <http://www.gnu.org/copyleft/gpl.html>. You can also      *
 *   obtain it by writing to the Free Software Foundation,                 *
 *   Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.        *
 *                                                                         *
 ***************************************************************************
}
unit conneditor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, db, fpdatadict, controls, comctrls, stdctrls, extctrls,
  graphics, imglist, lresources, RTTIGrids, querypanel, lazdatadeskstr;

Type

  { TConnectionEditor }
  TConnectionEditor = Class(TTabSheet)
  private
    FDescription: String;
    FEngine: TFPDDEngine;
    FPC : TPageControl;
    FDisplay: TPanel;
    FSplit : TSplitter;
    FTV: TTreeView;
    FImgList : TImageList;
    FTSDisplay : TTabsheet;
    FTSQuery : TTabsheet;
    FQueryPanel : TQueryPanel;
    procedure AddPair(LV: TListView; Const AName, AValue: String);
    procedure ClearDisplay;
    procedure DoSelectNode(Sender: TObject);
    function GetCurrentObjectType: TObjectType;
    function NewNode(TV: TTreeView; ParentNode: TTreeNode; ACaption: String;
      AImageIndex: Integer): TTreeNode;
    procedure SelectConnection;
    procedure SelectField(TableName, FieldName: String);
    procedure SelectFields(TableName: String);
    procedure SelectIndexes(TableName: String);
    procedure SelectTable(TableName: String);
    procedure SelectTables;
    procedure SetDescription(const AValue: String);
    procedure SetEngine(const AValue: TFPDDEngine);
    procedure ShowDatabase;
    procedure ShowFields(ATableName: String; ATV: TTreeView;
      ParentNode: TTreeNode);
    procedure ShowFields(ATableName: String; ALV: TListView);
    procedure ShowIndexes(ATableName: String; ATV: TTreeView;
      ParentNode: TTreeNode);
    procedure ShowIndexes(ATableName: String; ALV: TListView);
    procedure ShowTableData(ATableName: String);
    procedure ShowTables(ATV : TTreeView;ParentNode: TTreeNode; AddSubNodes : Boolean = False);
  Public
    Constructor Create(AOwner : TComponent); override;
    Destructor Destroy; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    Procedure Connect(Connectstring : String);
    Procedure DisConnect;
    Function CanCreateCode : Boolean;
    Procedure CreateCode;
    Property Engine : TFPDDEngine Read FEngine Write SetEngine;
    Property ObjectType : TObjectType Read GetCurrentObjectType;
    Property Description : String Read FDescription Write SetDescription;
  end;

Const
  // Image Index for nodes. Relative to ImageOffset;
  iiConnection   = 0;
  iiTables       = 1;
  iiTable        = 2;
  iiFields       = 3;
  iiField        = 4;
  iiIndexes      = 5;
  iiIndex        = 6;
  iiTableData    = 7;
  iiIndexFields  = 8;
  iiIndexOptions = 9;
  FimageOffset = 0;
  
{
  // later ?
  iiViews      = 5;
  iiView       = 6;
  iiProcedures = 7;
  iiProcedure  = 8;
}

implementation

uses typinfo, datapanel;

{ TConnectionEditor }

procedure TConnectionEditor.SetEngine(const AValue: TFPDDEngine);
begin
  if FEngine=AValue then exit;
  If (FEngine<>Nil) then
    FEngine.Disconnect;
  FEngine:=AValue;
  FQuerypanel.Engine:=AValue;
  If (FEngine<>Nil) then
    begin
    FEngine.FreeNotification(Self);
    FTSQuery.TabVisible:=(ecRunquery in Fengine.EngineCapabilities);
    end
  else
    FTSQuery.TabVisible:=False;
end;

constructor TConnectionEditor.Create(AOwner: TComponent);

Const
  ImageNames : Array[0..9] of string =
        ('ddconnection','ddtables','ddtable','ddfields','ddfield',
         'ddindexes','ddindex','ddtabledata',
         // Need images for these...
         'ddtables','ddtables');



Var
  P : TPortableNetworkGraphic;
  I : Integer;

begin
  inherited Create(AOwner);
  FTV:=TTreeView.Create(Self);
  FTV.Name:='FTV';
  FTV.Parent:=Self;
  FTV.Align:=alLeft;
  FTV.Width:=300;
  FTV.OnSelectionChanged:=@DoSelectNode;
  // Image list
  FImgList:=TImageList.Create(Self);
  For I:=0 to 8 do
    begin
    P:=TPortableNetworkGraphic.Create;
    try
      P.LoadFromLazarusResource(ImageNames[i]);
      FImgList.Add(P,Nil);
    finally
      P.Free;
    end;
    end;
  FTV.Images:=FImgList;
  // Splitter
  FSplit:=TSplitter.Create(Self);
  FSplit.Parent:=Self;
  FSplit.Align:=alLeft;
  // Page control
  FPC:=TPageControl.Create(Self);
  FPC.Parent:=Self;
  FPC.Name:='FPC';
  FPC.Align:=alClient;
  // Display tab sheet
  FTSDisplay:=TTabsheet.Create(Self);
  FTSDisplay.Name:='FTSDisplay';
  FTSDisplay.parent:=FPC;
  FTSDisplay.Caption:=SSelectedObject;
  // Query tab sheet
  FTSQuery:=TTabsheet.Create(Self);
  FTSQuery.Name:='FTSQuery';
  FTSQuery.parent:=FPC;
  FTSQuery.Caption:=SQuery;
  // Display panel
  FDisplay:=TPanel.Create(Self);
  FDisplay.Parent:=FTSDisplay;
  FDisplay.Name:='FDisplay';
  FDisplay.Align:=alClient;
  FDisplay.Caption:='';
  // Query panel
  FQueryPanel:= TQueryPanel.Create(Self);
  FQueryPanel.Name:='FQueryPanel';
  FQueryPanel.Parent:=FTSQuery;
  FQueryPanel.Align:=alClient;
  ShowDatabase;
end;

destructor TConnectionEditor.Destroy;
begin
  If Assigned(Fengine) then
    FEngine.Disconnect;
  inherited Destroy;
end;

procedure TConnectionEditor.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  If (Operation=opRemove) and (AComponent=FEngine) then
    FEngine:=Nil;
end;

procedure TConnectionEditor.Connect(Connectstring : String);
begin
  If FEngine.Connect(ConnectString) then
    ShowDatabase;
end;

procedure TConnectionEditor.DisConnect;
begin
  If Assigned(FEngine) then
    FEngine.Disconnect;
end;

function TConnectionEditor.CanCreateCode: Boolean;

Var
  C : TControl;

begin
  Result:=False;
  If FPC.ActivePage=FTSQuery then
    begin
    Result:=Assigned(FQueryPanel.Dataset) and FQueryPanel.Dataset.Active;
    end
  else  If FPC.ActivePage=FTSDisplay then
    begin
    C:=FDisplay.Controls[0];
    If Not (C is TDataPanel) then
      C:=Nil;
    Result:=Assigned(C);
    end;
end;

procedure TConnectionEditor.CreateCode;

Var
  C : TControl;

begin
  If FPC.ActivePage=FTSQuery then
    begin
    FQueryPanel.CreateCode;
    end
  else If FPC.ActivePage=FTSDisplay then
    begin
    C:=FDisplay.Controls[0];
    If (C is TDataPanel) then
      TDataPanel(C).CreateCode;
    end;
end;

function TConnectionEditor.NewNode(TV : TTreeView;ParentNode: TTreeNode; ACaption: String; AImageIndex : Integer
  ): TTreeNode;
begin
  Result:=TV.Items.AddChild(ParentNode,ACaption);
  If AImageIndex>=0 then
    begin
    Result.ImageIndex:=FImageOffset+AImageIndex;
    Result.SelectedIndex:=Result.ImageIndex;
    end;
end;

procedure TConnectionEditor.ShowDatabase;

Var
  S : String;
  FConnNode : TTreeNode;
  TablesNode : TTreeNode;
  
begin
  FTV.Items.BeginUpdate;
  try
    FTV.Items.Clear;
    If Assigned(FEngine) then
      begin
      S:=FDescription;
      If (S='') then
        S:=SNodeDatabase;
      FConnNode:=NewNode(FTV,Nil,S,iiConnection);
      TablesNode:=NewNode(FTV,FConnNode,SNodeTables,iiTables);
      ShowTables(FTV,TablesNode,True);
      FConnNode.Expand(False);
      TablesNode.Expand(False);
      FTV.Selected:=FConnNode;
      end;
  Finally
    FTV.Items.EndUpdate;
  end;
end;

procedure TConnectionEditor.ShowTables(ATV : TTreeView;ParentNode : TTreeNode; AddSubNodes : Boolean = False);

Var
  L : TStringList;
  I : Integer;
  N : TTreeNode;
  
begin
  L:=TStringList.Create;
  Try
    FEngine.GetTableList(L);
    L.Sorted:=True;
    For I:=0 to L.Count-1 do
      begin
      N:=NewNode(ATV,ParentNode,L[I],iiTable);
      If AddSubNodes then
        begin
        NewNode(ATV,N,SNodeFields,iiFields);
        If (ecTableIndexes in FEngine.EngineCapabilities) then
          NewNode(ATV,N,SNodeIndexes,iiIndexes);
        If (ecViewTable in FEngine.EngineCapabilities) then
          NewNode(ATV,N,SNodeTabledata,iiTableData);
        end;
      end;
  Finally
    L.Free;
  end;
end;

procedure TConnectionEditor.DoSelectNode(Sender: TObject);

Var
  N,PN,PPN : TTreeNode;

begin
  N:=FTV.Selected;
  If N=Nil then
    exit;
  If Assigned(N.Parent) then
    begin
    PN:=N.Parent;
    If Assigned(PN) then
      PPN:=PN.Parent;
    end;
  Case ObjectType of
    otUnknown    : ;
    otConnection : SelectConnection;
    otTables     : SelectTables;
    otTable      : SelectTable(N.Text);
    otFields     : If Assigned(PN) then
                     SelectFields(PN.Text);
    otField      : If Assigned(PPN) then
                     SelectField(PPN.Text,N.Text);
    otTableData  : If Assigned(PN) then
                     ShowTableData(PN.Text);
    otIndexDefs  : If Assigned(PN) then
                     SelectIndexes(PN.Text);
  end;
end;

procedure TConnectionEditor.ShowTableData(ATableName : String);

Var
  P : TDataPanel;

begin
  ClearDisplay;
  P:=TDataPanel.Create(Self);
  P.TableName:=ATableName;
  P.Parent:=FDisplay;
  P.Align:=alClient;
  P.Dataset:=FEngine.ViewTable(ATableName,Self);
  P.Dataset.Open;
end;


procedure TConnectionEditor.AddPair(LV : TListView; Const AName, AValue : String);

Var
  LI : TListItem;
  
begin
  LI:=LV.Items.Add;
  LI.Caption:=AName;
  LI.SubItems.Add(AValue);
end;

procedure TConnectionEditor.SelectConnection;


Var
  LV : TListView;
  LC : TListColumn;
  L : TStringList;
  N,V : String;
  I : Integer;


begin
  ClearDisplay;
  LV:=TListView.Create(Self);
  LV.ViewStyle:=vsReport;
  LV.ShowColumnHeaders:=True;
  LC:=LV.Columns.Add;
  LC.Caption:=SParameter;
  LC.Width:=100;
  LC:=LV.Columns.Add;
  LC.Caption:=SValue;
  LC.Width:=300;
  LV.Parent:=FDisplay;
  LV.Align:=alClient;
  LV.BeginUpdate;
  try
    AddPair(LV,SDescription,FDescription);
    AddPair(LV,SEngineType,FEngine.Description);
    L:=TStringList.Create;
    Try
      L.CommaText:=FEngine.ConnectString;
      For I:=0 to L.Count-1 do
        begin
        L.GetNameValue(I,N,V);
        If (CompareText(N,'Password')<>0) then
          AddPair(LV,N,V);
        end;
    Finally
      L.Free;
    end;
  finally
    LV.EndUpdate;
  end;
end;

procedure TConnectionEditor.ClearDisplay;

begin
  With FDisplay do
    While (ControlCount>0) do
      Controls[ControlCount-1].Free;
end;

procedure TConnectionEditor.SelectTables;

Var
  TV : TTreeView;

begin
  ClearDisplay;
  TV:=TTreeView.Create(Self);
  TV.Parent:=FDisplay;
  TV.Align:=alClient;
  ShowTables(TV,Nil);
end;

procedure TConnectionEditor.SetDescription(const AValue: String);
begin
  if FDescription=AValue then exit;
  FDescription:=AValue;
  Caption:=AValue;
end;

procedure TConnectionEditor.SelectTable(TableName : String);

Var
  TV : TTreeView;
  TN : TTreeNode;
  N : TTreeNode;
  
begin
  ClearDisplay;
  TV:=TTreeView.Create(Self);
  TV.Parent:=FDisplay;
  TV.Align:=alClient;
  TN:=NewNode(TV,Nil,TableName,iiTable);
  N:=NewNode(TV,TN,SNodeFields,iiFields);
  ShowFields(TableName,TV,N);
  N:=NewNode(TV,TN,SNodeIndexes,iiIndexes);
  ShowIndexes(TableName,TV,N);
  TN.Expand(True);
end;

procedure TConnectionEditor.SelectIndexes(TableName : String);

Var
  LV : TListView;
  LC : TListColumn;

begin
  ClearDisplay;
  LV:=TListView.Create(Self);
  LV.ViewStyle:=vsReport;
  LV.ShowColumnHeaders:=True;
  LC:=LV.Columns.Add;
  LC.Caption:=SColName;
  LC.Width:=200;
  LC:=LV.Columns.Add;
  LC.Caption:=SColFields;
  LC.Width:=80;
  LC:=LV.Columns.Add;
  LC.Caption:=SColOptions;
  LC.Width:=30;
  LV.Parent:=FDisplay;
  LV.Align:=alClient;
  LV.BeginUpdate;
  Try
    ShowIndexes(TableName,LV);
  Finally
    LV.EndUpdate;
  end;
end;

procedure TConnectionEditor.ShowIndexes(ATableName : String; ATV : TTreeView;ParentNode : TTreeNode);

Var
  L : TStringList;
  ID : TDDIndexDefs;
  D : TDDIndexDef;
  NI : TTreeNode;
  I : Integer;

begin
  L:=TStringList.Create;
  Try
    ID:=TDDIndexDefs.Create(ATableName);
    try
      FEngine.GetTableIndexDefs(ATableName,ID);
      For I:=0 to ID.Count-1 do
        L.AddObject(ID[I].IndexName,ID[I]);
      L.Sort;
      For I:=0 to L.Count-1 do
        begin
        D:=L.Objects[I] as TDDIndexDef;
        NI:=NewNode(ATV,ParentNode,D.IndexName,iiIndex);
        NewNode(ATV,NI,SNodeIndexFields+D.Fields,iiIndexFields);
        NewNode(ATV,NI,SNodeIndexOptions+IndexOptionsToString(D.Options),iiIndexOptions)
        end;
    finally
      ID.Free;
    end;
  Finally
    L.Free;
  end;
end;

procedure TConnectionEditor.ShowIndexes(ATableName : String; ALV : TListView);

Var
  L : TStringList;
  ID : TDDIndexDefs;
  D : TDDIndexDef;
  LI : TListItem;
  I : Integer;

begin
  L:=TStringList.Create;
  Try
    ID:=TDDIndexDefs.Create(ATableName);
    try
      FEngine.GetTableIndexDefs(ATableName,ID);
      For I:=0 to ID.Count-1 do
        L.AddObject(ID[I].IndexName,ID[I]);
      L.Sort;
      For I:=0 to L.Count-1 do
        begin
        D:=L.Objects[I] as TDDIndexDef;
        LI:=ALV.Items.Add;
        LI.Caption:=D.IndexName;
        LI.SubItems.Add(D.Fields);
        LI.SubItems.Add(IndexOptionsToString(D.Options));
        end;
    finally
      ID.Free;
    end;
  Finally
    L.Free;
  end;
end;

procedure TConnectionEditor.ShowFields(ATableName : String; ATV : TTreeView;ParentNode : TTreeNode);

Var
  L : TStringList;
  I : Integer;
  TD : TDDTableDef;
begin
  L:=TStringList.Create;
  Try
    TD:=TDDTableDef.Create(Nil);
    Try
      TD.TableName:=ATableName;
      FEngine.ImportFields(TD);
      For I:=0 to TD.Fields.Count-1 do
        L.Add(TD.Fields[I].FieldName);
    Finally
      TD.Free;
    end;
    L.Sorted:=True;
    For I:=0 to L.Count-1 do
      NewNode(ATV,ParentNode,L[I],iiField);
  Finally
    L.Free;
  end;
end;

procedure TConnectionEditor.ShowFields(ATableName : String; ALV : TListView);

Var
  L : TStringList;
  I : Integer;
  TD : TDDTableDef;
  FD : TDDFieldDef;
  LI : TListItem;
  
begin
  L:=TStringList.Create;
  Try
    TD:=TDDTableDef.Create(Nil);
    Try
      TD.TableName:=ATableName;
      FEngine.ImportFields(TD);
      For I:=0 to TD.Fields.Count-1 do
        L.AddObject(TD.Fields[I].FieldName,TD.Fields[I]);
      L.Sorted:=True;
      For I:=0 to L.Count-1 do
        begin
        LI:=ALV.Items.Add;
        FD:=L.Objects[I] as TDDFieldDef;
        LI.Caption:=FD.FieldName;
        LI.SubItems.Add(GetEnumName(TypeInfo(TFieldType),Ord(FD.FieldType)));
        LI.SubItems.Add(IntToStr(FD.Size));
        end;
    Finally
      TD.Free;
    end;
  Finally
    L.Free;
  end;
end;

procedure TConnectionEditor.SelectFields(TableName : String);

Var
  LV : TListView;
  LC : TListColumn;

begin
  ClearDisplay;
  LV:=TListView.Create(Self);
  LV.ViewStyle:=vsReport;
  LV.ShowColumnHeaders:=True;
  LC:=LV.Columns.Add;
  LC.Caption:=SColName;
  LC.Width:=200;
  LC:=LV.Columns.Add;
  LC.Caption:=SColType;
  LC.Width:=80;
  LC:=LV.Columns.Add;
  LC.Caption:=SColSize;
  LC.Width:=30;
  LV.Parent:=FDisplay;
  LV.Align:=alClient;
  LV.BeginUpdate;
  Try
    ShowFields(TableName,LV);
  Finally
    LV.EndUpdate;
  end;
end;

procedure TConnectionEditor.SelectField(TableName,FieldName : String);

begin
end;

function TConnectionEditor.GetCurrentObjectType: TObjectType;

Var
  N : TTreeNode;

begin
  Result:=otUnknown;
  N:=FTV.Selected;
  If N=Nil then
    exit;
  Case N.ImageIndex-FImageOffset of
    iiConnection : Result:=otConnection;
    iiTables     : Result:=otTables;
    iiTable      : Result:=otTable;
    iiFields     : Result:=otFields;
    iiField      : Result:=otField;
    iiTableData  : Result:=otTabledata;
    iiIndexes    : Result:=otIndexDefs;
  end;
end;

end.

