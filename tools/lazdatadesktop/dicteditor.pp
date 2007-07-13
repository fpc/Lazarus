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
unit dicteditor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpdatadict, controls, comctrls, stdctrls, extctrls, RTTIGrids;

Type

  { TDataDictEditor }

  TDataDictEditor = Class(TTabSheet)
  private
    FDD: TFPDataDictionary;
    FImageOffset: Integer;
    FModified: Boolean;
    FTV : TTreeView;
    FEdit : TPanel;
    FSplit : TSplitter;
    FDDNode,FTablesNode : TTreeNode;
    function GetCurrentField: TDDFieldDef;
    function GetCurrentObjectType: TObjectType;
    function GetCurrentTable: TDDTableDef;
    Function NewNode (TV : TTreeView;ParentNode : TTreeNode; ACaption : String; AImageIndex : Integer) : TTreeNode;
    Procedure SetCaption;
    Procedure DoSelectNode(Sender : TObject);
    Procedure DoPropertyModified(Sender : TObject);
    Procedure ClearEditor;
    Procedure SelectTable(TD : TDDTableDef);
    Procedure SelectField(FD : TDDFieldDef);
    procedure SelectDictionary;
    procedure SelectTables;
    procedure SelectFields(TableDef : TDDTableDef);
    procedure SetModified(const AValue: Boolean);
    procedure UpdateSelectedNode;
    Function GetObjectType(Node : TTreeNode): TObjectType;
    Function CreatePropertyGrid(P : TPersistent) : TTIPropertyGrid;
    procedure FieldsDblClick(Sender : TObject);
    Function  FindNodeWithData(TV : TTreeView; P : Pointer) : TTreeNode;
    procedure TablesDblClick(Sender : TObject);
  Public
    Constructor Create(AOwner : TComponent); override;
    Destructor Destroy; override;
    Procedure ShowDictionary;
    Procedure NewTable(ATableName: String);
    Procedure NewField(AFieldName : String; TD : TDDTableDef);
    Procedure ShowTables(TV : TTreeView;ParentNode: TTreeNode; AddFieldsNode : Boolean);
    Procedure ShowFields(TV : TTreeView;TableNode: TTreeNode; TableDef : TDDTableDef);
    Procedure LoadFromFile(AFileName : String);
    Procedure SaveToFile(AFileName : String);
    Procedure DeleteTable(TD : TDDTableDef);
    Procedure DeleteField(FD : TDDFieldDef);
    Property DataDictionary : TFPDataDictionary Read FDD;
    Property Modified : Boolean Read FModified Write SetModified;
    Property ImageOffset : Integer Read FImageOffset Write FImageOffset;
    Property ObjectType : TObjectType Read GetCurrentObjectType;
    Property CurrentTable : TDDTableDef Read GetCurrentTable;
    Property CurrentField : TDDFieldDef Read GetCurrentField;
  end;
  

Const
  // Image Index for nodes. Relative to ImageOffset;
  iiDataDict = 0;
  iiTables   = 1;
  iiTable    = 2;
  iiFields   = 3;
  iiField    = 4;

implementation

uses Dialogs;

ResourceString
  SNodeDataDictionary = 'Datadictionary';
  SNodeTables         = 'Tables';
  SNodeFields         = 'Fields';
  SNewDictionary      = 'New dictionary';
  
{ TDataDictEditor }

function TDataDictEditor.NewNode(TV : TTreeView;ParentNode: TTreeNode; ACaption: String; AImageIndex : Integer
  ): TTreeNode;
begin
  Result:=TV.Items.AddChild(ParentNode,ACaption);
  If AImageIndex>=0 then
    Result.ImageIndex:=FImageOffset+AImageIndex;
end;

function TDataDictEditor.GetCurrentObjectType: TObjectType;
begin
  Result:=GetObjectType(FTV.Selected);
end;

function TDataDictEditor.GetCurrentField: TDDFieldDef;

Var
  N: TTreeNode;

begin
  Result:=Nil;
  N:=FTV.Selected;
  While (N<>Nil) and (GetObjectType(N)<>otField) do
    N:=N.Parent;
  if (N<>Nil) then
    Result:=TDDFieldDef(N.Data);

end;

function TDataDictEditor.GetCurrentTable: TDDTableDef;

Var
  N: TTreeNode;
  
begin
  Result:=Nil;
  N:=FTV.Selected;
  While (N<>Nil) and (GetObjectType(N)<>otTable) do
    N:=N.Parent;
  if (N<>Nil) then
    Result:=TDDTableDef(N.Data);
end;

constructor TDataDictEditor.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDD:=TFPDataDictionary.Create;
  FEdit:=TPanel.Create(Self);
  FEdit.Parent:=Self;
  FEdit.Name:='FEdit';
  FEdit.Align:=alRight;
  FEdit.Caption:='';
  FEdit.Width:=200;
  FSplit:=TSplitter.Create(Self);
  FSplit.Parent:=Self;
  FSplit.Align:=alRight;
  FTV:=TTreeView.Create(Self);
  FTV.Name:='FTV';
  FTV.Parent:=Self;
  FTV.Align:=alClient;
  FTV.OnSelectionChanged:=@DoSelectNode;
  ShowDictionary;
end;

destructor TDataDictEditor.Destroy;
begin
  FreeAndNil(FTV);
  FreeAndNil(FDD);
  inherited Destroy;
end;

procedure TDataDictEditor.ShowDictionary;

var
  S : String;
  
begin
  FTV.Items.BeginUpdate;
  try
    FTV.Items.Clear;
    S:=FDD.Name;
    If (S='') then
      S:=SNodeDataDictionary;
    FDDNode:=NewNode(FTV,Nil,S,iiDataDict);
    FDDNode.Data:=FDD;
    FTablesNode:=NewNode(FTV,FDDNode,SNodeTables,iiTables);
    ShowTables(FTV,FTablesNode,True);
    SetCaption;
    FTV.Selected:=FDDNode;
  finally
    FTV.Items.EndUpdate;
  end;
end;

procedure TDataDictEditor.NewTable(ATableName: String);

Var
  TD : TDDTableDef;
  N : TTreeNode;
  
begin
  TD:=FDD.Tables.AddTable(ATableName);
  With FTV do
    begin
    N:=NewNode(FTV,FTablesNode,ATableName,iiTable);
    N.Data:=TD;
    Selected:=N;
    NewNode(FTV,FTV.Selected,SNodeFields,iiFields);
    Modified:=True;
    end;
end;

procedure TDataDictEditor.NewField(AFieldName: String; TD: TDDTableDef);

Var
  TN : TTreeNode;
  FD : TDDFieldDef;
  
begin
  TN:=FindNodeWithData(FTV,TD);
  TN:=TN.GetFirstChild;
  While (TN<>Nil) and (GetObjectType(TN)<>otFields) do
    TN:=TN.GetNextSibling;
  If (TN<>Nil) then
    begin
    FD:=TD.Fields.AddField(AFieldName);
    TN:=NewNode(FTV,TN,AFieldName,iiField);
    TN.Data:=FD;
    FTV.Selected:=TN;
    Modified:=True;
    end;
end;

procedure TDataDictEditor.SetCaption;

Var
  S : String;
  
begin
  If (FDD.Name<>'') then
    S:=FDD.Name
  else
    S:=ChangeFileExt(ExtractFileName(FDD.FileName),'');
  If (S='') then
    S:=SNewDictionary;
  if FModified then
    S:=S+' *';
  Caption:=S;
end;

procedure TDataDictEditor.DoSelectNode(Sender: TObject);

Var
  N : TTreeNode;
  O,OP : TObject;
  
begin
  N:=FTV.Selected;
  If N=Nil then
    exit;
  O:=TObject(N.Data);
  If Assigned(N.Parent) then
    OP:=TObject(N.Parent.Data);
  Case ObjectType of
    otUnknown    : ;
    otDictionary : SelectDictionary;
    otTables     : SelectTables;
    otTable      : SelectTable(O as TDDTableDef);
    otFields     : SelectFields(OP as TDDTableDef);
    otField      : SelectField(TDDFieldDef(O));
  end;
end;

procedure TDataDictEditor.DoPropertyModified(Sender: TObject);
begin
  Modified:=True;
  UpdateSelectedNode;
end;

procedure TDataDictEditor.UpdateSelectedNode;

Var
  N : TTreeNode;
  
begin
  N:=FTV.Selected;
  If (N.Data=Nil) then
    Exit;
  With N do
    Case ObjectType of
      otField : Text:=TDDFieldDef(N.Data).FieldName;
      otDictionary : Text:=TFPDataDictionary(N.Data).Name;
      otTable : Text:=TDDTableDef(N.Data).TableName;
    end;
end;

procedure TDataDictEditor.SelectDictionary;

begin
  ClearEditor;
  CreatePropertyGrid(FDD);
end;

procedure TDataDictEditor.SelectTables;

Var
  TV : TTreeView;

begin
  ClearEditor;
  TV:=TTreeView.Create(Self);
  TV.Parent:=FEdit;
  TV.Align:=alClient;
  ShowTables(TV,Nil,False);
  TV.OnDblClick:=@TablesDblClick;
end;

procedure TDataDictEditor.TablesDblClick(Sender : TObject);

Var
  TV : TTreeView;
  N: TTreeNode;

begin
  TV:=Sender As TTreeView;
  N:=TV.Selected;
  If (GetObjectType(N)=otTable) and (N.Data<>Nil) then
    FTV.Selected:=FindNodeWithData(FTV,N.Data);
end;

procedure TDataDictEditor.SelectFields(TableDef : TDDTableDef);

Var
  TV : TTreeView;

begin
  ClearEditor;
  TV:=TTreeView.Create(Self);
  TV.Parent:=FEdit;
  TV.Align:=alClient;
  ShowFields(TV,Nil,TableDef);
  TV.OnDblClick:=@FieldsDblClick;
end;

procedure TDataDictEditor.SetModified(const AValue: Boolean);
begin
  FModified:=AValue;
  SetCaption;
end;

Function TDataDictEditor.FindNodeWithData(TV : TTreeView; P : Pointer) : TTreeNode;

Var
  I : Integer;

begin
  I:=0;
  Result:=Nil;
  While (Result=Nil) and (I<TV.Items.Count) do
    begin
    if (TV.Items[i].Data=P) then
      Result:=TV.Items[i];
    Inc(i);
    end;
end;

procedure TDataDictEditor.FieldsDblClick(Sender : TObject);

Var
  TV : TTreeView;
  N : TTreeNode;

begin
  TV:=Sender As TTreeView;
  N:=TV.Selected;
  If (GetObjectType(N)=otField) and (N.Data<>Nil) then
    FTV.Selected:=FindNodeWithData(FTV,N.Data);
end;

procedure TDataDictEditor.ClearEditor;

begin
  With FEdit do
    While (ControlCount>0) do
      Controls[ControlCount-1].Free;
end;

Function TDataDictEditor.CreatePropertyGrid(P : TPersistent) : TTIPropertyGrid;

begin
  Result:=TTIPropertyGrid.Create(Self);
  With Result do
    begin
    Parent:=FEdit;
    Align:=alClient;
    TIObject:=P;
    OnModified:=@DoPropertyModified;
    end;
end;

Procedure TDataDictEditor.SelectTable(TD : TDDTableDef);

begin
  ClearEditor;
  CreatePropertyGrid(TD);
end;

Procedure TDataDictEditor.SelectField(FD : TDDFieldDef);

begin
  ClearEditor;
  CreatePropertyGrid(FD);
end;

function TDataDictEditor.GetObjectType(Node: TTreeNode): TObjectType;

Var
  I : Integer;
  
begin
  Result:=otUnknown;
  If Node<>Nil then
    begin
    I:=Node.ImageIndex;
    I:=I-ImageOffset+1;
    If (I>=0) and (I<=Ord(High(TObjectType))) then
      Result:=TObjectType(I);
    end;
end;

procedure TDataDictEditor.ShowTables(TV : TTreeView;ParentNode: TTreeNode; AddFieldsNode: Boolean);

Var
  TN,FN : TTreeNode;
  TL : TStringList;
  TD : TDDTableDef;
  I  : Integer;
  
begin
  TL:=TStringList.Create;
  Try
    TL.Sorted:=true;
    For I:=0 to FDD.Tables.Count-1 do
      TL.AddObject(FDD.Tables[i].TableName,FDD.Tables[i]);
    For I:=0 to TL.Count-1 do
      begin
      TD:=TL.Objects[i] as TDDTableDef;
      TN:=NewNode(TV,ParentNode,TD.TableName,iiTable);
      TN.Data:=TD;
      If AddFieldsNode then
        begin
        FN:=NewNode(TV,TN,SNodeFields,iiFields);
        ShowFields(TV,FN,TD);
        end;
      end;
    If Assigned(ParentNode) then
      ParentNode.Expand(False);
  Finally
    FreeAndNil(TL);
  end;
end;

procedure TDataDictEditor.ShowFields(TV : TTreeView;TableNode: TTreeNode; TableDef: TDDTableDef);

Var
  TN : TTreeNode;
  TL : TStringList;
  FD : TDDFieldDef;
  I  : Integer;

begin
  TL:=TStringList.Create;
  Try
    TL.Sorted:=true;
    For I:=0 to TableDef.Fields.Count-1 do
      TL.AddObject(TableDef.Fields[i].FieldName,TableDef.Fields[i]);
    For I:=0 to TL.Count-1 do
      begin
      FD:=TL.Objects[i] as TDDFieldDef;
      TN:=NewNode(TV,TableNode,FD.FieldName,iiField);
      TN.Data:=FD;
      end;
    If Assigned(TableNode) then
      TableNode.Expand(False);
  Finally
    FreeAndNil(TL);
  end;
end;

procedure TDataDictEditor.LoadFromFile(AFileName: String);
begin
  FDD.LoadFromFile(AFileName);
  ShowDictionary;
  SetCaption;
end;

procedure TDataDictEditor.SaveToFile(AFileName: String);
begin
  With FDD do
    begin
    If (Name='') then
      Name:=ChangeFileExt(ExtractFileName(AFileName),'');
    SaveToFile(AFileName);
    end;
  Modified:=False;
end;

procedure TDataDictEditor.DeleteTable(TD: TDDTableDef);

Var
  N,NN : TTreeNode;
  
begin
  N:=FindNodeWithData(FTV,Pointer(TD));
  NN:=N.GetNextSibling;
  If (NN=Nil) then
    begin
    NN:=N.GetPrevSibling;
    If (NN=Nil) then
      NN:=FDDNode;
    end;
  N.Free;
  FTV.Selected:=NN;
  TD.Free;
  Modified:=True;
end;

procedure TDataDictEditor.DeleteField(FD: TDDFieldDef);

Var
  N,NN : TTreeNode;

begin
  N:=FindNodeWithData(FTV,Pointer(FD));
  NN:=N.GetNextSibling;
  If (NN=Nil) then
    begin
    NN:=N.GetPrevSibling;
    If (NN=Nil) then
      begin
      NN:=N.Parent;
      If Assigned(NN) then
        NN:=NN.Parent;
      end;
    end;
  N.Free;
  FTV.Selected:=NN;
  FD.Free;
  Modified:=True;
end;

end.

