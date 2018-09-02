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
 *   Inc., 51 Franklin Street - Fifth Floor, Boston, MA 02110-1335, USA.   *
 *                                                                         *
 ***************************************************************************
}
unit dicteditor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpdatadict,
  Controls, ComCtrls, ExtCtrls, Graphics, Menus, Dialogs, RTTIGrids,
  LazUTF8;

Type
  TEditObjectType = (eotUnknown,eotDictionary,
                 eotTables,eotTable,
                 eotFields,eotField,
                 eotConnection,eotTableData,
                 eotIndexes,eotIndex,
                 eotSequences,eotSequence,
                 eotForeignKeys,eotForeignKey,
                 eotDomains,eotDomain);
Const
  SingleObjectTypes = [eotTable,eotDomain,eotSequence,
                       eotField,eotIndex,eotForeignKey];


Type
  { TDataDictEditor }

  TDataDictEditor = Class(TTabSheet)
  private
    FDD: TFPDataDictionary;
    FIMgList : TImageList;
    FImageOffset: Integer;
    FModified: Boolean;
    FTV : TTreeView;
    FEdit : TPanel;
    FSplit : TSplitter;
    FAllowDoubleClick : TEditObjectType;
    FDDNode,
    FTablesNode : TTreeNode;
    FMenu : TPopupMenu;
    FMINewTable,
    FMINewField,
    FMINewIndex,
    FMINewSequence,
    FMINewForeignKey,
    FMINewDomain,
    FMIDeleteObject: TMenuItem;
{$ifndef onlyoldobjects}
    FSequencesNode,
    FDomainsNode : TTreeNode;
{$endif}
    Function AddNewItemPopup(ObjectType: TEditObjectType; AImageIndex : Integer) : TMenuItem;
    procedure CreateGUI;
    procedure DoDoubleClick(Sender: TObject);
    procedure DoNewObject(Sender: TObject);
    procedure DoDeleteObject(Sender: TObject);
    function CurrentObjectWithType(AType: TEditObjectType): TObject;
    function GetCurrentObject: TPersistent;
    Function NewNode (TV : TTreeView;ParentNode : TTreeNode; ACaption : String; AImageIndex : Integer) : TTreeNode;
    Procedure SetCaption;
    Procedure DoSelectNode(Sender : TObject);
    Procedure DoPropertyModified(Sender : TObject);
    Procedure ClearEditor;
    procedure SetModified(const AValue: Boolean);
    procedure UpdateSelectedNode;
    Function GetObjectType(Node : TTreeNode): TEditObjectType;
    Function CreatePropertyGrid(P : TPersistent) : TTIPropertyGrid;
    Function FindNodeWithData(TV : TTreeView; P : Pointer) : TTreeNode;
    Function SelectNextNode (ANode : TTreeNode; ADefault: TTreeNode) : TTreeNode;
    function GetCurrentObjectType: TEditObjectType;
    function GetCurrentField: TDDFieldDef;
    function GetCurrentIndex: TDDIndexDef;
    function GetCurrentTable: TDDTableDef;
{$ifndef onlyoldobjects}
    function GetCurrentSequence: TDDSequenceDef;
    function GetCurrentForeignKey : TDDForeignKeyDef;
    function GetCurrentDomain : TDDDomainDef;
{$endif}
    procedure DoPopup(Sender: TObject);
    Procedure DeleteGlobalObject(AObject : TObject);
    Procedure DeleteTableObject(AObject : TObject);
    procedure SelectGlobalObjectList(AObjectType: TEditObjectType);
    procedure SelectTableObjectList(AObjectType: TEditObjectType; ATableDef : TDDTableDef);
    Procedure SelectSingleObject(AObject : TPersistent);
    procedure GetTableObjectsList(ATabledef: TDDTableDef; AObjectType: TEditObjectType; List: TStrings);
    procedure GetGlobalObjectsList(AObjectType: TEditObjectType; List: TStrings);
    procedure ShowSubLists(TV: TTreeView; ParentNode: TTreeNode; AObject: TObject);
    procedure ShowTableObjectList(TV: TTreeView; ParentNode: TTreeNode; ATableDef: TDDTableDef; AObjectType: TEditObjectType);
    procedure ShowGlobalObjectList(TV: TTreeView; ParentNode: TTreeNode; AObjectType: TEditObjectType; AShowSubLists : Boolean = False);
  Public
    Constructor Create(AOwner : TComponent); override;
    Destructor Destroy; override;
    // General methods.
    Procedure ShowDictionary;
    Procedure LoadFromFile(AFileName : String);
    Procedure SaveToFile(AFileName : String);
    Procedure CreateCode;
    // New items.
    function  NewGlobalObject(AObjectName: String; AObjectType: TEditObjectType): TObject;
    function  NewTableObject(AObjectName: String; TD: TDDTableDef;AObjectType: TEditObjectType): TObject;
    Procedure NewField(AFieldName : String; TD : TDDTableDef);
    Procedure NewIndex(AIndexName : String; TD : TDDTableDef);
    Procedure NewForeignKey(AKeyName : String; TD : TDDTableDef);
    // Delete items
{$ifndef onlyoldobjects}
    Procedure DeleteSequence(SD : TDDSequenceDef);
    Procedure DeleteDomain(DD : TDDDomainDef);
    Procedure DeleteForeignKey(KD : TDDForeignKeyDef);
{$endif onlyoldobjects}
    Procedure DeleteTable(TD : TDDTableDef);
    Procedure DeleteField(FD : TDDFieldDef);
    Procedure DeleteIndex(ID : TDDIndexDef);
    Procedure DeleteCurrentObject;
    // Properties
    Property DataDictionary : TFPDataDictionary Read FDD;
    Property Modified : Boolean Read FModified Write SetModified;
    Property ImageOffset : Integer Read FImageOffset Write FImageOffset;
    Property CurrentObject : TPersistent Read GetCurrentObject;
    Property ObjectType : TEditObjectType Read GetCurrentObjectType;
    Property CurrentTable : TDDTableDef Read GetCurrentTable;
    Property CurrentField : TDDFieldDef Read GetCurrentField;
    Property CurrentIndex : TDDIndexDef Read GetCurrentIndex;
{$ifndef onlyoldobjects}
    Property CurrentSequence : TDDSequenceDef Read GetCurrentSequence;
    Property CurrentDomain : TDDDomainDef Read GetCurrentDomain;
    Property CurrentForeignKey : TDDForeignKeyDef Read GetCurrentForeignKey;
{$endif onlyoldobjects}
  end;
  

Const
  // Image Index for nodes. Relative to ImageOffset;
  // Must match the TObjectType
  iiDataDict     = 0;
  iiTables       = 1;
  iiTable        = 2;
  iiFields       = 3;
  iiField        = 4;
  iiConnection   = 5;
  iiTableData    = 6;
  iiIndexes      = 7;
  iiIndex        = 8;
  iiSequences    = 9;
  iiSequence     = 10;
  iiForeignkeys  = 11;
  iiForeignKey   = 12;
  iiDomains      = 13;
  iiDomain       = 14;
  IIMaxObject    = IIDomain; // Should be last
  iiDelete       = iiMaxObject+1;

implementation

{$R dicteditor.res}

uses DB, MemDS, fpcodegenerator, TypInfo, lazdatadeskstr;

Function ObjectTypeName(ObjectType : TEditObjectType) : String;

Var
  S : String;

begin
  Case ObjectType of
    eotTable      : S:=STable;
    eotField      : S:=SField;
    eotIndex      : S:=SIndex;
    eotSequence   : S:=SSequence;
    eotForeignKey : S:=SForeignKey;
    eotDomain     : S:=SDomain
  else
    Raise EDataDict.CreateFmt(SErrUnknownType,[Ord(ObjectType)]);
  end;
  Result:=S;
end;

Function CreateDatasetFromTabledef(TD : TDDTableDef;AOwner : TComponent = Nil) : TDataset;

Var
  MDS : TMemDataset;
  I : Integer;
  FD : TFieldDef;
  FDD : TDDFieldDef;

begin
  MDS:=TMemDataset.Create(AOwner);
  try
    For I:=0 to TD.Fields.Count-1 do
      begin
      FDD:=TD.Fields[i];
      MDS.FieldDefs.Add(FDD.FieldName,FDD.FieldType);
      end;
    MDS.CreateTable;
    MDS.Open;
  except
    MDS.Free;
    Raise;
  end;
  Result:=MDS;
end;




{ TDataDictEditor }

function TDataDictEditor.NewNode(TV : TTreeView;ParentNode: TTreeNode; ACaption: String; AImageIndex : Integer
  ): TTreeNode;
begin
  Result:=TV.Items.AddChild(ParentNode,ACaption);
  If AImageIndex>=0 then
    begin
    Result.ImageIndex:=FImageOffset+AImageIndex;
    Result.SelectedIndex:=Result.ImageIndex;
    end;
end;

function TDataDictEditor.GetCurrentObjectType: TEditObjectType;
begin
  Result:=GetObjectType(FTV.Selected);
end;

function TDataDictEditor.CurrentObjectWithType(AType : TEditObjectType) : TObject;

Var
  N : TTreeNode;

begin
  Result:=Nil;
  N:=FTV.Selected;
  While (N<>Nil) and (GetObjectType(N)<>AType) do
    N:=N.Parent;
  if (N<>Nil) then
    Result:=TObject(N.Data);
end;

function TDataDictEditor.GetCurrentObject: TPersistent;

Var
  N : TTreeNode;

begin
  Result:=Nil;
  N:=FTV.Selected;
  While (N<>Nil) and Not (GetObjectType(N) in SingleObjectTypes) do
    N:=N.Parent;
  If Assigned(N) then
    Result:=TPersistent(N.Data);
end;

function TDataDictEditor.GetCurrentField: TDDFieldDef;

begin
  Result:=TDDFieldDef(CurrentObjectWithType(eotField));
end;

function TDataDictEditor.GetCurrentIndex: TDDIndexDef;

begin
  Result:=TDDIndexDef(CurrentObjectWithType(eotIndex));
end;

function TDataDictEditor.GetCurrentTable: TDDTableDef;

begin
  Result:=TDDTableDef(CurrentObjectWithType(eotTable));
end;

{$ifndef onlyoldobjects}
function TDataDictEditor.GetCurrentSequence: TDDSequenceDef;

begin
  Result:=TDDSequenceDef(CurrentObjectWithType(eotSequence));
end;

function TDataDictEditor.GetCurrentDomain: TDDDomainDef;

begin
  Result:=TDDDomainDef(CurrentObjectWithType(eotDomain));
end;

function TDataDictEditor.GetCurrentForeignKey: TDDForeignKeyDef;

begin
  Result:=TDDForeignKeyDef(CurrentObjectWithType(eotForeignKey));
end;
{$endif onlyoldobjects}

procedure TDataDictEditor.DoPopup(Sender: TObject);

Var
  B : Boolean;
  EOT : TEditObjectType;

begin
  // Check availablility of items;
  B:=CurrentTable<>Nil;
  FMINewField.Enabled:=B;
  FMINewIndex.Enabled:=B;
  FMINewForeignKey.Enabled:=B;
  EOT:=ObjectType;
  B:=EOT in SingleObjectTypes;
  FMIDeleteObject.Enabled:=B;
  If B then
    FMIDeleteObject.Caption:=Format(SDeleteObject,[ObjectTypeName(EOT)])
  else
    FMIDeleteObject.Caption:=Format(SDeleteObject,[SObject]);
end;


Function TDataDictEditor.AddNewItemPopup(ObjectType : TEditObjectType; AImageIndex : Integer) : TMenuItem;

Var
  S: String;

begin
  Result:=TMenuItem.Create(Self);
  Result.Name:='NewItem'+GetEnumName(TypeInfo(TEditObjectType),Ord(ObjectType));
  Result.Tag:=Ord(ObjectType);
  S:=ObjectTypeName(ObjectType);
  Result.Caption:=Format(SNew,[S]);
  Result.OnClick:=@DoNewObject;
  Result.ImageIndex:=AImageIndex;
  FMenu.Items.Add(Result);
end;

constructor TDataDictEditor.Create(AOwner: TComponent);


begin
  inherited Create(AOwner);
  FDD:=TFPDataDictionary.Create;
  //CreateGUI;
end;

Procedure TDataDictEditor.CreateGUI;

Const
  ImageNames : Array[0..IIMaxObject+1] of string =
        ('dddatadict','ddtables','ddtable','ddfields','ddfield',
         'ddtables','ddtabledata','ddindexes','ddindex',
         'ddsequences','ddsequence',
         'ddforeignkeys','ddforeignkey',
         'dddomains','dddomain','dddeleteobject');


Var
  P : TPortableNetworkGraphic;
  I : Integer;
begin
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
  FTV.ShowLines:=True;
  FMenu:=TPopupMenu.Create(Self);
  FMenu.Name:='FMenu';
  FMenu.OnPopup:=@DoPopup;
  FMINewTable:=AddNewItemPopup(eotTable,iiTable);
  FMINewField:=AddNewItemPopup(eotField,iiField);
  FMINewIndex:=AddNewItemPopup(eotIndex,iiIndex);
  FMINewSequence:=AddNewItemPopup(eotSequence,iiSequence);
  FMINewForeignKey:=AddNewItemPopup(eotForeignKey,iiForeignKey);
  FMINewDomain:=AddNewItemPopup(eotDomain,iiDomain);
  FMIDeleteObject:=TMenuItem.Create(Self);
  FMIDeleteObject.Caption:=Format(SDeleteObject,[SObject]);
  FMIDeleteObject.OnClick:=@DoDeleteObject;
  FMIDeleteObject.ImageIndex:=IIDelete;
  FMenu.Items.Add(FMIDeleteObject);
  FTV.PopupMenu:=FMenu;
  FIMgList:=TImageList.Create(Self);
  For I:=0 to IIMaxObject+1 do
    begin
    P:=TPortableNetworkGraphic.Create;
    try
      P.LoadFromResourceName(HInstance, ImageNames[i]);
      FImgList.Add(P,Nil);
    finally
      P.Free;
    end;
    end;
  FTV.Images:=FImgList;
  FMenu.Images:=FImgList;
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
    ShowGlobalObjectList(FTV,FTablesNode,eotTable,True);
{$ifndef onlyoldobjects}
    FSequencesNode:=NewNode(FTV,FDDNode,SNodeSequences,iiSequences);
    ShowGlobalObjectList(FTV,FSequencesNode,eotSequence,True);
    FDomainsNode:=NewNode(FTV,FDDNode,SNodeDomains,iiDomains);
    ShowGlobalObjectList(FTV,FDomainsNode,eotDomain,True);
{$endif onlyoldobjects}
    SetCaption;
    FTV.Selected:=FDDNode;
  finally
    FTV.Items.EndUpdate;
  end;
end;

Function TDataDictEditor.NewGlobalObject(AObjectName: String; AObjectType : TEditObjectType) : TObject;

Var
  II : Integer;
  N,PN : TTreeNode;
begin
  Case AObjectType of
    eotTable :
      begin
      Result:=FDD.Tables.AddTable(AObjectName);
      II:=IITable;
      PN:=FTablesNode;
      end;
{$ifndef onlyoldobjects}
    eotSequence :
      begin
      Result:=FDD.Sequences.AddSequence(AObjectName);
      II:=IISequence;
      PN:=FSequencesNode;
      end;
    eotDomain :
      begin
      Result:=FDD.Domains.AddDomain(AObjectName);
      II:=IIDomain;
      PN:=FDomainsNode;
      end;
{$endif onlyoldobjects}
  end;
  N:=NewNode(FTV,PN,AObjectName,II);
  N.Data:=Result;
  FTV.Selected:=N;
  ShowSubLists(FTV,N,Result);
  Modified:=True;
end;


Function TDataDictEditor.NewTableObject(AObjectName: String; TD: TDDTableDef; AObjectType : TEditObjectType) : TObject;

Var
  TN : TTreeNode;
  FD : TDDFieldDef;
  POT : TEditObjectType;
  II : Integer;

begin
  Case AObjectType of
    eotField         :
      begin
      POT:=eotFields;
      Result:=TD.Fields.AddField(AObjectName);
      II:=IIfield;
      end;
{$ifndef onlyoldobjects}
    eotIndex:
      begin
      POT:=eotIndexes;
      Result:=TD.Indexes.AddIndex(AObjectName);
      II:=IIIndex;
      end;
    eotForeignKey :
      begin
      POT:=eotForeignKeys;
      Result:=TD.foreignKeys.AddForeignKeyDef(AObjectName);
      II:=IIForeignkey;
      end;
{$endif onlyoldobjects}
  end;
  TN:=FindNodeWithData(FTV,TD);
  TN:=TN.GetFirstChild;
  While (TN<>Nil) and (GetObjectType(TN)<>POT) do
    TN:=TN.GetNextSibling;
  If (TN<>Nil) then
    begin
    TN:=NewNode(FTV,TN,AObjectName,II);
    TN.Data:=Result;
    FTV.Selected:=TN;
    Modified:=True;
    end
  else
    FreeAndNil(Result); // Error !!
end;


procedure TDataDictEditor.NewField(AFieldName: String; TD: TDDTableDef);


begin
  NewTableObject(AFieldName,TD,eotField);
end;

procedure TDataDictEditor.NewIndex(AIndexName: String; TD: TDDTableDef);

begin
  NewTableObject(AIndexName,TD,eotIndex);
end;

procedure TDataDictEditor.NewForeignKey(AKeyName: String; TD: TDDTableDef);


begin
  NewTableObject(AKeyName,TD,eotForeignKey);
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
    eotUnknown    : ;
    eotDictionary : SelectSingleObject(FDD);
    eotTables     : SelectGlobalObjectList(eotTable);
    eotTable      : SelectSingleObject(O as TPersistent);
    eotFields     : SelectTableObjectList(eotField,OP as TDDTableDef);
    eotField      : SelectSingleObject(O as TPersistent);
{$ifndef onlyoldobjects}
    eotIndexes    : SelectTableObjectList(eotIndex,OP as TDDTableDef);
    eotIndex      : SelectSingleObject(O as TPersistent);
    eotDomains      : SelectGlobalObjectList(eotDomain);
    eotDomain       : SelectSingleObject(O as TPersistent);
    eotSequences    : SelectGlobalObjectList(eotSequence);
    eotSequence     : SelectSingleObject(O as TPersistent);
    eotForeignKeys  : SelectTableObjectList(eotForeignKey,OP as TDDTableDef);
    eotForeignKey   : SelectSingleObject(O as TPersistent);
{$endif onlyoldobjects}
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
      eotField         : Text:=TDDFieldDef(N.Data).FieldName;
      eotDictionary    : Text:=TFPDataDictionary(N.Data).Name;
      eotTable         : Text:=TDDTableDef(N.Data).TableName;
{$ifndef onlyoldobjects}
      eotSequence   : Text:=TDDSequenceDef(N.Data).SequenceName;
      eotDomain     : Text:=TDDDomainDef(N.Data).DomainName;
      eotForeignKey : Text:=TDDForeignkeyDef(N.Data).KeyName;
{$endif onlyoldobjects}
    end;
end;

procedure TDataDictEditor.SelectGlobalObjectList(AObjectType : TEditObjectType);

Var
  TV : TTreeView;

begin
  ClearEditor;
  TV:=TTreeView.Create(Self);
  TV.ShowLines:=True;
  TV.Parent:=FEdit;
  TV.Align:=alClient;
  ShowGlobalObjectList(TV,Nil,AObjectType,False);
  TV.OnDblClick:=@DoDoubleClick;
  FAllowDoubleClick:=AObjectType;
end;

procedure TDataDictEditor.SelectTableObjectList(AObjectType: TEditObjectType; ATableDef : TDDTableDef);

Var
  TV : TTreeView;

begin
  ClearEditor;
  TV:=TTreeView.Create(Self);
  TV.ShowLines:=True;
  TV.Parent:=FEdit;
  TV.Align:=alClient;
  ShowTableObjectList(TV,Nil,ATableDef,AObjectType);
  TV.OnDblClick:=@DoDoubleClick;
  FAllowDoubleClick:=AObjectType;
end;

procedure TDataDictEditor.DoDoubleClick(Sender : TObject);

Var
  TV : TTreeView;
  N: TTreeNode;

begin
  TV:=Sender As TTreeView;
  N:=TV.Selected;
  If (GetObjectType(N)=FAllowDoubleClick) and (N.Data<>Nil) then
    FTV.Selected:=FindNodeWithData(FTV,N.Data);
end;

procedure TDataDictEditor.DoNewObject(Sender: TObject);

Var
  EOT : TEditObjectType;
  S,N : String;

begin
  EOT:=TEditObjectType((Sender as TMenuItem).Tag);
  S:=ObjectTypeName(EOT);
  if InputQuery(Format(SNewObject,[S]),Format(SNameFor,[S]),N) then
    begin
    case EOT of
      eotField : NewField(N,CurrentTable);
      eotIndex : NewIndex(N,CurrentTable);
      eotForeignKey : NewForeignKey(N,CurrentTable);
    else
      NewGlobalObject(N,EOT);
    end;
    end;
end;

procedure TDataDictEditor.DoDeleteObject(Sender: TObject);
begin
  DeleteCurrentObject;
end;

function TDataDictEditor.SelectNextNode(ANode: TTreeNode; ADefault : TTreeNode): TTreeNode;

Var
  NN : TTreeNode;

begin
  NN:=ANode.GetNextSibling;
  If (NN=Nil) then
    begin
    NN:=ANode.GetPrevSibling;
    If (NN=Nil) then
      if Assigned(ADefault) then
        NN:=ADefault
      else
        begin
        NN:=Anode.Parent;
        If Assigned(NN) then
          NN:=NN.Parent;
        end;
    end;
  ANode.Free;
  FTV.Selected:=NN;
  Result:=NN;
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

procedure TDataDictEditor.ClearEditor;

begin
  With FEdit do
    While (ControlCount>0) do
      Controls[ControlCount-1].Free;
end;

procedure TDataDictEditor.SelectSingleObject(AObject: TPersistent);
begin
  ClearEditor;
  CreatePropertyGrid(AObject);
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

function TDataDictEditor.GetObjectType(Node: TTreeNode): TEditObjectType;

Var
  I : Integer;
  
begin
  Result:=eotUnknown;
  If Node<>Nil then
    begin
    I:=Node.ImageIndex;
    I:=I-ImageOffset+1;
    If (I>=0) and (I<=Ord(High(TEditObjectType))) then
      Result:=TEditObjectType(I);
    end;
end;

procedure TDataDictEditor.GetTableObjectsList(ATabledef :TDDTableDef; AObjectType : TEditObjectType; List : TStrings);

Var
  I : Integer;

begin
  Case AObjectType of
    eotField :  For I:=0 to ATableDef.Fields.Count-1 do
                 List.AddObject(ATableDef.Fields[i].FieldName,ATableDef.Fields[i]);
{$ifndef onlyoldobjects}
    eotIndex :  For I:=0 to ATableDef.Indexes.Count-1 do
                 List.AddObject(ATableDef.Indexes[i].IndexName,ATableDef.Indexes[i]);
    eotForeignKey :  For I:=0 to ATableDef.ForeignKeys.Count-1 do
                 List.AddObject(ATableDef.ForeignKeys[i].KeyName,ATableDef.ForeignKeys[i]);
{$endif onlyoldobjects}
  end;
  If List is TStringList then
    TStringList(List).Sorted:=True;
end;

procedure TDataDictEditor.ShowTableObjectList(TV : TTreeView; ParentNode: TTreeNode; ATableDef: TDDTableDef;AObjectType : TEditObjectType);

Var
  TN : TTreeNode;
  TL : TStringList;
  II, I : Integer;

begin
  TL:=TStringList.Create;
  Try
    Case AObjectType of
      eotField : II:=iiField;
{$ifndef onlyoldobjects}
      eotIndex : II:=iiIndex;
      eotForeignKey : II:=iiForeignKey;
{$endif}
    end;
    GetTableObjectsList(ATableDef,AObjectType,TL);
    For I:=0 to TL.Count-1 do
      begin
      TN:=NewNode(TV,ParentNode,TL[i],II);
      TN.Data:=TL.Objects[i];
      end;
    If Assigned(ParentNode) then
      ParentNode.Expand(False);
  Finally
    FreeAndNil(TL);
  end;
end;

procedure TDataDictEditor.GetGlobalObjectsList(AObjectType: TEditObjectType;
  List: TStrings);

Var
  I : Integer;

begin
  Case AObjectType of
    eotTable:
      For I:=0 to FDD.Tables.Count-1 do
        List.AddObject(FDD.Tables[i].TableName,FDD.Tables[i]);
{$ifndef onlyoldobjects}
    eotSequence:
      For I:=0 to FDD.Sequences.Count-1 do
        List.AddObject(FDD.Sequences[i].SequenceName,FDD.Sequences[i]);

    eotDomain:
      For I:=0 to FDD.Domains.Count-1 do
        List.AddObject(FDD.Domains[i].DomainName,FDD.Domains[i]);
{$endif onlyoldobjects}
  end;
  If List is TStringList then
    TStringList(List).Sorted:=True;
end;

procedure TDataDictEditor.ShowSubLists(TV: TTreeView; ParentNode: TTreeNode; AObject : TObject);

Var
  TD : TDDTableDef;
  N : TTreeNode;
begin
  If AObject is TDDTableDef then
    begin
    TD:=AObject as TDDTableDef;
    N:=NewNode(TV,ParentNode,SNodeFields,iiFields);
    ShowTableObjectList(TV,N,TD,eotField);
{$ifndef onlyoldobjects}
    N:=NewNode(TV,ParentNode,SNodeIndexes,iiIndexes);
    ShowTableObjectList(TV,N,TD,eotIndex);
    N:=NewNode(TV,ParentNode,SNodeForeignKeys,iiForeignKeys);
    ShowTableObjectList(TV,N,TD,eotForeignKey);
{$endif onlyoldobjects}
    end;
end;

procedure TDataDictEditor.ShowGlobalObjectList(TV: TTreeView;
  ParentNode: TTreeNode; AObjectType: TEditObjectType; AShowSubLists : Boolean = False);

Var
  TN : TTreeNode;
  TL : TStringList;
  II, I : Integer;

begin
  TL:=TStringList.Create;
  Try
    Case AObjectType of
      eotTable    : II:=iiTable;
{$ifndef onlyoldobjects}
      eotSequence : II:=iiSequence;
      eotDomain   : II:=iiDomain;
{$endif onlyoldobjects}
    end;
    GetGlobalObjectsList(AObjectType,TL);
    For I:=0 to TL.Count-1 do
      begin
      TN:=NewNode(TV,ParentNode,TL[i],II);
      TN.Data:=TL.Objects[i];
      If AShowSubLists then
        ShowSubLists(TV,TN,TL.Objects[i]);
      end;
    If Assigned(ParentNode) then
      ParentNode.Expand(False);
  Finally
    FreeAndNil(TL);
  end;
end;

procedure TDataDictEditor.DeleteGlobalObject(AObject: TObject);

Var
  N,NN : TTreeNode;

begin
  N:=FindNodeWithData(FTV,Pointer(AObject));
  NN:=SelectNextNode(N,FDDNode);
  AObject.Free;
  Modified:=True;
end;

procedure TDataDictEditor.DeleteTableObject(AObject: TObject);

Var
  N,NN : TTreeNode;

begin
  N:=FindNodeWithData(FTV,Pointer(AObject));
  NN:=SelectNextNode(N,Nil);
  AObject.Free;
  Modified:=True;
end;

procedure TDataDictEditor.LoadFromFile(AFileName: String);
begin
  FDD.LoadFromFile(UTF8ToSys(AFileName));
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

begin
  DeleteGlobalObject(TD);
end;

procedure TDataDictEditor.DeleteField(FD: TDDFieldDef);

begin
  DeleteTableObject(FD);
end;

procedure TDataDictEditor.DeleteIndex(ID: TDDIndexDef);

begin
  DeleteTableObject(ID);
end;

procedure TDataDictEditor.DeleteCurrentObject;

Var
  N : TTreeNode;

begin
  N:=FTV.Selected;
  If GetCurrentObjectType in [eotField,eotIndex,eotForeignKey] then
    DeleteTableObject(TObject(N.Data))
  else
    DeleteGlobalObject(TObject(N.Data))
end;

{$ifndef onlyoldobjects}
procedure TDataDictEditor.DeleteSequence(SD: TDDSequenceDef);

begin
  DeleteGlobalObject(SD);
end;

procedure TDataDictEditor.DeleteDomain(DD: TDDDomainDef);

begin
  DeleteGlobalObject(DD);
end;

procedure TDataDictEditor.DeleteForeignKey(KD: TDDForeignKeyDef);

begin
  DeleteTableObject(KD);
end;
{$endif onlyoldobjects}

procedure TDataDictEditor.CreateCode;

Var
  TD : TDDTableDef;
  DS : TDataset;

begin
  TD:=CurrentTable;
  If Not assigned(TD) then
    exit;
  DS:=CreateDatasetFromTabledef(TD,Self);
  try
    With TFPCodeGenerator.Create(DS) do
      try
        DataSet:=DS;
        TableNameHint:=TD.TableName;
        Execute;
      Finally
        Free;
      end;
  finally
    DS.Free;
  end;
end;

end.

