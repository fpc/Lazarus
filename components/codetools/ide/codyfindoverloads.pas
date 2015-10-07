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

  Author: Mattias Gaertner

  Abstract:
    Shows all overloads of the procedure/method at sourcee editor position.
    Sortable columns:
      Path: unitnames.classes
      (If not filtered:) Compatibility: exact, compatible, incompatible
      Distance
      Last visited
    Filter:
      Params must be compatible: Yes, No
      (Only for method:) Only descendants, have same ancestor method, all
      (Only for method:) Show abstract methods and interfaces

  ToDo:
    -show line number
    -Sort columns
    -Jump to method
    -param compatibility
    -last visited
    -filter by ancestor
    -hint: show file name + param list
}
unit CodyFindOverloads;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, AVL_Tree, contnrs, FileUtil, LazLoggerBase, LazUtilities,
  CodyUtils, CodeToolManager, CodeTree, CodeCache, FindDeclarationTool,
  PascalParserTool, BasicCodeTools, CTUnitGraph, FileProcs, StdCodeTools,
  CodeGraph, LazIDEIntf, IDEWindowIntf, ProjectIntf, Forms, Controls, Graphics,
  Dialogs, ExtCtrls, StdCtrls, Grids, ComCtrls;

type
  TCFOUnit = class(TUGUnit)
  public
  end;

  TCFONode = class(TCodeGraphNode)
  public
    Tool: TFindDeclarationTool;

    // for ctnProcedure:
    Compatibility: TTypeCompatibility;
    Distance: integer;
    ShortestPathNode: TCFONode;
  end;

  TCFOEdgeType = (
    cfoetReachable, // FromNode (proc) is reachable by ToNode(program)
    cfoetMethodOf,  // FromNode (proc) is method of ToNode (class)
    cfoetDescendantOf  // FromNode (class) is descendant of ToNode (class)
    );

  TCFOEdge = class(TCodeGraphEdge)
  public
    Typ: TCFOEdgeType;
  end;

  TCFOProc = class
  public
    XYPos: TCodeXYPosition;
    Name: string;
    ClassPath: string;
    TheUnitName: string;
    Params: string;
    Distance: integer;
    Compatibility: TTypeCompatibility;
  end;

type
  TCFOFlag = (
    cfofParsing,
    cfofGatherProcs
    );
  TCFOFlags = set of TCFOFlag;

  { TCodyFindOverloadsWindow }

  TCodyFindOverloadsWindow = class(TForm)
    BtnPanel: TPanel;
    CompatibleParamsCheckBox: TCheckBox;
    FilterGroupBox: TGroupBox;
    HideAbstractCheckBox: TCheckBox;
    JumpToButton: TButton;
    ProgressBar1: TProgressBar;
    RefreshButton: TButton;
    RelationComboBox: TComboBox;
    RelationLabel: TLabel;
    ResultsGroupBox: TGroupBox;
    ResultsStringGrid: TStringGrid;
    Timer1: TTimer;
    procedure FormClose(Sender: TObject; var {%H-}CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure JumpToButtonClick(Sender: TObject);
    procedure OnIdle(Sender: TObject; var Done: Boolean);
    procedure RefreshButtonClick(Sender: TObject);
    procedure ResultsStringGridCompareCells(Sender: TObject; ACol, ARow, BCol,
      BRow: Integer; var Result: integer);
    procedure Timer1Timer(Sender: TObject);
  private
    FIdleConnected: boolean;
    FFlags: TCFOFlags;
    FProcList: TObjectList;
    FTargetName: string;
    FTargetPath: string;
    FTargetXYPosition: TCodeXYPosition;
    FUsesGraph: TUsesGraph;
    function GetProcCount: integer;
    function GetProcs(Index: integer): TCFOProc;
    procedure SetIdleConnected(AValue: boolean);
    procedure CreateUsesGraph(out TheUsesGraph: TUsesGraph);
    procedure StartParsing;
    procedure AbortParsing;
    procedure AddStartAndTargetUnits;
    procedure GatherProcsOfAllUnits;
    procedure GatherProcsOfUnit(NodeGraph: TCodeGraph; ProgNode: TCodeTreeNode;
      CurUnit: TCFOUnit; ExcludeAbstractProcs: boolean;
      var TargetGraphNode: TCFONode);
    procedure CalcDistances(NodeGraph: TCodeGraph; TargetGraphNode: TCFONode);
    procedure CreateProcList(NodeGraph: TCodeGraph; TargetGraphNode: TCFONode);
    procedure FillGrid;
    procedure FreeUsesGraph;
    function GetDefaultCaption: string;
    procedure FillFilterControls(ProcTool: TFindDeclarationTool;
      ProcNode: TCodeTreeNode);
  protected
    procedure UpdateShowing; override;
  public
    procedure JumpToIdentifier;
    function Init: boolean;
    property IdleConnected: boolean read FIdleConnected write SetIdleConnected;
    property ProcCount: integer read GetProcCount;
    property Procs[Index: integer]: TCFOProc read GetProcs;
    property TargetXYPosition: TCodeXYPosition read FTargetXYPosition;
    property TargetName: string read FTargetName;
    property TargetPath: string read FTargetPath;
    property UsesGraph: TUsesGraph read FUsesGraph;
  end;

var
  CodyFindOverloadsWindow: TCodyFindOverloadsWindow;

procedure ShowFindOverloadsClicked(Sender: TObject);
procedure ShowFindOverloads(State: TIWGetFormState = iwgfShowOnTop);

function CompareCFONodeByDistance(Node1, Node2: Pointer): integer;

implementation

procedure ShowFindOverloadsClicked(Sender: TObject);
begin
  ShowFindOverloads;
end;

procedure ShowFindOverloads(State: TIWGetFormState);
begin
  if CodyFindOverloadsWindow = nil then
    IDEWindowCreators.CreateForm(CodyFindOverloadsWindow,TCodyFindOverloadsWindow,
       State=iwgfDisabled,LazarusIDE.OwningComponent)
  else if State=iwgfDisabled then
    CodyFindOverloadsWindow.DisableAlign;
  if State>=iwgfShow then
    IDEWindowCreators.ShowForm(CodyFindOverloadsWindow,State=iwgfShowOnTop);
end;

function CompareCFONodeByDistance(Node1, Node2: Pointer): integer;
var
  n1: TCFONode absolute Node1;
  n2: TCFONode absolute Node2;
begin
  if n1.Distance<n2.Distance then
    exit(-1)
  else if n1.Distance>n2.Distance then
    exit(1)
  else
    Result:=ComparePointers(n1.Node,n2.Node);
end;

{$R *.lfm}

{ TCodyFindOverloadsWindow }

procedure TCodyFindOverloadsWindow.FormCreate(Sender: TObject);
begin
  FProcList:=TObjectList.Create(true);

  Caption:=GetDefaultCaption;
  RefreshButton.Caption:='Refresh';
  JumpToButton.Caption:='Jump to';

  FilterGroupBox.Caption:='Filter';
  CompatibleParamsCheckBox.Caption:='Only procedures with compatible parameters';
  CompatibleParamsCheckBox.Hint:='If unchecked list also procedures with same name, but incompatible parameter lists.';
  HideAbstractCheckBox.Caption:='Hide abstract methods and methods of class interfaces';
  RelationLabel.Caption:='Relations:';
end;

procedure TCodyFindOverloadsWindow.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FProcList);
end;

procedure TCodyFindOverloadsWindow.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  AbortParsing;
  FreeUsesGraph;
end;

procedure TCodyFindOverloadsWindow.JumpToButtonClick(Sender: TObject);
begin

end;

procedure TCodyFindOverloadsWindow.OnIdle(Sender: TObject; var Done: Boolean);
var
  Completed: boolean;
begin
  if cfofParsing in FFlags then begin
    fUsesGraph.Parse(true,Completed,200);
    if Completed then begin
      FFlags:=FFlags-[cfofParsing]+[cfofGatherProcs];
    end;
  end else if cfofGatherProcs in FFlags then begin
    GatherProcsOfAllUnits;
  end else
    IdleConnected:=false;
  Done:=not IdleConnected;
end;

procedure TCodyFindOverloadsWindow.RefreshButtonClick(Sender: TObject);
begin
  if cfofParsing in FFlags then exit;
  Init;
end;

procedure TCodyFindOverloadsWindow.ResultsStringGridCompareCells(
  Sender: TObject; ACol, ARow, BCol, BRow: Integer; var Result: integer);
begin
  if (aRow>0) and (ARow<=ProcCount) then begin

  end;
end;

procedure TCodyFindOverloadsWindow.Timer1Timer(Sender: TObject);
var
  Cnt: Integer;
begin
  if (FUsesGraph=nil) then exit;
  Cnt:=0;
  if FUsesGraph.FilesTree<>nil then
    Cnt:=FUsesGraph.FilesTree.Count;
  ResultsGroupBox.Caption:=Format('Scanning: %s units ...', [IntToStr(Cnt)]);
end;

procedure TCodyFindOverloadsWindow.SetIdleConnected(AValue: boolean);
begin
  if FIdleConnected=AValue then Exit;
  FIdleConnected:=AValue;
  if Application=nil then exit;
  if IdleConnected then
    Application.AddOnIdleHandler(@OnIdle)
  else
    Application.RemoveOnIdleHandler(@OnIdle);
end;

function TCodyFindOverloadsWindow.GetProcCount: integer;
begin
  Result:=FProcList.Count;
end;

function TCodyFindOverloadsWindow.GetProcs(Index: integer): TCFOProc;
begin
  Result:=TCFOProc(FProcList[Index]);
end;

procedure TCodyFindOverloadsWindow.CreateUsesGraph(out TheUsesGraph: TUsesGraph
  );
begin
  TheUsesGraph:=CodeToolBoss.CreateUsesGraph;
  if not TCFOUnit.InheritsFrom(TheUsesGraph.UnitClass) then
    RaiseCatchableException('');
  TheUsesGraph.UnitClass:=TCFOUnit;
end;

procedure TCodyFindOverloadsWindow.StartParsing;
begin
  if (FUsesGraph<>nil) or (cfofParsing in FFlags) then
    RaiseCatchableException('');
  Include(FFlags,cfofParsing);

  ProgressBar1.Visible:=true;
  ProgressBar1.Style:=pbstMarquee;
  ResultsGroupBox.Caption:='Scanning ...';
  Timer1.Enabled:=true;
  RefreshButton.Enabled:=false;

  CreateUsesGraph(FUsesGraph);

  LazarusIDE.BeginCodeTools;
  AddStartAndTargetUnits;

  IdleConnected:=true;
end;

procedure TCodyFindOverloadsWindow.AbortParsing;
begin
  FFlags:=[];
  IdleConnected:=false;
  ProgressBar1.Visible:=false;
  RefreshButton.Enabled:=true;
end;

procedure TCodyFindOverloadsWindow.AddStartAndTargetUnits;
var
  aProject: TLazProject;
begin
  FUsesGraph.TargetAll:=true;
  // project lpr
  aProject:=LazarusIDE.ActiveProject;
  if (aProject<>nil) and (aProject.MainFile<>nil) then
    FUsesGraph.AddStartUnit(aProject.MainFile.Filename);
end;

procedure TCodyFindOverloadsWindow.GatherProcsOfAllUnits;
var
  FileNode: TAVLTreeNode;
  CurUnit: TCFOUnit;
  NodeGraph: TCodeGraph;
  ProgNode: TCodeTreeNode;
  ExcludeAbstractProcs: Boolean;
  TargetGraphNode: TCFONode;
begin
  Exclude(FFlags,cfofGatherProcs);
  Timer1.Enabled:=false;
  // hide progress bar and update stats
  ProgressBar1.Visible:=false;
  RefreshButton.Enabled:=true;
  ResultsGroupBox.Caption:=Format('Units: %s', [IntToStr(FUsesGraph.FilesTree.Count)]);

  if FUsesGraph=nil then
    exit;
  debugln(['TCodyFindOverloadsWindow.GatherProcsOfAllUnits START']);

  ExcludeAbstractProcs:=HideAbstractCheckBox.Checked;
  ProgNode:=TCodeTreeNode.Create;
  NodeGraph:=TCodeGraph.Create(TCFONode,TCFOEdge);
  try
    NodeGraph.AddGraphNode(ProgNode);

    TargetGraphNode:=nil;
    FileNode:=FUsesGraph.FilesTree.FindLowest;
    while FileNode<>nil do begin
      CurUnit:=TCFOUnit(FileNode.Data);
      GatherProcsOfUnit(NodeGraph,ProgNode,CurUnit,ExcludeAbstractProcs,
        TargetGraphNode);
      FileNode:=FUsesGraph.FilesTree.FindSuccessor(FileNode);
    end;

    // ToDo: filter using RelationComboBox

    if TargetGraphNode<>nil then
      CalcDistances(NodeGraph,TargetGraphNode);

    CreateProcList(NodeGraph,TargetGraphNode);

    FillGrid;
  finally
    NodeGraph.Free;
    ProgNode.Free;
  end;
  debugln(['TCodyFindOverloadsWindow.GatherProcsOfAllUnits END']);
end;

procedure TCodyFindOverloadsWindow.GatherProcsOfUnit(NodeGraph: TCodeGraph;
  ProgNode: TCodeTreeNode; CurUnit: TCFOUnit; ExcludeAbstractProcs: boolean;
  var TargetGraphNode: TCFONode);

  procedure AddAncestors(Tool: TFindDeclarationTool; ClassNode: TCodeTreeNode); forward;

  function AddClassNode(Tool: TFindDeclarationTool; ClassNode: TCodeTreeNode): TCFONode;
  var
    Edge: TCFOEdge;
  begin
    if ClassNode=nil then
      RaiseCatchableException('');
    Result:=TCFONode(NodeGraph.GetGraphNode(ClassNode,false));
    if Result<>nil then exit;
    //debugln(['AddClassNode ',Tool.ExtractClassName(ClassNode,false)]);
    Result:=TCFONode(NodeGraph.AddGraphNode(ClassNode));
    Result.Tool:=Tool;
    // create edge "reachable", so that all nodes are reachable
    Edge:=TCFOEdge(NodeGraph.AddEdge(ClassNode,ProgNode));
    Edge.Typ:=cfoetReachable;
    AddAncestors(Tool,ClassNode);
  end;

  procedure AddAncestors(Tool: TFindDeclarationTool; ClassNode: TCodeTreeNode);
  var
    ListOfPFindContext: TFPList;
    Params: TFindDeclarationParams;
    Ancestor: PFindContext;
    i: Integer;
    Edge: TCFOEdge;
  begin
    //debugln(['AddAncestors ',Tool.ExtractClassName(ClassNode,false)]);
    ListOfPFindContext:=nil;
    Params:=TFindDeclarationParams.Create(nil);
    try
      Tool.FindAncestorsOfClass(ClassNode,ListOfPFindContext,Params,true,false);
      if ListOfPFindContext<>nil then begin
        for i:=0 to ListOfPFindContext.Count-1 do begin
          Ancestor:=PFindContext(ListOfPFindContext[i]);
          AddClassNode(Ancestor^.Tool,Ancestor^.Node);
          // create edge "descendant of"
          Edge:=TCFOEdge(NodeGraph.AddEdge(ClassNode,Ancestor^.Node));
          Edge.Typ:=cfoetDescendantOf;
        end;
      end;
    finally
      Params.Free;
      FreeListOfPFindContext(ListOfPFindContext);
    end;
  end;

  procedure AddProcNode(Tool: TFindDeclarationTool; ProcNode: TCodeTreeNode;
    TargetCleanPos: integer);
  var
    CurProcName: String;
    GraphProcNode: TCFONode;
    ClassNode: TCodeTreeNode;
    Edge: TCFOEdge;
    Compatibility: TTypeCompatibility;
    IsTargetProc: Boolean;
  begin
    // check name
    CurProcName:=Tool.ExtractProcName(ProcNode,[phpWithoutClassName]);
    if CompareIdentifiers(PChar(CurProcName),PChar(FTargetName))<>0 then exit;

    debugln(['TCodyFindOverloadsWindow.GatherProcsOfUnit ',Tool.CleanPosToStr(ProcNode.StartPos,true)]);

    // check if method
    ClassNode:=ProcNode.Parent;
    while ClassNode<>nil do begin
      if ClassNode.Desc in AllClasses then break;
      ClassNode:=ClassNode.Parent;
    end;
    if ClassNode<>nil then begin
      if ExcludeAbstractProcs then begin
        if ClassNode.Desc in AllClassInterfaces then exit;
        if Tool.ProcNodeHasSpecifier(ProcNode,psABSTRACT) then exit;
      end;
    end;

    Compatibility:=tcExact;
    if (TargetGraphNode=nil)
    and (ProcNode.StartPos<=TargetCleanPos)
    and (TargetCleanPos<ProcNode.FirstChild.EndPos) then begin
      // this is the target proc
      IsTargetProc:=true;
    end else begin
      // ToDo: check param compatibility
      IsTargetProc:=false;

    end;

    // add node
    GraphProcNode:=TCFONode(NodeGraph.AddGraphNode(ProcNode));
    GraphProcNode.Tool:=Tool;
    GraphProcNode.Compatibility:=Compatibility;
    if IsTargetProc then
      TargetGraphNode:=GraphProcNode;

    // add edges
    if ClassNode<>nil then begin
      // create nodes for class and ancestors
      AddClassNode(Tool,ClassNode);
      // create edge "is method of"
      Edge:=TCFOEdge(NodeGraph.AddEdge(ProcNode,ClassNode));
      Edge.Typ:=cfoetMethodOf;
    end else begin
      // not a method
      // create edge "reachable", so that all nodes are reachable
      Edge:=TCFOEdge(NodeGraph.AddEdge(ProcNode,ProgNode));
      Edge.Typ:=cfoetReachable;
    end;
  end;

var
  Tool: TStandardCodeTool;
  ProcNode: TCodeTreeNode;
  TargetCleanPos: integer;
begin
  if ugufLoadError in CurUnit.Flags then exit;
  if not (ugufReached in CurUnit.Flags) then exit; // this unit was not reached
  if ugufIsIncludeFile in CurUnit.Flags then exit;
  Tool:=CurUnit.Tool;
  if (TargetGraphNode<>nil)
  or (Tool.CaretToCleanPos(TargetXYPosition,TargetCleanPos)<>0) then
    TargetCleanPos:=0;

  ProcNode:=Tool.Tree.Root;
  while ProcNode<>nil do begin
    if ProcNode.Desc in [ctnImplementation,ctnBeginBlock] then break;
    if ProcNode.Desc=ctnProcedure then
      AddProcNode(Tool,ProcNode,TargetCleanPos);
    ProcNode:=ProcNode.Next;
  end;
end;

procedure TCodyFindOverloadsWindow.CalcDistances(NodeGraph: TCodeGraph;
  TargetGraphNode: TCFONode);
var
  Unvisited: TAVLTree;

  procedure UpdateDistancesAlongEdges(GraphNode: TCFONode; Edges: TAVLTree);
  var
    AVLNode: TAVLTreeNode;
    Edge: TCFOEdge;
    NewDistance: Integer;
    OtherNode: TCFONode;
    WasUnvisited: Boolean;
  begin
    if Edges=nil then exit;
    AVLNode:=Edges.FindLowest;
    while AVLNode<>nil do begin
      Edge:=TCFOEdge(AVLNode.Data);
      NewDistance:=GraphNode.Distance;
      case Edge.Typ of
      cfoetReachable: NewDistance+=100000;// not related
      cfoetMethodOf: ; // methods within one class are close
      cfoetDescendantOf:
        if GraphNode=Edge.FromNode then
          NewDistance+=10  // going to the ancestors
        else
          NewDistance+=1;  // going to the descendants
      end;
      if GraphNode=Edge.FromNode then begin
        OtherNode:=TCFONode(Edge.ToNode);
      end else begin
        OtherNode:=TCFONode(Edge.FromNode);
      end;
      if NewDistance<OtherNode.Distance then begin
        WasUnvisited:=Unvisited.Find(OtherNode)<>nil;
        if WasUnvisited then
          Unvisited.Remove(OtherNode);
        OtherNode.Distance:=NewDistance;
        OtherNode.ShortestPathNode:=GraphNode;
        if WasUnvisited then
          Unvisited.Add(OtherNode);
      end;
      AVLNode:=Edges.FindSuccessor(AVLNode);
    end;
  end;

var
  AVLNode: TAVLTreeNode;
  GraphNode: TCFONode;
begin
  debugln(['TCodyFindOverloadsWindow.CalcDistances ']);

  Unvisited:=TAVLTree.Create(@CompareCFONodeByDistance);
  try
    // Dijkstra's shotest path algorithm

    // build Unvisited queue, set Distance of TargetGraphNode to 0
    // infinite Distance all other
    AVLNode:=NodeGraph.Nodes.FindLowest;
    while AVLNode<>nil do begin
      GraphNode:=TCFONode(AVLNode.Data);
      if GraphNode=TargetGraphNode then
        GraphNode.Distance:=0
      else
        GraphNode.Distance:=High(integer);
      Unvisited.Add(GraphNode);
      AVLNode:=NodeGraph.Nodes.FindSuccessor(AVLNode);
    end;

    // for each node with minimum distance ...
    while Unvisited.Count>0 do begin
      // get unvisited node with minimum distance
      AVLNode:=Unvisited.FindLowest;
      GraphNode:=TCFONode(AVLNode.Data);
      //debugln(['TCodyFindOverloadsWindow.CalcDistances GraphNode=',GraphNode.Tool.ExtractProcName(GraphNode.Node,[phpAddClassName]),' Distance=',GraphNode.Distance]);
      Unvisited.Delete(AVLNode);
      UpdateDistancesAlongEdges(GraphNode,GraphNode.InTree);
      UpdateDistancesAlongEdges(GraphNode,GraphNode.OutTree);
    end;
  finally
    Unvisited.Free;
  end;
end;

procedure TCodyFindOverloadsWindow.CreateProcList(NodeGraph: TCodeGraph;
  TargetGraphNode: TCFONode);
var
  AVLNode: TAVLTreeNode;
  aProc: TCFOProc;
  GraphNode: TCFONode;
  Tool: TFindDeclarationTool;
  Node: TCodeTreeNode;
begin
  FProcList.Clear;

  AVLNode:=NodeGraph.Nodes.FindLowest;
  while AVLNode<>nil do begin
    GraphNode:=TCFONode(AVLNode.Data);
    AVLNode:=NodeGraph.Nodes.FindSuccessor(AVLNode);
    if GraphNode=TargetGraphNode then continue;
    if GraphNode.Node.Desc<>ctnProcedure then continue;
    aProc:=TCFOProc.Create;

    Tool:=GraphNode.Tool;
    Node:=GraphNode.Node;
    Tool.CleanPosToCaret(Node.FirstChild.StartPos,aProc.XYPos);
    aProc.Name:=Tool.ExtractProcName(Node,[phpWithoutClassName]);
    aProc.ClassPath:=Tool.ExtractClassPath(Node);
    aProc.TheUnitName:=Tool.GetSourceName(false);
    aProc.Compatibility:=GraphNode.Compatibility;
    aProc.Distance:=GraphNode.Distance;
    FProcList.Add(aProc);
  end;
end;

procedure TCodyFindOverloadsWindow.FillGrid;
var
  Grid: TStringGrid;
  Row: Integer;
  s: String;
  aProc: TCFOProc;
begin
  Grid:=ResultsStringGrid;
  Grid.BeginUpdate;
  Grid.Visible:=true;
  Grid.Columns[0].Title.Caption:='Name';
  Grid.Columns[1].Title.Caption:='Compatibility';
  Grid.Columns[2].Title.Caption:='Distance';

  Grid.RowCount:=ProcCount+1;
  for Row:=1 to ProcCount do begin
    aProc:=Procs[Row-1];

    // path
    s:=aProc.TheUnitName+': ';
    if aProc.ClassPath<>'' then
      s+=aProc.ClassPath+'.';
    s+=aProc.Name;
    Grid.Cells[0,Row]:=s;

    case aProc.Compatibility of
    tcExact: s:='fits exactly';
    tcCompatible: s:='compatible';
    tcIncompatible: s:='incompatible';
    end;
    Grid.Cells[1,Row]:=s;

    Grid.Cells[2,Row]:=IntToStr(aProc.Distance);
  end;

  Grid.EndUpdate(true);

  Grid.HandleNeeded;

  // ToDo: resize columns

end;

procedure TCodyFindOverloadsWindow.FreeUsesGraph;
begin
  FreeAndNil(FUsesGraph);
end;

function TCodyFindOverloadsWindow.GetDefaultCaption: string;
begin
  Result:='Cody - Find Overloads';
end;

procedure TCodyFindOverloadsWindow.FillFilterControls(
  ProcTool: TFindDeclarationTool; ProcNode: TCodeTreeNode);
var
  sl: TStringList;
  ClassNode: TCodeTreeNode;
  ListOfPFindContext: TFPList;
  i: Integer;
  aContext: PFindContext;
begin
  // RelationComboBox
  sl:=TStringList.Create;
  try
    ClassNode:=ProcNode;
    while (ClassNode<>nil) and (not (ClassNode.Desc in AllClasses)) do
      ClassNode:=ClassNode.Parent;
    if ClassNode<>nil then begin
      // method
      sl.Add('Only descendants of '+ProcTool.ExtractClassName(ClassNode,false));
      ListOfPFindContext:=nil;
      try
        ProcTool.FindClassAndAncestors(ClassNode,ListOfPFindContext,false);
        if ListOfPFindContext<>nil then begin
          for i:=0 to ListOfPFindContext.Count-1 do begin
            aContext:=PFindContext(ListOfPFindContext[i]);
            sl.Add('Only descendants of '+aContext^.Tool.ExtractClassName(aContext^.Node,false));
          end;
        end;
      finally
        FreeListOfPFindContext(ListOfPFindContext);
      end;
      sl.Add('Only methods');
    end else begin
      // procedure, non method
      sl.Add('Only non methods');
    end;
    sl.Add('Any');
    RelationComboBox.Items:=sl;
    if sl.IndexOf(RelationComboBox.Text)<0 then
      RelationComboBox.Text:='Any';
  finally
    sl.Free;
  end;
end;

procedure TCodyFindOverloadsWindow.UpdateShowing;
begin
  inherited UpdateShowing;
  if IsVisible and (FUsesGraph=nil) then begin
    Init;
  end;
end;

procedure TCodyFindOverloadsWindow.JumpToIdentifier;
begin

end;

function TCodyFindOverloadsWindow.Init: boolean;
var
  CurTool: TCodeTool;
  CurNode: TCodeTreeNode;
  CurCodePos: TCodeXYPosition;
  CurCleanPos: integer;

  procedure FindProcDeclaration(ProcTool: TFindDeclarationTool;
    var ProcNode: TCodeTreeNode);
  // find the method declaration of a method body
  // find the forward or interface declaration of a proc body
  var
    Node: TCodeTreeNode;
  begin
    if ProcNode=nil then exit;
    if ProcNode.Desc=ctnProcedureHead then
      ProcNode:=ProcNode.Parent;
    if ProcNode.Desc<>ctnProcedure then exit;
    if ProcNode.Parent.Desc=ctnInterface then exit;
    if (ProcNode.Parent.Desc=ctnImplementation)
    or ProcTool.NodeIsMethodBody(ProcNode)
    or (not ProcTool.NodeIsForwardProc(ProcNode)) then begin
      Node:=ProcTool.FindCorrespondingProcNode(ProcNode,[phpWithoutClassName]);
      if Node=nil then exit;
      ProcNode:=Node;
    end;
  end;

  function CheckCursorAtProcCall(StatementNode: TCodeTreeNode;
    out ProcTool: TFindDeclarationTool; out ProcNode: TCodeTreeNode): boolean;
  var
    CurIdentStart, CurIdentEnd, NewTopLine: integer;
    NewTool: TFindDeclarationTool;
    NewNode: TCodeTreeNode;
    NewPos: TCodeXYPosition;
  begin
    Result:=true;
    ProcTool:=nil;
    ProcNode:=nil;
    if StatementNode=nil then exit;
    // cursor in statement => check if on a proc call.
    if (CurCodePos.Code=nil) then exit;
    GetIdentStartEndAtPosition(CurTool.Src,CurCleanPos,CurIdentStart,CurIdentEnd);
    if CurIdentStart>=CurIdentEnd then exit;
    DebugLn(['TCodyFindOverloadsDialog.Init.IsCursorAtProcCall checking identifier "',copy(CurTool.Src,CurIdentStart,CurIdentEnd-CurIdentStart),'"']);
    if not CurTool.FindDeclaration(CurCodePos,DefaultFindSmartFlags,
      NewTool,NewNode,NewPos,NewTopLine)
    then begin
      ResultsGroupBox.Caption:='Parse error';
      LazarusIDE.DoJumpToCodeToolBossError;
      exit(false);
    end;
    if NewNode.Desc in [ctnProcedure,ctnProcedureHead] then begin
      ProcTool:=NewTool;
      ProcNode:=NewNode;
      FindProcDeclaration(ProcTool,ProcNode);
      debugln(['TCodyFindOverloadsDialog.Init.CheckCursorAtProcCall TargetProc ',ProcTool.CleanPosToStr(ProcNode.StartPos,true),' Class=',ProcTool.ExtractProcName(ProcNode,[phpAddClassName])]);
    end;
  end;

var
  ProcNode, Node, BeginNode, TargetProcNode: TCodeTreeNode;
  ErrorHandled: boolean;
  CurInitError: TCUParseError;
  TargetTool: TFindDeclarationTool;
begin
  Result:=false;
  Caption:=GetDefaultCaption;

  AbortParsing;
  ResultsStringGrid.Visible:=false;
  FTargetName:='';
  FTargetPath:='';
  FTargetXYPosition:=CleanCodeXYPosition;
  FreeUsesGraph;

  // parse source
  CurInitError:=ParseTilCursor(CurTool, CurCleanPos, CurNode, ErrorHandled, true, @CurCodePos);
  if CurInitError<>cupeSuccess then begin
    ResultsGroupBox.Caption:='Parse error';
    exit;
  end;

  // find target proc node
  ProcNode:=nil;
  BeginNode:=nil;
  Node:=CurNode;
  TargetTool:=nil;
  TargetProcNode:=nil;
  while Node<>nil do begin
    if Node.Desc=ctnProcedure then begin
      ProcNode:=Node;
      break;
    end else if (BeginNode=nil) and (Node.Desc=ctnBeginBlock) then begin
      BeginNode:=Node;
      if not CheckCursorAtProcCall(BeginNode,TargetTool,TargetProcNode) then
        exit;
    end;
    Node:=Node.Parent;
  end;

  FindProcDeclaration(CurTool,ProcNode);
  if ProcNode<>nil then begin
    // ToDo: add to visited procs
    debugln(['TCodyFindOverloadsDialog.Init ContextProc ',CurTool.CleanPosToStr(ProcNode.StartPos,true),' Class=',CurTool.ExtractProcName(ProcNode,[phpAddClassName])]);
  end;

  if TargetProcNode=nil then begin
    TargetTool:=CurTool;
    TargetProcNode:=ProcNode;
  end;
  if TargetProcNode=nil then begin
    ResultsGroupBox.Caption:='Need source editor at procedure call or declaration';
    exit;
  end;

  FTargetName:=TargetTool.ExtractProcName(TargetProcNode,[phpWithoutClassName]);
  FTargetPath:=TargetTool.ExtractProcName(TargetProcNode,[phpAddClassName]);
  TargetTool.CleanPosToCaret(TargetProcNode.StartPos,FTargetXYPosition);
  Caption:=GetDefaultCaption+' - '+FTargetPath;

  FillFilterControls(TargetTool,TargetProcNode);

  StartParsing;

  Result:=true;
end;

end.

