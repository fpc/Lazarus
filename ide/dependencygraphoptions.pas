unit DependencyGraphOptions;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ButtonPanel, StdCtrls,
  ExtCtrls, Spin, LvlGraphCtrl, Laz_XMLCfg, math, LazarusIDEStrConsts,
  EnvironmentOpts;

type

  { TLvlGraphOptions }

  TLvlGraphOptions = class(TIDESubOptions)
  private
    FCaptionPos: TLvlGraphNodeCaptionPosition;
    FEdgeShape: TLvlGraphEdgeShape;
    FEdgeSplit: TLvlGraphEdgeSplitMode;
    FExtraSpacingHoriz: integer;
    FExtraSpacingVert: integer;
    FHighLevels: Boolean;
    FLimitLvlHeighAbs: integer;
    FLimitLvlHeighRel: Single;
    FMinimizeEdges: Boolean;
    FOnLoaded: TNotifyEvent;
    FReduceBackEdges: Boolean;
    FStraightenGraph: Boolean;
  public
    constructor Create;
    procedure ReadFromXml(AnXmlConf: TRttiXMLConfig; APath: String); override;
    procedure WriteToXml(AnXmlConf: TRttiXMLConfig; APath: String); override;
    procedure Assign(ASrc: TPersistent); override;
    procedure WriteToGraph(AValue: TLvlGraphControl);
    procedure ReadFromGraph(AValue: TLvlGraphControl);
  published
    property MinimizeEdges: Boolean read FMinimizeEdges write FMinimizeEdges;
    property HighLevels: Boolean read FHighLevels write FHighLevels;
    property ReduceBackEdges: Boolean read FReduceBackEdges write FReduceBackEdges;
    property ExtraSpacingHoriz: integer read FExtraSpacingHoriz write FExtraSpacingHoriz;
    property ExtraSpacingVert: integer read FExtraSpacingVert write FExtraSpacingVert;
    property CaptionPos: TLvlGraphNodeCaptionPosition read FCaptionPos write FCaptionPos;
    property EdgeShape: TLvlGraphEdgeShape read FEdgeShape write FEdgeShape;
    property EdgeSplit: TLvlGraphEdgeSplitMode read FEdgeSplit write FEdgeSplit;
    property StraightenGraph: Boolean read FStraightenGraph write FStraightenGraph;
    property LimitLvlHeighAbs: integer read FLimitLvlHeighAbs write FLimitLvlHeighAbs;
    property LimitLvlHeighRel: Single read FLimitLvlHeighRel write FLimitLvlHeighRel;

    property OnLoaded: TNotifyEvent read FOnLoaded write FOnLoaded;
  end;

  TApplyOptionsProc = procedure(AnOpts: TLvlGraphOptions; AGraph: TLvlGraph) of object;
  TLvlGraphEdgeSplitModes = set of TLvlGraphEdgeSplitMode;

  { TDependencyGraphOptDialog }

  TDependencyGraphOptDialog = class(TForm)
    ApplyButton: TPanelBitBtn;
    ButtonPanel1: TButtonPanel;
    chkReduceBackEdges: TCheckBox;
    chkStraigtenGraph: TCheckBox;
    chkMinimizeEdges: TCheckBox;
    chkHighEdges: TCheckBox;
    chkCaptionOnTop: TCheckBox;
    dropEdgeShape: TComboBox;
    dropEdgeSplit: TComboBox;
    DummySpaceHolder: TLabel;
    lblExtraSpacing: TLabel;
    Panel11: TPanel;
    Panel12: TPanel;
    spinVertSpacing: TSpinEdit;
    spinHorizSpacing: TSpinEdit;
    spinLvlLimitRel: TFloatSpinEdit;
    lblMaxLevelHeight: TLabel;
    lblEdgeShape: TLabel;
    lblEdgeSplit: TLabel;
    lblSplitCount: TLabel;
    Panel1: TPanel;
    Panel10: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    Panel4: TPanel;
    Panel5: TPanel;
    Panel6: TPanel;
    Panel7: TPanel;
    Panel8: TPanel;
    Panel9: TPanel;
    spinLvlLimitAbs: TSpinEdit;
    valSplitCnt: TLabel;
    valLevelCnt: TLabel;
    valCrossCnt: TLabel;
    LblCrossCount: TLabel;
    lblNodeCnt: TLabel;
    valNodeCnt: TLabel;
    lblEdgeCount: TLabel;
    valEdgeCnt: TLabel;
    lblEdgeLen: TLabel;
    valEdgeLen: TLabel;
    LblLevelCount: TLabel;
    OptionsGroup: TGroupBox;
    InfoGroup: TGroupBox;
    ScrollBox1: TScrollBox;
    procedure ApplyButtonClick(Sender: TObject);
    procedure CancelButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure OKButtonClick(Sender: TObject);
  private
    FApplyCallback: TApplyOptionsProc;
    FGraph: TLvlGraph;
    FOptions: TLvlGraphOptions;
    FShowInfo: Boolean;
    FShowSplitModes: TLvlGraphEdgeSplitModes;
    FEdgeSplitMap: array[0..ord(high(TLvlGraphEdgeSplitMode))] of TLvlGraphEdgeSplitMode;
    FEdgeRevSplitMap: array[low(TLvlGraphEdgeSplitMode)..high(TLvlGraphEdgeSplitMode)] of integer;
    procedure SetApplyCallback(AValue: TApplyOptionsProc);
    procedure SetGraph(AValue: TLvlGraph);
    procedure SetOptions(AValue: TLvlGraphOptions);
    procedure SetShowInfo(AValue: Boolean);
    procedure SetShowSplitModes(AValue: TLvlGraphEdgeSplitModes);
    procedure PopulateEdgeSplit;
    procedure PopulateEdgeShape;
    procedure WriteToOpts;
    procedure ReadFromOpts;
    procedure UpdateInfo;
  public
    property Options: TLvlGraphOptions read FOptions write SetOptions;
    property Graph: TLvlGraph read FGraph write SetGraph;
    property ApplyCallback: TApplyOptionsProc read FApplyCallback write SetApplyCallback;
    property ShowInfo: Boolean read FShowInfo write SetShowInfo;
    property ShowSplitModes: TLvlGraphEdgeSplitModes read FShowSplitModes write SetShowSplitModes;
  end;

function ShowDependencyGraphOptions(AnOpts: TLvlGraphOptions; AGraph: TLvlGraph; ACaption: String;
  AnApplyCallback: TApplyOptionsProc = nil): TModalResult;

implementation

function ShowDependencyGraphOptions(AnOpts: TLvlGraphOptions; AGraph: TLvlGraph; ACaption: String;
  AnApplyCallback: TApplyOptionsProc = nil): TModalResult;
var
  Dlg: TDependencyGraphOptDialog;
begin
  Dlg := TDependencyGraphOptDialog.Create(Application);
  Dlg.Caption := ACaption;
  Dlg.Graph := AGraph;
  Dlg.Options := AnOpts;
  Dlg.ApplyCallback := AnApplyCallback;
  Result := Dlg.ShowModal;
  Dlg.Free;
end;

{$R *.lfm}

{ TDependencyGraphOptDialog }

procedure TDependencyGraphOptDialog.CancelButtonClick(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

procedure TDependencyGraphOptDialog.FormCreate(Sender: TObject);
begin
  FShowInfo := True;
  FShowSplitModes := [lgesSeparate, lgesMergeSource, lgesMergeTarget, lgesMergeHighest];

  chkMinimizeEdges.Caption := LvlGraphShapeMinimizeEdge;
  chkHighEdges.Caption := LvlGraphShapeCalculateLay;
  chkReduceBackEdges.Caption := LvlGraphReduceBackedges;
  chkStraigtenGraph.Caption := LvlGraphStraightenGraph;
  chkCaptionOnTop.Caption := LvlGraphNamesAboveNode;

  lblEdgeSplit.Caption := LvlGraphShapeEdgesSplitMo;
  lblEdgeShape.Caption := LvlGraphShapeEdgesShape;
  dropEdgeSplit.Constraints.MinWidth := Max(dropEdgeSplit.Constraints.MinWidth,
    lblEdgeSplit.Width);
  dropEdgeShape.Constraints.MinWidth := Max(dropEdgeShape.Constraints.MinWidth,
    lblEdgeShape.Width);

  OptionsGroup.Caption      := lisOptions;
  InfoGroup.Caption         := LvlGraphOptInfo;
  lblNodeCnt.Caption        := LvlGraphShapeNodes;
  lblEdgeCount.Caption      := LvlGraphOptEdges;
  lblEdgeLen.Caption        := LvlGraphOptEdgeLen;
  LblLevelCount.Caption     := LvlGraphOptLevels;
  LblCrossCount.Caption     := LvlGraphOptCrossings;
  lblSplitCount.Caption     := LvlGraphOptSplitpoints;
  lblMaxLevelHeight.Caption := LvlGraphOptLimitHeightOfLvl;
  lblExtraSpacing.Caption   := LvlGraphExtraSpacing;
  spinLvlLimitAbs.Hint      := Format(LvlGraphOptAbsoluteLimi, [LineEnding]);
  spinLvlLimitRel.Hint      := Format(LvlGraphOptLimitRelativ, [LineEnding]);
  spinHorizSpacing.Hint     := Format(LvlGraphAddHorizontalSpacing, [LineEnding]);
  spinVertSpacing.Hint      := Format(LvlGraphAddVerticalSpacingAr, [LineEnding]);

  PopulateEdgeSplit;
  PopulateEdgeShape;
end;

procedure TDependencyGraphOptDialog.OKButtonClick(Sender: TObject);
begin
  WriteToOpts;
  ModalResult := mrOK;
end;

procedure TDependencyGraphOptDialog.ApplyButtonClick(Sender: TObject);
begin
  if ApplyCallback <> nil then begin
    WriteToOpts;
    ApplyCallback(Options, Graph);
    ModalResult := mrNone;
    UpdateInfo;
  end;
end;

procedure TDependencyGraphOptDialog.SetApplyCallback(AValue: TApplyOptionsProc);
begin
  if FApplyCallback = AValue then Exit;
  FApplyCallback := AValue;
  if FApplyCallback <> nil then
    ButtonPanel1.ShowButtons := ButtonPanel1.ShowButtons + [pbClose]
  else
    ButtonPanel1.ShowButtons := ButtonPanel1.ShowButtons - [pbClose];
end;

procedure TDependencyGraphOptDialog.SetGraph(AValue: TLvlGraph);
begin
  if FGraph = AValue then Exit;
  FGraph := AValue;
  UpdateInfo;
end;

procedure TDependencyGraphOptDialog.SetOptions(AValue: TLvlGraphOptions);
begin
  if FOptions = AValue then Exit;
  FOptions := AValue;
  ReadFromOpts;
end;

procedure TDependencyGraphOptDialog.SetShowInfo(AValue: Boolean);
begin
  if FShowInfo = AValue then Exit;
  FShowInfo := AValue;
  UpdateInfo;
end;

procedure TDependencyGraphOptDialog.SetShowSplitModes(
  AValue: TLvlGraphEdgeSplitModes);
begin
  if FShowSplitModes = AValue then Exit;
  FShowSplitModes := AValue;
  PopulateEdgeSplit;
end;

procedure TDependencyGraphOptDialog.PopulateEdgeSplit;
var
  i: TLvlGraphEdgeSplitMode;
  j: Integer;
begin
  dropEdgeSplit.Items.Clear;
  j := 0;
  for i := low(TLvlGraphEdgeSplitMode) to high(TLvlGraphEdgeSplitMode) do begin
    if not (i in FShowSplitModes) then
      continue;
    FEdgeSplitMap[j] := i;
    FEdgeRevSplitMap[i] := j;
    case i of
      lgesNone:         dropEdgeSplit.Items.Add(LvlGraphSplitNone);
      lgesSeparate:     dropEdgeSplit.Items.Add(LvlGraphSplitSeparate);
      lgesMergeSource:  dropEdgeSplit.Items.Add(LvlGraphSplitMergeAtSourc);
      lgesMergeTarget:  dropEdgeSplit.Items.Add(LvlGraphSplitMergeAtTarge);
      lgesMergeHighest: dropEdgeSplit.Items.Add(LvlGraphSplitMergeAtHighe);
      else dropEdgeSplit.Items.Add('?');
    end;
    inc(j);
  end;
end;

procedure TDependencyGraphOptDialog.PopulateEdgeShape;
var
  i: TLvlGraphEdgeShape;
begin
  dropEdgeShape.Items.Clear;
  for i := low(TLvlGraphEdgeShape) to high(TLvlGraphEdgeShape) do begin
    case i of
      lgesStraight: dropEdgeShape.Items.Add(LvlGraphShapeStraight);
      lgesCurved:   dropEdgeShape.Items.Add(LvlGraphShapeCurved);
      else dropEdgeShape.Items.Add('?');
    end;
  end;
end;

procedure TDependencyGraphOptDialog.WriteToOpts;
begin
  Options.MinimizeEdges := chkMinimizeEdges.Checked;
  Options.HighLevels := chkHighEdges.Checked;
  Options.ReduceBackEdges := chkReduceBackEdges.Checked;
  Options.StraightenGraph := chkStraigtenGraph.Checked;
  Options.EdgeSplit := FEdgeSplitMap[max(0, dropEdgeSplit.ItemIndex)];
  Options.EdgeShape := TLvlGraphEdgeShape(max(0, dropEdgeShape.ItemIndex));
  if chkCaptionOnTop.Checked then
    Options.CaptionPos := lgncTop
  else
    Options.CaptionPos := lgncBottom;
  Options.ExtraSpacingHoriz := spinHorizSpacing.Value;
  Options.ExtraSpacingVert := spinVertSpacing.Value;
  Options.LimitLvlHeighAbs := spinLvlLimitAbs.Value;
  Options.LimitLvlHeighRel := spinLvlLimitRel.Value;
end;

procedure TDependencyGraphOptDialog.ReadFromOpts;
begin
  chkMinimizeEdges.Checked := Options.MinimizeEdges;
  chkHighEdges.Checked := Options.HighLevels;
  chkReduceBackEdges.Checked := Options.ReduceBackEdges;
  chkStraigtenGraph.Checked := Options.StraightenGraph;
  dropEdgeSplit.ItemIndex := FEdgeRevSplitMap[Options.EdgeSplit];
  dropEdgeShape.ItemIndex := ord(Options.EdgeShape);
  chkCaptionOnTop.Checked := Options.CaptionPos = lgncTop;
  spinHorizSpacing.Value := Options.ExtraSpacingHoriz;
  spinVertSpacing.Value := Options.ExtraSpacingVert;
  spinLvlLimitAbs.Value := Options.LimitLvlHeighAbs;
  spinLvlLimitRel.Value := Options.LimitLvlHeighRel;
end;

procedure TDependencyGraphOptDialog.UpdateInfo;
  function ComputeCrossCount: integer;
  var
    l,i,j,e1,e2: Integer;
    Level: TLvlGraphLevel;
    Node1, Node2, Target1, Target2: TLvlGraphNode;
  begin
    Result:=0;
    for l:=0 to Graph.LevelCount-2 do begin
      Level:=Graph.Levels[l];
      for i:=0 to Level.Count-2 do begin
        Node1:=Level.Nodes[i];
        for j:=i+1 to Level.Count-1 do begin
          Node2:=Level.Nodes[j];
          for e1:=0 to Node1.OutEdgeCount-1 do begin
            Target1:=Node1.OutEdges[e1].Target;
            for e2:=0 to Node2.OutEdgeCount-1 do begin
              Target2:=Node2.OutEdges[e2].Target;
              if Target1.IndexInLevel>Target2.IndexInLevel then
                Result+=1;
            end;
          end;
        end;
      end;
    end;
  end;
var
  InfoNodeCnt, InfoEdgeCnt, InfoEdgeLen, InfoSplitCnt: Integer;
  i, j: Integer;
  Node: TLvlGraphNode;
  Targets: TLvlGraphNodeArray;
begin
  InfoGroup.Visible := FGraph <> nil;
  if FGraph = nil then
    exit;
  InfoNodeCnt := 0;
  InfoEdgeCnt := 0;
  InfoEdgeLen := 0;
  InfoSplitCnt := 0;
  for i := 0 to Graph.NodeCount - 1 do begin
    Node := Graph.Nodes[i];
    if not Node.Visible then begin
      if (Node.OutEdgeCount > 1) or (Node.InEdgeCount > 1) then
        InfoSplitCnt := InfoSplitCnt + 1;
      continue;
    end;
    InfoNodeCnt := InfoNodeCnt + 1;
    Targets := Node.GetVisibleTargetNodes;
    InfoEdgeCnt := InfoEdgeCnt + length(Targets);
    for j := 0 to high(Targets) do
      InfoEdgeLen := InfoEdgeLen + abs(Targets[j].Level.Index - Node.Level.Index);
  end;
  valNodeCnt.Caption  := IntToStr(InfoNodeCnt);
  valEdgeCnt.Caption  := IntToStr(InfoEdgeCnt);
  valEdgeLen.Caption  := IntToStr(InfoEdgeLen);
  valLevelCnt.Caption := IntToStr(Graph.LevelCount);
  valCrossCnt.Caption := IntToStr(ComputeCrossCount);
  valSplitCnt.Caption := IntToStr(InfoSplitCnt);
end;

{ TLvlGraphOptions }

constructor TLvlGraphOptions.Create;
begin
  inherited;
  FMinimizeEdges := True;
  FHighLevels := False;
  ReduceBackEdges := True;
  FExtraSpacingVert := 0;
  FExtraSpacingHoriz := 0;
  FCaptionPos := lgncTop;
  FEdgeShape := lgesCurved;
  FEdgeSplit := lgesMergeHighest;
  FStraightenGraph := True;
  FLimitLvlHeighAbs := 0;
  FLimitLvlHeighRel := 1.5;
end;

procedure TLvlGraphOptions.ReadFromXml(AnXmlConf: TRttiXMLConfig; APath: String);
var
  Def: TLvlGraphOptions;
begin
  Def := TLvlGraphOptions.Create;
  AnXmlConf.ReadObject(APath, Self, Def);
  Def.Free;
  if OnLoaded <> nil then
    OnLoaded(Self);
end;

procedure TLvlGraphOptions.WriteToXml(AnXmlConf: TRttiXMLConfig; APath: String);
var
  Def: TLvlGraphOptions;
begin
  Def := TLvlGraphOptions.Create;
  AnXmlConf.WriteObject(APath, Self, Def);
  Def.Free;
end;

procedure TLvlGraphOptions.Assign(ASrc: TPersistent);
begin
  FMinimizeEdges := TLvlGraphOptions(ASrc).FMinimizeEdges;
  FHighLevels := TLvlGraphOptions(ASrc).FHighLevels;
  FReduceBackEdges := TLvlGraphOptions(ASrc).FReduceBackEdges;
  FExtraSpacingVert := TLvlGraphOptions(ASrc).FExtraSpacingVert;
  FExtraSpacingHoriz := TLvlGraphOptions(ASrc).FExtraSpacingHoriz;
  FCaptionPos := TLvlGraphOptions(ASrc).FCaptionPos;
  FEdgeShape := TLvlGraphOptions(ASrc).FEdgeShape;
  FEdgeSplit := TLvlGraphOptions(ASrc).FEdgeSplit;
  FStraightenGraph := TLvlGraphOptions(ASrc).FStraightenGraph;
  FLimitLvlHeighAbs := TLvlGraphOptions(ASrc).FLimitLvlHeighAbs;
  FLimitLvlHeighRel := TLvlGraphOptions(ASrc).FLimitLvlHeighRel;
end;

procedure TLvlGraphOptions.WriteToGraph(AValue: TLvlGraphControl);
var
  i: Integer;
begin
  if MinimizeEdges then
    AValue.Options := AValue.Options + [lgoMinimizeEdgeLens]
  else
    AValue.Options := AValue.Options - [lgoMinimizeEdgeLens];
  if HighLevels then
    AValue.Options := AValue.Options + [lgoHighLevels]
  else
    AValue.Options := AValue.Options - [lgoHighLevels];
  if ReduceBackEdges then
    AValue.Options := AValue.Options + [lgoReduceBackEdges]
  else
    AValue.Options := AValue.Options - [lgoReduceBackEdges];
  i := ExtraSpacingVert div 2;
  AValue.NodeStyle.GapTop    := DefaultLvlGraphNodeGapTop + i;
  AValue.NodeStyle.GapBottom := DefaultLvlGraphNodeGapBottom + ExtraSpacingVert - i;
  AValue.NodeStyle.GapRight := DefaultLvlGraphNodeGapRight + ExtraSpacingHoriz;
  AValue.NodeStyle.CaptionPosition := FCaptionPos;
  AValue.EdgeStyle.SplitMode := EdgeSplit;
  AValue.EdgeStyle.Shape  := EdgeShape;
  if StraightenGraph then
    AValue.Options := AValue.Options + [lgoStraightenGraph]
  else
    AValue.Options := AValue.Options - [lgoStraightenGraph];
  AValue.Limits.MaxLevelHeightAbs := LimitLvlHeighAbs;
  AValue.Limits.MaxLevelHeightRel := LimitLvlHeighRel;
end;

procedure TLvlGraphOptions.ReadFromGraph(AValue: TLvlGraphControl);
begin
  MinimizeEdges := lgoMinimizeEdgeLens in AValue.Options;
  HighLevels := lgoHighLevels in AValue.Options;
  ReduceBackEdges := lgoReduceBackEdges in AValue.Options;
  ExtraSpacingVert := AValue.NodeStyle.GapTop - DefaultLvlGraphNodeGapTop
    + AValue.NodeStyle.GapBottom - DefaultLvlGraphNodeGapBottom;
  ExtraSpacingHoriz := AValue.NodeStyle.GapRight - DefaultLvlGraphNodeGapRight;
  FCaptionPos := AValue.NodeStyle.CaptionPosition;
  EdgeSplit := AValue.EdgeStyle.SplitMode;
  EdgeShape := AValue.EdgeStyle.Shape;
  StraightenGraph := lgoStraightenGraph in AValue.Options;
  LimitLvlHeighAbs := AValue.Limits.MaxLevelHeightAbs;
  LimitLvlHeighRel := AValue.Limits.MaxLevelHeightRel;
end;

end.

