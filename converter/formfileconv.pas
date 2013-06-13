unit FormFileConv;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, contnrs,
  // codetools
  LFMTrees, CodeCache, LinkScanner, BasicCodeTools, KeywordFuncLists,
  // Converter
  ConverterTypes, ConvCodeTool;

type

  { TFormFileConverter }

  TFormFileConverter = class
  private
    fCTLink: TCodeToolLink;              // Link to codetools.
    fLFMBuf: TCodeBuffer;
    fLFMTree: TLFMTree;
    fVisOffsets: TVisualOffsets;
    fSrcCoordOffs: TObjectList;
    fSrcNewProps: TObjectList;
    // Statusbar has Panels. "SimplePanel = False" must be added if not there.
    fSBHasPanels: Boolean;
    fSBHasSimplePanelProp: Boolean;
    fSBHasSimpleText: Boolean;
    // Position for possible insertion of "SimplePanel = False".
    fSBEndPos: integer;
    function LeadingSpaceCount(Pos: integer): integer;
    procedure AddStatusbarProp(ParentType: string);
    procedure DoProperty(AProperty: TLFMPropertyNode; const GrandParType, AParentType: string);
    function DoObjectValues(ObjectNode: TLFMObjectNode; const GrandParType: string): boolean;
  public
    constructor Create(ACTLink: TCodeToolLink; ALFMBuf: TCodeBuffer);
    destructor Destroy; override;
    function Convert: TModalResult;
  public
    property VisOffsets: TVisualOffsets read fVisOffsets write fVisOffsets;
    property SrcCoordOffs: TObjectList read fSrcCoordOffs write fSrcCoordOffs;
    property SrcNewProps: TObjectList read fSrcNewProps write fSrcNewProps;
  end;


implementation


{ TFormFileConverter }

constructor TFormFileConverter.Create(ACTLink: TCodeToolLink; ALFMBuf: TCodeBuffer);
begin
  inherited Create;
  fCTLink:=ACTLink;
  fLFMBuf:=ALFMBuf;
end;

destructor TFormFileConverter.Destroy;
begin
  inherited Destroy;
end;

function TFormFileConverter.LeadingSpaceCount(Pos: integer): integer;
// returns the number of spaces in front of Pos in fLFMBuf.Source.
begin
  Result:=0;
  while (Pos>=0) and not (fLFMBuf.Source[Pos] in [#10,#13]) do begin
    if fLFMBuf.Source[Pos] = ' ' then
      Inc(Result);
    Dec(Pos);
  end;
end;

procedure TFormFileConverter.AddStatusbarProp(ParentType: string);
// Add 'SimplePanel = False' property if it is needed.
var
  SpaceCnt, StartPos: integer;
  s: string;
begin
  if fSBEndPos<>-1 then begin
    if fSBHasPanels and (not fSBHasSimpleText) and (not fSBHasSimplePanelProp) then begin
      SpaceCnt:=LeadingSpaceCount(fSBEndPos);
      // 2 spaces for indentation, then spaces in front of 'end'.
      s:=StringOfChar(' ', SpaceCnt+2);
      StartPos:=FindLineEndOrCodeInFrontOfPosition(fLFMBuf.Source,fSBEndPos,1,false);
      fSrcNewProps.Add(TAddPropEntry.Create(StartPos, StartPos, LineEnding+s,
                                            'SimplePanel = False', ParentType));
    end;
    fSBHasPanels:=False;
    fSBHasSimplePanelProp:=False;
    fSBEndPos:=-1;                 // Ready for next round.
  end;
end;

procedure TFormFileConverter.DoProperty(AProperty: TLFMPropertyNode;
                                        const GrandParType, AParentType: string);
// Take care of certain properties like Top and Left to be adjusted later.
// Parameters: AProperty is the property node
//             GrandParType and AParentType are the class type names.
var
  ind: Integer;
  ValNode: TLFMValueNode;
  CurNode: TLFMTreeNode;
  Prop: string;
begin
  // complete property name
  Prop:=AProperty.CompleteName;
  if Prop='' then exit;
  if AParentType='TStatusBar' then begin
    if Prop='SimplePanel' then begin
      if AProperty.FirstChild.TheType=lfmnValue then
        fSBHasSimplePanelProp:=True;
    end
    else if Prop='SimpleText' then begin
      if AProperty.FirstChild.TheType=lfmnValue then
        fSBHasSimpleText:=True;
    end
    else if Prop='Panels' then begin
      CurNode:=AProperty.FirstChild;
      while CurNode<>nil do begin
        // Collection items have ValueType=lfmvList, there are items under Panels.
        if (CurNode.TheType=lfmnValue) and (TLFMValueNode(CurNode).ValueType=lfmvCollection) then
          fSBHasPanels:=True;
        CurNode:=CurNode.NextSibling;
      end;
    end;
    fSBEndPos:=AProperty.EndPos;
  end;
  if (Prop='Top') or (Prop='Left') then begin
    if (GrandParType<>'') and fVisOffsets.Find(GrandParType, ind) then begin
      if AProperty.FirstChild is TLFMValueNode then begin
        Assert(AProperty.FirstChild.TheType=lfmnValue,
               'Property.FirstChild.TheType should be lfmnValue');
        ValNode:=AProperty.FirstChild as TLFMValueNode;
        fSrcCoordOffs.Add(TSrcPropOffset.Create(GrandParType,AParentType,
                                                Prop,ValNode.StartPos));
      end;
    end;
  end;
end;

function TFormFileConverter.DoObjectValues(ObjectNode: TLFMObjectNode;
                                           const GrandParType: string): boolean;
var
  CurNode: TLFMTreeNode;
  ParentType: string;
begin
  fSBHasPanels:=False;
  fSBHasSimplePanelProp:=False;
  fSBEndPos:=-1;
  ParentType:=ObjectNode.TypeName;
  CurNode:=ObjectNode.FirstChild;
  while CurNode<>nil do begin
    case CurNode.TheType of
    lfmnObject: begin
      AddStatusbarProp(ParentType);  // Check if previous object was a statusbar.
      DoObjectValues(TLFMObjectNode(CurNode), ParentType);   // Recursive call.
    end;
    lfmnProperty:
      DoProperty(TLFMPropertyNode(CurNode), GrandParType, ParentType);
    end;
    CurNode:=CurNode.NextSibling;
  end;
  AddStatusbarProp(ParentType);
  Result:=true;
end;

function TFormFileConverter.Convert: TModalResult;
var
  CurRootNode: TLFMTreeNode;
  RootObjNode: TLFMObjectNode;
begin
  Result:=mrCancel;
  // create tree from LFM file
  fLFMTree:=DefaultLFMTrees.GetLFMTree(fLFMBuf,true);
  fCTLink.CodeTool.ActivateGlobalWriteLock;
  try
    if not fLFMTree.ParseIfNeeded then exit;
    fCTLink.CodeTool.BuildTree(lsrImplementationStart);
    // Iterate the root objects
    CurRootNode:=fLFMTree.Root;
    while (CurRootNode<>nil) and (CurRootNode is TLFMObjectNode) do begin
      RootObjNode:=TLFMObjectNode(CurRootNode);
      if RootObjNode.TypeName='' then exit;
      if not DoObjectValues(RootObjNode, '') then exit;
      CurRootNode:=CurRootNode.NextSibling;
    end;
  finally
    fCTLink.CodeTool.DeactivateGlobalWriteLock;
  end;
  Result:=mrOK;
end;

end.

