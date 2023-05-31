unit IdeXmlConfigProcs;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Laz_AVL_Tree,
  // LazUtils
  Laz2_XMLCfg, AvgLvlTree;

procedure LoadStringList(XMLConfig: TXMLConfig; List: TStrings; const Path: string);
procedure SaveStringList(XMLConfig: TXMLConfig; List: TStrings; const Path: string);
procedure LoadRect(XMLConfig: TXMLConfig; const Path:string;
                   var ARect:TRect);
procedure LoadRect(XMLConfig: TXMLConfig; const Path:string;
                   var ARect:TRect; const DefaultRect: TRect);
procedure SaveRect(XMLConfig: TXMLConfig; const Path:string;
                   const ARect: TRect);
procedure SaveRect(XMLConfig: TXMLConfig; const Path:string;
                   const ARect, DefaultRect: TRect);
procedure LoadPoint(XMLConfig: TXMLConfig; const Path:string;
                    var APoint:TPoint; const DefaultPoint: TPoint);
procedure SavePoint(XMLConfig: TXMLConfig; const Path:string;
                    const APoint, DefaultPoint:TPoint);
procedure LoadStringToStringTree(XMLConfig: TXMLConfig;
                                 Tree: TStringToStringTree; const Path: string);
procedure SaveStringToStringTree(XMLConfig: TXMLConfig;
                                 Tree: TStringToStringTree; const Path: string);
procedure MakeXMLName(var Name: string);

implementation

procedure LoadStringList(XMLConfig: TXMLConfig; List: TStrings; const Path: string);
var
  i,Count: integer;
  s: string;
begin
  Count:=XMLConfig.GetValue(Path+'Count',0);
  List.Clear;
  for i:=1 to Count do begin
    s:=XMLConfig.GetValue(Path+'Item'+IntToStr(i)+'/Value','');
    if s<>'' then List.Add(s);
  end;
end;

procedure SaveStringList(XMLConfig: TXMLConfig; List: TStrings; const Path: string);
var
  i: integer;
begin
  XMLConfig.SetDeleteValue(Path+'Count',List.Count,0);
  for i:=0 to List.Count-1 do
    XMLConfig.SetDeleteValue(Path+'Item'+IntToStr(i+1)+'/Value',List[i],'');
end;

procedure LoadRect(XMLConfig: TXMLConfig; const Path: string;
  var ARect: TRect);
begin
  LoadRect(XMLConfig,Path,ARect,Rect(0,0,0,0));
end;

procedure LoadRect(XMLConfig: TXMLConfig; const Path:string; var ARect:TRect;
  const DefaultRect: TRect);
begin
  ARect.Left:=XMLConfig.GetValue(Path+'Left',DefaultRect.Left);
  ARect.Top:=XMLConfig.GetValue(Path+'Top',DefaultRect.Top);
  ARect.Right:=XMLConfig.GetValue(Path+'Right',DefaultRect.Right);
  ARect.Bottom:=XMLConfig.GetValue(Path+'Bottom',DefaultRect.Bottom);
end;

procedure SaveRect(XMLConfig: TXMLConfig; const Path: string; const ARect: TRect);
begin
  SaveRect(XMLConfig,Path,ARect,Rect(0,0,0,0));
end;

procedure SaveRect(XMLConfig: TXMLConfig; const Path:string;
  const ARect, DefaultRect: TRect);
begin
  XMLConfig.SetDeleteValue(Path+'Left',ARect.Left,DefaultRect.Left);
  XMLConfig.SetDeleteValue(Path+'Top',ARect.Top,DefaultRect.Top);
  XMLConfig.SetDeleteValue(Path+'Right',ARect.Right,DefaultRect.Right);
  XMLConfig.SetDeleteValue(Path+'Bottom',ARect.Bottom,DefaultRect.Bottom);
end;

procedure LoadPoint(XMLConfig: TXMLConfig; const Path: string;
                    var APoint: TPoint; const DefaultPoint: TPoint);
begin
  APoint.X:=XMLConfig.GetValue(Path+'X',DefaultPoint.X);
  APoint.Y:=XMLConfig.GetValue(Path+'Y',DefaultPoint.Y);
end;

procedure SavePoint(XMLConfig: TXMLConfig; const Path: string;
                    const APoint, DefaultPoint: TPoint);
begin
  XMLConfig.SetDeleteValue(Path+'X',APoint.X,DefaultPoint.X);
  XMLConfig.SetDeleteValue(Path+'Y',APoint.Y,DefaultPoint.Y);
end;

procedure LoadStringToStringTree(XMLConfig: TXMLConfig;
  Tree: TStringToStringTree; const Path: string);
var
  Cnt: LongInt;
  SubPath: String;
  CurName: String;
  CurValue: String;
  i: Integer;
begin
  Tree.Clear;
  Cnt:=XMLConfig.GetValue(Path+'Count',0);
  for i:=0 to Cnt-1 do begin
    SubPath:=Path+'Item'+IntToStr(i)+'/';
    CurName:=XMLConfig.GetValue(SubPath+'Name','');
    CurValue:=XMLConfig.GetValue(SubPath+'Value','');
    Tree.Values[CurName]:=CurValue;
  end;
end;

procedure SaveStringToStringTree(XMLConfig: TXMLConfig;
  Tree: TStringToStringTree; const Path: string);
var
  Node: TAVLTreeNode;
  Item: PStringToStringItem;
  i: Integer;
  SubPath: String;
begin
  XMLConfig.SetDeleteValue(Path+'Count',Tree.Tree.Count,0);
  Node:=Tree.Tree.FindLowest;
  i:=0;
  while Node<>nil do begin
    Item:=PStringToStringItem(Node.Data);
    SubPath:=Path+'Item'+IntToStr(i)+'/';
    XMLConfig.SetDeleteValue(SubPath+'Name',Item^.Name,'');
    XMLConfig.SetDeleteValue(SubPath+'Value',Item^.Value,'');
    Node:=Tree.Tree.FindSuccessor(Node);
    inc(i);
  end;
end;

procedure MakeXMLName(var Name: string);
var
  i: Integer;
begin
  i:=1;
  while i<=length(Name) do begin
    if (Name[i] in ['a'..'z','A'..'Z','_'])
    or (i>1) and (Name[i] in ['0'..'9']) then begin
      inc(i);
    end else begin
      System.Delete(Name,i,1);
    end;
  end;
end;

end.

