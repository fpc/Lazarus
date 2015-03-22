{ A LazReport loader for FastReport 3 formats

  Copyright (C) 2013 Jesus Reyes Aguilar jesusrmx@yahoo.com.mx

  This library is free software; you can redistribute it and/or modify it
  under the terms of the GNU Library General Public License as published by
  the Free Software Foundation; either version 2 of the License, or (at your
  option) any later version with the following modification:

  As a special exception, the copyright holders of this library give you
  permission to link this library with independent modules to produce an
  executable, regardless of the license terms of these independent modules,and
  to copy and distribute the resulting executable under terms of your choice,
  provided that you also meet, for each linked independent module, the terms
  and conditions of the license of that module. An independent module is a
  module which is not derived from or based on this library. If you modify
  this library, you may extend this exception to your version of the library,
  but you are not obligated to do so. If you do not wish to do so, delete this
  exception statement from your version.

  This program is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public License
  for more details.

  You should have received a copy of the GNU Library General Public License
  along with this library; if not, write to the Free Software Foundation,
  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
}
unit fr3tolrf;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Laz2_DOM, Laz2_XMLRead, LConvEncoding, FileUtil, Graphics, lr_class;

type
  EFR3ReaderException = class(Exception);

  function LoadFastReport3(Report: TfrReport; aFileName: string; out log:string): Integer;

implementation

type

  { TAttributeEnumerator }

  TAttributeEnumerator = class
  private
    fIndex: Integer;
    fNode: TDOMNode;
    function GetCurrent: TDOMNode;
  public
    constructor Create(const A: TDOMNode);
    property Current: TDOMNode read GetCurrent;
    function MoveNext: Boolean;
  end;

  operator Enumerator(const A: TDOMNode): TAttributeEnumerator;
   begin
     Result := TAttributeEnumerator.Create(A);
   end;

type
  TClassName = string[30];
  TKnownObjects = record
    frClass: TClassName;
    lrClass: TClassName;
    Typ: Integer;
    BandType: TfrBandType;
  end;

  { Tfr3Reader }

  Tfr3Reader = class
  private
    fReport: TfrReport;
    fLog: TStringList;
    fObj: array of TKnownObjects;
    fUsedNodes: TFPList;
    function GetMessages: string;
    function MMToPt(AValue: Extended): Integer;
    function MMToScrPt(AValue: Extended): Integer;
    function CreateView(Page: TfrPage; frClass: string): TfrView;
    function NodeValToCutStr(Node: TDOMNode): string;
    function NodeValToFloat(Node:TDOMNode; defValue:Extended=0.0): Extended;
    function MatchAttribute(Attr: TDOMNode; AttrName:string): boolean;
    function EncodeUTF8Checking(s:string): string;
    //
    procedure PopulateKnownObjects;
    function IndexOfFrClass(frClass: TClassName): Integer;
    procedure AddObj(frClass,lrClass:TClassName; Typ:Integer; BandType: TfrBandType);
  protected
    procedure LoadReport(Node: TDOMNode); virtual;
    procedure LoadPages(Node: TDOMNode); virtual;
    procedure LoadPage(Node: TDOMNode; Page: TfrPage); virtual;
    procedure LoadView(Node: TDOMNode; Page: TfrPage; View, ParentView: TfrView);
    procedure LoadCommonView(Node: TDOMNode; Page: TfrPage; ParentView, View: TfrView);
    procedure LoadMemoView(Node: TDOMNode; Page:TfrPage; View: TfrMemoView);
    procedure LoadBandView(Node: TDOMNode; Page:TfrPage; View: TfrBandView);
    procedure ProcessObject(Page:TfrPage; Node: TDOMNode; ParentView: TfrView = nil);
    procedure Log(s:string);
  public
    destructor destroy; override;
    procedure LoadFromFile(aFileName: string);
    property Report: TfrReport read fReport write fReport;
    property Messages: string read GetMessages;
  end;

function LoadFastReport3(Report: TfrReport; aFileName: string; out Log:string): Integer;
var
  Reader: Tfr3Reader;
begin
  Result := 0;
  Reader := Tfr3Reader.Create;
  Reader.Report := Report;
  try
    Reader.LoadFromFile(aFileName);
    log := Reader.Messages;
  finally
    Reader.Free;
  end;
end;

function DelphiIntToFpcFontStyle(aStyle: Integer): TFontStyles;
begin
  result := TFontStyles(aStyle);
end;

function DelphiIntToFPCFrameBorders(aFrameTyp: Integer): TfrFrameBorders;
begin
  result := [];
  if (aFrameTyp and 1) <> 0 then include(result, frbLeft);
  if (aFrameTyp and 2) <> 0 then include(result, frbRight);
  if (aFrameTyp and 4) <> 0 then include(result, frbTop);
  if (aFrameTyp and 8) <> 0 then include(result, frbBottom);
end;


{ TAttributeEnumerator }

function TAttributeEnumerator.GetCurrent: TDOMNode;
begin
  result := fNode.Attributes.Item[fIndex];
end;

constructor TAttributeEnumerator.Create(const A: TDOMNode);
begin
  inherited create;
  fIndex := -1;
  fNode := A;
end;

function TAttributeEnumerator.MoveNext: Boolean;
begin
  inc(fIndex);
  result := (fNode<>nil) and (fIndex<fNode.Attributes.Length);
end;

function Tfr3Reader.GetMessages: string;
begin
  if fLog<>nil then
    result := fLog.Text
  else
    result := '';
end;

function Tfr3Reader.MMToPt(AValue: Extended): Integer;
begin
  result := round(AValue * 25.4 / 72);
end;

function Tfr3Reader.MMToScrPt(AValue: Extended): Integer;
begin
  result := round(AValue * 18 / 5);
end;

function Tfr3Reader.NodeValToCutStr(Node: TDOMNode): string;
begin
  result := Copy(Node.NodeValue, 1, 40);
  if Length(Node.NodeValue)>40 then
    result := result + '...';
  result := '"'+result+'"';
end;

function Tfr3Reader.NodeValToFloat(Node: TDOMNode; defValue:Extended=0.0): Extended;
var
  s: string;
  settings: TFormatSettings;
begin
  s := Node.NodeValue;
  Settings.ThousandSeparator:=',';
  Settings.DecimalSeparator:='.';
  if not tryStrToFloat(s, result, settings) then begin
    if pos(',',s)>0 then begin
      Settings.ThousandSeparator:='.';
      settings.DecimalSeparator:=',';
      if not TryStrToFloat(s, result, settings) then
        result := defValue;
    end else
      result := defValue;
  end;
end;

function Tfr3Reader.MatchAttribute(Attr: TDOMNode; AttrName: string): boolean;
var
  i: Integer;
begin
  result := Attr.NodeName=AttrName;
  if result then begin
    if fUsedNodes=nil then
      fUsedNodes := TFPList.Create;
    i := fUsedNodes.IndexOf(Attr);
    if i<0 then
      fUsedNodes.Add(Attr);
  end;
end;

function Tfr3Reader.EncodeUTF8Checking(s: string): string;
var
  enc: String;
begin
  enc := GuessEncoding(s);
  result :=  ConvertEncoding(s, Enc, EncodingUTF8);
end;

procedure Tfr3Reader.PopulateKnownObjects;
begin
  AddObj('TfrxReportTitle',   'TfrBandView',    gtBand,     btReportTitle);
  AddObj('TfrxPageHeader',    'TfrBandView',    gtBand,     btPageHeader);
  AddObj('TfrxMasterData',    'TfrBandView',    gtBand,     btMasterData);
  AddObj('TfrxPageFooter',    'TfrBandView',    gtBand,     btPageFooter);

  AddObj('TfrxDetailData',    'TfrBandView',    gtBand,     btDetailData);
  AddObj('TfrxSubdetailData', 'TfrBandView',    gtBand,     btSubDetailData);

  AddObj('TfrxMemoView',      'TfrMemoView',    gtMemo,     btNone);
  AddObj('TfrxLineView',      'TfrLineView',    gtLine,     btNone);
  AddObj('TfrxShapeView',     'TfrShapeView',   gtaddIn,    btNone);
  AddObj('TfrxPictureView',   'TfrPictureView', gtPicture,  btNone);
end;

function Tfr3Reader.IndexOfFrClass(frClass: TClassName): Integer;
var
  i: Integer;
begin
  result := -1;
  for i:=0 to Length(fObj)-1 do
    if fObj[i].frClass=frClass then begin
      result := i;
      break;
    end;
end;

procedure Tfr3Reader.AddObj(frClass, lrClass: TClassName; Typ: Integer;
  BandType: TfrBandType);
var
  i: Integer;
begin
  i := IndexOfFrClass(frClass);
  if i<0 then begin
    i := Length(fObj);
    SetLength(fObj, i+1);
    fObj[i].frClass:=frClass;
    fObj[i].lrClass:=lrClass;
    fObj[i].Typ := Typ;
    fObj[i].BandType:=BandType;
  end;
end;

procedure Tfr3Reader.LoadReport(Node: TDOMNode);
var
  Attr: TDOMNode;
begin
  Report.Clear;

  PopulateKnownObjects;

  for Attr in Node do begin
    if Attr.NodeName='ReportOptions.CreateDate' then
      Report.ReportCreateDate := NodeValToFloat(Attr)
    else
    if Attr.NodeName='ReportOptions.Description.Text' then
      Report.Subject := Attr.NodeValue
    else
    if Attr.NodeName='ReportOptions.LastChange' then
      Report.ReportLastChange := NodeValToFloat(Attr)
    else
    if Attr.NodeName='ScriptLanguage' then
      Report.Script.Insert(0, '// ScriptLanguage='+Attr.NodeValue)
    else
    if Attr.NodeName='ScriptText.Text' then begin
      Report.Script.Text := Report.Script.Text + '{'^M + Attr.NodeValue + ^M'}';
    end
    else
      Log('Ignored: TfrxReport.'+Attr.NodeName+'='+NodeValToCutStr(Attr));
  end;

  LoadPages(Node);
end;

procedure Tfr3Reader.LoadPages(Node: TDOMNode);
var
  i: Integer;
begin
  Node := Node.FirstChild;
  while Node<>nil do begin
    if Node.NodeName='TfrxReportPage' then begin
      i := Report.Pages.Count;
      Report.Pages.Add;
      LoadPage(Node, Report.Pages[i]);
    end else
      Log('Unknown Page class: '+Node.NodeName);
    Node := Node.NextSibling;
  end;
end;

procedure Tfr3Reader.LoadPage(Node: TDOMNode; Page: TfrPage);
var
  Attr: TDOMNode;
begin

  for Attr in Node do begin
    if Attr.NodeName='Name' then
      Page.Name:= Attr.NodeValue
    else if Attr.NodeName='PaperWidth' then
      Page.Width := MMToPt(NodeValToFloat(Attr))
    else if Attr.NodeNAme='PaperHeight' then
      Page.Height := MMToPt(NodeValToFloat(Attr))
    else if Attr.NodeName='PaperSize' then
      Page.pgSize := StrToIntDef(Attr.NodeValue, 9)
    else if Attr.NodeName='LeftMargin' then
      Page.Margins.Left := MMToScrPt(NodeValToFloat(Attr))
    else if Attr.NodeName='TopMargin' then
      Page.Margins.Top := MMToScrPt(NodeValToFloat(Attr))
    else if Attr.NodeName='BottomMargin' then
      Page.Margins.Bottom := MMToScrPt(NodeValToFloat(Attr))
    else if Attr.NodeName='RightMargin' then
      Page.Margins.Right := MMToScrPt(NodeValToFloat(Attr))
    else
      Log('Ignored: TfrxPageReport.'+Attr.NodeName+'='+NodeValToCutStr(Attr));
  end;

  // objects
  Node := Node.FirstChild;
  while Node<>nil do begin
    ProcessObject(Page, Node);
    Node := Node.NextSibling;
  end;
end;

procedure Tfr3Reader.LoadView(Node: TDOMNode; Page: TfrPage; View, ParentView: TfrView);
var
  Attr: TDOMNode;
begin
  fUsedNodes := nil;

  LoadCommonView(Node, Page, ParentView, View);

  if View is TfrMemoView then
    LoadMemoView(Node, Page, TfrMemoView(View))
  else
  if View is TfrBandView then
    LoadBandView(Node, Page, TfrBandView(View));

  // dump ignored attributes
  for Attr in Node do begin
    // attributes that we know and don't support
    if Attr.NodeName='ParentFont' then
      continue;
    // check the rest
    if (fUsedNodes=nil) or (fUsedNodes.IndexOf(Attr)<0) then
      Log(format('Ignored Attribute: %s:%s %s=%s',[View.ClassName, View.Name, Attr.Nodename,NodeValToCutStr(Attr)]));
  end;

  fUsedNodes.Free;
end;

procedure Tfr3Reader.LoadCommonView(Node: TDOMNode; Page: TfrPage;
  ParentView, View: TfrView );
var
  attr: TDOMNode;
  k: double;
begin
  k := 0.945; // TODO: value obtained by try, needs to check real value
  for attr in Node do begin
    if MatchAttribute(Attr, 'Name') then
      View.Name := attr.NodeValue
    else if MatchAttribute(Attr, 'Visible') then
      View.Visible := StrToBoolDef(attr.NodeValue, true)
    else if MatchAttribute(Attr, 'Left') then
      View.Left := Page.Margins.Left + NodeValToFloat(Attr) * k
    else if MatchAttribute(Attr, 'Top') then begin
      View.Top := Page.Margins.Top + NodeValToFloat(Attr) * k;
      if ParentView<>nil then
        View.Top := ParentView.Top + View.Top - Page.Margins.Top;
    end else if MatchAttribute(Attr, 'Width') then
      View.Width := NodeValToFloat(Attr) * k
    else if MatchAttribute(Attr, 'Height') then
      View.Height := NodeValToFloat(Attr) * k
    else if MatchAttribute(Attr, 'Color') then
      View.FillColor := StrToIntDef(attr.NodeValue, clWhite)
    else if MatchAttribute(Attr, 'Frame.Typ') then
      View.Frames := DelphiIntToFPCFrameBorders(StrToIntDef(attr.NodeValue, 0))
    else if MatchAttribute(Attr, 'Frame.Width') then
      View.FrameWidth := NodeValToFloat(Attr, 1.0);
  end;
end;

procedure Tfr3Reader.LoadMemoView(Node: TDOMNode; Page: TfrPage;
  View: TfrMemoView);
var
  attr: TDOMNode;
  DFDecimalSeparator,DFFormatStr,DFKind: string;
begin

  DFDecimalSeparator := '';
  DFFormatStr := '';
  DFKind := '';

  for Attr in Node do begin

    if MatchAttribute(Attr, 'Font.Color') then
      View.Font.Color := StrToIntDef(attr.NodeValue, 0)
    else if MatchAttribute(Attr, 'Font.Charset') then
      View.Font.CharSet := StrToIntDef(attr.NodeValue, 1)
    else if MatchAttribute(Attr, 'Font.Height') then
      View.Font.Size := Round(-StrToIntDef(attr.NodeValue, -10)*72/96)
    else if MatchAttribute(Attr, 'Font.Name') then
      View.Font.Name := attr.NodeValue
    else if MatchAttribute(Attr, 'Font.Style') then
      View.Font.Style := DelphiIntToFPCFontStyle(StrToIntDef(attr.NodeValue, 0))
    else if MatchAttribute(Attr, 'HAlign') then begin
      if attr.NodeValue='haCenter' then View.Alignment := taCenter;
      if attr.NodeValue='haRight' then View.Alignment := taRightJustify;
    end else if MatchAttribute(Attr, 'VAlign') then begin
      if attr.NodeValue='vaCenter' then View.Layout := tlCenter;
    end else if MatchAttribute(Attr, 'Memo.Text') then
      View.Memo.Text := EncodeUTF8Checking(attr.NodeValue)
    else if MatchAttribute(Attr, 'Text') then
      View.Memo.Text := EncodeUTF8Checking(attr.NodeValue)
    else if MatchAttribute(Attr, 'WordWrap') then
      View.WordWrap := StrToBoolDef(attr.NodeValue, false)
    else if MatchAttribute(Attr, 'AutoWidth') then begin
      View.AutoSize := StrToBoolDef(attr.NodeValue, false);
      if View.AutoSize then
        View.Width := 150; // a default value
    end else if MatchAttribute(Attr, 'DisplayFormat.DecimalSeparator') then
      DFDecimalSeparator := attr.NodeValue
    else if MatchAttribute(Attr, 'DisplayFormat.FormatStr') then
      DFFormatStr := attr.NodeValue
    else if MatchAttribute(Attr, 'DisplayFormat.Kind') then begin
      DFKind := attr.NodeValue;
    end;
  end;

  if (DFFormatStr<>'') then begin
    if DFKind='fkNumeric' then begin
      if DFDecimalSeparator='' then
        DFDecimalSeparator := '.';
      View.Format := $01040000 or ord(DFDecimalSeparator[1]);
      View.FormatStr := DFFormatStr;
      if DFFormatStr='%2.2n' then
        View.FormatStr := '##.00'
      else begin
        View.FormatStr := '#';
        Log(Format('Warning: %s:%s changed default numeric format from %s to %s',
          [View.ClassName,View.Name,DFFormatStr,'#']));
      end;
    end else
      Log(format('Warning: %s:%s don''t know how to handle %s format using %s pattern',
        [View.ClassName, View.Name, DFKind, DFFormatStr]));
  end;
end;

procedure Tfr3Reader.LoadBandView(Node: TDOMNode; Page: TfrPage;
  View: TfrBandView);
var
  Attr: TDOMNode;
begin
  for Attr in Node do begin
    //if MatchAttribute(Attr, 'DataSet') then
    //  View.DataSet := Attr.NodeValue
    if MatchAttribute(Attr, 'DataSetName') then
      View.DataSet := Attr.NodeValue
    else if MatchAttribute(Attr, 'Stretched') then
      View.Stretched := StrToBoolDef(Attr.NodeValue, false);
  end;
end;

function Tfr3Reader.CreateView(Page: TfrPage; frClass: string): TfrView;
var
  i: Integer;
begin
  i := IndexOfFrClass(frClass);
  if i>=0 then begin
    result := frCreateObject(fObj[i].Typ, fObj[i].lrClass, Page);
    if result is TfrBandView then
      TfrBandView(result).BandType:=fObj[i].BandType;
  end else begin
    result := nil;
    Log(format('Found unknown class %s ',[frClass]));
  end;
end;

procedure Tfr3Reader.ProcessObject(Page:TfrPage; Node: TDOMNode;
  ParentView: TfrView = nil);
var
  cNode: TDOMNode;
  View: TfrView;
begin
  View := CreateView(Page, Node.NodeName);
  if View<>nil then
  begin
    View.BeginUpdate;
//    Page.Objects.Add(View);
    LoadView(Node, Page, View, ParentView);
    View.EndUpdate;
    // process any child
    cNode := Node.FirstChild;
    while cNode<>nil do
    begin
      ProcessObject(Page, cNode, View);
      cNode := cNode.NextSibling;
    end;
  end;
end;

procedure Tfr3Reader.Log(s: string);
begin
  if fLog=nil then
    fLog := TStringList.Create;
  fLog.Add(s);
end;

destructor Tfr3Reader.destroy;
begin
  fLog.Free;
  inherited destroy;
end;

procedure Tfr3Reader.LoadFromFile(aFileName: string);
var
  Node: TDOMNode;
  aDoc: TXMLDocument;
begin
  ReadXMLFile(aDoc, aFileName);

  try

    Node := aDoc.FindNode('TfrxReport');
    if Node=nil then
      raise EFR3ReaderException.Create('TfrxReport not found');

  LoadReport(Node);

  finally
    aDoc.Free;
  end;

end;

end.

