unit lrDBDialogControls;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, LRDialogControls, Graphics, LR_Class,
  Controls, DbCtrls, StdCtrls, DB;

type

  { TlrDBLookupComboBox }

  TlrDBLookupComboBox = class(TlrVisualControl)
  private
    FKeyField:string;
    FListField:string;
    FListSource:string;
    function GetKeyField: string;
    function GetListField: string;
    function GetListSource: string;
    function GetText: Variant;
    procedure SetKeyField(AValue: string);
    procedure SetListField(AValue: string);
    procedure SetListSource(AValue: string);
    procedure DBLookupComboBox1CloseUp(Sender: TObject);
    procedure SetText(AValue: Variant);
  protected
    procedure PaintDesignControl; override;
    function CreateControl:TControl;override;
    procedure AfterLoad;override;
  public
    constructor Create(AOwnerPage:TfrPage); override;
    procedure LoadFromXML(XML: TLrXMLConfig; const Path: String); override;
    procedure SaveToXML(XML: TLrXMLConfig; const Path: String); override;
  published
    property KeyField:string read GetKeyField write SetKeyField;
    property ListField:string read GetListField write SetListField;
    property ListSource:string read GetListSource write SetListSource;
    property Color;
    property Enabled;
    property Text:Variant read GetText write SetText;
    property OnClick;
  end;

implementation
uses lclintf, Themes, LCLType, DBPropEdits, PropEdits, LR_Utils, LR_DBRel;

var
  lrBMP_LRDBLookupComboBox:TBitmap = nil;

procedure InitLRComp;
begin
  DoRegsiterControl(lrBMP_LRDBLookupComboBox, TlrDBLookupComboBox);
end;

{ TlrDBLookupComboBox }

function TlrDBLookupComboBox.GetKeyField: string;
begin
  Result:=FKeyField;
end;

function TlrDBLookupComboBox.GetListField: string;
begin
  Result:=FListField;
end;

function TlrDBLookupComboBox.GetListSource: string;
begin
  Result:=FListSource;
end;

function TlrDBLookupComboBox.GetText: Variant;
begin
  Result:=TDBLookupComboBox(FControl).KeyValue;
end;

procedure TlrDBLookupComboBox.SetKeyField(AValue: string);
begin
  if FKeyField=AValue then Exit;
  FKeyField:=AValue;
  TDBLookupComboBox(FControl).KeyField:=AValue;
end;

procedure TlrDBLookupComboBox.SetListField(AValue: string);
begin
  if FListField=AValue then Exit;
  FListField:=AValue;
  TDBLookupComboBox(FControl).ListField:=AValue;
end;

procedure TlrDBLookupComboBox.SetListSource(AValue: string);
var
  D:TDataSet;
begin
  if FListSource=AValue then Exit;
  FListSource:=AValue;

  D:=frFindComponent(nil, AValue) as TDataSet;
  if Assigned(D) then
  begin
    TDBLookupComboBox(FControl).ListSource:=frGetDataSource(OwnerForm, D);
  end;
end;

procedure TlrDBLookupComboBox.DBLookupComboBox1CloseUp(Sender: TObject);
begin
  if Assigned(TDBLookupComboBox(FControl).ListSource) and Assigned(TDBLookupComboBox(FControl).ListSource.DataSet) then
    TDBLookupComboBox(FControl).ListSource.DataSet.Locate(TDBLookupComboBox(FControl).KeyField, TDBLookupComboBox(FControl).KeyValue, []);
end;

procedure TlrDBLookupComboBox.SetText(AValue: Variant);
begin
  TDBLookupComboBox(FControl).KeyValue:=AValue;
end;

procedure TlrDBLookupComboBox.PaintDesignControl;
var
  AY, aH:integer;
  R1:TRect;
begin
  AY:=(DRect.Top + DRect.Bottom) div 2;
  aH:=Canvas.TextHeight(Name) div 2;
  Canvas.Frame3d(DRect, 1, bvLowered);
  Canvas.Brush.Color := FControl.Color;
  Canvas.FillRect(DRect);
  Canvas.Font:=FControl.Font;
  Canvas.TextRect(DRect, DRect.Left + 3, AY - aH, Name);

  R1:=DRect;
  R1.Left:=R1.Right - 16;
  DrawFrameControl(Canvas.Handle, R1, DFC_BUTTON, DFCS_BUTTONPUSH);
end;

function TlrDBLookupComboBox.CreateControl: TControl;
begin
  Result:=TDBLookupComboBox.Create(OwnerForm);
  TDBLookupComboBox(Result).Style:=csDropDownList;
  TDBLookupComboBox(Result).OnCloseUp:=@DBLookupComboBox1CloseUp;
end;

procedure TlrDBLookupComboBox.AfterLoad;
var
  D:TDataSet;
begin
  inherited AfterLoad;

  D:=frFindComponent(nil, FListSource) as TDataSet;
  if Assigned(D) then
  begin
    try
      TDBLookupComboBox(FControl).ListSource:=frGetDataSource(OwnerForm, D);
    finally
    end;
  end;
end;

constructor TlrDBLookupComboBox.Create(AOwnerPage: TfrPage);
begin
  inherited Create(AOwnerPage);
  BaseName:='lrDBLookupComboBox';
end;

procedure TlrDBLookupComboBox.LoadFromXML(XML: TLrXMLConfig; const Path: String
  );
begin
  inherited LoadFromXML(XML, Path);
  KeyField:=XML.GetValue(Path+'KeyField/Value'{%H-}, '');
  ListField:=XML.GetValue(Path+'ListField/Value'{%H-}, '');
  FListSource:=XML.GetValue(Path+'ListSource/Value'{%H-}, '');
end;

procedure TlrDBLookupComboBox.SaveToXML(XML: TLrXMLConfig; const Path: String);
begin
  inherited SaveToXML(XML, Path);
  XML.SetValue(Path+'KeyField/Value'{%H-}, FKeyField);
  XML.SetValue(Path+'ListField/Value'{%H-}, FListField);
  XML.SetValue(Path+'ListSource/Value'{%H-}, FListSource);
end;

type

  { TlrDBLookupComboBoxListSourceProperty }

  TlrDBLookupComboBoxListSourceProperty = class(TFieldProperty)
  public
    procedure FillValues(const Values: TStringList); override;
  end;

  { TlrDBLookupComboBoxFiledsProperty }

  TlrDBLookupComboBoxFiledsProperty = class(TFieldProperty)
  public
    procedure FillValues(const Values: TStringList); override;
  end;

{ TlrDBLookupComboBoxFiledsProperty }

procedure TlrDBLookupComboBoxFiledsProperty.FillValues(const Values: TStringList
  );
var
  L:TlrDBLookupComboBox;
  DS:TDataSet;
  i:integer;
begin
  if (GetComponent(0) is TlrDBLookupComboBox) then
  begin
    L:=GetComponent(0) as TlrDBLookupComboBox;
    if Assigned(TDBLookupComboBox(L.Control).ListSource) then
      frGetFieldNames(TfrTDataSet(TDBLookupComboBox(L.Control).ListSource.DataSet) , Values);
  end;
end;

{ TlrDBLookupComboBoxListSourceProperty }

procedure TlrDBLookupComboBoxListSourceProperty.FillValues(
  const Values: TStringList);
begin
  if (GetComponent(0) is TlrDBLookupComboBox) then
    frGetComponents(nil, TDataSet, Values, nil);
end;

initialization
  {$I lrdbdialogcontrols_img.inc}
  InitLRComp;

  RegisterPropertyEditor(TypeInfo(string), TlrDBLookupComboBox, 'ListSource', TlrDBLookupComboBoxListSourceProperty);
  RegisterPropertyEditor(TypeInfo(string), TlrDBLookupComboBox, 'KeyField', TlrDBLookupComboBoxFiledsProperty);
  RegisterPropertyEditor(TypeInfo(string), TlrDBLookupComboBox, 'ListField', TlrDBLookupComboBoxFiledsProperty);

finalization
  if Assigned(lrBMP_LRDBLookupComboBox) then
    FreeAndNil(lrBMP_LRDBLookupComboBox);
end.

