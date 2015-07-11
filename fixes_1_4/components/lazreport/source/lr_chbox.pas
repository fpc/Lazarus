
{*****************************************}
{                                         }
{             FastReport v2.3             }
{         Checkbox Add-In Object          }
{                                         }
{  Copyright (c) 1998-99 by Tzyganenko A. }
{                                         }
{*****************************************}

unit LR_ChBox;

interface

{$I lr_vers.inc}

uses
  Classes, SysUtils, LResources,
  Graphics,GraphType, Controls, Forms, Dialogs,
  Menus, LCLType, LCLIntf, LR_Class;



type

  { TfrCheckBoxObject }

  TfrCheckBoxObject = class(TComponent)
  public
    Constructor Create(aOwner : TComponent); override;
  end;

  { TfrCheckBoxView }

  TfrCheckBoxView = class(TfrView)
  private
    fChecked: Boolean;
    
    procedure DrawCheck(ARect: TRect; aChecked: Boolean);
  public
    constructor Create(AOwnerPage:TfrPage); override;
    procedure Draw(aCanvas: TCanvas); override;
    procedure Print(Stream: TStream); override;
    procedure ExportData; override;
    procedure DefinePopupMenu(Popup: TPopupMenu); override;

    procedure Assign(Source: TPersistent); override;
    procedure LoadFromStream(Stream: TStream); override;
    procedure SaveToStream(Stream: TStream); override;
    procedure LoadFromXML(XML: TLrXMLConfig; const Path: String); override;
    procedure SaveToXML(XML: TLrXMLConfig; const Path: String); override;
  published
    property Checked : Boolean read fChecked write fChecked;
    property DataField;
    property FillColor;
    property FrameColor;
    property Frames;
    property FrameStyle;
    property FrameWidth;
    property Script;
    property Restrictions;
  end;


implementation

{$R lr_checkbox.res}

uses LR_Utils, LR_Const;

var
  lrBMPCheckBox : TBitMap;


procedure TfrCheckBoxView.DrawCheck(ARect: TRect; aChecked: Boolean);

  procedure Line(x, y, x1, y1: Integer);
  begin
    Canvas.MoveTo(x, y);
    Canvas.LineTo(x1, y1);
  end;
  
begin
  InflateRect(ARect, Round(-4 * ScaleX), Round(-4 * ScaleY));
  with Canvas, ARect do
  begin
    Pen.Mode := pmCopy;
    Pen.Style := psSolid;
    Pen.Color := clBlack;
    if aChecked then
    begin
      Pen.Color := clBlack;
      Pen.Width := Round(3 * ScaleX);
      Line(Left, Top, Right, Bottom);
      Line(Left, Bottom, Right, Top);
    end;
  end;
end;

constructor TfrCheckBoxView.Create(AOwnerPage: TfrPage);
begin
  inherited Create(AOwnerPage);
  BeginUpdate;
  try
    Typ := gtAddIn;
    FrameWidth := 2;
    Frames:=frAllFrames;
    Checked:=True;
    BaseName := 'Check';
  Finally
    EndUpdate;
  end;
end;

procedure TfrCheckBoxView.Draw(aCanvas: TCanvas);
var
  IsChecked: Boolean;
begin
  BeginDraw(aCanvas);
  Memo1.Assign(Memo);
  CalcGaps;
  ShowBackground;
  IsChecked := Self.Checked;
  if Memo1.Count > 0 then
    IsChecked := Memo1[0] = '1';
  DrawCheck(DRect, IsChecked);
//  DrawCheck(DRect, Self.Checked);
  ShowFrame;
  RestoreCoord;
end;

procedure TfrCheckBoxView.Print(Stream: TStream);
begin
  BeginDraw(Canvas);
  Memo1.Assign(Memo);
  CurReport.InternalOnEnterRect(Memo1, Self);
  frInterpretator.DoScript(Script);
  if not Visible then Exit;

  if Memo1.Count > 0 then
    Memo1[0] := IntToStr(Trunc(Extended(frParser.Calc(Memo1[0]))));
  Stream.Write(Typ, 1);
  frWriteString(Stream, ClassName);
  SaveToStream(Stream);
end;

procedure TfrCheckBoxView.ExportData;
var
  s: String;
begin
  inherited;
  s := '';
  if Self.Checked then
      s := 'X';
  CurReport.InternalOnExportText(x, y, s, Self);
end;

procedure TfrCheckBoxView.DefinePopupMenu(Popup: TPopupMenu);
begin
  // no specific items in popup menu
  if Popup=nil then;
end;

procedure TfrCheckBoxView.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  if Source is TfrCheckBoxView then
    Self.Checked := TfrCheckBoxView(Source).Checked;
end;

procedure TfrCheckBoxView.LoadFromStream(Stream: TStream);
begin
  inherited LoadFromStream(Stream);
  Stream.Read(fChecked, SizeOf(fChecked));
end;

procedure TfrCheckBoxView.SaveToStream(Stream: TStream);
begin
  inherited SaveToStream(Stream);
  Stream.Write(fChecked, SizeOf(fChecked));
end;

procedure TfrCheckBoxView.LoadFromXML(XML: TLrXMLConfig; const Path: String);
begin
  inherited LoadFromXML(XML, Path);
  RestoreProperty('Checked',XML.GetValue(Path+'Data/Checked/Value',''));
end;

procedure TfrCheckBoxView.SaveToXML(XML: TLrXMLConfig; const Path: String);
begin
  inherited SaveToXML(XML, Path);
  XML.SetValue(Path+'Data/Checked/Value', GetSaveProperty('Checked'));
end;



{ TfrCheckBoxObject }

constructor TfrCheckBoxObject.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  
  if not assigned(lrBMPCheckBox) {and not (csDesigning in ComponentState)} then
  begin
    lrBMPCheckBox := TBitMap.Create;
    lrBMPCheckBox.LoadFromResourceName(HInstance, 'fr_checkbox');

    frRegisterObject(TfrCheckBoxView, lrBMPCheckBox, sInsCheckBox, nil);
  end;
end;

initialization 
  lrBMPCheckBox:=nil;
finalization
  if Assigned(lrBMPCheckBox) then
    FreeAndNil(lrBMPCheckBox);
end.
