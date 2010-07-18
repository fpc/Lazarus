
{*****************************************}
{                                         }
{             FastReport v2.3             }
{             Object Inspector            }
{                                         }
{  Copyright (c) 1998-99 by Tzyganenko A. }
{                                         }
{*****************************************}

unit LR_insp;

interface

{$I LR_Vers.inc}

uses
  Classes, SysUtils, LResources,LMessages,Messages,
  Forms, Controls, Graphics, Dialogs,
  ExtCtrls, Buttons, StdCtrls,
  
  LCLIntf,LCLType,LCLProc,
  
  LR_Class, LR_Const, lr_propedit;

type
  TInspModifyEvent = procedure(Item: Integer; var EditText: String) of object;

  TCtrlStyle = (csEdit, csDefEditor);

  TProp = class
    Addr    : PChar;
    Style   : TCtrlStyle;
    Editor  : TPropEditor;
    Enabled : Boolean;
    
    constructor Create(a: PChar; st: TCtrlStyle; de: TPropEditor); virtual;
  end;

  TfrInspForm = class(TForm)
    Edit1: TEdit;
    PaintBox1: TPaintBox;
    SpeedButton1: TSpeedButton;
    
    procedure PaintBox1Paint(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure PaintBox1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure Edit1KeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure SpeedButton1Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormDeactivate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
//    procedure Edit1DblClick(Sender: TObject);
    procedure Edit1KeyPress(Sender: TObject; var Key: Char);
    procedure FormResize(Sender: TObject);
  private
    { Private declarations }
    FItems        : TStringList;
    FItemIndex    : Integer;
    FOnModify     : TInspModifyEvent;
    FRowHeight    : Integer;
    w, w1         : Integer;

    procedure SetItems(Value: TStringList);
    procedure SetItemIndex(Value: Integer);
    function GetCount: Integer;
    procedure DrawOneLine(aCanvas : TCanvas; i: Integer; a: Boolean);
    procedure SetItemValue(Value: String);
    function GetItemValue(i: Integer):String;
    function CurItem: TProp;

  protected
    procedure WMNCLButtonDblClk(var Message: TMessage); message LM_NCLBUTTONDBLCLK;
  public
    { Public declarations }
    View          : TfrView;
    HideProperties: Boolean;
    DefHeight     : Integer;
    DefWidth      : Integer;
    
    procedure ClearItems;
    procedure ItemsChanged;
    procedure EnableItem(Index: Integer; Enable: Boolean);
    property Items: TStringList read FItems write SetItems;
    property ItemIndex: Integer read FItemIndex write SetItemIndex;
    property Count: Integer read GetCount;
    
    property OnModify: TInspModifyEvent read FOnModify write FOnModify;
  end;


implementation

{$R *.lfm}

constructor TProp.Create(a: PChar; st: TCtrlStyle; de: TPropEditor);
begin
  inherited Create;
  Addr := a;
  Style := st;
  Editor := de;
  Enabled := True;
end;

function TfrInspForm.CurItem: TProp;
begin
  Result := nil;
  if (FItemIndex <> -1) and (Count > 0) then
    Result := TProp(FItems.Objects[FItemIndex]);
end;

procedure TfrInspForm.SetItems(Value: TStringList);
begin
  FItems.Assign(Value);
  FItemIndex := -1;
  PaintBox1.Invalidate;
  ItemIndex := 0;
end;

procedure TfrInspForm.SetItemValue(Value: String);
var
  p: TProp;
  s: String[255];
begin
  if HideProperties then Exit;
  p := TProp(FItems.Objects[FItemIndex]);
  s := Value;
  Move(s[0], p.Addr^, Ord(s[0]) + 1);
  if Assigned(FOnModify) then FOnModify(FItemIndex, Value);
  Edit1.Text := Value;
  Edit1.SelectAll;
  Edit1.Modified := False;
end;

function TfrInspForm.GetItemValue(i: Integer): String;
var
  p: TProp;
  s: String[255];
begin
  Result := '';
  p := TProp(FItems.Objects[i]);
  if p = nil then Exit;
  Move(p.Addr^, s[0], Ord(p.Addr^) + 1);
  Result := s;
end;

procedure TfrInspForm.SetItemIndex(Value: Integer);
var
  ww: Integer;
begin
  if Value>Count-1 then
    Value:=Count-1;
  if not TProp(FItems.Objects[Value]).Enabled then  Exit;
  
  Edit1.Visible:=((Count > 0) and not HideProperties);
  
  if (Count = 0) or (FItemIndex = Value) then   Exit;

  if (FItemIndex<>-1) and Edit1.Modified then
      SetItemValue(Edit1.Text);
  {$IFDEF DebugLR}
  DebugLn('TfrInspForm.SetItemIndex(',IntToStr(Value),')');
  {$ENDIF}
  FItemIndex := Value;
  SpeedButton1.Visible:=((CurItem.Style = csDefEditor) and not HideProperties);
  
  Edit1.ReadOnly:=(CurItem.Style=csDefEditor);
  ww:=w-w1-4;
  if SpeedButton1.Visible then
  begin
    SpeedButton1.SetBounds(w - 16, 2 + FItemIndex * FRowHeight + 1, 14, FRowHeight - 2);
    Dec(ww, 15);
    Edit1.Text := '(' + FItems[FItemIndex] + ')';
  end
  else
    Edit1.Text := GetItemValue(FItemIndex);
    
  Edit1.SetBounds(w1+2, 2+FItemIndex*FRowHeight+1, ww, FRowHeight-2);
  Edit1.SelectAll;
  Edit1.Modified := False;
  PaintBox1.Invalidate
end;

function TfrInspForm.GetCount: Integer;
begin
  Result := FItems.Count;
end;

procedure TfrInspForm.ItemsChanged;
begin
  FItemIndex := -1;
  ItemIndex := 0;
end;

procedure TfrInspForm.EnableItem(Index: Integer; Enable: Boolean);
begin
  TProp(FItems.Objects[Index]).Enabled := Enable;
  
  PaintBox1.Invalidate
end;

procedure TfrInspForm.DrawOneLine(aCanvas : TCanvas; i: Integer; a: Boolean);

  procedure IntLine(x, y, dx, dy: Integer);
  begin
    aCanvas.MoveTo(x, y);
    aCanvas.LineTo(x + dx, y + dy);
  end;
  
begin
  if not TProp(FItems.Objects[i]).Enabled then Exit;
  if Count > 0 then
  begin
    with aCanvas do
    begin
      Brush.Color := clBtnFace;
      Pen.Color := clBtnShadow;
      Font.Size := 8;
      Font.Style := [];
      Font.Color := clBlack;
      
      if a then
      begin
        Pen.Color := clBtnShadow;
        IntLine(2, 0 + i * FRowHeight, w - 4, 0);
        IntLine(w1 - 1, 2 + i * FRowHeight, 0, FRowHeight);
        Pen.Color := clBlack;
        IntLine(2, 1 + i * FRowHeight, w - 4, 0);
        IntLine(2, 1 + i * FRowHeight, 0, FRowHeight + 1);
        Pen.Color := clBtnHighlight;
        IntLine(3, FRowHeight + 1 + i * FRowHeight, w - 5, 0);
        IntLine(Edit1.Left, 2 + i * FRowHeight, Edit1.Width, 0);
        IntLine(w1, 2 + i * FRowHeight, 0, FRowHeight);
        IntLine(w1 + 1, 2 + i * FRowHeight, 0, FRowHeight);
        TextOut(7, 3 + i * FRowHeight, FItems[i]);
      end
      else
      begin
        IntLine(2, FRowHeight + 1 + i * FRowHeight, w - 4, 0);
        IntLine(w1 - 1, 2 + i * FRowHeight, 0, FRowHeight);
        Pen.Color := clBtnHighlight;
        IntLine(w1, 2 + i * FRowHeight, 0, FRowHeight);
        TextOut(7, 3 + i * FRowHeight, FItems[i]);
        Font.Color := clNavy;
        if TProp(FItems.Objects[i]).Style = csEdit then
          TextOut(w1 + 2, 3 + i * FRowHeight, GetItemValue(i))
        else
          TextOut(w1 + 2, 3 + i * FRowHeight, '(' + FItems[i] + ')');
      end;
    end;
  end;
end;

procedure TfrInspForm.PaintBox1Paint(Sender: TObject);
var
  i: Integer;
  Rc: TRect;
begin
  {$IFDEF DebugLR}
  DebugLn('TfrInspForm.PaintBox1Paint');
  DebugLn('Enabled=',BoolToStr(Enabled));
  DebugLn('PaintBox1.enabled=',BoolToStr(PaintBox1.Enabled));
  DebugLn('Edit1.Enabled=',BoolToStr(Edit1.Enabled));
  {$ENDIF}
  with PaintBox1.Canvas do
  begin
    Rc:=ClipRect;
    Brush.Color:=clBtnFace;
    FillRect(Rc);

    if not HideProperties then
    begin
      for i := 0 to Count-1 do
        if i <> FItemIndex then
           DrawOneLine(PaintBox1.Canvas,i, False);
      if FItemIndex <> -1 then
         DrawOneLine(PaintBox1.Canvas,fItemIndex, True);
    end;
  end;
end;

procedure TfrInspForm.FormCreate(Sender: TObject);
begin
  w := PaintBox1.Width;
  w1 := w div 2;
  SpeedButton1.Visible := False;
  FItemIndex := -1;
  FItems := TStringList.Create;
  Caption := sObjectInspector;
  DefHeight := Height - 3;
  DefWidth := Width;
  FRowHeight := -Font.Height + 5;
  FormResize(nil);
end;

procedure TfrInspForm.FormDestroy(Sender: TObject);
begin
  ClearItems;
  FItems.Free;
end;

procedure TfrInspForm.ClearItems;
var
  i: Integer;
begin
  for i := 0 to FItems.Count - 1 do
    TProp(FItems.Objects[i]).Free;
  FItems.Clear;
end;

procedure TfrInspForm.PaintBox1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  {$IFDEF DebugLR}
  DebugLn('PaintBox1MouseDown()');
  {$ENDIF}
  if HideProperties then Exit;
  {$IFDEF DebugLR}
  DebugLn('PaintBox1MouseDown ItemIndex:=',IntToStr(y div FRowHeight));
  {$ENDIF}
  ItemIndex := y div FRowHeight;
  
  Edit1.SetFocus;
  PaintBox1.Invalidate;
end;

procedure TfrInspForm.Edit1KeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if HideProperties then Exit;
  if Key = vk_Up then
  begin
    if ItemIndex > 0 then
      ItemIndex := ItemIndex - 1;
    Key := 0;
  end
  else if Key = vk_Down then
  begin
    if ItemIndex < Count - 1 then
      ItemIndex := ItemIndex + 1;
    Key := 0;
  end;
end;

procedure TfrInspForm.Edit1KeyPress(Sender: TObject; var Key: Char);
begin
  if Key = #13 then
  begin
    if CurItem.Style = csEdit then
    begin
      if Edit1.Modified then
        SetItemValue(Edit1.Text);
      Edit1.Modified := False;
    end
    else
      SpeedButton1Click(nil);
      
    Edit1.SelectAll;
    Key := #0;
  end;
end;

procedure TfrInspForm.SpeedButton1Click(Sender: TObject);
var
  s: String;
begin
  if HideProperties then Exit;
  with CurItem.Editor do
  begin
    View := self.View;
    s := '';
    if ShowEditor = mrOk then
      if Assigned(FOnModify) then
         FOnModify(FItemIndex, s);
  end;
end;

{procedure TfrInspForm.Edit1DblClick(Sender: TObject);
begin
  if CurItem.Style = csDefEditor then
    SpeedButton1Click(nil);
end;
}
procedure TfrInspForm.FormShow(Sender: TObject);
begin
  SetWindowPos(Handle, HWND_TOPMOST, 0, 0, 0, 0, SWP_NOMOVE or
    SWP_NOSIZE or SWP_NOACTIVATE);
end;

procedure TfrInspForm.FormDeactivate(Sender: TObject);
begin
  if CurItem = nil then Exit;
  if CurItem.Style = csEdit then
  begin
    if Edit1.Modified then
      SetItemValue(Edit1.Text);
    Edit1.Modified := False;
  end;
end;

procedure TfrInspForm.WMNCLButtonDblClk(var Message: TMessage);
begin
  Inherited;
  if Height = DefHeight then
  begin
    Height := 0;
    Width := DefWidth div 2;
    PaintBox1.Visible:=False;
  end
  else
  begin
    Height := DefHeight;
    Width := DefWidth;
    PaintBox1.Visible:=True;
  end;
end;

procedure TfrInspForm.FormResize(Sender: TObject);
begin
  w := PaintBox1.Width;
  Edit1.Width := w - w1 - 4;
  PaintBox1.Invalidate;
end;

end.

