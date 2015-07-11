unit PoCheckerMemoDlg;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, types, FileUtil, Forms, Controls, Graphics, Dialogs,
  Buttons, StdCtrls, LCLProc, PoCheckerSettings;

type

  { TMemoForm }

  TMemoForm = class(TForm)
    BitBtn1: TBitBtn;
    MsgMemo: TMemo;
  private
    FMsg: String;
    procedure SetMsg(AValue: String);
    { private declarations }
  public
    { public declarations }
    property Message: String read FMsg write SetMsg;
  end;

function MemoDlg(const ACaption, AMsg: String): TModalResult;

implementation


function MemoDlg(const ACaption, AMsg: String): TModalResult;
var
  Dlg: TMemoForm;
begin
  Dlg := TMemoForm.Create(nil);
  try
    Dlg.Caption := ACaption;
    Dlg.SetMsg(AMsg);
    Result := Dlg.ShowModal;
  finally
    Dlg.Free;
  end;
end;

{$R *.lfm}

{ TMemoForm }

procedure TMemoForm.SetMsg(AValue: String);
var
  i, LH, TW, MaxTW, BottomGap, ReqH: Integer;
  Size: TSize;
  ARect, WARect: TRect;
begin
  if FMsg = AValue then Exit;
  FMsg := AValue;
  MsgMemo.Text := AValue;

  //for i := 1 to 50 do MsgMemo.Lines.Add(IntToStr(i));

  Size := Canvas.TextExtent('qWM');
  LH := Size.cy;
  MaxTW := Constraints.MinWidth;
  for i := 0 to MsgMemo.Lines.Count - 1 do
  begin
    TW := Self.Canvas.TextWidth(MsgMemo.Lines[i]);
    if TW > MaxTW then MaxTW := TW;
  end;
  ClientWidth := MaxTW + 50;

  ReqH := MsgMemo.Lines.Count * LH;
  BottomGap := ClientHeight - MsgMemo.Height;
  ReqH := ReqH + BottomGap;
  ClientHeight := ReqH;
  ARect := Self.BoundsRect;
  WARect := Screen.WorkAreaRect;
  WARect.Right := WARect.Right - 50;
  WARect.Bottom := WARect.Bottom - 75;
  //debugln('ARect         = ',DbgS(ARect));
  ARect := FitToRect(ARect, WARect);
  //debugln('ARect         = ',DbgS(ARect));
  //debugln('Screen.WARect = ',DbgS(WARect));
  BoundsRect := ARect;
end;

end.

