unit mainform;
{
 Richmemo Inline demo

 Author: Dmitry 'skalogryz' Boyarintsev

 *****************************************************************************
 *                                                                           *
 *  This file is part of the Rich Memo package                               *
 *  You're free to use the project and the file in anyway you find fit.      *
 *  You're free to copy and modify the file. No need to keep the refernce    *
 *  to the origin of the file.                                               *
 *                                                                           *
 *  Cheetah logo image has been aquired from freepascal site.                *
 *  http://www.freepascal.org/pic/logo.gif                                   *
 *                                                                           *
 *****************************************************************************
}

{$mode objfpc}{$H+}

interface

uses
  Types, Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
  LCLIntf, StdCtrls, ExtCtrls, RichMemo, RichMemoUtils;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    ImageList1: TImageList;
    OpenDialog1: TOpenDialog;
    RichMemo1: TRichMemo;
    Timer1: TTimer;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
    framecnt : Integer;
    anims : TList;
    procedure AnimRemove(Sender: TObject);
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

type
  { TInlineImage }

  TInlineImage = class(TRichMemoInline)
  public
    imageList : TImageList;
    frame     : integer;
    startTime : LongWord;
    // in this particular demo, each animated image gets its own timer
    // infact, this is ineffecient way and it should only be one timer used
    visible   : Boolean;
    OnRemove  : TNotifyEvent;
    destructor Destroy; override;
    procedure Draw(Canvas: TCanvas; const ASize: TSize); override;
    procedure SetVisible(AVisible: Boolean); override;
  end;

{ TInlineImage }

destructor TInlineImage.Destroy;
begin
  if Assigned(OnRemove) then OnRemove(self);
  inherited Destroy;
end;


procedure TInlineImage.Draw(Canvas: TCanvas; const ASize: TSize);
begin
  imageList.Draw(Canvas, 0,0, frame);
end;

procedure TInlineImage.SetVisible(AVisible: Boolean);
begin
  visible:=AVisible;
end;

{ TForm1 }

procedure TForm1.Button1Click(Sender: TObject);
begin
  if OpenDialog1.Execute then
    LoadRTFFile( RichMemo1, OpenDialog1.FileName );
end;

procedure TForm1.Button2Click(Sender: TObject);
var
  inlineimg : TInlineImage;
begin
  inlineimg := TInlineImage.Create;
  inlineimg.imageList:=ImageList1;
  inlineimg.frame:=framecnt;
  RichMemo1.InDelInline(inlineimg, RichMemo1.SelStart, 0, Size(round(ImageList1.Width*72/96),round(ImageList1.Height*72/96)));

  inc(framecnt);
  if framecnt>=ImageList1.Count then framecnt:=0;
end;

procedure TForm1.Button3Click(Sender: TObject);
var
  inlineimg : TInlineImage;
const
  ScreenToDocDPI = 72 / 96;
begin
  inlineimg := TInlineImage.Create;
  inlineimg.imageList:=ImageList1;
  inlineimg.startTime:=GetTickCount;
  anims.add(inlineimg);
  RichMemo1.InDelInline(inlineimg, RichMemo1.SelStart, 0, Size(round(ImageList1.Width*ScreenToDocDPI),round(ImageList1.Height*ScreenToDocDPI)));
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  anims:=TList.Create;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  anims.Free;
  anims:=nil;
end;

procedure TForm1.Timer1Timer(Sender: TObject);
var
  i  : integer;
  im : TInlineImage;
  diff : LongWord;
  tck  : LongWord;
begin
  tck:=GetTickCount;
  for i:=0 to anims.Count-1 do begin
    im := TInlineImage(anims[i]);
    if not im.visible then Exit;
    diff:=tck-im.startTime;
    im.frame:=round(diff/55) mod im.imageList.Count;
    im.Invalidate;
  end;
end;

procedure TForm1.AnimRemove(Sender: TObject);
begin
  if Assigned(anims) then anims.Remove(Sender);
end;

end.


