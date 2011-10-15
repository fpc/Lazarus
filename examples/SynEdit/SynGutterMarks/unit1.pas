unit Unit1; 

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls, Spin,
  SynEdit, SynEditTypes, SynEditMarks;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    chkMarkGutterAutoSize: TCheckBox;
    chkDrawBookMarkFirst: TCheckBox;
    chkBookMark: TCheckBox;
    ImageList1: TImageList;
    imgBookMarks: TImageList;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    spinMarkGutterWidth: TSpinEdit;
    SpinLine: TSpinEdit;
    spinImg: TSpinEdit;
    SynEdit1: TSynEdit;
    procedure Button1Click(Sender: TObject);
    procedure chkDrawBookMarkFirstChange(Sender: TObject);
    procedure chkBookMarkChange(Sender: TObject);
    procedure chkMarkGutterAutoSizeChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure spinMarkGutterWidthChange(Sender: TObject);
    procedure SynEdit1StatusChange(Sender: TObject; Changes: TSynStatusChanges);
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  Form1: TForm1; 

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.SynEdit1StatusChange(Sender: TObject; Changes: TSynStatusChanges);
begin
  if scModified in Changes then SpinLine.MaxValue := SynEdit1.Lines.Count;
end;

procedure TForm1.Button1Click(Sender: TObject);
var
  m: TSynEditMark;
begin
  if chkBookMark.Checked
  then begin
    SynEdit1.SetBookMark(spinImg.Value, 1, SpinLine.Value);
  end
  else begin
    m := TSynEditMark.Create(SynEdit1);
    m.Line := SpinLine.Value;
    m.ImageList := ImageList1;
    m.ImageIndex := spinImg.Value;
    m.Visible := true;
    SynEdit1.Marks.Add(m);
  end;
end;

procedure TForm1.chkDrawBookMarkFirstChange(Sender: TObject);
begin
  SynEdit1.BookMarkOptions.DrawBookmarksFirst := chkDrawBookMarkFirst.Checked;
end;

procedure TForm1.chkBookMarkChange(Sender: TObject);
begin
  if chkBookMark.Checked
  then spinImg.MaxValue := 9
  else spinImg.MaxValue := 4;
end;

procedure TForm1.chkMarkGutterAutoSizeChange(Sender: TObject);
begin
  SynEdit1.Gutter.MarksPart(0).AutoSize := chkMarkGutterAutoSize.Checked;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  (* SynEdit currently does not include own images for bookmarks.
     If no images are given it will currently crash
  *)
  SynEdit1.BookMarkOptions.BookmarkImages := imgBookMarks;
  spinMarkGutterWidth.Value := SynEdit1.Gutter.MarksPart(0).Width;
  chkMarkGutterAutoSize.Checked := SynEdit1.Gutter.MarksPart(0).AutoSize;
  chkDrawBookMarkFirst.Checked := SynEdit1.BookMarkOptions.DrawBookmarksFirst;
end;

procedure TForm1.spinMarkGutterWidthChange(Sender: TObject);
begin
  SynEdit1.Gutter.MarksPart(0).Width := spinMarkGutterWidth.Value;
end;

end.

