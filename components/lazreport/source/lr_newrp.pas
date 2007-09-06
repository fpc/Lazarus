
{*****************************************}
{                                         }
{             FastReport v2.3             }
{             Template viewer             }
{                                         }
{  Copyright (c) 1998-99 by Tzyganenko A. }
{                                         }
{*****************************************}

unit LR_Newrp;

interface

{$I LR_Vers.inc}

uses
  Classes, SysUtils, LResources,
  Forms, Controls, Graphics, Dialogs,
  Buttons, StdCtrls,ExtCtrls,
  LR_Const;

type
  TfrTemplForm = class(TForm)
    GroupBox1: TGroupBox;
    Memo1: TMemo;
    Image1: TImage;
    Button1: TButton;
    Button2: TButton;
    LB1: TListBox;
    procedure FormActivate(Sender: TObject);
    procedure ListBox1Click(Sender: TObject);
    procedure FormDeactivate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure LB1DblClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    TemplName: String;
  end;

var
  frTemplForm: TfrTemplForm;

implementation

uses LR_Class, LR_Desgn;

var
  Path: String;


procedure TfrTemplForm.FormActivate(Sender: TObject);
var
  SearchRec: TSearchRec;
  r: Word;
begin
  if frTemplateDir = '' then
    Path := ''
  else
    Path := frTemplateDir + DirectorySeparator;
  LB1.Items.Clear;
  R := FindFirst(Path + '*.frt', faAnyFile, SearchRec);
  while R = 0 do
  begin
    if (SearchRec.Attr and faDirectory) = 0 then
      LB1.Items.Add(ChangeFileExt(SearchRec.Name, ''));
    R := FindNext(SearchRec);
  end;
  FindClose(SearchRec);
  Memo1.Lines.Clear;
  Image1.Picture.Clear;
  Button1.Enabled := False;
end;

procedure TfrTemplForm.ListBox1Click(Sender: TObject);
begin
  Button1.Enabled := LB1.ItemIndex <> -1;
  if Button1.Enabled then
  begin
    CurReport.LoadTemplate(Path + LB1.Items[LB1.ItemIndex] + '.frt',
      Memo1.Lines, Image1.Picture.Bitmap,False);
  end;
end;

procedure TfrTemplForm.LB1DblClick(Sender: TObject);
begin
  if Button1.Enabled then ModalResult := mrOk;
end;

procedure TfrTemplForm.FormDeactivate(Sender: TObject);
begin
  if ModalResult = mrOk then
    TemplName := Path + LB1.Items[LB1.ItemIndex] + '.frt';
end;

procedure TfrTemplForm.FormCreate(Sender: TObject);
begin
  Caption := sTemplFormNewRp;
  GroupBox1.Caption := sTemplFormDesc;
  Button1.Caption := sOk;
  Button2.Caption := sCancel;
end;

initialization
  {$I lr_newrp.lrs}

end.

