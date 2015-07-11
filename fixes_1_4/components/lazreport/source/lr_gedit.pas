
{*****************************************}
{                                         }
{             FastReport v2.3             }
{              Picture editor             }
{                                         }
{  Copyright (c) 1998-99 by Tzyganenko A. }
{                                         }
{*****************************************}

unit LR_GEdit;

interface

{$I LR_Vers.inc}

uses
  Classes, SysUtils, LResources,
  Forms, Controls, Graphics, Dialogs,
  ExtCtrls, Buttons, StdCtrls, ExtDlgs,

  LR_Const;

type

  { TfrGEditorForm }

  TfrGEditorForm = class(TForm)
    Image1: TImage;
    Bevel1: TBevel;
    CB1: TCheckBox;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    OpenDlg: TOpenPictureDialog;
    procedure BitBtn1Click(Sender: TObject);
    procedure CB1Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Button5Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frGEditorForm: TfrGEditorForm;

implementation

{$R *.lfm}

uses LR_Class, LR_Desgn;

procedure TfrGEditorForm.BitBtn1Click(Sender: TObject);
begin
  if OpenDlg.Execute then
    Image1.Picture.LoadFromFile(OpenDlg.FileName);
end;

procedure TfrGEditorForm.CB1Click(Sender: TObject);
begin
  Image1.Stretch := CB1.Checked;
end;

procedure TfrGEditorForm.Button4Click(Sender: TObject);
begin
  Image1.Picture.Assign(nil);
end;

procedure TfrGEditorForm.FormCreate(Sender: TObject);
begin
  Caption := sGEditorFormCapt;
  CB1.Caption := sGEditorFormStretch;
  Button3.Caption := sGEditorFormLoad;
  Button4.Caption := sGEditorFormClear;
  Button5.Caption := sGEditorFormMemo;
  Button1.Caption := sOk;
  Button2.Caption := sCancel;
end;

procedure TfrGEditorForm.Button5Click(Sender: TObject);
begin
  TfrDesignerForm(frDesigner).ShowMemoEditor;
end;

end.

