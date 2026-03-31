unit frmmain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Buttons, StdCtrls, ExtCtrls,
  markdown.control, markdown.processors, markdown.canvasrender;

type

  { TMainForm }

  TMainForm = class(TForm)
    btnFile: TButton;
    ODMarkDown: TOpenDialog;
    procedure btnFileClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  private
    FRender:TMarkDownCanvasRenderer;
    PMarkDown: TMarkDownControl;
    procedure DoGetImage(Sender: TObject; const aURL: string; var aImage: TPicture);
    procedure DoOpenURL(Sender: TObject; aURL: String);
    procedure RenderFile(const aFileName: string);
  public

  end;

var
  MainForm: TMainForm;
  SB : TScrollBox;

implementation

{$R *.lfm}

uses Clipbrd, lcltype, lclintf;

const
  DefaultFile = 'README.md';

{ TMainForm }

procedure TMainForm.btnFileClick(Sender: TObject);

begin
  with ODMarkDown do
    if Execute then
      PMarkDown.MarkDown.LoadFromFile(FileName);
end;

procedure TMainForm.FormCreate(Sender: TObject);

begin
  PMarkDown:=TMarkDownControl.Create(Self);
  PMarkDown.Parent:=Self;
  PMarkDown.Left:=16;
  PMarkDown.Top:=16;
  PMarkDown.AnchorParallel(akRight,16,Self);
  PMarkDown.AnchorToNeighbour(akBottom,16,btnFile);
  PMarkDown.OnGetImage:=@DoGetImage;
  PMarkDown.OnOpenURL:=@DoOpenURL;
  PMarkDown.Visible:=True;
  PMarkDown.MonoFontName:='Monospace';
  if FileExists(DefaultFile) then
    PMarkDown.MarkDown.LoadFromFile(DefaultFile);
end;

procedure TMainForm.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);

begin
  if ((Key=Ord('C')) or (Key=VK_INSERT)) and (Shift=[ssCtrl]) then
    begin
    // The control does this by itself, we do this only to be able to show the message
    PMarkDown.CopySelectionToClipBoard;
    ShowMessage('Selection copied to clipboard');
    end;
end;

procedure TMainForm.RenderFile(const aFileName : string);
var
  S : TStrings;
begin
  S:=TStringList.Create;
  try
    S.LoadFromFile(aFileName);
    FRender.ParseMarkdown(S);
  finally
    S.Free;
  end;
end;

procedure TMainForm.DoGetImage(Sender: TObject; const aURL: string; var aImage: TPicture);
begin
  if FileExists(aURL) then
    begin
    aImage:=TPicture.Create;
    aImage.LoadFromFile(aURL);
    end;
end;

procedure TMainForm.DoOpenURL(Sender: TObject; aURL: String);
begin
  OpenURL(aURL);
end;


end.

