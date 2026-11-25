unit frmmain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Buttons, StdCtrls, ExtCtrls,
  Markdown.Control, Markdown.Processors, Markdown.CanvasRender;

type

  { TMainForm }

  TMainForm = class(TForm)
    btnFile: TButton;
    cbWrapCode: TCheckBox;
    cbGitHubSpacing: TCheckBox;
    ODMarkdown: TOpenDialog;
    procedure btnFileClick(Sender: TObject);
    procedure cbGitHubSpacingChange(Sender: TObject);
    procedure cbWrapCodeChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  private
    FRender:TMarkdownCanvasRenderer;
    PMarkdown: TMarkdownControl;
    procedure DoGetImage(Sender: TObject; const aURL: string; var aImage: TPicture);
    procedure DoOpenURL(Sender: TObject; const aURL: String);
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
  with ODMarkdown do
    if Execute then
      PMarkdown.LoadFromFile(FileName);
end;

procedure TMainForm.cbGitHubSpacingChange(Sender: TObject);

begin
  if cbGitHubSpacing.Checked then
    begin
    // Approximation of GitHub spacing, for a line height of 1.5 em
    PMarkdown.ParagraphSpacing:=16;
    PMarkdown.HeadingSpacing:=33;
    PMarkdown.ListItemSpacing:=17;
    PMarkdown.CodeBlockPadding:=67;
    PMarkdown.TableCellPaddingX:=54;
    PMarkdown.TableCellPaddingY:=25;
    end
  else
    begin
    PMarkdown.ParagraphSpacing:=10;
    PMarkdown.HeadingSpacing:=0;
    PMarkdown.ListItemSpacing:=0;
    PMarkdown.CodeBlockPadding:=0;
    PMarkdown.TableCellPaddingX:=25;
    PMarkdown.TableCellPaddingY:=25;
    end;
end;

procedure TMainForm.cbWrapCodeChange(Sender: TObject);

begin
  PMarkdown.WrapCodeBlocks:=cbWrapCode.Checked;
end;

procedure TMainForm.FormCreate(Sender: TObject);

begin
  PMarkdown:=TMarkdownControl.Create(Self);
  PMarkdown.Parent:=Self;
  PMarkdown.Left:=16;
  PMarkdown.Top:=16;
  PMarkdown.AnchorParallel(akRight,16,Self);
  PMarkdown.AnchorToNeighbour(akBottom,16,btnFile);
  PMarkdown.OnGetImage:=@DoGetImage;
  PMarkdown.OnOpenURL:=@DoOpenURL;
  PMarkdown.Visible:=True;
  PMarkdown.MonoFontName:='Monospace';
  ActiveControl:=PMarkdown;
  if (ParamCount>0) and FileExists(ParamStr(1)) then
    PMarkdown.LoadFromFile(ParamStr(1))
  else if FileExists(DefaultFile) then
    PMarkdown.LoadFromFile(DefaultFile);
end;

procedure TMainForm.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);

begin
  if ((Key=Ord('C')) or (Key=VK_INSERT)) and (Shift=[ssCtrl]) then
    begin
    // The control does this by itself, we do this only to be able to show the message
    PMarkdown.CopySelectionToClipBoard;
    ShowMessage('Selection copied to clipboard:'+LineEnding+'['+PMarkdown.SelectedText+']');
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

procedure TMainForm.DoOpenURL(Sender: TObject; const aURL: String);
begin
  OpenURL(aURL);
end;


end.

