unit Unit1;
(*

This is an example how to write a highlighter from scratch.

    See the units for each example highlighter.

This is NOT about extending the IDE. This is about SynEdit and it's Highlighter only.
Therefore this does not include:
- registration in the component pallette.
- Using the Object Inspector
Those steps are the same as they would be for any other self wriiten componont.

*)
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, SynEdit, Forms, Controls, Graphics, Dialogs, StdCtrls,
  SimpleHl, ContextHL, FoldHl;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    SynEdit1: TSynEdit;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { private declarations }
    FSynDemoHl: TSynDemoHl;
    FSynDemoHlContext: TSynDemoHlContext;
    FSynDemoHlFold: TSynDemoHlFold;
  public
    { public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
  FSynDemoHl := TSynDemoHl.Create(Self);
  FSynDemoHlContext := TSynDemoHlContext.Create(Self);
  FSynDemoHlFold := TSynDemoHlFold.Create(Self);
  SynEdit1.Highlighter := FSynDemoHl;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  SynEdit1.Highlighter := FSynDemoHl;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  SynEdit1.Highlighter := FSynDemoHlContext;
end;

procedure TForm1.Button3Click(Sender: TObject);
begin
  SynEdit1.Highlighter := FSynDemoHlFold;
end;

end.

