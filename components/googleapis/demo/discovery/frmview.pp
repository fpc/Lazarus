unit frmview;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ButtonPanel,
  SynEdit, SynHighlighterMulti, SynHighlighterJScript, SynHighlighterPas;

type

  { TViewForm }
  TSyntax = (sPascal,sJSON);
  TViewForm = class(TForm)
    ButtonPanel1: TButtonPanel;
    SEFile: TSynEdit;
    SynFPC: TSynFreePascalSyn;
    SynJS: TSynJScriptSyn;
    procedure FormDestroy(Sender: TObject);
  private
    FFileName: String;
    FFreeStream: Boolean;
    FStream: TStream;
    FSyntax: TSyntax;
    procedure SetFileName(AValue: String);
    procedure SetStream(AValue: TStream);
    procedure SetSyntax(AValue: TSyntax);
    { private declarations }
  public
    { public declarations }
    Property FileName : String Read FFileName Write SetFileName;
    Property Syntax : TSyntax Read FSyntax Write SetSyntax;
    Property Stream : TStream Read FStream Write SetStream;
    Property FreeStream : Boolean Read FFreeStream Write FFreeStream;
  end;

var
  ViewForm: TViewForm;

implementation

{$R *.lfm}

{ TViewForm }

procedure TViewForm.FormDestroy(Sender: TObject);
begin
  If Assigned(FStream) and FFreeStream then
    FreeAndNil(FStream);
end;

procedure TViewForm.SetFileName(AValue: String);
begin
  if FFileName=AValue then Exit;
  FFileName:=AValue;
  Case lowercase(ExtractFileExt(FFilename)) of
   '.pas','.pp','.inc','.lpr','.dpr' :
      Syntax:=sPascal;
   '.json','.js':
      Syntax:=sJSON;
  end;
  SEFIle.Lines.LoadFromFile(AValue);
end;

procedure TViewForm.SetStream(AValue: TStream);
begin
  if FStream=AValue then Exit;
  FStream:=AValue;
  SEFIle.Lines.LoadFromStream(AValue);
end;

procedure TViewForm.SetSyntax(AValue: TSyntax);
begin
  if FSyntax=AValue then Exit;
  FSyntax:=AValue;
  Case FSyntax of
    sPascal: SEFile.Highlighter:=SynFPC;
    sJSON : SEFile.Highlighter:=SynJS;
  end;
  SEFile.Highlighter.Enabled:=True;
end;

end.

