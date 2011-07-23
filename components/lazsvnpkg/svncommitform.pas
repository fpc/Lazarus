unit SVNCommitForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  ButtonPanel, StdCtrls, Process, LCLProc;

type

  { TSVNCommitFrm }

  TSVNCommitFrm = class(TForm)
    ButtonPanel: TButtonPanel;
    SVNCommitMemo: TMemo;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure OKButtonClick(Sender: TObject);
  private
    { private declarations }
    FSVNCommandLine: string;
  public
    { public declarations }
    procedure Execute(Data: PtrInt);
    property SVNCommandLine: string read FSVNCommandLine write FSVNCommandLine;
  end; 

procedure ShowSVNCommitFrm(ACmdLine: string);

var
  SVNCommitFrm: TSVNCommitFrm;

implementation

{$R *.lfm}

uses
  SVNClasses;

procedure ShowSVNCommitFrm(ACmdLine: string);
begin
  if not Assigned(SVNCommitFrm) then
    SVNCommitFrm := TSVNCommitFrm.Create(nil);

  SVNCommitFrm.SVNCommandLine:=ACmdLine;
  SVNCommitFrm.ShowModal;
end;

{ TSVNCommitFrm }

procedure TSVNCommitFrm.FormShow(Sender: TObject);
begin
  Application.QueueAsyncCall(@Execute, 0);
end;

procedure TSVNCommitFrm.OKButtonClick(Sender: TObject);
begin
  Close;
end;

procedure TSVNCommitFrm.Execute(Data: PtrInt);
begin
  CmdLineToMemo(SVNCommandLine, SVNCommitMemo);
end;

procedure TSVNCommitFrm.FormCreate(Sender: TObject);
begin
  Caption := rsLazarusSVNCommit;
end;

procedure TSVNCommitFrm.FormDestroy(Sender: TObject);
begin
  SVNCommitFrm := nil;
end;

procedure TSVNCommitFrm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  CloseAction := caFree;
end;


end.

