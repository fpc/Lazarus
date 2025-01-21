unit FrmAIssistFPDocEdit;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  // aissist
  AIClient, StrAIssist, FrmAixplain;

const
  SDescribeProcPrompt = 'Explain the following function in one sentence:';

type

  { TAIssistFPDocEditDlg }

  TAIssistFPDocEditDlg = class(TAIxplainForm)
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject); override;
    procedure OKButtonClick(Sender: TObject);
  protected
    FSource: string;
    procedure CreatePrompt; override;
    procedure HandleAIResponse(Sender: TObject; aResponses: TPromptResponseArray); override;
  public
    Description: string;
    function Describe(aAIClient: TAIClient; const Src: string; out aDescription: string): boolean; virtual;
  end;

var
  AIssistFPDocEditDlg: TAIssistFPDocEditDlg;

implementation

{$R *.lfm}

{ TAIssistFPDocEditDlg }

procedure TAIssistFPDocEditDlg.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  inherited;
end;

procedure TAIssistFPDocEditDlg.FormCreate(Sender: TObject);
begin
  inherited;
end;

procedure TAIssistFPDocEditDlg.OKButtonClick(Sender: TObject);
begin
  // todo: extract the description

  ModalResult:=mrOK;
end;

procedure TAIssistFPDocEditDlg.CreatePrompt;
var
  Src: TStringList;
begin
  Src:=TStringList.Create;
  try
    Src.Text:=FSource;
    MPrompt.Lines.Add(SDescribeProcPrompt);
    MPrompt.Lines.Add('');
    MPrompt.Lines.AddStrings(Src);
  finally
    Src.Free;
  end;
end;

procedure TAIssistFPDocEditDlg.HandleAIResponse(Sender: TObject; aResponses: TPromptResponseArray);
begin
  inherited HandleAIResponse(Sender, aResponses);
  if Length(AResponses)=0 then
    Description:=''
  else
    Description:=aResponses[0].Response;
end;

function TAIssistFPDocEditDlg.Describe(aAIClient: TAIClient; const Src: string; out
  aDescription: string): boolean;
begin
  Result:=false;
  FSource:=Src;
  aDescription:='';

  FAIClient:=aAIClient;
  FAIClient.OnError:=@HandleAIError;
  FAIClient.SynchronizeCallBacks:=True;
  CreatePrompt;
  SendPrompt;

  if (ShowModal=mrOk) and (Description>'') then
  begin
    aDescription:=Description;
    Result:=true;
  end;
end;

end.

