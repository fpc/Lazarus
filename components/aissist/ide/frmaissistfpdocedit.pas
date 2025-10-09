unit FrmAIssistFPDocEdit;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  // aissist
  LLM.Client, FrmAixplain, StrAIssist, LazLoggerBase, CTXMLFixFragment;

const
  SDescribeProcPrompt = 'Explain the following function in one sentence:';

type

  { TAIssistFPDocEditDlg }

  TAIssistFPDocEditDlg = class(TAIxplainForm)
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction); override;
    procedure FormCreate(Sender: TObject); override;
    procedure OKButtonClick(Sender: TObject);
  protected
    FSource: string;
    procedure CreatePrompt; override;
    procedure HandlePromptResult(Sender: TObject; aResult: TSendPromptResult);
  public
    Description: string;
    function Describe(aLLMClient: TLLMClient; const Src: string; out aDescription: string): boolean; virtual;
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


procedure TAIssistFPDocEditDlg.HandlePromptResult(Sender: TObject; aResult: TSendPromptResult);

var
  S : TStrings;

begin
  FBusy:=False;
  ActivateResponse;
  if (not aResult.Success) or (Length(AResult.Value)=0) then
    begin
    Description:='';
    mExplain.Lines.Add(SNoExplanation);
    end
  else
    begin
    Description:=aResult.Value[0].Text;

    FixFPDocFragment(Description,true,true,nil,[fffRemoveWrongCloseTags]);

    mExplain.Lines.Add(SAIExplanation);
    S:=TStringList.Create;
    try
      S.Text:=Description;
      mExplain.Lines.AddStrings(S);
    finally
      S.Free;
    end;
    end;
  DebugLn(['TAIssistFPDocEditDlg.HandleAIResponse Description="',Description,'"']);
end;

function TAIssistFPDocEditDlg.Describe(aLLMClient: TLLMClient; const Src: string; out
  aDescription: string): boolean;
begin
  Result:=false;
  FSource:=Src;
  aDescription:='';
  FLLMClient:=aLLMClient;
  FLLMClient.SynchronizeCallBacks:=True;
  CreatePrompt;
  SendPrompt;

  if (ShowModal=mrOk) and (Description>'') then
  begin
    aDescription:=Description;
    Result:=true;
  end;
end;

end.

