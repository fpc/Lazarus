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
  protected
    FSource: string;
    procedure CreatePrompt; override;
  public
    function Describe(aAIClient: TAIClient; const Src: string; out aDescription: string): boolean; virtual;
  end;

var
  AIssistFPDocEditDlg: TAIssistFPDocEditDlg;

implementation

{$R *.lfm}

{ TAIssistFPDocEditDlg }

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

  if ShowModal=mrOk then
  begin
    aDescription:='';
    Result:=true;
  end;
end;

end.

