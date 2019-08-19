unit frmpas2jsnodejsprojectoptions;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ButtonPanel,
  strpas2jsdesign;

type

  { TNodeJSProjectOptionsForm }

  TNodeJSProjectOptionsForm = class(TForm)
    BPNode: TButtonPanel;
    CBUseNodeJSApplication: TCheckBox;
    procedure FormCreate(Sender: TObject);
  private
    function GetB(AIndex: Integer): Boolean;
    procedure SetB(AIndex: Integer; AValue: Boolean);

  public
    Property UseNodeJSApplication : Boolean Index 0 Read GetB Write SetB;
  end;

var
  NodeJSProjectOptionsForm: TNodeJSProjectOptionsForm;

implementation

{$R *.lfm}

{ TNodeJSProjectOptionsForm }

procedure TNodeJSProjectOptionsForm.FormCreate(Sender: TObject);
begin
  Caption:=pjsdNodeJSProjectOptions;
  CBUseNodeJSApplication.Caption:=pjsdUseNodeJSApplicationObject;
end;

function TNodeJSProjectOptionsForm.GetB(AIndex: Integer): Boolean;
begin
  Case Aindex of
    0 : Result:=CBUseNodeJSApplication.Checked;
  else
    Result:=False;
  end;
end;

procedure TNodeJSProjectOptionsForm.SetB(AIndex: Integer; AValue: Boolean);
begin
  Case Aindex of
    0 : CBUseNodeJSApplication.Checked:=AValue;
  end;
end;

end.

