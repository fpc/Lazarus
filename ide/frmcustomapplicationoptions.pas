unit frmcustomapplicationoptions;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls;

type

  { TCustomApplicationOptionsForm }

  TCustomApplicationOptionsForm = class(TForm)
    BCancel: TButton;
    BOK: TButton;
    CGOptions: TCheckGroup;
    EClassName: TEdit;
    ETitle: TEdit;
    LETitle: TLabel;
    LEClassName: TLabel;
    procedure EClassNameKeyPress(Sender: TObject; var Key: char);
    procedure FormCreate(Sender: TObject);
    function GetAppName: String;
    function GetBool(Index: integer): Boolean;
    function GetTitle: String;
  private
    { private declarations }
  public
    { public declarations }
    Property Title : String Read GetTitle;
    Property AppClassName : String Read GetAppName;
    Property CodeUsage : Boolean Index 0 Read GetBool;
    Property CodeStopOnError : Boolean Index 1 Read GetBool;
    Property CodeConstructor : Boolean Index 2 Read GetBool;
    Property CodeDestructor : Boolean Index 3 Read GetBool;
    Property CodeCheckOptions : Boolean Index 4 Read GetBool;
  end;

var
  CustomApplicationOptionsForm: TCustomApplicationOptionsForm;

implementation

{ TCustomApplicationOptionsForm }

function TCustomApplicationOptionsForm.GetAppName: String;
begin
  Result:=EClassName.Text;
end;

procedure TCustomApplicationOptionsForm.EClassNameKeyPress(Sender: TObject;
  var Key: char);

Const
  Alpha = ['a'..'z','A'..'Z'];
  Num   = ['0'..'9'];
  Oth   = ['_',#8,#9,#27]; // allow Backspace, tab, escape
  AllowedKeys = Alpha+Num+Oth;
  
begin
  If Not (Key in AllowedKeys) then
    Key:=#0;
end;

procedure TCustomApplicationOptionsForm.FormCreate(Sender: TObject);
var
  i: Integer;
begin
  // set all defaults to true
  for i:=0 to CGOptions.Items.Count-1 do
    CGOptions.Checked[i]:=true;
end;

function TCustomApplicationOptionsForm.GetBool(Index: integer): Boolean;
begin
  Result:= CGOptions.Checked[Index];
end;

function TCustomApplicationOptionsForm.GetTitle: String;
begin
  Result:=ETitle.Text;
end;

initialization
  {$I frmcustomapplicationoptions.lrs}

end.

