unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, LazLogger, Forms, Controls, Graphics, Dialogs, StdCtrls, ComCtrls;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    HideTimeoutLabel: TLabel;
    HideTimeoutTrackBar: TTrackBar;
    ReshowTimeoutLabel: TLabel;
    ReshowTimeoutTrackBar: TTrackBar;
    procedure FormCreate(Sender: TObject);
    procedure Button1ShowHint(Sender: TObject; HintInfo: PHintInfo);
  private
  public
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
  Button1.OnShowHint:=@Button1ShowHint;
end;

procedure TForm1.Button1ShowHint(Sender: TObject; HintInfo: PHintInfo);
var
  Hour: word;
  Minute: word;
  Second: word;
  MilliSecond: word;
begin
  DebugLn(['TForm1.Button1ShowHint',
    ' HintControl=',DbgSName(HintInfo^.HintControl),
    ' HintWindowClass=',DbgSName(HintInfo^.HintWindowClass),
    ' HintPos=',dbgs(HintInfo^.HintPos),
    ' HintMaxWidth=',HintInfo^.HintMaxWidth,
    ' HintColor=',dbgs(HintInfo^.HintColor),
    ' CursorRect=',dbgs(HintInfo^.CursorRect),
    ' CursorPos=',dbgs(HintInfo^.CursorPos),
    ' ReshowTimeout=',HintInfo^.ReshowTimeout,
    ' HideTimeout=',HintInfo^.HideTimeout,
    ' HintStr=',dbgstr(HintInfo^.HintStr),
    ' HintData=',dbgs(HintInfo^.HintData)
    ]);
  DecodeTime(Now, Hour, Minute, Second, MilliSecond);
  HintInfo^.HintStr:=Format('Time: %2D:%2D:%2D.%4D',[Hour, Minute, Second, MilliSecond]);
  HintInfo^.HideTimeout:=HideTimeoutTrackBar.Position;
  HintInfo^.ReshowTimeout:=ReshowTimeoutTrackBar.Position;
end;

end.

