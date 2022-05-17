unit Unit1;

{$MODE Delphi}

interface

uses
  LCLIntf, LCLType, LMessages, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls;

type
  TOnHintEvent = procedure(Sender: TObject; HintInfo: PHintInfo) of object;
  TMyHintControl = class (TCustomControl)
  private
    FBlueRect, FRedRect, FWhiteRect, FYellowRect: TRect;
    FOnHintEvent: TOnHintEvent;
    FShowOnlyRed: Boolean;
    procedure CMHintShow(var Message: TMessage); message CM_HINTSHOW;
    procedure SetOnHintEvent(const Value: TOnHintEvent);
    procedure SetShowOnlyRed(const Value: Boolean);
  protected
    procedure Resize; override;
  public
    constructor Create(TheOwner: TComponent); override;
    procedure Paint; override;
    property ShowOnlyRed: Boolean read FShowOnlyRed write SetShowOnlyRed;
    property OnHintEvent: TOnHintEvent read FOnHintEvent write SetOnHintEvent;
  end;

  TMyHintButton = class (TButton)
  private
    procedure CMHintShow(var Message: TMessage); message CM_HINTSHOW;
  protected
  public
  end;

  { TForm1 }

  TForm1 = class(TForm)
    Button7: TButton;
    Button8: TButton;
    CheckBox1: TCheckBox;
    ListBox1: TListBox;
    ButtonClear: TButton;
    GroupBoxNoShowHint: TGroupBox;
    ButtonShowHintNoParent1: TButton;
    Button1: TButton;
    Button2: TButton;
    ButtonNoShowHintShowParent: TButton;
    GroupBoxShowHint: TGroupBox;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    Button6: TButton;
    procedure CheckBox1Change(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ButtonClearClick(Sender: TObject);
  private
    { Private declarations }
    FMyHintControl,FMyHintControl2 : TMyHintControl;
    FMyHintButton: TMyHintButton;
    procedure HintEvent(Sender: TObject; HintInfo: PHintInfo);
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TMyHintControl }

procedure TMyHintControl.CMHintShow(var Message: TMessage);
begin
  with TCMHintShow(Message), HintInfo^ do
  begin
    if Assigned(FOnHintEvent) then
      FOnHintEvent(Self, HintInfo);
    Result := 1;
    if PtInRect(FRedRect, CursorPos) then
    begin
      Result := 0;
      HintStr := 'Red' + #13#10 + 'aaaaa_bbbbb_ccccc_dddddd_eeeeee';
      CursorRect := FRedRect;
    end;
    if FShowOnlyRed then
      Exit;
    if PtInRect(FBlueRect, CursorPos) then
    begin
      Result := 0;
      HintStr := 'Blue';
      CursorRect := FBlueRect;
    end;
    if PtInRect(FYellowRect, CursorPos) then
    begin
      Result := 0;
      HintStr := 'Yellow';
      CursorRect := FYellowRect;
    end;
    if PtInRect(FWhiteRect, CursorPos) then
    begin
      Result := 0;
      HintStr := 'White';
      CursorRect := FWhiteRect;
    end;
  end;
end;

constructor TMyHintControl.Create(TheOwner: TComponent);
begin
  inherited;
  Hint := 'Control Hint';
  ShowHint := True;
end;

procedure TMyHintControl.Paint;
begin
  with Canvas do
  begin
    Brush.Color := clRed;
    FillRect(FRedRect);

    Brush.Color := clWhite;
    FillRect(FWhiteRect);

    Brush.Color := clBlue;
    FillRect(FBlueRect);

    Brush.Color := clYellow;
    FillRect(FYellowRect);
  end;
end;

procedure TMyHintButton.CMHintShow(var Message: TMessage);
begin
  TCMHintShow(Message).HintInfo^.HintStr := 'CMHintShow';
end;


procedure TForm1.FormCreate(Sender: TObject);
begin
  FMyHintControl := TMyHintControl.Create(Self);
  with FMyHintControl do
  begin
    ShowOnlyRed := True;
    OnHintEvent := HintEvent;
    Parent := Self;
    SetBounds(10, 10, 100, 100);
    Visible := True;
  end;
  FMyHintControl2 := TMyHintControl.Create(Self);
  with FMyHintControl2 do
  begin
    OnHintEvent := HintEvent;
    Parent := Self;
    SetBounds(120, 10, 100, 100);
    Visible := True;
  end;
  FMyHintButton := TMyHintButton.Create(Self);
  FMyHintButton.Parent := GroupBoxShowHint;
  FMyHintButton.Left :=  Button7.Left;
  FMyHintButton.Width :=  Button7.Width;
  FMyHintButton.Top :=  Button7.Top + 36;
  FMyHintButton.ParentShowHint := True;
  FMyHintButton.ShowHint := True;
  FMyHintButton.Caption := 'ShowHint = True Parent = True / Hint = '''' / CMHintShow';
end;

procedure TForm1.CheckBox1Change(Sender: TObject);
begin
  if CheckBox1.Checked then
  begin
    GroupBoxNoShowHint.Hint := '';
    GroupBoxShowHint.Hint := '';
  end
  else
  begin
    GroupBoxNoShowHint.Hint := 'GroupBox';
    GroupBoxShowHint.Hint := 'GroupBox';
  end;
end;

procedure TMyHintControl.Resize;
begin
  inherited;
  FRedRect := Rect(0, 0, Width div 2, Height div 2);
  FWhiteRect := Rect(Width div 2, 0, Width, Height div 2);
  FBlueRect := Rect(0, Height div 2, Width div 2, Height);
  FYellowRect := Rect(Width div 2, Height div 2, Width, Height);
end;

procedure TMyHintControl.SetOnHintEvent(const Value: TOnHintEvent);
begin
  FOnHintEvent := Value;
end;

procedure TForm1.HintEvent(Sender: TObject; HintInfo: PHintInfo);
begin
  with HintInfo^ do
  begin
    ListBox1.Items.Add(Format('CursorPoint X: %d Y: %d', [CursorPos.X, CursorPos.Y]));
    ListBox1.Items.Add(Format('CursorRect L: %d T: %d R: %d B: %d',
      [CursorRect.Left, CursorRect.Top, CursorRect.Right, CursorRect.Bottom]));
  end;
end;

procedure TForm1.ButtonClearClick(Sender: TObject);
begin
  ListBox1.Clear;
end;

procedure TMyHintControl.SetShowOnlyRed(const Value: Boolean);
begin
  FShowOnlyRed := Value;
end;

end.
 
