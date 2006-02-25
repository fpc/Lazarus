{
 /***************************************************************************
                          synchronize - example
                          ---------------------

                   Just a simple example to show & verify functionality
                   of the lazarus TThread.Synchronize / TProgressBar classes.

                   Initial Revision  : Sun Aug 15 1999

                   by Stefan Hille <stoppok@osibisa.ms.sub.org>
                   and Micha Nelissen

 ***************************************************************************/

 ***************************************************************************
 *                                                                         *
 *   This source is free software; you can redistribute it and/or modify   *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 *   This code is distributed in the hope that it will be useful, but      *
 *   WITHOUT ANY WARRANTY; without even the implied warranty of            *
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU     *
 *   General Public License for more details.                              *
 *                                                                         *
 *   A copy of the GNU General Public License is available on the World    *
 *   Wide Web at <http://www.gnu.org/copyleft/gpl.html>. You can also      *
 *   obtain it by writing to the Free Software Foundation,                 *
 *   Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.        *
 *                                                                         *
 ***************************************************************************
}
program Synchronize;

{$mode objfpc}{$H+}

{ threading directive not needed anymore for 1.9.8+ }
{ $threading on}

uses
{$ifdef UNIX}
  CThreads,
{$endif}
  Interfaces, Classes, StdCtrls, Forms, Buttons, Menus, ComCtrls,
  SysUtils, Extctrls;


type

  TAThread = class(TThread)
  protected
    FTargetListBox: TListBox;
    FTargetProgress: TProgressBar;
    FTestStrings: TStrings;
    procedure ExecDone;
    procedure ShowStrings;
  public
    constructor Create(CreateSuspended: boolean);
    destructor Destroy; override;
  end;

  TThread1 = class(TAThread)
  public
    constructor Create;
    procedure Execute; override;
  end;

  TThread2 = class(TAThread)
  public
    constructor Create;
    procedure Execute; override;
  end;

  TForm1 = class(TForm)
  public
    Progre1: TProgressBar;
    Progre2: TProgressBar;
    Progre3: TProgressBar;
    Listbox1: TListBox;
    Listbox2: TListBox;
    Thread1: TThread1;
    Thread2: TThread2;
    ThreadList: TList;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    Button6: TButton;
    Button7: TButton;
    Button8: TButton;
    Button9: TButton;
    Button10: TButton;
    mnuFile: TMainMenu;
    itmFileQuit: TMenuItem;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure LoadMainMenu;
    procedure mnuQuitClicked(Sender : TObject);
  protected
    procedure Button1CLick(Sender : TObject);
    procedure Button2CLick(Sender : TObject);
    procedure Button3CLick(Sender : TObject);
    procedure Button4CLick(Sender : TObject);
    procedure Button5CLick(Sender : TObject);
    procedure Button6CLick(Sender : TObject);
    procedure Button7CLick(Sender : TObject);
    procedure Button8CLick(Sender : TObject);
    procedure Button9CLick(Sender : TObject);
    procedure Button10CLick(Sender : TObject);
    function CloseQuery: boolean; override;
  end;

threadvar
  threadvartest: integer;

var
  Form1 : TForm1;
  TotalCount: integer;
  { GlobalData is an example of what you should NOT do :)
    Access from multiple threads to same variable unprotected }
  GlobalData: integer;

constructor TAThread.Create(CreateSuspended: boolean);
begin
  inherited;

  FTestStrings := TStringList.Create;
end;

destructor TAThread.Destroy;
begin
  inherited;

  FTestStrings.Free;
end;

procedure TAThread.ExecDone;
var
  lPos: integer;
begin
  Form1.ThreadList.Remove(Self);
  FTargetListBox.Items.Insert(0, 'Thread terminated');
  if Form1.ThreadList.Count = 0 then
  begin
    lPos := Pos('[', Form1.Caption);
    if lPos > 0 then
      Form1.Caption := Copy(Form1.Caption, 1, lPos - 1) + '[done, ready to exit]';
  end;
end;

procedure TAThread.ShowStrings;
var
  i: integer;
begin
  FTargetListBox.Items.BeginUpdate;
  for i := 0 to FTestStrings.Count - 1 do
  begin
    FTargetListBox.Items.Insert(0, FTestStrings.Strings[i]);
    while FTargetListBox.Items.Count > 30 do
      FTargetListBox.Items.Delete(FTargetListBox.Items.Count-1);
  end;
  FTargetListBox.Items.EndUpdate;
  FTestStrings.Clear;
  if FTargetProgress.Position = FTargetProgress.Max then
    FTargetProgress.Position := FTargetProgress.Min;
  FTargetProgress.StepIt;
end;

constructor TThread1.Create;
begin
  FTargetListBox := Form1.Listbox1;
  FTargetProgress := Form1.Progre1;
  FreeOnTerminate := true;
  inherited Create(false);
end;

function DoCalculation: integer;
var
  i, k: integer;
  j: array[0..511] of integer;
begin
  for i := 0 to 100000 do
  begin
    j[i mod $1ff] := i * i;
    k := j[(i + 3) and $1ff] div (i+1);
    j[(i + 5) and $1ff] := k - 3;
  end;
  result := j[5];
end;

procedure TThread1.Execute;
var
  i: integer;
begin
  threadvartest := 10;
  FTestStrings.Add('Threadvar is @'+IntToStr(ptrint(@threadvartest)));
  for i := 0 to TotalCount - 1 do
  begin
    GlobalData += 3;
    DoCalculation;
    FTestStrings.Add('Information: '+IntToStr(GlobalData-3)+' '+IntToStr(threadvartest));
    GlobalData -= 3;
    DoCalculation;
    Synchronize(@ShowStrings);
    threadvartest := 10;
    if Terminated then break;
  end;
  Synchronize(@ExecDone);
end;

constructor TThread2.Create;
begin
  FTargetListBox := Form1.Listbox2;
  FTargetProgress := Form1.Progre2;
  FreeOnTerminate := true;
  inherited Create(false);
end;

procedure TThread2.Execute;
var
  i: integer;
begin
  threadvartest := 15;
  FTestStrings.Add('Threadvar is @'+IntToStr(ptrint(@threadvartest)));
  for i := 0 to TotalCount - 1 do
  begin
    GlobalData -= 3;
    DoCalculation;
    FTestStrings.Add('Information: '+IntToStr(GlobalData+3)+' '+IntToStr(threadvartest));
    threadvartest := 15;
    GlobalData += 3;
    DoCalculation;
    if (i and $3) = $3 then
      Synchronize(@ShowStrings);
    if Terminated then break;
  end;
  Synchronize(@ExecDone);
end;


constructor TForm1.Create(AOwner: TComponent);
begin
   inherited Create(AOwner);
   Caption := 'Thread Synchronize Demo v0.1';
   ThreadList := TList.Create;
   LoadMainMenu;
end;

destructor TForm1.Destroy;
begin
  inherited;

  FreeAndNil(ThreadList);
end;

function TForm1.CloseQuery: boolean;
var
  I: integer;
begin
  if ThreadList.Count > 0 then
  begin
    Caption := Caption + ' [wait for threads termination]';
    for I := 0 to ThreadList.Count - 1 do
      TThread(ThreadList.Items[I]).Terminate;
    Result := false;
  end else
    inherited;
end;

procedure TForm1.Button1Click(Sender : TObject);
Begin
   if assigned (progre3) then begin
        progre3.Position := 0;
        progre3.Min := progre3.Min - 1
   end;
End;

procedure TForm1.Button2Click(Sender : TObject);
Begin
   if assigned (progre3) then begin
        progre3.Position := 0;
        progre3.Min := progre3.Min + 1;
   end;
End;

procedure TForm1.Button3Click(Sender : TObject);
Begin
   if assigned (progre3) then begin
        progre3.Position := 0;
        progre3.Max := progre3.Max +1;
   end;
End;

procedure TForm1.Button4Click(Sender : TObject);
Begin
   if assigned (progre3) then begin
        progre3.Position := 0;
        progre3.Max := progre3.Max -1;
   end;
End;

procedure TForm1.Button10Click(Sender : TObject);
Begin
  if assigned (progre3) then begin
    if progre3.position >= progre3.max then
      progre3.position := progre3.min;
    progre3.stepit;
  end;
End;

procedure TForm1.Button5Click(Sender : TObject);
Begin
   if assigned (progre1) then begin
        Progre1.Smooth := not Progre1.Smooth;
        if assigned (Button6)
          then Button6.Visible := Progre1.Smooth;
   end;
End;

procedure TForm1.Button6Click(Sender : TObject);
Begin
   if assigned (progre1) then begin
        Progre1.BarShowtext := not Progre1.BarShowtext;
   end;
End;

procedure TForm1.Button7Click(Sender : TObject);
Begin
   if assigned (progre1) then
   begin
     case Progre1.Orientation of
        pbVertical      : Progre1.Orientation := pbRightToLeft;
        pbRightToLeft   : Progre1.Orientation := pbTopDown;
        pbTopDown       : Progre1.Orientation := pbHorizontal;
        pbHorizontal    : Progre1.Orientation := pbVertical;
     end;
   end;
end;

procedure TForm1.Button8Click(Sender : TObject);
begin
        { Create the threads }
        TotalCount := 1000;
        GlobalData := 100;
        threadvartest := 20;
        Thread1 := TThread1.Create;
        Thread2 := TThread2.Create;
        ThreadList.Add(Thread1);
        ThreadList.Add(Thread2);

End;

procedure TForm1.Button9Click(Sender : TObject);
begin
  Listbox1.Items.Clear;
  Listbox2.Items.Clear;
end;

{------------------------------------------------------------------------------}
procedure TForm1.LoadMainMenu;

begin
        { set the height and width }
        Height := 350;
        Width := 700;

        { Create a progressbar }
        Progre1 := TProgressBar.Create (Self);
        with Progre1 do
        begin
          Parent := Self;
          SetBounds(300, 10, 250, 20);
          Min := 0;
          Max := 10;
          Step := 1;
          BarShowText := true;
          Smooth := True;
          Show;
        end;

        Progre2 := TProgressBar.Create (Self);
        with Progre2 do
        begin
          Parent := Self;
          SetBounds(300, 35, 250, 20);
          Min := 0;
          Max := 10;
          Step := 1;
          BarShowText := true;
          Smooth := True;
          Show;
        end;

        Progre3 := TProgressBar.Create (Self);
        with Progre3 do
        begin
          Parent := Self;
          SetBounds(300, 60, 250, 20);
          Min := 0;
          Max := 10;
          Step := 1;
          BarShowText := true;
          Smooth := True;
          Show;
        end;

        { create listboxes to show thread results }
        Listbox1 := TListBox.Create(self);
        with Listbox1 do
        begin
          Parent := self;
          SetBounds(10, 120, 200, 180);
        end;

        Listbox2 := TListBox.Create(self);
        with Listbox2 do
        begin
          Parent := self;
          SetBounds(250, 120, 200, 180);
        end;


        { Create a few buttons }
        Button2 := TButton.Create(Self);
        Button2.Parent := Self;
        Button2.Left := 200;
        Button2.Top := 70;
        Button2.Width := 80;
        Button2.Height := 30;
        Button2.Show;
        Button2.Caption := 'PMin ++';
//      Button2.ToolTip := 'Tool Tip';
//      Button2.ShowToolTip := True;
        Button2.OnClick := @Button2Click;


        Button1 := TButton.Create(Self);
        Button1.Parent := Self;
        Button1.Left := 50;
        Button1.Top := 70;
        Button1.Width := 80;
        Button1.Height := 30;
        Button1.Show;
        Button1.Caption := 'PMin--';
        Button1.OnClick := @Button1Click;

        { Create 2 more buttons outside the groupbox }
        Button3 := TButton.Create(Self);
        Button3.Parent := Self;
        Button3.Left := 50;
        Button3.Top := 30;
        Button3.Width := 80;
        Button3.Height := 30;
        Button3.Show;
        Button3.Caption := 'PMax++';
//      Button3.ToolTip := 'Tool Tip';
//      Button3.ShowToolTip := True;
        Button3.OnClick := @Button3Click;

        Button4 := TButton.Create(Self);
        Button4.Parent := Self;
        Button4.Left := 200;
        Button4.Top := 30;
        Button4.Width := 80;
        Button4.Height := 30;
        Button4.Show;
        Button4.Caption := 'PMax--';
        Button4.OnClick := @Button4Click;

        Button10 := TButton.Create(Self);
        with Button10 do
        begin
          Parent := Self;
          SetBounds(140, 30, 50, 30);
          Show;
          Caption := 'Step It';
          OnClick := @Button10Click;
        end;

        Button5 := TButton.Create(Self);
        Button5.Parent := Self;
        Button5.Left := 500;
        Button5.Top := 110;
        Button5.Width := 130;
        Button5.Height := 30;
        Button5.Show;
        Button5.Caption := 'Toggle Smooth';
        Button5.OnClick := @Button5Click;

        Button6 := TButton.Create(Self);
        Button6.Parent := Self;
        Button6.Left := 500;
        Button6.Top := 150;
        Button6.Width := 130;
        Button6.Height := 30;
        Button6.Show;
        Button6.Caption := 'Toggle Text';
        Button6.OnClick := @Button6Click;
        Button6.Visible := Progre1.Smooth;

        Button7 := TButton.Create(Self);
        Button7.Parent := Self;
        Button7.Left := 500;
        Button7.Top := 190;
        Button7.Width := 130;
        Button7.Height := 30;
        Button7.Show;
        Button7.Caption := 'Orientation';
        Button7.OnClick := @Button7Click;

        Button8 := TButton.Create(Self);
        with Button8 do
        begin
          Parent := Self;
          SetBounds(500, 230, 130, 30);
          Show;
          Caption := 'Thread test';
          OnClick := @Button8Click;
        end;

        Button9 := TButton.Create(Self);
        with Button9 do
        begin
          Parent := Self;
          SetBounds(500, 270, 130, 30);
          Show;
          Caption := 'Clear listboxes';
          OnClick := @Button9Click;
        end;

        { create a menubar }
        mnuFile := TMainMenu.Create(Self);
        mnuFile.Name:='mnuFile';
        Menu := mnuFile;

        itmFileQuit := TMenuItem.Create(Self);
        itmFileQuit.Caption := 'Quit';
        itmFileQuit.OnClick := @mnuQuitClicked;
        mnuFile.Items.Add(itmFileQuit);

end;

{------------------------------------------------------------------------------}
procedure TForm1.mnuQuitClicked(Sender : TObject);
begin
  Close;
end;
{------------------------------------------------------------------------------}

begin
   Application.Initialize; { calls InitProcedure which starts up GTK }
   Application.CreateForm(TForm1, Form1);
   Application.Run;
end.

