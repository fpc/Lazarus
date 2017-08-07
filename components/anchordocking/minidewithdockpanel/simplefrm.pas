unit SimpleFrm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LCLProc, FileUtil, Forms, Controls, Graphics, Dialogs,
  StdCtrls;

type

  { TSimpleForm }

  TSimpleForm = class(TForm)
    Memo1: TMemo;
    procedure FormPaint(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  SimpleForm: TSimpleForm;

function CreateSimpleForm(Name, Title: string; NewBounds: TRect;
  DisableAutoSizing: boolean): TSimpleForm;

implementation

function CreateSimpleForm(Name, Title: string; NewBounds: TRect;
  DisableAutoSizing: boolean): TSimpleForm;
begin
  // first check if the form already exists
  // the LCL Screen has a list of all existing forms.
  // Note: Remember that the LCL allows as form names only standard
  // pascal identifiers and compares them case insensitive
  Result:=TSimpleForm(Screen.FindForm(Name));
  if Result is TSimpleForm then begin
    if DisableAutoSizing then
      Result.DisableAutoSizing;
    exit;
  end;

  // create it
  Result:=TSimpleForm(TSimpleForm.NewInstance);
  Result.DisableAutoSizing;
  Result.Create(Application);
  Result.Caption:=Title;
  Result.Name:=Name;
  Result.Memo1.Lines.Text:=Name;
  Result.BoundsRect:=NewBounds;
  if not DisableAutoSizing then
    Result.EnableAutoSizing;
end;

{$R *.lfm}

{ TSimpleForm }

procedure TSimpleForm.FormPaint(Sender: TObject);
begin
  with Canvas do begin
    Pen.Color:=clRed;
    MoveTo(0,0);
    LineTo(ClientWidth-1,0);
    MoveTo(ClientWidth-1,ClientHeight-1);
    MoveTo(0,ClientHeight-1);
    MoveTo(0,0);
    MoveTo(ClientWidth-1,ClientHeight-1);
  end;
end;

end.

