unit UnitSummary;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, FPDocFiles, StdCtrls, ExtCtrls,
  LCLIntf;

type

  { TFormSummary }

  TFormSummary = class(TForm)
    Button1: TButton;
    Button2: TButton;
    LabelFileName: TLabel;
    PaintBox: TPaintBox;
    procedure PaintBoxPaint(Sender: TObject);
  private
    { private declarations }
  public
    OldInfo, NewInfo: TFPDocInfo;
  end; 

var
  FormSummary: TFormSummary;

implementation

{ TFormSummary }

procedure TFormSummary.PaintBoxPaint(Sender: TObject);
var
  Y: Integer;
  
  procedure AddItem(S: String; R: Integer);
  begin
    if R = 0 then Exit;
    if R > 0 then
    begin
      SetTextColor(PaintBox.Canvas.Handle, clGreen);
      PaintBox.Canvas.TextOut(4, Y, Format('Added ' + S + ': %d', [R]));
    end
    else
    begin
      SetTextColor(PaintBox.Canvas.Handle, clRed);
      PaintBox.Canvas.TextOut(4, Y, Format('Removed ' + S + ': %d', [-R]));
    end;
    
    Inc(Y, PaintBox.Canvas.TextHeight('W') + 4);
  end;
  
begin
  Y := 4;

  AddItem('packages', NewInfo.Packages - OldInfo.Packages);
  AddItem('modules', NewInfo.Modules - OldInfo.Modules);
  AddItem('topics', NewInfo.Topics - OldInfo.Topics);
  AddItem('elements', NewInfo.Elements - OldInfo.Elements);
  AddItem('non empty elements', NewInfo.ElementsNonEmpty - OldInfo.ElementsNonEmpty);
  AddItem('shorts', NewInfo.Shorts - OldInfo.Shorts);
  AddItem('descriptions', NewInfo.Descriptions - OldInfo.Descriptions);
  AddItem('errors', NewInfo.Errors - OldInfo.Errors);
  AddItem('see alsos', NewInfo.SeeAlsos - OldInfo.SeeAlsos);
  AddItem('examples', NewInfo.Examples - OldInfo.Examples);
  
  if Y = 4 then
  begin
    SetTextColor(PaintBox.Canvas.Handle, clBlack);
    PaintBox.Canvas.TextOut(4, Y, 'No change was made!');
  end;
end;

initialization
  {$I unitsummary.lrs}

end.

