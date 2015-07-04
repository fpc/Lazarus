unit main;

{ fpc/Lazarus demo of TStringGrid and the associated cell/button types.

  Copyright (C) 2013 Windsurfer contact via fpc/Lazarus forum

  This library is free software; you can redistribute it and/or modify it
  under the terms of the GNU Library General Public License as published by
  the Free Software Foundation; either version 2 of the License, or (at your
  option) any later version with the following modification:

  As a special exception, the copyright holders of this library give you
  permission to link this library with independent modules to produce an
  executable, regardless of the license terms of these independent modules,and
  to copy and distribute the resulting executable under terms of your choice,
  provided that you also meet, for each linked independent module, the terms
  and conditions of the license of that module. An independent module is a
  module which is not derived from or based on this library. If you modify
  this library, you may extend this exception to your version of the library,
  but you are not obligated to do so. If you do not wish to do so, delete this
  exception statement from your version.

  This program is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public License
  for more details.

  You should have received a copy of the GNU Library General Public License
  along with this library; if not, write to the Free Software Foundation,
  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
}
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Graphics, Dialogs, Grids, StdCtrls, ExtDlgs;

type

  { TForm1 }

  TForm1 = class(TForm)
    CalculatorDialog1: TCalculatorDialog;
    ColorDialog1: TColorDialog;
    Edit1: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    StringGrid1: TStringGrid;
    procedure Edit1Change(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure StringGrid1ButtonClick(Sender: TObject; aCol, aRow: integer);
    procedure StringGrid1CheckboxToggled(Sender: TObject; aCol, aRow: integer;
      aState: TCheckboxState);
    procedure StringGrid1DrawCell(Sender: TObject; aCol, aRow: integer;
      aRect: TRect; aState: TGridDrawState);
    procedure StringGrid1GetCellHint(Sender: TObject; ACol, ARow: integer;
      var HintText: string);
    procedure StringGrid1GetCheckboxState(Sender: TObject; ACol, ARow: integer;
      var Value: TCheckboxState);
    procedure StringGrid1GetEditMask(Sender: TObject; ACol, ARow: integer;
      var Value: string);
    procedure StringGrid1SetEditText(Sender: TObject; ACol, ARow: integer;
      const Value: string);
    procedure StringGrid1ValidateEntry(Sender: TObject; aCol, aRow: integer;
      const OldValue: string; var NewValue: string);
  public
  end;

var
  Form1: TForm1;

type
  TMyInput = record
    Auto: string;
    EditMask: string;
    Button: TColor;
    ButtonColumn: string;
    CheckBox: TCheckBoxState;
    Ellipsis: string;
    None: string;
    PickList: string;
  end;

implementation

{$R main.lfm}

{ TForm1 }

//Additional Note:
//The cbsButton can call the DrawCell event and change the colour immediately
//the ColorDialog closes. The cbsEllipsis can only call the DrawCell event when
//focus moves to another cell.
//In Grids.pas it can be seen that cbsEllipsis calls TButtonCellEditor, but
//cbsButton calls both TButtonCellEditor and TStringEditor.
//Changing the ButtonStyle of Column 'Button' from cbsButton to cbsEllipsis will
//demonstrate this.

var
  ayMyInput: array of TMyInput;  //All status information is written to and read
//from here. This is not strictly necesary, but allows a real program to destroy
//the form and keep  the information.

procedure TForm1.FormCreate(Sender: TObject);
var
  I: integer;
begin
  SetLength(ayMyInput, StringGrid1.RowCount - 1); //grid and array count from 0
  // Ensure button column is correct colour. Otherwise, DrawCell event will paint it black.
  for I := 0 to length(ayMyInput) - 1 do
    ayMyInput[I].Button := clWindow;  //TColor
  for I := 0 to length(ayMyInput) - 1 do
    ayMyInput[I].CheckBox := cbUnChecked; //TCheckBoxState

  for I := 0 to length(ayMyInput) - 1 do
  begin
    ayMyInput[I].None := 'Not editable';  //'None' can only be changed in program
    StringGrid1.Cells[6, I + 1] := ayMyInput[I].None;
  end;

  Edit1.Text := ayMyInput[0].None;
  StringGrid1.Options := StringGrid1.Options + [goCellHints];
  StringGrid1.ShowHint := True;
  StringGrid1.Columns.Items[7].PickList.Add('Giraffe'); //Add an item progamatically
  //The others are added in the Object Inspector
  Application.HintPause := 1;
end;

procedure TForm1.StringGrid1GetEditMask(Sender: TObject; ACol, ARow: integer;
  var Value: string);
begin
  //'!' = delete leading blanks. '0' = position must be a number.
  //'1' = keep formatting symbols. '_' =  trailing '0'.
  //Does not limit fields to 23:59:59.
  //Use ValidateEntry and Copy()to check and change each character as the cell is exited.
  if (ARow > 0) and (ACol = 1) then
    Value := '!00:00:00;1;_';
end;

procedure TForm1.StringGrid1SetEditText(Sender: TObject; ACol, ARow: integer;
  const Value: string);
begin      //Capture text from columns 0 and 1 to ayMyInput.Auto and .EditMask
  //SetEditText works for each keystroke
  if (ARow > 0) then
    if (ACol = 0) then
      ayMyInput[aRow - 1].Auto := StringGrid1.Cells[ACol, ARow]
    else if (ACol = 1) then
      ayMyInput[aRow - 1].EditMask := StringGrid1.Cells[ACol, ARow];

  Label4.Caption := Value;   //Show text as it is typed
end;

procedure TForm1.StringGrid1ValidateEntry(Sender: TObject; aCol, aRow: integer;
  const OldValue: string; var NewValue: string);
begin
  //Constrain to '23:59:59'.
  //This only takes effect on leaving cell.
  if (aRow > 0) and (aCol = 1) then
  begin
    if Copy(NewValue, 1, 1) > '2' then
      NewValue[1] := '2';
    if Copy(NewValue, 2, 1) > '3' then
      NewValue[2] := '3';
    if Copy(NewValue, 4, 1) > '5' then
      NewValue[4] := '5';
    if Copy(NewValue, 7, 1) > '5' then
      NewValue[7] := '5';
  end;
end;

procedure TForm1.StringGrid1ButtonClick(Sender: TObject; aCol, aRow: integer);
begin
  //For these columns there is no manual entry into the cell,
  //so use ButtonClick event

  if (aCol = 2) and ColorDialog1.Execute then //Button
  begin
    ayMyInput[aRow - 1].Button := Colordialog1.Color; //store cell colour in array
    StringGrid1.Invalidate; //Could also use 'Repaint' te force DrawCell event
  end;

  if (aCol = 3) then  //ButtonColumn
  begin
    StringGrid1.Options := StringGrid1.Options - [goEditing];
    //Prevent write to previous cell
    ayMyInput[aRow - 1].ButtonColumn := IntToStr(aCol) + ',' + IntToStr(aRow);
    //store as string
    StringGrid1.Cells[aCol, aRow] := ayMyInput[aRow - 1].ButtonColumn;
    StringGrid1.Options := StringGrid1.Options + [goEditing]; //Turn cell editing back on
  end;

  if (aCol = 5) and CalculatorDialog1.Execute then //Ellipsis
  begin
    // Click 'tick' sign on calculator to get result
    ayMyInput[aRow - 1].Ellipsis := FloattoStr(Calculatordialog1.Value);
    //Store as string
    StringGrid1.Cells[aCol, aRow] := ayMyInput[aRow - 1].Ellipsis;
  end;
end;

procedure TForm1.StringGrid1CheckboxToggled(Sender: TObject;
  aCol, aRow: integer; aState: TCheckboxState);
begin
  if (ARow > 0) and (ACol = 4) then
    ayMyInput[ARow - 1].CheckBox := aState;
end;

procedure TForm1.StringGrid1GetCheckboxState(Sender: TObject;
  ACol, ARow: integer; var Value: TCheckboxState);
begin
  if (ARow > 0) and (ACol = 4) then
    Value := ayMyInput[ARow - 1].CheckBox;
end;

procedure TForm1.Edit1Change(Sender: TObject);
var
  I: integer;
begin
  for I := 1 to StringGrid1.RowCount - 1 do  //'None' can only be changed in program
  begin
    ayMyInput[I - 1].None := Edit1.Text;
    StringGrid1.Cells[6, I] := ayMyInput[I - 1].None;
  end;
end;

procedure TForm1.StringGrid1DrawCell(Sender: TObject; aCol, aRow: integer;
  aRect: TRect; aState: TGridDrawState);
begin
  //Note in Col 2 'Button' the Repaint event takes place before the focus changes
  //to another cell.

  if (aRow > 0) then         //Use DrawCell to paint rectangle
    if (ACol = 2) then
    begin                    //Get colour from array
      stringgrid1.canvas.Brush.Color := ayMyInput[aRow - 1].Button;
      stringgrid1.canvas.FillRect(aRect);   //Paint Cell
    end;
end;

procedure TForm1.StringGrid1GetCellHint(Sender: TObject; ACol, ARow: integer;
  var HintText: string);
begin
  case ACol of
    0: HintText := 'Button style cbsAuto sting grid column' +
        sLineBreak + ' - enter any text.';
    1: HintText := 'Button style cbsAuto, with basic Editmask for Time format.' +
        sLineBreak + 'Uses ValidateEntry as cell is exited to enforce  max of ''23:59:59''';
    2: HintText := 'Button style cbsButton that shows colour dialog' +
        sLineBreak + ' and changes cell colour.';
    3: HintText := 'Button style cbsButtonColumn that shows cell position.';
    4: HintText := 'Button style cbsCheckbox that toggles ''check'' mark.';
    5: HintText := 'Button style cbsEllipsis that opens calculator.' +
        sLineBreak + 'Click ''tick'' on calculator to send value to cell.';
    6: HintText := 'Button style cbsNone that cannot be changed manually.' +
        sLineBreak + 'Change Edit box contents to change displayed text.';
    7: HintText := 'Button style cbsPicklist that offers a choice from' +
        sLineBreak + 'a list set in the Object Inspector.';
  end;
end;

end.
