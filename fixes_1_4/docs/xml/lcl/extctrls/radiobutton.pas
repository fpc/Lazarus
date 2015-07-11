{Using TRadioGroup with three entries in Items:
Index 0: Red
Index 1: Amber
Index 2: Green}

procedure TTrivialForm1.RadioGroup1Click(Sender: TObject);
begin
  case RadioGroup1.ItemIndex of
  0:   Shape1.brush.color := clRed;
  1:   Shape1.brush.color := clYellow;
  2:   Shape1.brush.color := clGreen
  end
end;



{Using three separate RadioButtons}

procedure TForm1.RadioButton1Click(Sender: TObject);
begin
  Shape1.brush.color := clRed
end;

procedure TForm1.RadioButton2Click(Sender: TObject);
begin
  Shape1.brush.color := clYellow
end;

procedure TForm1.RadioButton3Click(Sender: TObject);
begin
  Shape1.brush.color := clGreen
end;