type
  TMyClass = class(TObject)
  public
    Created: string;
  end;

procedure TForm1.Button1Click(Sender: TObject);
var
  Node: TTreeNode;
begin
  if Assigned(TreeView1.Selected) then
    Node := TreeView1.Selected
  else
    Node := TreeView1.Items.GetFirstNode;

  with TreeView1.Items.AddChild(Node, Format('Node: %d', [TreeView1.Items.Count])) do
  begin
    Data := TMyClass.Create;
    TMyClass(Data).Created := FormatDateTime('hh:nn:ss',Now);
  end;
end;

procedure TForm1.TreeView1SelectionChanged(Sender: TObject);
begin
  Label1.Caption := 'Node creation time: ' + TMyClass(TreeView1.Selected.Data).Created;
end;