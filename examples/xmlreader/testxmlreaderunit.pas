unit TestXMLReaderUnit; 

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  EditBtn, StdCtrls, Buttons, ComCtrls, ExtCtrls, DOM, XMLRead;

type

  { TXMLRederForm }

  TXMLRederForm = class(TForm)
    BitBtn1: TBitBtn;
    Edit1: TEdit;
    Edit2: TEdit;
    FileNameEdit1: TFileNameEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    ListView1: TListView;
    Panel1: TPanel;
    Panel2: TPanel;
    Splitter1: TSplitter;
    TreeView1: TTreeView;
    procedure BitBtn1Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure TreeView1Click(Sender: TObject);
  private
    FDoc: TXMLDocument;
    procedure ParseDoc;
  public
    { public declarations }
  end; 

var
  XMLRederForm: TXMLRederForm;

implementation


{ TXMLRederForm }

procedure TXMLRederForm.BitBtn1Click(Sender: TObject);
begin
  if Assigned(FDoc) then
    FreeAndNil(FDoc);

  if FileExistsUTF8(FileNameEdit1.FileName) then
    ReadXMLFile(FDoc, UTF8ToSys(FileNameEdit1.FileName));
  if Assigned(FDoc) then
    ParseDoc;
end;

procedure TXMLRederForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  if Assigned(FDoc) then
    FDoc.Free;
end;

procedure TXMLRederForm.FormCreate(Sender: TObject);
begin
  if ParamCount > 0 then
  begin
    FileNameEdit1.FileName:=ParamStrUTF8(1);
    BitBtn1Click(nil);
  end;

  TreeView1Click(nil);
end;

procedure TXMLRederForm.TreeView1Click(Sender: TObject);
var
  Node:TDOMNode;
  Item:TListItem;
  i:Integer;
begin
  Edit1.Text:='';
  Edit2.Text:='';

  if not Assigned(TreeView1.Selected) then exit;
  Node:=TDOMNode(TreeView1.Selected.Data);
  if not Assigned(Node) then exit;

  Edit1.Text:=Node.NodeName;
  Edit2.Text:=Node.NodeValue;

  ListView1.BeginUpdate;
  ListView1.Items.Clear;

  if Assigned(Node.Attributes) then
    for i:=0 to Node.Attributes.Length-1 do
    begin
      Item:=ListView1.Items.Add;
      Item.Caption:=Node.Attributes[i].NodeName;
      Item.SubItems.Add(Node.Attributes[i].NodeValue);
    end;
  ListView1.EndUpdate;
end;

procedure TXMLRederForm.ParseDoc;

procedure DoFill(AOwner:TTreeNode; Node:TDOMNode);
var
  i: integer;
  AItem:TTreeNode;
begin
  if not Assigned(Node) then exit;
  for i:=0 to Node.ChildNodes.Count - 1 do
  begin
    AItem:=TreeView1.Items.AddChild(AOwner, Node.ChildNodes[i].NodeName);
    AItem.Data:=Node.ChildNodes[i];
    if not Assigned(TreeView1.Selected) then
      TreeView1.Selected:=AItem;
    DoFill(AItem, Node.ChildNodes[i]);
  end;
end;

begin
  TreeView1.Selected:=nil;
  TreeView1.Items.BeginUpdate;
  TreeView1.Items.Clear;
  DoFill(nil, FDoc);
  TreeView1.Items.EndUpdate;
  TreeView1Click(nil);
end;

initialization
  {$I testxmlreaderunit.lrs}

end.

