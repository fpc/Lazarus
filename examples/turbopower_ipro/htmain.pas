unit htmain;

{
  IMPORTANT:
  This DEFINE must be added to "Additions and Overrides" of the project
  options for this program to work correctly:
    -dHTML_RTTI
}

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, IpHtml, Forms, Controls, Graphics, Dialogs,
  ExtCtrls, ComCtrls, StdCtrls;

type

  { TMainForm }

  TMainForm = class(TForm)
    IpHtmlPanel1: TIpHtmlPanel;
    Label1: TLabel;
    ListBox1: TListBox;
    OpenDialog1: TOpenDialog;
    BtnOpenHTMLFile: TButton;
    Panel1: TPanel;
    Panel2: TPanel;
    Splitter1: TSplitter;
    Splitter2: TSplitter;
    TreeView1: TTreeView;
    procedure BtnOpenHTMLFileClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure IpHTMLPanel1CurElementChange(Sender: TObject);
    procedure IpHtmlPanel1HotChange(Sender: TObject);
    procedure IpHtmlPanel1HotClick(Sender: TObject);
    procedure ListBox1DblClick(Sender: TObject);
    procedure TreeView1Change(Sender: TObject; Node: TTreeNode);

  private
//    FCurCtrl: TControl;
    procedure AddNode(HTMLNode: TIpHTMLNode; AParent: TTreeNode);
    procedure HTMLGetImageX(Sender: TIpHtmlNode; const URL: string; var Picture: TPicture);
    procedure HTMLNodeChange(HTMLNode: TIpHtmlNode);
    procedure PopulateOutline(H: TIpHTML);
    procedure UpdateTreeView;

  public
    procedure OpenHTMLFile(const Filename: string);

  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

uses
  LazUTF8, LazFileUtils;

type
  TSimpleIpHtml = class(TIpHtml)
  public
    property OnGetImageX;
  end;


{ TMainForm }

procedure TMainForm.AddNode(HTMLNode: TIpHTMLNode; AParent: TTreeNode);
var
  i: Integer;
  treenode: TTreeNode;
begin
  treenode := TreeView1.Items.AddChildObject(AParent, HTMLNode.ClassName, HTMLNode);
  if HTMLNode is TIpHTMLNodeMulti then
    for i := 0 to pred(TIpHTMLNodeMulti(HTMLNode).ChildCount) do
      AddNode(TIpHTMLNodeMulti(HTMLNode).ChildNode[i], treenode);
end;

procedure TMainForm.BtnOpenHTMLFileClick(Sender: TObject);
begin
  if OpenDialog1.Execute then
    OpenHtmlFile(OpenDialog1.FileName);
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  OpenHTMLFile('index.html');
end;

procedure TMainForm.HTMLGetImageX(Sender: TIpHtmlNode; const URL: string;
  var Picture: TPicture);
var
  PicCreated: boolean;
begin
  Picture := nil;
  try
    if FileExistsUTF8(URL) then begin
      PicCreated := False;
      if Picture=nil then begin
        Picture := TPicture.Create;   // Note: will be destroyed by IpHtmlPanel
        PicCreated := True;
      end;
      Picture.LoadFromFile(URL);
    end;
  except
    if PicCreated then
      Picture.Free;
    Picture := nil;
  end;
end;

procedure TMainForm.HTMLNodeChange(HTMLNode: TIpHtmlNode);
var
  i: Integer;
begin
  if HTMLNode <> nil then
    for i := 0 to pred(TreeView1.Items.Count) do
      if TreeView1.Items[i].Data = pointer(HTMLNode) then begin
        TreeView1.Selected := TreeView1.Items[i];
        break;
      end;
end;

procedure TMainForm.IpHTMLPanel1CurElementChange(Sender: TObject);
begin
  if IpHTMLPanel1.CurElement <> nil then
    HTMLNodeChange(IpHTMLPanel1.CurElement^.Owner);
end;

procedure TMainForm.IpHtmlPanel1HotChange(Sender: TObject);
begin
  if (IpHTMLPanel1.HotNode <> nil) then begin
    IpHTMLPanel1.HotNode.GetAttributes(Listbox1.Items, True, False);
    HTMLNodeChange(IpHTMLPanel1.HotNode);
  end;
end;

procedure TMainForm.IpHtmlPanel1HotClick(Sender: TObject);
var
  NodeA: TIpHtmlNodeA;
  NewFilename: String;
begin
  if IpHtmlPanel1.HotNode is TIpHtmlNodeA then begin
    NodeA := TIpHtmlNodeA(IpHtmlPanel1.HotNode);
    NewFilename := NodeA.HRef;
    OpenHTMLFile(NewFilename);
  end;
end;

procedure TMainForm.ListBox1DblClick(Sender: TObject);
var
  S, V: string;
  P: Integer;
begin
  if Listbox1.ItemIndex <> -1 then begin
    S := Listbox1.Items[Listbox1.ItemIndex];
    P := pos('=', S);
    V := copy(S, P + 1, MAXINT);
    if InputQuery('Change attribute value', copy(S, 1, P - 1), V) then begin
      TIpHTMLNode(TreeView1.Selected.Data).SetAttributeValue(S, V);
      TIpHTMLNode(TreeView1.Selected.Data).GetAttributes(Listbox1.Items, True, false);
      IpHTMLPanel1.Invalidate;
    end;
  end;
end;

procedure TMainForm.OpenHTMLFile(const Filename: string);
var
  fs: TFileStream;
  NewHTML: TSimpleIpHtml;
begin
  try
    fs:=TFileStream.Create(UTF8ToSys(Filename),fmOpenRead);
    try
      NewHTML := TSimpleIpHtml.Create; // Beware: Will be freed automatically by IpHtmlPanel1
      NewHTML.OnGetImageX := @HTMLGetImageX;
      NewHTML.LoadFromStream(fs);
    finally
      fs.Free;
    end;
    IpHtmlPanel1.SetHtml(NewHTML);
    UpdateTreeView;
  except
    on E: Exception do begin
      MessageDlg('Unable to open HTML file',
        'HTML File: '+Filename+LineEnding
        +'Error: '+E.Message,mtError,[mbCancel],0);
    end;
  end;
end;

procedure TMainForm.PopulateOutline(H: TIpHTML);
begin
  if H <> nil then
    AddNode(H.HTMLNode, nil);
end;

procedure TMainForm.TreeView1Change(Sender: TObject; Node: TTreeNode);
begin
  if Node <> nil then begin
    if (Node.Data <> nil) then begin
      Assert(TObject(Node.Data) is TIpHTMLNode);
      TIpHTMLNode(Node.Data).GetAttributes(Listbox1.Items, True, False);
    end;
  end;
end;

procedure TMainForm.UpdateTreeView;
begin
  TreeView1.Selected := nil;
  TreeView1.Items.Clear;
  Listbox1.Items.Clear;
  TreeView1.Items.BeginUpdate;
  try
    IpHTMLPanel1.EnumDocuments(@PopulateOutline);
  finally
    TreeView1.Items.EndUpdate;
  end;
  TreeView1.FullExpand;
  if TreeView1.Items.Count > 0 then
    TreeView1.Selected := TreeView1.Items[0];
end;

end.

