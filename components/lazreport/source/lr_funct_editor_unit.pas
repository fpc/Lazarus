unit lr_funct_editor_unit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, LR_Class, Buttons;

type

  { TLR_FunctEditorForm }

  TLR_FunctEditorForm = class(TForm)
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    Label1: TLabel;
    Label2: TLabel;
    ListBox1: TListBox;
    ListBox2: TListBox;
    Panel1: TPanel;
    Panel2: TPanel;
    Splitter1: TSplitter;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure ListBox1Click(Sender: TObject);
    procedure ListBox2Click(Sender: TObject);
    procedure ListBox2DblClick(Sender: TObject);
  private
    { private declarations }
  public
    CurentFunctionDescription:TfrFunctionDescription;
  end;

implementation

{$R *.lfm}

{ TLR_FunctEditorForm }

procedure TLR_FunctEditorForm.FormCreate(Sender: TObject);
var
  i,j:integer;
  F:TfrFunctionLibrary;
  FD:TfrFunctionDescription;
begin
  for i:=0 to frFunctionsCount-1 do
  begin
    F:=frFunctions[i].FunctionLibrary;
    for j:=0 to F.FunctionCount-1 do
    begin
      FD := F.Description[j];
      if Assigned(FD) then
      begin
        if ListBox1.Items.IndexOf(FD.funGroup)=-1 then
          ListBox1.Items.Add(FD.funGroup);
      end
    end;
  end;
  if ListBox1.Items.Count>0 then
    ListBox1.ItemIndex:=0;
  ListBox1Click(nil);
  CurentFunctionDescription:=nil;
end;

procedure TLR_FunctEditorForm.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  if ModalResult = mrOk then
  begin
    if (ListBox2.Items.Count>0) and (ListBox2.ItemIndex>-1) and (ListBox2.ItemIndex<ListBox2.Items.Count) then
    begin
      CurentFunctionDescription:=TfrFunctionDescription(ListBox2.Items.Objects[ListBox2.ItemIndex]);
    end;
  end
end;

procedure TLR_FunctEditorForm.ListBox1Click(Sender: TObject);
var
  i,j:integer;
  F:TfrFunctionLibrary;
  FD:TfrFunctionDescription;
  GrpName:string;
begin
  if (ListBox1.Items.Count>0) and (ListBox1.ItemIndex>-1) and (ListBox1.ItemIndex<ListBox1.Items.Count) then
  begin
    GrpName:=ListBox1.Items[ListBox1.ItemIndex];
    ListBox2.Items.Clear;
    for i:=0 to frFunctionsCount-1 do
    begin
      F:=frFunctions[i].FunctionLibrary;
      for j:=0 to F.FunctionCount-1 do
      begin
        FD := F.Description[j];
        if Assigned(FD) then
        begin
          if FD.funGroup = GrpName then
          begin
            ListBox2.Items.Add(FD.funName);
            ListBox2.Items.Objects[ListBox2.Items.Count-1]:=FD;
          end;
        end
      end;
    end;
    if ListBox2.Items.Count>0 then
      ListBox2.ItemIndex:=0;
    ListBox2Click(nil);
  end;
end;

procedure TLR_FunctEditorForm.ListBox2Click(Sender: TObject);
var
  FD:TfrFunctionDescription;
  S:string;
begin
  if (ListBox2.Items.Count>0) and (ListBox2.ItemIndex>-1) and (ListBox2.ItemIndex<ListBox2.Items.Count) then
  begin
    FD:=TfrFunctionDescription(ListBox2.Items.Objects[ListBox2.ItemIndex]);
    if Assigned(FD) then
    begin
      S:=FD.funDescription;
      Label1.Caption:=Copy(S, 1, Pos('/', S)-1);
      Delete(S, 1, Pos('/', S));
      Label2.Caption:=S;
    end;
  end;
end;

procedure TLR_FunctEditorForm.ListBox2DblClick(Sender: TObject);
begin
  ModalResult:=mrOk;
end;

end.

