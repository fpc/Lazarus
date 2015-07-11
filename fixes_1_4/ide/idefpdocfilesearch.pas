unit IDEFPDocFileSearch;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ButtonPanel, Buttons, ComCtrls, ExtCtrls, LCLType,
  LazarusIDEStrConsts, PackageSystem, PackageDefs;

type

  { TFPDocFileSearchDialog }

  TFPDocFileSearchDialog = class(TForm)
    CloseBitBtn: TBitBtn;
    ResultsMemo: TMemo;
    ScopeGroupBox: TGroupBox;
    ScopeTreeView: TTreeView;
    SearchTextComboBox: TComboBox;
    SearchTextLabel: TLabel;
    Splitter1: TSplitter;
    procedure FormCreate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure OnIdle(Sender: TObject; var Done: Boolean);
  private
    FIdleConnected: boolean;
    procedure SetIdleConnected(AValue: boolean);
    procedure FillScope;
  public
    property IdleConnected: boolean read FIdleConnected write SetIdleConnected;
  end;

procedure ShowFPDocFileSearch;

implementation

procedure ShowFPDocFileSearch;
var
  FPDocFileSearchDialog: TFPDocFileSearchDialog;
begin
  FPDocFileSearchDialog:=TFPDocFileSearchDialog.Create(nil);
  try
    FPDocFileSearchDialog.ShowModal;
  finally
    FPDocFileSearchDialog.Free;
  end;
end;

{$R *.lfm}

{ TFPDocFileSearchDialog }

procedure TFPDocFileSearchDialog.FormCreate(Sender: TObject);
begin
  Caption:='Search in FPDoc files';
  CloseBitBtn.Caption:='Close';
  SearchTextLabel.Caption:='Search:';
  SearchTextComboBox.Text:='';
  ActiveControl:=SearchTextComboBox;
  ScopeGroupBox.Caption:='Scope';

  ResultsMemo.Clear;
  FillScope;
  IdleConnected:=true;
end;

procedure TFPDocFileSearchDialog.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key=VK_ESCAPE then
    ModalResult:=mrCancel;
end;

procedure TFPDocFileSearchDialog.OnIdle(Sender: TObject; var Done: Boolean);
begin
  // ToDo:


  // nothing to do
  IdleConnected:=false;
end;

procedure TFPDocFileSearchDialog.SetIdleConnected(AValue: boolean);
begin
  if FIdleConnected=AValue then Exit;
  FIdleConnected:=AValue;
  if IdleConnected then
    Application.AddOnIdleHandler(@OnIdle)
  else
    Application.RemoveOnIdleHandler(@OnIdle);
end;

procedure TFPDocFileSearchDialog.FillScope;
var
  PkgsNode: TTreeNode;
  i: Integer;
  Pkg: TLazPackage;
begin
  ScopeTreeView.Items.BeginUpdate;
  ScopeTreeView.Items.Clear;

  // packages with fpdoc
  PkgsNode:=ScopeTreeView.Items.Add(nil,'Packages');
  for i:=0 to PackageGraph.Count-1 do begin
    Pkg:=PackageGraph[i];
    if Pkg.FPDocPaths='' then continue;
    ScopeTreeView.Items.AddChild(PkgsNode,Pkg.Name);
  end;
  PkgsNode.Expanded:=true;

  ScopeTreeView.Items.EndUpdate;
end;

end.

