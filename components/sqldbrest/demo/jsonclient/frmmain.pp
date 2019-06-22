unit frmmain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, db, BufDataset, fphttpclient, Forms, Controls, Graphics,
  Dialogs, StdCtrls, ExtCtrls, DBCtrls, DBGrids, IniPropStorage, ComCtrls,
  fpJSON, SynEdit, SynHighlighterXML, fpjsondataset, sqldbrestdataset;

type

  { TMainForm }

  TMainForm = class(TForm)
    BGetresources: TButton;
    BFetchResource: TButton;
    DSResource: TDataSource;
    GResource: TDBGrid;
    HTCResource: TFPHTTPClient;
    NavResource: TDBNavigator;
    EURL: TEdit;
    EUserName: TEdit;
    EPassword: TEdit;
    GBServer: TGroupBox;
    PCData: TPageControl;
    PSMain: TIniPropStorage;
    LEURL: TLabel;
    LUserName: TLabel;
    LPassword: TLabel;
    LBResources: TListBox;
    PResource: TPanel;
    PData: TPanel;
    SERawData: TSynEdit;
    TSGrid: TTabSheet;
    TSRaw: TTabSheet;
    procedure BFetchResourceClick(Sender: TObject);
    procedure BGetresourcesClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure PSMainRestoreProperties(Sender: TObject);
    procedure PSMainSaveProperties(Sender: TObject);
  private
    JSResource : TSQLDBRestDataset;
    function ConfigConnection: String;
    procedure DisplayResources(J: TJSONObject);
    procedure GetResourceData;
    procedure GetResources;

  public

  end;

var
  MainForm: TMainForm;

implementation

uses strutils,httpprotocol, jsonparser, URIParser;

{$R *.lfm}

{ TMainForm }

Function TMainForm.ConfigConnection : String;
Var
  P : String;

begin
  HTCResource.UserName:=EUserName.Text;
  HTCResource.Password:=EPassword.Text;
  Result:=EURL.Text;
  if (Result='') then
    Raise Exception.Create('Need a URL to perform request');
  P:=LowerCase(ParseUri(Result,False).Protocol);
  if (P<>'http') and (P<>'https') then
     Result:='http://'+Result;
  Result:=IncludeHTTPPathDelimiter(Result);
end;


procedure TMainForm.DisplayResources(J : TJSONObject);

Var
  A : TJSONArray;
  I : Integer;
  R : TJSONObject;
  N : String;

begin
  LBResources.Items.Clear;
  A:=J.Get('data',TJSONArray(Nil));
  if not assigned(A) then
    exit;
  For I:=0 to A.Count-1 do
    begin
    R:=A.Objects[i];
    N:=R.Get('name','');
    if N<>'' then
      LBResources.Items.Add(N);
    end;
end;

procedure TMainForm.GetResources;

Var
  S : TMemoryStream;
  URL : String;
  D : TJSONData;
  J : TJSONObject absolute D;

begin
  URL:=ConfigConnection;
  S:=TMemoryStream.Create;
  try
    HTCResource.Get(URL+'metadata/?fmt=json&humanreadable=0',S);
    S.Position:=0;
    D:=GetJSON(S);
    if D is TJSONObject then
      DisplayResources(J);
  finally
    S.Free;
  end;
end;

procedure TMainForm.GetResourceData;

Var
  S : TMemoryStream;
  URL : String;

begin
  URL:=ConfigConnection;
  if LBResources.ItemIndex<>-1 then
    URL:=URL+LBResources.Items[LBResources.ItemIndex];
  S:=TMemoryStream.Create;
  try
    HTCResource.Get(URL+'?fmt=json&humanreadable=1',S);
    S.Position:=0;
    SERawData.Lines.LoadFromStream(S);
    S.Position:=0;
    JSResource.Close;
    JSResource.LoadFromStream(S);
    JSResource.Open;
  finally
    S.Free;
  end;
end;

procedure TMainForm.BGetresourcesClick(Sender: TObject);

begin
  GetResources;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  JSResource:=TSQLDBRestDataset.Create(Self);
  DSResource.DataSet:=JSResource;
end;

procedure TMainForm.PSMainRestoreProperties(Sender: TObject);

Var
  S: String;

begin
  S:=PSMAin.ReadString('pwd','');
  if (S<>'') then
    EPassword.Text:=XorDecode('secret',S)
  else
    EPassword.Clear;
end;

procedure TMainForm.PSMainSaveProperties(Sender: TObject);
Var
  S: String;
begin
  S:=EPassword.Text;
  If (S<>'') then
    PSMAin.WriteString('pwd',XorEncode('secret',S));
end;

procedure TMainForm.BFetchResourceClick(Sender: TObject);
begin
  GetResourceData;
end;



end.

