unit frarest;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, StdCtrls, Buttons, ActnList, ComCtrls, frmAuthentication, fphttpclient, SynEdit,
  SynHighlighterJScript;

type

  { TRestFrame }
  TRestRequest = Procedure (Sender : TObject; Stream : TStream) of object;

  { TRequestData }

  TRequestData = Class(TCollectionItem)
  private
    FContent: UTF8String;
    FHeaders: TStrings;
    FMethod: String;
    FName: UTF8String;
    FURL: String;
    procedure SetHeaders(AValue: TStrings);
  Public
    Constructor Create(aCollection : TCollection); override;
    Destructor Destroy; override;
    Procedure Assign(Source : TPersistent); override;
    Property Name : UTF8String Read FName Write FName;
    Property Method : String Read FMethod Write FMethod;
    Property URL : String Read FURL Write FURL;
    Property Headers : TStrings Read FHeaders Write SetHeaders;
    Property Content : UTF8String Read FContent Write FContent;
  end;

  { TRequestDataList }

  TRequestDataList = Class(TCollection)
  private
    FOnChanged : TNotifyEvent;
    function GetD(aIndex : Integer): TRequestData;
    procedure SetD(aIndex : Integer; AValue: TRequestData);
  Protected
    procedure Update(Item: TCollectionItem); override;
  Public
    Function AddRequest(aName : UTF8String) : TRequestData;
    Property Requests[aIndex : Integer] : TRequestData Read GetD Write SetD; default;
  end;

  TRestFrame = class(TFrame)
    AAddHeader: TAction;
    AAuthentication: TAction;
    AAddToFavourites: TAction;
    ASend: TAction;
    ADeleteHeader: TAction;
    AEditHeader: TAction;
    ALRest: TActionList;
    Button1: TButton;
    CBMethod: TComboBox;
    CBURL: TComboBox;
    CBUseCurrentTabContent: TCheckBox;
    ILRest: TImageList;
    LHTTPStatus: TLabel;
    LBHeaders: TListBox;
    LBResponseHeaders: TListBox;
    PCRest: TPageControl;
    SBAdd: TSpeedButton;
    SBDelete: TSpeedButton;
    SBEdit: TSpeedButton;
    SERequestContent: TSynEdit;
    SpeedButton1: TSpeedButton;
    SpeedButton2: TSpeedButton;
    SynContent: TSynJScriptSyn;
    TSResult: TTabSheet;
    TSRequestContent: TTabSheet;
    TSHeaders: TTabSheet;
    procedure AAddHeaderExecute(Sender: TObject);
    procedure AAddToFavouritesExecute(Sender: TObject);
    procedure AAddToFavouritesUpdate(Sender: TObject);
    procedure AAuthenticationExecute(Sender: TObject);
    procedure ADeleteHeaderExecute(Sender: TObject);
    procedure ADeleteHeaderUpdate(Sender: TObject);
    procedure AEditHeaderExecute(Sender: TObject);
    procedure AEditHeaderUpdate(Sender: TObject);
    procedure ASendExecute(Sender: TObject);
    procedure ASendUpdate(Sender: TObject);
    procedure CBURLKeyPress(Sender: TObject; var Key: char);
  private
    FFavourites: TRequestDataList;
    FOnContentReceived: TRestRequest;
    FOnSendContent: TRestRequest;
    function GetOnFavouritesChanged: TNotifyEvent;
    function GetRequestData: TStream;
    function GetURL: String;
    procedure GetUserNamePassword(out aUserName, aPassword: String);
    procedure SetFavourites(AValue: TRequestDataList);
    procedure SetOnFavouritesChanged(AValue: TNotifyEvent);
    procedure SetUserNamePassword(const aUserName, aPassword: String);
    procedure ShowResult(H: TFPHTTPClient; Resp: TStream);
  public
    Constructor Create(aOwner : TComponent); override;
    Destructor Destroy; override;
    Procedure ExecuteRequest;
    Function HaveFavouriteData: Boolean;
    Function AddToFavourites(Const AName : String; AddContent : Boolean) : TRequestData;
    Function AddToFavourites : TRequestData;
    Procedure ApplyFavourite(aFavourite : TRequestData);
    Procedure LoadFavourites(Const FileName : String);
    Procedure SaveFavourites(Const FileName : String);
    Property OnFavouritesChanged : TNotifyEvent Read GetOnFavouritesChanged Write SetOnFavouritesChanged;
    Property Favourites : TRequestDataList Read FFavourites Write SetFavourites;
    Property OnSendContent: TRestRequest Read FOnSendContent Write FOnSendContent;
    Property OnContentReceived: TRestRequest Read FOnContentReceived Write FOnContentReceived;
  end;

implementation

uses base64,dialogs, uriparser, frmheader, strutils, frmaddtofavourite, jsonconf;

Const
  SAuthorization = 'Authorization';
  SBasic = 'Basic';
{$R *.lfm}

{ TRequestData }

procedure TRequestData.SetHeaders(AValue: TStrings);
begin
  if FHeaders=AValue then Exit;
  FHeaders.Assign(aValue);
end;

constructor TRequestData.Create(aCollection: TCollection);
begin
  inherited Create(aCollection);
  FHeaders:=TStringList.Create;
  FHeaders.NameValueSeparator:=':';
end;

destructor TRequestData.Destroy;
begin
  FreeAndNil(Fheaders);
  inherited Destroy;
end;

procedure TRequestData.Assign(Source: TPersistent);

Var
  RD : TRequestData;

begin
  if (Source is TRequestData) then
    begin
    RD:=Source as TRequestData;
    FName:=RD.Name;
    FContent:=RD.Content;
    FHeaders:=RD.Headers;
    FMethod:=RD.Method;
    FURL:=RD.URL;
    end
  else
    inherited Assign(Source);
end;

{ TRequestDataList }

function TRequestDataList.GetD(aIndex : Integer): TRequestData;
begin
  Result:=Items[aIndex] as TRequestData;
end;

procedure TRequestDataList.SetD(aIndex : Integer; AValue: TRequestData);
begin
  Items[aIndex]:=AValue;
end;

procedure TRequestDataList.Update(Item: TCollectionItem);
begin
  inherited Update(Item);
  If Assigned(FOnChanged) then
    FOnChanged(Self);
end;

function TRequestDataList.AddRequest(aName: UTF8String): TRequestData;
begin
  Result:=Add as TRequestData;
  Result.Name:=AName;
end;

{ TRestFrame }

procedure TRestFrame.ASendUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled:=(CBURL.Text<>'') and (CBMethod.Text<>'');
end;

procedure TRestFrame.CBURLKeyPress(Sender: TObject; var Key: char);
begin
  if Key=#13 then
    ExecuteRequest;
end;

Function TRestFrame.GetURL : String;

Var
  URI : TURI;

begin
  URI:=ParseURI(CBURL.Text,'http',80,False);
  if URI.Port=80 then
    URI.Port:=0;
  Result:=EncodeURI(URI);
end;

Function TRestFrame.GetRequestData : TStream;

var
  Req : TStream;
begin
  Req:=Nil;
  if CBUseCurrentTabContent.Checked then
    begin
    Req:=TMemoryStream.Create;
    if Assigned(OnSendContent) then
      OnSendContent(Self,Req);
    end
  else if (Trim(SERequestContent.Text)<>'') then
    begin
    Req:=TMemoryStream.Create;
    SERequestContent.Lines.SaveToStream(Req);
    end;
  if Assigned(Req) then
    Req.Position:=0;
  Result:=Req;
end;

function TRestFrame.GetOnFavouritesChanged: TNotifyEvent;
begin
  Result:=FFavourites.FOnChanged;
end;

procedure TRestFrame.ShowResult(H : TFPHTTPClient; Resp : TStream);

begin
  TSResult.TabVisible:=True;
  PCRest.ActivePage:=TSResult;
  LBResponseHeaders.Items:=H.ResponseHeaders;
  With H do
    if ResponseStatusCode=0 then
      LHTTPStatus.Caption:='HTTP request failed'
    else
      LHTTPStatus.Caption:=Format('HTTP %s %d %s',[ServerHTTPVersion,ResponseStatusCode,ResponseStatusText]);
  if Assigned(OnContentReceived) and (Resp.Size>0) then
    OnContentReceived(Self,Resp);
end;

constructor TRestFrame.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  // For some reason this is not kept ?
  LBHeaders.Items.NameValueSeparator:=':';
  FFavourites:=TRequestDataList.Create(TRequestData);
end;

destructor TRestFrame.Destroy;
begin
  FreeAndNil(FFavourites);
  inherited Destroy;
end;

procedure TRestFrame.ExecuteRequest;

Var
  H : TFPHTTPClient;
  S : String;
  Req,Resp : TStream;

begin
  Resp:=nil;
  Req:=nil;
  H:=TFPHTTPClient.Create(Self);
  try
    For S in LBHeaders.Items do
      H.RequestHeaders.Add(S);
    if Not SameText(CBMethod.Text,'GET') then
      Req:=GetRequestData;
    H.RequestBody:=Req;
    Resp:=TMemoryStream.Create;
    try
      H.HTTPMethod(CBMethod.Text,GetURL,Resp,[]);
    except
      on E : Exception do
        ShowMessage(Format('Request failed with error %s : %s',[E.ClassName,E.Message]));
    end;
    ShowResult(H,Resp);
  finally
    Req.Free;
    Resp.Free;
    H.Free;
  end;
end;

function TRestFrame.AddToFavourites(const AName: String; AddContent: Boolean): TRequestData;

Var
  R : TRequestData;
  S : TStringStream;

begin
  FFavourites.BeginUpdate;
  try
    R:=FFavourites.AddRequest(aName);
    R.Method:=CBMethod.Text;
    R.Headers:=LBHeaders.Items;
    R.URL:=CBURL.Text;
    if AddContent then
      if Not (CBUseCurrentTabContent.Checked and Assigned(OnSendContent)) then
        R.Content:=SERequestContent.Text
      else
        begin
{$IF FPC_FULLVERSION>30004}
        S:=TStringStream.Create('',CP_UTF8);
{$ELSE}
        S:=TStringStream.Create('');
{$ENDIF}
        try
          OnSendContent(Self,S);
          R.Content:=S.DataString;
        finally
          S.Free;
        end;
        end;
    Result:=R;
  finally
    FFavourites.EndUpdate;
  end;
end;

procedure TRestFrame.ApplyFavourite(aFavourite: TRequestData);

begin
  CBMethod.Text:=aFavourite.Method;
  LBHeaders.Items:=aFavourite.Headers;
  CBURL.Text:=aFavourite.URL;
  if aFavourite.Content<>'' then
    begin
    CBUseCurrentTabContent.Checked:=False;
    SERequestContent.Text:=aFavourite.Content;
    end;
end;

procedure TRestFrame.LoadFavourites(const FileName: String);
Var
  C : TJSONConfig;
  I,aCount : Integer;
  N : UTF8String;

begin
  C:=Nil;
  FFavourites.BeginUpdate;
  try
    FFavourites.Clear;
    C:=TJSONConfig.Create(Self);
    C.Filename:=FileName;
    C.OpenKey('/Favourites',True);
    aCount:=C.GetValue('Count',0);
    For I:=1 to aCount do
      begin
      C.OpenKey(Format('/Favourites/Favourite%d',[I]),True);
      N:=C.GetValue('Name','');
      With FFavourites.AddRequest(N) do
        begin
        Method:=C.GetValue('Method','GET');
        URL:=C.GetValue('Url','');
        C.GetValue('Headers',Headers,'');
        end;
      end;
  finally
    FFavourites.EndUpdate;
    C.Free;
  end;
end;

procedure TRestFrame.SaveFavourites(const FileName: String);

Var
  C : TJSONConfig;
  I : Integer;

begin
  C:=TJSONConfig.Create(Self);
  try
    C.Filename:=FileName;
    C.OpenKey('/Favourites',True);
    C.SetValue('Count',FFavourites.Count);
    For I:=0 to FFavourites.Count-1 do
      begin
      C.OpenKey(Format('/Favourites/Favourite%d',[I+1]),True);
      With FFavourites[i] do
        begin
        C.SetValue('Name',FFavourites[i].Name);
        C.SetValue('Method',FFavourites[i].Method);
        C.SetValue('Url',FFavourites[i].URL);
        C.SetValue('Headers',FFavourites[i].Headers);
        end;
      end;
    C.Flush;
  finally
    C.Free;
  end;
end;

procedure TRestFrame.ASendExecute(Sender: TObject);
begin
  ExecuteRequest;
end;

procedure TRestFrame.AAddHeaderExecute(Sender: TObject);

begin
  LBHeaders.Items.NameValueSeparator:=':';
  With THeaderForm.Create(Self) do
    try
      if ShowModal=mrOK then
        if (HeaderValue<>'') then
          LBHeaders.Items.Values[HeaderName]:=' '+HeaderValue
        else
          LBHeaders.Items.Add(HeaderName+': ');
    finally
      Free;
    end;
end;

function TRestFrame.AddToFavourites: TRequestData;

begin
  Result:=nil;
  With TSaveRequestDataForm.Create(Self) do
    try
      if ShowModal=mrOK then
        Result:=AddToFavourites(RequestName,SaveContent);
    Finally
      Free;
    end;
end;


procedure TRestFrame.AAddToFavouritesExecute(Sender: TObject);
begin
  AddToFavourites;
end;

Function TRestFrame.HaveFavouriteData : Boolean;

begin
  Result:=(CBURL.Text<>'') and (CBMethod.Text<>'');
end;

procedure TRestFrame.AAddToFavouritesUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled:=HaveFavouriteData;
end;

procedure TRestFrame.GetUserNamePassword(Out aUserName,aPassword : String);

Var
  A : String;

begin
  aUserName:='';
  aPassword:='';
  A:=Trim(LBHeaders.Items.Values[SAuthorization]);
  if Not SameText(ExtractWord(1,A,[' ']),SBasic) then exit;
  A:=ExtractWord(2,A,[' ']);
  if A='' then exit;
  A:=DecodeStringBase64(A);
  aUserName:=ExtractWord(1,A,[':']);
  aPassword:=ExtractWord(2,A,[':']);
end;

procedure TRestFrame.SetFavourites(AValue: TRequestDataList);
begin
  if FFavourites=AValue then Exit;
  FFavourites.Assign(AValue);
end;

procedure TRestFrame.SetOnFavouritesChanged(AValue: TNotifyEvent);
begin
  if GetOnFavouritesChanged=AValue then Exit;
  FFavourites.FOnChanged:=AValue;
end;

procedure TRestFrame.SetUserNamePassword(Const aUserName,aPassword : String);

Var
  A : String;

begin
  A:=EncodeStringBase64(aUserName+':'+aPassword);
  LBHeaders.Items.Values[SAuthorization]:=SBasic+' '+A;
end;

procedure TRestFrame.AAuthenticationExecute(Sender: TObject);

Var
  UN, PW : String;

begin
  LBHeaders.Items.NameValueSeparator:=':';
  GetUserNamePassword(UN,PW);
  With TAuthenticationForm.Create(Self) do
    try
      Username:=UN;
      Password:=PW;
      If ShowModal=mrOK then
        SetUserNamePassword(UserName,Password);
    finally
      Free;
    end;
end;

procedure TRestFrame.ADeleteHeaderExecute(Sender: TObject);
begin
  With LBHeaders do
    if (ItemIndex<>-1) then
      Items.Delete(ItemIndex);
end;

procedure TRestFrame.ADeleteHeaderUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled:=(LBHeaders.ItemIndex<>-1);
end;

procedure TRestFrame.AEditHeaderExecute(Sender: TObject);

Var
  idx : Integer;

begin
  LBHeaders.Items.NameValueSeparator:=':';
  idx:=LBHeaders.ItemIndex;
  With THeaderForm.Create(Self) do
    try
      HeaderName:=LBHeaders.Items.Names[idx];
      HeaderValue:=LBHeaders.Items.ValueFromIndex[idx];
      if (ShowModal=mrOK) then
        if (HeaderValue<>'') then
          LBHeaders.Items[idx]:=HeaderName+': '+HeaderValue
        else
          LBHeaders.Items[idx]:=HeaderName+':';
    finally
      Free;
    end;
end;

procedure TRestFrame.AEditHeaderUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled:=(LBHeaders.ItemIndex<>-1);
end;

end.

