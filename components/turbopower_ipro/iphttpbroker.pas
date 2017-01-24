(* ***** BEGIN LICENSE BLOCK *****
 * Version: MPL 1.1
 *
 * The contents of this file are subject to the Mozilla Public License Version
 * 1.1 (the "License"); you may not use this file except in compliance with
 * the License. You may obtain a copy of the License at
 * http://www.mozilla.org/MPL/
 *
 * Software distributed under the License is distributed on an "AS IS" basis,
 * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
 * for the specific language governing rights and limitations under the
 * License.
 *
 * The Original Code is TurboPower Internet Professional
 *
 * The Initial Developer of the Original Code is
 * TurboPower Software
 *
 * Portions created by the Initial Developer are Copyright (C) 2000-2002
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 *
 *   Silvio Clecio - https://github.com/silvioprog
 *
 * ***** END LICENSE BLOCK ***** *)

(* Part of Ipbroker.pas allowing to use online files  Silvio Clecio Jan 2016 *)

unit Iphttpbroker;

{$I ipdefine.inc}

interface

uses
  IpFileBroker, FPHttpClient, IpMsg, IpUtils, IpHtml, Graphics, Classes;

type

  { TIpHttpClient }

  TIpHttpClient = class(TFPHTTPClient)
  end;

  { TIpHttpDataProvider }

  TIpHttpDataProvider = class(TIpCustomHtmlDataProvider)
  private
    FClient: TIpHttpClient;
    FDocumment: TMemoryStream;
    FContentType: string;
  protected
    property Documment: TMemoryStream read FDocumment;
    property Client: TIpHttpClient read FClient;
  public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;
    function GetHtmlStream(const AUrl: string;
      APostData: TIpFormDataEntity): TStream; override;
  {$IFDEF IP_LAZARUS}
    function DoGetStream(const AUrl: string): TStream; override;
  {$ENDIF}
    function CheckURL(const AUrl: string;
      var AContentType: string): Boolean; override;
    procedure Leave(AHtml: TIpHtml); override;
    procedure Reference(const AUrl: string); override;
    procedure GetImage(ASender: TIpHtmlNode; const AUrl: string;
      var APicture: TPicture); override;
    function CanHandle(const AUrl: string): Boolean; override;
  end;

procedure Register;

implementation

{ TIpHttpDataProvider }

constructor TIpHttpDataProvider.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FClient := TIpHttpClient.Create(nil);
  FDocumment := TMemoryStream.Create;
  HandledProtocols.Add('HTTP');
  FClient.ResponseHeaders.NameValueSeparator := ':';
{$IF FPC_FULLVERSION > 30000}
  FClient.AllowRedirect := True;
  FClient.MaxRedirects := High(Byte);
{$ENDIF}
end;

destructor TIpHttpDataProvider.Destroy;
begin
  FDocumment.Free;
  FClient.Free;
  inherited Destroy;
end;

function TIpHttpDataProvider.GetHtmlStream(const AUrl: string;
  APostData: TIpFormDataEntity): TStream;
begin
  Result := TMemoryStream.Create;
  Result.CopyFrom(FDocumment, 0);
  Result.Seek(0, soFromBeginning);
end;

{$IFDEF IP_LAZARUS}
function TIpHttpDataProvider.DoGetStream(const AUrl: string): TStream;
begin
  Result := TMemoryStream.Create;
  Result.CopyFrom(FDocumment, 0);
  Result.Seek(0, soFromBeginning);
end;
{$ENDIF}

function TIpHttpDataProvider.CheckURL(const AUrl: string;
  var AContentType: string): Boolean;
var
  VAddrRec: TIpAddrRec;
begin
{$IFDEF VER2_6}
  FillChar(VAddrRec, SizeOf(TIpAddrRec), 0);
{$ELSE}
  VAddrRec := Default(TIpAddrRec);
{$ENDIF}
  Initialize(VAddrRec);
  try
    IpParseURL(AUrl, VAddrRec);
    FDocumment.Clear;
    FClient.Get(AUrl, FDocumment);
    Result := (FClient.ResponseStatusCode = 200)
{$IF FPC_FULLVERSION > 30000}or FClient.IsRedirect(FClient.ResponseStatusCode){$ENDIF};
    if Result then
    begin
      FContentType := AllTrimSpaces(FClient.ResponseHeaders.Values['Content-Type']);
      AContentType := FContentType;
    end;
  finally
    Finalize(VAddrRec);
  end;
end;

procedure TIpHttpDataProvider.Leave(AHtml: TIpHtml);
begin
  inherited Leave(AHtml);
end;

procedure TIpHttpDataProvider.Reference(const AUrl: string);
begin
  inherited Reference(AUrl);
end;

procedure TIpHttpDataProvider.GetImage(ASender: TIpHtmlNode;
  const AUrl: string; var APicture: TPicture);
var
  VImgRaw: TStream;
begin
  APicture := nil;
  VImgRaw := TMemoryStream.Create;
  try
    FClient.Get(AUrl, VImgRaw);
    if (FClient.ResponseStatusCode = 200) and
      (Pos('image/', FClient.ResponseHeaders.Values['Content-Type']) > 0) then
      try
        VImgRaw.Seek(0, soFromBeginning);
        APicture := TPicture.Create;
        APicture.LoadFromStream(VImgRaw);
      except
        on EInvalidGraphic do
        begin
          APicture.Free;
          APicture := nil;
        end;
      end;
  finally
    VImgRaw.Free;
  end;
end;

function TIpHttpDataProvider.CanHandle(const AUrl: string): Boolean;
begin
  Result := Assigned(FDocumment) and ((Pos('text/html', FContentType) > 0) or
    (Pos('image/', FContentType) > 0));
end;

procedure Register;
begin
  RegisterComponents('IPro', [TIpHttpDataProvider]);
end;

end.
