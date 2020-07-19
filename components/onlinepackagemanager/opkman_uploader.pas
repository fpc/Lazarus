{
***************************************************************************
*                                                                         *
*   This source is free software; you can redistribute it and/or modify   *
*   it under the terms of the GNU General Public License as published by  *
*   the Free Software Foundation; either version 2 of the License, or     *
*   (at your option) any later version.                                   *
*                                                                         *
*   This code is distributed in the hope that it will be useful, but      *
*   WITHOUT ANY WARRANTY; without even the implied warranty of            *
*   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU     *
*   General Public License for more details.                              *
*                                                                         *
*   A copy of the GNU General Public License is available on the World    *
*   Wide Web at <http://www.gnu.org/copyleft/gpl.html>. You can also      *
*   obtain it by writing to the Free Software Foundation,                 *
*   Inc., 51 Franklin Street - Fifth Floor, Boston, MA 02110-1335, USA.   *
*                                                                         *
***************************************************************************

Author: Balázs Székely
Abstract:
  Implementation of the package uploader class.
}
unit opkman_uploader;

{$mode objfpc}{$H+}

{$INCLUDE opkman_fpcdef.inc}

interface

uses
  Classes, SysUtils, base64,
  // OpkMan
  opkman_options, opkman_const,
  {$IFDEF FPC320}fphttpclient, opensslsockets{$ELSE}opkman_httpclient{$ENDIF};

type
  TOnUploadProgress = procedure(Sender: TObject; AFileName: String) of object;
  TOnUploadError = procedure(Sender: TObject; AErrMsg: String) of object;

  { TUploader }

  TUploader = class(TThread)
  private
    FOnUploadProgress: TOnUploadProgress;
    FOnUploadError: TOnUploadError;
    FOnUploadCompleted: TNotifyEvent;
    FHTTPClient: TFPHTTPClient;
    FNeedToBreak: Boolean;
    FFileName: String;
    FURLZip: String;
    FURLJSON: String;
    FZip: String;
    FJSON: String;
    FJSONUpdate: String;
    procedure DoOnUploadProgress;
    procedure DoOnUploadError;
    procedure DoOnUploadCompleted;
    function PostFile(const AURL, AFieldName, AFileName: String): Boolean;
  protected
    procedure Execute; override;
  public
    constructor Create;
    destructor Destroy; override;
    procedure StartUpload(AURLZip, AURLJSON, AZip, AJSON, AJSONUpdate: String);
    procedure StopUpload;
  published
    property OnUploadProgress: TOnUploadProgress read FOnUploadProgress write FOnUploadProgress;
    property OnUploadError: TOnUploadError read FOnUploadError write FOnUploadError;
    property OnUploadCompleted: TNotifyEvent read FOnUploadCompleted write FOnUploadCompleted;
    property NeedToBreak: Boolean read FNeedToBreak write FNeedToBreak;
 end;

var
 Uploader: TUploader = nil;

implementation

{ TUploader }

procedure TUploader.DoOnUploadProgress;
begin
  if Assigned(FOnUploadProgress) then
    FOnUploadProgress(Self, FFileName);
end;

procedure TUploader.DoOnUploadError;
begin
  if Assigned(FOnUploadError) then
    FOnUploadError(Self, Format(rsCreateRepositoryPackageFrm_Error3, [FFileName]));
end;

procedure TUploader.DoOnUploadCompleted;
begin
  if Assigned(FOnUploadCompleted) then
    FOnUploadCompleted(Self);
end;

function TUploader.PostFile(const AURL, AFieldName, AFileName: String): Boolean;
var
  SS: TStringStream;
begin
  Result := False;
  SS := TStringStream.Create('');
  try
    FHTTPClient := TFPHTTPClient.Create(nil);
    try
      if Options.ProxyEnabled then
      begin
        FHTTPClient.Proxy.Host:= Options.ProxyServer;
        FHTTPClient.Proxy.Port:= Options.ProxyPort;
        FHTTPClient.Proxy.UserName:= Options.ProxyUser;
        FHTTPClient.Proxy.Password:= Options.ProxyPassword;
      end;
      try
        FHttpClient.FileFormPost(AURL, AFieldName, AFileName, SS);
      except
      end;
      case AFieldName of
        'zip' : Result := SS.DataString = 'zipok';
        'json': Result := SS.DataString = 'jsonok';
      end;
    finally
      FHTTPClient.Free;
      FHTTPClient := nil;
    end;
  finally
    SS.Free;
  end;
end;

procedure TUploader.Execute;
var
  CanGo: Boolean;
begin
  FFileName := ExtractFileName(FZip);
  CanGo := FileExists(FZip);
  if CanGo then
  begin
    Synchronize(@DoOnUploadProgress);
    CanGo := PostFile(FURLZip, 'zip', FZip);
  end;
  if (not CanGo) and (not FNeedToBreak) then
  begin
    Synchronize(@DoOnUploadError);
    Exit;
  end;
  if FNeedToBreak then
    Exit;

  FFileName := ExtractFileName(FJSON);
  CanGo := FileExists(FJSON);
  if CanGo then
  begin
    Synchronize(@DoOnUploadProgress);
    CanGo := PostFile(FURLJSON, 'json', FJSON);
    Sleep(2000);
  end;
  if (not CanGo) and (not FNeedToBreak) then
  begin
    Synchronize(@DoOnUploadError);
    Exit;
  end;
  if FNeedToBreak then
    Exit;

  if FJSONUpdate <> '' then
  begin
    FFileName := ExtractFileName(FJSONUpdate);
    CanGo := FileExists(FJSONUpdate);
    if CanGo then
    begin
      Synchronize(@DoOnUploadProgress);
      CanGo := PostFile(FURLJSON, 'json', FJSONUpdate);
      Sleep(2000);
    end;
    if (not CanGo) and (not FNeedToBreak) then
    begin
      Synchronize(@DoOnUploadError);
      Exit;
    end;
  end;
  if not FNeedToBreak then
    Synchronize(@DoOnUploadCompleted);
end;

constructor TUploader.Create;
begin
  inherited Create(True);
  FreeOnTerminate := True;
end;

destructor TUploader.Destroy;
begin
  //
  inherited Destroy;
end;

procedure TUploader.StartUpload(AURLZip, AURLJSON, AZip, AJSON, AJSONUpdate: String);
begin
  FURLZip := DecodeStringBase64(AURLZip);
  FURLJSON := DecodeStringBase64(AURLJSON);
  FZip := AZip;
  FJSON := AJSON;
  FJSONUpdate := AJSONUpdate;
  Start;
end;

procedure TUploader.StopUpload;
begin
  if Assigned(FHTTPClient) then
    FHTTPClient.Terminate;
  FNeedToBreak := True;
end;

end.

