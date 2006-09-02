{ Copyright (C) <2005> <Andrew Haines> lazchmhelp.pas

  This source is free software; you can redistribute it and/or modify it under
  the terms of the GNU General Public License as published by the Free
  Software Foundation; either version 2 of the License, or (at your option)
  any later version.

  This code is distributed in the hope that it will be useful, but WITHOUT ANY
  WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
  FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
  details.

  A copy of the GNU General Public License is available on the World Wide Web
  at <http://www.gnu.org/copyleft/gpl.html>. You can also obtain it by writing
  to the Free Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
  MA 02111-1307, USA.
}

unit LazChmHelp;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LazHelpIntf, HelpIntfs, LazConfigStorage,
  PropEdits, LHelpControl;
  
type
  
  { TChmHelpViewer }

  TChmHelpViewer = class(THelpViewer)
  private
    fHelpExe: String;
    fHelpLabel: String;
    fHelpConnection: TLHelpConnection;
    fChmsFilePath: String;
  protected
    function GetFileNameAndURL(RawUrl: String; out FileName: String; out URL: String): Boolean;
    procedure SetHelpEXE(AValue: String);
    procedure SetHelpLabel(AValue: String);
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    function SupportsTableOfContents: boolean; override;
    procedure ShowTableOfContents(Node: THelpNode); override;
    //function SupportsMimeType(const AMimeType: string): boolean; virtual;
    function ShowNode(Node: THelpNode; var ErrMsg: string): TShowHelpResult; override;
    //procedure Hide; virtual;
    procedure Assign(Source: TPersistent); override;
    procedure Load(Storage: TConfigStorage); override;
    procedure Save(Storage: TConfigStorage); override;
    function GetLocalizedName: string; override;
  published
    property HelpEXE: String read fHelpEXE write SetHelpEXE;
    property HelpLabel: String read fHelpLabel write SetHelpLabel;
    property HelpFilesPath: String read fChmsFilePath write fChmsFilePath;

  end;
  
  procedure Register;

implementation

{ TChmHelpViewer }


function TChmHelpViewer.GetFileNameAndURL(RawUrl:String; out FileName: String; out URL: String
  ): Boolean;
var
fPos: Integer;
begin
  Result := False;

  fPos := Pos(':/', RawUrl);
  if fPos = 0 then exit;
  FileName := Copy(RawUrl, 1, fPos-1);
  URL := Copy(RawUrl, fPos+2, Length(RawUrl)-(fPos-2));
  Result := True;
end;

procedure TChmHelpViewer.SetHelpEXE(AValue: String);
begin
  fHelpExe := AValue;
end;

procedure TChmHelpViewer.SetHelpLabel(AValue: String);
begin
 fHelpLabel := AValue;
end;

constructor TChmHelpViewer.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  fHelpConnection := TLHelpConnection.Create;
  AddSupportedMimeType('text/html');
end;

destructor TChmHelpViewer.Destroy;
begin
  fHelpConnection.Free;
  inherited Destroy;
end;

function TChmHelpViewer.SupportsTableOfContents: boolean;
begin
  Result:=True;
end;

procedure TChmHelpViewer.ShowTableOfContents(Node: THelpNode);
begin
//  inherited ShowTableOfContents(Node);
end;

function TChmHelpViewer.ShowNode(Node: THelpNode; var ErrMsg: string
  ): TShowHelpResult;
var
FileName: String;
Url: String;
begin
  Result:=shrNone;
  if not FileExists(fHelpEXE) then begin
    ErrMsg := 'The program "' + fHelpEXE + '" doesn''t seem to exist!';
    Exit(shrViewerNotFound);
  end;
  if not GetFileNameAndURL(Node.Url, FileName, Url) then begin
    ErrMsg := 'Couldn''t read the file/URL correctly';
    Exit(shrDatabaseNotFound);
  end;
  FileName := fChmsFilePath+FileName;
  fHelpConnection.StartHelpServer(fHelpLabel, fHelpExe);
  fHelpConnection.OpenURL(FileName, Url);
  Result := shrSuccess;
  //WriteLn('LOADING URL = ', Node.URL);
end;

procedure TChmHelpViewer.Assign(Source: TPersistent);
var
  Viewer: TChmHelpViewer;
begin
  if Source is TChmHelpViewer then begin
    Viewer:=TChmHelpViewer(Source);
    HelpEXE:=Viewer.HelpEXE;
    HelpLabel:=Viewer.HelpLabel;
    HelpFilesPath:=Viewer.HelpFilesPath;
  end;
  inherited Assign(Source);
end;

procedure TChmHelpViewer.Load(Storage: TConfigStorage);
begin
  HelpEXE:=Storage.GetValue('CHMHelp/Exe','');
  HelpLabel:=Storage.GetValue('CHMHelp/Name','lazhelp');
  HelpFilesPath := Storage.GetValue('CHMHelp/FilesPath','');
end;

procedure TChmHelpViewer.Save(Storage: TConfigStorage);
begin
  Storage.SetDeleteValue('CHMHelp/Exe',HelpEXE,'');
  Storage.SetDeleteValue('CHMHelp/Name',HelpLabel,'lazhelp');
  Storage.SetDeleteValue('CHMHelp/FilesPath',HelpFilesPath,'');
end;

function TChmHelpViewer.GetLocalizedName: string;
begin
  Result := 'CHM Help Viewer';
end;

procedure Register;
var
  ChmHelp: TChmHelpViewer;
begin
  ChmHelp := TChmHelpViewer.Create(nil);
  HelpViewers.RegisterViewer(ChmHelp);
end;

initialization
  RegisterPropertyEditor(TypeInfo(AnsiString),
    TCHmHelpViewer,'HelpEXE',TFileNamePropertyEditor);
  RegisterPropertyEditor(TypeInfo(AnsiString),
    TCHmHelpViewer,'HelpFilesPath',TFileNamePropertyEditor);
end.

