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
  Classes, SysUtils, HelpIntf, HelpManager, ConfigStorage, PropEdits, LHelpControl;
  
type
  
  { TChmHelpViewer }

  TChmHelpViewer = class(THelpViewer)
  private
    fHelpServerExe: String;
    fHelpServerName: String;
    fHelpConnection: TLHelpConnection;
    fChmFileName: String;
  protected
    procedure SetHelpServerEXE(AValue: String);
    procedure SetHelpServerName(AValue: String);
  public
    constructor Create;
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
    property HelpServerEXE: String read fHelpServerEXE write SetHelpServerEXE;
    property HelpServerName: String read fHelpServerName write SetHelpServerName;
    property ChmFileName: String read fChmFileName write fChmFileName;

  end;
  
  procedure Register;

implementation

{ TChmHelpViewer }

procedure TChmHelpViewer.SetHelpServerEXE(AValue: String);
begin
  fHelpServerExe := AValue;
end;

procedure TChmHelpViewer.SetHelpServerName(AValue: String);
begin
 fHelpServerName := AValue;
end;

constructor TChmHelpViewer.Create;
begin
  inherited Create;
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
begin
  Result:=shrNone;
  if not FileExists(fHelpServerEXE) then begin
    ErrMsg := 'The program "' + fHelpServerEXE + '" doesn''t seem to exist!';
    Exit(shrViewerNotFound);
  end;
  fHelpConnection.StartHelpServer(fHelpServerName, fHelpServerExe);
  fHelpConnection.OpenURL(fChmFileName, Copy(Node.URL, 1, Pos(':',Node.URL)-1));
  Result := shrSuccess;
  //WriteLn('LOADING URL = ', Node.URL);
end;

procedure TChmHelpViewer.Assign(Source: TPersistent);
var
  Viewer: TChmHelpViewer;
begin
  if Source is TChmHelpViewer then begin
    Viewer:=TChmHelpViewer(Source);
    HelpServerEXE:=Viewer.HelpServerEXE;
    HelpServerName:=Viewer.HelpServerName;
  end;
  inherited Assign(Source);
end;

procedure TChmHelpViewer.Load(Storage: TConfigStorage);
begin
  HelpServerEXE:=Storage.GetValue('CHMHelp/Exe','');
  HelpServerName:=Storage.GetValue('CHMHelp/Name','lazhelp');
  ChmFileNAme := Storage.GetValue('CHMHelp/ChmFileName','');
end;

procedure TChmHelpViewer.Save(Storage: TConfigStorage);
begin
  Storage.SetDeleteValue('CHMHelp/Exe',HelpServerEXE,'');
  Storage.SetDeleteValue('CHMHelp/Name',HelpServerName,'lazhelp');
  Storage.SetDeleteValue('CHMHelp/ChmFileName',ChmFileName,'');
end;

function TChmHelpViewer.GetLocalizedName: string;
begin
  Result := 'CHM Help Viewer';
end;

procedure Register;
var
  ChmHelp: TChmHelpViewer;
begin
  ChmHelp := TChmHelpViewer.Create;
  HelpViewers.RegisterViewer(ChmHelp);
end;

initialization
  RegisterPropertyEditor(TypeInfo(AnsiString),
    TCHmHelpViewer,'HelpServerEXE',TFileNamePropertyEditor);
  RegisterPropertyEditor(TypeInfo(AnsiString),
    TCHmHelpViewer,'ChmFileName',TFileNamePropertyEditor);
end.

