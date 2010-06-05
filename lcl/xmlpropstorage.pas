{  $Id$  }
{
 *****************************************************************************
 *                                                                           *
 *  This file is part of the Lazarus Component Library (LCL)                 *
 *                                                                           *
 *  See the file COPYING.modifiedLGPL.txt, included in this distribution,    *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************
}
unit XMLPropStorage;

{$mode objfpc}{$H+}

interface


uses
  Classes, SysUtils, FileUtil, LCLProc, Forms, PropertyStorage, XMLCfg, DOM,
  LazConfigStorage;

type
  { TPropStorageXMLConfig }

  TPropStorageXMLConfig = class(TXMLConfig)
  Public
    procedure DeleteSubNodes (const ARootNode: String);
  end;
  
  { TCustomXMLPropStorage }

  TCustomXMLPropStorage = class(TFormPropertyStorage)
  private
    FCount: Integer;
    FFileName: String;
    FXML: TPropStorageXMLConfig;
    FRootNodePath: String;
  protected
    function GetXMLFileName: string; virtual;
    function RootSection: String; Override;
    function FixPath(const APath: String): String; virtual;
    Property XMLConfig: TPropStorageXMLConfig Read FXML;
  public
    procedure StorageNeeded(ReadOnly: Boolean);override;
    procedure FreeStorage; override;
    function  DoReadString(const Section, Ident, TheDefault: string): string; override;
    procedure DoWriteString(const Section, Ident, Value: string); override;
    procedure DoEraseSections(const ARootSection: String);override;
  public
    property FileName: String Read FFileName Write FFileName;
    property RootNodePath: String Read FRootNodePath Write FRootNodePath;
  end;
  
  { TXMLPropStorage }

  TXMLPropStorage = class(TCustomXMLPropStorage)
  Published
    property StoredValues;
    property FileName;
    property RootNodePath;
    property Active;
    property OnSavingProperties;
    property OnSaveProperties;
    property OnRestoringProperties;
    property OnRestoreProperties;
  end;
  
  { TXMLConfigStorage }

  TXMLConfigStorage = class(TConfigStorage)
  private
    FFreeXMLConfig: boolean;
    FXMLConfig: TXMLConfig;
  protected
    function  GetFullPathValue(const APath, ADefault: String): String; override;
    function  GetFullPathValue(const APath: String; ADefault: Integer): Integer; override;
    function  GetFullPathValue(const APath: String; ADefault: Boolean): Boolean; override;
    procedure SetFullPathValue(const APath, AValue: String); override;
    procedure SetDeleteFullPathValue(const APath, AValue, DefValue: String); override;
    procedure SetFullPathValue(const APath: String; AValue: Integer); override;
    procedure SetDeleteFullPathValue(const APath: String; AValue, DefValue: Integer); override;
    procedure SetFullPathValue(const APath: String; AValue: Boolean); override;
    procedure SetDeleteFullPathValue(const APath: String; AValue, DefValue: Boolean); override;
    procedure DeleteFullPath(const APath: string); override;
    procedure DeleteFullPathValue(const APath: string); override;
  public
    procedure Clear; override;
    constructor Create(const Filename: string; LoadFromDisk: Boolean); override;
    constructor Create(TheXMLConfig: TXMLConfig);
    constructor Create(TheXMLConfig: TXMLConfig; const StartPath: string);
    destructor Destroy; override;
    property XMLConfig: TXMLConfig read FXMLConfig;
    property FreeXMLConfig: boolean read FFreeXMLConfig write FFreeXMLConfig;
    procedure WriteToDisk; override;
    function GetFilename: string; override;
  end;

procedure Register;


implementation


procedure Register;
begin
  RegisterComponents('Misc',[TXMLPropStorage]);
end;

{ TCustomXMLPropStorage }

procedure TCustomXMLPropStorage.StorageNeeded(ReadOnly: Boolean);
begin
  If (FXML=Nil) then begin
    FXML:=TPropStorageXMLConfig.Create(nil);
    FXML.FileName := GetXMLFileName;
  end;
  Inc(FCount);
  //debugln('TCustomXMLPropStorage.StorageNeeded ',dbgsname(FXML),' ',dbgs(FXML),' FCount=',dbgs(FCount));
end;

procedure TCustomXMLPropStorage.FreeStorage;
begin
  Dec(FCount);
  //debugln('TCustomXMLPropStorage.FreeStorage ',dbgsname(FXML),' ',dbgs(FXML),' FCount=',dbgs(FCount));
  If (FCount<=0) then
    begin
    FCount:=0;
    FreeAndNil(FXML);
    end;
end;

function TCustomXMLPropStorage.GetXMLFileName: string;
begin
  if (FFileName<>'') then
    Result:=FFIleName
  else
    {$ifdef unix}
    Result:=IncludeTrailingPathDelimiter(GetEnvironmentVariableUTF8('HOME'))
            +'.'+ExtractFileName(Application.ExeName);

    {$else}
    Result:=ChangeFileExt(Application.ExeName,'.xml');
    {$endif}
  //debugln('TCustomXMLPropStorage.GetXMLFileName "',Result,'"');
end;

function TCustomXMLPropStorage.FixPath(const APath: String): String;
begin
  Result:=StringReplace(APath,'.','/',[rfReplaceAll]);
end;

function TCustomXMLPropStorage.RootSection: String;
begin
  If (FRootNodePath<>'') then
    Result:=FRootNodePath
  else
    Result:=inherited RootSection;
  Result:=FixPath(Result);
end;

function TCustomXMLPropStorage.DoReadString(const Section, Ident,
  TheDefault: string): string;
begin
  Result:=FXML.GetValue(FixPath(Section)+'/'+Ident, TheDefault);
  //debugln('TCustomXMLPropStorage.DoReadString Section="',Section,'" Ident="',Ident,'" Result=',Result);
end;

procedure TCustomXMLPropStorage.DoWriteString(const Section, Ident,
  Value: string);
begin
  //debugln('TCustomXMLPropStorage.DoWriteString Section="',Section,'" Ident="',Ident,'" Value="',Value,'"');
  FXML.SetValue(FixPath(Section)+'/'+Ident, Value);
end;

procedure TCustomXMLPropStorage.DoEraseSections(const ARootSection: String);
begin
  //debugln('TCustomXMLPropStorage.DoEraseSections ARootSection="',ARootSection,'"');
  FXML.DeleteSubNodes(FixPath(ARootSection));
end;

{ TPropStorageXMLConfig }

procedure TPropStorageXMLConfig.DeleteSubNodes(const ARootNode: String);
var
  Node, Child: TDOMNode;
  i: Integer;
  NodePath: String;
begin
  Node := doc.DocumentElement;
  NodePath := ARootNode;
  while (Length(NodePath)>0) and (Node<>Nil) do
    begin
    i := Pos('/', NodePath);
    if i = 0 then
      I:=Length(NodePath)+1;
    Child := Node.FindNode(Copy(NodePath,1,i - 1));
    System.Delete(NodePath,1,I);
    Node := Child;
    end;
  If Assigned(Node) then begin
    //debugln('TPropStorageXMLConfig.DeleteSubNodes ',ARootNode);
    Node.Free;
  end;
end;

{ TXMLConfigStorage }

function TXMLConfigStorage.GetFullPathValue(const APath, ADefault: String
  ): String;
begin
  Result:=XMLConfig.GetValue(APath, ADefault);
end;

function TXMLConfigStorage.GetFullPathValue(const APath: String;
  ADefault: Integer): Integer;
begin
  Result:=XMLConfig.GetValue(APath, ADefault);
end;

function TXMLConfigStorage.GetFullPathValue(const APath: String;
  ADefault: Boolean): Boolean;
begin
  Result:=XMLConfig.GetValue(APath, ADefault);
end;

procedure TXMLConfigStorage.SetFullPathValue(const APath, AValue: String);
begin
  XMLConfig.SetValue(APath, AValue);
end;

procedure TXMLConfigStorage.SetDeleteFullPathValue(const APath, AValue,
  DefValue: String);
begin
  XMLConfig.SetDeleteValue(APath, AValue, DefValue);
end;

procedure TXMLConfigStorage.SetFullPathValue(const APath: String;
  AValue: Integer);
begin
  XMLConfig.SetValue(APath, AValue);
end;

procedure TXMLConfigStorage.SetDeleteFullPathValue(const APath: String;
  AValue, DefValue: Integer);
begin
  XMLConfig.SetDeleteValue(APath, AValue, DefValue);
end;

procedure TXMLConfigStorage.SetFullPathValue(const APath: String;
  AValue: Boolean);
begin
  XMLConfig.SetValue(APath, AValue);
end;

procedure TXMLConfigStorage.SetDeleteFullPathValue(const APath: String;
  AValue, DefValue: Boolean);
begin
  XMLConfig.SetDeleteValue(APath, AValue, DefValue);
end;

procedure TXMLConfigStorage.DeleteFullPath(const APath: string);
begin
  XMLConfig.DeletePath(APath);
end;

procedure TXMLConfigStorage.DeleteFullPathValue(const APath: string);
begin
  XMLConfig.DeleteValue(APath);
end;

procedure TXMLConfigStorage.Clear;
begin
  FXMLConfig.Clear;
end;

constructor TXMLConfigStorage.Create(const Filename: string;
  LoadFromDisk: Boolean);
begin
  FXMLConfig:=TXMLConfig.Create(nil);
  FXMLConfig.StartEmpty:=not LoadFromDisk;
  FXMLConfig.Filename:=Filename;
  FFreeXMLConfig:=true;
end;

constructor TXMLConfigStorage.Create(TheXMLConfig: TXMLConfig);
begin
  FXMLConfig:=TheXMLConfig;
  if FXMLConfig=nil then
    raise Exception.Create('');
end;

constructor TXMLConfigStorage.Create(TheXMLConfig: TXMLConfig;
  const StartPath: string);
begin
  Create(TheXMLConfig);
  AppendBasePath(StartPath);
end;

destructor TXMLConfigStorage.Destroy;
begin
  if FreeXMLConfig then FreeAndNil(FXMLConfig);
  inherited Destroy;
end;

procedure TXMLConfigStorage.WriteToDisk;
begin
  FXMLConfig.Flush;
end;

function TXMLConfigStorage.GetFilename: string;
begin
  Result:=FXMLConfig.Filename;
end;

end.
