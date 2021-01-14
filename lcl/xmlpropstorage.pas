{  $Id$  }
{
 *****************************************************************************
  This file is part of the Lazarus Component Library (LCL)

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************
}
unit XMLPropStorage;

{$mode objfpc}{$H+}

interface


uses
  // RTL, FCL
  Classes, SysUtils, XMLConf, DOM, XMLRead, XMLWrite,
  // LCL
  LCLProc, Forms,
  // LazUtils
  LazConfigStorage, LazUTF8;

type
  { TPropStorageXMLConfig }

  TPropStorageXMLConfig = class(TXMLConfig)
  Public
    procedure DeleteSubNodes(const ARootNode: String);
    procedure LoadFromStream(s: TStream); virtual;
    procedure SaveToStream(s: TStream); virtual;
    property XMLDoc: TXMLDocument read Doc;
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
    FFilename: string;
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
    constructor Create(s: TStream; const StartPath: string = '');
    destructor Destroy; override;
    property XMLConfig: TXMLConfig read FXMLConfig;
    property FreeXMLConfig: boolean read FFreeXMLConfig write FFreeXMLConfig;
    procedure WriteToDisk; override;
    function GetFilename: string; override;
    procedure SaveToStream(s: TStream); virtual;
  end;

procedure Register;


implementation

{$IFDEF FPC_HAS_CPSTRING}
  {$WARN IMPLICIT_STRING_CAST OFF}
  {$WARN IMPLICIT_STRING_CAST_LOSS OFF}
{$ENDIF}

procedure Register;
begin
  RegisterComponents('Misc',[TXMLPropStorage]);
end;

{ TCustomXMLPropStorage }

procedure TCustomXMLPropStorage.StorageNeeded(ReadOnly: Boolean);
begin
  If (FXML=Nil) and not (csDesigning in ComponentState) then
  begin
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
    Result:=FFileName
  else if csDesigning in ComponentState then
    raise Exception.Create('TCustomXMLPropStorage.GetXMLFileName: missing Filename')
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
var
  Res: UnicodeString;
begin
  Res:=FXML.GetValue(Utf8Decode(FixPath(Section)+'/'+Ident), Utf8Decode(TheDefault));
  Result := Utf8Encode(Res);
  //debugln('TCustomXMLPropStorage.DoReadString Section="',Section,'" Ident="',Ident,'" Result=',Result);
end;

procedure TCustomXMLPropStorage.DoWriteString(const Section, Ident,
  Value: string);
begin
  //debugln('TCustomXMLPropStorage.DoWriteString Section="',Section,'" Ident="',Ident,'" Value="',Value,'"');
  FXML.SetValue(Utf8Decode(FixPath(Section)+'/'+Ident), Utf8Decode(Value));
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
    Child := Node.FindNode(UTF8Decode(Copy(NodePath,1,i - 1)));
    System.Delete(NodePath,1,I);
    Node := Child;
    end;
  If Assigned(Node) then begin
    //debugln('TPropStorageXMLConfig.DeleteSubNodes ',ARootNode);
    Node.Free;
  end;
end;

procedure TPropStorageXMLConfig.LoadFromStream(s: TStream);
begin
  FreeAndNil(Doc);
  ReadXMLFile(Doc,s);
end;

procedure TPropStorageXMLConfig.SaveToStream(s: TStream);
begin
  WriteXMLFile(Doc,s);
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
var
  ms: TMemoryStream;
  fs: TFileStream;
begin
  FXMLConfig:=TPropStorageXMLConfig.Create(nil);
  FFilename:=Filename;
  FFreeXMLConfig:=true;
  if LoadFromDisk then
  begin
    fs:=TFileStream.Create(Filename,fmOpenRead+fmShareDenyWrite);
    try
      ms:=TMemoryStream.Create;
      try
        ms.CopyFrom(fs,fs.Size);
        ms.Position:=0;
        TPropStorageXMLConfig(FXMLConfig).LoadFromStream(ms);
      finally
        ms.Free;
      end;
    finally
      fs.Free;
    end;
  end;
end;

constructor TXMLConfigStorage.Create(TheXMLConfig: TXMLConfig);
begin
  FXMLConfig:=TheXMLConfig;
  FFilename:=FXMLConfig.Filename;
  if FXMLConfig=nil then
    raise Exception.Create('');
end;

constructor TXMLConfigStorage.Create(TheXMLConfig: TXMLConfig;
  const StartPath: string);
begin
  Create(TheXMLConfig);
  AppendBasePath(StartPath);
end;

constructor TXMLConfigStorage.Create(s: TStream; const StartPath: string);
begin
  FXMLConfig:=TPropStorageXMLConfig.Create(nil);
  FFreeXMLConfig:=true;
  TPropStorageXMLConfig(FXMLConfig).LoadFromStream(s);
  if StartPath<>'' then
    AppendBasePath(StartPath);
end;

destructor TXMLConfigStorage.Destroy;
begin
  if FreeXMLConfig then FreeAndNil(FXMLConfig);
  inherited Destroy;
end;

procedure TXMLConfigStorage.WriteToDisk;
var
  ms: TMemoryStream;
  fs: TFileStream;
begin
  if FXMLConfig is TPropStorageXMLConfig then
  begin
    ms:=TMemoryStream.Create;
    try
      TPropStorageXMLConfig(FXMLConfig).SaveToStream(ms);
      ms.Position:=0;
      fs:=TFileStream.Create(GetFilename,fmCreate);
      try
        fs.CopyFrom(ms,ms.Size);
      finally
        fs.Free;
      end;
    finally
      ms.Free;
    end;
  end else
    FXMLConfig.Flush;
end;

function TXMLConfigStorage.GetFilename: string;
begin
  Result:=FFilename;
end;

procedure TXMLConfigStorage.SaveToStream(s: TStream);
begin
  if FXMLConfig is TPropStorageXMLConfig then begin
    TPropStorageXMLConfig(FXMLConfig).SaveToStream(s);
  end else
    raise Exception.Create('TXMLConfigStorage.SaveToStream not supported for '+DbgSName(FXMLConfig));
end;

end.
