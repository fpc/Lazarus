{  $Id$  }
{
 *****************************************************************************
 *                                                                           *
 *  This file is part of the Lazarus Component Library (LCL)                 *
 *                                                                           *
 *  See the file COPYING.LCL, included in this distribution,                 *
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
  Classes, SysUtils, Forms, PropertyStorage, XMLCfg, DOM;

type
  { TXMLPropStorage }

  TPropStorageXMLConfig = class(TXMLConfig)
  Public
    Procedure DeleteSubNodes (const ARootNode : String);
  end;
  
  TXMLPropStorage = class(TCustomPropertyStorage)
  private
    FCount : Integer;
    FFileName: String;
    FXML: TPropStorageXMLConfig;
    FRootNode: String;
    FRootNodePath: String;
  protected
    procedure StorageNeeded(ReadOnly: Boolean);override;
    procedure FreeStorage; override;
    Function GetXMLFileName : string; virtual;
    Function RootSection : String; Override;
    Function FixPath(const APath : String) : String; virtual;
    Property XMLConfig: TPropStorageXMLConfig Read FXML;
  Public
    function  DoReadString(const Section, Ident, Default: string): string; override;
    procedure DoWriteString(const Section, Ident, Value: string); override;
    Procedure DoEraseSections(const ARootSection: String);override;
  Published
    Property FileName : String Read FFileName Write FFileName;
    Property RootNodePath : String Read FRootNode Write FRootNodePath;
  end;

procedure Register;


implementation


procedure Register;
begin
  RegisterComponents('Misc',[TXMLPropStorage]);
end;

{ TXMLPropStorage }

procedure TXMLPropStorage.StorageNeeded(ReadOnly: Boolean);
begin
  If (FXML=Nil) then
    FXML:=TPropStorageXMLConfig.Create(GetXMLFileName);
  Inc(FCount);
end;

procedure TXMLPropStorage.FreeStorage;
begin
  Dec(FCount);
  If (FCount<=0) then
    begin
    FCount:=0;
    FreeAndNil(FXML);
    end;
end;

function TXMLPropStorage.GetXMLFileName: string;
begin
  if (FFileName<>'') then
    Result:=FFIleName
  else
{$ifdef unix}
    Result:=IncludeTrailingPathDelimiter(GetEnvironmentVariable('HOME'))
            +'.'+ExtractFileName(Application.ExeName);

{$else}
    Result:=ChangeFileExt(Application.ExeName,'.xml');
{$endif}
end;

function TXMLPropStorage.FixPath(const APath : String) : String;

begin
  Result:=StringReplace(APath,'.','/',[rfReplaceAll]);
end;

function TXMLPropStorage.RootSection: String;
begin
  If (FRootNode<>'') then
    Result:=FRootNode
  else
    Result:=inherited RootSection;
  Result:=FixPath(Result);
end;

function TXMLPropStorage.DoReadString(const Section, Ident, Default: string
  ): string;
begin
  Result:=FXML.GetValue(FixPath(Section)+'/'+Ident, Default);
end;

procedure TXMLPropStorage.DoWriteString(const Section, Ident, Value: string);
begin
  FXML.SetValue(FixPath(Section)+'/'+Ident, Value);
end;

procedure TXMLPropStorage.DoEraseSections(const ARootSection: String);
begin
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
  If Assigned(Node) then
    Node.Free;
end;

end.
