unit AdvHistoryList;

{$mode objfpc}{$H+}

{$I ide.inc}

interface

uses
  Classes, SysUtils, AvgLvlTree, InputHistory, Laz2_XMLCfg, IDEProcs, StdCtrls;

Const
  hlCmdLineParamsHistoryList = 'CmdLineParamsHistoryList';
  hlLaunchingApplicationHistoryList = 'LaunchingApplicationHistoryList';

Type

  { TAdvHistoryList }

  TAdvHistoryList = class(THistoryList)
    private
      FSelected : String;
    protected
      FSaveSelected : Boolean;
      procedure AssignValues(Source : TPersistent);override;
    public
      constructor Create(TheListType: TRecentListType);override;
      procedure LoadFromXMLConfig(XMLConfig: TXMLConfig; const Path: string);override;
      procedure SaveToXMLConfig(XMLConfig: TXMLConfig; const Path: string);override;
      procedure AdvLoadFromXMLConfig(XMLConfig: TXMLConfig; const Path: string);virtual;
      procedure AdvSaveToXMLConfig(XMLConfig: TXMLConfig; const Path: string);virtual;
      procedure Clear; override;
      procedure SetComboBox(AComboBox : TComboBox);
      class function CreateMe(ATheListType : TRecentListType; AName : String) : THistoryList;override;
      procedure Assign(Source: TPersistent); override;
      procedure AssignKeyValue(Source : TPersistent);virtual;

      property Selected : String read FSelected write FSelected;
  end;


implementation

{ TAdvHistoryListCreator }

class function TAdvHistoryList.CreateMe(ATheListType : TRecentListType; AName : String) : THistoryList;
Var
  AAdvHistoryList : TAdvHistoryList;
begin
  AAdvHistoryList := TAdvHistoryList.Create(ATheListType);
  AAdvHistoryList.Name := AName;
  AAdvHistoryList.NameValueSeparator := #0;
  Result := AAdvHistoryList;
end;

constructor TAdvHistoryList.Create(TheListType: TRecentListType);
begin
  inherited;
  FSelected := '';
end;

procedure TAdvHistoryList.Assign(Source: TPersistent);
begin
  inherited;
  If Source is TAdvHistoryList Then
    If FSaveSelected Then
      Self.FSelected := TAdvHistoryList(Source).FSelected;
end;

procedure TAdvHistoryList.AssignKeyValue(Source : TPersistent);
Var
  I : Integer;
  AName, AValue : String;
Begin
  If Source is TAdvHistoryList Then Begin
    Self.NameValueSeparator := TAdvHistoryList(Source).NameValueSeparator;
    Self.AssignValues(Source);
    For I := 0 To TAdvHistoryList(Source).Count - 1 Do Begin
      TAdvHistoryList(Source).GetNameValue(I, AName, AValue);
      Self.Values[AName] := AValue;
    end;
  end;
end;

procedure TAdvHistoryList.AssignValues(Source : TPersistent);
Begin
  inherited;
  If Source is TAdvHistoryList Then
    If FSaveSelected Then
      Self.FSelected := TAdvHistoryList(Source).FSelected;
end;

procedure TAdvHistoryList.AdvLoadFromXMLConfig(XMLConfig: TXMLConfig; const Path: string);
begin
  FSaveSelected := True;
  Self.LoadFromXMLConfig(XMLConfig, Path);
  FSaveSelected := False;
end;

procedure TAdvHistoryList.AdvSaveToXMLConfig(XMLConfig: TXMLConfig; const Path: string);
begin
  FSaveSelected := True;
  Self.SaveToXMLConfig(XMLConfig, Path);
  FSaveSelected := False;
end;

procedure TAdvHistoryList.LoadFromXMLConfig(XMLConfig: TXMLConfig; const Path: string);
Var
  AStringToStringTree : TStringToStringTree;
  APStringToStringItem : PStringToStringItem;
begin
  AStringToStringTree := TStringToStringTree.Create(false);
  if Name='' then
    Name:=XMLConfig.GetValue(Path+'Name','');
  MaxCount:=XMLConfig.GetValue(Path+'MaxCount',MaxCount);
  FListType:=StrToRecentListType(XMLConfig.GetValue(Path+'Type',''));

  If FSaveSelected Then
    FSelected := XMLConfig.GetValue(Path+'Selected','');

  LoadStringToStringTree(XMLConfig, AStringToStringTree, Path);

  For APStringToStringItem in AStringToStringTree do
    Self.Values[APStringToStringItem^.Name] := APStringToStringItem^.Value;

  CleanUpRecentList(Self,ListType);

  FreeAndNil(AStringToStringTree);
end;

procedure TAdvHistoryList.SaveToXMLConfig(XMLConfig: TXMLConfig; const Path: string);
Var
  AStringToStringTree : TStringToStringTree;
begin
  AStringToStringTree := TStringToStringTree.Create(false);

  XMLConfig.SetDeleteValue(Path+'Name',Name,'');
  XMLConfig.SetDeleteValue(Path+'Type',RecentListTypeNames[ListType],
                           RecentListTypeNames[rltCaseSensitive]);
  XMLConfig.SetDeleteValue(Path+'MaxCount',MaxCount,20);

  If FSaveSelected Then
    XMLConfig.SetDeleteValue(Path+'Selected', FSelected, '');

  AStringToStringTree.AddNameValues(Self);

  SaveStringToStringTree(XMLConfig, AStringToStringTree, Path);

  FreeAndNil(AStringToStringTree);
end;

procedure TAdvHistoryList.Clear;
begin
  inherited;
  FSelected := '';
end;

procedure TAdvHistoryList.SetComboBox(AComboBox : TComboBox);
Var
  I, AIndex : Integer;
  AValue : String;
Begin
  AComboBox.Items.Clear;
  For I := 0 To Self.Count - 1 Do Begin
    AValue := Self.Names[I];
    AIndex := AComboBox.Items.IndexOf(AValue);
    If AIndex = -1 Then
      If AValue <> '' Then
        AComboBox.Items.Add(AValue);
  end;
end;

initialization
  THistoryLists.RegisterHistoryListClass(TAdvHistoryList, hlCmdLineParamsHistoryList);
  THistoryLists.RegisterHistoryListClass(TAdvHistoryList, hlLaunchingApplicationHistoryList);

end.

