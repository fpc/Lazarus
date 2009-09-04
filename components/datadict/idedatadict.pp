unit idedatadict;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, fpdatadict, ProjectIntf, LazIDEIntf, ldd_consts;

Type

  { TIDEDataDictionary }

  { TDDFile }

  TDDFile = Class(TObject)
    FFileName : String;
    Constructor Create(AFileName : String);
  end;

  TIDEDataDictionary = Class(TComponent)
  private
    FDataDict: TFPDatadictionary;
    FDictName: String;
    FFileName: String;
    FKnownDicts : TStringList;
    procedure DDProgress(Sender: TObject; const Msg: String);
    procedure SetDictName(const AValue: String);
    procedure SetFileName(const AValue: String);
  Protected
    function GetActive: Boolean;
    Procedure SetDictNameAndFile(Const AName,AFileName : String); virtual;
    procedure SetActive(const AValue: Boolean);
    Procedure Initialize; virtual;
  Public
    Constructor Create(AOwner : TComponent); override;
    Procedure Update;
    Procedure Load;
    procedure Save;
    Procedure GetKnownDictionaries(List : TStrings);
    Function GetDictionaryFileName(ADict : String) : String;
    Property Dictionary :  TFPDatadictionary Read FDataDict;
    Property FileName :  String Read FFileName Write SetFileName;
    Property DictionaryName : String Read FDictName Write SetDictName;
    Property Active : Boolean Read GetActive Write SetActive;
  end;

Function IDEDatadictionary : TIDEDataDictionary;
Procedure InitDDSettings;
procedure SaveDDSettings;

Var
  DataDesktopBinary : String;
  DefaultDictionaryDir : String;
  
implementation

uses inifiles,BaseIDEIntf,idemsgintf;

Const
  // Do not localize
  DDXML      = 'datadict.xml';
  KeyDesktop = 'DatabaseDesktop';
  KeyDir     = 'DictionaryDir';

Var
  Inited : Boolean;

Procedure InitDDSettings;

begin
  If Inited then
    Exit;
  With GetIDEConfigStorage(DDXML,True) do
    try
      DataDesktopBinary:=GetValue(KeyDesktop,'');
      DefaultDictionaryDir:=GetValue(KeyDir,'');
    Finally
      Free;
    end;
  Inited:=true;
end;

procedure SaveDDSettings;

begin
  With GetIDEConfigStorage(DDXML,False) do
    try
      SetValue(KeyDesktop,DataDesktopBinary);
      SetValue(KeyDir,DefaultDictionaryDir);
      WriteToDisk;
    Finally
      Free;
    end;
end;


{ ---------------------------------------------------------------------
  Application name magic
  ---------------------------------------------------------------------}
Var
  OldAppName : TGetAppNameEvent;

Function FakeName : String;

begin
  Result:='Lazarus Data Desktop';
end;

Procedure OverrideAppName;

begin
  OldAppName:=OnGetApplicationName;
  OnGetApplicationName:=@FakeName;
end;

Procedure RestoreAppName;

begin
  OnGetApplicationName:=OldAppName;
end;

{ ---------------------------------------------------------------------
  Public functions
  ---------------------------------------------------------------------}

Var
  TheDataDict : TIDEDataDictionary;
  
Function IDEDatadictionary : TIDEDataDictionary;

begin
  If Not Assigned(TheDataDict) then
    TheDataDict:=TIDEDataDictionary.Create(nil);
  TheDataDict.Update;
  Result:=TheDataDict;
end;


{ ---------------------------------------------------------------------
  TIDEDataDictionary
  ---------------------------------------------------------------------}

procedure TIDEDataDictionary.SetDictName(const AValue: String);
begin
  if (FDictName<>AValue) then
    SetDictNameAndFile(AValue,GetDictionaryFileName(AValue));
end;

function TIDEDataDictionary.GetActive: Boolean;
begin
  Result:=FDataDict<>Nil;
end;

procedure TIDEDataDictionary.SetDictNameAndFile(const AName, AFileName: String
  );
begin
  FDictName:=AName;
  FFileName:=AFileName;
  If Active then
    FDataDict.LoadFromFile(UTF8ToSys(FFileName));
end;

procedure TIDEDataDictionary.SetActive(const AValue: Boolean);
begin
  If (AValue<>GetActive) then
    If AValue then
      begin
      FDataDict:=TFPDataDictionary.Create;
      FDataDict.OnProgress:=@DDProgress;
      Load;
      end
    else
      FreeAndNil(FDataDict);
end;

procedure TIDEDataDictionary.DDProgress(Sender : TObject; Const Msg : String);

begin
  IDEMessagesWindow.AddMsg(SLoadingDataDict+Msg,'',2);
end;

procedure TIDEDataDictionary.SetFileName(const AValue: String);
begin
  If (AValue<>FFileName) or (FDictName<>'') then
    SetDictnameAndFile('',AValue);
end;

procedure TIDEDataDictionary.Initialize;

Var
  FN,DN : String;
  Ini : TMemInifile;
  I,Count : Integer;

begin
  OverrideAppName;
  try
    FN:=GetAppConfigFile(False);
  finally
    RestoreAppName;
  end;
  Ini:=TMemIniFile.Create(FN);
  try
    FKnownDicts.Clear;
    Count:=Ini.ReadInteger('RecentDicts','Count',0);
    For I:=0 to Count do
      begin
      DN:=Ini.ReadString('RecentDicts','DataDict'+InttoStr(I),'');
      If (DN<>'') then
        FKnownDicts.AddObject(DN,TDDFile.Create(Ini.ReadString('DD_'+DN,'FileName','')));
      end;
  finally
    Ini.Free;
  end;
end;

constructor TIDEDataDictionary.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FKnownDicts:=TStringList.Create;
  FKnownDicts.Sorted:=True;
  Initialize;
end;

procedure TIDEDataDictionary.Update;

Var
  ProjActive : Boolean;
  ProjFileName : String;
  ProjDD : String;
  NeedReload : Boolean;

begin
  If Not(Assigned(LazarusIDE) and Assigned(LazarusIDE.ActiveProject)) then
    begin
    Active:=False;
    FileName:='';
    Exit;
    end;
  With LazarusIDE.ActiveProject do
    begin
    ProjDD:=Customdata.Values['DataDict/KnownDict'];
    ProjFileName:=Customdata.Values['DataDict/FileName'];
    ProjActive:=(ProjDD<>'') or (ProjFileName<>'');
    If ProjActive then
      ProjActive:=StrToIntDef(Customdata.Values['DataDict/Active'],ord(ProjActive))<>0;
    end;
  If (Active<>ProjActive) or (ProjDD<>DictionaryName) or (FileName<>ProjFileName) then
    begin
    Active:=ProjActive;
    If (ProjDD<>'') then
      DictionaryName:=ProjDD
    else
      FileName:=ProjFileName;
    end;
end;


procedure TIDEDataDictionary.Save;

begin
  If Not(Assigned(LazarusIDE) and Assigned(LazarusIDE.ActiveProject)) then
    Exit;
  With LazarusIDE.ActiveProject do
    begin
    Customdata.Values['DataDict/FileName']:=FileName;
    Customdata.Values['DataDict/KnownDict']:=DictionaryName;
    Customdata.Values['DataDict/Active']:=IntToStr(Ord(Active));
    end;
end;


procedure TIDEDataDictionary.Load;
begin
  If (FFileName<>'') and Active then
    begin
    IDEMessagesWindow.BeginBlock(False);
    Try
      IDEMessagesWindow.AddMsg(SLoadingDataDict+SFromfile+FFileName,'',2);
      FDataDict.LoadFromFile(UTF8ToSys(FFileName));
    Finally
      IDEMessagesWindow.EndBlock;
    end;
    end;
    
end;



procedure TIDEDataDictionary.GetKnownDictionaries(List: TStrings);

begin
  List.Clear;
  List.AddStrings(FKnownDicts);
end;

function TIDEDataDictionary.GetDictionaryFileName(ADict: String): String;

Var
  I : Integer;

begin
  I:=FKnownDicts.IndexOf(ADict);
  If (I<>-1) then
    Result:=(FKnownDicts.Objects[i] as TDDFile).FFileName;

end;

{ TDDFile }

constructor TDDFile.Create(AFileName: String);
begin
  FFileName:=AFileName;
end;

end.

