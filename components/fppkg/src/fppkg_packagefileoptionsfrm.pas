unit fppkg_packagefileoptionsfrm;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  TypInfo,
  fpmkunit,
  Laz2_XMLCfg,
  LazFileUtils,
  Forms,
  Controls,
  StdCtrls,
  ExtCtrls,
  CheckLst,
  PackageIntf,
  IDEOptEditorIntf,
  IDEOptionsIntf,
  fppkg_const;

type

  { TFppkgPackageFileIDEOptions }

  TFppkgPackageFileIDEOptions = class(TAbstractPackageFileIDEOptions)
  private
    FIsParsed: Boolean;
    FPackageFile: TLazPackageFile;
    FLazPackage: TIDEPackage;
    FAvailableOnAllTargetCPUs: Boolean;
    FAvailableOnAllTargetOSes: Boolean;
    FAvailableOnTargetOSes: TOSes;
    FAvailableOnTargetCPUs: TCPUS;
  protected
    function GetPackageFile: TLazPackageFile; override;
    function GetPackage: TIDEPackage; override;
  public
    constructor Create(APackage: TIDEPackage; APackageFile: TLazPackageFile); override;
    class function GetInstance(APackage: TIDEPackage; AFile: TLazPackageFile): TAbstractIDEOptions; overload; override;
    class function GetGroupCaption: string; override;

    // Parse the options from the CustomOptions of the package-file
    procedure ParseOptions;

    // These are called when the options are read/written to the GUI for the package-options
    procedure DoBeforeRead; override;
    procedure DoAfterWrite(Restore: boolean); override;

    property AvailableOnAllTargetCPUs: Boolean read FAvailableOnAllTargetCPUs write FAvailableOnAllTargetCPUs;
    property AvailableOnAllTargetOSes: Boolean read FAvailableOnAllTargetOSes write FAvailableOnAllTargetOSes;
    property AvailableOnTargetOSes: TOSes read FAvailableOnTargetOSes write FAvailableOnTargetOSes;
    property AvailableOnTargetCPUs: TCPUS read FAvailableOnTargetCPUs write FAvailableOnTargetCPUs;
  end;


  { TFppkgPackageFileOptionsFrm }

  TFppkgPackageFileOptionsFrm = class(TAbstractIDEOptionsEditor)
    FileTargetOSPanel: TPanel;
    FileTargetOSCheckList: TCheckListBox;
    FileTargetOSComboBox: TComboBox;
    FileTargetCPUPanel: TPanel;
    FileTargetCPUCheckList: TCheckListBox;
    FileTargetCPUComboBox: TComboBox;
    procedure FileTargetOSCheckListClickCheck(Sender: TObject);
    procedure FileTargetCPUCheckListClickCheck(Sender: TObject);
    procedure FileTargetOSComboBoxChange(Sender: TObject);
    procedure FileTargetCPUComboBoxChange(Sender: TObject);
  public
    procedure Setup(ADialog: TAbstractOptionsEditorDialog); override;
    procedure ReadSettings(AOptions: TAbstractIDEOptions); override;
    procedure WriteSettings(AOptions: TAbstractIDEOptions); override;

    class function SupportedOptionsClass: TAbstractIDEOptionsClass; override;
    function GetTitle: String; override;
  end;

implementation

{$R *.lfm}

{ TFppkgPackageFileIDEOptions }

constructor TFppkgPackageFileIDEOptions.Create(APackage: TIDEPackage; APackageFile: TLazPackageFile);
begin
  FLazPackage := APackage as TIDEPackage;
  FPackageFile := APackageFile;
end;

class function TFppkgPackageFileIDEOptions.GetGroupCaption: string;
begin
  Result := rsPackageFileOptionsTitle;
end;

class function TFppkgPackageFileIDEOptions.GetInstance(APackage: TIDEPackage; AFile: TLazPackageFile): TAbstractIDEOptions;
begin
  Result := AFile.GetOptionsInstanceOf(TFppkgPackageFileIDEOptions);
end;

function TFppkgPackageFileIDEOptions.GetPackage: TIDEPackage;
begin
  Result := FLazPackage;
end;

function TFppkgPackageFileIDEOptions.GetPackageFile: TLazPackageFile;
begin
  Result := FPackageFile;
end;

Const
  SetDelim = ['[',']',',',' '];

Function GetNextElement(Var S : String) : String;
Var
  J : Integer;
begin
  J:=1;
  Result:='';
  If Length(S)>0 then
    begin
      While (J<=Length(S)) and Not (S[j] in SetDelim) do
        Inc(j);
      Result:=Copy(S,1,j-1);
      Delete(S,1,j);
    end;
end;

Function StringToOSes(S : String) : TOSes;
Var
  T : String;
  I : Integer;
  PTI : PTypeInfo;

begin
  Result:=[];
  PTI:=TypeInfo(TOS);
  I:=1;
  If Length(S)>0 then
    begin
      While (I<=Length(S)) and (S[i] in SetDelim) do
        Inc(I);
      Delete(S,1,i-1);
    end;
  While (S<>'') do
    begin
      T:=GetNextElement(S);
      if T<>'' then
        begin
          I:=GetEnumValue(PTI,T);
          if (I>-1) then
            Result:=Result + [TOS(I)];
        end;
    end;
end;

procedure TFppkgPackageFileIDEOptions.DoBeforeRead;
begin
  ParseOptions;
  inherited DoBeforeRead;
end;

procedure TFppkgPackageFileIDEOptions.DoAfterWrite(Restore: boolean);
begin
  inherited DoAfterWrite(Restore);
  PackageFile.CustomOptions.SetDeleteValue('FPMake/AllOSes', AvailableOnAllTargetOSes, True);
  PackageFile.CustomOptions.SetDeleteValue('FPMake/AllCPUs', AvailableOnAllTargetCPUs, True);
  PackageFile.CustomOptions.SetDeleteValue('FPMake/TargetOSes', OSesToString(AvailableOnTargetOSes), '');
  PackageFile.CustomOptions.SetDeleteValue('FPMake/TargetCPUs',  CPUSToString(AvailableOnTargetCPUs), '');
end;

procedure TFppkgPackageFileIDEOptions.ParseOptions;
begin
  if not FIsParsed then
    begin
    AvailableOnAllTargetOSes := PackageFile.CustomOptions.GetValue('FPMake/AllOSes', True);
    AvailableOnAllTargetCPUs := PackageFile.CustomOptions.GetValue('FPMake/AllCPUs', True);
    AvailableOnTargetOSes := StringToOSes(PackageFile.CustomOptions.GetValue('FPMake/TargetOSes', ''));
    AvailableOnTargetCPUs := StringToCPUS(PackageFile.CustomOptions.GetValue('FPMake/TargetCPUs', ''));
    FIsParsed := True;
    end;
end;

{ TFppkgPackageFileOptionsFrm }

class function TFppkgPackageFileOptionsFrm.SupportedOptionsClass: TAbstractIDEOptionsClass;
begin
  result := TFppkgPackageFileIDEOptions;
end;

function TFppkgPackageFileOptionsFrm.GetTitle: String;
begin
  Result := rsPackageFileOptionsTitle;
end;

procedure TFppkgPackageFileOptionsFrm.Setup(ADialog: TAbstractOptionsEditorDialog);
var
  OS: TOS;
  CPU: TCpu;
begin
  FileTargetOSCheckList.Items.Clear;
  for OS := Succ(Low(OS)) to High(OS) do begin
    FileTargetOSCheckList.Items.add(OSToString(OS));
  end;
  FileTargetCPUCheckList.Items.Clear;
  for CPU := Succ(Low(CPU)) to High(CPU) do begin
    FileTargetCPUCheckList.Items.add(CPUToString(CPU));
  end;
end;

procedure TFppkgPackageFileOptionsFrm.ReadSettings(AOptions: TAbstractIDEOptions);
var
  FileSettings: TFppkgPackageFileIDEOptions;
  OS: TOS;
  CPU: TCpu;
begin
  if not Assigned(AOptions) then
    Exit;
  FileSettings := AOptions as TFppkgPackageFileIDEOptions;
  // Supported FPMake CPU's and OS'es
  if FileSettings.AvailableOnAllTargetOSes then
    FileTargetOSComboBox.ItemIndex := 0
  else
    FileTargetOSComboBox.ItemIndex := 1;

  if FileSettings.AvailableOnAllTargetCPUs then
    FileTargetCPUComboBox.ItemIndex := 0
  else
    FileTargetCPUComboBox.ItemIndex := 1;

  for OS := Succ(Low(OS)) to High(OS) do
    FileTargetOSCheckList.Checked[Ord(OS) -1] := (OS in FileSettings.AvailableOnTargetOSes);
  for CPU := Succ(Low(CPU)) to High(CPU) do
    FileTargetCPUCheckList.Checked[Ord(CPU) -1] := (CPU in FileSettings.AvailableOnTargetCPUs);
end;

procedure TFppkgPackageFileOptionsFrm.WriteSettings(AOptions: TAbstractIDEOptions);
var
  OS: TOS;
  CPU: TCPU;
  FileSettings: TFppkgPackageFileIDEOptions;
begin
  if not Assigned(AOptions) then
    Exit;
  FileSettings := AOptions as TFppkgPackageFileIDEOptions;
  FileSettings.AvailableOnTargetOSes := [];
  for OS := Succ(Low(OS)) to High(OS) do
    if FileTargetOSCheckList.Checked[Ord(OS) -1] then
      FileSettings.AvailableOnTargetOSes := FileSettings.AvailableOnTargetOSes + [OS];

  FileSettings.AvailableOnTargetCPUs := [];
  for CPU := Succ(Low(CPU)) to High(CPU) do
    if FileTargetCPUCheckList.Checked[Ord(CPU) -1] then
      FileSettings.AvailableOnTargetCPUs := FileSettings.AvailableOnTargetCPUs + [CPU];

  FileSettings.AvailableOnAllTargetCPUs := (FileTargetCPUComboBox.ItemIndex=0);
  FileSettings.AvailableOnAllTargetOSes := (FileTargetOSComboBox.ItemIndex=0);
end;

procedure TFppkgPackageFileOptionsFrm.FileTargetOSCheckListClickCheck(Sender: TObject);
begin
  DoOnChange;
end;

procedure TFppkgPackageFileOptionsFrm.FileTargetCPUCheckListClickCheck(Sender: TObject);
begin
  DoOnChange;
end;

procedure TFppkgPackageFileOptionsFrm.FileTargetOSComboBoxChange(Sender: TObject);
begin
  DoOnChange;
end;

procedure TFppkgPackageFileOptionsFrm.FileTargetCPUComboBoxChange(Sender: TObject);
begin
  DoOnChange;
end;

var
  OptionsGroup: Integer;

initialization
  OptionsGroup := GetFreeIDEOptionsGroupIndex(GroupPackageFile);
  RegisterIDEOptionsGroup(OptionsGroup, TFppkgPackageFileIDEOptions);
  RegisterIDEOptionsEditor(OptionsGroup, TFppkgPackageFileOptionsFrm, 1);
end.

