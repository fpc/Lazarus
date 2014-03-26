unit ImportTypelib;

{$mode objfpc}{$H+}

interface

{$ifndef wince}
uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, EditBtn,
  StdCtrls, ButtonPanel, ComCtrls, registry, strutils, activexstrconsts, LazUTF8,
  lazideintf, projectintf, PackageIntf;

type

  TEntry = class
  public
    path: string;
    progID: string;
    typeLib: string;
    version: string;
    Name: string;
    clsID: string;
    isControl: boolean;
  end;

  { TFrmTL }

  TFrmTL = class(TForm)
    ButtonPanel: TButtonPanel;
    CBxTLActiveX: TCheckBox;
    CBxTLPackage: TCheckBox;
    CBxTLRecurse: TCheckBox;
    FNETL: TFileNameEdit;
    GroupBox1: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    lstControls: TListBox;
    lstFiltered: TListBox;
    lstRefs: TListBox;
    PageControl1: TPageControl;
    SelectDirectoryDialog1: TSelectDirectoryDialog;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    txtSearch: TEdit;
    procedure CBxTLActiveXChange(Sender: TObject);
    procedure CBxTLPackageChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure lstControlsClick(Sender: TObject);
    procedure lstFilteredClick(Sender: TObject);
    procedure lstRefsClick(Sender: TObject);
    procedure PageControl1Change(Sender: TObject);
    procedure txtSearchChange(Sender: TObject);
  private
    procedure ListClickHandler(lst: TListBox);
  public

  end;

var
  FrmTL: TFrmTL;

procedure ImpTypeLib(Sender: TObject);
{$endif wince}

implementation

{$ifndef wince}

uses typelib;

function ImpPackage(TLI: TTypeLibImporter; aUnitName: string): string;
var
  F: Text;
begin
  with FrmTL.SelectDirectoryDialog1 do
  begin
    Title := Format(axSelectDirectoryToStorePackagePLpk, [aUnitName]);
    Execute;
    Result := Filename;
  end;
  if (Result <> '') and (Result[length(Result)] <> '\') then
    Result := Result + '\';
  AssignFile(F, UTF8ToSys(Result + aUnitName + 'P.lpk'));
  Rewrite(F);
  Write(F, TLI.PackageSource.Text);
  CloseFile(F);
  AssignFile(F, UTF8ToSys(Result + aUnitName + 'Preg.pas'));
  Rewrite(F);
  Write(F, TLI.PackageRegUnitSource.Text);
  CloseFile(F);
  if PackageEditingInterface.FindPackageWithName(aUnitName + 'P') <> nil then
  begin
    PackageEditingInterface.DoOpenPackageFile(Result+aUnitName+'P.lpk', [pofRevert], False);
    PackageEditingInterface.DoOpenPackageWithName(aUnitName + 'P', [], False);
  end
  else
    PackageEditingInterface.DoOpenPackageFile(Result+aUnitName+'P.lpk', [pofAddToRecent], False);
end;

procedure ImpFile(TLI: TTypeLibImporter; aFileName: string);
var
  F: Text;
begin
  AssignFile(F, UTF8ToSys(aFileName));
  Rewrite(F);
  Write(F, TLI.UnitSource.Text);
  CloseFile(F);
end;

procedure ImpTypeLib(Sender: TObject);
var
  TLI: TTypeLibImporter;
  bPackage, bActiveX, bRecurse: boolean;
  slTypelibs: TStringList; //sys charset
  sDir, sUnitName: string;
  i, j: integer;
begin
  FrmTL := TFrmTL.Create(nil);
  slTypelibs := TStringList.Create;
  try
    if (FrmTL.ShowModal <> mrOk) or (FrmTL.FNETL.Filename = '') then Exit;
    slTypelibs.add(UTF8ToSys(FrmTL.FNETL.Filename));
    bActiveX := FrmTL.CBxTLActiveX.Checked;
    bPackage := FrmTL.CBxTLPackage.Checked;
    bRecurse := FrmTL.CBxTLRecurse.Checked;
    i := 0;
    sDir := '';
    repeat
      TLI := TTypeLibImporter.Create(nil);
      try
        TLI.InputFileName := slTypelibs[i];
        TLI.ActiveX := bActiveX;
        TLI.CreatePackage := bPackage;
        try
          TLI.Execute;
          sUnitName := SysToUTF8(TLI.UnitName);
          if bPackage then
            sDir := ImpPackage(TLI, sUnitName);
          if sDir = '' then           // no package, open file in editor
            LazarusIDE.DoNewEditorFile(FileDescriptorUnit, sUnitName + '.pas',
              TLI.UnitSource.Text, [nfIsPartOfProject, nfOpenInEditor])
          else
            ImpFile(TLI, sDir + sUnitName + '.pas');  // save in same dir as package

          // don't create package or ActiveX container for dependencies
          bPackage := False;
          bActiveX := False;
          for j := 0 to TLI.Dependencies.Count - 1 do
            if slTypelibs.IndexOf(TLI.Dependencies[j]) = -1 then
              slTypelibs.Add(TLI.Dependencies[j]);

        except
          on E: Exception do
            ShowMessage(UTF16ToUTF8(E.Message));
        end;
      finally
        TLI.Destroy;
      end;
      Inc(i);
    until not bRecurse or (i = slTypelibs.Count);
  finally
    slTypelibs.Free;
    FrmTL.Destroy;
  end;
end;

procedure FreeObjects(const strings: TStrings);
var
  i: integer;
begin
  if strings.Count < 1 then
    exit;
  for i := 0 to Pred(strings.Count) do
  begin
    if Assigned(strings.Objects[i]) then
    begin
      strings.Objects[i].Free;
      strings.Objects[i] := nil;
    end;
  end;
end;

function ReadDefaultVal(path: string; reg: TRegistry): string;
begin
  if reg.OpenKeyReadOnly(path) then
  begin
    Result := reg.ReadString('');
    reg.CloseKey;
  end;
end;

function EnumKeys(path: string; reg: TRegistry; lst: TStringList): boolean;
begin
  Result := False;
  if reg.OpenKeyReadOnly(path) then
  begin
    reg.GetKeyNames(lst);
    reg.CloseKey;
    if lst.Count > 0 then
      Result := True;
  end;
end;

function GetTlbName(tlbid: string): string;
var
  reg: TRegistry;
  subkeys: TStringList;
  key: string;
  Name: string;
begin
  if length(tlbid) = 0 then exit;
  reg := Tregistry.Create;
  subkeys := TStringList.Create;
  try
    reg.RootKey := HKEY_CLASSES_ROOT;
    if reg.OpenKeyReadOnly('\TypeLib\' + tlbid) then
    begin
      reg.GetKeyNames(subkeys);
      reg.CloseKey;
      for key in subkeys do
      begin
        Name := ReadDefaultVal('\TypeLib\' + tlbid + '\' + key, reg);
        if length(Name) > 0 then
        begin
          Result := Name;
          break;
        end;
      end;
    end;
  finally
    reg.Free;
    subkeys.Free;
  end;
end;

//some paths will have a c:\..\file.ocx\version appended on end..
function ValidatePath(path: string): string;
var
  a, b: integer;
begin
  Result := path;
  a := LastDelimiter('.', path);
  b := LastDelimiter('\', path);
  if (b > a) and (a > 0) then
    Result := MidStr(path, 0, b - 1);
end;

procedure LoadVisualControls(lst: TListBox);
var
  reg: TRegistry;
  clsids: TStringList;
  e: TEntry;
  clsid, clsidPath: string;
  map: TStringList;
const
  catid_control = '\Implemented Categories\{40FC6ED4-2438-11cf-A3DB-080036F12502}';
begin
  lst.Clear;
  reg := TRegistry.Create;
  clsids := TStringList.Create;
  map := TStringList.Create;
  e := Nil;
  try
    reg.RootKey := HKEY_CLASSES_ROOT;
    if EnumKeys('\CLSID',reg, clsids) then
    begin
      for clsid in clsids do
      begin
        if e = Nil then
          e := TEntry.Create;
        e.clsID := clsid;
        clsidPath := '\CLSID\' + clsid;

        e.typeLib := ReadDefaultVal(clsidPath + '\TypeLib', reg);
        e.isControl := reg.KeyExists(clsidPath + '\Control');
        if not e.isControl then
          e.isControl := reg.KeyExists(clsidPath + catid_control);

        e.Name := GetTlbName(e.typeLib);

        if e.isControl and (length(e.typeLib) > 0)
        and (map.IndexOf(e.typeLib) = -1) and (length(e.Name) > 0) then
        begin
          e.path := ReadDefaultVal(clsidPath + '\InprocServer32', reg);
          e.progID := ReadDefaultVal(clsidPath + '\ProgID', reg);
          e.version := ReadDefaultVal(clsidPath + '\Version', reg);
          map.Add(e.typeLib);
          lst.AddItem(e.Name, e);
          e := Nil;
        end;
      end;
    end;
  finally
    e.Free;
    reg.Free;
    clsids.Free;
    map.Free;
  end;
end;

procedure LoadActiveXLibs(lst: TListBox);
var
  reg: TRegistry;
  clsids: TStringList;
  e: TEntry;
  clsid: string;
  vers: TStringList;
  revs: TStringList;
  map: TStringList;
  ver: string;
begin
  lst.Clear;
  reg := TRegistry.Create;
  clsids := TStringList.Create;
  map := TStringList.Create;
  vers := TStringList.Create;
  revs := TStringList.Create;
  e := Nil;
  try
    reg.RootKey := HKEY_CLASSES_ROOT;
    if EnumKeys('\TypeLib', reg, clsids) then
    begin
      for clsid in clsids do
      begin
        if e = Nil then
          e := TEntry.Create;
        e.clsID := clsid;

        vers.Clear;
        if not EnumKeys('\TypeLib\' + clsid, reg, vers) then
          continue;
        ver := vers[vers.Count - 1];
        revs.Clear;
        if not EnumKeys('\TypeLib\' + clsid + '\' + ver, reg, revs) then
          continue;

        e.Name := ReadDefaultVal('\TypeLib\' + clsID + '\' + ver, reg);
        e.path := ReadDefaultVal('\TypeLib\' + clsID + '\' + ver + '\' + revs[0] + '\win32', reg);
        e.version := ver + '.' + revs[0];
        e.path := ValidatePath(e.path);

        if (length(e.Name) > 0) and (map.IndexOf(e.Name) = -1) and FileExists(e.path) then
        begin
          lst.AddItem(e.Name, e);
          map.Add(e.Name);
          e := Nil;
        end;
      end;
    end;
  finally
    e.Free;
    revs.Free;
    vers.Free;
    reg.Free;
    clsids.Free;
    map.Free;
  end;
end;

{ TFrmTL }

procedure TFrmTL.CBxTLActiveXChange(Sender: TObject);
begin
  if not CBxTLActiveX.Checked then
    CBxTLPackage.Checked := False;
end;

procedure TFrmTL.CBxTLPackageChange(Sender: TObject);
begin
  if CBxTLPackage.Checked then
    CBxTLActiveX.Checked := True;
end;

procedure TFrmTL.FormCreate(Sender: TObject);
begin
  Caption := axImportTypeLibrary;
  Label1.Caption := axFileContainingTypeLibrary;
  CBxTLActiveX.Caption := axCreateVisualComponentTActiveXContainerDescendant;
  CBxTLPackage.Caption := axCreatePackage;
  CBxTLRecurse.Caption := axConvertDependantTypelibs;
  FNETL.Filter := axTypeLibraryFilesTlbDllExeOcxOlbTlbDllExeOcxOlbAllF;
  pagecontrol1.TabIndex := 0;
  LoadVisualControls(lstControls);
end;

procedure TFrmTL.FormDestroy(Sender: TObject);
begin
  FreeObjects(lstControls.items);
  FreeObjects(lstrefs.items);
end;

procedure TFrmTL.FormResize(Sender: TObject);
begin
  { not anchored and handled dynamically so you can see
    it exists seperate from others and still access/click others in IDE
    lstFiltered is not a child of pagecontrol and floats over other controls
    same as the groupbox with the search edit control does. (multiuse) }
   try
     lstFiltered.SetBounds(lstrefs.Left + pagecontrol1.Left + 4,
                           lstrefs.Top + pagecontrol1.Top + 4,
                           lstrefs.Width, lstrefs.Height);
   finally
   end;
end;

procedure TFrmTL.ListClickHandler(lst: TListBox);
var
  e: TEntry;
begin
  e := lst.Items.Objects[lst.ItemIndex] as TEntry;
  FNETL.Text := e.path;
end;

procedure TFrmTL.lstControlsClick(Sender: TObject);
begin
  ListClickHandler(lstControls);
end;

procedure TFrmTL.lstRefsClick(Sender: TObject);
begin
  ListClickHandler(lstRefs);
end;

procedure TFrmTL.lstFilteredClick(Sender: TObject);
var
  lst: TListBox;
  s: string;
  i: integer;
  e: TEntry;
begin
  if (lstfiltered.ItemIndex < 0) then
    exit;
  if PageControl1.TabIndex = 1 then
    lst := lstRefs
  else
    lst := lstControls;
  s := lstFiltered.Items.Strings[lstfiltered.ItemIndex];
  i := lst.Items.IndexOf(s);
  if i < 0 then
    exit;
  e := lst.Items.Objects[i] as TEntry;
  FNETL.Text := e.path;
end;

procedure TFrmTL.PageControl1Change(Sender: TObject);
begin
  //loaded on demand to reduce startup time..
  if (PageControl1.TabIndex = 1) and (lstrefs.Items.Count = 0) then
    LoadActiveXLibs(lstRefs);
  txtsearch.Text := '';
  lstfiltered.Visible := False;
end;

procedure TFrmTL.txtSearchChange(Sender: TObject);
var
  i: integer;
  item: string;
  lst: TListBox;
begin
  lstfiltered.Clear;
  if PageControl1.TabIndex = 1 then
    lst := lstRefs
  else
    lst := lstControls;

  if length(txtsearch.Text) = 0 then
  begin
    lstfiltered.Visible := False;
    exit;
  end;

  lstfiltered.Visible := True;

  for i := 0 to lst.Items.Count - 1 do
  begin
    item := lst.Items.Strings[i];
    if AnsiContainsText(item, txtsearch.Text) then
      lstfiltered.items.Add(item);
  end;
end;

{$R *.lfm}
{$endif wince}

end.

