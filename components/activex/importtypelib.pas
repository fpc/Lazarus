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

procedure ImpTypeLib(Sender: TObject);

var
  TLI: TTypeLibImporter;
  bPackage, bActiveX, bRecurse: boolean;
  slTypelibs: TStringList; //sys charset
  i, j: integer;
  F: Text;
  sDir, sUnitName: string; //utf8
begin
  FrmTL := TFrmTL.Create(nil);
  try
    if (FrmTL.ShowModal = mrOk) and (FrmTL.FNETL.Filename <> '') then
    begin
      slTypelibs := TStringList.Create;
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
            begin
              with FrmTL.SelectDirectoryDialog1 do
              begin
                Title := Format(axSelectDirectoryToStorePackagePLpk, [sUnitName]);
                Execute;
                sDir := Filename;
              end;

              if (sDir <> '') and (sDir[length(sdir)] <> '\') then
                sDir := sDir + '\';
              AssignFile(F, UTF8ToSys(sDir + sUnitName + 'P.lpk'));
              Rewrite(F);
              Write(F, TLI.PackageSource.Text);
              CloseFile(F);
              AssignFile(F, UTF8ToSys(sDir + sUnitName + 'Preg.pas'));
              Rewrite(F);
              Write(F, TLI.PackageRegUnitSource.Text);
              CloseFile(F);

              if PackageEditingInterface.FindPackageWithName(sUnitName + 'P') <> nil then
              begin
                PackageEditingInterface.DoOpenPackageFile(sDir+sUnitName+'P.lpk', [pofRevert], False);
                PackageEditingInterface.DoOpenPackageWithName(sUnitName + 'P', [], False);
              end
              else
                PackageEditingInterface.DoOpenPackageFile(sDir+sUnitName+'P.lpk', [pofAddToRecent], False);
            end;

            if sDir = '' then  // no package, open file in editor
              LazarusIDE.DoNewEditorFile(FileDescriptorUnit, sUnitName + '.pas',
                TLI.UnitSource.Text, [nfIsPartOfProject, nfOpenInEditor])
            else
            begin //save in same dir as package
              AssignFile(F, UTF8ToSys(sDir + sUnitName + '.pas'));
              Rewrite(F);
              Write(F, TLI.UnitSource.Text);
              CloseFile(F);
            end;

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
        i := i + 1;
      until not bRecurse or (i = slTypelibs.Count);
    end;
  finally
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
      try
        strings.Objects[i].Free;
      finally
        strings.Objects[i] := nil;
      end;
    end;
  end;
end;

function ReadDefaultVal(path: string; reg: TRegistry): string;
begin
  reg.RootKey := HKEY_CLASSES_ROOT;
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
  clsid: string;
  map: TStringList;
const
  catid_control = '\Implemented Categories\{40FC6ED4-2438-11cf-A3DB-080036F12502}';
begin
  lst.Clear;
  reg := TRegistry.Create;
  clsids := TStringList.Create;
  map := TStringList.Create;
  try
    reg.RootKey := HKEY_CLASSES_ROOT;
    if reg.OpenKeyReadOnly('\CLSID') then
    begin
      reg.GetKeyNames(clsids);
      reg.CloseKey;

      for clsid in clsids do
      begin
        e := TEntry.Create;
        e.clsID := clsid;
        clsid := '\CLSID\' + clsid;

        e.typeLib := ReadDefaultVal(clsid + '\TypeLib', reg);
        e.isControl := reg.KeyExists(clsID + '\Control');
        if not e.isControl then
          e.isControl := reg.KeyExists(clsID + catid_control);

        if e.isControl and (length(e.typeLib) > 0) and
          (map.IndexOf(e.typeLib) = -1) then
        begin
          e.Name := GetTlbName(e.typeLib);
          if length(e.Name) > 0 then
          begin
            e.path := ReadDefaultVal(clsid + '\InprocServer32', reg);
            e.progID := ReadDefaultVal(clsid + '\ProgID', reg);
            e.version := ReadDefaultVal(clsid + '\Version', reg);
            map.Add(e.typeLib);
            lst.AddItem(e.Name, e);
          end;
        end
        else
          e.Free;
      end;
    end;
  finally
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
  try
    reg.RootKey := HKEY_CLASSES_ROOT;
    if EnumKeys('\TypeLib', reg, clsids) then
    begin
      for clsid in clsids do
      begin
        e := TEntry.Create;
        e.clsID := clsid;

        vers := TStringList.Create;
        if not EnumKeys('\TypeLib\' + clsid, reg, vers) then
        begin
          vers.Free;
          e.Free;
          continue;
        end;
        ver := vers[vers.Count - 1];
        revs := TStringList.Create;
        if not EnumKeys('\TypeLib\' + clsid + '\' + ver, reg, revs) then
        begin
          revs.Free;
          e.Free;
          continue;
        end;

        e.Name := ReadDefaultVal('\TypeLib\' + clsID + '\' + ver, reg);
        e.path := ReadDefaultVal('\TypeLib\' + clsID + '\' + ver + '\' + revs[0] + '\win32', reg);
        e.version := ver + '.' + revs[0];
        e.path := ValidatePath(e.path);

        if (length(e.Name) > 0) and (map.IndexOf(e.Name) = -1) and FileExists(e.path) then
        begin
          lst.AddItem(e.Name, e);
          map.Add(e.Name);
        end
        else
          e.Free;

        vers.Free;
        revs.Free;
      end;
    end;
  finally
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
  lstcontrols.SetBounds(lstrefs.Left, lstrefs.Top, lstrefs.Width, lstrefs.Height);
  lstFiltered.SetBounds(lstrefs.Left + pagecontrol1.Left + 4, lstrefs.Top +
    pagecontrol1.Top + 4, lstrefs.Width, lstrefs.Height);
  LoadVisualControls(lstControls);
end;

procedure TFrmTL.FormDestroy(Sender: TObject);
begin
  FreeObjects(lstControls.items);
  FreeObjects(lstrefs.items);
end;

procedure TFrmTL.ListClickHandler(lst: TListBox);
var
  e: TEntry;
begin
  try
    e := lst.Items.Objects[lst.ItemIndex] as TEntry;
    FNETL.Text := e.path;
  finally
  end;
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
  if PageControl1.TabIndex = 1 then
    lst := lstRefs
  else
    lst := lstControls;
  s := lstFiltered.Items.Strings[lstfiltered.ItemIndex];
  i := lst.Items.IndexOf(s);
  if i < 0 then
    exit;
  try
    e := lst.Items.Objects[i] as TEntry;
    FNETL.Text := e.path;
  finally
  end;
end;

procedure TFrmTL.PageControl1Change(Sender: TObject);
begin
  //loaded on depand to reduce startup time..
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

