unit ImportTypelib;

{$mode objfpc}{$H+}

interface

{$ifndef wince}
uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, EditBtn,
  StdCtrls, ButtonPanel, lazideintf, projectintf, PackageIntf, activexstrconsts,
  LazUTF8;

type

  { TFrmTL }

  TFrmTL = class(TForm)
    ButtonPanel: TButtonPanel;
    CBxTLActiveX: TCheckBox;
    CBxTLPackage: TCheckBox;
    CBxTLRecurse: TCheckBox;
    FNETL: TFileNameEdit;
    Label1: TLabel;
    SelectDirectoryDialog1: TSelectDirectoryDialog;
    procedure CBxTLActiveXChange(Sender: TObject);
    procedure CBxTLPackageChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  FrmTL: TFrmTL;

procedure ImpTypeLib(Sender: TObject);
{$endif wince}

implementation

{$ifndef wince}

uses typelib;

procedure ImpTypeLib(Sender: TObject);

var TLI:TTypeLibImporter;
  bPackage,bActiveX,bRecurse:boolean;
  slTypelibs:TStringList; //sys charset
  i,j:integer;
  F:text;
  sDir,sUnitName:string;  //utf8
begin
  FrmTL:= TFrmTL.create(nil);
  try
  if (FrmTL.ShowModal=mrOK) and (FrmTL.FNETL.Filename<>'') then
    begin
    slTypelibs:=TStringList.Create;
    slTypelibs.add(UTF8ToSys(FrmTL.FNETL.Filename));
    bActiveX:=FrmTL.CBxTLActiveX.Checked;
    bPackage:=FrmTL.CBxTLPackage.Checked;
    bRecurse:=FrmTL.CBxTLRecurse.Checked;
    i:=0;
    sDir:='';
    repeat
      TLI:=TTypeLibImporter.Create(nil);
      try
        TLI.InputFileName:=slTypelibs[i];
        TLI.ActiveX:=bActiveX;
        TLI.CreatePackage:=bPackage;
        try
          TLI.Execute;
          sUnitName:=SysToUTF8(TLI.UnitName);
          if bPackage then
            begin
            with FrmTL.SelectDirectoryDialog1 do
              begin
              Title:=Format(axSelectDirectoryToStorePackagePLpk, [sUnitName]);
              Execute;
              sDir:=Filename;
              end;
            if (sDir<>'') and (sDir[length(sdir)]<>'\') then
              sDir:=sDir+'\';
            AssignFile(F,UTF8ToSys(sDir+sUnitName+'P.lpk'));
            Rewrite(F);
            Write(F,TLI.PackageSource.Text);
            CloseFile(F);
            AssignFile(F,UTF8ToSys(sDir+sUnitName+'Preg.pas'));
            Rewrite(F);
            Write(F,TLI.PackageRegUnitSource.Text);
            CloseFile(F);
            if PackageEditingInterface.FindPackageWithName(sUnitName+'P')<>nil then
              begin
              PackageEditingInterface.DoOpenPackageFile(sDir+sUnitName+'P.lpk',[pofRevert],false);
              PackageEditingInterface.DoOpenPackageWithName(sUnitName+'P',[],false);
              end
            else
              PackageEditingInterface.DoOpenPackageFile(sDir+sUnitName+'P.lpk',[pofAddToRecent],false);
            end;
          if sDir='' then  // no package, open file in editor
            LazarusIDE.DoNewEditorFile(FileDescriptorUnit,sUnitName+'.pas',
               TLI.UnitSource.Text,[nfIsPartOfProject,nfOpenInEditor])
          else
            begin //save in same dir as package
            AssignFile(F,UTF8ToSys(sDir+sUnitName+'.pas'));
            Rewrite(F);
            Write(F,TLI.UnitSource.Text);
            CloseFile(F);
            end;
          // don't create package or ActiveX container for dependencies
          bPackage:=false;
          bActiveX:=false;
          for j:=0 to TLI.Dependencies.Count-1 do
            if slTypelibs.IndexOf(TLI.Dependencies[j])=-1 then
              slTypelibs.Add(TLI.Dependencies[j]);
        except
          on E: Exception do ShowMessage(UTF16ToUTF8(E.Message));
        end;
      finally
        TLI.destroy;
      end;
      i:=i+1;
    until not bRecurse or (i=slTypelibs.Count)
    end;
  finally
    FrmTL.Destroy;
  end;
end;

{ TFrmTL }

procedure TFrmTL.CBxTLActiveXChange(Sender: TObject);
begin
  if not CBxTLActiveX.Checked then
    CBxTLPackage.Checked:=false;
end;

procedure TFrmTL.CBxTLPackageChange(Sender: TObject);
begin
  if CBxTLPackage.Checked then
    CBxTLActiveX.Checked:=true;
end;

procedure TFrmTL.FormCreate(Sender: TObject);
begin
  Caption:=axImportTypeLibrary;
  Label1.Caption:=axFileContainingTypeLibrary;
  CBxTLActiveX.Caption:=axCreateVisualComponentTActiveXContainerDescendant;
  CBxTLPackage.Caption:=axCreatePackage;
  CBxTLRecurse.Caption:=axConvertDependantTypelibs;
  FNETL.Filter:=axTypeLibraryFilesTlbDllExeOcxOlbTlbDllExeOcxOlbAllF;
end;

{$R *.lfm}
{$endif wince}

end.

