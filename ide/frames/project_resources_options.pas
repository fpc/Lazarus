unit project_resources_options;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Graphics, Forms, Controls, ComCtrls, Dialogs, LCLProc,
  IDEOptionsIntf, IDEImagesIntf,
  Project, ProjectUserResources, LCLStrConsts, LazarusIDEStrConsts;

type

  { TResourcesOptionsFrame }

  TResourcesOptionsFrame = class(TAbstractIDEOptionsEditor)
    lbResources: TListView;
    dlgOpen: TOpenDialog;
    ToolBar1: TToolBar;
    btnAdd: TToolButton;
    btnDelete: TToolButton;
    procedure btnAddClick(Sender: TObject);
    procedure btnDeleteClick(Sender: TObject);
  private
    procedure AddResource(AFileName: String);
    procedure AddResourceItem(ResFile: String; ResType: TUserResourceType; ResName: String);
  public
    function GetTitle: string; override;
    procedure Setup(ADialog: TAbstractOptionsEditorDialog); override;
    procedure ReadSettings(AOptions: TAbstractIDEOptions); override;
    procedure WriteSettings(AOptions: TAbstractIDEOptions); override;
    class function SupportedOptionsClass: TAbstractIDEOptionsClass; override;
  end;

implementation

{$R *.lfm}

{ TResourcesOptionsFrame }

procedure TResourcesOptionsFrame.btnAddClick(Sender: TObject);
begin
  if dlgOpen.Execute then
    AddResource(dlgOpen.FileName);
end;

procedure TResourcesOptionsFrame.btnDeleteClick(Sender: TObject);
var
  Index: Integer;
begin
  Index := lbResources.ItemIndex;
  if (Index <> -1) then
    lbResources.Items.Delete(Index);
end;

procedure TResourcesOptionsFrame.AddResourceItem(ResFile: String; ResType: TUserResourceType; ResName: String);
var
  Item: TListItem;
begin
  Item := lbResources.Items.Add;
  Item.Caption := ResFile;
  Item.SubItems.Add(ExtractFileName(ResFile));
  Item.SubItems.Add(ResourceTypeToStr[ResType]);
  Item.SubItems.Add(ResName);
end;

procedure TResourcesOptionsFrame.AddResource(AFileName: String);
var
  ResName, Ext: String;
begin
  Ext := UTF8UpperCase(ExtractFileExt(AFileName));
  ResName := UTF8UpperCase(ExtractFileNameOnly(AFileName));
  case Ext of
    '.BMP': AddResourceItem(AFileName, rtBitmap, ResName);
    '.CUR': AddResourceItem(AFileName, rtCursor, ResName);
    '.ICO': AddResourceItem(AFileName, rtIcon, ResName);
    //'.FNT', '.FON', '.TTF': AddResourceItem(AFileName, rtFont, ResName);
  else
    AddResourceItem(AFileName, rtRCData, ResName);
  end;
end;

function TResourcesOptionsFrame.GetTitle: string;
begin
  Result := dlgPOResources;
end;

procedure TResourcesOptionsFrame.Setup(ADialog: TAbstractOptionsEditorDialog);
begin
  ToolBar1.Images := IDEImages.Images_16;
  lbResources.Column[1].Caption := rsResourceFileName;
  lbResources.Column[2].Caption := rsResourceType;
  lbResources.Column[3].Caption := rsResource;
  btnAdd.Caption := lisBtnAdd;
  btnDelete.Caption := lisBtnDelete;
  btnAdd.ImageIndex := IDEImages.LoadImage(16, 'laz_add');
  btnDelete.ImageIndex := IDEImages.LoadImage(16, 'laz_delete');
  // system resources can be:
  // 1. Graphic files
  // 2. Font files
  // 3. Any files
  dlgOpen.Filter := GraphicFilter(TGraphic)+'|'+Format(rsAllFiles,[GetAllFilesMask, GetAllFilesMask,'']);
end;

procedure TResourcesOptionsFrame.ReadSettings(AOptions: TAbstractIDEOptions);
var
  Project: TProject absolute AOptions;
  List: TResourceList;
  I: Integer;
begin
  lbResources.Items.Clear;
  List := Project.ProjResources.UserResources.List;
  for I := 0 to List.Count - 1 do
    AddResourceItem(List[I]^.FileName, List[I]^.ResType, List[I]^.ResName);
end;

procedure TResourcesOptionsFrame.WriteSettings(AOptions: TAbstractIDEOptions);
var
  Project: TProject absolute AOptions;
  I: Integer;
begin
  Project.ProjResources.UserResources.List.Clear;
  for I := 0 to lbResources.Items.Count - 1 do
    Project.ProjResources.UserResources.List.AddResource(lbResources.Items[I].Caption,
      StrToResourceType(lbResources.Items[I].SubItems[1]), lbResources.Items[I].SubItems[2]);
end;

class function TResourcesOptionsFrame.SupportedOptionsClass: TAbstractIDEOptionsClass;
begin
  Result := TProject;
end;

initialization
  RegisterIDEOptionsEditor(GroupProject, TResourcesOptionsFrame, ProjectOptionsResources);

end.

