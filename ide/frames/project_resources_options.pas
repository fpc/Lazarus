unit project_resources_options;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Graphics, Forms, Controls, ComCtrls, Dialogs, LCLProc,
  IDEOptionsIntf, IDEImagesIntf,
  Project, LCLStrConsts, LazarusIDEStrConsts;

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

procedure TResourcesOptionsFrame.AddResource(AFileName: String);

  procedure AddItem(ResType, ResName: String);
  var
    Item: TListItem;
  begin
    Item := lbResources.Items.Add;
    Item.Caption := ResType;
    Item.SubItems.Add(UTF8UpperCase(ResName));
  end;

  procedure AddBitmap(AFileName: String);
  begin
    AddItem('BITMAP', ExtractFileNameOnly(AFileName));
  end;

  procedure AddCursor(AFileName: String);
  begin
    AddItem('CURSOR', ExtractFileNameOnly(AFileName));
  end;

  procedure AddIcon(AFileName: String);
  begin
    AddItem('ICON', ExtractFileNameOnly(AFileName));
  end;

  procedure AddRCData(AFileName: String);
  begin
    AddItem('RCDATA', ExtractFileNameOnly(AFileName));
  end;

var
  Ext: String;
begin
  Ext := UTF8UpperCase(ExtractFileExt(AFileName));
  case Ext of
    '.BMP': AddBitmap(AFileName);
    '.CUR': AddCursor(AFileName);
    '.ICO': AddIcon(AFileName);
    //'.FNT', '.FON', '.TTF': AddFont(AFileName);
  else
    AddRCData(AFileName);
  end;
end;

function TResourcesOptionsFrame.GetTitle: string;
begin
  Result := dlgPOResources;
end;

procedure TResourcesOptionsFrame.Setup(ADialog: TAbstractOptionsEditorDialog);
begin
  ToolBar1.Images := IDEImages.Images_16;
  lbResources.Column[0].Caption := rsResourceType;
  lbResources.Column[1].Caption := rsResource;
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
begin

end;

procedure TResourcesOptionsFrame.WriteSettings(AOptions: TAbstractIDEOptions);
begin

end;

class function TResourcesOptionsFrame.SupportedOptionsClass: TAbstractIDEOptionsClass;
begin
  Result := TProject;
end;

initialization
  RegisterIDEOptionsEditor(GroupProject, TResourcesOptionsFrame, ProjectOptionsResources);

end.

