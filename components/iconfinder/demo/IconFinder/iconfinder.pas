unit iconfinder;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  ImgList, IconThumbnails, IconViewer;

type

  { TIconFinderForm }

  TIconFinderForm = class(TForm)
    btnSelect: TButton;
    btnCancel: TButton;
    Panel1: TPanel;
    procedure btnSelectClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    FViewer: TIconViewerFrame;
    FImageList: TCustomImageList;
    FImageIndex: Integer;
    procedure SetImageIndex(AValue: Integer);
    procedure IconDblClickHandler(Sender: TObject);
  public
    property ImageList: TCustomImageList read FImageList write FImageList;
    property ImageIndex: Integer read FImageIndex write SetImageIndex;

  end;

var
  IconFinderForm: TIconFinderForm;

implementation

{$R *.lfm}

const
  {$IFDEF MSWINDOWS}
  GENERAL_PURPOSE_IMAGES = 'C:\Lazarus\lazarus-main_fpc3.2.2\images\general_purpose';
  {$ELSE}
  GENERAL_PURPOSE_IMAGES = '/home/werner/Laz-main/images/general_purpose';
  {$ENDIF}

{ TIconFinderForm }

procedure TIconFinderForm.FormCreate(Sender: TObject);
begin
  FImageIndex := -1;
  FViewer := TIconViewerFrame.Create(self);
  FViewer.Parent := Self;
  FViewer.Align := alClient;
  FViewer.BorderSpacing.Top := 6;
  FViewer.OnIconDblClick := @IconDblClickHandler;
  FViewer.AddIconFolder(GENERAL_PURPOSE_IMAGES);
  FViewer.IconViewer.FilterByIconSize := '16 x 16';
  FViewer.cmbFilterBySize.ItemIndex := FViewer.cmbFilterBySize.Items.IndexOf(FViewer.IconViewer.FilterByIconSize);
end;

procedure TIconFinderForm.btnSelectClick(Sender: TObject);
var
  item: TIconItem;
begin
  item := FViewer.SelectedIcon;
  if (item <> nil) and (FImageList <> nil) then
  begin
    if FImageIndex = -1 then
      FImageIndex := FImageList.Add(item.Picture.Bitmap, nil)
    else
      FImageList.Replace(FImageIndex, item.Picture.Bitmap, nil, true);
  end;
end;

procedure TIconFinderForm.IconDblClickHandler(Sender: TObject);
begin
  btnSelectClick(nil);
  Modalresult := mrYes;
end;

procedure TIconFinderForm.SetImageIndex(AValue: Integer);
begin
  if AValue<> FImageIndex then
  begin
    FImageIndex := AValue;
    FViewer.IconViewer.SelectedIndex := FImageIndex;
  end;
end;

end.

