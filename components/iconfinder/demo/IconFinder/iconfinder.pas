unit iconfinder;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  ImgList, IconThumbnails, IconViewer;

type

  { TIconFinderForm }

  TIconFinderForm = class(TForm)
    Bevel1:TBevel;
    btnSelect: TButton;
    btnCancel: TButton;
    Panel1: TPanel;
    procedure btnSelectClick(Sender: TObject);
    procedure FormActivate(Sender:TObject);
    procedure FormCreate(Sender: TObject);
  private
    FViewer: TIconViewerFrame;
    FImageList: TCustomImageList;
    FImageIndex: Integer;
    FActivated: Boolean;
    procedure SetImageIndex(AValue: Integer);
    procedure IconDblClickHandler(Sender: TObject);
    procedure IconViewerChangeHandler(Sender: TObject);
  public
    property ImageList: TCustomImageList read FImageList write FImageList;
    property ImageIndex: Integer read FImageIndex write SetImageIndex;

  end;

var
  IconFinderForm: TIconFinderForm;

implementation

{$R *.lfm}

const
  // Please adjust LAZ_DIR to the location of your Lazarus installation
  {$IFDEF MSWINDOWS}
  LAZ_DIR = 'C:\Lazarus\lazarus-main_fpc3.2.2\';
  {$ELSE}
  LAZ_DIR = '/home/werner/Laz-main/';
  {$ENDIF}
  GENERAL_PURPOSE_IMAGES = LAZ_DIR + 'images/general_purpose';

{ TIconFinderForm }

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

procedure TIconFinderForm.FormActivate(Sender:TObject);
begin
  if not FActivated then
  begin
    FActivated := true;
    FViewer.FocusKeywordFilter;
  end;
end;

procedure TIconFinderForm.FormCreate(Sender: TObject);
begin
  FImageIndex := -1;
  FViewer := TIconViewerFrame.Create(self);
  FViewer.Parent := Self;
  FViewer.Align := alClient;
  FViewer.BorderSpacing.Top := 6;
  FViewer.OnChange := @IconViewerChangeHandler;
  FViewer.OnIconDblClick := @IconDblClickHandler;
  FViewer.AddIconFolder(GENERAL_PURPOSE_IMAGES);
  FViewer.IconViewer.FilterByIconSize := '16 x 16';
  FViewer.cmbFilterBySize.ItemIndex := FViewer.cmbFilterBySize.Items.IndexOf(FViewer.IconViewer.FilterByIconSize);
  FViewer.TabStop := true;
  FViewer.TabOrder := 0;
end;

procedure TIconFinderForm.IconDblClickHandler(Sender: TObject);
begin
  btnSelectClick(nil);
  Modalresult := mrYes;
end;

procedure TIconFinderForm.IconViewerChangeHandler(Sender: TObject);
begin
  btnSelect.Enabled := FViewer.SelectedIcon <> nil;
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

