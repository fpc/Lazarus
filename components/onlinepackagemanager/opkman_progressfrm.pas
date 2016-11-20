{
***************************************************************************
*                                                                         *
*   This source is free software; you can redistribute it and/or modify   *
*   it under the terms of the GNU General Public License as published by  *
*   the Free Software Foundation; either version 2 of the License, or     *
*   (at your option) any later version.                                   *
*                                                                         *
*   This code is distributed in the hope that it will be useful, but      *
*   WITHOUT ANY WARRANTY; without even the implied warranty of            *
*   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU     *
*   General Public License for more details.                              *
*                                                                         *
*   A copy of the GNU General Public License is available on the World    *
*   Wide Web at <http://www.gnu.org/copyleft/gpl.html>. You can also      *
*   obtain it by writing to the Free Software Foundation,                 *
*   Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.        *
*                                                                         *
***************************************************************************

Author: Balázs Székely
}
unit opkman_progressfrm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls,
  StdCtrls, ExtCtrls, opkman_serializablepackages, opkman_installer, opkman_VirtualTrees;

type

  { TProgressFrm }

  TProgressFrm = class(TForm)
    bCancel: TButton;
    cbExtractOpen: TCheckBox;
    imTree: TImageList;
    lbEllapsed: TLabel;
    lbEllapsedData: TLabel;
    lbPackage: TLabel;
    lbPackageData: TLabel;
    lbReceived: TLabel;
    lbReceivedTotal: TLabel;
    lbRemaining: TLabel;
    lbRemainingData: TLabel;
    lbSpeed: TLabel;
    lbSpeedData: TLabel;
    pb: TProgressBar;
    pbTotal: TProgressBar;
    pnLabels: TPanel;
    pnButtons: TPanel;
    tmWait: TTimer;
    procedure bCancelClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: char);
    procedure FormResize(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure tmWaitTimer(Sender: TObject);
  private
    FCanClose: Boolean;
    FSuccess: Boolean;
    FNeedToRebuild: Boolean;
    FInstallStatus: TInstallStatus;
    FMdlRes: TModalResult;
    FType: Integer;
    FCnt, FTotCnt: Integer;
    FVST: TVirtualStringTree;
    procedure VSTGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; {%H-}TextType: TVSTTextType; var CellText: String);
    procedure VSTGetImageIndex(Sender: TBaseVirtualTree; Node: PVirtualNode;
      {%H-}Kind: TVTImageKind; Column: TColumnIndex; var {%H-}Ghosted: Boolean;
      var ImageIndex: Integer);
    procedure VSTFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure VSTPaintText(Sender: TBaseVirtualTree; const TargetCanvas: TCanvas;
      Node: PVirtualNode; {%H-}Column: TColumnIndex; {%H-}TextType: TVSTTextType);
  public
    procedure DoOnPackageDownloadProgress(Sender: TObject; {%H-}AFrom, ATo: String; ACnt, ATotCnt: Integer;
      ACurPos, ACurSize, ATotPos, ATotSize: Int64; AEllapsed, ARemaining, ASpeed: LongInt);
    procedure DoOnPackageDownloadError(Sender: TObject; APackageName: String; const AErrMsg: String = '');
    procedure DoOnPackageDownloadCompleted(Sender: TObject);
    procedure DoOnZipProgress(Sender: TObject; AZipfile: String; ACnt, ATotCnt: Integer;
      ACurPos, ACurSize, ATotPos, ATotSize: Int64; AEllapsed, ARemaining, ASpeed: LongInt);
    procedure DoOnZipError(Sender: TObject; APackageName: String; const AErrMsg: String);
    procedure DoOnZipCompleted(Sender: TObject);
    procedure DoOnPackageInstallProgress(Sender: TObject; ACnt, ATotCnt: Integer; APackageName: String; AInstallMessage: TInstallMessage);
    procedure DoOnPackageInstallError(Sender: TObject; APackageName, AErrMsg: String);
    procedure DoOnPackageInstallCompleted(Sender: TObject; ANeedToRebuild: Boolean; AInstallStatus: TInstallStatus);
    procedure DoOnPackageUpdateProgress(Sender: TObject; AUPackageName, AUPackageURL: String; ACnt, ATotCnt: Integer; AUTyp: Integer; AUErrMsg: String);
    procedure DoOnPackageUpdateCompleted(Sender: TObject; AUSuccess: Boolean);
    procedure SetupControls(const AType: Integer);
  published
    property NeedToRebuild: Boolean read FNeedToRebuild;
    property InstallStatus: TInstallStatus read FInstallStatus;
  end;

var
  ProgressFrm: TProgressFrm;

implementation
uses opkman_common, opkman_const, opkman_downloader, opkman_zipper;
{$R *.lfm}

{ TProgressFrm }

type
  PData = ^TData;
  TData = record
    FName: String;
    FImageIndex: Integer;
  end;

procedure TProgressFrm.FormShow(Sender: TObject);
begin
  FCanClose := False;
  FCnt := 0;
  FTotCnt := 0;
  FMdlRes := mrNone;
  if Assigned(PackageInstaller) then
    tmWait.Enabled := True;
end;

procedure TProgressFrm.tmWaitTimer(Sender: TObject);
begin
  tmWait.Enabled := False;
  PackageInstaller.StartInstall;
end;


procedure TProgressFrm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  if not FCanClose then
    CloseAction := caNone;
  if FSuccess then
    ModalResult := mrOk
  else
    ModalResult := mrCancel;
end;

procedure TProgressFrm.FormCreate(Sender: TObject);
begin
  FVST := TVirtualStringTree.Create(nil);
  with FVST do
  begin
    Parent := Self;
    Align := alClient;
    Anchors := [akLeft, akTop, akRight];
    Images := imTree;
    Color := clBtnFace;
    DefaultNodeHeight := 25;
    Indent := 0;
    TabOrder := 1;
    DefaultText := '';
    Header.AutoSizeIndex := 0;
    Header.Height := 25;
    Visible := False;
    Colors.BorderColor := clBlack;
    BorderSpacing.Top := 10;
    BorderSpacing.Left := 10;
    BorderSpacing.Right := 10;
    with Header.Columns.Add do begin
      Position := 0;
      Width := 250;
      Text := 'PackageName';
    end;
    Header.Options := [hoAutoResize, hoColumnResize, hoRestrictDrag, hoShowSortGlyphs, hoAutoSpring];
    Header.SortColumn := 0;
    TabOrder := 2;
    TreeOptions.MiscOptions := [toFullRepaintOnResize, toInitOnSave, toToggleOnDblClick, toWheelPanning];
    TreeOptions.PaintOptions := [toHideFocusRect, toAlwaysHideSelection, toPopupMode, toShowButtons, toShowDropmark, toThemeAware, toUseBlendedImages];
    TreeOptions.SelectionOptions := [toFullRowSelect, toRightClickSelect];
    TreeOptions.AutoOptions := [toAutoTristateTracking];
    OnGetText := @VSTGetText;
    OnGetImageIndex := @VSTGetImageIndex;
    OnFreeNode := @VSTFreeNode;
    OnPaintText := @VSTPaintText;
  end;
  FVST.NodeDataSize := SizeOf(TData);
  PackageInstaller := nil;
end;

procedure TProgressFrm.FormDestroy(Sender: TObject);
begin
  FVST.Clear;
  FVST.Free;
  if Assigned(PackageInstaller) then
    FreeAndNil(PackageInstaller);
end;

procedure TProgressFrm.FormKeyPress(Sender: TObject; var Key: char);
begin
  if (Key = #13) or (Key = #27) then
  begin
    FCanClose := True;
    FSuccess := False;
    Close;
  end;
end;

procedure TProgressFrm.FormResize(Sender: TObject);
begin
  bCancel.Left := (Self.Width - bCancel.Width) div 2;
end;

procedure TProgressFrm.DoOnPackageDownloadProgress(Sender: TObject; AFrom, ATo: String;
  ACnt, ATotCnt: Integer; ACurPos, ACurSize, ATotPos, ATotSize: Int64;
  AEllapsed, ARemaining, ASpeed: LongInt);
begin
  Caption := rsProgressfrmCaption0 + '(' + IntToStr(ACnt) + '/' + IntToStr(ATotCnt) +')' + rsProgressFrmCaption4;
  lbPackageData.Caption := ExtractFileName(ATo);
  lbSpeedData.Caption := FormatSpeed(ASpeed);
  lbSpeedData.Update;
  lbEllapsedData.Caption := SecToHourAndMin(AEllapsed);
  lbEllapsedData.Update;
  lbRemainingData.Caption := SecToHourAndMin(ARemaining);
  lbRemainingData.Update;
  if ACurSize > 0 then
    lbReceived.Caption := rsProgressFrmlbReceivedCaption0 + '  ' + FormatSize(ACurPos) + ' / ' + FormatSize(ACurSize)
  else
    lbReceived.Caption := rsProgressFrmlbReceivedCaption0 + '  ' + FormatSize(ACurPos) + ' / ' + rsProgressFrmCaption5;
  lbReceived.Update;
  pb.Position := Round((ACurPos/ACurSize) * 100);
  pb.Update;
  lbReceivedTotal.Caption := rsProgressFrmlbReceivedTotalCaption0 + '  ' + FormatSize(ATotPos) + ' / ' + FormatSize(ATotSize);
  lbReceivedTotal.Update;
  pbTotal.Position := Round((ATotPos/ATotSize) * 100);
  pbTotal.Update;
  FCnt := ACnt;
  FTotCnt := ATotCnt;
end;

procedure TProgressFrm.DoOnPackageDownloadError(Sender: TObject; APackageName: String;
  const AErrMsg: String);
var
  Msg: String;
begin
  if ((FMdlRes = mrNone) or (FMdlRes = mrYes) or (FMdlRes = mrNo)) then
  begin
    if (FCnt < FTotCnt) then
    begin
      Msg := rsProgressFrmError0 + ' "' + APackageName + '". ' + rsProgressFrmError1 + sLineBreak  + '"' +
             AErrMsg + '"' + sLineBreak + rsProgressFrmConfirm0;
      FMdlRes := MessageDlgEx(Msg, mtError, [mbYes, mbYesToAll, mbNo], Self);
    end
    else
    begin
      Msg :=  rsProgressFrmError0 + ' "' + APackageName + '". ' + rsProgressFrmError1 + sLineBreak  + '"' + AErrMsg + '"';
      MessageDlgEx(Msg, mtError, [mbOk], Self);
      FMdlRes := mrNo;
    end;
  end;
  if FMdlRes = mrNo then
  begin
    FCanClose := True;
    FSuccess := False;
    PackageDownloader.OnPackageDownloadProgress := nil;
    PackageDownloader.OnPackageDownloadError := nil;
    PackageDownloader.CancelDownloadPackages;
    Close;
  end;
end;

procedure TProgressFrm.DoOnPackageDownloadCompleted(Sender: TObject);
begin
  FCanClose := True;
  FSuccess := True;
  Close;
end;

procedure TProgressFrm.DoOnZipProgress(Sender: TObject; AZipfile: String;
  ACnt, ATotCnt: Integer;  ACurPos, ACurSize, ATotPos, ATotSize: Int64;
  AEllapsed, ARemaining, ASpeed: LongInt);
begin
  Caption := rsProgressfrmCaption1 + '(' + IntToStr(ACnt) + '/' + IntToStr(ATotCnt) +')' + rsProgressFrmCaption4;
  lbPackageData.Caption := AZipFile;
  lbSpeedData.Caption := FormatSpeed(ASpeed);
  lbSpeedData.Update;
  lbEllapsedData.Caption := SecToHourAndMin(AEllapsed);
  lbEllapsedData.Update;
  lbRemainingData.Caption := SecToHourAndMin(ARemaining);
  lbRemainingData.Update;
  lbReceived.Caption := rsProgressFrmlbReceivedCaption1 + '  ' + FormatSize(ACurPos) + ' / ' + FormatSize(ACurSize);
  lbReceived.Update;
  pb.Position := Round((ACurPos/ACurSize) * 100);
  pb.Update;
  lbReceivedTotal.Caption := rsProgressFrmlbReceivedTotalCaption1 + '  ' + FormatSize(ATotPos) + ' / ' + FormatSize(ATotSize);
  lbReceivedTotal.Update;
  pbTotal.Position := Round((ATotPos/ATotSize) * 100);
  pbTotal.Update;
  FCnt := ACnt;
  FTotCnt := ATotCnt;
end;

procedure TProgressFrm.DoOnZipError(Sender: TObject; APackageName: String; const AErrMsg: String);
var
  Msg: String;
begin
  if ((FMdlRes = mrNone) or (FMdlRes = mrYes) or (FMdlRes = mrNo)) then
  begin
    if (FCnt < FTotCnt) then
    begin
      Msg := rsProgressFrmError2 + ' "' + APackageName + '". ' + rsProgressFrmError1 + sLineBreak + '" ' +
             AErrMsg + '"' + sLineBreak + rsProgressFrmConfirm0;
      FMdlRes := MessageDlgEx(Msg, mtError, [mbYes, mbYesToAll, mbNo],  Self);
    end
    else
    begin
      Msg := rsProgressFrmError2 + ' "' + APackageName + '". ' + rsProgressFrmError1 + sLineBreak + '"' + AErrMsg + '"';
      MessageDlgEx(Msg, mtError, [mbOk], Self);
      FMdlRes := mrNo;
    end;
  end;
  if FMdlRes = mrNo then
  begin
    FCanClose := True;
    FSuccess := False;
    PackageUnzipper.OnZipProgress := nil;
    PackageUnzipper.OnZipError := nil;
    PackageUnzipper.StopUnZip;
    Close;
  end;
end;

procedure TProgressFrm.DoOnZipCompleted(Sender: TObject);
begin
  FCanClose := True;
  FSuccess := True;
  Close;
end;

procedure TProgressFrm.DoOnPackageInstallProgress(Sender: TObject; ACnt, ATotCnt: Integer; APackageName: String;
  AInstallMessage: TInstallMessage);
var
  Node: PVirtualNode;
  Data: PData;
  Str: String;
  I: Integer;
begin
  FCnt := ACnt;
  FTotCnt := ATotCnt;
  Caption := rsProgressFrmCaption2 + '(' + IntToStr(ACnt) + '/' + IntToStr(ATotCnt) + ')' + rsProgressFrmCaption4;
  Node := FVST.AddChild(nil);
  Data := FVST.GetNodeData(Node);
  case AInstallMessage of
    imOpenPackage:
       begin
         Data^.FName := rsInstallInfoOpenPackage + ' "' + APackageName + '".';
         Data^.FImageIndex := 0;
       end;
    imOpenPackageSuccess:
       begin
         Data^.FName := rsInstallInfoOpenPackageSuccess;
         Data^.FImageIndex := 1;
       end;
    imCompilePackage:
       begin
         Data^.FName := rsInstallInfoCompilePackage + ' "' + APackageName + '".';
         Data^.FImageIndex := 0;
       end;
    imCompilePackageSuccess:
       begin
         Data^.FName := rsInstallInfoCompilePackageSuccess;
         Data^.FImageIndex := 1;
       end;
    imInstallPackage:
       begin
         Data^.FName := rsInstallInfoInstallPackage + ' "' + APackageName + '".';
         Data^.FImageIndex := 0;
       end;
    imInstallPackageSuccess:
       begin
         Data^.FName := rsInstallInfoInstallPackageSuccess;
         Data^.FImageIndex := 1;
       end;
    imPackageCompleted:
       begin
         Str := '';
         for I := 1 to 85 do
           Str := Str + '_';
         Data^.FName := Str;
         Data^.FImageIndex := -1;
       end;
  end;
  FVST.TopNode := Node;
  Application.ProcessMessages;
end;

procedure TProgressFrm.DoOnPackageInstallError(Sender: TObject; APackageName: String;
  AErrMsg: String);
var
  Msg: String;
  Node: PVirtualNode;
  Data: PData;
begin
  Node := FVST.AddChild(nil);
  Data := FVST.GetNodeData(Node);
  Data^.FName := AErrMsg;
  Data^.FImageIndex := 2;
  FVST.TopNode := Node;
  if ((FMdlRes = mrNone) or (FMdlRes = mrYes) or (FMdlRes = mrNo)) then
  begin
    if (FCnt < FTotCnt) then
    begin
      Msg := rsProgressFrmError3 + ' "' + APackageName + '". ' + rsProgressFrmConfirm0;
      FMdlRes := MessageDlgEx(Msg, mtError, [mbYes, mbYesToAll, mbNo],  Self);
    end
    else
    begin
      Msg := rsProgressFrmError3 + ' "' + APackageName + '". ';
      MessageDlgEx(Msg, mtError, [mbOk], Self);
      FMdlRes := mrNo;
    end;
  end;

  if FMdlRes = mrNo then
  begin
    FCanClose := True;
    FSuccess := False;
    PackageInstaller.OnPackageInstallProgress := nil;
    PackageInstaller.OnPackageInstallError := nil;
    PackageInstaller.NeedToBreak := True;
    Close;
  end;
  Application.ProcessMessages;
end;

procedure TProgressFrm.DoOnPackageInstallCompleted(Sender: TObject; ANeedToRebuild: Boolean; AInstallStatus: TInstallStatus);
begin
  FCanClose := True;
  FSuccess := True;
  FNeedToRebuild := ANeedToRebuild;
  FInstallStatus := AInstallStatus;
  Sleep(1000);
  Close;
end;

procedure TProgressFrm.DoOnPackageUpdateProgress(Sender: TObject; AUPackageName,
  AUPackageURL: String; ACnt, ATotCnt: Integer; AUTyp: Integer; AUErrMsg: String);
var
  Node: PVirtualNode;
  Data: PData;
begin
  FCnt := ACnt;
  FTotCnt := ATotCnt;
  Caption := rsProgressFrmCaption3 + '(' + IntToStr(ACnt) + '/' + IntToStr(ATotCnt) +')' + rsProgressFrmCaption4;
  Node := FVST.AddChild(nil);
  Data := FVST.GetNodeData(Node);
  case AUtyp of
    0: Data^.FName := rsProgressFrmInfo2 + ' "' + AUPackageName + '"( ' + AUPackageURL + ')';
    1: Data^.FName := rsProgressFrmInfo1;
    2: if AUErrMsg <> '' then
         Data^.FName := rsProgressFrmError7 + ': ' + AUErrMsg + '';
       else
         Data^.FName := rsProgressFrmError7 + '.';
  end;
  Data^.FImageIndex := AUTyp;
  FVST.TopNode := Node;
end;

procedure TProgressFrm.DoOnPackageUpdateCompleted(Sender: TObject;
  AUSuccess: Boolean);
var
  Data: PData;
  Node: PVirtualNode;
begin
  if AUSuccess then
  begin
    Node := FVST.AddChild(nil);
    Data := FVST.GetNodeData(Node);
    Data^.FName := rsProgressFrmInfo3;
    Data^.FImageIndex := 0;
    FVST.TopNode := Node;
    FVST.RepaintNode(Node);
    Sleep(3000);
    SetupControls(0);
    Application.ProcessMessages;
  end
  else
  begin
    Node := FVST.AddChild(nil);
    Data := FVST.GetNodeData(Node);
    Data^.FName := rsProgressFrmError8;
    Data^.FImageIndex := 2;
    FVST.TopNode := Node;
    FVST.RepaintNode(Node);
    Sleep(2000);
    FCanClose := True;
    FSuccess := False;
    Close;
  end;
end;

procedure TProgressFrm.bCancelClick(Sender: TObject);
begin
  Self.Caption := rsProgressFrmInfo4;
  bCancel.Enabled := False;
  FCanClose := True;
  case FType of
    0: begin
         FSuccess := False;
         PackageDownloader.OnPackageDownloadProgress := nil;
         PackageDownloader.OnPackageDownloadError := nil;
         PackageDownloader.CancelDownloadPackages;
       end;
    1: begin
         FSuccess := False;
         PackageUnzipper.OnZipProgress := nil;
         PackageUnzipper.OnZipError := nil;
         PackageUnzipper.StopUnZip;
       end;
    2: begin
         FSuccess := False;
         PackageInstaller.OnPackageInstallProgress := nil;
         PackageInstaller.OnPackageInstallError := nil;
         PackageInstaller.StopInstall;
       end;
    3: begin
         FSuccess := False;
         PackageDownloader.OnPackageUpdateProgress := nil;
         PackageDownloader.OnPackageDownloadError := nil;
         PackageDownloader.CancelUpdatePackages;
       end;
   end;
  Close;
end;

procedure TProgressFrm.SetupControls(const AType: Integer);
begin
  FType := AType;
  case AType of
    0: begin  //download
         Caption := rsProgressfrmCaption0 + rsProgressFrmCaption4;
         pnLabels.Visible := True;
         pnLabels.BringToFront;
         FVST.Visible := False;
         lbReceived.Caption := rsProgressFrmlbReceivedCaption0;
         lbReceivedTotal.Caption := rsProgressFrmlbReceivedTotalCaption0;
         cbExtractOpen.Caption := rsProgressFrmcbExtractOpenCaption0;
         cbExtractOpen.Visible := (PackageAction <> paInstall) and (PackageAction <> paUpdate);
       end;
    1: begin //extract
         Caption := rsProgressfrmCaption1 + rsProgressFrmCaption4;
         pnLabels.Visible := True;
         FVST.Visible := False;
         lbReceived.Caption := rsProgressFrmlbReceivedCaption1;
         lbReceivedTotal.Caption := rsProgressFrmlbReceivedTotalCaption1;
         cbExtractOpen.Caption := rsProgressFrmcbExtractOpenCaption1;
         cbExtractOpen.Checked := False;
         cbExtractOpen.Visible := (PackageAction <> paInstall) and (PackageAction <> paUpdate);
       end;
    2: begin //install
         Caption := rsProgressfrmCaption2 + rsProgressFrmCaption4;
         pnLabels.Visible := False;
         FVST.Visible := True;
         cbExtractOpen.Visible := False;
         PackageInstaller := TPackageInstaller.Create;
         PackageInstaller.OnPackageInstallProgress := @ProgressFrm.DoOnPackageInstallProgress;
         PackageInstaller.OnPackageInstallError := @ProgressFrm.DoOnPackageInstallError;
         PackageInstaller.OnPackageInstallCompleted := @ProgressFrm.DoOnPackageInstallCompleted;
       end;
    3: begin //update
         Caption := rsProgressFrmCaption3 + rsProgressFrmCaption4;
         pnLabels.Visible := False;
         FVST.Visible := True;
         cbExtractOpen.Visible := False;
       end;
  end;
  lbPackage.Caption := rsProgressFrmlbPackageCaption;
  lbSpeed.Caption := rsProgressFrmlbSpeedCaption;
  lbSpeedData.Caption := rsProgressFrmlbSpeedCalcCaption;
  lbEllapsed.Caption := rsProgressFrmlbEllapsedCaption;
  lbRemaining.Caption := rsProgressFrmlbRemainingCaption;
  pb.Top := lbReceived.Top + lbReceived.Height + 1;
  pbTotal.Top := lbReceivedTotal.Top + lbReceivedTotal.Height + 1;
  bCancel.Top := (pnButtons.Height - bCancel.Height) div 2;
  cbExtractOpen.Top := bCancel.Top + (bCancel.Height - cbExtractOpen.Height) div 2;
end;

procedure TProgressFrm.VSTGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
  Column: TColumnIndex; TextType: TVSTTextType; var CellText: String);
var
  Data: PData;
begin
  Data := FVST.GetNodeData(Node);
  if Column = 0 then
    CellText := Data^.FName;
end;

procedure TProgressFrm.VSTGetImageIndex(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
  var Ghosted: Boolean; var ImageIndex: Integer);
var
  Data: PData;
begin
  Data := FVST.GetNodeData(Node);
  if Column = 0 then
    ImageIndex := Data^.FImageIndex;
end;

procedure TProgressFrm.VSTPaintText(Sender: TBaseVirtualTree;
  const TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  TextType: TVSTTextType);
var
  Data: PData;
begin
  Data := FVST.GetNodeData(Node);
  if Data^.FImageIndex = -1 then
    TargetCanvas.Font.Color := clGray;
end;

procedure TProgressFrm.VSTFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
var
  Data: PData;
begin
  Data := FVST.GetNodeData(Node);
  Finalize(Data^);
end;

end.

