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
*   Inc., 51 Franklin Street - Fifth Floor, Boston, MA 02110-1335, USA.   *
*                                                                         *
***************************************************************************

Author: Balázs Székely
}
unit opkman_progressfrm;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, laz.VirtualTrees,
  // LCL
  Forms, Controls, Graphics, Dialogs, ComCtrls, StdCtrls, ExtCtrls, ButtonPanel,
  // OpkMan
  opkman_installer, opkman_common, opkman_const, opkman_downloader, opkman_zipper,
  opkman_options, opkman_maindm;

type

  { TProgressFrm }

  TProgressFrm = class(TForm)
    cbExtractOpen: TCheckBox;
    lbElapsed: TLabel;
    lbElapsedData: TLabel;
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
    bpCancel: TButtonPanel;
    pnLabels: TPanel;
    tmWait: TTimer;
    procedure bCancelClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: char);
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
    FVST: TLazVirtualStringTree;
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
      ACurPos, ACurSize, ATotPos, ATotSize: Int64; AElapsed, ARemaining, ASpeed: LongInt);
    procedure DoOnPackageDownloadError(Sender: TObject; APackageName: String; const AErrMsg: String = '');
    procedure DoOnPackageDownloadCompleted(Sender: TObject);
    procedure DoOnZipProgress(Sender: TObject; AZipfile: String; ACnt, ATotCnt: Integer;
      ACurPos, ACurSize, ATotPos, ATotSize: Int64; AElapsed, ARemaining, ASpeed: LongInt);
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
  if not Options.UseDefaultTheme then
    Self.Color := clBtnFace;
  FVST := TLazVirtualStringTree.Create(nil);
  with FVST do
  begin
    Parent := Self;
    Align := alClient;
    Anchors := [akLeft, akTop, akRight];
    Images := MainDM.Images;
    if not Options.UseDefaultTheme then
      Color := clBtnFace;
    DefaultNodeHeight := Scale96ToForm(25);
    Indent := 0;
    TabOrder := 1;
    DefaultText := '';
    Header.AutoSizeIndex := 0;
    Header.Height := Scale96ToForm(25);
    Visible := False;
    Colors.BorderColor := clBlack;
    BorderSpacing.Top := Scale96ToForm(10);
    BorderSpacing.Left := Scale96ToForm(10);
    BorderSpacing.Right := Scale96ToForm(10);
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

procedure TProgressFrm.DoOnPackageDownloadProgress(Sender: TObject; AFrom, ATo: String;
  ACnt, ATotCnt: Integer; ACurPos, ACurSize, ATotPos, ATotSize: Int64;
  AElapsed, ARemaining, ASpeed: LongInt);
begin
  Caption := rsProgressfrm_Caption0 + ' (' + IntToStr(ACnt) + '/' + IntToStr(ATotCnt) +')' + rsProgressFrm_Caption4;
  lbPackageData.Caption := ExtractFileName(ATo);
  lbSpeedData.Caption := FormatSpeed(ASpeed);
  lbSpeedData.Update;
  lbElapsedData.Caption := SecToHourAndMin(AElapsed);
  lbElapsedData.Update;
  lbRemainingData.Caption := SecToHourAndMin(ARemaining);
  lbRemainingData.Update;
  if ACurSize > 0 then
    lbReceived.Caption := rsProgressFrm_lbReceived_Caption0 + '  ' + FormatSize(ACurPos) + ' / ' + FormatSize(ACurSize)
  else
    lbReceived.Caption := rsProgressFrm_lbReceived_Caption0 + '  ' + FormatSize(ACurPos) + ' / ' + rsProgressFrm_Caption5;
  lbReceived.Update;
  if ACurSize > 0 then
    pb.Position := Round((ACurPos/ACurSize) * 100);
  pb.Update;
  lbReceivedTotal.Caption := rsProgressFrm_lbReceivedTotal_Caption0 + '  ' + FormatSize(ATotPos) + ' / ' + FormatSize(ATotSize);
  lbReceivedTotal.Update;
  if ATotSize > 0 then
    pbTotal.Position := Round((ATotPos/ATotSize) * 100);
  pbTotal.Update;
  FCnt := ACnt;
  FTotCnt := ATotCnt;
  Application.ProcessMessages;
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
      Msg := rsProgressFrm_Error0 + ' "' + APackageName + '". ' + rsProgressFrm_Error1 + sLineBreak  + '"' +
             AErrMsg + '"' + sLineBreak + rsProgressFrm_Conf0;
      FMdlRes := MessageDlgEx(Msg, mtError, [mbYes, mbYesToAll, mbNo], Self);
    end
    else
    begin
      Msg :=  rsProgressFrm_Error0 + ' "' + APackageName + '". ' + rsProgressFrm_Error1 + sLineBreak  + '"' + AErrMsg + '"';
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
    PackageDownloader.Cancel;
    Close;
  end;
end;

procedure TProgressFrm.DoOnPackageDownloadCompleted(Sender: TObject);
begin
  FCanClose := True;
  if FMdlRes <> mrNo then
    FSuccess := True;
  Close;
end;

procedure TProgressFrm.DoOnZipProgress(Sender: TObject; AZipfile: String;
  ACnt, ATotCnt: Integer;  ACurPos, ACurSize, ATotPos, ATotSize: Int64;
  AElapsed, ARemaining, ASpeed: LongInt);
begin
  Caption := rsProgressfrm_Caption1 + ' (' + IntToStr(ACnt) + '/' + IntToStr(ATotCnt) +')' + rsProgressFrm_Caption4;
  lbPackageData.Caption := AZipFile;
  lbSpeedData.Caption := FormatSpeed(ASpeed);
  lbSpeedData.Update;
  lbElapsedData.Caption := SecToHourAndMin(AElapsed);
  lbElapsedData.Update;
  lbRemainingData.Caption := SecToHourAndMin(ARemaining);
  lbRemainingData.Update;
  lbReceived.Caption := rsProgressFrm_lbReceived_Caption1 + '  ' + FormatSize(ACurPos) + ' / ' + FormatSize(ACurSize);
  lbReceived.Update;
  if ACurSize > 0 then
    pb.Position := Round((ACurPos/ACurSize) * 100);
  pb.Update;
  lbReceivedTotal.Caption := rsProgressFrm_lbReceivedTotal_Caption1 + '  ' + FormatSize(ATotPos) + ' / ' + FormatSize(ATotSize);
  lbReceivedTotal.Update;
  if ATotSize > 0 then
    pbTotal.Position := Round((ATotPos/ATotSize) * 100);
  pbTotal.Update;
  FCnt := ACnt;
  FTotCnt := ATotCnt;
  Application.ProcessMessages;
end;

procedure TProgressFrm.DoOnZipError(Sender: TObject; APackageName: String; const AErrMsg: String);
var
  Msg: String;
begin
  if ((FMdlRes = mrNone) or (FMdlRes = mrYes) or (FMdlRes = mrNo)) then
  begin
    if (FCnt < FTotCnt) then
    begin
      Msg := rsProgressFrm_Error2 + ' "' + APackageName + '". ' + rsProgressFrm_Error1 + sLineBreak + '" ' +
             AErrMsg + '"' + sLineBreak + rsProgressFrm_Conf0;
      FMdlRes := MessageDlgEx(Msg, mtError, [mbYes, mbYesToAll, mbNo],  Self);
    end
    else
    begin
      Msg := rsProgressFrm_Error2 + ' "' + APackageName + '". ' + rsProgressFrm_Error1 + sLineBreak + '"' + AErrMsg + '"';
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
  Application.ProcessMessages;
  FCanClose := True;
  if FMdlRes <> mrNo then
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
  Caption := rsProgressFrm_Caption2 + ' (' + IntToStr(ACnt) + '/' + IntToStr(ATotCnt) + ')' + rsProgressFrm_Caption4;
  Node := FVST.AddChild(nil);
  Data := FVST.GetNodeData(Node);
  case AInstallMessage of
    imOpenPackage:
       begin
         Data^.FName := rsProgressFrm_Info5 + ' "' + APackageName + '".';
         Data^.FImageIndex := IMG_INFO;
       end;
    imOpenPackageSuccess:
       begin
         Data^.FName := rsProgressFrm_Info1;
         Data^.FImageIndex := IMG_OK;
       end;
    imCompilePackage:
       begin
         Data^.FName := rsProgressFrm_Info6 + ' "' + APackageName + '".';
         Data^.FImageIndex := IMG_INFO;
       end;
    imCompilePackageSuccess:
       begin
         Data^.FName := rsProgressFrm_Info1;
         Data^.FImageIndex := IMG_OK;
       end;
    imInstallPackage:
       begin
         Data^.FName := rsProgressFrm_Info0 + ' "' + APackageName + '".';
         Data^.FImageIndex := IMG_INFO;
       end;
    imInstallPackageSuccess:
       begin
         Data^.FName := rsProgressFrm_Info1;
         Data^.FImageIndex := IMG_OK;
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
  Data^.FImageIndex := IMG_ERROR;
  FVST.TopNode := Node;
  if ((FMdlRes = mrNone) or (FMdlRes = mrYes) or (FMdlRes = mrNo)) then
  begin
    if (FCnt < FTotCnt) then
    begin
      Msg := rsProgressFrm_Error3 + ' "' + APackageName + '". ' + rsProgressFrm_Conf0;
      FMdlRes := MessageDlgEx(Msg, mtError, [mbYes, mbYesToAll, mbNo],  Self);
    end
    else
    begin
      Msg := rsProgressFrm_Error3 + ' "' + APackageName + '". ';
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
  if FMdlRes <> mrNo then
    FSuccess := True;
  FNeedToRebuild := ANeedToRebuild;
  FInstallStatus := AInstallStatus;
  Application.ProcessMessages;
  Sleep(1000);
  Close;
end;

procedure TProgressFrm.DoOnPackageUpdateProgress(Sender: TObject; AUPackageName,
  AUPackageURL: String; ACnt, ATotCnt: Integer; AUTyp: Integer; AUErrMsg: String);
const
  IMG_UTyp: array[0..2] of Integer = (IMG_INFO, IMG_OK, IMG_ERROR);
var
  Node: PVirtualNode;
  Data: PData;
begin
  FCnt := ACnt;
  FTotCnt := ATotCnt;
  Caption := rsProgressFrm_Caption3 + ' (' + IntToStr(ACnt) + '/' + IntToStr(ATotCnt) +')' + rsProgressFrm_Caption4;
  Node := FVST.AddChild(nil);
  Data := FVST.GetNodeData(Node);
  case AUtyp of
    0: Data^.FName := Format(rsProgressFrm_Info2, [AUPackageName, AUPackageURL]);
    1: Data^.FName := rsProgressFrm_Info1;
    2: if AUErrMsg <> '' then
         Data^.FName := rsProgressFrm_Error5 + ': ' + AUErrMsg + '';
       else
         Data^.FName := rsProgressFrm_Error5 + '.';
  end;
  Data^.FImageIndex := IMG_UTyp[AUTyp];
  FVST.TopNode := Node;
  Application.ProcessMessages;
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
    Data^.FName := rsProgressFrm_Info3;
    Data^.FImageIndex := IMG_INFO;
    FVST.TopNode := Node;
    FVST.RepaintNode(Node);
    Application.ProcessMessages;
    Sleep(2000);
    SetupControls(0);
  end
  else
  begin
    Node := FVST.AddChild(nil);
    Data := FVST.GetNodeData(Node);
    Data^.FName := rsProgressFrm_Error6;
    Data^.FImageIndex := IMG_ERROR;
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
  Self.Caption := rsProgressFrm_Info4;
  bpCancel.CancelButton.Enabled := False;
  FCanClose := True;
  case FType of
    0: begin
         FSuccess := False;
         PackageDownloader.OnPackageDownloadProgress := nil;
         PackageDownloader.OnPackageDownloadError := nil;
         PackageDownloader.Cancel;
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
         PackageDownloader.Cancel;
       end;
   end;
  Close;
end;

procedure TProgressFrm.SetupControls(const AType: Integer);
begin
  FType := AType;
  case AType of
    0: begin  //download
         Caption := rsProgressFrm_Caption0 + rsProgressFrm_Caption4;
         pnLabels.Visible := True;
         pnLabels.BringToFront;
         FVST.Visible := False;
         lbReceived.Caption := rsProgressFrm_lbReceived_Caption0;
         lbReceivedTotal.Caption := rsProgressFrm_lbReceivedTotal_Caption0;
         cbExtractOpen.Caption := rsProgressFrm_cbExtractOpen_Caption0;
         cbExtractOpen.Visible := (PackageAction <> paInstall) and (PackageAction <> paUpdate);
       end;
    1: begin //extract
         Caption := rsProgressFrm_Caption1 + rsProgressFrm_Caption4;
         pnLabels.Visible := True;
         FVST.Visible := False;
         lbReceived.Caption := rsProgressFrm_lbReceived_Caption1;
         lbReceivedTotal.Caption := rsProgressFrm_lbReceivedTotal_Caption1;
         cbExtractOpen.Caption := rsProgressFrm_cbExtractOpen_Caption1;
         cbExtractOpen.Checked := False;
         cbExtractOpen.Visible := (PackageAction <> paInstall) and (PackageAction <> paUpdate);
       end;
    2: begin //install
         Caption := rsProgressFrm_Caption2 + rsProgressFrm_Caption4;
         pnLabels.Visible := False;
         FVST.Visible := True;
         cbExtractOpen.Visible := False;
         PackageInstaller := TPackageInstaller.Create;
         PackageInstaller.OnPackageInstallProgress := @ProgressFrm.DoOnPackageInstallProgress;
         PackageInstaller.OnPackageInstallError := @ProgressFrm.DoOnPackageInstallError;
         PackageInstaller.OnPackageInstallCompleted := @ProgressFrm.DoOnPackageInstallCompleted;
       end;
    3: begin //update
         Caption := rsProgressFrm_Caption3 + rsProgressFrm_Caption4;
         pnLabels.Visible := False;
         FVST.Visible := True;
         cbExtractOpen.Visible := False;
       end;
  end;
  lbPackage.Caption := rsProgressFrm_lbPackage_Caption;
  lbSpeed.Caption := rsProgressFrm_lbSpeed_Caption;
  lbSpeedData.Caption := rsProgressFrm_lbSpeedCalc_Caption;
  lbElapsed.Caption := rsProgressFrm_lbElapsed_Caption;
  lbRemaining.Caption := rsProgressFrm_lbRemaining_Caption;
  //pb.Top := lbReceived.Top + lbReceived.Height + 1;
  //pbTotal.Top := lbReceivedTotal.Top + lbReceivedTotal.Height + 1;
  //bCancel.Top := (bpCancel.Height - bCancel.Height) div 2;
  //cbExtractOpen.Top := bCancel.Top + (bCancel.Height - cbExtractOpen.Height) div 2;
  Application.ProcessMessages;
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

