// included by dialogs.pp

{******************************************************************************
                                  TFileDialog
 ******************************************************************************

 *****************************************************************************
 *                                                                           *
 *  This file is part of the Lazarus Component Library (LCL)                 *
 *                                                                           *
 *  See the file COPYING.LCL, included in this distribution,                 *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************
}

{------------------------------------------------------------------------------}
{  TFileDialog Create                                                          }
{------------------------------------------------------------------------------}
constructor TFileDialog.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fCompStyle := csFileDialog;
  FFiles := TStringList.Create;
  FHistoryList:=TStringList.Create;
end;

{------------------------------------------------------------------------------}
{  TFileDialog Destroy                                                         }
{------------------------------------------------------------------------------}
destructor TFileDialog.Destroy;
begin
  FHistoryList.Free;
  FFiles.Free;
  inherited;
end;

{------------------------------------------------------------------------------}
{  TFileDialog Execute                                                         }
{------------------------------------------------------------------------------}
function TFileDialog.Execute : boolean;
begin
  FOldWorkingDir:=GetCurrentDir;
  if FInitialDir<>'' then SetCurrentDir(FInitialDir);
  try
    FUserChoice := mrNone;
    CNSendMessage(LM_CREATE, Self, nil);
    Result:= DoExecute;
    CNSendMessage(LM_DESTROY, Self, nil);
    FHandle := 0;
  finally
    SetCurrentDir(FOldWorkingDir);
  end;
end;

{------------------------------------------------------------------------------}
{  TFileDialog DoExecute                                                         }
{------------------------------------------------------------------------------}
procedure TFileDialog.SetHistoryList(const AValue: TStrings);
begin
  FHistoryList.Assign(AValue);
end;

procedure TFileDialog.SetDefaultExt(const AValue: string);
begin
  FDefaultExt:=AValue;
  if (FDefaultExt<>'') and (FDefaultExt[1]<>'.') then
    FDefaultExt:='.'+FDefaultExt;
end;

{------------------------------------------------------------------------------}
{  TFileDialog DoExecute                                                         }
{------------------------------------------------------------------------------}
function TFileDialog.DoExecute : boolean;
begin
  Result:= inherited DoExecute;
end;

{------------------------------------------------------------------------------}
{  TFileDialog SetFilter                                                       }
{------------------------------------------------------------------------------}
procedure TFileDialog.SetFilter(value : string);
begin
  FFilter := value;  // make sure this is defined first before the CNSendMessage
end;

{------------------------------------------------------------------------------}
{  TFileDialog SetFileName                                                     }
{------------------------------------------------------------------------------}
procedure TFileDialog.SetFileName(value : string);
begin
  FFileName := value;  // make sure this is defined first before the CNSendMessage
end;

{******************************************************************************
                                  TOpenDialog
 ******************************************************************************}

{------------------------------------------------------------------------------
  Method: TOpenDialog.DoExecute
  Params:  none
  Returns: true if valid was selected

  Starts dialogs and lets user choose a filename.
 ------------------------------------------------------------------------------}
function TOpenDialog.DoExecute: boolean;

  procedure DereferenceLinks;
  var i: integer;
  begin
    if Filename<>'' then
      Filename:=ExpandFilename(Filename);
    if Files<>nil then begin
      for i:=0 to Files.Count-1 do begin
        if Files[i]<>'' then
          Files[i]:=ExpandFilename(Files[i]);
      end;
    end;
  end;

begin
  Result:=inherited DoExecute;
  if (not (ofNoDereferenceLinks in Options)) then begin
    DereferenceLinks;
  end;
  if (not (ofNoChangeDir in Options)) then begin
    if (ExtractFilePath(Filename)<>'') then
      InitialDir:=ExtractFilePath(Filename)
    else if (Files.Count>0) and (ExtractFilePath(Files[0])<>'') then
      InitialDir:=ExtractFilePath(Files[0]);
  end;
  if Result then begin
    if (DefaultExt<>'') and (ExtractFileExt(Filename)='')
    and (not FileExists(Filename)) then begin
      Filename:=Filename+DefaultExt;
    end;
    if (ofOverwritePrompt in Options) and FileExists(Filename) then
    begin
      Result:=MessageDlg(rsfdOverwriteFile,Format(rsfdFileAlreadyExists,[FileName]),
               mtConfirmation,[mbOk,mbCancel],0)=mrOk;
      if not Result then exit;
    end;
    if (ofPathMustExist in Options)
    and (not FileExists(ExtractFilePath(Filename))) then begin
      Result:=false;
      MessageDlg(rsfdPathMustExist,Format(rsfdPathNoExist,[ExtractFilePath(Filename)]),
                 mtError,[mbCancel],0);
      exit;
    end;
    if (ofFileMustExist in Options)
    and (not FileExists(Filename)) then
    begin
      Result:=false;
      MessageDlg(rsfdFileMustExist,Format(rsfdFileNotExist,[FileName]),mtError,[mbCancel],0);
      exit;
    end;
    if (ofNoReadOnlyReturn in Options)
    and (not FileIsWritable(Filename)) then begin
      Result:=false;
      MessageDlg(rsfdFileReadOnlyTitle,Format(rsfdFileReadOnly,[FileName]),mtError,[mbCancel],0);
      exit;
    end;
  end;
end;

{------------------------------------------------------------------------------
  Method: TOpenDialog.Create
  Params:  AOwner: the owner of the class
  Returns: Nothing

  Constructor for the class.
 ------------------------------------------------------------------------------}
constructor TOpenDialog.Create (AOwner : TComponent);
begin
  inherited Create(AOwner);
  FTitle:= rsfdOpenFile;
  FOptions := [ofEnableSizing, ofViewDetail];
end;

{******************************************************************************
                                  TSaveDialog
 ******************************************************************************}
{------------------------------------------------------------------------------
  Method: TSaveDialog.Create
  Params:  AOwner: the owner of the class
  Returns: Nothing

  Constructor for the class.
 ------------------------------------------------------------------------------}
constructor TSaveDialog.Create (AOwner : TComponent);
begin
  inherited Create(AOwner);
  fCompStyle := csFileDialog;
  FTitle:= rsfdFileSaveAs;
end;

{ =============================================================================

  $Log$
  Revision 1.7  2003/02/28 10:21:16  mattias
  lcl localization from Olivier

  Revision 1.6  2002/05/30 14:11:11  lazarus
  MG: added filters and history to TOpenDialog

  Revision 1.5  2002/05/29 21:44:38  lazarus
  MG: improved TCommon/File/OpenDialog, fixed TListView scrolling and broder

  Revision 1.4  2002/05/10 06:05:52  lazarus
  MG: changed license to LGPL

  Revision 1.3  2001/12/11 14:36:41  lazarus
  MG: started multiselection for TOpenDialog

  Revision 1.2  2001/03/27 11:11:13  lazarus
  MG: fixed mouse msg, added filedialog initialdir

  Revision 1.1  2000/07/13 10:28:25  michael
  + Initial import

  Revision 1.2  2000/04/17 19:50:06  lazarus
  Added some compiler stuff built into Lazarus.
  This depends on the path to your compiler being correct in the compileroptions
  dialog.
  Shane

  Revision 1.1  2000/04/02 20:49:56  lazarus
  MWE:
    Moved lazarus/lcl/*.inc files to lazarus/lcl/include

  Revision 1.9  2000/03/30 18:07:53  lazarus
  Added some drag and drop code
  Added code to change the unit name when it's saved as a different name.  Not perfect yet because if you are in a comment it fails.

  Shane

  Revision 1.8  2000/02/28 19:16:04  lazarus
  Added code to the FILE CLOSE to check if the file was modified.  HAven't gotten the application.messagebox working yet though.  It won't stay visible.
  Shane

  Revision 1.7  2000/02/22 22:19:49  lazarus
  TCustomDialog is a descendant of TComponent.
  Initial cuts a form's proper Close behaviour.

  Revision 1.6  2000/02/22 17:32:49  lazarus
  Modified the ShowModal call.
  For TCustomForm is simply sets the visible to true now and adds fsModal to FFormState.  In gtkObject.inc FFormState is checked.  If it contains fsModal then either gtk_grab_add or gtk_grab_remove is called depending on the value of VISIBLE.

  The same goes for TCustomDialog (open, save, font, color).
  I moved the Execute out of the individual dialogs and moved it into TCustomDialog and made it virtual because FONT needs to set some stuff before calling the inherited execute.
  Shane

  Revision 1.5  1999/12/10 00:47:01  lazarus
  MWE:
    Fixed some samples
    Fixed Dialog parent is no longer needed
    Fixed (Win)Control Destruction
    Fixed MenuClick

  Revision 1.4  1999/09/15 02:14:44  lazarus
  *** empty log message ***

  Revision 1.3  1999/08/07 17:59:18  lazarus

        buttons.pp   the DoLeave and DoEnter were connected to the wrong
                     event.

        The rest were modified to use the new CNSendMessage function.   MAH

  Revision 1.2  1999/07/31 06:39:24  lazarus

       Modified the IntCNSendMessage3 to include a data variable. It isn't used
       yet but will help in merging the Message2 and Message3 features.

       Adjusted TColor routines to match Delphi color format

       Added a TGdkColorToTColor routine in gtkproc.inc

       Finished the TColorDialog added to comDialog example.        MAH

 }
