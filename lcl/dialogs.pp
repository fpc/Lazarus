{
 /***************************************************************************
                               dialogs.pp
                               ----------
                Component Library Standard dialogs Controls


 ***************************************************************************/

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

{
@author(TMyClass - Michael A. Hess <author@emailaddress.com>)                       
@author(TMyOtherClass - Other Author Name <otherauthor@emailaddress.com>)                       
@created()
@lastmod()

Detailed description of the Unit.
} 

unit Dialogs;

{$mode objfpc}{$H+}

interface

uses Classes, Forms, Controls, VCLGlobals, LMessages;

//type
//   TDialogButtons = (mbYes, mbNo, mbOK, mbCancel, mbAbort, mbRetry,
//                    mbIgnore, mbAll, mbNoToAll, mbYesToAll, mbHelp);
//   TDialogButtonsSet = set of TDialogButtons;
type
   TMsgDlgType    = (mtWarning, mtError, mtInformation, mtConfirmation,
                     mtCustom);
   TMsgDlgBtn     = (mbYes, mbNo, mbOK, mbCancel, mbAbort, mbRetry, mbIgnore,
                     mbAll, mbNoToAll, mbYesToAll, mbHelp);
   TMsgDlgButtons = set of TMsgDlgBtn;

   
const
   mbYesNoCancel = [mbYes, mbNo, mbCancel];
   mbOKCancel = [mbOK, mbCancel];
   mbAbortRetryIgnore = [mbAbort, mbRetry, mbIgnore];

type
  TCommonDialog = class(TComponent)
  private
    FHandle : integer;
    FOnShow, FOnClose : TNotifyEvent;
    FTitle : string;
    FUserChoice: integer;
  protected
    function DoExecute : boolean; virtual;
  public
    FCompStyle : LongInt;
    constructor Create (AOwner : TComponent); override;
    function Execute : boolean; virtual;
    property Handle : integer read FHandle write FHandle;
    property Title : string read FTitle write FTitle;
    property UserChoice : integer read FUserChoice write FUserChoice;
    property OnClose : TNotifyEvent read FOnClose write FOnClose;
    property OnShow : TNotifyEvent read FOnShow write FOnShow;
  end;

  TFileDialog = class(TCommonDialog)
  private
    FFileName : String;
    FFiles: TStrings;
    FFilter: String;
    FInitialDir: string;
    FOldWorkingDir: string;
  protected
    function DoExecute : boolean; override;
    procedure SetFileName(value :String);
    procedure SetFilter(value :String);
  public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;
    function Execute : boolean; override;
    property Files: TStrings read FFiles;
  published
    property FileName : String read FFileName write SetFileName;
    property Filter : String read FFilter write SetFilter;
    property InitialDir: string read FInitialDir write FInitialDir;
  end;

  TOpenOption = (ofReadOnly, ofOverwritePrompt, ofHideReadOnly,
    ofNoChangeDir, ofShowHelp, ofNoValidate, ofAllowMultiSelect,
    ofExtensionDifferent, ofPathMustExist, ofFileMustExist, ofCreatePrompt,
    ofShareAware, ofNoReadOnlyReturn, ofNoTestFileCreate, ofNoNetworkButton,
    ofNoLongNames, ofOldStyleDialog, ofNoDereferenceLinks{, ofEnableIncludeNotify,
    ofEnableSizing});
  TOpenOptions = set of TOpenOption;
  
  TOpenDialog = class(TFileDialog)
  private
    FOptions: TOpenOptions;
  public
    constructor Create (AOwner : TComponent); override;
  published
    property Options: TOpenOptions read FOptions write FOptions
      {default [ofHideReadOnly, ofEnableSizing]};
  end;

  TSaveDialog  = class(TFileDialog)
  public
    constructor Create (AOwner : TComponent); override;
  end;


  TColorDialog = class(TCommonDialog)
  private
    FColor : TColor;
  public
    constructor Create (AOwner : TComponent); override;
  published
    property Color : TColor read FColor write FColor;
  end;

  TFontDialog = class(TCommonDialog)
  private
    FFontName : String;
  public
    constructor Create (AOwner : TComponent); override;
  published
    property FontName : String read FFontName write FFontName;
  end;

  function CreateMessageDialog(const aMsg: string; DlgType: TMsgDlgType;
              Buttons: TMsgDlgButtons): TForm;
  function MessageDlg(const aMsg: string; DlgType: TMsgDlgType;
              Buttons: TMsgDlgButtons; HelpCtx: Longint): Integer;
  function MessageDlg(const aCaption, aMsg: string; DlgType: TMsgDlgType;
              Buttons: TMsgDlgButtons; HelpCtx: Longint): Integer;
  function MessageDlgPos(const aMsg: string; DlgType: TMsgDlgType;
              Buttons: TMsgDlgButtons; HelpCtx: Longint; X, Y: Integer): Integer;
  function MessageDlgPosHelp(const aMsg: string; DlgType: TMsgDlgType;
              Buttons: TMsgDlgButtons; HelpCtx: Longint; X, Y: Integer;
              const HelpFileName: string): Integer;
  procedure ShowMessage(const aMsg: string);
  procedure ShowMessageFmt(const aMsg: string; Params: array of const);
  procedure ShowMessagePos(const aMsg: string; X, Y: Integer);

implementation

uses
  Buttons, StdCtrls, LCLlinux, Graphics, SysUtils;

resourcestring
   rsMbYes    = 'Yes';
   rsMbNo     = 'No';
   rsMbOK     = 'OK';
   rsMbCancel = 'Cancel';
   rsMbAbort  = 'Abort';
   rsMbRetry  = 'Retry';
   rsMbIgnore = 'Ignore';
   rsMbAll      = 'All?';
   rsMbNoToAll  = 'No to all';
   rsMbYesToAll = 'Yes to all';
   rsMbHelp     = 'Help';
   rsMtWarning      = 'Warning';
   rsMtError        = 'Error';
   rsMtInformation  = 'Information';
   rsMtConfirmation = 'Confirmation';
   rsMtCustom       = 'Custom';

const
   cMtCaption : array [TMsgDlgType] of string = 
        (rsMtWarning, rsMtError, rsMtInformation, rsMtConfirmation, rsMtCustom);
   cMbCaption : array [TMsgDlgbtn] of string = 
        (rsMbYes,    rsMbNo,  rsMbOK,      rsMbCancel,   rsMbAbort, rsMbRetry,
         rsMbIgnore, rsMbAll, rsMbNoToAll, rsMbYesToAll, rsMbHelp);
   cMbResult : array [TMsgDlgbtn] of TModalResult = 
//TODO: think of more modalresults!
        (mrYes, mrNo, mrOK, mrCAncel, mrAbort, mrRetry, mrIgnore, mrAll,
         mrNoToAll, mrYesToAll, 0);

type
   PCharArray32x32 = Array [0..36]  of PChar;

var
   mtImages   : Array [TMsgDlgType] of PCharArray32x32;
   mbImages   : array [TMsgDlgBtn] of PCharArray;


{$I commondialog.inc}
{$I filedialog.inc}
{$I colordialog.inc}
{$I fontdialog.inc}
{$I messagedialogpixmaps.inc}
{$I messagedialogs.inc}

procedure InitImages;
begin
   mbImages[mbYes]    := IMGOK_Check;
   mbImages[mbNo]     := IMGCancel_X;
   mbImages[mbOK]     := IMGOK_Check;
   mbImages[mbCancel] := IMGCancel_X;
   mbImages[mbAbort]  := IMGCancel_X; 
   mbImages[mbRetry]  := IMGClose;
   mbImages[mbIgnore] := IMGClose;
   mbImages[mbAll]    := IMGAll_Check;
   mbImages[mbNoToAll]:= IMGAll_Check;
   mbImages[mbYesToAll]:= IMGAll_Check;
   mbImages[mbHelp]   := IMGHELP;

   mtImages [MtWarning     ] := IMGWarning;
   mtImages [MtError       ] := IMGError;
   mtImages [MtInformation ] := IMGInfo;
   mtImages [MtConfirmation] := IMGConfirmation;
   mtImages [MtCustom      ] := IMGInfo;
end;

initialization

   InitImages;

finalization

end.

{ =============================================================================

  $Log$
  Revision 1.11  2002/05/10 06:05:49  lazarus
  MG: changed license to LGPL

  Revision 1.10  2002/01/25 19:42:56  lazarus
  Keith: Improved events and common dialogs on Win32

  Revision 1.9  2001/12/11 14:36:41  lazarus
  MG: started multiselection for TOpenDialog

  Revision 1.8  2001/11/01 18:48:51  lazarus
  Changed Application.Messagebox to use TMessageBox class.
  Added icon images for mtError and mtConfirmation
  Shane

  Revision 1.7  2001/07/31 18:40:24  lazarus
  MG: added unit info, arrow xpms, and many changes from jens arm

  Revision 1.6  2001/06/06 12:30:41  lazarus
  MG: bugfixes

  Revision 1.5  2001/03/27 11:11:13  lazarus
  MG: fixed mouse msg, added filedialog initialdir

  Revision 1.4  2001/03/03 00:48:03  lazarus
  + added support for message dialogs
  stoppok

  Revision 1.3  2000/08/10 10:55:45  lazarus
  Changed TCustomDialog to TCommonDialog
  Shane

  Revision 1.2  2000/08/09 18:32:10  lazarus
  Added more code for the find function.
  Shane

  Revision 1.1  2000/07/13 10:28:23  michael
  + Initial import

  Revision 1.10  2000/02/28 19:16:04  lazarus
  Added code to the FILE CLOSE to check if the file was modified.  HAven't gotten the application.messagebox working yet though.  It won't stay visible.
  Shane

  Revision 1.9  2000/02/22 22:19:49  lazarus
  TCustomDialog is a descendant of TComponent.
  Initial cuts a form's proper Close behaviour.

  Revision 1.8  2000/02/22 17:32:49  lazarus
  Modified the ShowModal call.
  For TCustomForm is simply sets the visible to true now and adds fsModal to FFormState.  In gtkObject.inc FFormState is checked.  If it contains fsModal then either gtk_grab_add or gtk_grab_remove is called depending on the value of VISIBLE.

  The same goes for TCustomDialog (open, save, font, color).
  I moved the Execute out of the individual dialogs and moved it into TCustomDialog and made it virtual because FONT needs to set some stuff before calling the inherited execute.
  Shane

  Revision 1.7  1999/12/10 00:47:01  lazarus
  MWE:
    Fixed some samples
    Fixed Dialog parent is no longer needed
    Fixed (Win)Control Destruction
    Fixed MenuClick

  Revision 1.6  1999/09/15 02:14:44  lazarus
  *** empty log message ***

  Revision 1.5  1999/08/16 18:45:38  lazarus
  Added a TFont Dialog plus minor additions.

  Shane Aug 16th 1999  14:07 CST

  Revision 1.4  1999/07/31 06:39:21  lazarus

       Modified the IntSendMessage3 to include a data variable. It isn't used
       yet but will help in merging the Message2 and Message3 features.

       Adjusted TColor routines to match Delphi color format

       Added a TGdkColorToTColor routine in gtkproc.inc

       Finished the TColorDialog added to comDialog example.        MAH

 }
