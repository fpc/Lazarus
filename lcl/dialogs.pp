{
 /***************************************************************************
                               dialogs.pp
                             -------------------
                             Component Library Standard dialogs Controls
                   Initial Revision  : 


 ***************************************************************************/

/***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 ***************************************************************************/
}

{
@author(TMyClass - Michael A. Hess <author@emailaddress.com>)                       
@author(TMyOtherClass - Other Author Name <otherauthor@emailaddress.com>)                       
@created()
@lastmod()

Detailed description of the Unit.
} 

unit dialogs;

{$mode objfpc}

interface

uses classes, Forms, Controls, vclGlobals, lmessages;

type
   TDialogButtons = (mbYes, mbNo, mbOK, mbCancel, mbAbort, mbRetry,
                    mbIgnore, mbAll, mbNoToAll, mbYesToAll, mbHelp);
   TDialogButtonsSet = set of TDialogButtons;
   
const
   mbYesNoCancel = [mbYes, mbNo, mbCancel];
   mbOKCancel = [mbOK, mbCancel];
   mbAbortRetryIgnore = [mbAbort, mbRetry, mbIgnore];


type
   TCustomDialog = class(TComponent)
   private
     FHandle : integer;
     FOnShow, FOnClose : TNotifyEvent;
     FTitle : string;
     FUserChoice: integer;
   protected
   public
      FCompStyle : LongInt;
      constructor Create (AOwner : TComponent); override;
      function DoExecute : boolean; virtual;
      function Execute : boolean; virtual;
      property Handle : integer read FHandle write FHandle;
      property Title : string read FTitle write FTitle;
      property UserChoice : integer read FUserChoice write FUserChoice;
      property OnClose : TNotifyEvent read FOnClose write FOnClose;
      property OnShow : TNotifyEvent read FOnShow write FOnShow;
   end;

   TFileDialog = class(TCustomDialog)
   private
      FFileName : String;
      FFilter: String;
   protected
      procedure SetFileName(value :String);
      procedure SetFilter(value :String);
   public
      function DoExecute : boolean; override;
      property FileName : String read FFileName write SetFileName;
      property Filter : String read FFilter write SetFilter;
   end;

   TOpenDialog = class(TFileDialog)
   public
      constructor Create (AOwner : TComponent); override;
   end;

   TSaveDialog  = class(TFileDialog)
   public
      constructor Create (AOwner : TComponent); override;
   end;


   TColorDialog = class(TCustomDialog)
   private
      FColor : TColor;
   public
      constructor Create (AOwner : TComponent); override;
      property Color : TColor read FColor write FColor;
   end;

   TFontDialog = class(TCustomDialog)
   private
      FFontName : String;
   public
      constructor Create (AOwner : TComponent); override;
      property FontName : String read FFontName write FFontName;
   end;

implementation

uses
  SysUtils;

{$I customdialog.inc}
{$I filedialog.inc}
{$I colordialog.inc}
{$I fontdialog.inc}
end.

{ =============================================================================

  $Log$
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
