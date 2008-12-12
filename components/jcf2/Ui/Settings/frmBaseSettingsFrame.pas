{(*}
(*------------------------------------------------------------------------------
 Delphi Code formatter source code 

The Original Code is frmBaseSettingsFrame.pas, released April 2000.
The Initial Developer of the Original Code is Anthony Steele. 
Portions created by Anthony Steele are Copyright (C) 1999-2008 Anthony Steele.
All Rights Reserved. 
Contributor(s): Anthony Steele.

The contents of this file are subject to the Mozilla Public License Version 1.1
(the "License"). you may not use this file except in compliance with the License.
You may obtain a copy of the License at http://www.mozilla.org/NPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied.
See the License for the specific language governing rights and limitations 
under the License.

Alternatively, the contents of this file may be used under the terms of
the GNU General Public License Version 2 or later (the "GPL") 
See http://www.gnu.org/licenses/gpl.html
------------------------------------------------------------------------------*)
{*)}

unit frmBaseSettingsFrame;

{ SetttingsFrame
  AFS 29 Dec 1999
  Subclass of TFrame with common interface for settings }

{$I JcfGlobal.inc}

interface

uses
  {delphi }
  Windows, Classes, Controls,
  Forms, ShellAPI, SysUtils,
  { local }
  frDrop;

type
  TfrSettingsFrame = class(TFrameDrop)
  private
    // event handler
    fcOnChange: TNotifyEvent;

  protected
    fiHelpContext: THelpContext;

    procedure CallOnChange;

  public
    constructor Create(aOwner: TComponent); override;

    procedure Read; virtual; abstract;
    procedure Write; virtual; abstract;

    procedure ShowContextHelp;

    property OnChange: TNotifyEvent Read fcOnChange Write fcOnChange;
  end;

  TSettingsFrameClass = class of TfrSettingsFrame;

const
  GUI_PAD = 3;

implementation

uses JCFHelp;

{$ifdef FPC}
  {$R *.lfm}
{$else}
  {$R *.dfm}
{$endif}

constructor TfrSettingsFrame.Create(aOwner: TComponent);
begin
  inherited;
  fcOnChange := nil;
  fiHelpContext := 0;
end;

procedure TfrSettingsFrame.CallOnChange;
begin
  if Assigned(fcOnChange) then
    fcOnChange(self);
end;

procedure TfrSettingsFrame.ShowContextHelp;
var
  liHelpContext: integer;
begin
  liHelpContext := fiHelpContext;
  if liHelpContext <= 0 then
    liHelpContext := HELP_MAIN;

  try
    Application.HelpContext(liHelpContext);
  except
    if FileExists(Application.HelpFile) then
      ShellExecute(Handle, 'open', PChar(Application.HelpFile), nil, nil, SW_SHOWNORMAL);
  end;
end;

end.
