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

  Author: Mattias Gaertner

  Abstract:
    TJITForm - just-in-time form.
    TJITDataModule = just-in-time datamodule
    
    This TForm descendent is used by the IDE as a template for creating forms
    at run time (the designed forms).
    Because the IDE does wild things with this form, like creating an own class
    for each TJITForm and dynamically creating methods for it, you can't use
    some special compiling modes like -pg (gprof) with it.
    Therefore this unit is kept in a directory of its own.
}
unit JITForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls;

type
  // TJITForm is a template TForm descendent class that can be altered at
  // runtime
  // OBSOLETE:
  {TJITForm = class(TForm)
  public
  end;

  TJITFormClass = class of TJITForm;
  

  // TJITDataModule is a template TDataModule descendent class that can be
  // altered at runtime
  // OBSOLETE:
  TJITDataModule = class(TDataModule)
  public
  end;

  TJITDataModuleClass = class of TJITDataModule;}


  // TPersistentWithTemplates
  TPersistentWithTemplates = class(TPersistent)
  published
    // the dummy template 'procedure of object' for all events
    procedure DoNothing;
  end;
  
  TJITClass = class of TPersistent;

procedure SetComponentDesignMode(AComponent: TComponent; Value: Boolean);
  
implementation

// Define a dummy component to set the csDesigning flag which can not be set
// by a TForm, because SetDesigning is protected.
type
  TSetDesigningComponent = class(TComponent)
  public
    class procedure SetDesigningOfComponent(AComponent: TComponent; Value: Boolean);
  end;

procedure SetComponentDesignMode(AComponent: TComponent; Value: Boolean);
begin
  TSetDesigningComponent.SetDesigningOfComponent(AComponent,true);
end;

procedure TSetDesigningComponent.SetDesigningOfComponent(
  AComponent: TComponent; Value: Boolean);
begin
  AComponent.SetDesigning(Value);
end;

{ TPersistentWithTemplates }
{$IFOPT S+}{$DEFINE StackCheckOn}{$ENDIF}
{$S-}
procedure TPersistentWithTemplates.DoNothing;
// this is the template procedure for all events of the designed components
begin
  // !!! do not write any code in here !!!
end;
{$IFDEF StackCheckOn}{$S+}{$ENDIF}

end.

