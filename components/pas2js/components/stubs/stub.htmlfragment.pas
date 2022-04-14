unit stub.htmlfragment;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

Type
  TAllowUnrenderEvent = Procedure (Sender : TObject; var allowClose : Boolean) of object;

  { TCustomHTMLFragment }

  TCustomHTMLFragment = class(TDatamodule)
  private
    FAppendToParent: Boolean;
    FHTMLFile: String;
    FOnAllowUnrender: TAllowUnrenderEvent;
    FOnHTMLLoaded: TNotifyEvent;
    FOnRendered: TNotifyEvent;
    FOnRenderFail: TNotifyEvent;
    FOnUnRendered: TNotifyEvent;
    FParentID: String;
    FTemplateName: String;
    FTemplate : String;
    FIsRendered : Boolean;
  Protected
    Property ParentID : String Read FParentID Write FParentID;
    Property TemplateName : String Read FTemplateName Write FTemplateName;
    Property HTMLFileName : String Read FHTMLFile Write FHTMLFile;
    Property OnRendered : TNotifyEvent Read FOnRendered Write FOnrendered;
    Property OnUnRendered : TNotifyEvent Read FOnUnRendered Write FOnUnrendered;
    Property OnRenderFail : TNotifyEvent Read FOnRenderFail Write FOnrenderFail;
    Property OnHTMLLoaded : TNotifyEvent Read FOnHTMLLoaded Write FOnHTMLLoaded;
  //    Property AppendToParent : Boolean Read FAppendToParent Write FAppendToParent;
    Property OnAllowUnrender : TAllowUnrenderEvent Read FOnAllowUnrender Write FOnAllowUnrender;
  Public
    Procedure Render; virtual;
    Procedure UnRender;
    Procedure Show;
    Procedure Hide;
    Property IsRendered : Boolean Read FIsRendered;
  end;

  THTMLFragment = class(TCustomHTMLFragment)
  Published
    Property ParentID;
    Property TemplateName;
    Property HTMLFileName;
    Property OnRendered;
    Property OnHTMLLoaded;
  //    Property AppendToParent;
    Property OnAllowUnrender;
    Property OnUnrendered;
  end;


implementation

{ TCustomHTMLFragment }


procedure TCustomHTMLFragment.Render;
begin

end;

procedure TCustomHTMLFragment.UnRender;
begin

end;

procedure TCustomHTMLFragment.Show;
begin

end;

procedure TCustomHTMLFragment.Hide;
begin

end;

end.

