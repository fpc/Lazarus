{%MainUnit gtk3widgets.pas}
const
  GTK_NOTEBOOK_CLASS_SIZE = SizeOf(TGtkNotebookClass);
  GTK_NOTEBOOK_INSTANCE_SIZE = SizeOf(TGtkNotebook);

procedure LCLGtkNotebookGetPreferredWidth(widget: PGtkWidget; min_width, nat_width: Pgint); cdecl;
var
  AControl: TGtk3Widget;
  ParentClass: PGtkWidgetClass;
begin
  if not Assigned(min_width) or not Assigned(nat_width) then
  begin
    DebugLn('Error: LCLGtkNotebookGetPreferredWidth invalid params.');
    Exit;
  end;

  min_width^ := 0;
  nat_width^ := 0;

  if not Gtk3IsWidget(widget) then
  begin
    DebugLn('Error: LCLGtkNotebookGetPreferredWidth widget param is not PGtkWidget.');
    Exit;
  end;

  ParentClass := PGtkWidgetClass(g_type_class_peek_parent(widget^.g_type_instance.g_class));
  if not Assigned(ParentClass) then
  begin
    DebugLn('Error: LCLGtkNotebookGetPreferredWidth cannot get ParentClass !');
    Exit;
  end;

  ParentClass^.get_preferred_width(widget, min_width, nat_width);

  AControl := TGtk3Widget(HwndFromGtkWidget(widget));
  if not Assigned(AControl) then
    Exit;

  // writeln('====> FGPW(',dbgsName(AControl.LCLObject),'): AutoSize=', AControl.LCLObject.AutoSize,' minW=',min_width^,' natW=',nat_width^,' LCLWidth=',AControl.LCLWidth,' LCLHeight=',AControl.LCLHeight);

  if AControl.LCLWidth = 0 then
  begin
    min_width^ := 0;
    nat_width^ := AControl.LCLObject.Width;
  end else
  begin
    min_width^ := 0;
    nat_width^ := AControl.LCLWidth;
  end;
  // writeln('<==== FGPW(',dbgsName(AControl.LCLObject),'): AutoSize=', AControl.LCLObject.AutoSize,' minW=',min_width^,' natW=',nat_width^,' LCLWidth=',AControl.LCLWidth,' LCLHeight=',AControl.LCLHeight,' LCLW=',AControl.LCLObject.Width,' LCLH=',AControl.LCLObject.Height);
end;

procedure LCLGtkNotebookGetPreferredHeight(widget: PGtkWidget; min_height, nat_height: Pgint); cdecl;
var
  AControl: TGtk3Widget;
  ParentClass: PGtkWidgetClass;
begin
  if not Assigned(min_height) or not Assigned(nat_height) then
  begin
    DebugLn('Error: LCLGtkNotebookGetPreferredHeight invalid params.');
    Exit;
  end;

  min_height^ := 0;
  nat_height^ := 0;

  if not Gtk3IsWidget(widget) then
  begin
    DebugLn('Error: LCLGtkNotebookGetPreferredHeight widget param is not PGtkWidget.');
    Exit;
  end;

  ParentClass := PGtkWidgetClass(g_type_class_peek_parent(widget^.g_type_instance.g_class));
  if not Assigned(ParentClass) then
  begin
    DebugLn('Error: LCLGtkNotebookGetPreferredHeight cannot get ParentClass !');
    Exit;
  end;

  ParentClass^.get_preferred_height(widget, min_height, nat_height);

  AControl := TGtk3Widget(HwndFromGtkWidget(widget));
  if not Assigned(AControl) then
    Exit;

  // writeln('====> FGPH(',dbgsName(AControl.LCLObject),'): AutoSize=', AControl.LCLObject.AutoSize,' minH=',min_height^,' natH=',nat_height^,' LCLWidth=',AControl.LCLWidth,' Height=',AControl.LCLHeight);

  if AControl.LCLHeight = 0 then
  begin
    min_height^ := 0; //, AControl.LCLObject.Height);
    nat_height^ := Max(0, AControl.LCLObject.Height);
  end else
  begin
    min_height^ := 0; // Max(36, AControl.LCLHeight);
    nat_height^ := Max(0, AControl.LCLHeight);
  end;
  // writeln('<==== FGPH(',dbgsName(AControl.LCLObject),'): AutoSize=', AControl.LCLObject.AutoSize,' minH=',min_height^,' natH=',nat_height^,' LCLWidth=',AControl.LCLWidth,' Height=',AControl.LCLHeight);
end;

procedure LCLGtkNotebookClassInit(klass: PGTypeClass; {%H-}data: Pointer); cdecl;
var
  AWidgetClass: PGtkWidgetClass;
begin
  AWidgetClass := PGtkWidgetClass(klass);
  AWidgetClass^.get_preferred_width := @LCLGtkNotebookGetPreferredWidth;
  AWidgetClass^.get_preferred_height := @LCLGtkNotebookGetPreferredHeight;
end;

procedure LCLGtkNotebookInstanceInit({%H-}instance: PGTypeInstance; {%H-}klass: PGTypeClass); cdecl;
begin
  //
end;

var
  LCLGtkNotebookType: TGType = 0;

function LCLGtkNotebookGetType: TGType; cdecl;
const
  lcl_Notebook_type_info: TGTypeInfo = (
    class_size: GTK_NOTEBOOK_CLASS_SIZE;
    base_init: nil;
    base_finalize: nil;
    class_init: @LCLGtkNotebookClassInit;
    class_finalize: nil;
    class_data: nil;
    instance_size: GTK_NOTEBOOK_INSTANCE_SIZE;
    n_preallocs: 0;
    instance_init: @LCLGtkNotebookInstanceInit;
    value_table: nil;
  );
begin
  if LCLGtkNotebookType = 0 then
    LCLGtkNotebookType := g_type_register_static(gtk_notebook_get_type, 'LCLGtkNotebook', @lcl_notebook_type_info, G_TYPE_FLAG_NONE);
  Result := LCLGtkNotebookType;
end;

function LCLGtkNotebookNew: PGtkNotebook;
begin                                                         {in gtk3 this is default}
  Result := PGtkNotebook(g_object_new(LCLGtkNotebookGetType(),'show-tabs',[gboolean(True), nil]));
end;




