{%MainUnit ../printersdlgs.pp}


const
  SExecute = 'Execute';

{ TPageSetupDialog }

function TPageSetupDialog.DoExecute: Boolean;
var
  CocoaPrinter: TCocoaPrinter;
  pageLayout: NSPageLayout;
  pInfo: NSPrintInfo;
begin

  if not Assigned(Printer) then Exit;

  CocoaPrinter := Printer as TCocoaPrinter;
  pInfo := NSPrintInfo.sharedPrintInfo;
  pageLayout := NSPageLayout.pageLayout;
  result := pageLayout.runModalWithPrintInfo(pInfo)=NSOKButton;
  if result then
    CocoaPrinter.Validate;

  // TODO: make it through the delegate for sheet support
end;


{ TPrinterSetupDialog }

function TPrinterSetupDialog.DoExecute: Boolean;
var
  delegate: PrintDialogDelegate;
  window: NSWindow = nil;
begin
  Result := False;
  if not Assigned(Printer) then Exit;
  if Printer.Printers.Count <= 0 then Exit;
  if AttachTo<>nil then
    window := NSView(AttachTo.Handle).window;

  delegate := PrintDialogDelegate.alloc.init;
  delegate.sender := self;
  delegate.response := OnDialogResult;
  delegate.attachToWindow := window;
  result := delegate.RunSetupPrinter;
end;


{ TPrintDialog }

function TPrintDialog.DoExecute: Boolean;
var
  window: NSWindow = nil;
begin
  Result := False;
  // TODO: Options, Title

  if not Assigned(Printer) then Exit;
  if AttachTo<>nil then
    window := NSView(AttachTo.Handle).window;

  printDelegate := PrintDialogDelegate.alloc.init;
  printDelegate.renderView := nil;
  printDelegate.sender := self;
  printDelegate.response := OnDialogResult;
  printDelegate.attachToWindow := window;

  printDelegate.printDialog := Self;

  if poBeforeBeginDoc in Options then
  begin
    result := printDelegate.RunPrintJob;
    printDelegate := nil;
  end
  else
    result := true;

end;

