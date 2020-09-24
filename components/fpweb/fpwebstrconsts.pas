unit fpWebStrConsts;

{$mode objfpc}{$H+}

interface

resourcestring
  SHTMLTitle         = 'Html &title - <title>';
  SHTMLAutor         = 'Html &author - <meta name="author">';
  SHTMLCopyright     = 'Html cop&yright - meta name="copyright">';
  SHTMLCharset       = 'HTML chars&et';
  SHTMLCssFile       = '&CSS file';
  SHTMLJSFile        = '&Javascript file';
  SNewHtmlFileProps  = 'New Html file properties';
  SEnterYouText      = 'Enter your text ...';

  SmiHTMLEditor      = 'HTML Editor';
  SmiHTMLStandart    = 'Standard';

  SmiHTMLTextBold           = 'Bold';
  SmiHTMLTextItalic         = 'Italic';
  SmiHTMLTextUnderline      = 'Underline';
  SmiHTMLInsertBR           = 'Insert new line';
  //para
  SmiHTMLInsertNBSP         = 'Insert Non Breaking Space';
  SmiHTMLInsertHR           = 'Insert horizontal line';
  SmiHTMLInsertComment      = 'Insert HTML comment';
  SmiHTMLInsertIMG          = 'Insert image';
  SmiHTMLInsertLink         = 'Insert link (HREF)';
  SmiHTMLTextAlignLeft      = 'Text align left';
  SmiHTMLTextAlignRight     = 'Text align right';
  SmiHTMLTextAlignCenter    = 'Text align center';
  SmiHTMLTextAlignJustify   = 'Text align justify';

  SmiHTMLStyle       = 'Styles';
  SmiHTMLLists       = 'Lists';
  SmiHTMLTables      = 'Tables';
  SmiHTMLForms       = 'Forms';
  SmiHTMLOther       = 'Other';

  SmiHTMLInsertTable        = 'Insert HTML table';
  SmiHTMLInsertTableRow     = 'Insert HTML table row';
  SmiHTMLInsertTableData    = 'Insert HTML table data';
  SmiHTMLInsertTableRowWD   = 'Insert HTML table row with dialog';
  SmiHTMLInsertTableDataWD  = 'Insert HTML table data with dialog';

  SmiHTMLInsertList         = 'Insert HTML list';

  SmiHTMLInsertHeader1Level = 'Insert HTML level 1 header';
  SmiHTMLInsertHeader2Level = 'Insert HTML level 2 header';
  SmiHTMLInsertHeader3Level = 'Insert HTML level 3 header';
  SmiHTMLInsertHeader4Level = 'Insert HTML level 4 header';
  SmiHTMLInsertHeader5Level = 'Insert HTML level 5 header';
  SmiHTMLInsertColor        = 'Insert HTML color value';
  SmiHTMLInsertDIVBlock     = 'Insert HTML DIV tag';
  SmiHTMLInsertSpanText     = 'Insert HTML SPAN tag';
  SmiHTMLInsertPre          = 'Insert HTML PRE tag';
  SmiHTMLInsertSub          = 'Insert Subscript';
  SmiHTMLInsertSuper        = 'Insert Superscript';


  SmiHTMLInsertForm         = 'Insert HTML Form';
  SmiHTMLFormSelect         = 'Insert Select control';
  SmiHTMLFormSelectOpt      = 'Insert Select options';
  SmiHTMLFormSelectOptWD    = 'Insert Select options with dialog';
  SmiHTMLFormCheckBox       = 'Insert CheckBox control';
  SmiHTMLFormRadioBtn       = 'Insert RadioBtn control';
  SmiHTMLFormButtton        = 'Insert Button control';
  SmiHTMLInsertInput        = 'Insert HTML INPUT tag';
  SmiHTMLInsertInputSubmit  = 'Insert "Submit" button ';
  SmiHTMLInsertInputReset   = 'Insert "Reset" button';
  SmiHTMLFormFieldSet       = 'Insert HTML FIELDSET tag';
  SmiHTMLFormLegend         = 'Insert HTML LEGEND tag';

  SmiOtherInsertFN          = 'Insert file name';

  SHTMLTagCaptionSubmit     = 'Submit';
  SHTMLTagCaptionReset      = 'Reset';

  SHtmlDesign               = 'HTML design';
  SHtmlFile                 = 'HTML file';
  SHtmlFileDesc             = 'Create new HTML file ...';

  SJSFile                   = 'Javascript file';
  SJSFileDesc               = 'Create new javascript file ...';
  SJSSource                 = 'Enter your javascript code here';

  SCSSFile                   = 'CSS file';
  SCSSFileDesc               = 'Create new CSS file ...';
  SCSSSource                 = 'Enter your classes/style definitions here';

  SHTMLTagProperty           = 'Tag property: %s';

  SHTMLTableFormCaption      = 'New HTML table ...';
  SHTMLTableFormColumnCount  = 'Column count';
  SHTMLTableFormRowCount     = 'Row count';
  SHTMLTableFormBorderWidth  = 'Border width';
  SHTMLTableFormUseHeader    = 'Use header row';
  SHTMLTableFormCellpadding  = 'Cell padding';
  SHTMLTableFormCellspacing  = 'Cell spacing';
  SHTMLTableFormWidth        = 'Width';
  SHTMLTableFormHeaderBGColor= 'Header bg color';

  SHTMLInputFormCaption      = 'Tag property: INPUT';
  SHTMLInputFormType         = 'Type';
  SHTMLInputFormName         = 'Name';
  SHTMLInputFormValue        = 'Value';
  SHTMLInputFormSize         = 'Size';
  SHTMLInputFormMaxLen       = 'Max length';
  SHTMLInputFormAlt          = 'Alt';
  SHTMLInputFormImageSrc     = 'Image src';
  SHTMLInputFormTabIndex     = 'Tab Index';
  SHTMLInputFormAlign        = 'Align';
  SHTMLInputFormAccessKey    = 'Access key';

  sHTTPPort = '&Port to listen for requests:';
  sNewHTTPApp = 'New HTTP application';
  sRegisterFiles = '&Register location to serve files from';
  sDocumentLocation = '&Location';
  sDocumentRoot = '&Directory';
  sUseThreads = 'Use &threads to serve requests in';

  sCaption = 'Create a new JSON-RPC module';
  sRegisterJSON = 'Register JSON-RPC handlers in factory';
  sJSONClass = 'JSON-RPC class';
  sRegisterWebM = 'Register web module';
  sHTTPPath = 'HTTP Path';

  rsCGIApplicati = 'CGI Application';
  rsCGIApplicati2 = 'A CGI (Common Gateway Interface) program in Free Pascal using webmodules.';
  rsCustomCGIApp = 'Custom CGI Application';
  rsCustomCGIApp2 = 'A CGI (Common Gateway Interface) program in Free Pascal.';
  rsWebModule = 'Web Module';
  rsWEBModuleADa = 'A datamodule for WEB (HTTP) applications.';
  rsHTMLWebModul = 'HTML Web Module';
  rsHTMLWEBModul2 = 'A Web datamodule for producing strict HTML.';
  rsApacheModule = 'Apache Module';
  rsApacheModule2 = 'An Apache loadable module in Free Pascal using webmodules. '
    +'The main library file is automatically maintained by Lazarus.';
  rsCustomFastCG = 'Custom FastCGI Application';
  rsCustomFastCG2 = 'A FastCGI (Common Gateway Interface) program in Free Pascal.';
  rsFastCGIAppli = 'FastCGI Application';
  rsFastCGIAppli2 = 'A FastCGI (Common Gateway Interface) '
    +'program in Free Pascal using webmodules.';
  rsWebDataProvi = 'Web DataProvider Module';
  rsWEBDataProvi2 = 'A datamodule to handle data requests for WEB (HTTP) '
    +'applications using WebDataProvider components.';
  rsWebJSONRPCMo = 'Web JSON-RPC Module';
  rsWEBJSONRPCMo2 = 'A datamodule to dispatch JSON-RPC requests in WEB (HTTP) '
    +'applications using TJSONRPCHandler components.';
  rsWebExtDirect = 'Web Ext.Direct Module';
  rsWEBExtDirect2 = 'A datamodule to dispatch Ext.Direct requests in WEB (HTTP) '
    +'applications using TJSONRPCHandler components.';
  rsHTTPAppli = 'HTTP server Application';
  rsHTTPAppli2 = 'Complete HTTP Server program in Free Pascal using webmodules.';

implementation

end.

