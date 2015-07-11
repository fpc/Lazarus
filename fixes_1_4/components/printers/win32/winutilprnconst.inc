//WinSpool consts

  //Commands printer
  PRINTER_CONTROL_PAUSE            = 1; //Pauses the printer
  PRINTER_CONTROL_RESUME           = 2; //Resumes a paused printer
  PRINTER_CONTROL_PURGE            = 3; //Deletes all print jobs in the printer
  PRINTER_CONTROL_SET_STATUS       = 4; //Sets the printer status.

  //Status of Printer
  PRINTER_STATUS_PAUSED            = $00000001; //The printer is paused
  PRINTER_STATUS_ERROR             = $00000002; //The printer is in an error state
  PRINTER_STATUS_PENDING_DELETION  = $00000004; //The printer is being deleted
  PRINTER_STATUS_PAPER_JAM         = $00000008; //Paper is jammed in the printer
  PRINTER_STATUS_PAPER_OUT         = $00000010; //The printer is out of paper
  PRINTER_STATUS_MANUAL_FEED       = $00000020; //The printer is in a manual feed state
  PRINTER_STATUS_PAPER_PROBLEM     = $00000040; //The printer has a paper problem
  PRINTER_STATUS_OFFLINE           = $00000080; //The printer is offline
  PRINTER_STATUS_IO_ACTIVE         = $00000100; //The printer is in an active input/output state
  PRINTER_STATUS_BUSY              = $00000200; //The printer is busy
  PRINTER_STATUS_PRINTING          = $00000400; //The printer is printing
  PRINTER_STATUS_OUTPUT_BIN_FULL   = $00000800; //The printer's output bin is full
  PRINTER_STATUS_NOT_AVAILABLE     = $00001000; //The printer is not available for printing
  PRINTER_STATUS_WAITING           = $00002000; //The printer is waiting
  PRINTER_STATUS_PROCESSING        = $00004000; //The printer is processing a print job
  PRINTER_STATUS_INITIALIZING      = $00008000; //The printer is initializing
  PRINTER_STATUS_WARMING_UP        = $00010000; //The printer is warming up
  PRINTER_STATUS_TONER_LOW         = $00020000; //The printer is low on toner
  PRINTER_STATUS_NO_TONER          = $00040000; //The printer is out of toner
  PRINTER_STATUS_PAGE_PUNT         = $00080000; //The printer cannot print the current page.
  PRINTER_STATUS_USER_INTERVENTION = $00100000; //The printer has an error that requires the user to do something
  PRINTER_STATUS_OUT_OF_MEMORY     = $00200000; //The printer has run out of memory
  PRINTER_STATUS_DOOR_OPEN         = $00400000; //The printer door is open
  PRINTER_STATUS_SERVER_UNKNOWN    = $00800000; //The printer status is unknown
  PRINTER_STATUS_POWER_SAVE        = $01000000; //The printer is in power save mode

  //Attribute of printer
  PRINTER_ATTRIBUTE_QUEUED            = $00000001; //if set, the printer spools and starts printing after the last page is spooled. If not set and PRINTER_ATTRIBUTE_DIRECT is not set, the printer spools and prints while spooling
  PRINTER_ATTRIBUTE_DIRECT            = $00000002; //Job is sent directly to the printer (it is not spooled).
  PRINTER_ATTRIBUTE_DEFAULT           = $00000004; //Win9x/Me: Indicates the printer is the default printer in the system
  PRINTER_ATTRIBUTE_SHARED            = $00000008; //Printer is shared
  PRINTER_ATTRIBUTE_NETWORK           = $00000010; //Printer is a network printer connection
  PRINTER_ATTRIBUTE_HIDDEN            = $00000020; //Reserved
  PRINTER_ATTRIBUTE_LOCAL             = $00000040; //Printer is a local printer
  PRINTER_ATTRIBUTE_ENABLE_DEVQ       = $00000080;
  PRINTER_ATTRIBUTE_KEEPPRINTEDJOBS   = $00000100; //If set, jobs are kept after they are printed. If unset, jobs are deleted
  PRINTER_ATTRIBUTE_DO_COMPLETE_FIRST = $00000200; //If set and printer is set for print-while-spooling, any jobs that have completed spooling are scheduled to print before jobs that have not completed spooling
  PRINTER_ATTRIBUTE_WORK_OFFLINE      = $00000400; //Win9x/Me: Indicates whether the printer is currently connected. If the printer is not currently connected, print jobs will continue to spool.
  PRINTER_ATTRIBUTE_ENABLE_BIDI       = $00000800; //Win9x/Me: Indicates whether bi-directional communications are enabled for the printer
  PRINTER_ATTRIBUTE_RAW_ONLY          = $00001000; //Indicates that only raw data type print jobs can be spooled


  //Jobs priorities of printer
  NO_PRIORITY   = 0;   //no priority
  MAX_PRIORITY  = 99;  //Maximum priority value
  MIN_PRIORITY  = 1;   //Minimum priority value
  DEF_PRIORITY  = 1;   //Default value of priority

  JOB_CONTROL_PAUSE              = 1;
  JOB_CONTROL_RESUME             = 2;
  JOB_CONTROL_CANCEL             = 3;
  JOB_CONTROL_RESTART            = 4;
  JOB_CONTROL_DELETE             = 5;
  JOB_CONTROL_SENT_TO_PRINTER    = 6;
  JOB_CONTROL_LAST_PAGE_EJECTED  = 7;

  JOB_STATUS_PAUSED              = $00000001;
  JOB_STATUS_ERROR               = $00000002;
  JOB_STATUS_DELETING            = $00000004;
  JOB_STATUS_SPOOLING            = $00000008;
  JOB_STATUS_PRINTING            = $00000010;
  JOB_STATUS_OFFLINE             = $00000020;
  JOB_STATUS_PAPEROUT            = $00000040;
  JOB_STATUS_PRINTED             = $00000080;
  JOB_STATUS_DELETED             = $00000100;
  JOB_STATUS_BLOCKED_DEVQ        = $00000200;
  JOB_STATUS_USER_INTERVENTION   = $00000400;
  JOB_STATUS_RESTART             = $00000800;

  JOB_POSITION_UNSPECIFIED       = 0;

  DI_CHANNEL                     = 1;
  DI_READ_SPOOL_JOB              = 3;

  FORM_USER               = $00000000;
  FORM_BUILTIN            = $00000001;
  FORM_PRINTER            = $00000002;

  PORT_TYPE_WRITE         = $0001;
  PORT_TYPE_READ          = $0002;
  PORT_TYPE_REDIRECTED    = $0004;
  PORT_TYPE_NET_ATTACHED  = $0008;

  PORT_STATUS_TYPE_ERROR  = 1;
  PORT_STATUS_TYPE_WARNING= 2;
  PORT_STATUS_TYPE_INFO   = 3;

  PORT_STATUS_OFFLINE          = 1;
  PORT_STATUS_PAPER_JAM        = 2;
  PORT_STATUS_PAPER_OUT        = 3;
  PORT_STATUS_OUTPUT_BIN_FULL  = 4;
  PORT_STATUS_PAPER_PROBLEM    = 5;
  PORT_STATUS_NO_TONER         = 6;
  PORT_STATUS_DOOR_OPEN        = 7;
  PORT_STATUS_USER_INTERVENTION= 8;
  PORT_STATUS_OUT_OF_MEMORY    = 9;
  PORT_STATUS_TONER_LOW        = 10;
  PORT_STATUS_WARMING_UP       = 11;
  PORT_STATUS_POWER_SAVE       = 12;

  PRINTER_ENUM_DEFAULT         = $00000001;
  PRINTER_ENUM_LOCAL           = $00000002;
  PRINTER_ENUM_CONNECTIONS     = $00000004;
  PRINTER_ENUM_FAVORITE        = $00000004;
  PRINTER_ENUM_NAME            = $00000008;
  PRINTER_ENUM_REMOTE          = $00000010;
  PRINTER_ENUM_SHARED          = $00000020;
  PRINTER_ENUM_NETWORK         = $00000040;
  PRINTER_ENUM_EXPAND          = $00004000;
  PRINTER_ENUM_CONTAINER       = $00008000;
  PRINTER_ENUM_ICONMASK        = $00ff0000;
  PRINTER_ENUM_ICON1           = $00010000;
  PRINTER_ENUM_ICON2           = $00020000;
  PRINTER_ENUM_ICON3           = $00040000;
  PRINTER_ENUM_ICON4           = $00080000;
  PRINTER_ENUM_ICON5           = $00100000;
  PRINTER_ENUM_ICON6           = $00200000;
  PRINTER_ENUM_ICON7           = $00400000;
  PRINTER_ENUM_ICON8           = $00800000;

  PRINTER_NOTIFY_TYPE = $00;
  JOB_NOTIFY_TYPE     = $01;

  PRINTER_NOTIFY_FIELD_SERVER_NAME        = $00;
  PRINTER_NOTIFY_FIELD_PRINTER_NAME       = $01;
  PRINTER_NOTIFY_FIELD_SHARE_NAME         = $02;
  PRINTER_NOTIFY_FIELD_PORT_NAME          = $03;
  PRINTER_NOTIFY_FIELD_DRIVER_NAME        = $04;
  PRINTER_NOTIFY_FIELD_COMMENT            = $05;
  PRINTER_NOTIFY_FIELD_LOCATION           = $06;
  PRINTER_NOTIFY_FIELD_DEVMODE            = $07;
  PRINTER_NOTIFY_FIELD_SEPFILE            = $08;
  PRINTER_NOTIFY_FIELD_PRINT_PROCESSOR    = $09;
  PRINTER_NOTIFY_FIELD_PARAMETERS         = $0A;
  PRINTER_NOTIFY_FIELD_DATATYPE           = $0B;
  PRINTER_NOTIFY_FIELD_SECURITY_DESCRIPTOR= $0C;
  PRINTER_NOTIFY_FIELD_ATTRIBUTES         = $0D;
  PRINTER_NOTIFY_FIELD_PRIORITY           = $0E;
  PRINTER_NOTIFY_FIELD_DEFAULT_PRIORITY   = $0F;
  PRINTER_NOTIFY_FIELD_START_TIME         = $10;
  PRINTER_NOTIFY_FIELD_UNTIL_TIME         = $11;
  PRINTER_NOTIFY_FIELD_STATUS             = $12;
  PRINTER_NOTIFY_FIELD_STATUS_STRING      = $13;
  PRINTER_NOTIFY_FIELD_CJOBS              = $14;
  PRINTER_NOTIFY_FIELD_AVERAGE_PPM        = $15;
  PRINTER_NOTIFY_FIELD_TOTAL_PAGES        = $16;
  PRINTER_NOTIFY_FIELD_PAGES_PRINTED      = $17;
  PRINTER_NOTIFY_FIELD_TOTAL_BYTES        = $18;
  PRINTER_NOTIFY_FIELD_BYTES_PRINTED      = $19;

  JOB_NOTIFY_FIELD_PRINTER_NAME           = $00;
  JOB_NOTIFY_FIELD_MACHINE_NAME           = $01;
  JOB_NOTIFY_FIELD_PORT_NAME              = $02;
  JOB_NOTIFY_FIELD_USER_NAME              = $03;
  JOB_NOTIFY_FIELD_NOTIFY_NAME            = $04;
  JOB_NOTIFY_FIELD_DATATYPE               = $05;
  JOB_NOTIFY_FIELD_PRINT_PROCESSOR        = $06;
  JOB_NOTIFY_FIELD_PARAMETERS             = $07;
  JOB_NOTIFY_FIELD_DRIVER_NAME            = $08;
  JOB_NOTIFY_FIELD_DEVMODE                = $09;
  JOB_NOTIFY_FIELD_STATUS                 = $0A;
  JOB_NOTIFY_FIELD_STATUS_STRING          = $0B;
  JOB_NOTIFY_FIELD_SECURITY_DESCRIPTOR    = $0C;
  JOB_NOTIFY_FIELD_DOCUMENT               = $0D;
  JOB_NOTIFY_FIELD_PRIORITY               = $0E;
  JOB_NOTIFY_FIELD_POSITION               = $0F;
  JOB_NOTIFY_FIELD_SUBMITTED              = $10;
  JOB_NOTIFY_FIELD_START_TIME             = $11;
  JOB_NOTIFY_FIELD_UNTIL_TIME             = $12;
  JOB_NOTIFY_FIELD_TIME                   = $13;
  JOB_NOTIFY_FIELD_TOTAL_PAGES            = $14;
  JOB_NOTIFY_FIELD_PAGES_PRINTED          = $15;
  JOB_NOTIFY_FIELD_TOTAL_BYTES            = $16;
  JOB_NOTIFY_FIELD_BYTES_PRINTED          = $17;

  PRINTER_NOTIFY_OPTIONS_REFRESH          = $01;
  PRINTER_NOTIFY_INFO_DISCARDED           = $01;

  PRINTER_CHANGE_ADD_PRINTER              = $00000001;
  PRINTER_CHANGE_SET_PRINTER              = $00000002;
  PRINTER_CHANGE_DELETE_PRINTER           = $00000004;
  PRINTER_CHANGE_FAILED_CONNECTION_PRINTER= $00000008;
  PRINTER_CHANGE_PRINTER                  = $000000FF;
  PRINTER_CHANGE_ADD_JOB                  = $00000100;
  PRINTER_CHANGE_SET_JOB                  = $00000200;
  PRINTER_CHANGE_DELETE_JOB               = $00000400;
  PRINTER_CHANGE_WRITE_JOB                = $00000800;
  PRINTER_CHANGE_JOB                      = $0000FF00;
  PRINTER_CHANGE_ADD_FORM                 = $00010000;
  PRINTER_CHANGE_SET_FORM                 = $00020000;
  PRINTER_CHANGE_DELETE_FORM              = $00040000;
  PRINTER_CHANGE_FORM                     = $00070000;
  PRINTER_CHANGE_ADD_PORT                 = $00100000;
  PRINTER_CHANGE_CONFIGURE_PORT           = $00200000;
  PRINTER_CHANGE_DELETE_PORT              = $00400000;
  PRINTER_CHANGE_PORT                     = $00700000;
  PRINTER_CHANGE_ADD_PRINT_PROCESSOR      = $01000000;
  PRINTER_CHANGE_DELETE_PRINT_PROCESSOR   = $04000000;
  PRINTER_CHANGE_PRINT_PROCESSOR          = $07000000;
  PRINTER_CHANGE_ADD_PRINTER_DRIVER       = $10000000;
  PRINTER_CHANGE_SET_PRINTER_DRIVER       = $20000000;
  PRINTER_CHANGE_DELETE_PRINTER_DRIVER    = $40000000;
  PRINTER_CHANGE_PRINTER_DRIVER           = $70000000;
  PRINTER_CHANGE_TIMEOUT                  = $80000000;
  PRINTER_CHANGE_ALL                      = $7777FFFF;

  PRINTER_ERROR_INFORMATION               = $80000000;
  PRINTER_ERROR_WARNING                   = $40000000;
  PRINTER_ERROR_SEVERE                    = $20000000;
  PRINTER_ERROR_OUTOFPAPER                = $00000001;
  PRINTER_ERROR_JAM                       = $00000002;
  PRINTER_ERROR_OUTOFTONER                = $00000004;

  //PrintDlg Flags
  PD_ALLPAGES                             = $00000000;
  PD_SELECTION                            = $00000001;
  PD_PAGENUMS                             = $00000002;
  PD_NOSELECTION                          = $00000004;
  PD_NOPAGENUMS                           = $00000008;
  PD_COLLATE                              = $00000010;
  PD_PRINTTOFILE                          = $00000020;
  PD_PRINTSETUP                           = $00000040;
  PD_NOWARNING                            = $00000080;
  PD_RETURNDC                             = $00000100;
  PD_RETURNIC                             = $00000200;
  PD_RETURNDEFAULT                        = $00000400;
  PD_SHOWHELP                             = $00000800;
  PD_ENABLEPRINTHOOK                      = $00001000;
  PD_ENABLESETUPHOOK                      = $00002000;
  PD_ENABLEPRINTTEMPLATE                  = $00004000;
  PD_ENABLESETUPTEMPLATE                  = $00008000;
  PD_ENABLEPRINTTEMPLATEHANDLE            = $00010000;
  PD_ENABLESETUPTEMPLATEHANDLE            = $00020000;
  PD_USEDEVMODECOPIES                     = $00040000;
  PD_USEDEVMODECOPIESANDCOLLATE           = $00040000;
  PD_DISABLEPRINTTOFILE                   = $00080000;
  PD_HIDEPRINTTOFILE                      = $00100000;
  PD_NONETWORKBUTTON                      = $00200000;



