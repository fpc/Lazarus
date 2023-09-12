unit PkgSysBasePkgs;

{$mode ObjFPC}{$H+}

interface

type
  // the base packages needed by the minimal IDE
  TLazarusIDEBasePkg = (
    libpFCL,
    libpLazUtils,
    libpFreeTypeLaz,
    libpBuildIntf,
    libpCodeTools,
    libpLazEdit,
    libpLCLBase,
    libpLCL,
    libpIDEIntf,
    libpSynEdit,
    libpLazDebuggerIntf,
    libpDebuggerIntf,
    libpCmdLineDebuggerBase,
    libpfpdebug,
    libpLazDebuggerGdbmi,
    libpLazDebuggerFp,
    libpLazDebuggerLldb,
    libpLazDebuggerFpLldb,
    libpLazControls,
    libpLazControlDsgn,
    libpLCLExtensions_package,
    libpLazVirtualtreeview_package,
    libpIdeConfig,
    libpIdeDebugger
    );
const
  LazarusIDEBasePkgNames: array[TLazarusIDEBasePkg] of string = (
    'FCL',
    'LazUtils',
    'freetypelaz',
    'BuildIntf',
    'CodeTools',
    'LazEdit',
    'LCLBase',
    'LCL',
    'IDEIntf',
    'SynEdit',
    'LazDebuggerIntf',
    'DebuggerIntf',
    'CmdLineDebuggerBase',
    'fpdebug',
    'LazDebuggerGdbmi',
    'LazDebuggerFp',
    'LazDebuggerLldb',
    'LazDebuggerFpLldb',
    'LazControls',
    'LazControlDsgn',
    'lclextensions_package',
    'laz.virtualtreeview_package',
    'IdeConfig',
    'IdeDebugger'
    );

implementation

end.

