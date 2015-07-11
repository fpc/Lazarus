{
    This file is part of the Free Pascal test suite.
    Copyright (c) 2002 by the Free Pascal development team.

    This unit stores the strings used by
    dotest program, so that they can be also used by
    figest program.

    See the file COPYING.FPC, included in this distribution,
    for details about the license.
 **********************************************************************}

unit teststr;

interface

const
  error = 'Error';
  failed = 'Failed';
  success = 'OK';
  skipped = 'Ignored';
  known_problem = ' known problem: ';
  line_separation = '>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>';

  ResLogfile  : string[32] = 'log';

implementation

end.
