This sample shows a way to save and load reports from a database,
It's a modification of the sample project included in the issue
https://gitlab.com/freepascal.org/lazarus/lazarus/-/issues/41806)

The data is stored in two TBufDataset, one for storing the reports, the reports
database, and one as a data source available in the report designer, the data
database. The data in this sample was converted from sqlite to TBufDataset
from the mushrooms sample available in the directory
lazarus/examples/database/image_mushrooms.

The reports database has tree fields: the 'key' field which is incremented
automatically each time a new report is added. the 'title' field, which holds
the report name, and the 'report' field which holds the LazReport report itself.

There are two buttons: 'designer' and 'preview'. The designer button shows the
report designer, and edits the report in the active record. If the database has
no records, the designer starts with an empty report.

New reports are added by pressing the '+' button in reports db navigator.

An existing report, can be loaded from the database by two methods
implemented in the sample by two TRadioButton components:

  'Load from record':

        Before to call the report designer, it loads the report from the
        'Report' field and sets the report name from the 'title' field.

        If the report is new or the reports database is empty, the report
        name is set to a special value sUntitle, which is a LazReport
        translated constant which identifies new reports.


  'Load from designer':

        The report designer starts empty and is expected that the users
        opens a report using the designers File open action. When this
        happen, the TfrDesigner OnLoadReport is called and the report can
        be loaded from the database field 'Report'.

'Load from record' loads a report


