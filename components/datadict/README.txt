
This package contains data dictionary support for the Lazarus IDE.

When installed in the IDE, several things happen:

- A 'Data Dictionary' item appears under the project menu. 
  This has a submenu with 3 items:
  + 'Set...'
    This will set the data dictionary for the current application
  + 'Open'
    This will open the seleted data dictionary in the database desktop.
  + 'Configure'
    This will show the data dictionary configuration dialog:
    - It allows to set the path where data dictionaries are stored
    - It allows to select the location of the database desktop

- A 'Database desktop' menu item is registered unders tools. It will start
  the 'database desktop'.

- A context menu item is created in the Form Designer: 'Data Dictionary'
  with 3 items:
  - Apply DD. This will apply the data dictionary to all selected datasets.
  - Edit SQL. Starts a visual Query editor with tables from the data
    dictionary. (to be implemented)
  - Create code: allows to create Object Pascal code based on the TDataset.
    - Create a SQL constant (only if a SQL property is found)
    - Create code to create a DBF file with the same structure as the dataset.
    - Create a class and code to load this class from the dataset.
    - Create a TiOPF class and visitors to load the class from the dataset.
    (see fcl-db/src/codegen for more information)

The code generator component is registered on the component palette, under
"Data Access"

This needs a recent version of FPC (rev. 9389 or higher) to compile.

Enjoy!