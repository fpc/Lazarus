This project shows how to add and delete levels to a stacked bar series
at runtime. For every new level, a new chart style is created, and for
every deleted level, the corresponding chart style is deleted as well.

Background info on ChartStyles at runtime:
- The TChartStyles component houses a collection named Styles which contains
  all styles. Therefore, you simply call ChartStyles1.Styles.Add to create a
  new ChartStyle and add it to the collection. The Add method returns a pointer
  to the newly created collectionitem; you must cast it TChartStyle to get
  access to its properties.
- In order to delete a style just call the Delete method of the collection with
  the index of the corresponding style.

Instructions for running the demo:
- Click "Add bar" to add new data points (bars)
- Click "Add level" to add a new level to the stacked bars
- Click "Delete selected" to remove the level selected in the listbox from the series.

See also:
- http://forum.lazarus.freepascal.org/index.php/topic,35538.0.html

