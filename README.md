### SPINDLE ZOO
Scripts and plots from: ["A comparative analysis of spindle morphometrics across metazoans"](http://www.cell.com/current-biology/abstract/S0960-9822(15)00487-X) or [#SpindleZoo](https://twitter.com/hashtag/SpindleZoo?src=hash).
     
In the spirit of [#OpenScience](https://twitter.com/search?q=%23OpenScience&src=typd), our complete data set (measurements) and R and Python code generated in the course of data analysis is deposited here. We are working on making the raw data (images) available too. We will provide a link here once we manage to do so.     

You are free to use data and code for scientific and educational purposes. We only ask you to [cite the paper](http://dx.doi.org/10.1016/j.cub.2015.04.036)!    
   
####[DATA FILES](https://github.com/emmaggie/SPINDLE_ZOO/tree/master/Final/Data)        
All data files are in the folder [Final/Data](https://github.com/emmaggie/SPINDLE_ZOO/tree/master/Final/Data).
The raw data file was: **ZOODATA_140923.xlsx**. The 'clean data' version of this file is: **original.csv**.    
         
There is a number of modified files we saved along the way. They are all in this directory. **df2_original_for_SQL.csv** contains mostly 'clean' data. The same is true for **original.csv**. They differ with respect to the types of categorical columns. **original.csv** has 41 columns. All catergorical columns have a an equivalent ending with 'CAT', which encodes strings (words) with integers.   

Two files: **meiotic_false.csv** and  **meiotic_true.csv** are the result of splitting the **original** data frame on the column **meiotic** (i.e. whether spindles are meiotic or mitotic). The final two files: **mitotic_anaphase.to_csv** and **mitotic_metaphase.csv** are the result of and additional data split in file **meiotic_true.csv** on the column **stage** (i.e. stage of the cell cycle). SQLite database (**df2_original.db**) contains the same data as in the file: **df2_original_for_SQL.csv**; title of the table is **data_for_DB**.   

There are two additionally processed files: **original_SQL_WO_dupes_WO_metadata_from_R.csv** and  **original_SQL_metaphase_only_from_R.csv**. File **original_SQL_WO_dupes_WO_metadata_from_R.csv** contains the same data as **original.csv** but has variables characterizing two sides of the spindle averaged out (see: **Spindle_zoo_part_3_..** for details). File **original_SQL_metaphase_only_from_R.csv** is created from **original_SQL_WO_dupes_WO_metadata_from_R.csv** but contains metaphase spindles only (both meiotic and mitotic).   
     
Please refer to the file **SpindleZooColumnKey.txt** for the description of columns. 

### [CODE](https://github.com/emmaggie/SPINDLE_ZOO/tree/master/Final/Code)     
As of May 2015, Github renders Jupyter notebooks. You should see the ouput (i.e. not raw JSON), when you click the file on-line (have patience, some of them are a bit large and take time to render).     
     
Alternative way to visualize our code is through [nbviewer](http://nbviewer.ipython.org). All files ending with .ipynb can be scrolled through this way (you can see the code and the output). Simply, go to the [folder with all our code](https://github.com/emmaggie/SPINDLE_ZOO/tree/master/Final/Code), right click on the file ending with **.ipynb**, copy the link address, paste it [here](http://nbviewer.ipython.org) and hit **Go!**.     
[This is what it should look like](http://nbviewer.ipython.org/github/emmaggie/SPINDLE_ZOO/blob/master/Final/Code/6_piecewise_regression_Fig2_and_S2.ipynb).   
The code with no plots in the manuscript has a suffix *_appendix*.    
        
### [SITE](http://emmaggie.github.io/SPINDLE_ZOO)     
The project site lives [here](http://emmaggie.github.io/SPINDLE_ZOO).   



