### SPINDLE ZOO
Scripts and plots from: "The Spindle Zoo: A comparative analysis of spindle morphometrics across metazoans."   
     
In the spirit of open science, complete R and Python code generated in the course of data analysis is deposited here.    

You are free to use data and code for scientific and educational purposes. We only ask you to cite the paper!    
   
####DATA FILES
The raw data file is: **ZOODATA_140923.xlsx**.     
      
There is a number of modified files we saved along the way. They are all in this directory. **df2_original_for_SQL.csv** contains mostly 'clean' data. The same is true for **original.csv**. They differ with respect to the types of categorical columns. **original.csv** has 41 columns. All catergorical columns have a an equivalent ending with 'CAT', which encodes strings (words) with integers.   

Two files: **meiotic_false.csv** and  **meiotic_true.csv** are the result of splitting the **original** data frame on the column **meiotic** (i.e. whether spindles are meiotic or mitotic). The final two files: **mitotic_anaphase.to_csv** and **mitotic_metaphase.csv** are the result of and additional data split in file **meiotic_true.csv** on the column **stage** (i.e. stage of the cell cycle). SQLite database (**df2_original.db**) contains the same data as in the file: **df2_original_for_SQL.csv**; title of the table is **data_for_DB**.   

There are two additionally processed files: **original_SQL_WO_dupes_WO_metadata_from_R.csv** and  **original_SQL_metaphase_only_from_R.csv**. File **original_SQL_WO_dupes_WO_metadata_from_R.csv** contains the same data as **original.csv** but has variables characterizing two sides of the spindle averaged out (see: **Spindle_zoo_part_3_..** for details). File **original_SQL_metaphase_only_from_R.csv** is created from **original_SQL_WO_dupes_WO_metadata_from_R.csv** but contains metaphase spindles only (both meiotic and mitotic).   
     
Please refer to the file **SpindleZooColumnKey.txt** for the description of columns. 

####R CODE
Keynote_plots_R_p9_unsupervised.R - contains additional data pre-processing
Keynote_plots_R_p8_piecewise_regression.R - contains additional data 

The project site lives [here](http://emmaggie.github.io/SPINDLE_ZOO).   



