cd "C:\Users\CEGONZALEZ\Documents\GFSF\ciat_rice_beansRicky_379\BEANS\beanDrought"
set linesize 80
qui log using gfsf, replace



/* 
 #This is heading 1
 ##This is heading 2
 ###This is heading 3
 This is a text paragraph and its content can be *italic* or **bold**. 
 as long as you move on to the next lines without adding 2 or more spaces to the 
 end of each line, Markdown will be interpreted as one paragraph and the lines appear 
 continuous in your document.

 to begin a new paragraph, leave an empty line between your text. Also remember that you can 
 use underscore to make your text _italic_, __bold__, or even
 ___italic & bold___. 
 
![BeanYield](C:\Users\CEGONZALEZ\Documents\GFSF\ciat_rice_beansRicky_379\BEANS\beanDrought/yield_dssat__379__bean__drought__base_2000__boring___29feb16_2120.png  "Optional Title") 
*/

markdoc gfsf, export(docx) replace
markdoc gfsf, export(html)
