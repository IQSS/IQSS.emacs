#!/bin/bash

pandoc +RTS -K512m -RTS README.org --to html --output index.html --smart --email-obfuscation none --self-contained --standalone --section-divs --table-of-contents --toc-depth 3 
--variable toc_float=1 --variable toc_selectors=h1,h2,h3 --variable toc_collapsed=1 --variable toc_smooth_scroll=1 --variable toc_print=1 --template 
/home/izahn/R/x86_64-pc-linux-gnu-library/3.3/rmarkdown/rmd/h/default.html --include-in-header include.html --highlight-style tango --variable 'theme:bootstrap'

