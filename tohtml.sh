#!/bin/bash

pandoc +RTS -K512m -RTS init.org \
       --to html \
       --output init.html \
       --email-obfuscation none \
       --self-contained \
       --standalone \
       --section-divs \
       --table-of-contents \
       --toc-depth 3 \
       --variable toc_float=1 \
       --variable toc_selectors=h1,h2,h3 \
       --variable toc_collapsed=1 \
       --variable toc_smooth_scroll=1 \
       --variable toc_print=1 \
       --template template.html \
       --include-in-header include.html \
       --highlight-style tango \
       --variable 'theme:bootstrap'

pandoc +RTS -K512m -RTS UsefulPrograms.org \
       --to html \
       --output UsefulPrograms.html \
       --email-obfuscation none \
       --self-contained \
       --standalone \
       --section-divs \
       --table-of-contents \
       --toc-depth 3 \
       --variable toc_float=1 \
       --variable toc_selectors=h1,h2,h3 \
       --variable toc_collapsed=1 \
       --variable toc_smooth_scroll=1 \
       --variable toc_print=1 \
       --template template.html \
       --include-in-header include.html \
       --highlight-style tango \
       --variable 'theme:bootstrap'

pandoc +RTS -K512m -RTS README.md \
       --to html \
       --output index.html \
       --email-obfuscation none \
       --self-contained \
       --standalone \
       --section-divs \
       --table-of-contents \
       --toc-depth 3 \
       --variable toc_float=1 \
       --variable toc_selectors=h1,h2,h3 \
       --variable toc_collapsed=1 \
       --variable toc_smooth_scroll=1 \
       --variable toc_print=1 \
       --template template.html \
       --include-in-header include.html \
       --highlight-style tango \
       --variable 'theme:bootstrap'
