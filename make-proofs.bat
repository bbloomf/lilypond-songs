rem "C:\Program Files\gs\gs9.06\bin\gswin64c" -dBATCH -dNOPAUSE -sDEVICE=pdfwrite -c "[/CropBox [27 18 585 774] /PAGES pdfmark" -sOutputFile="!!playbook-sftpd.pdf" -f !contents.pdf !full.pdf pdfmarks.txt

rem "C:\Program Files\gs\gs9.06\bin\gswin64c" -o sftpd.pdf -dBATCH -dNOPAUSE -sDEVICE=pdfwrite -f !full.pdf pdfmarks.txt
copy !full.pdf gh-pages\PDFs\sftpd.pdf
copy !full.pdf 9781484022801_content.pdf
pdftk A=!full.pdf cat Aodd output odd.pdf
pdftk A=!full.pdf cat Aeven output even.pdf
gswin64c -o !odd.pdf -dBATCH -dNOPAUSE -sDEVICE=pdfwrite -c "[/CropBox [45 18 585 774] /PAGES pdfmark" -f odd.pdf
gswin64c -o !even.pdf -dBATCH -dNOPAUSE -sDEVICE=pdfwrite -c "[/CropBox [27 18 567 774] /PAGES pdfmark" -f even.pdf

pdftk A=!odd.pdf B=!even.pdf shuffle A B output !sftpd-playbook.pdf

gswin64c -o gh-pages\PDFs\sftpd-playbook.pdf -dBATCH -dNOPAUSE -sDEVICE=pdfwrite -c "[/CropBox [27 18 585 774] /PAGES pdfmark" -f !sftpd-playbook.pdf pdfmarks.txt
del even.pdf odd.pdf !even.pdf !odd.pdf !sftpd-playbook.pdf