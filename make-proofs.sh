# "C:\Program Files\gs\gs9.06\bin\gswin64c" -dBATCH -dNOPAUSE -sDEVICE=pdfwrite -c "[/CropBox [27 18 585 774] /PAGES pdfmark" -sOutputFile="!!playbook-sftpd.pdf" -f !contents.pdf !full.pdf pdfmarks.txt

# "C:\Program Files\gs\gs9.06\bin\gswin64c" -o sftpd.pdf -dBATCH -dNOPAUSE -sDEVICE=pdfwrite -f !full.pdf pdfmarks.txt
pushd gh-pages
echo 'Updating gh-pages repository'
git pull
popd
echo 'Updating lilypond-songs repository'
if git pull
then
  cp \!full.pdf gh-pages/PDFs/sftpd.pdf
  cp \!full.pdf 9781484022801_content.pdf
  echo 'Extracting odd pages.'
  pdftk A=\!full.pdf cat Aodd output odd.pdf
  echo 'Extracting even pages.'
  pdftk A=\!full.pdf cat Aeven output even.pdf
  gswin64c -o \!odd.pdf -dBATCH -dNOPAUSE -sDEVICE=pdfwrite -c "[/CropBox [45 18 585 774] /PAGES pdfmark" -f odd.pdf
  gswin64c -o \!even.pdf -dBATCH -dNOPAUSE -sDEVICE=pdfwrite -c "[/CropBox [27 18 567 774] /PAGES pdfmark" -f even.pdf

  pdftk A=\!odd.pdf B=\!even.pdf shuffle A B output \!sftpd-playbook.pdf

  if gswin64c -o gh-pages/PDFs/sftpd-playbook.pdf -dBATCH -dNOPAUSE -sDEVICE=pdfwrite -c "[/CropBox [27 18 585 774] /PAGES pdfmark" -f \!sftpd-playbook.pdf pdfmarks.txt
  then
    pushd gh-pages
    if git add PDFs/sftpd-playbook.pdf PDFs/sftpd.pdf
    then
      if git commit -m 'Updated PDFs'
      then
        if git push
        then
          echo 'Success!'
        else
          echo 'Error from git push --^'
        fi
      else
        echo 'Error from git commit --^'
      fi
    else
      echo 'Error from git add --^'
    fi
    popd
  else
    echo 'Error from gswin64c --^'
  fi
  rm even.pdf odd.pdf \!even.pdf \!odd.pdf \!sftpd-playbook.pdf
else
  echo 'Error from git --^'
fi