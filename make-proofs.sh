# "C:\Program Files\gs\gs9.06\bin\gswin64c" -dBATCH -dNOPAUSE -sDEVICE=pdfwrite -c "[/CropBox [27 18 585 774] /PAGES pdfmark" -sOutputFile="!!playbook-sftpd.pdf" -f !contents.pdf !full.pdf pdfmarks.txt

# "C:\Program Files\gs\gs9.06\bin\gswin64c" -o sftpd.pdf -dBATCH -dNOPAUSE -sDEVICE=pdfwrite -f !full.pdf pdfmarks.txt
pushd gh-pages
echo 'Updating gh-pages repository'
git pull
popd
echo 'Updating lilypond-songs repository'
if git pull
then
  cp \!full.pdf 9781484022801_content.pdf

  echo 'Converting pdfmarks to WINDOWS-1252'
  iconv -f UTF-8 -t WINDOWS-1252 ly/Contents/pdfmarks.txt > pdfmarks.txt
  echo 'Adding bookmarks to 8.5x11 version'
  gs -o gh-pages/PDFs/sftpd.pdf -dBATCH -dNOPAUSE -sDEVICE=pdfwrite -f \!full.pdf pdfmarks.txt
  echo 'Making tablet version'
  gs -o gh-pages/PDFs/sftpd-playbook.pdf -dBATCH -dNOPAUSE -sDEVICE=pdfwrite -c "<</EndPage {0 eq {2 mod 0 eq {[/CropBox [64.8 18 565.2 774] /PAGE pdfmark true} {[/CropBox [46.8 18 547.2 774] /PAGE pdfmark true} ifelse}{false}ifelse}>> setpagedevice" -f \!full.pdf pdfmarks.txt

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
  echo 'Error from git --^'
fi