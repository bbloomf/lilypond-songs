$regexTitle = [regex]'(?<=title = \\markup{\\override #''\(font-name . "Garamond Premier Pro Semibold"\){ \\abs-fontsize #18 \\smallCapsOldStyle")[^"]*(?=")';
$regexFileTitle = [regex]'(?<=\d+-).*(?=\.ly)';
$regexComment = [regex]'\s*%.*$';
$regexMapLine = [regex]'(\d+):(?:([^;]+);?)+';
$regexPageNum = [regex]'(?<=first-page-number\s+=\s+#)\d+';
$regexRaggedLast = [regex]'(?<=ragged-last-bottom\s+=\s+##)t';

$map = Get-Content !map.txt;

$allFiles = New-Object Collections.ArrayList;
foreach($_ in (dir *.ly)) {
    $allFiles.Add($_.Name);
}

Function getBlock([string]$haystack, [long]$index) {
    $open = 0;
    $pos = $haystack.IndexOf('{',$index);
    while($pos -ge 0) {
        if($haystack[$pos] -eq '{') {
            $open++;
        } else {
            $open--;
        }
        if($open -eq 0) {
            return $haystack.Substring($index, $pos + 1 - $index);
        }
        $pos = $haystack.IndexOfAny("{}",$pos+1)
    }
    return '';
}
$pages = '';
del mapped\* -Recurse;
foreach($_ in $map) {
    $_ = $_ -replace $regexComment,'';
    if($_.length -eq 0) {
        continue;
    }
    $m = $regexMapLine.Match($_);
    if($m.Success) {
        $page = $m.Groups[1].Value;
        $pages += " 'tmp\$page.ps'";
        $lys = $m.Groups[2].Captures;
        echo $page;
        if($lys[0].Value -eq '*') {
            $files = (Get-ChildItem -Filter *.ly)
            $file = $files[0];
        } else {
            $files = $null;
            $file = $lys[0].Value + '.ly';
        }
        $allFiles.remove($file);
        $current = Get-Content -Path $file -Encoding UTF8 -Raw
        $result = '';
        #update Page number
        $current = $current -replace $regexPageNum,$page;

        #set print-all-headers = ##t in \paper block
        
        $i = 0;
        while($current.Length -gt 0) {
            $numScoreBlocks = 0;
            $scoreBlockEndBrace = -1;

            #add LyricText override if not present
##            $pos = 0;
##            while( ($pos = $current.IndexOf('\context {',$pos+1)) -ge 0) {
##                $layoutBlock = getBlock $current $pos;
##                if($layoutBlock.Contains('\Lyrics')) {
##                    if(!$layoutBlock.Contains("\override LyricText #'X-offset = #center-on-word")) {
##                        $current = $current.Substring(0, $pos + $layoutBlock.Length - 1) + "  \override LyricText #'X-offset = #center-on-word
##    }" + $current.Substring($pos + $layoutBlock.Length);
##                        Set-Content -Path $file -Value $current -Encoding UTF8;
##                    }
##                }
##            }
                        
            #remove midi blocks
            $pos = 0;
            while( ($pos = $current.IndexOf('\midi',$pos)) -ge 0) {
                $midiBlock = getBlock $current $pos;
##                if(!$midiBlock.Contains('\context')) {
##                    $current = $current.Substring(0, $pos + $midiBlock.Length - 1) + '
##    \context {
##      \Voice
##      \remove "Dynamic_performer"
##    }
##  }' + $current.Substring($pos + $midiBlock.Length);
##                    Set-Content -Path $file -Value $current -Encoding UTF8
##                }
                $current = $current.Substring(0,$pos) + $current.Substring($pos + $midiBlock.length);
            }
            
            $pos = 0;
            #remove score blocks that do not contain \layout block
            while( ($pos = $current.IndexOf('\score',$pos)) -ge 0) {
                $scoreBlock = getBlock $current $pos;
                if($scoreBlock.IndexOf('\layout') -lt 0) {
                    $current = $current.Substring(0,$pos) + $current.Substring($pos + $scoreBlock.length);
                } else {
                    $pos += $scoreBlock.length;
                    $numScoreBlocks++;
                    $scoreBlockEndBrace = $pos - 1;
                }
            }
            
            #Give Warning if more than one score block.
            if($numScoreBlocks -ne 1) {
                echo "Found $numScoreBlocks score blocks; only expected one.";
            }

            #move \header block to end of score block
            if($scoreBlockEndBrace -ge 0) {
                $pos = $current.IndexOf('\header');
                if($pos -ge 0) {
                    $headerBlock = getBlock $current $pos;
                    $replaceHeaderBlock = '\header{ tagline = ""}';
                    if($i -ne 0) {
                        $replaceHeaderBlock = '';
                    }
                    $current = $current.Substring(0,$pos) +
                        $replaceHeaderBlock +
                        $current.Substring($pos + $headerBlock.length, $scoreBlockEndBrace - ($pos + $headerBlock.length)) +
                        $headerBlock +
                        $current.Substring($scoreBlockEndBrace);
                }
            }

            if($i -ne 0) {
                #remove beginning part of file
                $pos = $current.IndexOf('global = {');
                if($pos -ge 0) {
                    $current = $current.Substring($pos);
                }
            } else {
                #Add line: print-all-headers = ##t
                #Add line: ragged-right = ##f
                $pos = $current.IndexOf('\paper');
                if($pos -lt 0) {
                    echo "\paper block not found...EXITING";
                    return;
                }
                $pos = $current.IndexOf('{',$pos);
                if($pos -lt 0) {
                    echo "\paper{} block not found...EXITING";
                    return;
                }
                $pos++;
                $current = $current.Substring(0, $pos) + "`n  print-all-headers = ##t`n  ragged-right = ##f" +
                    $current.Substring($pos);
                #set ragged-last = ##f
                $current = $current -replace $regexRaggedLast,'f';
            }
            
            $result += $current;
            ++$i;
            if($files -ne $null) {
                if($i -ge $files.Count) {
                    break;
                }
                $allFiles.remove($files[$i]);
                $current = Get-Content -Path $files[$i] -Encoding UTF8 -Raw
                continue;
            }
            if($i -ge $lys.Count) {
                break;
            }
            $file = $lys[$i].Value + '.ly';
            $allFiles.remove($file);
            $current = Get-Content -Path $file -Encoding UTF8 -Raw
        }
        Set-Content -Path "mapped\$page.ly" -Value $result -Encoding UTF8
        #&'C:\Program Files (x86)\lilypond\usr\bin\lilypond.exe' -dno-point-and-click --ps -o"tmp/$page" "mapped\$page.ly"
    }
}
#iex "&'c:\Program Files\gs\gs9.06\bin\gswin64c.exe' -q -dSAFER -dDEVICEWIDTHPOINTS=612 -dDEVICEHEIGHTPOINTS=792 -dCompatibilityLevel='1.4' -dNOPAUSE -dBATCH -r1200 -sDEVICE=pdfwrite -dEmbedAllFonts=true -dSubsetFonts=true -sOutputFile=""!full.pdf"" -c.setpdfwrite -f$pages";

if($allFiles.Count -gt 0) {
    echo "Warning: The following files were not used.";
    echo $allFiles;
}

$files = (dir *.ly)
foreach ($_ in $files) {
  $name = $regexFileTitle.Match($_.Name);
  if(!$name.Success) {
    continue;
  }
  $f = Get-Content $_ -Encoding UTF8;
  $m = $regexTitle.Match($f);
  if($name.Value -ne $m.Value) {
    echo $m.Value;
    echo $name.Value;
  }
}