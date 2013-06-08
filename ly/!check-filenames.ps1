$regexTitle = [regex]'(?<=title = \\markup{\\override #''\(font-name . "Garamond Premier Pro Semibold"\){ \\abs-fontsize #18 \\smallCapsOldStyle")[^"]*(?=")';
$regexFileTitle = [regex]'(?<=\d+-).*(?=\.ly)';

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