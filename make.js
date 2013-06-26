var child_process = require('child_process'),
    fs = require('fs');

var regexTitle = /(?:title = \\markup{\\override #'\(font-name . "Garamond Premier Pro Semibold"\){ \\abs-fontsize #18 \\smallCapsOldStyle")([^"]*)(?=")/,
    regexFileTitle = /(?:\d+-)(.*)(?=\.ly)/,
    regexComment = /\s*%.*$/g,
    regexMapLine = /(\d+):((?:([^;]+);?)+)/,
    regexPageNum = /(?:first-page-number\s+=\s+#)(\d+)',
    regexRaggedLast = /(?:ragged-last-bottom\s+=\s+##)(t)';

function getBlock(haystack, index) {
    var open = 0,
    pos = haystack.indexOf('{',index);
    while(pos >= 0) {
        if(haystack[pos] == '{') {
            open++;
        } else {
            open--;
        }
        if(open === 0) {
            return haystack.slice(index, pos + 1);
        }
        var posA = haystack.indexOf('{',pos+1);
        var posB = haystack.indexOf('}',pos+1);
        pos = (posA<0)? posB : (posB<0)? posA : Math.min(posA,posB);
    }
    return '';
}

var allFiles = fs.readdirSync('ly/'),
    temp = [],
    map = fs.readFileSync('ly/!map.txt','utf8').split('\n');
for(i in allFiles){
  var file = allFiles[i];
  if(file.match(/\.ly$/)) temp.push(file);
}
allFiles = temp;
var pages = '';
//TODO: del mapped\* -Recurse;
var toc = fs.readFileSync('ly/!Contents.ly','utf8');
var contents = fs.readFileSync('ly/Contents/!Contents.ly','utf8');
if(toc.indexOf('%CONTENTS%') >= 0) {
  toc = toc.replace('%CONTENTS%',contents);
} else {
  toc += contents;
}
fs.writeFile('ly/mapped/0.ly',toc);
for(j in map) {
    var file = map[j].trim().replace(regexComment,'');
    if(file.length == 0) {
        continue;
    }
    var m = regexMapLine.exec(file);
    if(m) {
        var page = m[1];
        pages += " 'tmp/"+page+".ps'";
        var lys = m[2].split(';');
        console.info(page);
        if(lys[0] == '*') {
            files = []
            file = '';
        } else {
            files = null;
            file = lys[0] + '.ly';
        }
        var index = allFiles.indexOf(file);
        if(index>=0) allFiles.splice(index,1);
        var current = fs.readFileSync('ly/' + file,'utf8');
        var result = '';
        //update Page number
        current = current.replace(regexPageNum,'first-page-number = #' + page);

        //set print-all-headers = ##t in \paper block
        
        var i = 0;
        while(current.length > 0) {
            var numScoreBlocks = 0,
                scoreBlockEndBrace = -1;
                       
            //remove midi blocks
            var pos = 0;
            while( (pos = current.indexOf('\\midi',pos)) >= 0) {
                var midiBlock = getBlock(current,pos);
                current = current.slice(0,pos) + current.slice(pos + midiBlock.length);
            }
            
            pos = 0;
            //remove score blocks that do not contain \layout block
            while( (pos = current.indexOf('\\score',pos)) >= 0) {
                scoreBlock = getBlock(current,pos);
                if(scoreBlock.indexOf('\\layout') < 0) {
                    current = current.slice(0,pos) + current.slice(pos + $scoreBlock.length);
                } else {
                    pos += scoreBlock.length;
                    numScoreBlocks++;
                    scoreBlockEndBrace = pos - 1;
                }
            }
            
            //Give Warning if more than one score block.
            if(numScoreBlocks != 1) {
                console.warn("Found " + numScoreBlocks + " score blocks; only expected one.");
            }

            //move \header block to end of score block
            if(scoreBlockEndBrace >= 0) {
                pos = current.indexOf('\\header');
                if(pos >= 0) {
                    var headerBlock = getBlock(current,pos);
                    var replaceHeaderBlock = '\\header{ tagline = ""}';
                    if(i != 0) {
                        replaceHeaderBlock = '';
                    }
                    current = current.slice(0,pos) +
                        replaceHeaderBlock +
                        current.slice(pos + headerBlock.length, scoreBlockEndBrace) +
                        headerBlock +
                        current.slice(scoreBlockEndBrace);
                }
            }

            if(i != 0) {
                //remove beginning part of file
                pos = current.indexOf('global = {');
                if(pos >= 0) {
                    current = current.slice(pos);
                }
            } else {
                //Add line: print-all-headers = ##t
                //Add line: ragged-right = ##f
                pos = current.IndexOf('\\paper');
                if(pos < 0) {
                    console.error("\\paper block not found...EXITING");
                    return;
                }
                pos = current.indexOf('{',pos);
                if(pos < 0) {
                    console.error("\\paper{} block not found...EXITING");
                    return;
                }
                pos++;
                current = current.slice(0, pos) + "\n  print-all-headers = ##t\n  ragged-right = ##f" +
                    current.slice(pos);
                //set ragged-last = ##f
                current = current.replace(regexRaggedLast,'ragged-last-bottom = ##f');
            }
            
            result += current;
            ++i;
            if(files != null) {
                if(i >= files.length) {
                    break;
                }
                var index = allFiles.indexOf(files[i]);
                if(index>=0) allFiles.splice(index,1);
                current = fs.readFileSync('ly/' + files[i],'utf8');
                continue;
            }
            if(i >= lys.length) {
                break;
            }
            file = lys[i] + '.ly';
            var index = allFiles.indexOf(file);
            if(index>=0) allFiles.splice(index);
            current = fs.readFileSync('ly/' + file,'utf8');
        }
        fs.writeFile('ly/mapped/' + page + '.ly',result);
        //&'C:\Program Files (x86)\lilypond\usr\bin\lilypond.exe' -dno-point-and-click --ps -o"tmp/$page" "mapped\$page.ly"
    }
}
//iex "&'c:\Program Files\gs\gs9.06\bin\gswin64c.exe' -q -dSAFER -dDEVICEWIDTHPOINTS=612 -dDEVICEHEIGHTPOINTS=792 -dCompatibilityLevel='1.4' -dNOPAUSE -dBATCH -r1200 -sDEVICE=pdfwrite -dEmbedAllFonts=true -dSubsetFonts=true -sOutputFile=""!full.pdf"" -c.setpdfwrite -f$pages";

if(allFiles.length > 0) {
    console.warn("Warning: The following files were not used.");
    console.warn(allFiles);
}

    
function processLy(lyFile,callback) {
    var outputName = lyFile.match(/^(?:.*\/)?((\d+).*\.ly)$/);
    if(!outputName) {
        console.info('Skipping "' + lyFile + '" because it is not a .ly file.');
        if(typeof(callback)=='function') {
            callback(null,'','',undefined,false);
        }
        return false;
    }
    var lyName = 'lytemp/'+outputName[1];
    outputName = 'lytemp/'+outputName[2];
    var psName = outputName + '.ps',
        lyContent = fs.readFileSync(lyFile,'utf8');
    if(fs.existsSync(psName)) {
        //Check if the .ly file was the same.
        if(fs.existsSync(lyName)) {
            var oldLyContent = fs.readFileSync(lyName,'utf8');
            if(lyContent == oldLyContent) {
                //console.info('Skipping "' + lyFile + '" because its .ps file already exists and the lilypond content was the same.');
                if(typeof(callback)=='function') {
                    callback(null,'','',psName,false);
                }
                return false;
            }
        }
        fs.unlinkSync(psName);
    }
    if(fs.existsSync(lyName)) {
        fs.unlinkSync(lyName);
    }
    var args = ['-dno-point-and-click','--ps','-o'+outputName,lyFile];
    console.info('Processing ' + lyFile);
    child_process.execFile('lilypond',args,undefined,function(error,stdout,stderr){
        if(error) {
            console.error(error);
            console.error(stderr);
            console.info(stdout);
        } else {
            fs.writeFile(lyName,lyContent);
        }
        
        if(typeof(callback)=='function'){
            callback(error,stdout,stderr,psName,true);
        }
    });
    return true;
}
var gsCmds = ['gs','gswin64c','gswin32c'],
    gsI = 0;
function ps2pdf(psFiles,width,height,outputName) {
    outputName = outputName || '!full.pdf';
    width *= 72;
    height*= 72;
    if(typeof(psFiles)=='string') psFiles = [psFiles];
    var args = ['-q','-dSAFER','-dDEVICEWIDTHPOINTS='+width,'-dDEVICEHEIGHTPOINTS='+height,'-dCompatibilityLevel=1.4','-dNOPAUSE','-dBATCH',
                '-r1200','-sDEVICE=pdfwrite','-dEmbedAllFonts=true','-dSubsetFonts=true','-sOutputFile='+outputName,'-c.setpdfwrite','-f'].concat(psFiles);
    console.info('Processing PDF of ' + psFiles.length + ' files...');
    //console.info('gs ' + args.join(' '));
    var gsCmd = gsCmds[gsI];
    var cb = function(error,stdout,stderr){
        if(error) {
            console.error(error);
            console.error(stderr);
            console.info(stdout);
            if ((gsCmd=gsCmds[++gsI])) child_process.execFile(gsCmd,args,undefined,cb);
            return;
        }
        console.info('Finished');
    };
    child_process.execFile(gsCmd,args,undefined,cb);
}

//processLy('ly/001-Contents.ly');
//ps2pdf('lytemp/001.ps',8.5,11,'test.pdf');
var dir = 'ly/mapped/',
    files = fs.readdirSync(dir).sort(),
    maxConcurrent = 6,
    currentlyActive = 0,
    i = 0,
    psFiles = [],
    callback = function(error,stdout,stderr,psName,startedWorker) {
        if(typeof(psName)=='string' && psName.length > 0) {
            psFiles.push(psName);
        }
        if(currentlyActive > 0 && startedWorker) --currentlyActive;
        while(i < files.length && currentlyActive < maxConcurrent) {
            //++currentlyActive;
            if(processLy(dir + files[i++], callback)) {
                console.info('Processing file ' + i + ' of ' + files.length + '; ' + (++currentlyActive) + ' active');
            }
        }
        if(i==files.length && currentlyActive === 0) {
            ps2pdf(psFiles.sort(),8.5,11,'!full.pdf');
            ++i;
            ++currentlyActive;
        }
    };
callback();