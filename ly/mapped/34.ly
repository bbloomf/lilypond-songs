\version "2.14.2"
\include "util.ly"
\header{ tagline = ""}
\paper {
  print-all-headers = ##t
  ragged-right = ##f
  %print-all-headers = ##t
  paper-height = 11\in
  paper-width = 8.5\in
  indent = 0\in
  %system-system-spacing = #'((basic-distance . 10) (padding . 0))
  system-system-spacing =
    #'((basic-distance . 0)
       (minimum-distance . 0)
       (padding . -3)
       (stretchability . 100))
  score-markup-spacing =
    #'((basic-distance . 0)
       (minimum-distance . 0)
       (padding . 0)
       (stretchability . 80))
  ragged-last-bottom = ##f
  ragged-bottom = ##f
  two-sided = ##t
  inner-margin = 0.95\in
  outer-margin = 0.7\in
  top-margin = 0.26\in
  bottom-margin = 0.25\in
  first-page-number = #34
  print-first-page-number = ##t
  headerLine = ""
  oddHeaderMarkup = \markup\fill-line{
     \override #'(font-name . "Garamond Premier Pro")\abs-fontsize #12.5
     \combine 
        \fill-line{"" \on-the-fly #print-page-number-check-first
        \oldStylePageNum""
        }
        \fill-line{\headerLine}
  }
  evenHeaderMarkup = \markup {
     \override #'(font-name . "Garamond Premier Pro")\abs-fontsize #12.5
     \combine
        \on-the-fly #print-page-number-check-first
        \oldStylePageNum""
        \fill-line{\headerLine}
  }
}
#(set-global-staff-size 18) \paper{ #(define fonts (make-pango-font-tree "Garamond Premier Pro" "Garamond Premier Pro" "Garamond Premier Pro" (/ 18 20))) }
global = {
  \key a \major
  \time 6/8
  \dynamicUp
  %\set crescendoSpanner = #'dashed-line
  %\set midiInstrument = "recorder"
  \autoBeamOff
  \override DynamicTextSpanner #'style = #'none
  \mergeDifferentlyDottedOn
}

sopMusic = \relative c' {
	\partial 8
  e8 |
  a4 a8 b[ a] b |
  cis16[ e8.] cis8 b[ a] b |
  cis8.[ b16] a8 a16[ fis8.] e8 |
  e8.[ fis16] a8 b4\fermata \bar""\break e,8 |
  a4 a8 b[ a] b |
  
  cis8[ e] cis b[ a] b |
  cis8.[ b16] a8 a16[ fis8.] e8 |
  e[ fis] a a4\fermata \bar""\break cis16[ d] |
  e4 fis8 e16[ cis8.] a8 |
  e'4 fis8 e[ cis] a |
  e'[ cis] a e'[ cis] a |
  
  fis'8.[ e16] cis8 \acciaccatura cis8 b4\fermata \bar""\break e,8 |
  a4 a8 b[ a] b |
  cis8[ e] cis b[ a] b |
  cis8.[ b16] a8 a16[ fis8.] e8 |
  e[ fis] a a4 \bar"|."
}
sopWords = \lyricmode {
  \set stanza = #"1. "
	Ye banks and braes o’ bon -- nie Doon,
  How can ye bloom sae fresh and fair?
  How can ye chaunt, ye lit -- tle birds,
  And I sae wea -- ry, fu’ of care?
  Thou’lt break my heart, thou warb -- ling bird,
  That won -- tons through the flow -- ’ry thorn,
  Thou mindst me o’ de -- part -- ed joys,
  De -- part -- ed nev -- er to __ re -- turn.
}

sopWordsII = \lyricmode {
  \set stanza = #"2. "
  Oft hae I rov’d by bon -- nie Doon,
  To see the rose and wood -- bine twine;
  When il -- ka bird sang o’ its love,
  And fond -- ly sae did I o’ mine.
  Wi’ light -- some heart I pu’d a rose,
  Fu’ sweet up -- on __ its thorn -- y tree;
  But my fause lov -- er stole my rose,
  And, ah! he left the thorn wi’ me.
}

sopWordsIII = \lyricmode {
  \set stanza = #"3. "
  
}

sopWordsIV = \lyricmode {
  \set stanza = #"4. "
}

sopWordsV = \lyricmode {
  \set stanza = #"5. "
}

altoMusic = \relative c' {
  \partial 8
  e8 |
  e4 e8 gis[ fis] gis |
  a4 a8 gis8[ fis] gis |
  a8.[ gis16] a8 fis16[ d8.] cis8 |
  cis4 cis8 e4 e8 |
  cis4 cis8 e4 e8 |
  
  a4 e8 e4 e8 |
  e4 e8 fis16[ d8.] cis8 |
  cis8[ d] cis cis4 a'8 |
  a4 fis8 e16[ a8.] a8 |
  a4 fis8 e[ a] a |
  a4 a8 a4 a8 |
  
  fis8.[ a16] a8 gis4 e8 |
  cis4 cis8 e4 e8 |
  e4 e8 e4 e8 |
  e4 e8 fis16[ d8.] cis8 |
  cis8[ d] cis cis4 \bar"|."
}
altoWords = \lyricmode {
}
altoWordsII = \lyricmode {
%\markup\italic
  \set stanza = #"2. "
}
altoWordsIII = \lyricmode {
  \set stanza = #"3. "
}
altoWordsIV = \lyricmode {
  \set stanza = #"4. "
}
altoWordsV = \lyricmode {
  \set stanza = #"5. "
  \set ignoreMelismata = ##t
}
altoWordsVI = \lyricmode {
  \set stanza = #"6. "
  \set ignoreMelismata = ##t
}
tenorMusic = \relative c' {
  \partial 8
  cis8 |
  cis4 cis8 e4 e8 |
  e16[ cis8.] e8 e4 e8 |
  cis8.[ d16] cis8 d16[ a8.] a8 |
  a4 a8 gis4 gis8 |
  a4 a8 gis[ fis] gis |
  
  a[ cis] a gis4 gis8 |
  a8.[ b16] cis8 d16[ a8.] a8 |
  a4 a8 a4 a16[ b] |
  cis4 d8 cis16[ a8.] cis8 |
  cis4 d8 cis[ a] cis |
  cis[ e] cis cis[ e] cis |
  
  d8.[ cis16] a8 e'4 e,8 |
  a4 a8 gis4 gis8 |
  a[ cis] a gis[ fis] gis |
  a8.[ b16] cis8 d16[ a8.] a8 |
  a4 e8 e4 \bar"|."
}

tenorWords = \lyricmode {
}

tenorWordsII = \lyricmode {
}

tenorWordsIII = \lyricmode {
}

bassMusic = \relative c' {
  \partial 8
  a8 |
  a4 a8 e4 e8 |
  a4 a8 e4 e8 |
  a4 a8 d,4 a8 |
  a4 a8 e'4\fermata e8 |
  a,4 a8 e'4 e8 |
  
  a4 a,8 e'4 e8 |
  a4 a8 d,4 a8 |
  a4 a8 a4 a'8 |
  a4 a8 a4 a8 |
  a4 a8 a4 a8 |
  a4 a8 a4 a8 |
  
  a4 a8 e4\fermata e8 |
  a,4 a8 e'4 e8 |
  a4 a8 e4 e8 |
  a4 a8 d,4 a'8 |
  a[ d,] a a4 \bar"|."
}
bassWords = \lyricmode {
}

pianoRH = \relative c' {
}
pianoLH = \relative c' {
}

\score {
<<
   \new ChoirStaff <<
    \new Staff = women <<
      \new Voice = "sopranos" { \voiceOne << \global \sopMusic >> }
      \new Voice = "altos" { \voiceTwo << \global \altoMusic >> }
    >>
    \new Lyrics = "altos"  \lyricsto "sopranos" \sopWords
    \new Lyrics = "altosII"  \lyricsto "sopranos" \sopWordsII
    \new Lyrics = "altosIII"  \lyricsto "sopranos" \sopWordsIII
    \new Lyrics = "altosIV"  \lyricsto "sopranos" \sopWordsIV
    \new Lyrics = "altosV"  \lyricsto "sopranos" \sopWordsV
   \new Staff = men <<
      \clef bass
      \new Voice = "tenors" { \voiceOne << \global \tenorMusic >> }
      \new Voice = "basses" { \voiceTwo << \global \bassMusic >> }
    >>
    \new Lyrics \with { alignAboveContext = #"tenors" } \lyricsto "tenors" \tenorWordsIII
    \new Lyrics \with { alignAboveContext = #"tenors" } \lyricsto "tenors" \tenorWordsII
    \new Lyrics \with { alignAboveContext = #"tenors" } \lyricsto "tenors" \tenorWords
  >>
%    \new PianoStaff << \new Staff { \new Voice { \pianoRH } } \new Staff { \clef "bass" \pianoLH } >>
  >>
  
  \layout {
    \context {
      \Lyrics
      \override LyricText #'font-size = #1.3
      \override VerticalAxisGroup #'staff-affinity = #0
      \override LyricText #'X-offset = #center-on-word
    }
    \context {
      \Score
      \override SpacingSpanner #'base-shortest-duration = #(ly:make-moment 1 8)
      \override SpacingSpanner #'common-shortest-duration = #(ly:make-moment 1 4)
    }
    \context {
      % Remove all empty staves
      \Staff \RemoveEmptyStaves \override VerticalAxisGroup #'remove-first = ##t
      
      \override VerticalAxisGroup #'staff-staff-spacing =
      #'((basic-distance . 0)
         (minimum-distance . 0)
         (padding . -1)
         (stretchability . 2))
    }
  }
\header {
  title = \markup{\override #'(font-name . "Garamond Premier Pro Semibold"){ \abs-fontsize #18 \smallCapsOldStyle"Bonnie Doon"}}
  poet = \markup\oldStyleNum"Robert Burns (1759–1796)"
  composer = \markup\oldStyleNum{"Scotch Air," \italic"The Caledonian Hunt’s Delight"}
  tagline = ""
}}


global = {
  \key e \major
  \time 4/4
  \dynamicUp
  %\set crescendoSpanner = #'dashed-line
  %\set midiInstrument = "recorder"
  \autoBeamOff
  \override DynamicTextSpanner #'style = #'none
}

sopMusic = \relative c' {
	gis'4 b a b |
  gis b fis b |
  gis b a fis |
  e dis e2 |
  gis4 b a b |
  
  gis b fis b |
  gis b a fis |
  e dis e2 |
  \repeat volta 2 {
    e'4 dis e b |
    a fis gis b |
    
    e dis e b |
    a( fis) e2 |
  } \break
  e4.\p e8 e4 e |
  fis4. fis8 fis4 fis |
  e^\markup\italic"rit." e e fis |
  e dis e2 \bar"|."
}
sopWords = \lyricmode {
  \set stanza = #"1. "
	Hark! the ves -- per hymn is steal -- ing
  O’er the wa -- ters soft and clear;
  Near -- er yet and near -- er peal -- ing
  Soft it breaks up -- on the ear,
  \markup\italic Ju -- \markup\italic bi -- \markup\italic la -- \markup\italic te,
  \markup\italic Ju -- \markup\italic bi -- \markup\italic la -- \markup\italic te,
  \markup\italic Ju -- \markup\italic bi -- \markup\italic la -- \markup\italic te, \markup\italic A -- \markup\italic men.
  Far -- ther now and far -- ther steal -- ing
  Soft it fades up -- on the ear.
}

sopWordsII = \lyricmode {
  \set stanza = #"2. "
  Now like moon -- light waves re -- treat -- ing
  To the shore it dies a -- long;
  Now like an -- gry sur -- ges meet -- ing
  Breaks the min -- gled tide of song.
  \markup\italic Ju -- \markup\italic bi -- \markup\italic la -- \markup\italic te,
  \markup\italic Ju -- \markup\italic bi -- \markup\italic la -- \markup\italic te,
  \markup\italic Ju -- \markup\italic bi -- \markup\italic la -- \markup\italic te, \markup\italic A -- \markup\italic men.
  Hark! a -- gain like waves re -- treat -- ing
  To the shore it dies a -- long
}

sopWordsIII = \lyricmode {
  \set stanza = #"3. "
  Once a -- gain sweet voic -- es ring -- ing
  Loud -- er still the mu -- sic swells;
  While on sum -- mer breez -- es wing -- ing
  Comes the chime of ves -- per bells.
  \markup\italic Ju -- \markup\italic bi -- \markup\italic la -- \markup\italic te,
  \markup\italic Ju -- \markup\italic bi -- \markup\italic la -- \markup\italic te,
  \markup\italic Ju -- \markup\italic bi -- \markup\italic la -- \markup\italic te, \markup\italic A -- \markup\italic men.
  On the sum -- mer breez -- es wing -- ing
  Fades the chime of ves -- per bells.
}

sopWordsIV = \lyricmode {
  \set stanza = #"4. "
}

sopWordsV = \lyricmode {
  \set stanza = #"5. "
}

altoMusic = \relative c' {
  e4 gis fis dis |
  e e dis fis |
  e d cis cis |
  b b b2 |
  e4 gis fis dis |
  
  e e dis fis |
  e d cis cis |
  b b8[ a] gis2 |
  \repeat volta 2 {
    gis'4 fis gis gis |
    fis dis e gis |
    
    gis fis gis gis |
    fis( dis) e2 |
  }
  b4. b8 cis4 b |
  e4. e8 dis4 dis |
  e d cis cis |
  b b8[ a] gis2 \bar"|."
}
altoWords = \lyricmode {
}
altoWordsII = \lyricmode {
%\markup\italic
  \set stanza = #"2. "
}
altoWordsIII = \lyricmode {
  \set stanza = #"3. "
}
altoWordsIV = \lyricmode {
  \set stanza = #"4. "
}
altoWordsV = \lyricmode {
  \set stanza = #"5. "
  \set ignoreMelismata = ##t
}
altoWordsVI = \lyricmode {
  \set stanza = #"6. "
  \set ignoreMelismata = ##t
}
tenorMusic = \relative c' {
  b4 b b b |
  b gis b b |
  b b a a |
  gis fis gis2 |
  b4 b b b |
  
  b gis b b |
  b b a a |
  gis fis e2 |
  \repeat volta 2 {
    b'4 b b b |
    b b b b |
    
    b b b b |
    cis( a) gis2 |
  }
  gis4. gis8 gis4 gis |
  cis4. cis8 b4 b |
  gis gis a a |
  gis fis e2 \bar"|."
}

tenorWords = \lyricmode {
}

tenorWordsII = \lyricmode {
}

tenorWordsIII = \lyricmode {
}

bassMusic = \relative c' {
  e,4 e dis b |
  e e b dis |
  e gis a a, |
  b b e2 |
  e4 e dis b |
  
  e e b dis |
  e gis a a, |
  b b e2 |
  \repeat volta 2 {
    e4 b e gis |
    b b, e e |
    
    e b e gis |
    fis( b,) e2 |
  }
  e4 dis cis b |
  a ais b4 b |
  e4 e e a, |
  b b e2 \bar"|."
}
bassWords = \lyricmode {
}

pianoRH = \relative c' {
}
pianoLH = \relative c' {
}

\score {
<<
   \new ChoirStaff <<
    \new Staff = women <<
      \new Voice = "sopranos" { \voiceOne << \global \sopMusic >> }
      \new Voice = "altos" { \voiceTwo << \global \altoMusic >> }
    >>
    \new Lyrics = "altos"  \lyricsto "sopranos" \sopWords
    \new Lyrics = "altosII"  \lyricsto "sopranos" \sopWordsII
    \new Lyrics = "altosIII"  \lyricsto "sopranos" \sopWordsIII
    \new Lyrics = "altosIV"  \lyricsto "sopranos" \sopWordsIV
    \new Lyrics = "altosV"  \lyricsto "sopranos" \sopWordsV
   \new Staff = men <<
      \clef bass
      \new Voice = "tenors" { \voiceOne << \global \tenorMusic >> }
      \new Voice = "basses" { \voiceTwo << \global \bassMusic >> }
    >>
    \new Lyrics \with { alignAboveContext = #"tenors" } \lyricsto "tenors" \tenorWordsIII
    \new Lyrics \with { alignAboveContext = #"tenors" } \lyricsto "tenors" \tenorWordsII
    \new Lyrics \with { alignAboveContext = #"tenors" } \lyricsto "tenors" \tenorWords
  >>
%    \new PianoStaff << \new Staff { \new Voice { \pianoRH } } \new Staff { \clef "bass" \pianoLH } >>
  >>
  
  \layout {
    \context {
      \Lyrics
      \override LyricText #'font-size = #1.3
      \override VerticalAxisGroup #'staff-affinity = #0
      \override LyricText #'X-offset = #center-on-word
    }
    \context {
      \Score
      \override SpacingSpanner #'base-shortest-duration = #(ly:make-moment 1 8)
      \override SpacingSpanner #'common-shortest-duration = #(ly:make-moment 1 4)
    }
    \context {
      % Remove all empty staves
      \Staff \RemoveEmptyStaves \override VerticalAxisGroup #'remove-first = ##t
      
      \override VerticalAxisGroup #'staff-staff-spacing =
      #'((basic-distance . 0)
         (minimum-distance . 0)
         (padding . -1)
         (stretchability . 2))
    }
  }
\header {
  title = \markup{\override #'(font-name . "Garamond Premier Pro Semibold"){ \abs-fontsize #18 \smallCapsOldStyle"Hark! the vesper hymn is stealing"}}
  composer = \markup\oldStyleNum"Folk Song"
  tagline = ""
}}


global = {
  \key f \major
  \time 4/4
  \dynamicUp
  %\set crescendoSpanner = #'dashed-line
  %\set midiInstrument = "recorder"
  \autoBeamOff
  \override DynamicTextSpanner #'style = #'none
}

sopMusic = \relative c' {
	f4 f f8([ g] a4) |
  g4\mf g g8([ a] bes4) |
  a4.\f bes8 c4 bes |
  
  a g f2 |
  a4.\p bes8 c4 d |
  c8[ bes] a[ g] bes4 a |
  a4.\cresc bes8 c4 d |
  
  c8[ bes] a[ g] bes4 a |
  a4\f a g g8[ c] |
  e4 d c2 |
  
  f,4\p f f8([ g] a4) |
  g4\cresc g g8([ a] bes4) |
  a4.\f bes8 c[ f] d[ bes] |
  a4 g f2 \bar "|."
}
sopWords = \lyricmode {
  \set stanza = #"1. "
	In the Spring, in the Spring,
  Sweet and fresh is ev -- ’ry -- thing;
  
  Win -- ter winds no more are blow -- ing,
  Blos -- soms fair a -- gain are grow -- ing,
  Gai -- ly mounts the lark on high!
  
  In the Spring, in the Spring,
  Sweet and fresh is ev -- ’ry -- thing.
}

sopWordsII = \lyricmode {
  \set stanza = #"2. "
  As God will, as God will,
  My fond heart yearns toward Him still.
  
  Should the heav’ns be o -- ver -- cloud -- ed,
  All the earth in dark -- ness shroud -- ed,
  Light will sure -- ly shine a -- gain.
  
  As God will, as God will,
  My fond heart yearns toward Him still.
  
}

sopWordsIII = \lyricmode {
  \set stanza = #"3. "
  Hush, my heart, hush, my heart!
  Joy will come and pain de -- part.
  
  If in sor -- row thou art weep -- ing,
  Great -- er peace thou shalt be reap -- ing,
  Ev -- er lift thine eyes a -- bove.
  
  Hush, my heart, hush, my heart!
  Joy will come and pain de -- part.
}

sopWordsIV = \lyricmode {
  \set stanza = #"4. "
}

sopWordsV = \lyricmode {
  \set stanza = #"5. "
}

altoMusic = \relative c' {
  f4\p f f2 |
  e4 e g2 |
  f4. f8 g4 g |
  
  f e f2 |
  f4. f8 g4 bes |
  g f8[ e] f4 f |
  f4. f8 g4 bes |
  
  g f8[ e] f4 f |
  f f f e8[ g] |
  g4 g8[ f] e2 |
  
  f4 f f2 |
  e4 f e2 |
  f4. f8 f4 f8[ g] |
  f4 e c2 \bar"|."
}
altoWords = \lyricmode {
}
altoWordsII = \lyricmode {
%\markup\italic
  \set stanza = #"2. "
}
altoWordsIII = \lyricmode {
  \set stanza = #"3. "
}
altoWordsIV = \lyricmode {
  \set stanza = #"4. "
}
altoWordsV = \lyricmode {
  \set stanza = #"5. "
  \set ignoreMelismata = ##t
}
altoWordsVI = \lyricmode {
  \set stanza = #"6. "
  \set ignoreMelismata = ##t
}
tenorMusic = \relative c' {
  a4 a a8([ bes] c4) |
  c4 c d2 |
  d4. d8 e4 d |
  
  c4 c8[ bes] a2 |
  c4. d8 e4 f |
  e c d c |
  d4. d8 e4 f |
  
  e4 c d c |
  c d d c |
  c b g8([ a] bes4) |
  
  a4 a a8([ bes] c4) |
  c d c2 |
  f4. e8 ees4 d4 |
  c c8[ bes] a2 \bar"|."
}

tenorWords = \lyricmode {
}

tenorWordsII = \lyricmode {
}

tenorWordsIII = \lyricmode {
}

bassMusic = \relative c' {
  f,4 f f2 |
  c'4 c bes8([ a] g4) |
  d4. d8 c4 g |
  
  a8([ bes] c4) f2 |
  f4. d8 c4 bes |
  c c' f, f |
  d4. d8 c4 bes |
  
  c4 c' f, f |
  f d8[ c] b4 c8[ e] |
  g4 g c,2 |
  f4 f f2 |
  c'4 b c2 |
  f,4. g8 a4 bes |
  c c, f2 \bar"|."
}
bassWords = \lyricmode {
}

pianoRH = \relative c' {
}
pianoLH = \relative c' {
}

\score {
<<
   \new ChoirStaff <<
    \new Staff = women <<
      \new Voice = "sopranos" { \voiceOne << \global \sopMusic >> }
      \new Voice = "altos" { \voiceTwo << \global \altoMusic >> }
    >>
    \new Lyrics = "altos"  \lyricsto "sopranos" \sopWords
    \new Lyrics = "altosII"  \lyricsto "sopranos" \sopWordsII
    \new Lyrics = "altosIII"  \lyricsto "sopranos" \sopWordsIII
    \new Lyrics = "altosIV"  \lyricsto "sopranos" \sopWordsIV
    \new Lyrics = "altosV"  \lyricsto "sopranos" \sopWordsV
   \new Staff = men <<
      \clef bass
      \new Voice = "tenors" { \voiceOne << \global \tenorMusic >> }
      \new Voice = "basses" { \voiceTwo << \global \bassMusic >> }
    >>
    \new Lyrics \with { alignAboveContext = #"tenors" } \lyricsto "tenors" \tenorWordsIII
    \new Lyrics \with { alignAboveContext = #"tenors" } \lyricsto "tenors" \tenorWordsII
    \new Lyrics \with { alignAboveContext = #"tenors" } \lyricsto "tenors" \tenorWords
  >>
%    \new PianoStaff << \new Staff { \new Voice { \pianoRH } } \new Staff { \clef "bass" \pianoLH } >>
  >>
  
  \layout {
    \context {
      \Lyrics
      \override LyricText #'font-size = #1.3
      \override VerticalAxisGroup #'staff-affinity = #0
      \override LyricText #'X-offset = #center-on-word
    }
    \context {
      \Score
      \override SpacingSpanner #'base-shortest-duration = #(ly:make-moment 1 8)
      \override SpacingSpanner #'common-shortest-duration = #(ly:make-moment 1 4)
    }
    \context {
      % Remove all empty staves
      \Staff \RemoveEmptyStaves \override VerticalAxisGroup #'remove-first = ##t
      
      \override VerticalAxisGroup #'staff-staff-spacing =
      #'((basic-distance . 0)
         (minimum-distance . 0)
         (padding . -1)
         (stretchability . 2))
    }
  }
\header {
  title = \markup{\override #'(font-name . "Garamond Premier Pro Semibold"){ \abs-fontsize #18 \smallCapsOldStyle"In the Spring"}}
  composer = \markup\oldStyleNum"Folk Song"
  tagline = ""
}}


