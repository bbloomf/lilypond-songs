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
       (padding . 0.2)
       (stretchability . 100))
  score-markup-spacing =
    #'((basic-distance . 0)
       (minimum-distance . 0)
       (padding . -14)
       (stretchability . 60))
  top-system-spacing =
    #'((basic-distance . 0)
       (minimum-distance . 0)
       (padding . -1.5)
       (stretchability . 0))
  ragged-last-bottom = ##f
  ragged-bottom = ##f
  two-sided = ##t
  inner-margin = 1\in
  outer-margin = 0.75\in
  top-margin = 0.26\in
  bottom-margin = 0.25\in
  first-page-number = #98
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
  \key e \major
  \time 4/4
  \dynamicUp
  %\set crescendoSpanner = #'dashed-line
  %\set midiInstrument = "recorder"
  \autoBeamOff
  \override DynamicTextSpanner #'style = #'none
}

sopMusic = \relative c' {
	b'4 gis8. a16 b8. e16 dis8. cis16 |
  \slurDashed b2( gis) |
  \slurSolid a4( a16[) gis a8] b4 a |
  gis2. b4\rest |
  b gis8. a16 b8. e16 dis8. cis16 |
  b2 gis |
  fis4 ais8 cis e4 ais, |
  b2. b4\rest |
  
  b4 gis8. a16 b8. e16 dis8. cis16 |
  b2 gis |
  a4( a16)[ gis a8] b4 a |
  gis2. b4\rest |
  b4 gis8. a16 b8. e16 dis8. cis16 |
  b2 gis |
  fis4 b8. a16 gis4 fis |
  e2. b'4\rest \bar"||"\break
  
  e4. e8 e4 cis |
  b4. cis8 b4 gis |
  cis4. cis8 cis4 e |
  dis2. b4\rest |
  b gis8. a16 b8. e16 dis8. cis16 |
  b2 gis
  fis4 b8 a gis4 fis |
  e2. b'4\rest \bar"|."
}
sopWords = \lyricmode {
  \set stanza = #"1. "
	\set ignoreMelismata = ##t
  Hail to the crys -- tal foun -- tain flow -- ing
  \unset ignoreMelismata
  Pure, bright and clear;
  Touch not the ru -- by wine -- cup glow -- ing,
  Shun it with man -- ly fear.
  
  A -- way the dazz -- ling soul of mad -- ness,
  Of grief and pain!
  We hail the spark -- ling fount of glad -- ness,
  It can -- not leave a stain.
}

sopWordsII = \lyricmode {
  \set stanza = #"2. "
  \set ignoreMelismata = ##t
  Let not Co -- lum -- bia’s sons and daugh -- ters
  \unset ignoreMelismata
  The wine ex -- tol;
  But let us quaff the spark -- ling wa -- ters,
  Wine to the he -- ro’s soul.
  
  We need no oth -- er in -- spir -- a -- tion
  Than truth and right;
  Cool heads and hands must guard the na -- tion,
  Her hon -- or is her might.
  
  Hail the march of Pro -- hi -- bi -- tion!
  May its ban -- ner float,
  Up -- held by temp -- ’rance leg -- is -- la -- tion,
  Hon -- ored by voice and vote.
}

sopWordsIII = \lyricmode {
  \set stanza = #"3. "
  No lur -- ing blush shall chain the hours __
  That Free -- dom loves;
  No lurk -- ing fiend shall steal the pow -- ers
  Vir -- tue so well ap -- proves.
  
  No sting the spark -- ling wa -- ter hid -- eth,
  No dead -- ly care;
  No mis -- e -- ry or woe be -- tid -- eth,
  For spark -- ling truth is there.
}

sopWordsIV = \lyricmode {
  \set stanza = #"4. "
}

sopWordsV = \lyricmode {
  \set stanza = #"5. "
}

altoMusic = \relative c' {
  gis'4 e8. fis16 gis8. gis16 b8. a16 |
  \slurDashed gis2( e) |
  \slurSolid fis4( fis16)[ e fis8] gis4 fis |
  e2. s4 |
  gis4 e8. fis16 gis8. gis16 b8. a16 |
  gis2 e |
  dis4 fis8 fis fis4 e |
  dis2. s4 |
  
  gis4 e8. fis16 gis8. gis16 b8. a16 |
  gis2 e |
  fis4( fis16)[ e fis8] gis4 fis |
  e2. s4 |
  gis4 e8. fis16 gis8. gis16 b8. a16 |
  gis2 e |
  dis4 dis8. dis16 e4 dis |
  e2. s4 |
  
  gis4. gis8 a4 a |
  gis4. a8 gis4 e |
  e4. e8 a4 a |
  fis2. s4 |
  gis e8. fis16 gis8. gis16 b8. a16 |
  gis2 e |
  dis4 dis8 dis e4 dis |
  e2. s4 \bar"|."
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
  e4 b8. b16 b8. b16 b8. b16 |
  \slurDashed e2( b) |
  \slurSolid b b4 b |
  b2. s4 |
  e4 b8. b16 b8. b16 b8. b16 |
  e2 b |
  b4 cis8 ais ais4 cis |
  b2. s4 |
  
  e4 b8. b16 b8. b16 b8. b16 |
  e2 b |
  b b4 b |
  b2. s4 |
  e4 b8. b16 b8. b16 b8. b16 |
  e2 b |
  b4 fis8. fis16 b4 a |
  gis2. s4 |
  
  b4. b8 cis4 e |
  e4. e8 e4 b |
  a4. a8 e'4 cis |
  b2. s4 |
  e4 b8. b16 b8. b16 b8. b16 |
  e2 b |
  b4 fis8 fis b4 a |
  gis2. s4 \bar"|."
  
}

tenorWords = \lyricmode {
}

tenorWordsII = \lyricmode {
}

tenorWordsIII = \lyricmode {
}

bassMusic = \relative c' {
  e,4 e8. e16 e8. e16 e8. e16 |
  \tieDashed e2~ e |
  \tieSolid b b4 b |
  e2. d4\rest |
  e4 e8. e16 e8. e16 e8. e16 |
  e2 e |
  fis4 fis8 fis fis4 fis |
  b,2. d4\rest |
  
  e4 e8. e16 e8. e16 e8. e16 |
  e2 e |
  b b4 b |
  e2. d4\rest |
  e4 e8. e16 e8. e16 e8. e16 |
  e2 e |
  b4 b8. b16 b4 b |
  e2. d4\rest |
  
  e4. e8 a4 a |
  e4. e8 e4 e |
  a4. a8 a4 a |
  b2. d,4\rest |
  e4 e8. e16 e8. e16 e8. e16 |
  e2 e |
  b4 b8 b b4 b |
  e2. d4\rest \bar"|."
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
  title = \markup{\override #'(font-name . "Garamond Premier Pro Semibold"){ \abs-fontsize #18 \smallCapsOldStyle"The March of Prohibition"}}
  poet = \markup\oldStyleNum"M. Rebecca Darr"
  composer = \markup\oldStyleNum"Jas. L. Orr"
  tagline = ""
}}


global = {
  \key des \major
  \time 4/4
  \dynamicUp
  %\set crescendoSpanner = #'dashed-line
  %\set midiInstrument = "recorder"
  \autoBeamOff
  \override DynamicTextSpanner #'style = #'none
}

sopMusic = \relative c' {
	f4. f8 f f f f |
  aes4 ees f b\rest |
  bes4. bes8 bes bes bes bes |
  bes2 aes4 b\rest |
  
  aes4. aes8 aes aes aes aes |
  aes2 f4 b\rest |
  f4. f8 bes aes des, ees |
  f4 ees des \bar""\break
  
  f8. ges16 |
  aes4( des f) ees8. des16 |
  bes4( des2) c8. bes16 |
  aes4. bes8 aes[ f] des[ f] |
  ees2. \bar""\break
  
  f8. ges16 |
  aes4( des f) ees8. des16 |
  bes4( des2)\fermata des8.\fermata bes16 |
  aes8 f des ees f4 ees |
  des2. b'4\rest \bar"|."
}
sopWords = \lyricmode {
  \set stanza = #"1. "
	God be with you till we meet a -- gain,
  By His coun -- sels guide, up -- hold you,
  With His sheep se -- cure -- ly fold you,
  God be with you till we meet a -- gain,
  
  Till we meet, __
  till we meet,
  Till we meet at Je -- sus’ feet;
  
  Till we meet, __
  till we meet,
  God be with you till we meet a -- gain.
}

sopWordsII = \lyricmode {
  \set stanza = #"2. "
  God be with you till we meet a -- gain,
  ’Neath His wings pro -- tect -- ing hide you,
  Dai -- ly man -- na still pro -- vide you,
  God be with you till we meet a -- gain,
  
}

sopWordsIII = \lyricmode {
  \set stanza = #"3. "
  God be with you till we meet a -- gain,
  When life’s per -- ils thick con -- found you,
  Put His arms un -- fail -- ing ’round you,
  God be with you till we meet a -- gain,
}

sopWordsIV = \lyricmode {
  \set stanza = #"4. "
  God be with you till we meet a -- gain,
  Keep love’s ban -- ner float -- ing o’er you,
  Smite death’s threat -- ’ning wave be -- fore you,
  God be with you till we meet a -- gain,
}

sopWordsV = \lyricmode {
  \set stanza = #"5. "
}

altoMusic = \relative c' {
  des4. des8 des des des des |
  des4 c des s |
  des4. ges8 ges ges ges ges |
  ges2 f4 s |
  
  ees4. ees8 ees ees ges ges |
  f2 des4 s |
  des4. des8 ges f des des |
  des4 c des
  
  des8. ees16 |
  f2( aes4) aes8. aes16 |
  ges4( bes2) aes8. ges16 |
  f4. ges8 f[ des] des4 |
  c2.
  
  des8. ees16 |
  f2( aes4) aes8. aes16 |
  ges4( bes2) ges8. ges16 |
  f8 des des des des4 c |
  des2. s4 \bar"|."
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
  aes4. aes8 aes aes aes aes |
  aes4 aes aes s |
  ges4. des'8 des des des des |
  des2 des4 s |
  
  c4. c8 c c ees ees |
  des2 aes4 s |
  aes4. aes8 des des aes bes |
  aes4 ges f
  
  s |
  s aes8 aes des4 des8. des16 |
  des4 des8 des des4 des8. des16 |
  des4. des8 des[ aes] f[ aes] |
  aes4 aes8 aes aes4
  
  s4 |
  s aes8 aes8 des4 des8. des16 |
  des4 des8 des des4 bes8. des16 |
  des8 aes aes bes aes4 ges |
  f2. s4 \bar"|."
}

tenorWords = \lyricmode {
  \repeat unfold 34 ""
  Till we meet, till we meet, till we meet,
  \repeat unfold 7 ""
  Till we meet
  Till we meet, till we meet, till we meet,
}

tenorWordsII = \lyricmode {
}

tenorWordsIII = \lyricmode {
}

bassMusic = \relative c' {
  des,4. des8 des des des des |
  f4 aes des, d\rest |
  ges4. ges8 ges ges ges ges |
  des2 des4 d\rest |
  
  aes'4. aes8 aes aes aes, aes |
  des2 des4 d\rest |
  des4. des8 des des f ges |
  aes4 aes, des
  
  d\rest |
  d\rest des8 des des4 f8. f16 |
  ges4 ges8 ges ges4 ges8. ges16 |
  des4. des8 des4 des |
  aes4 aes8 aes aes4 
  
  d\rest |
  d\rest des8 des8 des4 f8. f16 |
  ges4 ges8 ges ges4\fermata ges8.\fermata ges16 |
  des8 des f ges aes4 aes, |
  des2. d4\rest \bar"|."
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
    \new Lyrics = "tenors"
   \new Staff = men <<
      \clef bass
      \new Voice = "tenors" { \voiceOne << \global \tenorMusic >> }
      \new Voice = "basses" { \voiceTwo << \global \bassMusic >> }
    >>
    \context Lyrics = "tenors" \lyricsto "tenors" \tenorWords
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
  title = \markup{\override #'(font-name . "Garamond Premier Pro Semibold"){ \abs-fontsize #18 \smallCapsOldStyle"God be with you till we meet again"}}
  composer = \markup\oldStyleNum"Jeremiah E. Rankin (1828–1904)"
  composer = \markup\oldStyleNum"William G. Tomer (1833–1896)"
  tagline = ""
}}



