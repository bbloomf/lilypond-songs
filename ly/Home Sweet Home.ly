\version "2.14.2"
\include "util.ly"
\header {
  title = " "
  instrument = \markup{\override #'(font-name . "Garamond Premr Pro Smdb Subh"){ \abs-fontsize #18 \smallCapsOldStyle"Home Sweet Home"}}
  poet = \markup\oldStyleNum"John Howard Payne (1791–1852)"
  composer = \markup\oldStyleNum"Sir Henry Rowley Bishop (1786–1855)"
  tagline = ""
}
\paper {
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
       (padding . -14)
       (stretchability . 100))
  top-system-spacing =
    #'((basic-distance . 0)
       (minimum-distance . 0)
       (padding . -1)
       (stretchability . 0))
  ragged-last-bottom = ##f
  ragged-bottom = ##f
  two-sided = ##t
  inner-margin = 1\in
  outer-margin = 0.75\in
  top-margin = 0.26\in
  bottom-margin = 0.25\in
  first-page-number = #196
  print-first-page-number = ##t
  headerLine = ""
  oddHeaderMarkup = \markup\fill-line{
     \override #'(font-name . "Garamond Premr Pro")\abs-fontsize #12.5
     \combine 
        \fill-line{"" \on-the-fly #print-page-number-check-first
        \oldStylePageNum""
        }
        \fill-line{\headerLine}
  }
  evenHeaderMarkup = \markup {
     \override #'(font-name . "Garamond Premr Pro")\abs-fontsize #12.5
     \combine
        \on-the-fly #print-page-number-check-first
        \oldStylePageNum""
        \fill-line{\headerLine}
  }
}
#(set-global-staff-size 18) \paper{ #(define fonts (make-pango-font-tree "Garamond Premr Pro" "Garamond Premr Pro" "Garamond Premr Pro" (/ 18 20))) }
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
	\partial 4
  e8[ fis] |
  gis4.( a8) a4 cis |
  \slurDashed b4.( gis8) gis4( b) | \slurSolid
  a4.( gis8) a4 fis |
  gis2 b4\rest e,8[ fis] |
  gis4.( a8) a4 cis |
  
  b2 gis4 b |
  a4.( gis8) a4 fis |
  e2 b'4\rest \tieDashed b8~ b | \tieSolid
  e4.( dis8) cis4. b8 |
  b2 gis4 b |
  a4.( gis8) a4 fis |
  
  gis2 b4\rest \tieDashed b8~ b | \tieSolid
  e4.( dis8) cis4. b8 |
  b2 gis4 b |
  b4 a2 fis4 |
  e2. b'4\rest |
  b1 |
  a2( fis) |
  
  e4 b'\rest fis b\rest |
  gis2 b4\rest b |
  e4.( dis8) cis4. b8 |
  b2 gis4 b |
  a4.( gis8) a4 fis |
  e2 b'4\rest \bar"|."
}
sopWords = \lyricmode {
  \set stanza = #"1. "
	’Mid plea -- sures and \set ignoreMelismata = ##t
  pal -- a -- ces __ _ though _ we may roam,
  Be it ev -- _ er so hum -- ble, there’s no __ _ place like home;
  \unset ignoreMelismata
  A __ charm from the skies seems to hal -- low us there,
  Which, seek through the world, is ne’er met with else -- where.
  
}

sopWordsII = \lyricmode {
  \set stanza = #"2. "
  I __ gaze on the moon \set ignoreMelismata = ##t as I \unset ignoreMelismata tread __ the drear wild,
  And feel that my moth -- er now thinks of her child;
  \set ignoreMelismata = ##t
  As she \unset ignoreMelismata looks on that moon from our own cot -- tage door
  \set ignoreMelismata = ##t
  Through the \unset ignoreMelismata wood -- bine whose fra -- grance shall cheer me no more.
  
  Home, home, home, sweet home,
  There’s no __ place like home,
  Oh there’s no __ place like home.
}

sopWordsIII = \lyricmode {
  \set stanza = #"3. "
  An ex -- ile from home, \set ignoreMelismata = ##t splen -- dor \unset ignoreMelismata daz -- zles in vain;
  Oh! give me my low -- ly thatched cot -- tage a -- gain;
  The birds sing -- ing gai -- ly, that came at my call;
  \set ignoreMelismata = ##t
  Give me \unset ignoreMelismata them and that peace of mind, dear -- er than all.
}

sopWordsIV = \lyricmode {
  \set stanza = #"4. "
}

sopWordsV = \lyricmode {
  \set stanza = #"5. "
}

altoMusic = \relative c' {
  e8[ dis] |
  e4.( fis8) fis4 a |
  \slurDashed gis4.( e8) e4( gis) | \slurSolid
  fis4.( e8) fis4 dis |
  e2 s4 e8[ dis] |
  e4.( fis8) fis4 a |
  
  gis2 e4 gis |
  fis4.( e8) fis4 dis |
  e2 s4 \tieDashed gis8~ gis | \tieSolid
  gis4.( b8) a4. gis8 |
  gis2 e4 gis |
  fis4.( e8) fis4 dis |
  
  e2 s4 \tieDashed gis8~ gis | \tieSolid
  gis4.( b8) a4. gis8 |
  gis2 e4 gis |
  fis4 dis2 dis4 |
  e2. s4 |
  gis1 |
  fis2( dis) |
  
  e4 s dis s |
  e2 s4 gis |
  gis4.( b8) a4. gis8 |
  gis2 e4 gis |
  fis4.( e8) fis4 dis |
  e2 s4 \bar"|."
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
  gis8[ a] |
  b2 b4 e |
  \tieDashed \slurDashed e4.( b8) b4~ b | \slurSolid \tieSolid
  b2 b4 b |
  b2 s4 gis8[ a] |
  b2 b4 e |
  
  e2 b4 b |
  b2 b4 a |
  gis2 s4 \tieDashed b8~ b | \tieSolid
  b2 e4. e8 |
  e2 b4 b |
  b2 b4 b |
  
  b2 s4 \tieDashed b8~ b | \tieSolid
  b2 e4. e8 |
  e2 b4 b |
  dis4 b2 a4 |
  gis2. s4 |
  b1 |
  b |
  
  gis4 s b s |
  b2 s4 b |
  b2 e4. e8 |
  e2 b4 b |
  b2 b4 a |
  gis2 s4 \bar"|."
}

tenorWords = \lyricmode {
}

tenorWordsII = \lyricmode {
}

tenorWordsIII = \lyricmode {
}

bassMusic = \relative c' {
  e,4 |
  e2 e4 e |
  \tieDashed e4.~ e8 e4~ e | \tieSolid
  b2 b4 b |
  e2 d4\rest e |
  e2 e4 e |
  
  e2 e4 e |
  b2 b4 b |
  e2 d4\rest \tieDashed e8~ e | \tieSolid
  e2 e4. e8 |
  e2 e4 e |
  b2 b4 b |
  
  e2 d4\rest \tieDashed e8~ e | \tieSolid
  e2 e4. e8 |
  e2 e4 e |
  b4 b2 b4 |
  e2. d4\rest |
  e1 |
  b |
  
  cis4 d\rest b d\rest |
  e2 d4\rest e |
  e2 e4. e8 |
  e2 e4 e |
  b2 b4 b |
  e2 d4\rest \bar"|."
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
  \midi {
    \tempo 4 = 115
    \set Staff.midiInstrument = "flute"
  
    \context {
      \Voice
      \remove "Dynamic_performer"
    }
  }
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
}


