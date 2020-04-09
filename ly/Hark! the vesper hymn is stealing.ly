\version "2.14.2"
\include "util.ly"
\header {
  title = \markup{\override #'(font-name . "Garamond Premr Pro Smdb Subh"){ \abs-fontsize #18 \smallCapsOldStyle"Hark! the vesper hymn is stealing"}}
  composer = \markup\oldStyleNum"Folk Song"
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
       (padding . 1)
       (stretchability . 100))
  ragged-last-bottom = ##t
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
  \midi {
    \tempo 4 = 105
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


