\version "2.14.2"
\include "util.ly"
\header {
  title = \markup{\override #'(font-name . "Garamond Premr Pro Smdb Subh"){ \abs-fontsize #18 \smallCapsOldStyle"An Hymn to be sung between the Epistle and Gospel"}}
  %subtitle = \markup{\override #'(font-name . "Garamond Premr Pro Smdb Subh"){ \abs-fontsize #12.5 \smallCapsOldStyle"Luke II."}}
  composer = \markup\oldStyleNum"William Knapp (1698–1768)"
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
       (padding . 2)
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
  \key g\major
  \time 3/2
  \dynamicUp
  %\set crescendoSpanner = #'dashed-line
  %\set midiInstrument = "recorder"
  \autoBeamOff
  \override DynamicTextSpanner #'style = #'none
}

sopMusic = \relative c' {
  b'1 b2 |
  d1 d2 |
  b1 g2 |
  fis1. |
  g1. \bar"||"
  b1\rest
  \repeat volta 2 {
    b2 |
    b1 b2 |

    b1 a4( b) |
    c2( b) a |
    a1 \bar"||"
    b2 |
    a1 a2 |
    b1 d2 |
    b2.( a4) g2 |
    fis1 \bar"||"

    a2 |
    g1 d'2 |
    b1 g2 |
    g1 fis2 |
    g1
  }
}
sopWords = \lyricmode {
  Ho -- ly, ho -- ly, ho -- ly Lord God,
  All Things de -- clare thy Ma -- jes -- ty,
  An -- gels and Men still cry __ a -- loud,
  Glo -- ry to Thee, O \set associatedVoice = "altos" God, most high.
}

sopWordsII = \lyricmode {
  \set stanza = #"2. "
  
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
  d1 g2 |
  fis1 d2 |
  d1 d2 |
  d1. |
  b1. \bar"||"
  s1
  g'2 |
  g1 g2 |

  g1 fis2 |
  e1 e2 |
  fis1 \bar"||"
  g2 |
  fis1 d2 |
  d( e) d |
  g( fis) e |
  <d \tweak #'font-size #-2 a'>1 \bar"||"

  d2 |
  g1 a2 |
  g1 d2 |
  e2 d2.( c4) |
  b1
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
  g1 g2 |
  a1 a2 |
  b1 b2 |
  a4.( b8 c4 b a4. g8) |
  g1. \bar"||"
  s1 b4( c) |
  d1 d2 |

  d1 d2 |
  e( d) cis |
  d1 \bar"||"
  d2 |
  d1 d,2 |
  g1 g2 |
  b4.( c8 d2) c4( b) |
  a1 \bar"||"

  d4( c) |
  b1 a2 |
  b1 d2 |
  c b( a) |
  g1
}

tenorWords = \lyricmode {
}

tenorWordsII = \lyricmode {
}

tenorWordsIII = \lyricmode {
}

bassMusic = \relative c' {
  g1 g2 |
  d1 d2 |
  g1 g2 |
  d1. |
  g, \bar"||"
  d'1\rest g2 |
  g1 g2 |

  g1 a4( g) |
  fis2( g) a |
  d,1 \bar"||"
  g2 |
  d1 d2 |
  b1 b2 |
  g2.( a4) b( c) |
  d1 \bar"||"

  d2 |
  e1 fis2 |
  g1 b,2 |
  c2 d1 |
  g,1
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
    \new Lyrics = "altos"
    \new Lyrics = "altosII"  \lyricsto "sopranos" \sopWordsII
    \new Lyrics = "altosIII"  \lyricsto "sopranos" \sopWordsIII
    \new Lyrics = "altosIV"  \lyricsto "sopranos" \sopWordsIV
    \new Lyrics = "altosV"  \lyricsto "sopranos" \sopWordsV
   \new Staff = men <<
      \clef bass
      \new Voice = "tenors" { \voiceOne << \global \tenorMusic >> }
      \new Voice = "basses" { \voiceTwo << \global \bassMusic >> }
    >>
    \context Lyrics = "altos" \lyricsto "sopranos" \sopWords
    \new Lyrics \with { alignAboveContext = #"tenors" } \lyricsto "tenors" \tenorWordsIII
    \new Lyrics \with { alignAboveContext = #"tenors" } \lyricsto "tenors" \tenorWordsII
    \new Lyrics \with { alignAboveContext = #"tenors" } \lyricsto "tenors" \tenorWords
  >>
%    \new PianoStaff << \new Staff { \new Voice { \pianoRH } } \new Staff { \clef "bass" \pianoLH } >>
  >>
  \midi {
    \tempo 4 = 90
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
