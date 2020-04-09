\version "2.14.2"
\include "util.ly"
\header {
  title = \markup{\override #'(font-name . "Garamond Premr Pro Smdb Subh"){ \abs-fontsize #18 \smallCapsOldStyle"Angel Band"}}
  composer = \markup\oldStyleNum"Jefferson Hascall (1807–1887)"
  poet = \markup\oldStyleNum"William Bradbury (1816–1868)"
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
  \key c \major
  \time 6/8
  \dynamicUp
  %\set crescendoSpanner = #'dashed-line
  %\set midiInstrument = "recorder"
  \autoBeamOff
  \override DynamicTextSpanner #'style = #'none
}

sopMusic = \relative c' {
  \partial 8
  \repeat unfold 2 {
    \bar""\break
    g'8 |
    g4 g8 g4 g8 |
    a4 c8 c4 a8 |
    g4 c8 e4 d8 |
    c4.~ c4 
  } b8\rest |

  \break
  d4. d |
  e4 d8 c4. |
  d4. d4 c8 |
  e4 d8 c4 \bar"" c8 |
  a c c d c a |
  g4 e8 g4 \bar"" g8 |

  c4 c8 d4 c8 |
  e4( d8 c4) \bar"" c8 |
  a c c d c a |
  g4 e8 g4 \bar"" g8 |
  c4 c8 e4 d8 |
  c4.~ c4 \bar"|."
}
sopWords = \lyricmode {
  \set stanza = #"1. "
  My lat -- est sun is sink -- ing fast,
  My race is near -- ly run;
  My strong -- est tri -- als now are past,
  My tri -- umph is be -- gun.

  O come, an -- gel band,
  come, and a -- round me stand;
  O bear me a -- way on your snow -- y wings
  To my im -- mor -- tal home;
  O bear me a -- way on your snow -- y wings
  To my im -- mor -- tal home.
}

sopWordsII = \lyricmode {
  \set stanza = #"2. "
  I’m near -- ing now the ho -- ly ranks
  Of friends and kin -- dred dear;
  I brush the dews on Jor -- dan’s banks—
  The cross -- ing must be near.
}

sopWordsIII = \lyricmode {
  \set stanza = #"3. "
  I’ve al -- most gained my heav’n -- ly home,
  My spi -- rit loud -- ly sings;
  The ho -- ly ones, be -- hold they come!
  I hear the noise of wings.
}

sopWordsIV = \lyricmode {
  \set stanza = #"4. "
  O bear my long -- ing heart to Him
  Who bled and died for me;
  Whose blood now cleans -- es from all sin,
  And gives me vic -- to -- ry.
}

sopWordsV = \lyricmode {
  \set stanza = #"5. "
}

altoMusic = \relative c' {
  \repeat unfold 2 {
    e8 |
    e4 e8 e4 e8 |
    f4 a8 a4 f8 |
    e4 e8 g4 f8 |
    e4.~ e4
  } s8 |

  g4. g |
  g4 f8 e4. |
  g g4 e8 |
  g4 f8 e4 e8 |
  f f f f f f |
  e4 c8 e4 e8 |

  g4 g8 g4 e8 |
  g4( f8 e4) e8 |
  f f f f f f |
  e4 c8 e4 e8 |
  e4 e8 g4 f8 |
  e4.~ e4 \bar"|."
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
  \repeat unfold 2 {
    g8 |
    g4 g8 c4 c8 |
    c4 c8 c4 c8 |
    c4 c8 c4 b8 |
    c4.~ c4
  } s8 |

  b4. b |
  c4 b8 c4. |
  b b4 c8 |
  c4 b8 c4 c8 |
  c a a a a c |
  c4 g8 c4 c8 |

  c4 c8 b4 c8 |
  c4( b8 c4) c8 |
  c a a a a c |
  c4 g8 c4 c8 |
  c4 c8 c4 b8 |
  c4.~ c4 \bar"|."
}

tenorWords = \lyricmode {
}

tenorWordsII = \lyricmode {
}

tenorWordsIII = \lyricmode {
}

bassMusic = \relative c' {
  \repeat unfold 2 {
    c,8 |
    c4 c8 c4 c8 |
    f4 f8 f4 f8 |
    g4 g8 g4 g8 |
    c,4.~ c4
  } d8\rest |

  g4. g |
  c4 g8 c,4. |
  g' g4 g8 |
  c4 g8 c,4 c8 |
  f f f f f f |
  c4 c8 c4 c8 |

  e4 e8 g4 g8 |
  c4( g8 c,4) c8 |
  f f f f f f |
  c4 c8 c4 c8 |
  g'4 g8 g4 g8 |
  c,4.~ c4 \bar"|."
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
