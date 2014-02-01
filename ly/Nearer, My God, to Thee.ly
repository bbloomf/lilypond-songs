\version "2.14.2"
\include "util.ly"
\header {
  title = \markup{\override #'(font-name . "Garamond Premier Pro Semibold"){ \abs-fontsize #18 \smallCapsOldStyle"Nearer, My God, to Thee"}}
  composer = \markup\oldStyleNum"Lowell Mason (1792–1872)"
  poet = \markup\oldStyleNum"Sarah Flower Adams (1805–1848)"
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
  \key g \major
  \time 6/4
  \dynamicUp
  %\set crescendoSpanner = #'dashed-line
  %\set midiInstrument = "recorder"
  \autoBeamOff
  \override DynamicTextSpanner #'style = #'none
}

sopMusic = \relative c' {
  b'2. a2 g4 |
  g2 e4 e2. |
  d g2 b4 |
  a2.~ a2 b4\rest |

  b2. a2 g4 |
  g2 e4 e2. |
  d2( g4) fis2 a4 |
  g2.~ g2 b4\rest |

  d2. e2 d4 |
  d2 b4 d2. |
  d2. e2 d4 |
  d2 b4 a2. |

  b2. a2 g4 |
  g2 e4 e2. |
  d2( g4) fis2 a4 |
  g2.~ g2 b4\rest \bar"|."
}
sopWords = \lyricmode {
  \set stanza = #"1. "
  Near -- er, my God, to Thee,
  Near -- er to Thee!
  E’en though it be a cross
  That rais -- eth me,

  Still all my song shall be,
  Near -- er, my 
}

sopWordsII = \lyricmode {
  \set stanza = #"2. "
  Though, like the wand -- er -- er,
  The sun gone down,
  Dark -- ness be o -- ver me,
  My rest a stone;

  Yet in my dreams I’d be
  Near -- er, my God, to Thee,

  Near -- er, my God, to Thee,
  Near -- er to Thee!
}

sopWordsIII = \lyricmode {
  \set stanza = #"3. "
  There let the way ap -- pear,
  Steps un -- to heav’n;
  All that Thou send -- est me,
  In mer -- cy giv’n;

  An -- gels to beck -- on me
  Near -- er, my 
}

sopWordsIV = \lyricmode {
  \set stanza = #"4. "
}

sopWordsV = \lyricmode {
  \set stanza = #"5. "
}

altoMusic = \relative c' {
  d2. c2 b4 |
  e2 c4 c2. |
  d d2 d4 |
  d2.~ d2 s4 |

  d2. c2 b4 |
  e2 c4 c2. |
  b2( d4) d2 d4 |
  d2.~ d2 s4 |

  g2. g2 g4 |
  g2 g4 g2. |
  g g2 g4 |
  d2 d4 d2. |
  d c2 b4 |
  e2 c4 c2. |
  b2( d4) d2 d4 |
  d2.~ d2 s4 \bar"|."
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
  g2. fis2 g4 |
  c2 g4 g2. |
  b g2 g4 |
  fis2.~ fis2 s4 |
  g2. fis2 g4 |
  c2 g4 g2. |
  g2( b4) a2 c4 |
  b2.~ b2 s4 |

  b2. c2 b4 |
  b2 g4 b2. |
  b c2 b4 |
  a2 g4 fis2. |
  g fis2 g4 |
  c2 g4 g2. |
  g2( b4) a2 c4 |
  b2.~ b2 s4 \bar"|."
}

tenorWords = \lyricmode {
}

tenorWordsII = \lyricmode {
}

tenorWordsIII = \lyricmode {
}

bassMusic = \relative c' {
  g2. d2 e4 |
  c2 c4 c2. |
  g b2 g4 |
  d'2.~ d2 d4\rest |
  g2. d2 e4 |
  c2 c4 c2. |
  d d2 d4 |
  g,2.~ g2 d'4\rest |

  g2. g2 g4 |
  g2 g4 g2. |
  g c,2 g'4 |
  fis2 g4 d2. |
  g d2 e4 |
  c2 c4 c2. |
  d d2 d4 |
  g,2.~ g2 d'4\rest \bar"|."
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
