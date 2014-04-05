\version "2.14.2"
\include "util.ly"
\header {
  title = \markup{\override #'(font-name . "Garamond Premier Pro Semibold"){ \abs-fontsize #18 \smallCapsOldStyle"Our Blest Redeemer"}}
  composer = \markup\oldStyleNum"John Bacchus Dykes (1823–1876)"
  composer = \markup\oldStyleNum"Harriet Auber (1773–1862)"
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
  \key ees \major
  \time 4/4
  \dynamicUp
  %\set crescendoSpanner = #'dashed-line
  %\set midiInstrument = "recorder"
  \autoBeamOff
  \override DynamicTextSpanner #'style = #'none
}

sopMusic = \relative c' {
  \partial 4 ees4 |
  ees4. ees8 f4 g |
  aes4. aes8 g4 \bar"" bes |
  ees g, aes a |
  bes2. \bar""\break

  aes?4 |
  g4. g8 aes4 bes |
  c c bes \bar"" g |
  bes2. aes4 |
  g2. \bar"|."
}
sopWords = \lyricmode {
  \set stanza = #"1. "
  Our blest Re -- deem -- er, ere he breathed
  His ten -- der, last fare -- well,
  A Guide, a Com -- fort -- er, be -- queathed,
  \set associatedVoice = "tenors"
  With us to dwell.
}

sopWordsII = \lyricmode {
  \set stanza = #"2. "
  He came, in tongues of liv -- ing flame,
  To teach, con -- vince, sub -- due;
  All pow’r -- ful as the wind he came,
  \set associatedVoice = "tenors"
  As view -- less too.
}

sopWordsIII = \lyricmode {
  \set stanza = #"3. "
  He came sweet in -- flu -- ence to impart,
  A gra -- cious, will -- ing guest,
  While he can find one hum -- ble heart
  \set associatedVoice = "tenors"
  Where -- in to rest.
}

sopWordsIV = \lyricmode {
  \set stanza = #"4. "
  And His that gen -- tle voice we hear,
  Soft as the breath of even,
  That checks each fault, that calms each fear
  \set associatedVoice = "tenors"
  And speaks of heav’n.
}

sopWordsV = \lyricmode {
  \set stanza = #"5. "
  And eve -- ry vir -- tue we pos -- sess,
  And eve -- ry vic -- t’ry won,
  And eve -- ry thought of ho -- li -- ness,
  \set associatedVoice = "tenors"
  Are His a -- lone.
}

sopWordsVI = \lyricmode {
  \set stanza = #"6. "
  Spi -- rit of pu -- ri -- ty and grace,
  Our weak -- ness pit -- ying see;
  O, make our hearts Thy dwell -- ing -- place,
  \set associatedVoice = "tenors"
  And wor -- thier Thee!
}

altoMusic = \relative c' {
  bes4 |
  bes4. bes8 bes4 ees |
  ees4. ees8 ees4 ees8[ d] |

  ees4 ees ees ees |
  d2.

  d4 |
  bes4. ees8 ees4 des |
  c d? ees ees |

  ees2 d |
  ees2. \bar"|."
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
  g4 |
  g4. g8 aes4 bes |
  c c bes g8[ aes] |

  bes4 bes bes c |
  bes2.

  f4 |
  g4. bes8 c4 g |
  aes aes bes bes |

  g2 f4( bes) |
  bes2. \bar"|."
}

tenorWords = \lyricmode {
}

tenorWordsII = \lyricmode {
}

tenorWordsIII = \lyricmode {
}

bassMusic = \relative c' {
  ees,4 |
  ees4. ees8 ees4 ees |
  aes, aes ees' ees8[ f] |

  g4 ees c f |
  bes,2.

  bes4 |
  ees4. ees8 ees4 ees |
  aes f g ees |

  bes2 bes |
  ees2. \bar"|."
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
    \new Lyrics = "altosII"
    \new Lyrics = "altosIII"
    \new Lyrics = "altosIV"
    \new Lyrics = "altosV"
    \new Lyrics = "altosVI"
   \new Staff = men <<
      \clef bass
      \new Voice = "tenors" { \voiceOne << \global \tenorMusic >> }
      \new Voice = "basses" { \voiceTwo << \global \bassMusic >> }
    >>
    \context Lyrics = "altos"  \lyricsto "altos" \sopWords
    \context Lyrics = "altosII"  \lyricsto "altos" \sopWordsII
    \context Lyrics = "altosIII"  \lyricsto "altos" \sopWordsIII
    \context Lyrics = "altosIV"  \lyricsto "altos" \sopWordsIV
    \context Lyrics = "altosV"  \lyricsto "altos" \sopWordsV
    \context Lyrics = "altosVI"  \lyricsto "altos" \sopWordsVI
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
