\version "2.14.2"
\include "util.ly"
\header {
  title = \markup{\override #'(font-name . "Garamond Premier Pro Semibold"){ \abs-fontsize #18 \smallCapsOldStyle"My Lodging is on the Cold Ground"}}
  composer = \markup\oldStyleNum"English Folk Song, 17th or 18th Century"
  poet = \markup\oldStyleNum"John Gay (1685–1732), based on a song from 1665 or earlier"
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
  \time 6/8
  \dynamicUp
  %\set crescendoSpanner = #'dashed-line
  %\set midiInstrument = "recorder"
  \autoBeamOff
  \override DynamicTextSpanner #'style = #'none
}

sopMusic = \relative c' {
	\partial 8 g'16[ f] |
  ees8. f16 ees8 ees[ g] bes |
  aes[ c ees] ees4 d16[ c] |

  bes8. aes16 g8 f8. ees16 f8 |
  g4.~ g4 g16[ f] |
  ees8.[ f16] ees8 ees[ g] bes |

  aes[ c ees]~ ees d c |
  bes[ ees] g, f8.[ ees16] f8 |
  ees4.~ ees4 bes'8 |

  %page2
  bes[ g] ees' ees4 bes8 |
  c[ aes ees'] ees4 d16[ c] |
  bes8. aes16 g8 f8.[ ees16] f8 |

  g4.~ g4 g16[ f] |
  ees8. f16 ees8 ees[ g] bes |
  aes[ c ees] ees4\fermata d16[ c] |
  
  bes8[ ees] g, f8.[ ees16] f8 |
  ees4.~ ees4 \bar"|."
}
sopWords = \lyricmode {
  \set stanza = #"1. "
	My lodg -- ing is on __ the cold __ ground,
  And hard, ver -- y hard is my fare, __
  But that __ which grieves me more __ is
  The cold -- ness of __ my dear. __
  Yet still I cry, oh! turn, __ love,
  I __ pri -- thee, love, turn __ to me; __
  For thou art the on -- ly one, __ love,
  That art __ a -- dor’d __ by me. __
}

sopWordsII = \lyricmode {
  \set stanza = #"2. "
  \set ignoreMelismata = ##t
  I’ll _ twine thee a gar -- land of straw, __ _ _ love,
  I’ll _ mar -- ry thee with a rush ring; __ _
  My _ froz -- _ en hopes _ will thaw, __ _ _ _ love,
  And mer -- ri -- ly we __ _ will sing. __ _
  Then turn _ to me, my own __ _ _ love,
  \unset ignoreMelismata
  I __ pri -- thee, love, turn __ to me; __
  For thou art the on -- ly one, __ love,
  That art __ a -- dor’d __ by me. __
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
  ees8 |
  bes8. bes16 bes8 bes[ ees] g |
  aes[ ees c'] aes4 f16[ aes] |
  g8. f16 ees8 d8. c16 d8 |
  ees4.~ ees4 ees8 |

  bes4 bes8 bes[ ees] des |
  c[ ees aes]( fis) fis fis |
  g[ g] ees d8.[ c16] d8 |
  bes4.~ bes4 g'8 |

  g[ ees] g g4 g8 |
  ees8[ c aes'] ees4 f16[ aes] |
  g8. f16 ees8 d8.[ c16] d8 |
  ees4.~ ees4 ees8 |

  bes8. bes16 bes8 bes[ ees] g |
  aes[ ees c'] aes4 f16[ aes] |
  g4 ees8 d8.[ c16] d8 |
  bes4.~ bes4 \bar"|."
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
  bes8 |
  g8. aes16 g8 g[ bes] bes |
  c[ aes aes] c4 bes16[ ees] |
  bes8. bes16 bes8 aes8. g16 aes8 |
  bes4.~ bes4 bes8 |

  g8.[ aes16] g8 g[ bes] g8 |
  aes8[ c aes]( c) c c |
  ees[ ees] bes aes8.[ g16] aes8 |
  g4.~ g4 bes8 |

  bes4 bes8 bes4 ees8 |
  c[ ees aes,] c4 bes16[ ees] |
  bes8. bes16 bes8 aes8.[ g16] aes8 |
  bes4.~ bes4 bes8 |

  g8. aes16 g8 g[ bes] bes |
  c[ aes aes] c4 bes16[ ees] |
  bes4 bes8 aes8.[ g16] aes8 |
  g4.~ g4 \bar"|."
}

tenorWords = \lyricmode {
}

tenorWordsII = \lyricmode {
}

tenorWordsIII = \lyricmode {
}

bassMusic = \relative c' {
  ees,8 |
  ees8. d16 ees8 ees8[ ees] ees |
  ees8[ c aes] aes4 bes16[ c] |

  ees8. ees16 ees8 bes8. c16 bes8 |
  ees4.~ ees4 ees8 |
  ees8.[ d16] ees8 ees4 ees8 |

  aes,4.( a8) a a |
  bes8[ bes] bes bes8.[ c16] bes8 |
  ees4.~ ees4 ees8 |

  %page2
  ees4 ees8 ees4 g8 |
  aes[ ees c] aes4 bes16[ c] |
  ees8. ees16 ees8 bes8.[ c16] bes8 |

  ees4.~ ees4 ees8 |
  ees8. d16 ees8 ees4 ees8 |
  ees8[ c aes] aes4 bes16[ c] |

  ees4 ees8 bes8.[ c16] bes8 |
  ees4.~ ees4 \bar"|."
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
