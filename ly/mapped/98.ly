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
       (padding . 1)
       (stretchability . 100))
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
  \key ees \major
  \time 6/8
  \dynamicUp
  %\set crescendoSpanner = #'dashed-line
  \autoBeamOff
  \override DynamicTextSpanner #'style = #'none
}

sopMusic = \relative c' {
  \partial 8
  \oneVoice\set Staff.midiInstrument = #"acoustic grand"
  bes'8_~ |

  <bes ees g>8.[ f'16 ees8] <f bes,>16[( bes8.) g,8]_~ |
  <g c ees>8.[ d'16 c8] <d g,>16[( g8.) ees,8]_~ |
  <ees aes c>8.[ bes'16 aes8] <bes ees,>16[ ees8.] <g, ees>8 |
  <f d>4.( ees4) bes'8\rest \bar"||"\break
  \once \override Score.RehearsalMark #'break-visibility = #end-of-line-visible
  \once \override Score.RehearsalMark #'self-alignment-X = #RIGHT
  \mark"Fine"

  \voiceOne\set Staff.midiInstrument = #"flute"
	ees,8. f16 ees8 ees'8. d16 ees8 |
  c8. bes16 c8 bes4 \teeny bes16 bes \normalsize |
  ees,8. f16 ees8 ees'8. d16 ees8 |
  
  c4. bes |
  ees,8. f16 ees8 ees'8. d16 ees8 |
  c8. bes16 c8 bes4 aes8 |

  g8. aes16 bes8 bes aes g |
  f4. ees4 b'8\rest | \break
  g8. aes16 bes8 bes4. |
  bes8. aes16 g8 f4 ees8 |
  g8. aes16 bes8 bes8. c16 bes8 |
  aes8. g16 f8 ees4. |

  g8. aes16 bes8 bes4. |
  bes8. aes16 g8 f4 ees8 |
  g8. aes16 bes8 c8. d16 ees8 |
  
  g,8. aes16 f8 ees4 \bar"|."
}
sopWords = \lyricmode {
  \repeat unfold 6 \skip1
  \set stanza = #"1. "
  \set ignoreMelismata = ##t
	Here’s to the maid -- en of bash -- ful fif -- teen, "" ""
  Here’s to the wid -- ow of fif -- ty;
  Here’s to the flant -- ing, ex -- trav -- a -- gant queen,
  And here’s to the house -- wife who’s thrift -- y.
}

sopWordsII = \lyricmode {
  \repeat unfold 6 \skip1
  \set stanza = #"2. "
  \set ignoreMelismata = ##t
  Here’s to the charm -- er whose dim -- ples we prize, "" ""
  Now to the maid who has none, Sir;
  Here’s to the girl with a pair of blue eyes,
  And here’s to the nymph with but one, Sir.

  Let the toast pass, drink to the lass;
  I war -- rant she’ll prove an ex -- cuse for the glass.
  Let the toast pass, drink to the lass;
  I war -- rant she’ll prove an ex -- cuse for the glass.
}

sopWordsIII = \lyricmode {
  \repeat unfold 6 \skip1
  \set stanza = #"3. "
  \set ignoreMelismata = ##t
  Here’s to the maid with a bo -- som of snow,
  Now to her that’s as brown as a ber -- ry;
  Here’s to the wife with a face full of woe!
  And here’s to the dam -- sel that’s mer -- ry.
}

sopWordsIV = \lyricmode {
  \repeat unfold 6 \skip1
  \set stanza = #"4. "
  Let her be clum -- sy or let her be slim,
  Young or an -- cient I care not a fea -- ther.
  Fill up your glass -- es quite up to the brim,
  And let us e’en toast them to -- geth -- er.
}

sopWordsV = \lyricmode {
  \set stanza = #"5. "
}

altoMusic = \relative c' {
  s8 s1.*2 |

  bes8. bes16 ees8 g8. g16 g8 |
  ees8. ees16 ees8 g4 \teeny g16 g \normalsize |
  ees8. bes16 bes8 c8 ees c |
  ees4. ees |

  bes8. bes16 ees8 g8. g16 g8 |
  ees8. ees16 ees8 g4 f8 |
  ees8. f16 g8 g f ees |
  d4. ees4 s8 |

  ees8. f16 g8 g4. |
  f8. f16 ees8 d4 ees8 |
  ees8. f16 g8 g8. g16 g8 |
  f8. ees16 d8 ees4. |

  ees8. f16 g8 g4. |
  g8. f16 ees8 d4 ees8 |
  ees8. f16 g8 aes8. bes16 c8 |
  ees,8. d16 d8 ees4
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
  s8 s1.*2 |
  g8. aes16 g8 bes8. bes16 bes8 |
  aes8. g16 aes8 bes4 \teeny bes16 bes \normalsize |
  g8. aes16 g8 g8 g g |
  aes4. g |

  g8. aes16 g8 bes8. bes16 bes8 |
  aes8. g16 aes8 bes4 d8 |
  ees8. ees16 bes8 bes bes bes |
  aes4. g4 s8 |

  bes8. bes16 bes8 bes4. |
  d8. d16 bes8 aes4 g8 |
  bes8. bes16 bes8 ees8. ees16 ees8 |
  d8. bes16 aes8 g4. |

  g8. aes16 bes8 bes4. |
  bes8. aes16 g8 bes4 ees,8 |
  bes'8. bes16 bes8 aes8. aes16 aes8 |
  bes8. bes16 aes8 g4
}

tenorWords = \lyricmode {
}

tenorWordsII = \lyricmode {
}

tenorWordsIII = \lyricmode {
}

bassMusic = \relative c' {
  %accompaniment
  \oneVoice\set Staff.midiInstrument = #"acoustic grand"
  d,8 |
  <ees ees'>4-> d8\rest <d d'>4-> d8\rest |
  <c c'>4-> d8\rest <bes bes'>4-> d8\rest |
  <aes aes'>4 d8\rest <g g,>8 d\rest bes8~ |
  <bes aes'>4.( <ees g>4) d8\rest \bar"||"

  \voiceTwo\set Staff.midiInstrument = #"flute"
  ees8. ees16 ees8 ees8. ees16 ees8 |
  aes,8 ees' aes, ees'4 \teeny ees16 ees \normalsize |
  ees8. ees16 ees8 c8 c c |
  
  aes4( c8) ees4. |
  ees8. ees16 ees8 ees8. ees16 ees8 |
  aes,8 ees' aes, ees'4 bes8 |

  ees8. ees16 ees8 ees ees ees |
  bes8.[ c16 d8] ees4 d8\rest |
  ees8. ees16 ees8 ees4. |

  bes8. bes16 bes8 bes4 ees8 |
  ees8. ees16 ees8 ees8. ees16 ees8 |
  bes8. bes16 bes8 ees4. |

  ees8. f16 g8 ees4. |
  g8. f16 ees8 bes4 ees8 |
  ees8. ees16 ees8 aes,8. aes16 aes8 |

  bes8. bes16 bes8 ees4 \bar"|."
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
      \new Voice = "sopranos" { \voiceOne << \global \set Staff.midiInstrument = #"flute" \sopMusic >> }
      \new Voice = "altos" { \voiceTwo << \global \set Staff.midiInstrument = #"flute" \altoMusic >> }
    >>
    \new Lyrics = "altos"  \lyricsto "sopranos" \sopWords
    \new Lyrics = "altosII"  \lyricsto "sopranos" \sopWordsII
    \new Lyrics = "altosIII"  \lyricsto "sopranos" \sopWordsIII
    \new Lyrics = "altosIV"  \lyricsto "sopranos" \sopWordsIV
    \new Lyrics = "altosV"  \lyricsto "sopranos" \sopWordsV
   \new Staff = men <<
      \clef bass
      \new Voice = "tenors" { \voiceOne << \global \set Staff.midiInstrument = #"flute" \tenorMusic >> }
      \new Voice = "basses" { \voiceTwo << \global \set Staff.midiInstrument = #"flute"\bassMusic >> }
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
         (minimum-distance . 10)
         (padding . 3)
         (stretchability . 2))
    }
  }
\header {
  title = \markup{\override #'(font-name . "Garamond Premier Pro Semibold"){ \abs-fontsize #18 \smallCapsOldStyle"Here’s to the Maiden of Bashful Fifteen"}}
  poet = \markup\oldStyleNum\concat{"from " \italic"The School for Scandal" ", by Richard Sheridan (1751–1816)"}
  composer = \markup\oldStyleNum"Thomas Linley (1725–1795)"
  tagline = ""
}}
