\version "2.14.2"
\include "util.ly"
\header {
  title = \markup{\override #'(font-name . "Garamond Premier Pro Semibold"){ \abs-fontsize #18 \smallCapsOldStyle"The Separation"}}
  poet = \markup\oldStyleNum"Thomas Moore (1779–1852)"
  composer = \markup\oldStyleNum"J. C. Engelbrecht, 1857"
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
       (padding . 5)
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
  \key aes \major
  \time 4/4
  \dynamicUp
  %\set crescendoSpanner = #'dashed-line
  %\set midiInstrument = "recorder"
  \autoBeamOff
  \override DynamicTextSpanner #'style = #'none
}

sopMusic = \relative c' {
	\partial 4 ees4 |
  bes'4. bes8 aes g f ees |
  aes2*1/16 \teeny ees8*3/4 aes4. \normalsize b4\rest \bar"" aes8[ bes] |
  c4. c8 des c bes aes |
  bes2 bes4\rest \bar"" ees, |

  f4. g8 aes8. bes16 aes8. f16 |
  ees2*1/16 \teeny ees8*3/4 aes4. \normalsize bes4\rest \bar"" aes8[ aes] |
  \tieDashed g4.~ g8~ g g f g \tieSolid |
  aes2 bes4\rest \bar"" c |

  ees4. ees8 des c bes aes |
  f4.( g8 bes[ aes g]) f |
  ees4. aes8 g g f g |
  aes2 bes4\rest \bar"|."
}
sopWords = \lyricmode {
  \set stanza = #"1. "
  \set ignoreMelismata = ##t
	With all my soul then let us part, "" ""
  Since _ both are anx -- ious to be free,
  And I will send you home your heart, "" ""
  If __ _ you will send back mine to me,
  \unset ignoreMelismata
  And I will send you home your heart, __
  If you will send back mine to me.
}

sopWordsII = \lyricmode {
  \set stanza = #"2. "
  \set ignoreMelismata = ##t
  We’ve had some hap -- py hours to -- \skip1 geth -- er,
  But _ Joy must of -- ten change its wing,
  And spring would be but gloom -- y "" wea -- ther,
  If __ _ we had no -- thing else but spring.
  \unset ignoreMelismata
  And spring would be but gloom -- y wea -- ther,
  If we had nought else but spring.
}

sopWordsIII = \lyricmode {
  \set stanza = #"3. "
  \set ignoreMelismata = ##t
  Fare -- well, and when some fu -- ture "" lov -- er
  Shall _ claim the heart which I re -- sign,
  And in ex -- ult -- ing joy dis -- \skip1 cov -- er
  All the charms __ _ _ that once were mine,
  \unset ignoreMelismata
  And in ex -- ult -- ing joy dis -- cov -- er
  All the charms that once were mine.
}

sopWordsIV = \lyricmode {
  \set stanza = #"4. "
  \set ignoreMelismata = ##t
  I think I should be sweet -- ly blest, "" ""
  If _ in a fond im -- per -- fect sigh,
  You’d say while to his bo -- som prest, "" ""
  He __ _ loves not half so well as I.
  \unset ignoreMelismata
  And say while to his bo -- som prest, __
  He loves not half so well as I.
}

sopWordsV = \lyricmode {
  \set stanza = #"5. "
}

altoMusic = \relative c' {
  ees4 |
  g4. g8 f ees des ees |
  c2*1/16 \teeny c8*3/4 c4. \normalsize s4 c8[ des] |
  ees4. ees8 ees ees ees ees |
  des2 s4 des |

  des4. ees8 f8. f16 f8. des16 |
  c2*1/16 \teeny c8*3/4 c4. \normalsize s4 c8[ c] |
  \tieDashed des4.~ des8~ des des des des \tieSolid |
  c2 s4 ees4 |

  aes4. aes8 aes aes f ees |
  des4.( bes8 g'[ f ees]) des |
  c4. c8 des des des des |
  c2 s4 \bar"|."
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
  ees4 |
  des4. des8 des bes aes g |
  aes2*1/16 \teeny aes8*3/4 aes4. \normalsize s4 aes |
  aes4. aes8 aes aes bes c |
  g2 s4 g |

  aes4. aes8 aes8. aes16 aes8. aes16 |
  aes2*1/16 \teeny aes8*3/4 aes4. \normalsize s4 aes8[ aes] |
  \tieDashed bes4.~ bes8~ bes bes aes bes \tieSolid |
  aes2 s4 aes |

  c4. c8 f ees des aes |
  aes4.( des8)~ des8[ aes g] aes8 |
  aes4. aes8 bes bes aes bes |
  aes2 s4 \bar"|."
}

tenorWords = \lyricmode {
}

tenorWordsII = \lyricmode {
}

tenorWordsIII = \lyricmode {
}

bassMusic = \relative c' {
  ees,4 |
  ees4. ees8 ees ees ees ees |
  aes,2*1/16 \teeny aes8*3/4 aes4. \normalsize d4\rest aes4 |
  aes4. aes8 aes aes g aes |
  ees'2 d4\rest ees |

  des4. des8 des8. des16 des8. des16 |
  aes2*1/16 \teeny aes8*3/4 aes4. \normalsize d4\rest aes8[ aes] |
  \tieDashed ees'4.~ ees8~ ees ees ees ees |
  \tieSolid
  aes,2 d4\rest aes |

  aes4. aes8 aes aes bes c |
  des4.( ees8 des4.) des8 |
  ees4. ees8 ees ees ees ees |
  aes,2 d4\rest \bar"|."
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
